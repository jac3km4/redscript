use std::cell::Cell;
use std::fmt;
use std::str::FromStr;

use itertools::Itertools;
use redscript::ast::{BinOp, Constant, Expr, Ident, Literal, Seq, SwitchCase, TypeName, UnOp};
use redscript::bundle::ConstantPool;
use redscript::definition::{AnyDefinition, Definition, Function, Type};
use redscript::io::DeferredFmt;

use crate::error::Error;
use crate::lines::LineCount;
use crate::{Decompiler, RawAst};

#[derive(Debug, Clone, Copy)]
pub enum OutputMode {
    Code { verbose: bool },
    SyntaxTree,
    Bytecode,
}

pub fn display_definition<'a>(
    definition: &'a Definition,
    pool: &'a ConstantPool,
    mode: OutputMode,
) -> impl fmt::Display + 'a {
    display_definition_with_line_count(definition, pool, mode).0
}

pub fn display_definition_with_line_count<'a>(
    definition: &'a Definition,
    pool: &'a ConstantPool,
    mode: OutputMode,
) -> (impl fmt::Display + 'a, LineCount) {
    let line_count = LineCount::default();
    let line_count_copy = line_count.clone();
    let fmt = DeferredFmt::new(move |fmt: &mut fmt::Formatter<'_>| {
        Writer {
            fmt,
            pool,
            mode,
            current_line: line_count_copy.get_cell(),
        }
        .write_definition(definition, 0)
        .map_err(|_| fmt::Error)
    });
    (fmt, line_count)
}

struct Writer<'ctx, 'buf> {
    fmt: &'ctx mut fmt::Formatter<'buf>,
    pool: &'ctx ConstantPool,
    current_line: &'ctx Cell<u32>,
    mode: OutputMode,
}

impl<'a, 'f> Writer<'a, 'f> {
    fn write_definition(&mut self, definition: &Definition, depth: usize) -> Result<(), Error> {
        match &definition.value {
            AnyDefinition::Type(_) => write!(self.fmt, "{}", format_type(definition, self.pool)?)?,
            AnyDefinition::Class(class) => {
                writeln!(self.fmt)?;
                write!(self.fmt, "{} ", class.visibility)?;
                if class.flags.is_abstract() {
                    write!(self.fmt, "abstract ")?;
                }
                if class.flags.is_final() {
                    write!(self.fmt, "final ")?;
                }
                if class.flags.is_import_only() {
                    write!(self.fmt, "importonly ")?;
                } else if class.flags.is_native() {
                    write!(self.fmt, "native ")?;
                }
                if class.flags.is_struct() {
                    write!(self.fmt, "struct ")?;
                } else {
                    write!(self.fmt, "class ")?;
                }
                write!(self.fmt, "{} ", self.pool.names.get(definition.name)?)?;
                if !class.base.is_undefined() {
                    write!(self.fmt, "extends {} ", self.pool.def_name(class.base)?)?;
                }
                writeln!(self.fmt, "{{")?;

                for field_index in &class.fields {
                    let field = self.pool.definition(*field_index)?;
                    self.write_definition(field, depth + 1)?;
                }

                for method_index in &class.methods {
                    let method = self.pool.definition(*method_index)?;
                    if let Err(err) = self.write_definition(method, depth + 1) {
                        log::error!("Method decompilation {} failed (caused by {})", method_index, err);
                    }
                }
                writeln!(self.fmt, "}}")?;
            }
            AnyDefinition::EnumValue(val) => {
                let name = self.pool.names.get(definition.name)?;
                self.write_indent(depth)?;
                writeln!(self.fmt, "{} = {},", name, val)?;
            }
            AnyDefinition::Enum(enum_) => {
                writeln!(self.fmt)?;
                writeln!(self.fmt, "enum {} {{", self.pool.names.get(definition.name)?)?;

                for member in &enum_.members {
                    self.write_definition(self.pool.definition(*member)?, depth + 1)?;
                }

                writeln!(self.fmt, "}}")?;
            }
            AnyDefinition::Function(fun) => {
                let return_type = fun.return_type.map_or_else(
                    || "Void".to_owned(),
                    |idx| format_type(self.pool.definition(idx).unwrap(), self.pool).unwrap(),
                );

                let name = self.pool.names.get(definition.name)?;
                let pretty_name = name.split(';').next().expect("Function with empty name");

                let params = fun
                    .parameters
                    .iter()
                    .map(|param| format_param(self.pool.definition(*param).unwrap(), self.pool).unwrap())
                    .format(", ");

                writeln!(self.fmt)?;

                if let Some(source) = &fun.source {
                    self.current_line.set(source.line);
                }

                self.write_indent(depth)?;
                write!(self.fmt, "{} ", fun.visibility)?;
                if fun.flags.is_final() {
                    write!(self.fmt, "final ")?;
                }
                if fun.flags.is_static() {
                    write!(self.fmt, "static ")?;
                }
                if fun.flags.is_native() {
                    write!(self.fmt, "native ")?;
                }
                if fun.flags.is_exec() {
                    write!(self.fmt, "exec ")?;
                }
                if fun.flags.is_const() {
                    write!(self.fmt, "const ")?;
                }
                if fun.flags.is_quest() {
                    write!(self.fmt, "quest ")?;
                }
                if fun.flags.is_callback() {
                    write!(self.fmt, "cb ")?;
                }
                write!(self.fmt, "func {}({}) -> {}", pretty_name, params, return_type)?;

                if fun.flags.has_body() {
                    self.write_function_body(fun, depth)?;
                } else {
                    write!(self.fmt, ";")?;
                }
                writeln!(self.fmt)?;
            }
            AnyDefinition::Parameter(_) => write!(self.fmt, "{}", format_param(definition, self.pool)?)?,
            AnyDefinition::Local(local) => {
                let type_name = format_type(self.pool.definition(local.type_)?, self.pool)?;
                let name = self.pool.names.get(definition.name)?;
                self.write_indent(depth)?;
                if local.flags.is_const() {
                    write!(self.fmt, "const ")?;
                } else {
                    write!(self.fmt, "let ")?;
                }
                write!(self.fmt, "{}: {};", name, type_name)?;
            }
            AnyDefinition::Field(field) => {
                let type_name = format_type(self.pool.definition(field.type_)?, self.pool)?;
                let field_name = self.pool.names.get(definition.name)?;

                writeln!(self.fmt)?;
                for property in &field.attributes {
                    self.write_indent(depth)?;
                    writeln!(
                        self.fmt,
                        "@runtimeProperty(\"{}\", \"{}\")",
                        property.name, property.value
                    )?;
                }

                for property in &field.defaults {
                    self.write_indent(depth)?;
                    writeln!(self.fmt, "@default({}, {})", property.name, property.value)?;
                }

                self.write_indent(depth)?;
                write!(self.fmt, "{} ", field.visibility)?;
                if field.flags.is_inline() {
                    write!(self.fmt, "inline ")?;
                }
                if field.flags.is_replicated() {
                    write!(self.fmt, "replicated ")?;
                }
                if field.flags.is_editable() {
                    write!(self.fmt, "edit ")?;
                }
                if field.flags.is_native() {
                    write!(self.fmt, "native ")?;
                }
                if field.flags.is_persistent() {
                    write!(self.fmt, "persistent ")?;
                }
                if field.flags.is_const() {
                    write!(self.fmt, "const ")?;
                }
                writeln!(self.fmt, "let {}: {};", field_name, type_name)?;
            }
            AnyDefinition::SourceFile(_) => panic!(),
        }
        Ok(())
    }

    fn write_function_body(&mut self, fun: &Function, depth: usize) -> Result<(), Error> {
        writeln!(self.fmt, " {{")?;
        match self.mode {
            OutputMode::Code { .. } => {
                let code = Decompiler::decompiled(fun, self.pool)?;
                self.write_seq(&code, depth + 1)?;
            }
            OutputMode::SyntaxTree => {
                let code = Decompiler::decompiled(fun, self.pool)?;
                for expr in code.exprs {
                    self.write_indent(depth + 1)?;
                    writeln!(self.fmt, "{:#?}", expr)?;
                }
            }
            OutputMode::Bytecode => {
                for local in &fun.locals {
                    self.write_definition(self.pool.definition(*local)?, depth + 1)?;
                    writeln!(self.fmt)?;
                }
                for (offset, instr) in fun.code.iter() {
                    let op = format!("{:?}", instr).to_lowercase();
                    self.write_indent(depth + 1)?;
                    writeln!(self.fmt, "{}: {}", offset.value, op)?;
                }
            }
        }

        self.write_indent(depth)?;
        write!(self.fmt, "}}")?;
        Ok(())
    }

    fn write_seq(&mut self, code: &Seq<RawAst>, depth: usize) -> Result<(), Error> {
        for expr in code.exprs.iter().filter(|expr| !expr.is_empty()) {
            self.write_indent(depth)?;
            self.write_expr(expr, depth)?;
            writeln!(self.fmt, ";")?;
        }
        Ok(())
    }

    fn write_expr(&mut self, expr: &Expr<RawAst>, depth: usize) -> Result<(), Error> {
        self.write_expr_nested(expr, None, depth)
    }

    fn write_expr_nested(
        &mut self,
        expr: &Expr<RawAst>,
        parent_op: Option<ParentOp>,
        depth: usize,
    ) -> Result<(), Error> {
        match expr {
            Expr::Ident(ident, _) => write!(self.fmt, "{}", ident)?,
            Expr::Constant(cons, _) => match cons {
                Constant::String(Literal::String, str) => write!(self.fmt, "\"{}\"", str::escape_default(str))?,
                Constant::String(Literal::Name, str) => write!(self.fmt, "n\"{}\"", str::escape_default(str))?,
                Constant::String(Literal::Resource, str) => write!(self.fmt, "r\"{}\"", str::escape_default(str))?,
                Constant::String(Literal::TweakDbId, str) => write!(self.fmt, "t\"{}\"", str::escape_default(str))?,
                Constant::I32(lit) => write!(self.fmt, "{}", lit)?,
                Constant::I64(lit) => write!(self.fmt, "{}l", lit)?,
                Constant::U32(lit) => write!(self.fmt, "{}u", lit)?,
                Constant::U64(lit) => write!(self.fmt, "{}u", lit)?,
                Constant::F32(lit) => write!(self.fmt, "{:.2}", lit)?,
                Constant::F64(lit) => write!(self.fmt, "{:.2}d", lit)?,
                Constant::Bool(true) => write!(self.fmt, "true")?,
                Constant::Bool(false) => write!(self.fmt, "false")?,
            },
            Expr::DynCast(type_, expr, _) => {
                if parent_op.is_some() {
                    write!(self.fmt, "(")?;
                    self.write_expr(expr, 0)?;
                    write!(self.fmt, " as {}", type_)?;
                    write!(self.fmt, ")")?;
                } else {
                    self.write_expr(expr, 0)?;
                    write!(self.fmt, " as {}", type_)?;
                }
            }
            Expr::Declare(name, type_, val, _) => {
                write!(self.fmt, "let {}", name)?;
                if let Some(type_) = type_ {
                    write!(self.fmt, ": {}", type_)?;
                }
                if let Some(val) = val {
                    write!(self.fmt, " = ")?;
                    self.write_expr(val, 0)?;
                }
            }
            Expr::Assign(lhs, rhs, _, _) => {
                self.write_expr(lhs, 0)?;
                write!(self.fmt, " = ")?;
                self.write_expr(rhs, 0)?;
            }
            Expr::Call(fun, _, type_args, params, _, line) => {
                if let &Some(line) = line {
                    self.current_line.set(line.into());
                }
                match &**fun {
                    Expr::Ident(name, _) => {
                        self.write_call(name, type_args, params, parent_op)?;
                    }
                    Expr::Member(expr, name, _) => {
                        self.write_expr_nested(expr, Some(ParentOp::Dot), 0)?;
                        write!(self.fmt, ".")?;
                        self.write_call(name, &[], params, None)?;
                    }
                    _ => {}
                }
            }
            Expr::ArrayElem(arr, idx, _, _) => {
                self.write_expr(arr, 0)?;
                write!(self.fmt, "[")?;
                self.write_expr(idx, 0)?;
                write!(self.fmt, "]")?;
            }
            Expr::New(ident, params, _) => {
                write!(self.fmt, "new {}(", ident)?;
                if !params.is_empty() {
                    for param in params.iter().take(params.len() - 1) {
                        self.write_expr(param, depth)?;
                        write!(self.fmt, ", ")?;
                    }
                    self.write_expr(params.last().unwrap(), depth)?;
                }
                write!(self.fmt, ")")?;
            }
            Expr::Return(Some(expr), _) => {
                write!(self.fmt, "return ")?;
                self.write_expr(expr, depth)?;
            }
            Expr::Return(None, _) => write!(self.fmt, "return")?,
            Expr::Seq(exprs) => self.write_seq(exprs, depth)?,
            Expr::Switch(expr, cases, default, _, _) => {
                write!(self.fmt, "switch ")?;
                self.write_expr(expr, 0)?;
                writeln!(self.fmt, " {{")?;
                for SwitchCase { matcher, body } in cases {
                    self.write_indent(depth + 1)?;
                    write!(self.fmt, "case ")?;
                    self.write_expr(matcher, 0)?;
                    writeln!(self.fmt, ":")?;
                    self.write_seq(body, depth + 2)?;
                }
                if let Some(default_body) = default {
                    self.write_indent(depth + 1)?;
                    writeln!(self.fmt, "default:")?;
                    self.write_seq(default_body, depth + 2)?;
                }
                self.write_indent(depth)?;
                write!(self.fmt, "}}")?;
            }
            Expr::Goto(jump, _) => write!(self.fmt, "goto {}", jump.position)?,
            Expr::If(condition, true_, false_, _) => {
                write!(self.fmt, "if ")?;
                self.write_expr(condition, 0)?;
                writeln!(self.fmt, " {{")?;
                self.write_seq(true_, depth + 1)?;
                self.write_indent(depth)?;
                write!(self.fmt, "}}")?;
                if let Some(branch) = false_ {
                    writeln!(self.fmt, " else {{")?;
                    self.write_seq(branch, depth + 1)?;
                    self.write_indent(depth)?;
                    write!(self.fmt, "}}")?;
                }
            }
            Expr::Conditional(condition, true_, false_, _) => {
                self.write_expr(condition, 0)?;
                write!(self.fmt, " ? ")?;
                self.write_expr(true_, 0)?;
                write!(self.fmt, " : ")?;
                self.write_expr(false_, 0)?;
            }
            Expr::While(condition, body, _) => {
                write!(self.fmt, "while ")?;
                self.write_expr(condition, 0)?;
                writeln!(self.fmt, " {{")?;
                self.write_seq(body, depth + 1)?;
                self.write_indent(depth)?;
                write!(self.fmt, "}}")?;
            }
            Expr::Member(expr, accessor, _) => {
                self.write_expr(expr, 0)?;
                write!(self.fmt, ".{}", accessor)?;
            }
            Expr::BinOp(lhs, rhs, op, _) => {
                self.write_binop(lhs, rhs, *op)?;
            }
            Expr::UnOp(val, op, _) => {
                self.write_unop(val, *op)?;
            }
            Expr::Break(_) => write!(self.fmt, "break")?,
            Expr::Null(_) => write!(self.fmt, "null")?,
            Expr::This(_) => write!(self.fmt, "this")?,
            Expr::Super(_) => write!(self.fmt, "super")?,
            _ => panic!("Shouldn't get here"),
        };
        Ok(())
    }

    fn write_call(
        &mut self,
        name: &Ident,
        type_params: &[TypeName],
        params: &[Expr<RawAst>],
        parent_op: Option<ParentOp>,
    ) -> Result<(), Error> {
        let fun_name = name.as_ref().split(';').next().expect("Empty function name");

        if let Ok(binop) = BinOp::from_str(fun_name) {
            if parent_op
                .filter(|op| match op {
                    ParentOp::UnOp(_) | ParentOp::Dot => true,
                    ParentOp::BinOp(op) => !binop.does_associate(*op),
                })
                .is_some()
            {
                write!(self.fmt, "(")?;
                self.write_binop(&params[0], &params[1], binop)?;
                write!(self.fmt, ")")?;
                Ok(())
            } else {
                self.write_binop(&params[0], &params[1], binop)
            }
        } else if let Ok(unop) = UnOp::from_str(fun_name) {
            self.write_unop(&params[0], unop)
        } else if (fun_name == "WeakRefToRef" || fun_name == "RefToWeakRef" || fun_name == "AsRef")
            && matches!(self.mode, OutputMode::Code { verbose: false })
        {
            self.write_expr(&params[0], 0)
        } else {
            write!(self.fmt, "{}", fun_name)?;
            if !type_params.is_empty() {
                write!(self.fmt, "<{}>", type_params.iter().format(", "))?;
            }
            write!(self.fmt, "(")?;
            if !params.is_empty() {
                for param in params.iter().take(params.len() - 1) {
                    self.write_expr(param, 0)?;
                    write!(self.fmt, ", ")?;
                }
                self.write_expr(params.last().unwrap(), 0)?;
            }
            write!(self.fmt, ")")?;
            Ok(())
        }
    }

    fn write_binop(&mut self, lhs: &Expr<RawAst>, rhs: &Expr<RawAst>, op: BinOp) -> Result<(), Error> {
        self.write_expr_nested(lhs, Some(ParentOp::BinOp(op)), 0)?;
        write!(self.fmt, " {} ", format_binop(op))?;
        self.write_expr_nested(rhs, Some(ParentOp::BinOp(op)), 0)
    }

    fn write_unop(&mut self, param: &Expr<RawAst>, op: UnOp) -> Result<(), Error> {
        write!(self.fmt, "{}", format_unop(op))?;
        self.write_expr_nested(param, Some(ParentOp::UnOp(op)), 0)
    }

    fn write_indent(&mut self, depth: usize) -> Result<(), Error> {
        const INDENT_CHAR: char = ' ';
        const INDENT_SIZE: usize = 2;

        if depth > 0 {
            write!(self.fmt, "{0:1$}", INDENT_CHAR, INDENT_SIZE * depth)?;
        }
        Ok(())
    }
}

fn format_param(def: &Definition, pool: &ConstantPool) -> Result<String, Error> {
    let param = def.value.as_parameter().expect("Expected a param definition");
    let type_name = format_type(pool.definition(param.type_)?, pool)?;
    let name = pool.names.get(def.name)?;
    let out = if param.flags.is_out() { "out " } else { "" };
    let optional = if param.flags.is_optional() { "opt " } else { "" };
    let const_ = if param.flags.is_const() { "const " } else { "" };
    Ok(format!("{const_}{out}{optional}{name}: {type_name}"))
}

fn format_type(def: &Definition, pool: &ConstantPool) -> Result<String, Error> {
    let type_ = def.value.as_type().expect("Expected a type definition");
    let result = match type_ {
        Type::Prim | Type::Class => pool.names.get(def.name)?.to_string(),
        Type::Ref(nested) => format!("ref<{}>", format_type(pool.definition(*nested)?, pool)?),
        Type::WeakRef(nested) => format!("wref<{}>", format_type(pool.definition(*nested)?, pool)?),
        Type::Array(nested) => format!("array<{}>", format_type(pool.definition(*nested)?, pool)?),
        Type::StaticArray(nested, size) => {
            format!("array<{}; {size}>", format_type(pool.definition(*nested)?, pool)?)
        }
        Type::ScriptRef(nested) => format!("script_ref<{}>", format_type(pool.definition(*nested)?, pool)?),
    };
    Ok(result)
}

fn format_binop(op: BinOp) -> &'static str {
    match op {
        BinOp::AssignAdd => "+=",
        BinOp::AssignSubtract => "-=",
        BinOp::AssignMultiply => "*=",
        BinOp::AssignDivide => "/=",
        BinOp::AssignOr => "|=",
        BinOp::AssignAnd => "&=",
        BinOp::LogicOr => "||",
        BinOp::LogicAnd => "&&",
        BinOp::Or => "|",
        BinOp::Xor => "^",
        BinOp::Equal => "==",
        BinOp::NotEqual => "!=",
        BinOp::And => "&",
        BinOp::Less => "<",
        BinOp::LessEqual => "<=",
        BinOp::Greater => ">",
        BinOp::GreaterEqual => ">=",
        BinOp::Add => "+",
        BinOp::Subtract => "-",
        BinOp::Multiply => "*",
        BinOp::Divide => "/",
        BinOp::Modulo => "%",
    }
}

fn format_unop(op: UnOp) -> &'static str {
    match op {
        UnOp::BitNot => "~",
        UnOp::LogicNot => "!",
        UnOp::Neg => "-",
    }
}

enum ParentOp {
    UnOp(UnOp),
    BinOp(BinOp),
    Dot,
}
