use std::io::Write;
use std::rc::Rc;

use redscript::ast::{BinOp, Constant, Expr, Ident, Literal, Seq, Source, SwitchCase, UnOp};
use redscript::bundle::ConstantPool;
use redscript::definition::{Definition, DefinitionValue, Function, Type};
use redscript::error::Error;

use crate::Decompiler;

const INDENT: &str = "  ";

#[derive(Debug, Clone, Copy)]
pub enum OutputMode {
    Code { verbose: bool },
    SyntaxTree,
    Bytecode,
}

pub fn write_definition<W: Write>(
    out: &mut W,
    definition: &Definition,
    pool: &ConstantPool,
    depth: usize,
    mode: OutputMode,
) -> Result<(), Error> {
    let padding = INDENT.repeat(depth);

    match &definition.value {
        DefinitionValue::Type(_) => write!(out, "{}", format_type(definition, pool)?)?,
        DefinitionValue::Class(class) => {
            writeln!(out)?;
            write!(out, "{} ", class.visibility)?;
            if class.flags.is_abstract() {
                write!(out, "abstract ")?;
            }
            if class.flags.is_final() {
                write!(out, "final ")?;
            }
            if class.flags.is_native() {
                write!(out, "native ")?;
            }
            if class.flags.is_struct() {
                write!(out, "struct ")?;
            } else {
                write!(out, "class ")?;
            }
            write!(out, "{} ", pool.names.get(definition.name)?)?;
            if !class.base.is_undefined() {
                write!(out, "extends {} ", pool.definition_name(class.base)?)?;
            }
            writeln!(out, "{{")?;

            for field_index in &class.fields {
                let field = pool.definition(*field_index)?;
                write_definition(out, field, pool, depth + 1, mode)?;
            }

            for method_index in &class.functions {
                let method = pool.definition(*method_index)?;
                if let Err(err) = write_definition(out, method, pool, depth + 1, mode) {
                    log::error!("Method decompilation {:?} failed due to: {:?}", method_index, err)
                }
            }
            writeln!(out, "}}")?
        }
        DefinitionValue::EnumValue(val) => {
            let name = if definition.name.is_undefined() {
                Rc::new("Undefined".to_owned())
            } else {
                pool.names.get(definition.name)?
            };
            writeln!(out, "{}{} = {},", padding, name, val)?
        }
        DefinitionValue::Enum(enum_) => {
            writeln!(out)?;
            writeln!(out, "enum {} {{", pool.names.get(definition.name)?)?;

            for member in &enum_.members {
                write_definition(out, pool.definition(*member)?, pool, depth + 1, mode)?;
            }

            writeln!(out, "}}")?
        }
        DefinitionValue::Function(fun) => {
            let return_type = fun
                .return_type
                .map(|idx| format_type(pool.definition(idx).unwrap(), pool).unwrap())
                .unwrap_or_else(|| "Void".to_owned());

            let name = pool.names.get(definition.name)?;
            let pretty_name = name.split(';').next().expect("Function with empty name");

            let params = fun
                .parameters
                .iter()
                .map(|param| format_param(pool.definition(*param).unwrap(), pool).unwrap())
                .collect::<Vec<_>>()
                .join(", ");

            writeln!(out)?;
            write!(out, "{}{} ", padding, fun.visibility)?;
            if fun.flags.is_final() {
                write!(out, "final ")?;
            }
            if fun.flags.is_static() {
                write!(out, "static ")?;
            }
            if fun.flags.is_native() {
                write!(out, "native ")?;
            }
            if fun.flags.is_exec() {
                write!(out, "exec ")?;
            }
            if fun.flags.is_const() {
                write!(out, "const ")?;
            }
            if fun.flags.is_callback() {
                write!(out, "cb ")?;
            }
            write!(out, "func {}({}) -> {}", pretty_name, params, return_type)?;

            if fun.flags.has_body() {
                write_function_body(out, fun, pool, depth, mode)?;
            }
            writeln!(out)?;
        }
        DefinitionValue::Parameter(_) => write!(out, "{}", format_param(definition, pool)?)?,
        DefinitionValue::Local(local) => {
            let type_name = format_type(pool.definition(local.type_)?, pool)?;
            let name = pool.names.get(definition.name)?;
            write!(out, "{}", padding)?;
            if local.flags.is_const() {
                write!(out, "const ")?;
            } else {
                write!(out, "let ")?;
            }
            write!(out, "{}: {};", name, type_name)?
        }
        DefinitionValue::Field(field) => {
            let type_name = format_type(pool.definition(field.type_)?, pool)?;
            let field_name = pool.names.get(definition.name)?;

            writeln!(out)?;
            for property in &field.attributes {
                writeln!(out, "{}@attrib({}, \"{}\")", padding, property.name, property.value)?;
            }

            for property in &field.defaults {
                writeln!(out, "{}@default({}, {})", padding, property.name, property.value)?;
            }

            write!(out, "{}{} ", padding, field.visibility)?;
            if field.flags.is_inline() {
                write!(out, "inline ")?;
            }
            if field.flags.is_rep() {
                write!(out, "rep ")?;
            }
            if field.flags.is_edit() {
                write!(out, "edit ")?;
            }
            if field.flags.is_native() {
                write!(out, "native ")?;
            }
            if field.flags.is_persistent() {
                write!(out, "persistent ")?;
            }
            if field.flags.is_const() {
                write!(out, "const ")?;
            }
            writeln!(out, "let {}: {};", field_name, type_name)?
        }
        DefinitionValue::SourceFile(_) => panic!(),
    }
    Ok(())
}

fn write_function_body<W: Write>(
    out: &mut W,
    fun: &Function,
    pool: &ConstantPool,
    depth: usize,
    mode: OutputMode,
) -> Result<(), Error> {
    writeln!(out, " {{")?;
    for local in &fun.locals {
        write_definition(out, pool.definition(*local)?, pool, depth + 1, mode)?;
        writeln!(out)?;
    }
    match mode {
        OutputMode::Code { verbose } => {
            let code = Decompiler::new(&mut fun.code.cursor(), pool).decompile()?;
            write_seq(out, &code, verbose, depth + 1)?;
        }
        OutputMode::SyntaxTree => {
            let code = Decompiler::new(&mut fun.code.cursor(), pool).decompile()?;
            for expr in code.exprs {
                writeln!(out, "{}{:?}", INDENT.repeat(depth + 1), expr)?;
            }
        }
        OutputMode::Bytecode => {
            for (offset, instr) in fun.code.cursor() {
                let op = format!("{:?}", instr).to_lowercase();
                writeln!(out, "{}{}: {}", INDENT.repeat(depth + 1), offset.value, op)?;
            }
        }
    }

    write!(out, "{}}}", INDENT.repeat(depth))?;
    Ok(())
}

fn write_seq<W: Write>(out: &mut W, code: &Seq<Source>, verbose: bool, depth: usize) -> Result<(), Error> {
    for expr in code.exprs.iter().filter(|expr| !expr.is_empty()) {
        write!(out, "{}", INDENT.repeat(depth))?;
        write_expr(out, &expr, verbose, depth)?;
        writeln!(out, ";")?;
    }
    Ok(())
}

fn write_expr<W: Write>(out: &mut W, expr: &Expr<Source>, verbose: bool, depth: usize) -> Result<(), Error> {
    let padding = INDENT.repeat(depth);

    match expr {
        Expr::Ident(ident, _) => write!(out, "{}", ident)?,
        Expr::Constant(cons, _) => match cons {
            Constant::String(Literal::String, str) => write!(out, "\"{}\"", str)?,
            Constant::String(Literal::Name, str) => write!(out, "n\"{}\"", str)?,
            Constant::String(Literal::Resource, str) => write!(out, "r\"{}\"", str)?,
            Constant::String(Literal::TweakDbId, str) => write!(out, "t\"{}\"", str)?,
            Constant::Int(lit) => write!(out, "{}", lit)?,
            Constant::Uint(lit) => write!(out, "{}", lit)?,
            Constant::Float(lit) => write!(out, "{:.2}", lit)?,
            Constant::Bool(true) => write!(out, "true")?,
            Constant::Bool(false) => write!(out, "false")?,
        },
        Expr::Cast(type_, expr, _) => {
            write!(out, "(")?;
            write_expr(out, expr, verbose, 0)?;
            write!(out, " as {})", type_.repr())?;
        }
        Expr::Declare(_, _, _, _) => {}
        Expr::Assign(lhs, rhs, _) => {
            write_expr(out, lhs, verbose, 0)?;
            write!(out, " = ")?;
            write_expr(out, rhs, verbose, 0)?
        }
        Expr::Call(fun, params, _) => write_call(out, fun, params, verbose)?,
        Expr::MethodCall(obj, fun, params, _) => {
            write_expr(out, obj, verbose, 0)?;
            write!(out, ".")?;
            write_call(out, fun, params, verbose)?
        }
        Expr::ArrayElem(arr, idx, _) => {
            write_expr(out, arr, verbose, 0)?;
            write!(out, "[")?;
            write_expr(out, idx, verbose, 0)?;
            write!(out, "]")?;
        }
        Expr::New(ident, params, _) => {
            write!(out, "new {}(", ident)?;
            if !params.is_empty() {
                for param in params.iter().take(params.len() - 1) {
                    write_expr(out, param, verbose, depth)?;
                    write!(out, ", ")?;
                }
                write_expr(out, params.last().unwrap(), verbose, depth)?;
            }
            write!(out, ")")?
        }
        Expr::Return(Some(expr), _) => {
            write!(out, "return ")?;
            write_expr(out, expr, verbose, depth)?
        }
        Expr::Return(None, _) => write!(out, "return")?,
        Expr::Seq(exprs) => write_seq(out, exprs, verbose, depth)?,
        Expr::Switch(expr, cases, default) => {
            write!(out, "switch ")?;
            write_expr(out, expr, verbose, 0)?;
            writeln!(out, " {{")?;
            for SwitchCase { matcher, body } in cases {
                write!(out, "{}  case ", padding)?;
                write_expr(out, matcher, verbose, 0)?;
                writeln!(out, ":")?;
                write_seq(out, body, verbose, depth + 2)?;
            }
            if let Some(default_body) = default {
                writeln!(out, "{}  default:", padding)?;
                write_seq(out, default_body, verbose, depth + 2)?;
            }
            write!(out, "{}}}", padding)?
        }
        Expr::Goto(jump, _) if !jump.resolved => write!(out, "goto {}", jump.position)?,
        Expr::Goto(_, _) => (),
        Expr::If(condition, true_, false_, _) => {
            write!(out, "if ")?;
            write_expr(out, condition, verbose, 0)?;
            writeln!(out, " {{")?;
            write_seq(out, true_, verbose, depth + 1)?;
            write!(out, "{}}}", padding)?;
            if let Some(branch) = false_ {
                writeln!(out, " else {{")?;
                write_seq(out, branch, verbose, depth + 1)?;
                write!(out, "{}}}", padding)?
            }
        }
        Expr::Conditional(condition, true_, false_, _) => {
            write_expr(out, condition, verbose, 0)?;
            write!(out, " ? ")?;
            write_expr(out, true_, verbose, 0)?;
            write!(out, " : ")?;
            write_expr(out, false_, verbose, 0)?;
        }
        Expr::While(condition, body, _) => {
            write!(out, "while ")?;
            write_expr(out, condition, verbose, 0)?;
            writeln!(out, " {{")?;
            write_seq(out, body, verbose, depth + 1)?;
            write!(out, "{}}}", padding)?;
        }
        Expr::Member(expr, accessor, _) => {
            write_expr(out, expr, verbose, 0)?;
            write!(out, ".{}", accessor)?;
        }
        Expr::BinOp(lhs, rhs, op, _) => {
            write_binop(out, lhs, rhs, *op, verbose)?;
        }
        Expr::UnOp(val, op, _) => {
            write_unop(out, val, *op, verbose)?;
        }
        Expr::Break(_) => write!(out, "break")?,
        Expr::Null => write!(out, "null")?,
        Expr::This(_) => write!(out, "this")?,
        Expr::Super(_) => write!(out, "super")?,
    };
    Ok(())
}

fn write_call<W: Write>(out: &mut W, name: &Ident, params: &[Expr<Source>], verbose: bool) -> Result<(), Error> {
    let extracted = name.as_ref().split(';').next().expect("Empty function name");
    let fun_name = if extracted.is_empty() { "undefined" } else { extracted };
    match fun_name {
        "OperatorLogicOr" => write_binop(out, &params[0], &params[1], BinOp::LogicOr, verbose),
        "OperatorLogicAnd" => write_binop(out, &params[0], &params[1], BinOp::LogicAnd, verbose),
        "OperatorOr" => write_binop(out, &params[0], &params[1], BinOp::Or, verbose),
        "OperatorAnd" => write_binop(out, &params[0], &params[1], BinOp::And, verbose),
        "OperatorXor" => write_binop(out, &params[0], &params[1], BinOp::Xor, verbose),
        "OperatorEqual" => write_binop(out, &params[0], &params[1], BinOp::Equal, verbose),
        "OperatorNotEqual" => write_binop(out, &params[0], &params[1], BinOp::NotEqual, verbose),
        "OperatorGreater" => write_binop(out, &params[0], &params[1], BinOp::Greater, verbose),
        "OperatorLess" => write_binop(out, &params[0], &params[1], BinOp::Less, verbose),
        "OperatorAdd" => write_binop(out, &params[0], &params[1], BinOp::Add, verbose),
        "OperatorSubtract" => write_binop(out, &params[0], &params[1], BinOp::Subtract, verbose),
        "OperatorDivide" => write_binop(out, &params[0], &params[1], BinOp::Divide, verbose),
        "OperatorMultiply" => write_binop(out, &params[0], &params[1], BinOp::Multiply, verbose),
        "OperatorModulo" => write_binop(out, &params[0], &params[1], BinOp::Modulo, verbose),
        "OperatorGreaterEqual" => write_binop(out, &params[0], &params[1], BinOp::GreaterEqual, verbose),
        "OperatorLessEqual" => write_binop(out, &params[0], &params[1], BinOp::Less, verbose),
        "OperatorAssignAdd" => write_binop(out, &params[0], &params[1], BinOp::AssignAdd, verbose),
        "OperatorAssignSubtract" => write_binop(out, &params[0], &params[1], BinOp::AssignSubtract, verbose),
        "OperatorAssignMultiply" => write_binop(out, &params[0], &params[1], BinOp::AssignMultiply, verbose),
        "OperatorAssignDivide" => write_binop(out, &params[0], &params[1], BinOp::AssignDivide, verbose),
        "OperatorLogicNot" => write_unop(out, &params[0], UnOp::LogicNot, verbose),
        "OperatorBitNot" => write_unop(out, &params[0], UnOp::BitNot, verbose),
        "OperatorNeg" => write_unop(out, &params[0], UnOp::Neg, verbose),
        "WeakRefToRef" if !verbose => write_expr(out, &params[0], verbose, 0),
        "RefToWeakRef" if !verbose => write_expr(out, &params[0], verbose, 0),
        _ => {
            write!(out, "{}(", fun_name)?;
            if !params.is_empty() {
                for param in params.iter().take(params.len() - 1) {
                    write_expr(out, param, verbose, 0)?;
                    write!(out, ", ")?;
                }
                write_expr(out, params.last().unwrap(), verbose, 0)?;
            }
            write!(out, ")")?;
            Ok(())
        }
    }
}

fn write_binop<W: Write>(
    out: &mut W,
    lhs: &Expr<Source>,
    rhs: &Expr<Source>,
    op: BinOp,
    verbose: bool,
) -> Result<(), Error> {
    write_expr(out, lhs, verbose, 0)?;
    write!(out, " {} ", format_binop(op))?;
    write_expr(out, rhs, verbose, 0)
}

fn write_unop<W: Write>(out: &mut W, param: &Expr<Source>, op: UnOp, verbose: bool) -> Result<(), Error> {
    write!(out, "{}", format_unop(op))?;
    write_expr(out, param, verbose, 0)
}

fn format_param(def: &Definition, pool: &ConstantPool) -> Result<String, Error> {
    if let DefinitionValue::Parameter(ref param) = def.value {
        let type_name = format_type(pool.definition(param.type_)?, pool)?;
        let name = pool.names.get(def.name)?;
        let out = if param.flags.is_out() { "out " } else { "" };
        let optional = if param.flags.is_optional() { "opt " } else { "" };
        Ok(format!("{}{}{}: {}", out, optional, name, type_name))
    } else {
        Err(Error::DecompileError("Invalid type definition received".to_owned()))
    }
}

fn format_type(def: &Definition, pool: &ConstantPool) -> Result<String, Error> {
    if let DefinitionValue::Type(ref type_) = def.value {
        let result = match type_ {
            Type::Prim => pool.names.get(def.name)?.to_string(),
            Type::Class => pool.names.get(def.name)?.to_string(),
            Type::Ref(nested) => format!("ref<{}>", format_type(pool.definition(*nested)?, pool)?),
            Type::WeakRef(nested) => format!("wref<{}>", format_type(pool.definition(*nested)?, pool)?),
            Type::Array(nested) => format!("array<{}>", format_type(pool.definition(*nested)?, pool)?),
            Type::StaticArray(nested, size) => {
                format!("array<{}; {}>", format_type(pool.definition(*nested)?, pool)?, size)
            }
            Type::ScriptRef(nested) => format!("script_ref<{}>", format_type(pool.definition(*nested)?, pool)?),
        };
        Ok(result)
    } else {
        Err(Error::DecompileError("Invalid type definition received".to_owned()))
    }
}

fn format_binop(op: BinOp) -> &'static str {
    match op {
        BinOp::AssignAdd => "+=",
        BinOp::AssignSubtract => "-=",
        BinOp::AssignMultiply => "*=",
        BinOp::AssignDivide => "/=",
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
