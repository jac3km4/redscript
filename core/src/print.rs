use std::io::Write;
use std::ops::Deref;
use std::rc::Rc;

use crate::ast::{BinOp, Expr, Ident, Seq, SwitchCase};
use crate::bundle::ConstantPool;
use crate::decompiler::Decompiler;
use crate::definition::{Definition, DefinitionValue, Function, Type};
use crate::error::Error;

const INDENT: &str = "  ";

#[derive(Debug, Clone, Copy)]
pub enum OutputMode {
    Code,
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
            write!(out, "{} ", pool.name(definition.name)?)?;
            if !class.base.is_undefined() {
                write!(out, "extends {} ", pool.definition_name(class.base)?)?;
            }
            write!(out, "{{\n")?;

            for field_index in &class.fields {
                let field = pool.definition(*field_index)?;
                write_definition(out, field, pool, depth + 1, mode)?;
            }

            for method_index in &class.functions {
                let method = pool.definition(*method_index)?;
                if let Err(err) = write_definition(out, method, pool, depth + 1, mode) {
                    println!("Method decompilation {:?} failed due to: {:?}", method_index, err)
                }
            }
            writeln!(out, "}}")?
        }
        DefinitionValue::EnumValue(val) => {
            let name = if definition.name.is_undefined() {
                Rc::new("Undefined".to_owned())
            } else {
                pool.name(definition.name)?
            };
            writeln!(out, "{}{} = {},", padding, name, val)?
        }
        DefinitionValue::Enum(enum_) => {
            writeln!(out)?;
            writeln!(out, "enum {} {{", pool.name(definition.name)?)?;

            for member in &enum_.members {
                write_definition(out, pool.definition(*member)?, pool, depth + 1, mode)?;
            }

            writeln!(out, "}}")?
        }
        DefinitionValue::Function(fun) => {
            let return_type = fun
                .return_type
                .map(|idx| format_type(pool.definition(idx).unwrap(), pool).unwrap())
                .unwrap_or("void".to_owned());

            let name = pool.name(definition.name)?;
            let pretty_name = name.split(";").next().expect("Function with empty name");

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
            write!(out, "{} {}({})", return_type, pretty_name, params)?;

            if fun.flags.has_body() {
                write_function_body(out, fun, pool, depth, mode)?;
            }
            write!(out, "\n")?;
        }
        DefinitionValue::Parameter(_) => write!(out, "{}", format_param(definition, pool)?)?,
        DefinitionValue::Local(local) => {
            let type_name = format_type(pool.definition(local.type_)?, pool)?;
            let name = pool.name(definition.name)?;
            write!(out, "{}", padding)?;
            if local.flags.is_const() {
                write!(out, "const ")?;
            }
            write!(out, "{} {};", type_name, name)?
        }
        DefinitionValue::Field(field) => {
            let type_name = format_type(pool.definition(field.type_)?, pool)?;
            let field_name = pool.name(definition.name)?;

            writeln!(out)?;
            for property in &field.attributes {
                writeln!(out, "{}[Attrib({}, \"{}\")]", padding, property.name, property.value)?;
            }

            for property in &field.defaults {
                writeln!(out, "{}[Default({}, {}))]", padding, property.name, property.value)?;
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
            writeln!(out, "{} {};", type_name, field_name)?
        }
        DefinitionValue::SourceFile(_) => panic!(),
    }
    Ok(())
}

fn write_function_body<W: Write>(
    out: &mut W,
    fun: &Function,
    pool: &ConstantPool,
    indent: usize,
    mode: OutputMode,
) -> Result<(), Error> {
    write!(out, " {{\n")?;
    for local in &fun.locals {
        write_definition(out, pool.definition(*local)?, pool, indent + 1, mode)?;
        write!(out, "\n")?;
    }
    match mode {
        OutputMode::Code => {
            let code = Decompiler::new(&mut fun.code.cursor(), pool).decompile()?;
            write_seq(out, &code, indent + 1)?;
        }
        OutputMode::SyntaxTree => {
            let code = Decompiler::new(&mut fun.code.cursor(), pool).decompile()?;
            for expr in code.exprs {
                writeln!(out, "{}{:?}", INDENT.repeat(indent + 1), expr)?;
            }
        }
        OutputMode::Bytecode => {
            for (offset, instr) in fun.code.cursor() {
                let op = format!("{:?}", instr).to_lowercase();
                writeln!(out, "{}{}: {}", INDENT.repeat(indent + 1), offset.value, op)?;
            }
        }
    }

    write!(out, "{}}}", INDENT.repeat(indent))?;
    Ok(())
}

fn write_seq<W: Write>(out: &mut W, code: &Seq, indent: usize) -> Result<(), Error> {
    for expr in code.exprs.iter().filter(|expr| !expr.is_empty()) {
        write!(out, "{}", INDENT.repeat(indent))?;
        write_expr(out, &expr, indent)?;
        write!(out, ";\n")?;
    }
    Ok(())
}

fn write_expr<W: Write>(out: &mut W, expr: &Expr, indent: usize) -> Result<(), Error> {
    let padding = INDENT.repeat(indent);

    match expr {
        Expr::Ident(ident) => write!(out, "{}", ident.0)?,
        Expr::StringLit(str) => write!(out, "\"{}\"", str)?,
        Expr::NumLit(lit) => write!(out, "{}", lit)?,
        Expr::Assign(lhs, rhs) => {
            write_expr(out, lhs, 0)?;
            write!(out, " = ")?;
            write_expr(out, rhs, 0)?
        }
        Expr::Call(fun, params) => write_call(out, fun, params)?,
        Expr::ArrayElem(arr, idx) => {
            write_expr(out, arr, 0)?;
            write!(out, "[")?;
            write_expr(out, idx, 0)?;
            write!(out, "]")?;
        }
        Expr::New(ident, params) => {
            write!(out, "new {}(", ident.0)?;
            if !params.is_empty() {
                for param in params.iter().take(params.len() - 1) {
                    write_expr(out, param, indent)?;
                    write!(out, ",")?;
                }
                write_expr(out, params.last().unwrap(), indent)?;
            }
            write!(out, ")")?
        }
        Expr::Return(Some(expr)) => {
            write!(out, "return ")?;
            write_expr(out, expr, indent)?
        }
        Expr::Return(None) => write!(out, "return")?,
        Expr::Seq(exprs) => write_seq(out, exprs, indent)?,
        Expr::Switch(expr, cases, default) => {
            write!(out, "switch(")?;
            write_expr(out, expr, 0)?;
            write!(out, ") {{\n")?;
            for SwitchCase(matcher, body) in cases {
                write!(out, "{}  case ", padding)?;
                write_expr(out, matcher, 0)?;
                write!(out, ":\n")?;
                write_seq(out, body, indent + 2)?;
            }
            if let Some(default_body) = default {
                write!(out, "{}  default:\n", padding)?;
                write_seq(out, default_body, indent + 2)?;
            }
            write!(out, "{}}}", padding)?
        }
        Expr::Goto(jump) if !jump.resolved => write!(out, "goto {}", jump.position)?,
        Expr::Goto(_) => (),
        Expr::If(condition, true_, false_) => {
            write!(out, "if(")?;
            write_expr(out, condition, 0)?;
            write!(out, ") {{\n")?;
            write_seq(out, true_, indent + 1)?;
            write!(out, "{}}}", padding)?;
            if let Some(branch) = false_ {
                write!(out, " else {{\n")?;
                write_seq(out, branch, indent + 1)?;
                write!(out, "{}}}", padding)?
            }
        }
        Expr::Conditional(condition, true_, false_) => {
            write_expr(out, condition, 0)?;
            write!(out, " ? ")?;
            write_expr(out, true_, 0)?;
            write!(out, " : ")?;
            write_expr(out, false_, 0)?;
        }
        Expr::While(condition, body) => {
            write!(out, "while(")?;
            write_expr(out, condition, 0)?;
            write!(out, ") {{\n")?;
            write_seq(out, body, indent + 1)?;
            write!(out, "{}}}", padding)?;
        }
        Expr::Member(expr, accessor) => {
            // remove intermediate .this
            if let Expr::Member(lhs, rhs) = accessor.deref() {
                if let Expr::This = lhs.deref() {
                    write_expr(out, expr, 0)?;
                    write!(out, ".")?;
                    write_expr(out, rhs, 0)?;
                    return Ok(());
                }
            }
            write_expr(out, expr, 0)?;
            write!(out, ".")?;
            write_expr(out, accessor, 0)?;
        }
        Expr::BinOp(lhs, rhs, op) => {
            write_expr(out, lhs, 0)?;
            write!(out, " {} ", format_op(op))?;
            write_expr(out, rhs, 0)?;
        }
        Expr::Break => write!(out, "break")?,
        Expr::True => write!(out, "true")?,
        Expr::False => write!(out, "false")?,
        Expr::Null => write!(out, "null")?,
        Expr::This => write!(out, "this")?,
    };
    Ok(())
}

fn write_call<W: Write>(out: &mut W, name: &Ident, params: &Vec<Expr>) -> Result<(), Error> {
    let extracted = name.0.split(";").next().expect("Empty function name");
    let fun_name = if extracted.is_empty() { "undefined" } else { extracted };
    match fun_name {
        "OperatorLogicOr" => write_binop(out, &params[0], &params[1], "||"),
        "OperatorLogicAnd" => write_binop(out, &params[0], &params[1], "&&"),
        "OperatorOr" => write_binop(out, &params[0], &params[1], "|"),
        "OperatorAnd" => write_binop(out, &params[0], &params[1], "&"),
        "OperatorXor" => write_binop(out, &params[0], &params[1], "^"),
        "OperatorEqual" => write_binop(out, &params[0], &params[1], "=="),
        "OperatorNotEqual" => write_binop(out, &params[0], &params[1], "!="),
        "OperatorGreater" => write_binop(out, &params[0], &params[1], ">"),
        "OperatorLess" => write_binop(out, &params[0], &params[1], "<"),
        "OperatorAdd" => write_binop(out, &params[0], &params[1], "+"),
        "OperatorSubtract" => write_binop(out, &params[0], &params[1], "-"),
        "OperatorDivide" => write_binop(out, &params[0], &params[1], "/"),
        "OperatorMultiply" => write_binop(out, &params[0], &params[1], "*"),
        "OperatorModulo" => write_binop(out, &params[0], &params[1], "%"),
        "OperatorGreaterEqual" => write_binop(out, &params[0], &params[1], ">="),
        "OperatorLessEqual" => write_binop(out, &params[0], &params[1], "<="),
        "OperatorAssignAdd" => write_binop(out, &params[0], &params[1], "+="),
        "OperatorAssignSubtract" => write_binop(out, &params[0], &params[1], "-="),
        "OperatorAssignMultiply" => write_binop(out, &params[0], &params[1], "*="),
        "OperatorAssignDivide" => write_binop(out, &params[0], &params[1], "/="),
        "OperatorLogicNot" => write_unop(out, &params[0], "!"),
        "OperatorBitNot" => write_unop(out, &params[0], "~"),
        "OperatorNeg" => write_unop(out, &params[0], "-"),
        _ => {
            write!(out, "{}(", fun_name)?;
            if !params.is_empty() {
                for param in params.iter().take(params.len() - 1) {
                    write_expr(out, param, 0)?;
                    write!(out, ", ")?;
                }
                write_expr(out, params.last().unwrap(), 0)?;
            }
            write!(out, ")")?;
            Ok(())
        }
    }
}

fn write_binop<W: Write>(out: &mut W, lhs: &Expr, rhs: &Expr, op: &str) -> Result<(), Error> {
    write_expr(out, lhs, 0)?;
    write!(out, " {} ", op)?;
    write_expr(out, rhs, 0)
}

fn write_unop<W: Write>(out: &mut W, param: &Expr, op: &str) -> Result<(), Error> {
    write!(out, "{}", op)?;
    write_expr(out, param, 0)
}

fn format_param(def: &Definition, pool: &ConstantPool) -> Result<String, Error> {
    if let DefinitionValue::Parameter(ref param) = def.value {
        let type_name = format_type(pool.definition(param.type_)?, pool)?;
        let name = pool.name(def.name)?;
        let qualifier = if param.flags.is_out_param() { "out " } else { "" };
        let optional = if param.flags.is_optional() { "?" } else { "" };
        Ok(format!("{}{} {}{}", qualifier, type_name, name, optional))
    } else {
        Err(Error::DecompileError("Invalid type definition received".to_owned()))
    }
}

fn format_type(def: &Definition, pool: &ConstantPool) -> Result<String, Error> {
    if let DefinitionValue::Type(ref type_) = def.value {
        let result = match type_ {
            Type::Prim => pool.name(def.name)?.to_lowercase(),
            Type::Class => pool.name(def.name)?.deref().clone(),
            Type::Handle(nested) => format!("Handle<{}>", format_type(pool.definition(*nested)?, pool)?),
            Type::WeakHandle(nested) => format!("WHandle<{}>", format_type(pool.definition(*nested)?, pool)?),
            Type::Array(nested) => format!("Array<{}>", format_type(pool.definition(*nested)?, pool)?),
            Type::StaticArray(nested, size) => {
                format!("Array<{}; {}>", format_type(pool.definition(*nested)?, pool)?, size)
            }
            Type::ScriptRef(nested) => format!("ScriptRef<{}>", format_type(pool.definition(*nested)?, pool)?),
        };
        Ok(result)
    } else {
        Err(Error::DecompileError("Invalid type definition received".to_owned()))
    }
}

fn format_op(op: &BinOp) -> String {
    match op {
        BinOp::Eq => "==".to_owned(),
        BinOp::Neq => "!=".to_owned(),
    }
}
