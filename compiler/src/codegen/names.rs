use std::fmt::Display;

use redscript::{str_fmt, Str};

pub fn shadowed(id: impl Display) -> Str {
    str_fmt!("shadowed${id}")
}

pub fn lambda(id: impl Display) -> Str {
    str_fmt!("lambda${id}")
}

pub fn local(id: impl Display) -> Str {
    str_fmt!("local${id}")
}

pub fn param(id: impl Display) -> Str {
    str_fmt!("param${id}")
}

pub fn field(id: impl Display) -> Str {
    str_fmt!("field${id}")
}
