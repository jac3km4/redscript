//! REDmod looks up scriptable systems by their bare class name:
//! `container.Get(n"MyMod.Runtime.MySystem")` -> `container.Get(n"MySystem")`.

use std::borrow::Cow;

use redscript_ast::{Constant, Expr, SourceModule};

use crate::Cx;
use crate::walk::{MutVisitor, SExpr, for_each_function, walk_body, walk_expr};

pub fn run<'src>(module: &mut SourceModule<'src>, cx: &mut Cx<'_, 'src>) {
    for_each_function(module, |_, func| {
        if let Some(body) = &mut func.body {
            walk_body(&mut SystemNames { cx: &mut *cx }, body);
        }
    });
}

struct SystemNames<'a, 'b, 'src> {
    cx: &'a mut Cx<'b, 'src>,
}

impl<'src> MutVisitor<'src> for SystemNames<'_, '_, 'src> {
    fn visit_expr(&mut self, expr: &mut SExpr<'src>) {
        walk_expr(self, expr);
        let Expr::Call {
            expr: callee, args, ..
        } = &mut expr.0
        else {
            return;
        };
        if !matches!(&callee.0, Expr::Member { member: "Get", .. }) {
            return;
        }
        let [(Expr::Constant(Constant::CName(name)), span)] = &mut args[..] else {
            return;
        };
        let Some((_, bare)) = name.rsplit_once('.') else {
            return;
        };
        let bare = bare.to_owned();
        self.cx.info(
            *span,
            "system-name",
            format!(
                "REDmod looks up scriptable systems by bare class name, `{name}` was shortened \
                 to `{bare}`"
            ),
        );
        *name = Cow::Owned(bare);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_util::{codes, squash, to_redmod};

    #[test]
    fn qualified_system_names_are_shortened() {
        let (out, diags) = to_redmod(
            "func F(game: GameInstance) {\n  let s: ref<Sys> = GameInstance.GetScriptableSystemsContainer(game).Get(n\"MyMod.Runtime.Sys\") as Sys;\n  let t = GameInstance.GetScriptableSystemsContainer(game).Get(n\"Plain\");\n}\n",
            run,
        );
        let out = squash(&out);
        assert!(out.contains(".Get(n\"Sys\") as Sys"), "{out}");
        assert!(out.contains(".Get(n\"Plain\")"), "{out}");
        assert_eq!(codes(&diags), ["system-name"]);
    }
}
