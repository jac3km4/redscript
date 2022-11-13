#[macro_export]
macro_rules! visit_expr {
    ($self:expr, $fun:ident, $expr:expr$(, $mut:ident)?) => {
        match $expr {
            Expr::ArrayLit(exprs, _, _) => {
                for expr in &$($mut)? exprs[..] {
                    $self.$fun(expr);
                }
            }
            Expr::InterpolatedString(_, parts, _) => {
                for (expr, _) in parts {
                    $self.$fun(expr);
                }
            }
            Expr::Declare(_, _, Some(expr), _) => {
                $self.$fun(expr);
            }
            Expr::DynCast(_, expr, _) => {
                $self.$fun(expr);
            }
            Expr::Assign(lhs, rhs, _, _) => {
                $self.$fun(lhs);
                $self.$fun(rhs);
            }
            Expr::Call(expr, _, _, args, _, _) => {
                $self.$fun(expr);
                for expr in &$($mut)? args[..] {
                    $self.$fun(expr);
                }
            }
            Expr::Lambda(_, body, _) => {
                $self.$fun(body);
            }
            Expr::Member(context, _, _) => {
                $self.$fun(context);
            }
            Expr::ArrayElem(expr, index, _, _) => {
                $self.$fun(expr);
                $self.$fun(index);
            }
            Expr::New(_, args, _) => {
                for expr in &$($mut)? args[..] {
                    $self.$fun(expr);
                }
            }
            Expr::Return(Some(expr), _) => {
                $self.$fun(expr);
            }
            Expr::Seq(seq) => {
                (&$($mut)? seq.exprs).into_iter().for_each(|e| $self.$fun(e));
            }
            Expr::Switch(matched, cases, default, _) => {
                $self.$fun(matched);
                for case in &$($mut)? cases[..] {
                    $self.$fun(&$($mut)? case.matcher);
                    (&$($mut)? case.body.exprs).into_iter().for_each(|e| $self.$fun(e));
                }
                if let Some(seq) = default {
                    (&$($mut)? seq.exprs).into_iter().for_each(|e| $self.$fun(e));
                }
            }
            Expr::If(cond, if_, else_, _) => {
                $self.$fun(cond);
                (&$($mut)? if_.exprs).into_iter().for_each(|e| $self.$fun(e));
                if let Some(seq) = else_ {
                    (&$($mut)? seq.exprs).into_iter().for_each(|e| $self.$fun(e));
                }
            }
            Expr::Conditional(cond, true_, false_, _) => {
                $self.$fun(cond);
                $self.$fun(true_);
                $self.$fun(false_);
            }
            Expr::While(cond, body, _) => {
                $self.$fun(cond);
                (&$($mut)? body.exprs).into_iter().for_each(|e| $self.$fun(e));
            }
            Expr::ForIn(_, array, body, _) => {
                $self.$fun(array);
                (&$($mut)? body.exprs).into_iter().for_each(|e| $self.$fun(e));
            }
            Expr::BinOp(lhs, rhs, _, _) => {
                $self.$fun(lhs);
                $self.$fun(rhs);
            }
            Expr::UnOp(expr, _, _) => {
                $self.$fun(expr);
            }
            _ => {}
        }
    };
}
