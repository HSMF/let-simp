use std::{collections::HashSet, fs::File, ops::Range, path::Path};

use crate::{
    BinopKind, Expr, ExprKind, pretty_print,
    visitor::{FoldExpr, Visitor, fold_expr},
};

#[derive(Debug, PartialEq, Eq)]
enum Value {
    Int(i128),
    Bool(bool),
}

struct Replace<'a> {
    old: &'a str,
    new: Expr,
}
impl FoldExpr for Replace<'_> {
    fn fold_var(&mut self, span: Range<usize>, name: String) -> Expr {
        if name == self.old {
            self.new.clone()
        } else {
            Expr {
                span,
                kind: ExprKind::Var(name),
            }
        }
    }
}

fn replace(e: Expr, old: &str, new: Expr) -> Expr {
    fold_expr(&mut Replace { old, new }, e)
}

#[derive(Default)]
struct UsedVars {
    v: HashSet<String>,
}
impl Visitor for UsedVars {
    type Output = ();

    fn visit_int(&mut self, _span: &Range<usize>, _value: i128) -> Self::Output {}

    fn visit_var(&mut self, _span: &Range<usize>, name: &str) -> Self::Output {
        self.v.insert(name.to_owned());
    }

    fn visit_call(&mut self, _span: &Range<usize>, _callee: &Expr, _args: &[Expr]) -> Self::Output {
    }

    fn visit_let(
        &mut self,
        _span: &Range<usize>,
        _name: &str,
        _value: &Expr,
        _body: &Expr,
    ) -> Self::Output {
    }

    fn visit_field_access(
        &mut self,
        _span: &Range<usize>,
        _object: &Expr,
        _field: &str,
    ) -> Self::Output {
    }

    fn visit_binop(
        &mut self,
        _span: &Range<usize>,
        _left: &Expr,
        _right: &Expr,
        _kind: BinopKind,
    ) -> Self::Output {
    }

    fn visit_ternary(
        &mut self,
        _span: &Range<usize>,
        _cond: &Expr,
        _if_true: &Expr,
        _if_false: &Expr,
    ) -> Self::Output {
    }
}
struct RemoveTrivialLets;
impl FoldExpr for RemoveTrivialLets {
    fn fold_let(&mut self, span: Range<usize>, name: String, value: Expr, body: Expr) -> Expr {
        if let ExprKind::Var(v) = &body.kind {
            if v == &name {
                return value;
            } else {
                return body;
            }
        }
        if let ExprKind::Var(v) = value.kind {
            replace(
                body,
                &name,
                Expr {
                    span: 0..0,
                    kind: ExprKind::Var(v),
                },
            )
        } else {
            Expr {
                span,
                kind: ExprKind::Let(name, Box::new(value), Box::new(body)),
            }
        }
    }
}

struct RemoveUnusedBindings;
impl FoldExpr for RemoveUnusedBindings {
    fn fold_let(&mut self, span: Range<usize>, name: String, value: Expr, body: Expr) -> Expr {
        let mut used = UsedVars::default();
        used.visit_expr(&body);
        if used.v.contains(&name) {
            let kind = ExprKind::Let(name, Box::new(value), Box::new(body));
            Expr { span, kind }
        } else {
            body
        }
    }
}

/// turns `let x = (let y = E1 in E2) in E3` into `let y = E1 in let x = E2 in E3`
/// requires all variable bindings to be unique
struct UnnestLetBindings;
impl FoldExpr for UnnestLetBindings {
    fn fold_let(&mut self, span: Range<usize>, x: String, value: Expr, e3: Expr) -> Expr {
        let mut x = x;
        let mut value = value;
        let mut e3 = e3;
        while let ExprKind::Let(y, e1, e2) = value.kind {
            e3 = self.fold_let(span.clone(), x, *e2, e3);
            value = *e1;
            x = y;
        }
        Expr {
            span,
            kind: ExprKind::Let(x, Box::new(value), Box::new(e3)),
        }
    }
}

struct InlineDefs<F> {
    f: F,
}
impl<F> InlineDefs<F> {
    pub fn new(f: F) -> Self {
        Self { f }
    }
}
impl<F> FoldExpr for InlineDefs<F>
where
    F: Fn(&str, &Expr) -> bool,
{
    fn fold_let(&mut self, span: Range<usize>, name: String, value: Expr, body: Expr) -> Expr {
        if (self.f)(&name, &value) {
            replace(body, &name, value)
        } else {
            Expr {
                span,
                kind: ExprKind::Let(name, Box::new(value), Box::new(body)),
            }
        }
    }
}

/// `s_4_Tuple_cons(a, b, c, d).s_4_Tuple_0` ==> `a`
struct SimplifyFieldAccess;
impl FoldExpr for SimplifyFieldAccess {
    fn fold_field_access(&mut self, span: Range<usize>, object: Expr, field: String) -> Expr {
        let Some(rest) = field.strip_prefix("s_4_Tuple_") else {
            return Expr::field_access(span, object, field);
        };
        let Ok(idx) = rest.parse::<usize>() else {
            return Expr::field_access(span, object, field);
        };
        let mut args = match object.kind {
            ExprKind::Call(
                box Expr {
                    kind: ExprKind::Var(tuple_cons),
                    ..
                },
                args,
            ) if tuple_cons == "s_4_Tuple_cons" => args,
            _ => {
                return Expr::field_access(span, object, field);
            }
        };

        args.swap_remove(idx)
    }
}

/// `f(f^-1(x))` ==> `x`
struct SimplifySelfInverse {
    invs: &'static [(&'static str, &'static str)],
}

impl SimplifySelfInverse {
    fn matches(&self, a: &str, b: &str) -> bool {
        for &i in self.invs {
            if (a, b) == i || (b, a) == i {
                return true;
            }
        }
        false
    }
}
impl FoldExpr for SimplifySelfInverse {
    fn fold_call(&mut self, span: Range<usize>, callee: Expr, args: Vec<Expr>) -> Expr {
        if args.len() != 1 {
            return Expr::call(span, callee, args);
        }
        let ExprKind::Var(a) = &callee.kind else {
            return Expr::call(span, callee, args);
        };
        if let ExprKind::Call(
            box Expr {
                kind: ExprKind::Var(b),
                ..
            },
            args,
        ) = &args[0].kind
            && self.matches(a, b)
            && args.len() == 1
        {
            args[0].clone()
        } else {
            Expr::call(span, callee, args)
        }
    }
}

struct ConstantFold;

fn literal(e: &Expr) -> Option<Value> {
    let ExprKind::Call(expr, args) = &e.kind else {
        return None;
    };
    if args.len() != 1 {
        return None;
    }
    let ExprKind::Var(name) = &expr.kind else {
        return None;
    };
    let value = match &args[0].kind {
        ExprKind::Int(i) => Value::Int(*i),
        ExprKind::Var(v) if v == "false" || v == "true" => Value::Bool(v == "true"),
        _ => return None,
    };
    if (name.starts_with("s_") && name.ends_with("cons_prim")) || name == "s_Bool_cons" {
        return Some(value);
    }
    None
}

fn is_booly(s: &str) -> bool {
    matches!(s, "true" | "false")
}
impl FoldExpr for ConstantFold {
    fn fold_binop(&mut self, span: Range<usize>, left: Expr, right: Expr, kind: BinopKind) -> Expr {
        match (&left.kind, &right.kind) {
            (ExprKind::Var(l), ExprKind::Var(r)) if is_booly(l) && is_booly(r) => {
                let l = l == "true";
                let r = r == "true";

                match kind {
                    BinopKind::CmpEq => Expr::var((l == r).to_string()),
                }
            }
            _ => Expr::binop(span, left, right, kind),
        }
    }

    fn fold_call(&mut self, span: Range<usize>, callee: Expr, args: Vec<Expr>) -> Expr {
        // binops encoded as functions
        if args.len() != 2 {
            return Expr::call(span, callee, args);
        }
        let ExprKind::Var(operand) = &callee.kind else {
            return Expr::call(span, callee, args);
        };
        let (Some(left), Some(right)) = (literal(&args[0]), literal(&args[1])) else {
            return Expr::call(span, callee, args);
        };

        if operand.starts_with("mir_binop_Eq") {
            return Expr::bool(left == right);
        }

        Expr::call(span, callee, args)
    }

    fn fold_ternary(
        &mut self,
        span: Range<usize>,
        cond: Expr,
        if_true: Expr,
        if_false: Expr,
    ) -> Expr {
        match cond.kind {
            ExprKind::Var(v) if is_booly(&v) => {
                if v == "true" {
                    if_true
                } else {
                    if_false
                }
            }
            _ => Expr::ternary(span, cond, if_true, if_false),
        }
    }
}

pub fn simplify(mut e: Expr) -> Expr {
    // TODO: ensure different names
    for _ in 0..2 {
        e = fold_expr(&mut RemoveTrivialLets, e);
        e = fold_expr(&mut RemoveUnusedBindings, e);
        e = fold_expr(&mut UnnestLetBindings, e);
        e = fold_expr(
            &mut InlineDefs::new(|name: &str, value: &Expr| {
                matches!(
                    &value.kind,
                    ExprKind::Call(
                        box Expr {
                            kind: ExprKind::Var(tuple),
                            ..
                        },
                        ..,
                    )
                    if name.contains("_phi_")
                    && tuple.ends_with("Tuple_cons")
                ) || literal(value).is_some()
            }),
            e,
        );
        e = fold_expr(&mut SimplifyFieldAccess, e);
        e = fold_expr(
            &mut SimplifySelfInverse {
                invs: &[
                    ("make_generic_s_Bool", "make_concrete_s_Bool"),
                    ("make_generic_s_Int_i64", "make_concrete_s_Int_i64"),
                    ("s_Bool_value", "s_Bool_cons"),
                ],
            },
            e,
        );
        e = fold_expr(&mut ConstantFold, e);
        // TODO: unnest let exprs
    }

    e
}

#[allow(unused)]
fn dump_to_file(e: &Expr, p: impl AsRef<Path>) -> std::io::Result<()> {
    let f = File::create(p)?;
    pretty_print(e, f)?;
    Ok(())
}
