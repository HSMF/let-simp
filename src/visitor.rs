use std::ops::Range;

use crate::{BinopKind, Expr, ExprKind};

pub trait FoldExpr: Sized {
    fn fold_expr(&mut self, expr: Expr) -> Expr {
        fold_expr(self, expr)
    }

    fn fold_int(&mut self, span: Range<usize>, value: i128) -> Expr {
        Expr {
            span,
            kind: ExprKind::Int(value),
        }
    }

    fn fold_var(&mut self, span: Range<usize>, name: String) -> Expr {
        Expr {
            span,
            kind: ExprKind::Var(name),
        }
    }

    fn fold_call(&mut self, span: Range<usize>, callee: Expr, args: Vec<Expr>) -> Expr {
        Expr {
            span,
            kind: ExprKind::Call(Box::new(callee), args),
        }
    }

    fn fold_let(&mut self, span: Range<usize>, name: String, value: Expr, body: Expr) -> Expr {
        Expr {
            span,
            kind: ExprKind::Let(name, Box::new(value), Box::new(body)),
        }
    }

    fn fold_field_access(&mut self, span: Range<usize>, object: Expr, field: String) -> Expr {
        Expr {
            span,
            kind: ExprKind::FieldAccess(Box::new(object), field),
        }
    }

    fn fold_binop(&mut self, span: Range<usize>, left: Expr, right: Expr, kind: BinopKind) -> Expr {
        Expr {
            span,
            kind: ExprKind::Binop(Box::new(left), Box::new(right), kind),
        }
    }

    fn fold_ternary(
        &mut self,
        span: Range<usize>,
        cond: Expr,
        if_true: Expr,
        if_false: Expr,
    ) -> Expr {
        Expr {
            span,
            kind: ExprKind::Ternary(Box::new(cond), Box::new(if_true), Box::new(if_false)),
        }
    }
}

pub fn fold_expr<F: FoldExpr>(folder: &mut F, expr: Expr) -> Expr {
    let Expr { span, kind } = expr;
    match kind {
        ExprKind::Int(n) => folder.fold_int(span, n),
        ExprKind::Var(name) => folder.fold_var(span, name),
        ExprKind::Call(callee, args) => {
            let callee = folder.fold_expr(*callee);
            let args = args.into_iter().map(|a| folder.fold_expr(a)).collect();
            folder.fold_call(span, callee, args)
        }
        ExprKind::Let(name, value, body) => {
            let value = folder.fold_expr(*value);
            let body = folder.fold_expr(*body);
            folder.fold_let(span, name, value, body)
        }
        ExprKind::FieldAccess(object, field) => {
            let object = folder.fold_expr(*object);
            folder.fold_field_access(span, object, field)
        }
        ExprKind::Binop(left, right, op) => {
            let left = folder.fold_expr(*left);
            let right = folder.fold_expr(*right);
            folder.fold_binop(span, left, right, op)
        }
        ExprKind::Ternary(cond, if_true, if_false) => {
            let cond = folder.fold_expr(*cond);
            let if_true = folder.fold_expr(*if_true);
            let if_false = folder.fold_expr(*if_false);
            folder.fold_ternary(span, cond, if_true, if_false)
        }
    }
}

pub trait Visitor: Sized {
    type Output;

    fn visit_expr(&mut self, expr: &Expr) -> Self::Output {
        walk_expr(self, expr)
    }

    fn visit_int(&mut self, span: &Range<usize>, value: i128) -> Self::Output;
    fn visit_var(&mut self, span: &Range<usize>, name: &str) -> Self::Output;
    fn visit_call(&mut self, span: &Range<usize>, callee: &Expr, args: &[Expr]) -> Self::Output;
    fn visit_let(
        &mut self,
        span: &Range<usize>,
        name: &str,
        value: &Expr,
        body: &Expr,
    ) -> Self::Output;
    fn visit_field_access(
        &mut self,
        span: &Range<usize>,
        object: &Expr,
        field: &str,
    ) -> Self::Output;
    fn visit_binop(
        &mut self,
        span: &Range<usize>,
        left: &Expr,
        right: &Expr,
        kind: BinopKind,
    ) -> Self::Output;
    fn visit_ternary(
        &mut self,
        span: &Range<usize>,
        cond: &Expr,
        if_true: &Expr,
        if_false: &Expr,
    ) -> Self::Output;
}

pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: &Expr) -> V::Output {
    match &expr.kind {
        ExprKind::Int(n) => visitor.visit_int(&expr.span, *n),
        ExprKind::Var(name) => visitor.visit_var(&expr.span, name),
        ExprKind::Call(callee, args) => {
            visitor.visit_expr(callee);
            for arg in args {
                visitor.visit_expr(arg);
            }
            visitor.visit_call(&expr.span, callee, args)
        }
        ExprKind::Let(name, value, body) => {
            visitor.visit_expr(value);
            visitor.visit_expr(body);
            visitor.visit_let(&expr.span, name, value, body)
        }
        ExprKind::FieldAccess(object, field) => {
            visitor.visit_expr(object);
            visitor.visit_field_access(&expr.span, object, field)
        }
        ExprKind::Binop(left, right, op) => {
            visitor.visit_expr(left);
            visitor.visit_expr(right);
            visitor.visit_binop(&expr.span, left, right, *op)
        }
        ExprKind::Ternary(cond, if_true, if_false) => {
            visitor.visit_expr(cond);
            visitor.visit_expr(if_true);
            visitor.visit_expr(if_false);
            visitor.visit_ternary(&expr.span, cond, if_true, if_false)
        }
    }
}
