#![feature(box_patterns)]
//! simplifier for the let expressions that prusti emits

use std::{collections::HashSet, fs::File, iter::Peekable, ops::Range, path::Path};

use logos::{Logos, SpannedIter};

use crate::visitor::{FoldExpr, Visitor, fold_expr};

pub type Result<T> = std::result::Result<T, Error>;
mod visitor;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("unexpected token {0:?}")]
    UnexpectedToken(Token, Range<usize>),
    #[error("invalid token")]
    InvalidToken(Range<usize>),
    #[error("unexpected EOF")]
    UnexpectedEof,
    #[error("{0}")]
    Other(String),
}

#[derive(Logos, Debug, PartialEq, Eq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[regex("[0-9]+", |lex| lex.slice().parse::<i128>().unwrap())]
    Int(i128),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_owned())]
    Ident(String),
    #[token("==")]
    EqEq,
}

#[derive(Debug, Clone)]
pub struct Expr {
    span: Range<usize>,
    kind: ExprKind,
}

impl Expr {
    fn field_access(span: Range<usize>, object: Expr, field: String) -> Self {
        Self {
            span,
            kind: ExprKind::FieldAccess(Box::new(object), field),
        }
    }

    fn call(span: Range<usize>, callee: Expr, args: Vec<Expr>) -> Expr {
        Self {
            span,
            kind: ExprKind::Call(Box::new(callee), args),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Int(i128),
    Var(String),
    Call(Box<Expr>, Vec<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    FieldAccess(Box<Expr>, String),
}

impl ExprKind {
    fn name(&self) -> &'static str {
        match self {
            ExprKind::Int(_) => "int",
            ExprKind::Var(_) => "var",
            ExprKind::Call(..) => "call",
            ExprKind::Let(..) => "let",
            ExprKind::FieldAccess(..) => "field",
        }
    }
}

type Lex<'a> = Peekable<SpannedIter<'a, Token>>;
pub fn parse_expr(s: &str) -> Result<Expr> {
    let mut lexer = Token::lexer(s).spanned().peekable();
    top(&mut lexer)
}

#[macro_export]
macro_rules! expect {
    ($lexer:expr, $token:pat) => {
        $crate::expect!($lexer, $token => ());
    };
    ($lexer:expr, $token:pat => $e:expr) => {{
        match $lexer.next() {
            $token => $e,
            Some((Ok(k), span)) => return Err(Error::UnexpectedToken(k, span)),
            Some((Err(()), span)) => return Err(Error::InvalidToken(span)),
            _ => return Err(Error::UnexpectedEof),
        }
    }};
}

fn terminal(lexer: &mut Lex) -> Result<Expr> {
    match lexer.next() {
        Some((Ok(Token::Int(i)), span)) => Ok(Expr {
            span,
            kind: ExprKind::Int(i),
        }),
        Some((Ok(Token::Ident(i)), span)) => Ok(Expr {
            span,
            kind: ExprKind::Var(i),
        }),
        Some((Ok(Token::OpenParen), _)) => {
            let e = top(lexer)?;
            expect!(lexer, Some((Ok(Token::CloseParen), _)));
            Ok(e)
        }
        Some((Ok(k), span)) => Err(Error::UnexpectedToken(k, span)),
        Some((Err(()), span)) => Err(Error::InvalidToken(span)),
        _ => Err(Error::UnexpectedEof),
    }
}

fn field_access(lexer: &mut Lex) -> Result<Expr> {
    let e = terminal(lexer)?;

    if let Some((Ok(Token::Dot), _)) = lexer.peek() {
        lexer.next();
        let (i, span) = expect!(lexer, Some((Ok(Token::Ident(i)), span)) => (i, span));
        return Ok(Expr {
            span: e.span.start..span.end,
            kind: ExprKind::FieldAccess(Box::new(e), i),
        });
    }

    Ok(e)
}

fn call_expr(lexer: &mut Lex) -> Result<Expr> {
    let e = field_access(lexer)?;

    if let Some((Ok(Token::OpenParen), _)) = lexer.peek() {
        lexer.next();
        let mut args = vec![];
        let mut needs_comma = false;
        loop {
            if let Some((Ok(Token::CloseParen), end)) = lexer.peek() {
                let end = end.end;
                lexer.next();
                return Ok(Expr {
                    span: e.span.start..end,
                    kind: ExprKind::Call(Box::new(e), args),
                });
            }
            if needs_comma {
                expect!(lexer, Some((Ok(Token::Comma), _)));
                needs_comma = false;
                continue;
            }
            let arg = top(lexer)?;
            needs_comma = true;
            args.push(arg);
        }
    }

    Ok(e)
}

fn top(lexer: &mut Lex) -> Result<Expr> {
    if let Some((Ok(Token::Let), span)) = lexer.peek() {
        let span = span.clone();
        lexer.next();
        let id = expect!(lexer, Some((Ok(Token::Ident(id)), _)) => id);
        expect!(lexer, Some((Ok(Token::EqEq), _)));
        expect!(lexer, Some((Ok(Token::OpenParen), _)));
        let binding = top(lexer)?;
        expect!(lexer, Some((Ok(Token::CloseParen), _)));
        expect!(lexer, Some((Ok(Token::In), _)));
        let expr = top(lexer)?;
        return Ok(Expr {
            span: span.start..expr.span.end,
            kind: ExprKind::Let(id, Box::new(binding), Box::new(expr)),
        });
    };
    call_expr(lexer)
}

pub fn pretty_print(expr: &Expr, mut w: impl std::io::Write) -> std::io::Result<()> {
    write!(w, "")?;
    PrettyPrinter { depth: 0 }.show_expr(expr, &mut w)
}

struct PrettyPrinter {
    depth: usize,
}

impl PrettyPrinter {
    fn indent(&self, w: &mut impl std::io::Write) -> std::io::Result<()> {
        for _ in 0..self.depth {
            write!(w, "  ")?;
        }
        Ok(())
    }
    fn open_paren(&mut self, w: &mut impl std::io::Write) -> std::io::Result<()> {
        self.depth += 1;
        write!(w, "(")
    }
    fn close_paren(&mut self, w: &mut impl std::io::Write) -> std::io::Result<()> {
        self.depth -= 1;
        write!(w, ")")
    }
    fn show_expr(&mut self, e: &Expr, w: &mut impl std::io::Write) -> std::io::Result<()> {
        match &e.kind {
            ExprKind::Int(i) => write!(w, "{i}")?,
            ExprKind::Var(i) => write!(w, "{i}")?,
            ExprKind::Call(expr, exprs) => {
                self.show_expr(expr, w)?;
                self.open_paren(w)?;
                let mut need_comma = false;
                let multiline = exprs.len() > 1;
                for e in exprs {
                    if need_comma {
                        write!(w, ", ")?;
                    }
                    if multiline {
                        writeln!(w)?;
                        self.indent(w)?;
                    }
                    need_comma = true;
                    self.show_expr(e, w)?;
                }
                self.close_paren(w)?;
            }
            ExprKind::Let(v, expr, expr1) => {
                writeln!(w)?;
                self.indent(w)?;

                write!(w, "let {v} == ")?;
                self.open_paren(w)?;
                self.show_expr(expr, w)?;
                self.close_paren(w)?;
                write!(w, " in ")?;
                self.show_expr(expr1, w)?;
            }
            ExprKind::FieldAccess(expr, field) => {
                self.show_expr(expr, w)?;
                write!(w, ".{field}")?;
            }
        }
        Ok(())
    }
}

fn replace(e: Expr, old: &str, new: Expr) -> Expr {
    let Expr { span, kind } = e;
    let kind = match kind {
        ExprKind::Call(expr, exprs) => ExprKind::Call(
            Box::new(replace(*expr, old, new.clone())),
            exprs
                .into_iter()
                .map(|e| replace(e, old, new.clone()))
                .collect(),
        ),
        ExprKind::Let(v, expr, expr1) => {
            let expr = Box::new(replace(*expr, old, new.clone()));
            if v == old {
                ExprKind::Let(v, expr, expr1)
            } else {
                ExprKind::Let(v, expr, Box::new(replace(*expr1, old, new.clone())))
            }
        }
        ExprKind::FieldAccess(expr, field) => {
            let expr = Box::new(replace(*expr, old, new.clone()));
            ExprKind::FieldAccess(expr, field)
        }
        ExprKind::Var(v) if v == old => return new,
        _ => kind,
    };
    Expr { span, kind }
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

#[allow(unused)]
fn dump_to_file(e: &Expr, p: impl AsRef<Path>) -> std::io::Result<()> {
    let f = File::create(p)?;
    pretty_print(e, f)?;
    Ok(())
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
                )
            }),
            e,
        );
        e = fold_expr(&mut SimplifyFieldAccess, e);
        e = fold_expr(
            &mut SimplifySelfInverse {
                invs: &[
                    ("make_generic_s_Bool", "make_concrete_s_Bool"),
                    ("make_generic_s_Int_i64", "make_concrete_s_Int_i64"),
                ],
            },
            e,
        );
        // TODO: unnest let exprs
    }

    e
}
