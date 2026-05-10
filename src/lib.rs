#![feature(box_patterns)]
//! simplifier for the let expressions that prusti emits

use std::{fmt::Display, iter::Peekable, ops::Range};

use logos::{Logos, SpannedIter};

pub type Result<T> = std::result::Result<T, Error>;
mod simplify;
mod visitor;
pub use simplify::simplify;

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
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_$]*", |lex| lex.slice().to_owned())]
    Ident(String),
    #[token("==")]
    EqEq,
    #[token("?")]
    QuestionMark,
    #[token(":")]
    Colon,
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

    fn var(v: String) -> Expr {
        Expr {
            span: 0..0,
            kind: ExprKind::Var(v),
        }
    }

    fn bool(value: bool) -> Expr {
        Expr::call(
            0..0,
            Self::var("s_Bool_cons".to_owned()),
            vec![Self::var(value.to_string())],
        )
    }

    fn binop(span: Range<usize>, left: Expr, right: Expr, kind: BinopKind) -> Expr {
        Expr {
            span,
            kind: ExprKind::Binop(Box::new(left), Box::new(right), kind),
        }
    }

    fn ternary(span: Range<usize>, cond: Expr, if_true: Expr, if_false: Expr) -> Expr {
        Expr {
            span,
            kind: ExprKind::Ternary(Box::new(cond), Box::new(if_true), Box::new(if_false)),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinopKind {
    CmpEq,
}

impl Display for BinopKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = match self {
            Self::CmpEq => "==",
        };
        write!(f, "{op}")
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Int(i128),
    Var(String),
    Binop(Box<Expr>, Box<Expr>, BinopKind),
    Call(Box<Expr>, Vec<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    FieldAccess(Box<Expr>, String),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
}

impl ExprKind {
    #[allow(unused)]
    fn name(&self) -> &'static str {
        match self {
            ExprKind::Int(_) => "int",
            ExprKind::Var(_) => "var",
            ExprKind::Call(..) => "call",
            ExprKind::Let(..) => "let",
            ExprKind::FieldAccess(..) => "field",
            ExprKind::Binop(..) => "binop",
            ExprKind::Ternary(..) => "ternary",
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

fn comparison(lexer: &mut Lex) -> Result<Expr> {
    let left = call_expr(lexer)?;
    if let Some((Ok(Token::EqEq), ..)) = lexer.peek() {
        lexer.next();
        let right = comparison(lexer)?;

        Ok(Expr {
            span: left.span.start..right.span.end,
            kind: ExprKind::Binop(Box::new(left), Box::new(right), BinopKind::CmpEq),
        })
    } else {
        Ok(left)
    }
}

fn ternary(lexer: &mut Lex) -> Result<Expr> {
    let cond = comparison(lexer)?;

    let Some((Ok(Token::QuestionMark), ..)) = lexer.peek() else {
        return Ok(cond);
    };

    lexer.next();
    let then = comparison(lexer)?;
    expect!(lexer, Some((Ok(Token::Colon), _)));
    let otherwise = comparison(lexer)?;

    Ok(Expr {
        span: cond.span.start..otherwise.span.end,
        kind: ExprKind::Ternary(Box::new(cond), Box::new(then), Box::new(otherwise)),
    })
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
    ternary(lexer)
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
            ExprKind::Binop(left, right, binop_kind) => {
                write!(w, "(")?;
                self.show_expr(left, w)?;
                write!(w, " {binop_kind} ")?;
                self.show_expr(right, w)?;
                write!(w, ")")?;
            }
            ExprKind::Ternary(cond, if_true, if_false) => {
                write!(w, "(")?;
                self.show_expr(cond, w)?;
                write!(w, " ? ")?;
                self.show_expr(if_true, w)?;
                write!(w, " : ")?;
                self.show_expr(if_false, w)?;
                write!(w, ")")?;
            }
        }
        Ok(())
    }
}
