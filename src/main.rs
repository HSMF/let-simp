use std::{env, fs, io::Read};

use ariadne::Label;
use let_simp::{parse_expr, pretty_print, simplify};

fn print_err(filename: &str, src: &str, kind: &str, msg: String, span: std::ops::Range<usize>) {
    use ariadne::{ColorGenerator, Report, ReportKind, Source};

    let mut colors = ColorGenerator::new();

    let a = colors.next();

    Report::build(ReportKind::Error, (&filename, span.clone()))
        .with_message(kind.to_string())
        .with_label(
            Label::new((&filename, span))
                .with_message(msg)
                .with_color(a),
        )
        .finish()
        .eprint((&filename, Source::from(src)))
        .unwrap();
}

fn main() {
    let (filename, src) = if let Some(filename) = env::args().nth(1) {
        let src = fs::read_to_string(&filename).expect("Failed to read file");
        (filename, src)
    } else {
        let mut s = String::new();
        std::io::stdin().read_to_string(&mut s).unwrap();
        ("stdin".into(), s)
    };

    let expr = match parse_expr(src.as_str()) {
        Ok(e) => e,
        Err(let_simp::Error::UnexpectedToken(tok, span)) => {
            print_err(
                &filename,
                &src,
                "unexpected token",
                format!("{tok:?}"),
                span,
            );
            panic!();
        }
        Err(let_simp::Error::InvalidToken(span)) => {
            print_err(
                &filename,
                &src,
                "unknown token",
                "unknown token".to_string(),
                span,
            );
            panic!();
        }
        Err(let_simp::Error::UnexpectedEof) => {
            print_err(
                &filename,
                &src,
                "unexpected EOF",
                "unexpected EOF".to_string(),
                src.len()..src.len(),
            );
            panic!();
        }
        Err(report) => {
            eprintln!("{report:?}");
            panic!();
        }
    };

    let expr = simplify(expr);
    pretty_print(&expr, std::io::stdout().lock()).unwrap();
    println!();
}
