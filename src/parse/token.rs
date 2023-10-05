use logos::{Lexer, Logos, Span};

fn slice<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> &'a str {
    lexer.slice()
}

#[derive(Debug, Clone, Logos)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token<'a> {
    #[token(",")]
    Comma,

    #[token(":")]
    Colon,

    #[token(";")]
    Semicolon,

    #[token("=")]
    Equals,

    #[token("*")]
    Asterisk,

    #[token("(")]
    ParenthesesLeft,

    #[token(")")]
    ParenthesesRight,

    #[token("{")]
    CurlyBracketLeft,

    #[token("}")]
    CurlyBracketRight,

    #[token("->")]
    ArrowSlim,

    #[token("=>")]
    ArrowFat,

    #[token("Type")]
    KeywordType,

    #[token("Int32")]
    KeywordInt32,

    #[token("Flt32")]
    KeywordFlt32,

    #[token("with")]
    KeywordWith,

    #[token("do")]
    KeywordDo,

    #[token("when")]
    KeywordWhen,

    #[token("unreachable")]
    KeywordUnreachable,

    #[token("is")]
    KeywordIs,

    #[regex(r"((\+)|(\-)|(\*)|(\\)|(&)|(\|)|(==)|(!=)|(<)|(<=)|(>)(>=))(i|f)", slice)]
    Innate(&'a str),

    #[regex(r"(\+?|\-)[0-9]+", slice)]
    Integer(&'a str),

    #[regex(r"(\+?|\-)[0-9]+\.[0-9]+", slice)]
    Decimal(&'a str),

    #[regex(r":[_a-zA-Z][_a-zA-Z0-9]*", slice)]
    Rune(&'a str),

    #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", slice)]
    Name(&'a str),
}

impl<'a> Token<'a> {
    pub fn spanned(self, span: Span) -> (usize, Self, usize) {
        (span.start, self, span.end)
    }

    pub fn spanned_lexer(input: &'a str) -> SpannedLexer<'a> {
        SpannedLexer {
            lexer: Self::lexer(input),
        }
    }
}

#[derive(Debug)]
pub struct Unknown {
    pub slice: String,
    pub start: usize,
    pub end: usize,
}

impl Unknown {
    pub fn new(slice: impl Into<String>, span: Span) -> Self {
        Self {
            slice: slice.into(),
            start: span.start,
            end: span.end,
        }
    }
}

#[derive(Debug)]
pub struct SpannedLexer<'a> {
    lexer: Lexer<'a, Token<'a>>,
}

impl<'a> Iterator for SpannedLexer<'a> {
    type Item = Result<(usize, Token<'a>, usize), Unknown>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(match self.lexer.next()? {
            Ok(token) => Ok(token.spanned(self.lexer.span())),
            Err(()) => Err(Unknown::new(self.lexer.slice(), self.lexer.span())),
        })
    }
}

pub mod helpers {
    use crate::{BinOp, BoolOp, CompOp, Term};

    pub fn with(left: &str, right: &str, scrutinee: Term, output: Term) -> Term {
        Term::with(scrutinee, left, right, output)
    }

    pub fn when_unreachable(scrutinee: Term) -> Term {
        Term::when(scrutinee, Vec::<(&str, Term)>::new())
    }

    pub fn when_is(scrutinee: Term, branches: Vec<(&str, Term)>) -> Term {
        let branches = branches
            .into_iter()
            .map(|(rune, term)| (rune.strip_prefix(':').unwrap(), term));

        Term::when(scrutinee, branches)
    }

    pub fn apply(function: Term, arguments: Vec<Term>) -> Term {
        arguments.into_iter().fold(function, Term::apply)
    }

    pub fn rune_type(runes: Vec<&str>) -> Term {
        Term::rune_type(runes.into_iter().map(|rune| rune.strip_prefix(':').unwrap()))
    }

    pub fn innate(innate: &str, left: Term, right: Term) -> Term {
        let mut innate = String::from(innate);

        match innate.pop().unwrap() {
            'i' => match innate.as_ref() {
                "+" => Term::int32_bin_op(BinOp::Add, left, right),
                "-" => Term::int32_bin_op(BinOp::Sub, left, right),
                "*" => Term::int32_bin_op(BinOp::Mul, left, right),
                "/" => Term::int32_bin_op(BinOp::Div, left, right),
                "&" => Term::int32_bool_op(BoolOp::And, left, right),
                "|" => Term::int32_bool_op(BoolOp::Or, left, right),
                "==" => Term::int32_comp_op(CompOp::Eq, left, right),
                "!=" => Term::int32_comp_op(CompOp::Ne, left, right),
                "<" => Term::int32_comp_op(CompOp::Lt, left, right),
                "<=" => Term::int32_comp_op(CompOp::Le, left, right),
                ">" => Term::int32_comp_op(CompOp::Gt, left, right),
                ">=" => Term::int32_comp_op(CompOp::Ge, left, right),
                _ => unreachable!(),
            },
            'f' => match innate.as_ref() {
                "+" => Term::flt32_bin_op(BinOp::Add, left, right),
                "-" => Term::flt32_bin_op(BinOp::Sub, left, right),
                "*" => Term::flt32_bin_op(BinOp::Mul, left, right),
                "/" => Term::flt32_bin_op(BinOp::Div, left, right),
                "&" => panic!("Boolean comparisons aren't available to decimals"),
                "|" => panic!("Boolean comparisons aren't available to decimals"),
                "==" => Term::flt32_comp_op(CompOp::Eq, left, right),
                "!=" => Term::flt32_comp_op(CompOp::Ne, left, right),
                "<" => Term::flt32_comp_op(CompOp::Lt, left, right),
                "<=" => Term::flt32_comp_op(CompOp::Le, left, right),
                ">" => Term::flt32_comp_op(CompOp::Gt, left, right),
                ">=" => Term::flt32_comp_op(CompOp::Ge, left, right),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    pub fn integer(number: &str) -> Term {
        Term::Int32(number.parse().unwrap())
    }

    pub fn decimal(number: &str) -> Term {
        Term::Flt32(number.parse().unwrap())
    }

    pub fn rune(rune: &str) -> Term {
        Term::rune(rune.strip_prefix(':').unwrap())
    }

    pub fn name(name: &str) -> Term {
        Term::local(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::ModuleParser;

    static EXAMPLE: &str = r#"
        Unit: Type = {:unit};
        unit: Unit = :unit;
        example: 0 = +i :a :b;
    "#;

    #[test]
    #[ignore]
    fn it_works() {
        let lexer = Token::spanned_lexer(EXAMPLE);
        let result = ModuleParser::new().parse(lexer);
        println!("{result:?}");
    }
}
