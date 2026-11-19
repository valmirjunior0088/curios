//! Parsing a source and printing it back: the harness every case in these suites asserts through.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use {
    crate::*,
    curios_common::{Sign, Source},
    curios_core::Numeral,
    curios_num::Integer,
};

// The radix carries the written width, so a fixture numeral takes the width its own decimal spelling has — which is what the parser reads back for it.
pub fn num_lit(magnitude: u32, sign: Sign) -> Term {
    let integer = Integer::from(magnitude);
    Subterm::NumLit(NumLit {
        value: match sign {
            Sign::Unmarked => Numeral::Natural(magnitude.into()),
            Sign::Positive => Numeral::Integer(integer),
            Sign::Negative => Numeral::Integer(-integer),
        },
        radix: Radix::Dec(magnitude.to_string().len()),
    })
    .into()
}

pub fn name(label: &str) -> Term {
    Subterm::Name(Name::from([label.to_string()])).into()
}

/// The captured comment texts of one parse, in offset order.
pub fn comments_of(source: &str) -> Vec<String> {
    let source = Source::inline(source);
    let (_, comments) = Module::parse_with_comments(&source).expect("fixture parses");
    comments
        .iter()
        .map(|span| span.source.text()[span.start..span.end].to_string())
        .collect()
}

/// The source text a label's span covers — what a report about it would underline.
pub fn spelled(label: &Label) -> String {
    let span = label.span().expect("a parsed label carries a span");
    span.source.text()[span.start..span.end].to_string()
}
