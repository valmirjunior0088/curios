//! Parsing a source and printing it back: the harness every case in these suites asserts through.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use crate::*;

// The radix carries the written width, so a fixture numeral takes the width its own decimal spelling has — which is what the parser reads back for it.
pub(super) fn num_lit(magnitude: u32, sign: curios_utilities::Sign) -> Term {
    Subterm::NumLit(NumLit {
        magnitude: magnitude.into(),
        radix: Radix::Dec(magnitude.to_string().len()),
        sign,
    })
    .into()
}

pub(super) fn name(label: &str) -> Term {
    Subterm::Name(Name::from([label.to_string()])).into()
}

pub(super) fn cond_arm(condition: Term, body: Term) -> ChooseArm {
    ChooseArm {
        test: ChooseTest::Cond(condition),
        body,
    }
}

/// The captured comment texts of one parse, in offset order.
pub(super) fn comments_of(source: &str) -> Vec<String> {
    let source = curios_utilities::Source::inline(source);
    let (_, comments) = Module::parse_with_comments(&source).expect("fixture parses");
    comments
        .iter()
        .map(|span| span.source.text[span.start..span.end].to_string())
        .collect()
}

/// The source text a label's span covers — what a report about it would underline.
pub(super) fn spelled(label: &Label) -> String {
    let span = label.span().expect("a parsed label carries a span");
    span.source.text[span.start..span.end].to_string()
}
