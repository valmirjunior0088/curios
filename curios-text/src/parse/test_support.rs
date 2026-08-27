//! Parsing a source and printing it back: the harness every case in these suites asserts through.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use crate::*;

pub(super) fn num_lit(magnitude: u32, signed: bool, negative: bool) -> Term {
    Subterm::NumLit(NumLit {
        magnitude: magnitude.into(),
        radix: Radix::Dec,
        signed,
        negative,
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
