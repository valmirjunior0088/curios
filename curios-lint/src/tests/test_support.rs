//! Lowering a fixture and deciding its lints: the harness every case in these suites asserts through.

use {
    curios_module::{RootSource, UnitSource, into_core_unit, test_support::*},
    curios_surface::Entrypoint,
};

/// The lints of `src` lowered alone, each as `kind: message`: decided as a unit's are where nothing elaborated it, so no binder is one a proof read.
pub fn lints(src: &str) -> Vec<String> {
    let entrypoint = src.parse::<Entrypoint>().unwrap();
    let loader = RootSource::none().tested();
    let text = into_core_unit(&UnitSource::entry(&entrypoint, &loader), &[], &SYNTAX).unwrap();

    crate::lints(
        text.lints(),
        text.zeros(),
        |_| false,
        |_| None,
        text.anchors(),
    )
    .iter()
    .map(|lint| format!("{}: {}", lint.kind.name(), lint.report(&[]).message()))
    .collect()
}
