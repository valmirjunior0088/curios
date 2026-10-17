//! What filling promises: every hole filled, every value asked for, and the template's own braces left as they stand.

use {super::*, std::panic};

#[test]
fn every_hole_takes_its_value_and_a_brace_stays_single() {
    let name = "Str";
    let count = 2;

    assert_eq!(
        crate::fill!(
            "use /std/{${name}}; let n = ${count}; let s = ${say};",
            name,
            count,
            say = "hi"
        ),
        "use /std/{Str}; let n = 2; let s = hi;"
    );
}

#[test]
fn a_hole_with_no_value_panics() {
    let unfilled = panic::catch_unwind(|| fill("let x = ${x};", &[]));

    assert!(unfilled.is_err());
}

#[test]
fn a_value_the_template_never_asks_for_panics() {
    let unasked = panic::catch_unwind(|| fill("let x = 1;", &[("x", &1)]));

    assert!(unasked.is_err());
}
