//! `/std/Tui`, for the claims a corpus unit cannot make.
//!
//! Everything the library *computes* is a `test` declaration in `curios/src/tests/corpus/tui/`, where one compile serves the whole unit. What stays here is what needs the compiler's own answer rather than a program's: a spelling that must be refused, which a corpus unit could only express by failing to compile and taking its every other test with it.

use crate::tests::typecheck;

// The palette bound is a decided proposition, so an index past the sixteen is refused where it is written rather than clamped or wrapped at runtime.
#[test]
fn a_palette_index_past_the_sixteen_is_refused() {
    let source = r#"
        use /std/{Handle, Tui};
        use /std/Tui/{Color};
        let picked: Color = Color/ansi(16);
        /std/print("unreachable")
        "#;

    let report = typecheck(source).expect_err("16 is not one of the sixteen");
    assert!(
        report.contains("Lt"),
        "the refusal should name the bound it could not discharge, got: {report}"
    );
}
