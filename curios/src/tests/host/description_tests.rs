//! A host call as a term: a description whose declaration and operands are all it is, so two calls of one row are one where their operands convert, to each checker alike.

use crate::tests::typecheck;

/// The declarations every claim below is stated over: `close` called through two definitions, and through one over an operand that converts with its binder without being it.
const CALLS: &str = r#"
    use /std/{Eq, Handle, Io};
    use /std/Handle/{close};
    let id(h: Handle) -> Handle = h;
    let one(h: Handle) -> Io({}) = close(h);
    let two(h: Handle) -> Io({}) = close(h);
    let through(h: Handle) -> Io({}) = close(id(h));
"#;

/// Two definitions that unfold to one host call are one, as posed, applied and under a binder, and so is one whose operand converts with the other's: the kernel held two calls one only where nothing had unfolded either side.
#[test]
fn two_calls_of_one_row_over_operands_that_convert_are_one() {
    for claim in [
        "let same: Eq()(one, two) = Eq/refl();",
        "let same(h: Handle) -> Eq()(one(h), two(h)) = Eq/refl();",
        "let same: Eq()((h: Handle) => close(h), (h: Handle) => close(h)) = Eq/refl();",
        "let same: Eq()(one, through) = Eq/refl();",
    ] {
        let source = format!("{CALLS}\n{claim}\n/std/print(\"ok\")");
        assert_eq!(typecheck(&source), Ok(()), "{claim}");
    }
}
