//! The standard library's witnesses on the tuple shapes, end to end: `/std/Tuple`'s `Show`, `Eql` and `Ord`.

use crate::tests::run;

// A tuple shows as its literal is written, so the rendering re-reads as source: the empty shape as `()`, the one-field shape keeping the trailing comma that separates it from a parenthesized term, and a nested tuple showing through the same witness at its own shape.
#[test]
fn a_tuple_shows_as_its_literal() {
    let source = r#"
        use /std/{Show, Nat, Bool, Str, List};
        let parts: List(Str) = [
            Show/show((1, true)),
            Show/show(()),
            Show/show((5,)),
            Show/show((1, (2, 3))),
            Show/show((1, 2, 3, 4, 5, 6, 7, 8)),
        ];
        /std/print(Str/join(" ", parts))
        "#;

    assert_eq!(
        run(source),
        b"(1, true) () (5,) (1, (2, 3)) (1, 2, 3, 4, 5, 6, 7, 8)"
    );
}

// The tuple witness is reached through another witness's premise with nothing written at the call: `Show(List({Nat, Bool}))` resolves the list witness, whose `Show(A)` premise keys on the shape.
#[test]
fn a_tuple_shows_through_a_premise() {
    let source = r#"
        use /std/{Show, Nat, Bool};
        /std/print(Show/show([(1, true), (2, false)]))
        "#;

    assert_eq!(run(source), b"[(1, true), (2, false)]");
}

// `%` dispatches through `Show`, so a format directive reaches the tuple witnesses like any other.
#[test]
fn a_format_directive_shows_a_tuple() {
    let source = r#"
        use /std/{Nat, Bool, Fmt};
        let a: {Nat, Bool} = (1, true);
        let u: {} = ();
        Fmt/print("% and %")(a)(u)
        "#;

    assert_eq!(run(source), b"(1, true) and ()");
}

#[test]
fn equality_is_componentwise() {
    let source = r#"
        use /std/{Show, Eql, Nat, Bool, Str, List};
        let a: {Nat, Bool} = (1, true);
        let b: {Nat, Bool} = (2, true);
        let u: {} = ();
        let parts: List(Str) = [
            Show/show(a == a),
            Show/show(a == b),
            Show/show(a != b),
            Show/show(u == u),
        ];
        /std/print(Str/join(" ", parts))
        "#;

    assert_eq!(run(source), b"true false true true");
}

// Lexicographic: the first component that is not `eq` decides, and a tie falls through to the next. `Ord`'s `Eql` superclass slot is left to resolution and lands on the tuple `Eql` witness, whose own premises come from projecting the `Ord` premises — the ordinary machinery, composing.
#[test]
fn ordering_is_lexicographic() {
    let source = r#"
        use /std/{Show, Ord, Order, Nat, Str, List};
        let p: {Nat, Nat} = (1, 2);
        let q: {Nat, Nat} = (1, 3);
        let r: {Nat, Nat} = (2, 2);
        let parts: List(Str) = [
            Show/show(Ord/cmp(p, q)),
            Show/show(Ord/cmp(r, q)),
            Show/show(Ord/cmp(p, p)),
        ];
        /std/print(Str/join(" ", parts))
        "#;

    assert_eq!(run(source), b"lt gt eq");
}
