//! How written members meet the slots of the telescope they fill: one rule at a call, a lambda and a concept literal.

use crate::tests::{error, run};

const TWO: &str = r#"
        use /std/{Nat, Bool, Str, Show, print};
        let first: Show(Nat) = Show { show(n) = "a" };
        let other: Show(Bool) = Show { show(b) = "b" };
        let two(@A: Type, use Show(A), @B: Type, use Show(B), a: A, b: B) -> Str =
            Str/concat(Show/show(a), Show/show(b));
"#;

// Hidden arguments are written in order from the first of their run, and the rest of the run may be left out: none of them, the first alone, both dictionaries, and — through the placeholder that holds a slot's place — the second dictionary alone.
#[test]
fn hidden_arguments_are_written_from_the_first_of_their_run() {
    for (call, printed) in [
        ("two(1, true)", "1true"),
        ("two(@Nat, 1, true)", "1true"),
        ("two(@_, use first, 1, true)", "atrue"),
        ("two(@_, use first, @_, use other, 1, true)", "ab"),
        ("two(@_, use _, @Bool, use other, 1, true)", "1b"),
    ] {
        let source = format!("{TWO}        print({call})\n");
        assert_eq!(run(&source), printed.as_bytes(), "{call}");
    }
}

// A hidden argument fills its position in the run and never the next slot of its mark, so a dictionary written first meets the `@` slot the run opens with, and one written after the plain argument it precedes meets no slot at all. Each report says which.
#[test]
fn a_hidden_argument_out_of_its_run_is_refused() {
    let skipped = error(&format!("{TWO}        print(two(use first, 1, true))\n"));
    assert!(
        skipped.contains("a `use` member is written where the `@` member 'A' stands")
            && skipped.contains("write `@_` first"),
        "got: {skipped}"
    );

    let late = error(&format!("{TWO}        print(two(1, true, use first))\n"));
    assert!(
        late.contains("this `use` member has no slot"),
        "got: {late}"
    );

    let surplus = error(&format!(
        "{TWO}        print(two(@_, use _, @_, use _, @Nat, 1, true))\n"
    ));
    assert!(
        surplus.contains("this `@` member has no slot"),
        "got: {surplus}"
    );
}

// A plain `_` holds no place: plain members are always written, so it is the name it spells.
#[test]
fn a_plain_argument_is_never_a_placeholder() {
    let message = error(&format!("{TWO}        print(two(_, true))\n"));
    assert!(message.contains("unbound variable: _"), "got: {message}");
}

// A lambda's binders meet their slots by the same rule: a hidden binder written first binds the first slot of the run, whatever it is called, and a `use _` written where the run opens with `@` is refused rather than skipping to the witness slot.
#[test]
fn a_lambdas_hidden_binders_are_written_from_the_first_of_their_run() {
    let accepted = r#"
        use /std/{Nat, Str, Show, print};
        let plain: (@A: Type, use Show(A), A) -> Str = (x) => Show/show(x);
        let first: (@A: Type, use Show(A), A) -> Str = (@T, x) => Show/show(x);
        let both: (@A: Type, use Show(A), A) -> Str = (@T, use _, x) => Show/show(x);
        print(Str/concat(plain(1), Str/concat(first(2), both(3))))
        "#;
    assert_eq!(run(accepted), b"123");

    let refused = error(
        r#"
        use /std/{Nat, Str, Show, print};
        let skips: (@A: Type, use Show(A), A) -> Str = (use _, x) => Show/show(x);
        print(skips(1))
        "#,
    );
    assert!(
        refused.contains("a `use` member is written where the `@` member 'A' stands"),
        "got: {refused}"
    );
}

const BOTH: &str = r#"
        use /std/{Nat, Str, print};
        concept E(A: Type): pub Type { e: Nat }
        concept F(A: Type): pub Type { f: Nat }
        concept Both(A: Type): pub Type { use E(A), use F(A), both: Nat }
        satisfy E(Nat) { e = 1 }
        satisfy F(Nat) { f = 2 }
        let e9: E(Nat) = E { e = 9 };
        let f9: F(Nat) = F { f = 9 };
        let edges(use Both(Nat)) -> Nat = E/e(@Nat) * 10 + F/f(@Nat);
"#;

// A concept literal's entries follow its declaration: the superclass edges before a field are written from the first, an edge left out or written `use _` is resolved, and `use _` is how the second is written alone.
#[test]
fn a_concept_literals_edges_are_written_from_the_first() {
    for (literal, printed) in [
        ("Both { both = 0 }", "12"),
        ("Both { use e9, both = 0 }", "92"),
        ("Both { use e9, use f9, both = 0 }", "99"),
        ("Both { use _, use f9, both = 0 }", "19"),
    ] {
        let source = format!(
            "{BOTH}        let b: Both(Nat) = {literal};\n        print(Nat/to_str(edges(use b)))\n"
        );
        assert_eq!(run(&source), printed.as_bytes(), "{literal}");
    }

    let late = error(&format!(
        "{BOTH}        let b: Both(Nat) = Both {{ both = 0, use e9 }};\n        print(\"no\")\n"
    ));
    assert!(
        late.contains("this `use` member has no slot"),
        "got: {late}"
    );
}

// After a spread an edge left out is copied from the base, `use _` says so in writing, and a `use` entry replaces the next edge after the entries before it.
#[test]
fn after_a_spread_an_edge_is_copied_unless_written() {
    for (literal, printed) in [
        ("Both { ..base }", "99"),
        ("Both { ..base, use _ }", "99"),
        ("Both { ..base, use _, use F { f = 3 } }", "93"),
        ("Both { ..base, use E { e = 4 } }", "49"),
    ] {
        let source = format!(
            "{BOTH}        let base: Both(Nat) = Both {{ use e9, use f9, both = 0 }};\n        let b: Both(Nat) = {literal};\n        print(Nat/to_str(edges(use b)))\n"
        );
        assert_eq!(run(&source), printed.as_bytes(), "{literal}");
    }
}
