//! The matrix form's own rules: consistent arity, no duplicate row or tag, where a motive may sit, and where the row that cannot fail is typed.

use crate::tests::{error, run};

// Two rows write a tuple pattern of different arity in the same column — there is no single shape to explode into projections.
#[test]
fn match_rejects_inconsistent_tuple_arity() {
    let source = r#"
        use /std/{Nat};
        let f(p : { Nat, Nat }) -> Nat =
            match p
            | (x, y) => x
            | (x,) => x
            end;
        /std/print(Nat/to_str(f((3, 4))))
        "#;

    let error = error(source);
    assert!(
        error.contains("disagree on shape"),
        "unexpected error: {error}"
    );
}

// Two rows are identical in every column, several levels deep — an overlapping arm the full-enumeration matrix has no priority order to resolve.
#[test]
fn match_rejects_duplicate_row() {
    let source = r#"
        use /std/{Option, Nat};
        let f(a : Option(Nat), b : Option(Nat)) -> Nat =
            match (a, b)
            | (some(x), some(y)) => x + y
            | (some(x), some(y)) => x
            | (some(x), none()) => x
            | (none(), some(y)) => y
            | (none(), none()) => 0
            end;
        /std/print(Nat/to_str(f(Option/some(3), Option/some(4))))
        "#;

    let error = error(source);
    assert!(
        error.contains("duplicate or overlapping"),
        "unexpected error: {error}"
    );
}

// A literal repeated constructor tag in a flat, single-column match, refused rather than collapsed to whichever arm's tag survives collection, dropping the other.
#[test]
fn match_rejects_duplicate_flat_tag() {
    let source = r#"
        use /std/{Option, Nat};
        let f(o : Option(Nat)) -> Nat =
            match o
            | some(a) => a
            | some(b) => b
            | none() => 0
            end;
        /std/print(Nat/to_str(f(Option/some(3))))
        "#;

    let error = error(source);
    assert!(
        error.contains("duplicate or overlapping"),
        "unexpected error: {error}"
    );
}

// Of two arms that take the same value, the later is the one no value reaches, and the report stands under its pattern rather than under the whole match.
#[test]
fn a_duplicate_arm_is_reported_at_the_later_arm() {
    let source = "use /std/{Option, Nat};\n\nlet f(o: Option(Nat)) -> Nat =\n    match o\n    | some(x) => x\n    | none() => 0\n    | none() => 1\n    end;\n\n/std/print(Nat/to_str(f(Option/some(3))))\n";

    let error = error(source);
    assert!(
        error.contains("    7 |     | none() => 1\n      |       ^^^^^^"),
        "unexpected error: {error}"
    );
}

// A dependent motive requires the head to dispatch on a constructor tag directly — there is no core `Match` node for a tuple-headed match to attach it to.
#[test]
fn match_rejects_a_motive_on_tuple_head() {
    // A tuple-headed matrix explodes into projections and builds no core `Match` node, so there is no eliminator for a motive to be checked against. Rejected rather than silently discarded.
    let source = r#"
        use /std/{Nat};
        let f(p : { Nat, Nat }) -> Nat =
            match p : (q) => Nat
            | (x, y) => x
            end;
        /std/print(Nat/to_str(f((3, 4))))
        "#;

    let error = error(source);
    assert!(
        error.contains("written motive"),
        "unexpected error: {error}"
    );
}

// A `Nat` column missing its `0` case entirely, reported as the value no arm matches.
#[test]
fn match_rejects_incomplete_nat_pattern() {
    let source = r#"
        use /std/{Nat};
        let f(n : Nat) -> Nat =
            match n
            | n2 + 1; ih => n2
            end;
        /std/print(Nat/to_str(f(3)))
        "#;

    let error = error(source);
    assert!(
        error.contains("no arm matches `0`"),
        "unexpected error: {error}"
    );
}

// A dependent motive is legal on a top-level `Nat` head too, not just a constructor head. The arms are written succ-case-first, which changes nothing: the full-enumeration matrix gives rows no priority order.
#[test]
fn match_allows_dependent_motive_on_nat_head() {
    let source = r#"
        use /std/{Nat};
        let f(n : Nat) -> Nat =
            match n : (m) => Nat
            | m + 1; ih => m
            | 0 => 0
            end;
        /std/print(Nat/to_str(f(3)))
        "#;

    assert_eq!(run(source), b"2");
}

// A literal inside a constructor's payload, with a `_` to fall through to, is a dispatch by value and no fold.
#[test]
fn a_nested_nat_literal_dispatches_by_value() {
    let source = r#"
        use /std/{Option, Nat};
        let f(m : Option(Nat)) -> Nat =
            match m | some(5) => 1 | _ => 0 end;
        /std/print(Nat/to_str(f(Option/some(5)) + 10 * f(Option/some(4)) + 100 * f(Option/none())))
        "#;

    assert_eq!(run(source), b"1");
}

// A literal arm and a successor arm in one `Nat` column are the dispatch and the fold, and one column is one of them.
#[test]
fn nat_literal_mixed_with_succ_is_rejected() {
    let source = r#"
        use /std/{Option, Nat};
        let f(m : Option(Nat)) -> Nat =
            match m | some(5) => 1 | some(n + 1; ih) => n | _ => 0 end;
        /std/print(Nat/to_str(f(Option/some(5))))
        "#;

    let error = error(source);
    assert!(
        error.contains("mixes successor-peeling"),
        "unexpected error: {error}"
    );
}

// A refusal of a match's shape stands under the match, wherever in a body the match is written: a whole body, an operand, a lambda's body.
#[test]
fn a_match_shape_refusal_is_located_at_the_match() {
    for (source, line) in [
        (
            "use /std/{Option, Nat};\n\nlet f(b: Option(Nat)) -> Nat =\n    match b\n    | some(5) => 1\n    | some(n + 1; ih) => n\n    | _ => 0\n    end;\n\n/std/print(Nat/to_str(f(Option/some(5))))\n",
            "    4 |     match b",
        ),
        (
            "use /std/{Option, Nat};\n\nlet f(b: Option(Nat)) -> Nat =\n    1 + (match b | some(5) => 1 | some(n + 1; ih) => n | _ => 0 end);\n\n/std/print(Nat/to_str(f(Option/some(5))))\n",
            "    4 |     1 + (match b",
        ),
        (
            "use /std/{Option, Nat};\n\nlet f(b: Option(Nat)) -> Nat =\n    let g = (k: Option(Nat)) =>\n        match k | some(5) => 1 | some(n + 1; ih) => n | _ => 0 end;\n    g(b);\n\n/std/print(Nat/to_str(f(Option/some(5))))\n",
            "    5 |         match k",
        ),
    ] {
        let error = error(source);
        assert!(
            error.contains("mixes successor-peeling") && error.contains(line),
            "{source:?} reported {error}"
        );
    }
}

// The row that cannot fail is read where the match stands and at the match's own type, however many dispatches reach it: a numeral in it is realized at the type the match is checked at.
#[test]
fn a_default_reached_from_a_nested_dispatch_is_typed_at_the_matchs_type() {
    let source = r#"
        use /std/{Int, Option};
        let f(m : Option(Option(Int))) -> Int =
            match m | some(some(x)) => x | _ => 0 end;
        /std/print(Int/to_str(f(Option/some(Option/some(-7))) + f(Option/some(Option/none())) + f(Option/none())))
        "#;

    assert_eq!(run(source), b"-7");
}

// And an action in it is sequenced in its region's monad, which the match's type states.
#[test]
fn a_default_reached_from_a_nested_dispatch_sequences_in_the_matchs_monad() {
    let source = r#"
        use /std/{Io, Nat, Option, print};
        let h(m : Option(Option(Nat))) -> Io({}) =
            match m
            | some(some(_)) => print("deep\n")
            | _ =>
                let _ = print("a\n")!;
                print("b\n")
            end;
        let _ = h(Option/some(Option/none()))!;
        h(Option/some(Option/some(1)))
        "#;

    assert_eq!(run(source), b"a\nb\ndeep\n");
}

// With no type to be checked at, it is read once the arms have said what the match answers.
#[test]
fn a_default_reached_from_a_nested_dispatch_is_typed_after_the_arms_in_inference() {
    let source = r#"
        use /std/{Int, Option};
        let f(m : Option(Option(Int))) -> Int =
            let answer = match m | some(some(x)) => x | _ => 0 end;
            answer;
        /std/print(Int/to_str(f(Option/some(Option/some(-7))) + f(Option/none())))
        "#;

    assert_eq!(run(source), b"-7");
}
