//! Plicity at a constructor pattern: an implicit payload matched, a hidden one left out or written under its mark, and the binders that claim no slot.

use crate::tests::{error, run};

// A constructor pattern names an implicit payload under `@`, ahead of the plain payload it precedes. `Vec/cons` declares its length index implicit, so the arm that names it binds it with `@`.
#[test]
fn constructor_pattern_matches_an_implicit_payload() {
    let source = r#"
        use /std/{Nat, Str};
        induct Vec(T : Type) : (n : Nat) -> pub Type
        | nil() : (0)
        | cons(@n : Nat, head : T, tail : Vec(T)(n)) : (n + 1)
        end
        let head3(v : Vec(Nat)(3)) -> Nat =
            match v : (_, _) => Nat
            | cons(@m, x, xs) => x
            end;
        /std/print(Nat/to_str(head3(Vec/cons(1, Vec/cons(2, Vec/cons(3, Vec/nil()))))))
        "#;
    assert_eq!(run(source), b"1");
}

// A hidden payload takes no position in a pattern: the arm writes the plain payloads, as the call that builds the value does, and the length is bound all the same.
#[test]
fn constructor_pattern_leaves_a_hidden_payload_out() {
    let source = r#"
        use /std/{Nat, Str};
        induct Vec(T : Type) : (n : Nat) -> pub Type
        | nil() : (0)
        | cons(@n : Nat, head : T, tail : Vec(T)(n)) : (n + 1)
        end
        let head3(v : Vec(Nat)(3)) -> Nat =
            match v : (_, _) => Nat
            | cons(x, xs) => x
            end;
        /std/print(Nat/to_str(head3(Vec/cons(1, Vec/cons(2, Vec/cons(3, Vec/nil()))))))
        "#;
    assert_eq!(run(source), b"1");
}

// A constructor whose every payload is hidden is matched as it is built, by its name alone: `refl()` binds the value the equality's two sides share, and the arm is checked with the two identified.
#[test]
fn constructor_pattern_of_hidden_payloads_alone_writes_none() {
    let source = r#"
        use /std/{Nat, Str, Eq, print};
        let flip(@x: Nat, @y: Nat, p: Eq()(x, y)) -> Eq()(y, x) =
            match p | refl() => Eq/refl() end;
        let carried(@x: Nat, @y: Nat, p: Eq()(x, y), n: Nat) -> Nat =
            match flip(p) | refl() => n end;
        print(Nat/to_str(carried(@4, @4, Eq/refl(), 7)))
        "#;
    assert_eq!(run(source), b"7");
}

// A hidden payload left out is in scope as its mark makes it: the bound the constructor states is a fact in the arm, and the call there is discharged by it.
#[test]
fn a_hidden_payload_left_out_is_a_fact_in_the_arm() {
    let source = r#"
        use /std/{Nat, Str, print};
        use /std/Bool/{Holds};
        induct Positive: pub Type
        | at(n: Nat, @Holds(0 < n))
        end
        let halve(n: Nat, @_: Holds(0 < n)) -> Nat = n / 2;
        let below(p: Positive) -> Nat = match p | at(n) => halve(n) end;
        print(Nat/to_str(below(Positive/at(4))))
        "#;
    assert_eq!(run(source), b"2");
}

// A plain binder never binds a hidden payload: against `cons(@n, head, tail)` three plain binders are one more than the plain payloads, and the count names those, which are the only ones an arm must write.
#[test]
fn constructor_pattern_plain_binder_never_binds_a_hidden_payload() {
    let source = r#"
        use /std/{Nat, Str};
        induct Vec(T : Type) : (n : Nat) -> pub Type
        | nil() : (0)
        | cons(@n : Nat, head : T, tail : Vec(T)(n)) : (n + 1)
        end
        let head3(v : Vec(Nat)(3)) -> Nat =
            match v : (_, _) => Nat
            | cons(m, x, xs) => x
            end;
        /std/print(Nat/to_str(head3(Vec/cons(1, Vec/cons(2, Vec/cons(3, Vec/nil()))))))
        "#;
    assert!(
        error(source).contains("constructor 'cons' takes 2 argument(s) but the match arm binds 3"),
        "{}",
        error(source)
    );
}

// A marked binder written after the plain payloads has no slot: the hidden payloads stand ahead of the plain one they precede, and none follows the last.
#[test]
fn constructor_pattern_hidden_binder_past_its_run_has_no_slot() {
    let source = r#"
        use /std/{Nat, Str};
        induct Vec(T : Type) : (n : Nat) -> pub Type
        | nil() : (0)
        | cons(@n : Nat, head : T, tail : Vec(T)(n)) : (n + 1)
        end
        let head3(v : Vec(Nat)(3)) -> Nat =
            match v : (_, _) => Nat
            | cons(x, xs, @m) => x
            end;
        /std/print(Nat/to_str(head3(Vec/cons(1, Vec/cons(2, Vec/cons(3, Vec/nil()))))))
        "#;
    assert!(
        error(source).contains("this `@` member has no slot"),
        "{}",
        error(source)
    );
}

// Rows of one constructor meet its payloads each by its own marks: a row that names the length stands beside one that leaves it out.
#[test]
fn constructor_pattern_rows_of_one_constructor_write_their_own_hidden_payloads() {
    let source = r#"
        use /std/{Nat, Str, Option};
        induct Vec(T : Type) : (n : Nat) -> pub Type
        | nil() : (0)
        | cons(@n : Nat, head : T, tail : Vec(T)(n)) : (n + 1)
        end
        let first(@n : Nat, v : Vec(Nat)(n), o : Option(Nat)) -> Nat =
            match (v, o)
            | (cons(@m, x, xs), some(y)) => x + y
            | (cons(x, xs), none()) => x
            | (nil(), some(y)) => y
            | (nil(), none()) => 0
            end;
        /std/print(Nat/to_str(first(Vec/cons(4, Vec/nil()), Option/none())))
        "#;

    assert_eq!(run(source), b"4");
}

// Marking a plain payload slot with `@` is likewise rejected.
#[test]
fn constructor_pattern_mark_on_explicit_payload_is_rejected() {
    let source = r#"
        use /std/{Nat, Str};
        induct Vec(T : Type) : (n : Nat) -> pub Type
        | nil() : (0)
        | cons(@n : Nat, head : T, tail : Vec(T)(n)) : (n + 1)
        end
        let head3(v : Vec(Nat)(3)) -> Nat =
            match v : (_, _) => Nat
            | cons(@m, @x, xs) => x
            end;
        /std/print(Nat/to_str(head3(Vec/cons(1, Vec/cons(2, Vec/cons(3, Vec/nil()))))))
        "#;
    assert!(
        error(source).contains(
            "payload 2 of 'cons' is an explicit payload (written with no mark), but was written with `@`"
        ),
        "{}",
        error(source)
    );
}
