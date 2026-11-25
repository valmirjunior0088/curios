//! How an infix resolves: against a literal's or a variable's type, to a user type's witness or a local premise, and the refusals that name what is missing.

use crate::tests::test_support::*;

// A default is a literal's. An operator with no literal operand takes none: its operand type waits for whatever pins it, here the line after, and where nothing does the program is refused at the parameter nothing typed, not read at `Nat`.
#[test]
fn an_operator_with_no_literal_operand_takes_no_default() {
    assert_eq!(
        run(r#"
        use /std/{Byte, Bool, print};
        let id(@A: Type, a: A) -> A = a;
        let same = id((x) => x == x);
        let at_byte: (Byte) -> Bool = same;
        print(Bool/to_str(at_byte(7)))
        "#),
        b"true"
    );

    let report = error(
        r#"
        use /std/{Bool};
        let id(@A: Type, a: A) -> A = a;
        let same = id((x) => x == x);
        /std/print("unreachable")
        "#,
    );
    assert!(
        report.contains("the type of the parameter `x` was never determined"),
        "unexpected report:\n{report}"
    );
}

#[test]
fn infix_resolves_to_int_for_signed_literals() {
    // Both operands carry a written sign, so the operand type defaults to `Int`.
    assert_eq!(
        run(r#"
            use /std/{Str, Int, Io};
            let entry() -> /std/Io({}) =
                let _ = /std/print(Int/to_str(-2 - +3))!;
                /std/Io/pure(());
            entry()
        "#),
        b"-5"
    );
}

#[test]
fn infix_resolves_against_a_bound_variable_type() {
    // A bare literal takes the type of the variable it is combined with: here `x : Int`, so `x + 1` is `Int` addition, not a `Nat`/`Int` mismatch.
    assert_eq!(
        run(r#"
            use /std/{Str, Int, Io};
            let x : Int = -10;
            let entry() -> /std/Io({}) =
                let _ = /std/print(Int/to_str(x + 1))!;
                /std/Io/pure(());
            entry()
        "#),
        b"-9"
    );
}

#[test]
fn infix_mixes_a_float_variable_with_an_integer_literal() {
    // `x : Flt` pins the operand type, so the bare literal `1` in `x + 1` becomes a `Flt`, which `Flt/lt` then compares with the `Flt` literal `4.5`.
    assert_eq!(
        run(r#"
            use /std/{Str, Flt, Io};
            let x : Flt = 3.0;
            let entry() -> /std/Io({}) =
                let _ = (match Flt/lt(x + 1, 4.5)
                | true => /std/print("less")
                | false => /std/print("more")
                end)!;
                /std/Io/pure(());
            entry()
        "#),
        b"less"
    );
}

#[test]
fn infix_undefined_operator_for_type_is_rejected() {
    // `&&` only has a witness on `Bool`, so `nat && nat` has no `And(Nat)` — a compile-time error, same as any other operator at an un-witnessed type.
    let source = r#"
        use /std/{Str, Io};
        let entry() -> /std/Io({}) =
            let _ = (match 1 && 2
            | true => /std/print("yes")
            | false => /std/print("no")
            end)!;
            /std/Io/pure(());
        entry()
    "#;
    error(source);
}

#[test]
fn infix_mismatched_operand_types_are_rejected() {
    // No implicit coercion: a `Nat` and an `Int` cannot be added.
    let source = r#"
        use /std/{Str, Int, Io};
        let n : Nat = 1;
        let i : Int = -1;
        let entry() -> /std/Io({}) =
            let _ = /std/print(Int/to_str(n + i))!;
            /std/Io/pure(());
        entry()
    "#;
    error(source);
}

// === Concept-dispatched operators (one path, no overload table) =============

// `+` on a user struct resolves the user's `Add` witness — the operator surface is witness-governed.
#[test]
fn infix_add_on_a_user_record_resolves_its_witness() {
    assert_eq!(
        run(r#"
            use /std/{Nat, Str, Io};
            use /std/ops/{Add};
            struct Point : pub Type { x : Nat, y : Nat }
            satisfy Add(Point) {
                add(a, b) = Point { x = a.x + b.x, y = a.y + b.y }
            }
            let p : Point = Point { x = 3, y = 4 };
            let q : Point = p + p;
            let entry() -> /std/Io({}) =
                let _ = /std/print(Nat/to_str(q.y))!;
                /std/Io/pure(());
            entry()
        "#),
        b"8"
    );
}

// `Flt` answers no comparison operator: IEEE's `<` is no total order, false between a NaN and anything, so `<` between two floats is refused for want of a witness, as `==` between them is, and the control compares them by name. Mutation-checked: with `Flt`'s witness of the comparisons put back, the refusal is gone.
#[test]
fn a_comparison_of_two_floats_is_refused_as_their_equality_is() {
    let refusal = error(
        r#"
        use /std/{Flt, Bool};
        let below(a: Flt, b: Flt) -> Bool = a < b;
        /std/print("")
        "#,
    );
    assert!(
        refusal.contains("no witness") && refusal.contains("(Flt)"),
        "{refusal}"
    );
    assert_eq!(
        run(r#"
            use /std/{Flt, Bool, print};
            print(Bool/to_str(Flt/lt(1.0, 2.0)))
        "#),
        b"true"
    );
}

// `a > b` is `lt(b, a)`, read once the lowering has hoisted each operand's `!` in the order written, so the effects run as the program writes them, `f` before `g`, whatever order the operator reads its operands in.
#[test]
fn a_swapped_comparison_runs_its_operands_in_written_order() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Bool, Str, print};
        let f() -> /std/Io(Nat) = let _ = print("f ")!; /std/Io/pure(1);
        let g() -> /std/Io(Nat) = let _ = print("g ")!; /std/Io/pure(2);
        let entry() -> /std/Io({}) = print(Bool/to_str(f()! > g()!));
        entry()
        "#),
        b"f g false"
    );
}

// A refused `>` names the operator written, not the method it reads.
#[test]
fn a_refused_swapped_comparison_names_its_own_operator() {
    let message = error(
        r#"
        use /std/{Nat, Bool, Str, print};
        struct Point: pub Type { x: Nat }
        let p: Point = Point { x = 1 };
        print(Bool/to_str(p > p))
        "#,
    );
    assert!(message.contains("`>` operator"), "{message}");
}

// `!=` is the negation of the `eql` a witness states, with no method of its own to disagree with it: a witness answering `true` at every pair, as the law has it on a type of one value, answers `false` for `!=` at every pair. A `!=` reading `eql` as it stands breaks every bound `/std` states over `!=` before any test runs, so the claim is held here by the fixture alone, which the shape with a `neq` of its own refused for the method it did not write.
#[test]
fn a_disequality_is_the_negation_of_its_witness_s_equality() {
    assert_eq!(
        run(r#"
            use /std/{Bool, Eq, print};
            use /std/Bool/{True};
            use /std/ops/{Eql};
            induct Any : pub Type | any() end
            satisfy Eql(Any) {
                eql(_, _) = true,
                eq(a, b, _) = match a | any() => match b | any() => Eq/refl() end end,
                refl(_) = True/qed(),
            }
            let a : Any = Any/any();
            let b : Any = Any/any();
            print(Bool/to_str(a != b))
        "#),
        b"false"
    );
}

// In generic code, `x + x` resolves against the local `use Add(A)` premise, which resolution tries before the table — no extra machinery.
#[test]
fn infix_resolves_against_a_local_use_premise() {
    assert_eq!(
        run(r#"
            use /std/{Nat, Str, Io};
            use /std/ops/{Add};
            pub let double(@A : Type, use Add(A), x : A) -> A = x + x;
            let entry() -> /std/Io({}) =
                let _ = /std/print(Nat/to_str(double(21)))!;
                /std/Io/pure(());
            entry()
        "#),
        b"42"
    );
}

// An operator at a type with no witness is a missing-witness diagnostic — the single operator error vocabulary (`operator ... not defined` survives only for the exotic no-prelude embedding, where the concept itself is absent from the context).
#[test]
fn infix_without_witness_reports_no_witness() {
    let source = r#"
        use /std/{Bool, Str, Io};
        let b : Bool = true + false;
        let entry() -> /std/Io({}) =
            let _ = /std/print(Bool/to_str(b))!;
            /std/Io/pure(());
        entry()
    "#;
    let error = error(source);
    assert!(error.contains("no witness"), "unexpected error: {error}");
}

// A literal mixed with a witnessed user type still errors on the literal: `p` pins the operand type to `Point`, and `1` has no `Point` realization.
#[test]
fn infix_literal_against_a_user_type_is_rejected() {
    let source = r#"
        use /std/{Nat, Str, Io};
        use /std/ops/{Add};
        struct Point : pub Type { x : Nat, y : Nat }
        satisfy Add(Point) {
            add(a, b) = Point { x = a.x + b.x, y = a.y + b.y }
        }
        let p : Point = Point { x = 1, y = 2 };
        let q : Point = p + 1;
        let entry() -> /std/Io({}) =
            let _ = /std/print(Nat/to_str(q.x))!;
            /std/Io/pure(());
        entry()
    "#;
    error(source);
}

// A literal refused against the operand type names that type, not the placeholder it arrived through: the shared operand type is a metavariable solved to `Str` by the time `1` is checked, and the report renders the solution rather than `?`.
#[test]
fn infix_literal_against_a_solved_operand_type_names_the_type() {
    let source = r#"
        use /std/{Nat, Str, Io};
        let s : Str = "a" + 1;
        let entry() -> /std/Io({}) =
            let _ = /std/print(s)!;
            /std/Io/pure(());
        entry()
    "#;
    let error = error(source);
    assert!(
        error.contains("inferred: `Nat`") && error.contains("expected: `Str`"),
        "unexpected error: {error}"
    );
}

// An operator's witness is resolved where it stands, both operands read and no part of an operator naming a dictionary: parked for the operator's completion it is answered under a frame that settles each guard's scrutinee again, and this function costs four times the budget.
#[test]
fn infix_under_a_guard_takes_its_witness_where_it_stands() {
    let source = r#"
        use /std/{Nat, Bool};
        let guarded(a: Nat, b: Nat, c: Nat) -> Nat =
            match (a * b + c) % 7 <= (a + b * c) % 5
            | true =>
                match (a + c) / 3 <= (b * b + a) % 11
                | true =>
                    (a + b) * (b + c) + (c + a) * (a + b) + (a * a + b * b) * (c + 1)
                        + (a + 2 * b) * (b + 3 * c) + (a * b + b * c + c * a) % 13
                        + (a + b + c) / 2 + (a * c + 5) * (b + 7) + (a + b * 9) % 17
                        + (a + b) * (b + c) + (c + a) * (a + b) + (a * a + b * b) * (c + 1)
                        + (a + 2 * b) * (b + 3 * c) + (a * b + b * c + c * a) % 13
                        + (a + b + c) / 2 + (a * c + 5) * (b + 7) + (a + b * 9) % 17
                | false => 0
                end
            | false => 0
            end;
        /std/print("ok")
    "#;

    typecheck_within(120_000, source).unwrap();
}

/// `+` is no operator of text, and the refusal says what is: the function that joins two, and the one that builds a text from a format.
#[test]
fn adding_two_texts_is_told_what_joins_them() {
    let refusal = error(
        r#"
        use /std/{Str, Fmt, print};
        let name = "world";
        print("Hello, " + name + "!\n")
        "#,
    );

    assert!(
        refusal.contains("no witness") && refusal.contains("Add(Str)"),
        "expected the missing witness, got: {refusal}"
    );
    assert!(
        refusal
            .contains("`Str/concat` joins two texts\n│  `Fmt/render` builds a text from a format"),
        "expected what is written instead, got: {refusal}"
    );
}

/// The same operator at a type of the program's own names no function of text: what joins two of them is its author's to say.
#[test]
fn adding_two_values_of_another_type_names_no_function_of_text() {
    let refusal = error(
        r#"
        use /std/{Io};
        struct Point: Type { x: /std/Nat }
        let p = Point { x = 1 };
        let q = p + p;
        Io/pure(())
        "#,
    );

    assert!(refusal.contains("no witness"), "{refusal}");
    assert!(!refusal.contains("joins two texts"), "{refusal}");
}
