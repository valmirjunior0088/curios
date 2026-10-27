//! A problem nothing wakes, and one the drain decides: what each refusal names, and an argument held to the value its call's type states.

use crate::tests::error;

// A lambda whose expectation never gains structure settles by synthesizing its own type — annotations state what they state, and an unannotated domain stands as a metavariable for the body, or whatever the settled type later meets, to pin. `(x) => x` pins nothing anywhere, so the survivor is the domain itself, and it is reported as the parameter it is rather than as an internal expectation.
#[test]
fn a_domain_nothing_pins_is_reported_as_its_parameter() {
    let source = r#"
        use /std/{Nat, Str};
        let use_it(@A : Type, a : A) -> Nat = 0;
        let z : Nat = use_it((x) => x);
        /std/print(Nat/to_str(z))
        "#;
    let error = error(source);
    assert!(
        error.contains("the type of parameter 'x' was never determined"),
        "{error}"
    );
}

// The same refusal where nothing settles at all: a lambda bound by a `let` that states no type is synthesized on the spot, and its unannotated parameter is named there too rather than reported as an expression whose type cannot be inferred.
#[test]
fn an_unannotated_parameter_of_a_let_bound_lambda_is_reported_by_name() {
    let source = r#"
        use /std/{Nat};
        let g = (x) => x;
        /std/print(Nat/to_str(g(1)))
        "#;
    let error = error(source);
    assert!(
        error.contains("the type of parameter 'x' was never determined"),
        "{error}"
    );
}

// A postponement whose blocker is itself blocked. `List/slice` carries a `Holds(start + length <= len)` bound; undischarged inside `bad`, its proof metavariable rides into the candidate for `resize`'s implicit length when the reducer unfolds `bad`'s body, and `Convert::solve`'s embedded-metavariable guard postpones that candidate rather than committing a solution of a wider context. The drain follows the recorded blocking edges to the end of the chain and reports what nothing was ever going to solve, rather than the goal that merely waited — which would name an implicit the author never wrote (`the implicit argument 'n' of '/resize'`), at the `resize` call rather than at the bound, without mentioning `List/slice` at all.
#[test]
fn a_postponement_reports_the_bound_its_blocker_never_discharged() {
    let source = r#"
        use /std/{Nat, List};

        induct Vec(T: Type): (n: Nat) -> pub Type
        | nil(): (0)
        | cons(@n: Nat, head: T, tail: Vec(T)(n)): (n + 1)
        end

        let of_list(@T: Type, l: List(T)) -> {n: Nat, Vec(T)(n)} =
            match l | [] => (0, Vec/nil()) | [x, .._]; (m, v) => (m + 1, Vec/cons(x, v)) end;

        let resize(@T: Type, fill: T, m: Nat, @n: Nat, v: Vec(T)(n)) -> Vec(T)(m) =
            (match m: (k) => (j: Nat, Vec(T)(j)) -> Vec(T)(k)
            | 0 => (j, x) => Vec/nil()
            | p + 1; ih => (j, x) =>
                match x
                | nil() => Vec/cons(fill, ih(0, Vec/nil()))
                | cons(@q, y, ys) => Vec/cons(y, ih(q, ys))
                end
            end)(n, v);

        let cascade(l: List(Nat), k: Nat, w: Nat) -> Vec(Nat)(w) =
            let bad(u: List(Nat)) -> List(Nat) = List/slice(u, k, 3);
            let paired = of_list(bad(l));
            resize(0, w, paired.1);

        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("'within' of 'List/slice'") && report.contains("nothing discharged"),
        "the report should name the bound nothing discharged, got: {report}"
    );
    assert!(
        !report.contains("postponed conversion"),
        "the waiting goal should not be reported in place of its cause, got: {report}"
    );
}

// A conversion parked between two metavariables that never solve reports the metavariables and nothing else: `between: ?` and `and: ?` name nothing a reader can act on, so the report leads with the implicit that was never inferred, as the `List/len([])` report does.
#[test]
fn a_postponed_conversion_between_two_holes_names_only_what_never_solved() {
    let report = error(
        r#"
        use /std/{Nat, Eq, Result};
        pub let probe(f: (Nat) -> Nat, x: Nat) -> Eq()(Result/map(Result/success(x), f), Result/success(f(x))) = Eq/refl();
        /std/print("ok")
        "#,
    );
    assert!(
        !report.contains("between: ?")
            && report.contains("never solved: the implicit argument 'E'"),
        "unexpected report:\n{report}"
    );
}

// A projection whose head's type nothing decided is refused where it is read, at the field: a destructuring lowers each field to a projection located at the field's pattern rather than at the `let` of the value it destructures. Mutation-checked: lowering the projections unlocated reports the line of `let p`.
#[test]
fn a_projection_nothing_types_is_refused_at_the_field_it_reads() {
    let report = error(
        r#"
        use /std/{Nat};
        let g(n: Nat) -> Nat =
            let p = ?;
            let (a, b) = p;
            a;
        /std/print("unreachable")
        "#,
    );
    assert!(
        report.contains("projected from a non-tuple") && report.contains("let (a, b) = p;"),
        "the report should point at the destructuring:\n{report}"
    );
}

// A match whose scrutinee's type nothing decided is refused where it is read, as its eliminator refuses a scrutinee of any other type: the report names the carrier the match expected.
#[test]
fn a_match_nothing_types_is_refused_as_its_eliminator_refuses() {
    let report = error(
        r#"
        use /std/{Nat};
        let g(n: Nat) -> Nat =
            let p = ?;
            match p | [] => 0 | [_, .._] => 1 end;
        /std/print("unreachable")
        "#,
    );
    assert!(
        report.contains("expected List but got"),
        "the report should name the carrier the match expected:\n{report}"
    );
}

// The two lambdas have their type where `Eq()`'s call completes, so the declared type is whole before the body is read, and reflexivity is refused between two whole types: the fault keeps a mismatch's words, with both sides spelled.
#[test]
fn reflexivity_at_two_lambdas_that_differ_is_a_mismatch() {
    let report = error(
        r#"
        use /std/{Eq, Nat};
        let differ(g : (Nat) -> Nat) -> Eq()((x : Nat) => g(0), (x : Nat) => g(1)) = Eq/refl();
        /std/print("unreachable")
        "#,
    );
    assert!(
        report.contains("type mismatch") && report.contains("g(0)") && report.contains("g(1)"),
        "{report}"
    );
}

// A parked problem that fails where its item is drained is the item's refusal as much as one raised while the item is elaborated: the report leads with the declaration. And what failed is a comparison of two terms the declared type states at one position, neither a type and neither inferred, so the report says that two terms differ and names the one the program wrote as written; with the position's type written out the same fault is one between two whole types, and keeps a mismatch's words.
#[test]
fn a_comparison_that_fails_at_the_drain_names_its_declaration_and_what_was_compared() {
    let boxed = error(
        r#"
        use /std/{Nat};
        induct Boxed(@A : Type) : (A) -> Type
        | box(z : A) : (z)
        end
        let boxed : Boxed()((n : Nat) => n + 7) = Boxed/box((n : Nat) => n + 1);
        /std/print("unreachable")
        "#,
    );
    assert!(boxed.starts_with("while elaborating boxed:"), "{boxed}");
    assert!(
        boxed.contains("terms differ") && boxed.contains("written: (n) => n + 1"),
        "{boxed}"
    );

    let typed = error(
        r#"
        use /std/{List, Nat};
        induct Tagged(@A : Type) : (A) -> Type
        | tag(@z : A) : (z)
        end
        let relevant : Tagged()([7, 8, 9]) = Tagged/tag(@List(Nat), @[1]);
        /std/print("unreachable")
        "#,
    );
    assert!(
        typed.contains("type mismatch") && typed.contains("inferred: Tagged(@List(Nat))([1])"),
        "{typed}"
    );
}

// A refusal raised where the entry is drained undoes what the entry recorded, as one raised at an item's drain does. The passes over the recorded terms then read nothing of a refused entry, so the implicit its call leaves open is reported once, at the call, and not again from its callee's declaration by each of those passes.
#[test]
fn a_refusal_at_the_entrys_drain_is_reported_once() {
    let report = error(
        r#"
        use /std/{Process};
        let entry() -> /std/Io({}) =
            let _ = Process/exit(1)!;
            /std/print("unreachable");
        entry()
        "#,
    );
    assert!(
        report.contains("never solved: ")
            && report.contains("the implicit argument 'A' of 'Process/exit'"),
        "{report}"
    );
    assert!(!report.contains("was not inferred"), "{report}");
}

// A written argument is checked as written, and then held to its call's type. A tuple at a slot whose type is still a metavariable waits behind a placeholder, as a numeral does in a list read where it stands, and the call's expected type states the slot's value before the argument is checked. Conversion waits on the placeholder as it waits on a recursive group's slot, so nothing but the argument's own check fills it, and the value the type states meets the argument that was written. Mutation-checked: letting conversion solve a placeholder accepts both, with the type's value standing where the argument was written.
#[test]
fn a_hidden_argument_is_held_to_the_value_its_calls_type_states() {
    let tagged = error(
        r#"
        use /std/{List, Nat};
        induct Tagged(@A : Type) : (A) -> Type
        | tag(@z : A) : (z)
        end
        let relevant : Tagged()([7]) = Tagged/tag(@_, @[1]);
        /std/print("unreachable")
        "#,
    );
    assert!(tagged.contains("terms differ"), "{tagged}");

    let reflexive = error(
        r#"
        use /std/{Eq, List, Nat};
        let also(x : Nat, y : Nat) -> Eq()((x, x), (x, x)) = Eq/refl(@_, @(y, y));
        /std/print("unreachable")
        "#,
    );
    assert!(reflexive.contains("terms differ"), "{reflexive}");
}

// A plain argument as well, where what would stand in its place is a value the program runs with. Mutation-checked, as above: the payload read back is the type's.
#[test]
fn a_plain_argument_is_held_to_the_value_its_calls_type_states() {
    let list = error(
        r#"
        use /std/{List, Nat};
        induct Boxed(@A : Type) : (A) -> Type
        | box(z : A) : (z)
        end
        let boxed : Boxed()([7]) = Boxed/box([1]);
        /std/print("unreachable")
        "#,
    );
    assert!(list.contains("terms differ"), "{list}");

    let tuple = error(
        r#"
        use /std/{Nat};
        induct Boxed(@A : Type) : (A) -> Type
        | box(z : A) : (z)
        end
        let boxed : Boxed()((7, 8)) = Boxed/box((1, 2));
        /std/print("unreachable")
        "#,
    );
    assert!(tuple.contains("terms differ"), "{tuple}");
}

// A literal that parks inside an argument — a match arm's tuple, a `let` body's — is run by its retry or by the completion of the term that awaits its type, and is held to the call's type as the argument itself is. Mutation-checked, as above.
#[test]
fn a_literal_inside_an_argument_is_held_to_the_value_its_calls_type_states() {
    let arm = error(
        r#"
        use /std/{Nat, Bool};
        induct Boxed(@A : Type) : (A) -> Type
        | box(z : A) : (z)
        end
        let boxed : Boxed()((7, 8)) =
            Boxed/box(match true | true => (1, 2) | false => (3, 4) end);
        /std/print("unreachable")
        "#,
    );
    assert!(arm.contains("terms differ"), "{arm}");

    let body = error(
        r#"
        use /std/{Nat};
        induct Boxed(@A : Type) : (A) -> Type
        | box(z : A) : (z)
        end
        let boxed : Boxed()((7, 8)) = Boxed/box(let t = 1; (t, 2));
        /std/print("unreachable")
        "#,
    );
    assert!(body.contains("terms differ"), "{body}");
}
