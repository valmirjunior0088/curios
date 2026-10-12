//! The tuple literal, which is checked-only and therefore lives or dies by its expectation.
//!
//! Three rules meet here. A literal whose expectation never gains structure takes the non-dependent product where the term that awaits its type has read its parts, because by then nothing is left to send a dependent telescope. Two literals that wait on one type are one shape there, each filling what the other left open, whichever is read first. A literal whose expectation is merely *stuck* — a `rec` waiting on an index it has not been given — waits instead, since a solution is still coming. The refusals matter as much as the acceptances: a rigid non-tuple expectation and a rigidly stuck one both refuse at the literal.

use crate::tests::{error, run};

// A tuple argument that is the *only* thing determining a call's type variable, where the result type does not mention it — so the apply's turnaround pins nothing and the literal is left holding its own expectation. The call gives it its shape where it completes, rather than leaving it for the item's drain.
#[test]
fn a_tuple_argument_no_caller_pins_settles_to_its_product() {
    let source = r#"
        use /std/{Nat, Io};
        let swallow(@A : Type, a : A) -> Nat = 0;
        let n : Nat = swallow((1, 2));
        let _ = Io/write(Io/stdout, /std/Str/to_bytes(Nat/to_str(n)))!;
        /std/Io/pure(())
        "#;

    assert_eq!(run(source), b"0");
}

// A non-empty tuple literal parks against a bare expected metavariable, and rightly: a dependent telescope can only ever arrive from the expectation, so committing to the non-dependent product while one could still arrive would be a guess. When the drain has established that nothing is left to send one, the guess is no longer a guess — and `?` must answer, as it already does for `()` and for a list literal.
#[test]
fn a_tuple_literal_synthesizes_when_its_expected_type_never_gains_structure() {
    let source = r#"
        use /std/{Nat, Bool};
        let y : ? = (1, true);
        /std/print("ok\n")
        "#;

    let report = error(source);
    assert!(
        report.contains("? = {Nat, Bool}"),
        "expected the goal to report the synthesized product:\n{report}"
    );
}

// The one-field literal takes the same route. Its trailing comma is all that separates it from a parenthesized term, so a reader has no other way to learn which one the elaborator saw.
#[test]
fn a_one_field_tuple_literal_synthesizes_against_a_written_goal() {
    let source = r#"
        use /std/{Nat};
        let y : ? = (1,);
        /std/print("ok\n")
        "#;

    let report = error(source);
    assert!(
        report.contains("? = {Nat}"),
        "expected a one-field product:\n{report}"
    );
}

// Settling the literal wakes whatever was parked on the metavariable it solved, and a woken obligation reports for itself. Here that is the missing tuple witness — the answer the program deserves, rather than that some type never gained structure.
//
// The literal is *labeled* so the woken goal is one that genuinely misses: `/std/Tuple` shows every positional shape, and labels are part of a tuple type's identity. Synthesis from a labeled literal solves the metavariable exactly as a positional one does, so the mechanism under test is untouched.
#[test]
fn a_settled_tuple_reports_the_obligation_it_unblocked() {
    let source = r#"
        use /std/{Bool, Show, Str};
        let s : Str = Show/show((x = true, y = false));
        /std/print("ok\n")
        "#;

    let report = error(source);
    assert!(
        report.contains("no witness of Show({x: Bool, y: Bool})"),
        "expected the witness goal the tuple unblocked:\n{report}"
    );
}

// An expectation that *does* arrive must still win: the product here is written, and the literal is checked against it rather than synthesized.
#[test]
fn a_written_tuple_type_still_checks_the_literal_against_itself() {
    let source = r#"
        use /std/{Nat, Bool, Io};
        let id(@A : Type, a : A) -> A = a;
        let z : {Nat, Bool} = id((1, true));
        /std/print(Nat/to_str(z.0))
        "#;

    assert_eq!(run(source), b"1");
}

// A description-indexed constructor written the way a user writes one, with the index left implicit. The payload's type is `Count(?L)` — a `rec` that cannot choose an arm until `?L` is known, so reduction hands back the folded call itself. That is not a tuple type *yet*, and refusing there refuses one step before the turnaround that solves `?L` from the written result type.
#[test]
fn a_tuple_payload_waits_for_the_index_that_types_it() {
    let source = r#"
        use /std/{Nat, Str, Io};
        induct Labels : pub Type | nil() | cons(Str, Labels) end
        let Count(L : Labels) -> Type =
            match L : (_) => Type
            | nil() => {}
            | cons(l, rest) => {Nat, Count(rest)}
            end;
        induct Boxed(L : Labels) : pub Type | mk(Count(L)) end
        let b : Boxed(Labels/cons("a", Labels/nil())) = Boxed/mk((1, ()));
        match b | mk(_) => /std/print("unlocked") end
        "#;

    assert_eq!(run(source), b"unlocked");
}

// The blocker has to be a *metavariable*, not merely a stuck reduction. `Count(L)` over a bound parameter is stuck for good — no solution is coming — so it refuses at the literal with the expectation named, rather than parking into a report about structure that never arrived.
#[test]
fn a_payload_stuck_on_a_rigid_index_is_refused_at_the_literal() {
    let source = r#"
        use /std/{Nat, Str, Io};
        induct Labels : pub Type | nil() | cons(Str, Labels) end
        let Count(L : Labels) -> Type =
            match L : (_) => Type
            | nil() => {}
            | cons(l, rest) => {Nat, Count(rest)}
            end;
        let f(L : Labels) -> Nat =
            let x : Count(L) = (1, ());
            0;
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("expected type is not a tuple type") && report.contains("Count(L)"),
        "expected the refusal to name the stuck expectation:\n{report}"
    );
}

// Waiting also sharpens the wrong program's report: once the index solves, the payload type reduces and the literal is measured against it, so a mismatched shape is named as the arity it is instead of as a type that never became a tuple.
#[test]
fn a_settled_index_measures_the_literal_against_the_type_it_computes() {
    let source = r#"
        use /std/{Nat, Str, Io};
        induct Labels : pub Type | nil() | cons(Str, Labels) end
        let Count(L : Labels) -> Type =
            match L : (_) => Type
            | nil() => {}
            | cons(l, rest) => {Nat, Count(rest)}
            end;
        induct Boxed(L : Labels) : pub Type | mk(Count(L)) end
        let b : Boxed(Labels/nil()) = Boxed/mk((1, ()));
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("tuple has 2 field(s) but expected type has 0"),
        "expected the arity report the reduced payload type makes possible:\n{report}"
    );
}

// Labels are part of a tuple type's identity, so a literal synthesized with nothing expecting it must keep the labels it wrote: the product is what its projections are resolved against, and an unlabeled one has no field called `a`.
#[test]
fn a_labeled_literal_synthesizes_with_its_labels() {
    let source = r#"
        use /std/{Nat, Bool, Io};
        let z = (a = 1, b = true);
        /std/print(Nat/to_str(z.a))
        "#;

    assert_eq!(run(source), b"1");
}

// The same product, read back through the oracle: the labels are in the type, not only in the literal.
#[test]
fn a_synthesized_labeled_product_reports_its_labels() {
    let source = r#"
        use /std/{Nat, Bool};
        let y : ? = (a = 1, b = true);
        /std/print("ok\n")
        "#;

    let report = error(source);
    assert!(
        report.contains("? = {a: Nat, b: Bool}"),
        "expected the synthesized product to carry its labels:\n{report}"
    );
}

// An inferred call has no expectation to turn around, so its completion is what gives a tuple argument its shape. Shaped there, `z.0` reads the product at once instead of waiting for the item's end, which is also what commits the call to the product (below).
#[test]
fn a_tuple_argument_settles_inside_an_inferred_call() {
    let source = r#"
        use /std/{Nat, Bool, Io};
        let id(@A : Type, a : A) -> A = a;
        let z = id((1, true));
        /std/print(Nat/to_str(z.0))
        "#;

    assert_eq!(run(source), b"1");
}

// Shaping inside the call commits to the argument's own product, so a later labeled annotation can no longer relabel it — the same commitment a bare `let z = (1, true)` makes. Pinned so the trade is visible: a program relying on a pin from a later line is refused by the same report as the bare literal.
#[test]
fn an_inferred_call_commits_to_the_product_as_a_bare_literal_does() {
    let source = r#"
        use /std/{Nat, Bool};
        let id(@A : Type, a : A) -> A = a;
        let z = id((1, true));
        let w : {a: Nat, b: Bool} = z;
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("inferred: {Nat, Bool}") && report.contains("expected: {a: Nat, b: Bool}"),
        "expected the bare literal's mismatch:\n{report}"
    );
}

/// `body` over the declarations the programs below share, its last line the text printed. Each is written in both orders and run, so the kernel has judged what the elaborator admitted.
fn program(body: &str) -> String {
    format!(
        r#"
        use /std/{{Nat, Int, Byte, Bool, List, Str, Show, print}};
        use /std/Bool/{{True, Holds}};
        let id(@A: Type, a: A) -> A = a;
        let g(@A: Type, x: A, pin: A) -> Nat = 0;
        let first(@A: Type, x: A, pin: A) -> A = x;
        let second(@A: Type, pin: A, x: A) -> A = x;
        struct Pair(T: Type): pub Type {{ a: T, z: T }}
        let b: Byte = 7;
        let c: Bool = true;
        let p: Holds(true) = True/qed();
        let and(s: Str, t: Str) -> Str = Str/concat(s, Str/concat(" ", t));
        let pair(q: {{Byte, Byte}}) -> Str = and(Show/show(q.0), Show/show(q.1));
        {body}
        "#
    )
}

// Two literals at one type are one shape: the numerals of one wait for the fields of the other, through a call of the literal's own as well, and where both hold numerals the two take one default a field.
#[test]
fn two_literals_at_one_type_fill_each_other_whichever_is_read_first() {
    let source = program(
        r#"
        let call = and(pair(first((5, 1), (b, b))), pair(second((b, b), (5, 1))));
        let through = and(pair(first(id((5, 1)), (b, b))), pair(second((b, b), id((5, 1)))));
        let mixed(q: {Int, Byte}) -> Str = and(Int/to_str(q.0), Show/show(q.1));
        let signed = and(mixed(first((5, 1), (-3, b))), mixed(second((-3, b), (5, 1))));
        print(and(call, and(through, signed)))
        "#,
    );

    assert_eq!(run(&source), b"5 1 5 1 5 1 5 1 +5 1 +5 1");
}

// A list's elements, a structure's fields and a match's arms each wait on one type as a call's arguments do.
#[test]
fn literals_in_a_list_a_structure_and_a_match_share_one_shape() {
    let source = program(
        r#"
        let head(ys: List({Byte, Byte})) -> Str = match ys | [] => "none" | [h, ..t] => pair(h) end;
        let literal_first = [(5, 1), (b, b)];
        let literal_last = [(b, b), (5, 1)];
        let leading = Pair { a = (5, 1), z = (b, b) };
        let trailing = Pair { a = (b, b), z = (5, 1) };
        let arm_first = match c | true => (5, 1) | false => (b, b) end;
        let arm_last = match c | true => (b, b) | false => (5, 1) end;
        let listed = and(head(literal_first), head(literal_last));
        let built = and(pair(leading.a), pair(trailing.z));
        let matched = and(pair(arm_first), pair(arm_last));
        print(and(listed, and(built, matched)))
        "#,
    );

    assert_eq!(run(&source), b"5 1 7 7 5 1 5 1 5 1 7 7");
}

// A literal among a literal's fields takes its shape with it, and a list is a list whatever it holds: its numeral waits for the other list's element.
#[test]
fn a_literal_inside_a_literal_is_shaped_with_it() {
    let source = program(
        r#"
        let lone(ys: List(Byte)) -> Str = match ys | [] => "none" | [h, ..t] => Show/show(h) end;
        let nested = and(pair(first(((5, 1), c), ((b, b), c)).0), pair(second(((b, b), c), ((5, 1), c)).0));
        let lists = and(lone(first([5], [b])), lone(second([b], [5])));
        print(and(nested, lists))
        "#,
    );

    assert_eq!(run(&source), b"5 1 5 1 5 5");
}

// A sort among a literal's fields is met by an open type, and takes a level of its own as one that is an argument does: `Nat` and `Type` stand in one field whichever literal is read first.
#[test]
fn a_sort_inside_a_literal_takes_a_level_of_its_own() {
    let source = program(
        r#"
        let sorts = g((Nat, c), (Type, c)) + g((Type, c), (Nat, c));
        print(Nat/to_str(sorts))
        "#,
    );

    assert_eq!(run(&source), b"0");
}

// A lambda among a literal's fields waits on its field's type, which the annotated one beside it gives.
#[test]
fn a_lambda_inside_a_literal_is_typed_by_the_literal_beside_it() {
    let source = program(
        r#"
        let lambdas = g((c, (y) => y), (c, (x: Nat) => x)) + g((c, (x: Nat) => x), (c, (y) => y));
        print(Nat/to_str(lambdas))
        "#,
    );

    assert_eq!(run(&source), b"0");
}

// A product is a proposition where every field is one, which the fields alone say: the literal's fields are read before the product is held to the type it shapes, a literal among them first.
#[test]
fn a_tuple_of_proofs_is_a_proposition_where_it_takes_its_shape() {
    let source = program(
        r#"
        let idp(@P: Prop, q: P) -> P = q;
        let gp(@P: Prop, x: P, pin: P) -> Nat = 0;
        let alone = idp((p, p));
        let nested = idp(((p, p), (p, p)));
        let listed = [(p, p)];
        let chosen = match c | true => (p, p) | false => (p, p) end;
        let beside = id((5, p));
        print(Nat/to_str(gp((p, p), (p, p)) + List/len(listed) + beside.0))
        "#,
    );

    assert_eq!(run(&source), b"6");
}

// And where a type is declared, a tuple of proofs is refused as a proof is, by the elaborator and with the implicit named.
#[test]
fn a_tuple_of_proofs_is_refused_where_a_type_is_declared() {
    let report = error(&program(
        r#"
        let r = id((p, p));
        print("unreachable")
        "#,
    ));

    assert!(
        report.contains(
            "a proposition was given where the implicit argument 'A' of 'id' takes a type"
        ) && report.contains("{True, True} is a proposition"),
        "unexpected report:\n{report}"
    );
}

// A lambda whose every domain is annotated says its type of itself, so among a literal's fields it is typed before the product is held, as a literal there is shaped: a pair of functions into propositions is a proposition where it stands, and is refused by the same report.
#[test]
fn a_tuple_of_functions_into_propositions_is_refused_where_a_type_is_declared() {
    let report = error(&program(
        r#"
        let r = id(((x: Nat) => p, (x: Nat) => p));
        print("unreachable")
        "#,
    ));

    assert!(
        report.contains(
            "a proposition was given where the implicit argument 'A' of 'id' takes a type"
        ) && report
            .contains("is a proposition, and no proposition stands where a Type is declared"),
        "unexpected report:\n{report}"
    );
}

// And at a sort left to inference, a list's element, the pair is a proposition as soon as it is read: the line after meets a list of propositions and takes its length, `List/len` standing over an element at any sort. Read as a type's there, the element would be held to a type by that call and the kernel would refuse it.
#[test]
fn a_pair_of_functions_into_propositions_is_a_proposition_on_the_next_line() {
    let source = program(
        r#"
        let functions = [((x: Nat) => p, (x: Nat) => p)];
        print(Nat/to_str(List/len(functions)))
        "#,
    );

    assert_eq!(run(&source), b"1");
}

// A lambda with a domain left open is typed after the product is held, since a literal beside it may give the domain, so a product of such is read again once the item has settled it.
#[test]
fn a_tuple_of_open_functions_into_propositions_is_refused_where_a_type_is_declared() {
    let report = error(&program(
        r#"
        let r = g(((x) => p, (x) => p), ((x: Nat) => p, (x: Nat) => p));
        print("unreachable")
        "#,
    ));

    assert!(
        report.contains(
            "a proposition was given where the implicit argument 'A' of 'g' takes a type"
        ),
        "unexpected report:\n{report}"
    );
}
