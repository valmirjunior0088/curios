//! A checking problem that cannot proceed yet, and what wakes it.
//!
//! A checked-only form met by an expectation with no structure is postponed rather than judged, and re-checked under its frozen frame once a watched metavariable lands. These pin both halves: the problems an outer pin resolves, and the ones nothing ever will — which must be reported at their own span rather than accepted.

use crate::tests::{error, run};

#[test]
fn parked_constraints_let_nested_constructor_metas_resolve() {
    // `sym2(Eq2/refl())` — the argument's fresh metas meet the domain's fresh metas as flex–flex pairs embedded under the inductive type. Before the constraint store, the argument's `expect` failed at quiescence, seconds before the result-type unification would have pinned everything. Now the pairs park, the output `expect` solves the domain metas against the annotation, and the wake retries the parked pairs.
    let source = r#"
        use /std/{Nat, Handle};
        induct Eq2(@A : Type) : (x : A, y : A) -> Type
        | refl(@z : A) : (z, z)
        end
        let sym2(@A : Type, @x : A, @y : A, p : Eq2(x, y)) -> Eq2(y, x) =
            match p : (s, t, q) => Eq2(t, s)
            | refl(@z) => Eq2/refl()
            end;
        let direct : Eq2(2, 2) = sym2(Eq2/refl());
        let chained : Eq2(3, 3) = sym2(sym2(Eq2/refl()));
        match chained : (_, _, _) => /std/Io({})
        | refl(@z) => let _ = Handle/write(Handle/stdout, /std/Str/to_bytes(Nat/to_str(z)))!; /std/Io/pure(())
        end
        "#;

    assert_eq!(run(source), b"3");
}

#[test]
fn a_tuple_pattern_in_a_later_parameter_waits_for_the_accumulator() {
    // `fold`'s initial accumulator is a tuple literal parked against `?A`, and the step lambda projects its *second* parameter, whose domain is that same `?A`: the lambda is postponed like one whose first domain is stuck, the force tier settles the tuple first, and the projection meets a product.
    let source = r#"
        use /std/{Nat, List, Str};
        let counted: {Nat, Nat} =
            List/fold([3, 4, 5], (0, 0), (n, (sum, count)) => (sum + n, count + 1));
        let (sum, count) = counted;
        /std/print(Str/concat(Nat/to_str(sum), Str/concat(" ", Nat/to_str(count))))
        "#;

    assert_eq!(run(source), b"12 3");
}

#[test]
fn a_list_of_tuples_settles_before_the_lambda_that_projects_them() {
    // The literal's tuples park against the element metavariable and nothing would wake them before the lambda's body projects the element, so the literal itself is postponed and settled at the force tier, ahead of the lambda in slot order.
    let source = r#"
        use /std/{Nat, List, Str};
        let sums: List(Nat) = List/map([(1, 2), (3, 4)], ((a, b)) => a + b);
        /std/print(Str/join(", ", List/map(sums, Nat/to_str)))
        "#;

    assert_eq!(run(source), b"3, 7");
}

#[test]
fn parked_constraints_still_reject_the_unsolvable() {
    // An undecidable-at-first constraint that never resolves must still fail — at the item drain, attributed to its origin. `refl` forces both indices equal; `2` and `3` are not.
    let source = r#"
        use /std/{Nat, Handle};
        induct Eq2(@A : Type) : (x : A, y : A) -> Type
        | refl(@z : A) : (z, z)
        end
        let bad : Eq2(2, 3) = Eq2/refl();
        let _ = Handle/write(Handle/stdout, /std/Str/to_bytes("no"))!;
        /std/Io/pure(())
        "#;

    error(source);
}

#[test]
fn bare_tuple_continuation_tail_infers() {
    // The recorded dead-end from the result-directed elaboration work: a bare tuple in a monadic continuation's tail, its expected type a metavariable pinned only by the *outer* apply's result unification. The in-apply postponement defers the tuple, the constraint store parks the flex–flex codomain pair across the inner apply, and the outer pin wakes both.
    let source = r#"
        use /std/{Parse, Byte, Nat, Bytes, Handle};
        let pairer : Parse({ Byte, Byte }) =
            Parse/bind(Parse/any_byte, (a) => Parse/pure((a, a)));
        let with_sugar : Parse({ Byte, Byte }) =
            let a = Parse/any_byte!;
            Parse/pure((a, 0));
        match Parse/run(pairer, /std/Str/to_bytes("hi"))
        | success(pair) => /std/print(Nat/to_str(Byte/to_nat(pair.0)))
        | failure(_) => /std/print("error")
        end
        "#;

    assert_eq!(run(source), b"104");
}

#[test]
fn checking_problem_parks_until_an_outer_pin_lands() {
    // The constraint store's own window: the inner apply's output expect parks (provisional success), so the postponed tuple re-check meets a still-unsolved expected type — it now parks as a *checking problem* behind a placeholder metavariable, and the outer annotation's pin wakes it. Before ParkedWork::Checking this was a NotATupleType error.
    let source = r#"
        use /std/{Nat, List, Handle};
        let mk(@A : Type, a : A) -> List(A) = [a];
        let use_(@B : Type, l : List(B)) -> List(B) = l;
        let v : List({ Nat, Nat }) = use_(mk((1, 2)));
        match v : (_) => /std/Io({})
        | [] => /std/Io/pure(())
        | [p, ..rest] => let _ = Handle/write(Handle/stdout, /std/Str/to_bytes(Nat/to_str(p.1)))!; /std/Io/pure(())
        end
        "#;

    assert_eq!(run(source), b"2");
}

// A postponed argument keeps its *raw* surface spelling when `elaborate_apply` opens the rest of the telescope, and that spelling is load-bearing: reducing through it is what lets the result `expect` pin the metavariables the slot is waiting on. But `elaborate_proj` only resolves a label projection on the *checked* form, so beta-reducing a raw lambda body through the result type manufactures `head.label` where the settled spelling is `head.index` — a term `reduce_proj` once declared `unreachable!`. The result `expect` is now two-phase: best-effort through the raw spelling, then authoritative through the settled arguments.
#[test]
fn postponed_lambda_projecting_by_label_elaborates() {
    let source = r#"
        use /std/{Nat, Eq};
        struct Boxed : pub Type {
            value : Nat
        }
        let cong_value(@s : Boxed, @t : Boxed, p : Eq(s, t)) -> Eq(s.value, t.value) =
            Eq/cong((b : Boxed) => b.value, p);
        let boxed : Boxed = Boxed { value = 7 };
        let same : Eq(boxed, boxed) = Eq/refl();
        let lifted : Eq(boxed.value, boxed.value) = cong_value(same);
        /std/print(Nat/to_str(boxed.value))
        "#;

    assert_eq!(run(source), b"7");
}

// A lambda whose expectation never gains structure settles by synthesizing its own type — annotations state what they state, and an unannotated domain stands as a metavariable for the body, or whatever the settled type later meets, to pin. `(x) => x` pins nothing anywhere, so the survivor is the domain itself, and it is reported as the parameter it is rather than as the internal expectation that once waited on it.
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

// The settle in action, annotated: the lambda's own annotation is the type nothing else could supply, so the bare implicit pins to `(Nat) -> Nat` and the call compiles.
#[test]
fn an_annotated_lambda_settles_a_bare_implicit() {
    let source = r#"
        use /std/{Nat, Str};
        let use_it(@A : Type, a : A) -> Nat = 0;
        let z : Nat = use_it((n : Nat) => n + 1);
        /std/print(Nat/to_str(z))
        "#;
    assert_eq!(run(source), b"0");
}

// The settle in action, unannotated: the domain stands as a metavariable and the body pins it — `n + 1` defaults its operand type to `Nat` — so the bare spelling compiles too.
#[test]
fn a_lambda_body_pins_its_settled_domain() {
    let source = r#"
        use /std/{Nat, Str};
        let use_it(@A : Type, a : A) -> Nat = 0;
        let z : Nat = use_it((n) => n + 1);
        /std/print(Nat/to_str(z))
        "#;
    assert_eq!(run(source), b"0");
}

#[test]
fn a_typeless_local_let_still_infers_its_body() {
    // The positive control for the fix above: an absent annotation is the origin-less hole, and keeps the inference path — a lambda body needs it, since checking a lambda against an unsolved hole would park and never resolve.
    let source = r#"
        use /std/{Nat};

        let g(x : Nat) -> Nat =
            let f = (n : Nat) => n + 1;
            f(x);

        match g(1) == 2
        | true => /std/print("ok\n")
        | false => /std/print("bad\n")
        end
        "#;
    assert_eq!(run(source), b"ok\n");
}
