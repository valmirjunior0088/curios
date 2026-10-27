//! A checking problem that cannot proceed yet, and what wakes it.

use crate::tests::{error, run};

#[test]
fn parked_constraints_let_nested_constructor_metas_resolve() {
    // `sym2(Eq2/refl())` — the argument's fresh metas meet the domain's fresh metas as flex–flex pairs embedded under the inductive type. The pairs park in the constraint store, the output `expect` solves the domain metas against the annotation, and the wake retries the parked pairs; judged at once, the argument's `expect` would fail before the result-type unification pins everything.
    let source = r#"
        use /std/{Nat, Io};
        induct Eq2(@A : Type) : (x : A, y : A) -> Type
        | refl(@z : A) : (z, z)
        end
        let sym2(@A : Type, @x : A, @y : A, p : Eq2()(x, y)) -> Eq2()(y, x) =
            match p : (s, t, q) => Eq2()(t, s)
            | refl(@z) => Eq2/refl()
            end;
        let direct : Eq2()(2, 2) = sym2(Eq2/refl());
        let chained : Eq2()(3, 3) = sym2(sym2(Eq2/refl()));
        let entry() -> /std/Io({}) =
            match chained : (_, _, _) => /std/Io({})
            | refl(@z) => let _ = /std/print(Nat/to_str(z))!; /std/Io/pure(())
            end;
        entry()
        "#;

    assert_eq!(run(source), b"3");
}

#[test]
fn a_tuple_pattern_in_a_later_parameter_waits_for_the_accumulator() {
    // `fold`'s initial accumulator is a tuple literal parked against `?A`, and the step lambda projects its *second* parameter, whose domain is that same `?A`: the lambda is postponed like one whose first domain is stuck, the call gives the tuple its shape where it completes, ahead of the lambda, and the projection meets a product.
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
    // The literal's tuples park against the element metavariable and nothing would wake them before the lambda's body projects the element, so the lambda waits on its domain as they do, and the call shapes them where it completes, a shape being taken ahead of a lambda's check.
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
        use /std/{Nat, Io};
        induct Eq2(@A : Type) : (x : A, y : A) -> Type
        | refl(@z : A) : (z, z)
        end
        let bad : Eq2()(2, 3) = Eq2/refl();
        let entry() -> /std/Io({}) =
            let _ = /std/print("no")!;
            /std/Io/pure(());
        entry()
        "#;

    error(source);
}

#[test]
fn bare_tuple_continuation_tail_infers() {
    // A bare tuple in a monadic continuation's tail, its expected type a metavariable pinned only by the *outer* apply's result unification. The in-apply postponement defers the tuple, the constraint store parks the flex–flex codomain pair across the inner apply, and the outer pin wakes both.
    let source = r#"
        use /std/{Parse, Byte, Nat, Bytes, Io};
        let pairer : Parse(Bytes, { Byte, Byte }) =
            Parse/bind(Parse/bytes/byte, (a) => Parse/pure((a, a)));
        let with_sugar() -> Parse(Bytes, { Byte, Byte }) =
            let a = Parse/bytes/byte!;
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
    // The constraint store's own window: the inner apply's output expect parks (provisional success), so the postponed tuple re-check meets a still-unsolved expected type — it parks as a *checking problem* (`ParkedWork::Checking`) behind a placeholder metavariable rather than failing as no tuple type, and the outer annotation's pin wakes it.
    let source = r#"
        use /std/{Nat, List, Io};
        let mk(@A : Type, a : A) -> List(A) = [a];
        let use_(@B : Type, l : List(B)) -> List(B) = l;
        let v : List({ Nat, Nat }) = use_(mk((1, 2)));
        let entry() -> /std/Io({}) =
            match v : (_) => /std/Io({})
            | [] => /std/Io/pure(())
            | [p, ..rest] => let _ = /std/print(Nat/to_str(p.1))!; /std/Io/pure(())
            end;
        entry()
        "#;

    assert_eq!(run(source), b"2");
}

// A label projection in a postponed lambda's body is resolved on the checked form. The walk opens the rest of the telescope through the argument's placeholder, so no raw body reaches the call's result type, where a `head.label` would stand for what the settled argument spells `head.index`.
#[test]
fn postponed_lambda_projecting_by_label_elaborates() {
    let source = r#"
        use /std/{Nat, Eq};
        struct Boxed : pub Type {
            value : Nat
        }
        let cong_value(@s : Boxed, @t : Boxed, p : Eq()(s, t)) -> Eq()(s.value, t.value) =
            Eq/cong((b : Boxed) => b.value, p);
        let boxed : Boxed = Boxed { value = 7 };
        let same : Eq()(boxed, boxed) = Eq/refl();
        let lifted : Eq()(boxed.value, boxed.value) = cong_value(same);
        /std/print(Nat/to_str(boxed.value))
        "#;

    assert_eq!(run(source), b"7");
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
    // The positive control for `goal_tests`' `let y : ? = e`: an absent annotation is the origin-less hole, and keeps the inference path — a lambda body needs it, since checking a lambda against an unsolved hole would park and never resolve.
    let source = r#"
        use /std/{Nat};
        use /std/Bool/{Holds};

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

// A projection reads a type its head already has. `p`'s type is the unannotated match's, which only its tuple arms decide, and the match completes them where it has read them all: they give its type their product before the next line destructures `p`, so no projection waits for it.
#[test]
fn a_destructuring_reads_the_product_its_heads_tuple_arms_gave() {
    let source = r#"
        use /std/{Str, List, Nat};
        let joined(raw: List(Str)) -> Str =
            let p = match raw | [] => ([], 0) | [_, .._] => ([], 1) end;
            let (xs, n) = p;
            Str/flatten([Nat/to_str(n), "|", Str/join(",", xs)]);
        /std/print(joined(["a"]))
        "#;

    assert_eq!(run(source), b"1|");
}

// A match reads a carrier its scrutinee already has. `xs` is read off `p`, whose type the match that binds it gave where it completed its tuple arms, so the match on `xs` meets a list and nothing waits for one.
#[test]
fn a_match_reads_the_carrier_a_destructured_field_has() {
    let source = r#"
        use /std/{Str, List, Nat};
        let first(raw: List(Str)) -> Str =
            let p = match raw | [] => ([], 0) | [_, .._] => (["x"], 1) end;
            let (xs, n) = p;
            match xs | [] => Nat/to_str(n) | [s, .._] => s end;
        /std/print(Str/flatten([first(["a"]), first([])]))
        "#;

    assert_eq!(run(source), b"x0");
}

// A candidate re-validation cannot judge yet waits, so what compiles does not follow the order parked work woke in. The claim's two lambdas park against the claim's type, and `Eq/refl()`'s own implicit `z : ?A` meets the first of them at the item's drain while `?A` is still unsolved, one retry short of the equation that solves it. Checking the lambda against a type that is not known yet judges nothing: the solution waits, the drain's next sweep solves `?A`, and the lambda checks.
//
// The refusal beside it is what says the wait is no acceptance: the same claim over two lambdas that differ is a mismatch, as it was.
#[test]
fn reflexivity_at_a_lambda_waits_for_the_type_the_drain_settles() {
    let source = r#"
        use /std/{Eq, Nat};
        let same(g : (Nat) -> Nat) -> Eq()((x : Nat) => g(0), (x : Nat) => g(0)) = Eq/refl();
        /std/print(Nat/to_str(1))
        "#;

    assert_eq!(run(source), b"1");
}

// What the wait gives: the argument is checked first, so what it says reaches the metavariables its call's type leaves open — `h` and `l` are read off the written list — and an argument that agrees with the type up to conversion is the one the program holds. Mutation-checked: with conversion solving the placeholder, the written list is never compared and `h` is reported as not inferred.
#[test]
fn a_written_argument_solves_what_its_calls_type_leaves_open() {
    let source = r#"
        use /std/{Eq, List, Nat, Str};
        induct Boxed(@A : Type) : (A) -> Type
        | box(z : A) : (z)
        end

        let tail_of(@T : Type, @h : T, @l : List(T), p : Eq()([h, ..l], [h, ..l])) -> List(T) =
            l;
        let literal(x : Nat, y : Nat) -> List(Nat) =
            tail_of(Eq/refl(@_, @[y, x]));

        let computed(x : Nat) -> Boxed()([x + 0, 2]) = Boxed/box([x, 1 + 1]);
        let held(@A : Type, @a : A, b : Boxed()(a)) -> A =
            match b | box(z) => z end;

        /std/print(Str/join(" ", [
            Nat/to_str(List/len(literal(1, 2))),
            Nat/to_str(List/len(held(computed(5)))),
        ]))
        "#;

    assert_eq!(run(source), b"1 2");
}
