//! A checking problem that cannot proceed yet, and what wakes it.
//!
//! A checked-only form met by an expectation with no structure is postponed rather than judged, and re-checked under its frozen frame once a watched metavariable lands. These pin both halves: the problems an outer pin resolves, and the ones nothing ever will — which must be reported at their own span rather than accepted.

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
        match chained : (_, _, _) => /std/Io({})
        | refl(@z) => let _ = Io/write(Io/stdout, /std/Str/to_bytes(Nat/to_str(z)))!; /std/Io/pure(())
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
        use /std/{Nat, Io};
        induct Eq2(@A : Type) : (x : A, y : A) -> Type
        | refl(@z : A) : (z, z)
        end
        let bad : Eq2()(2, 3) = Eq2/refl();
        let _ = Io/write(Io/stdout, /std/Str/to_bytes("no"))!;
        /std/Io/pure(())
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
        let with_sugar : Parse(Bytes, { Byte, Byte }) =
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
        match v : (_) => /std/Io({})
        | [] => /std/Io/pure(())
        | [p, ..rest] => let _ = Io/write(Io/stdout, /std/Str/to_bytes(Nat/to_str(p.1)))!; /std/Io/pure(())
        end
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
        pub let probe(f: (Nat) -> Nat, x: Nat) -> Eq()(Result/map_success(Result/success(x), f), Result/success(f(x))) = Eq/refl();
        /std/print("ok")
        "#,
    );
    assert!(
        !report.contains("between: ?")
            && report.contains("never solved: the implicit argument 'E'"),
        "unexpected report:\n{report}"
    );
}

// A projection waits for its head's type as a checked-only form waits for its expectation. `p`'s type is the unannotated match's, which only its tuple arms decide, and those park against it until the drain settles them to their product; destructuring `p` reads its fields before then, and refusing the projection there as one from a non-tuple would refuse one step before the settle that types its head. Mutation-checked: refusing a projection whose head's type is stuck refuses the program.
#[test]
fn a_projection_waits_for_the_tuple_arms_that_type_its_head() {
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

// A projection whose head's type nothing ever decides is refused at the drain, at the field it reads: a destructuring lowers each field to a projection located at the field's pattern rather than at the `let` of the value it destructures. Mutation-checked: lowering the projections unlocated reports the line of `let p`.
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

// A match waits for its scrutinee's type as a projection waits for its head's. `xs` is read off `p` by a projection that waits for the drain to settle `p`'s tuple arms, so its type is still a metavariable when the match on it is met, and refusing the match there as `expected List but got ?` would refuse one step before the settle that types its scrutinee. Mutation-checked: refusing a match whose scrutinee's type is stuck refuses the program.
#[test]
fn a_match_waits_for_the_type_of_its_scrutinee() {
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

// A match whose scrutinee's type nothing ever decides is refused at the drain as its eliminator refuses a scrutinee of another type, not as a check that merely waited. Mutation-checked: reporting it as a postponed check loses the carrier it expected.
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

#[test]
fn reflexivity_at_two_lambdas_that_differ_is_a_mismatch() {
    let report = error(
        r#"
        use /std/{Eq, Nat};
        let differ(g : (Nat) -> Nat) -> Eq()((x : Nat) => g(0), (x : Nat) => g(1)) = Eq/refl();
        /std/print("unreachable")
        "#,
    );
    assert!(report.contains("terms differ"), "{report}");
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
        use /std/{proc};
        let _ = proc/exit(1)!;
        /std/print("unreachable")
        "#,
    );
    assert!(
        report.contains("never solved: ")
            && report.contains("the implicit argument 'A' of 'proc/exit'"),
        "{report}"
    );
    assert!(!report.contains("was not inferred"), "{report}");
}

// A written argument is checked as written, and then held to its call's type. A list or a tuple at a slot whose type is still a metavariable waits behind a placeholder, and the call's expected type states the slot's value before the argument is checked. Conversion waits on the placeholder as it waits on a recursive group's slot, so nothing but the argument's own check fills it, and the value the type states meets the argument that was written. Mutation-checked: letting conversion solve a placeholder accepts both, with the type's list standing where the argument was written.
#[test]
fn a_hidden_argument_is_held_to_the_value_its_calls_type_states() {
    let tagged = error(
        r#"
        use /std/{List, Nat};
        induct Tagged(@A : Type) : (A) -> Type
        | tag(@z : A) : (z)
        end
        let relevant : Tagged()([7, 8, 9]) = Tagged/tag(@_, @[1]);
        /std/print("unreachable")
        "#,
    );
    assert!(tagged.contains("terms differ"), "{tagged}");

    let reflexive = error(
        r#"
        use /std/{Eq, List, Nat};
        let also(x : Nat, y : Nat) -> Eq()([x], [x]) = Eq/refl(@_, @[y]);
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
        let boxed : Boxed()([7, 8, 9]) = Boxed/box([1]);
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

// A literal that parks inside an argument — a match arm's tuple, a `let` body's — is no pending of the call's own, so no force tier runs it and only its retry does. It is held to the type as the argument itself is. Mutation-checked, as above.
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
