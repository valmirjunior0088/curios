//! Solving an implicit argument from what the call site already fixes.

use crate::tests::{core_elab, error, run, typecheck_within};

#[test]
fn an_implicit_solves_against_a_reduction_through_a_let() {
    // `Eq/refl()`'s implicit must be solved against `through(x)`, whose weak-head form is a match stuck on `0 < x` with arms mentioning the `let`-bound `y`. The reducer substitutes a `let` as the kernel does, so the candidate names nothing the scope does not cover; binding `y` as a fresh context definition instead would leave a name the scope check refuses as out of scope.
    let source = r#"
        use /std/{Nat, Eq, Str, Io};

        let through(x : Nat) -> Nat =
            let y = x + 1;
            match 0 < x
            | true => y
            | false => y + 1
            end;

        let probe(x : Nat) -> Eq()(through(x), through(x)) = Eq/refl();
        let entry() -> /std/Io({}) =
            let _ = /std/print("ok")!;
            /std/Io/pure(());
        entry()
        "#;

    assert_eq!(run(source), b"ok");
}

#[test]
fn implicit_inductive_type_param_executes() {
    // A `@`-marked inductive parameter is implicit at the type constructor too: `Eq2(2, 2)` infers `A` from the indices, `Eq2(@Nat, 3, 3)` pins it, and the eliminator's motive type-pattern still spells every slot. Running (not just checking) also guards metavariable spines through the Π-domain close/reopen round trip: a solved implicit type-arg's solution names a sibling binder, and without the delayed substitution the two spellings of the same domain compare as distinct.
    let source = r#"
        use /std/{Nat, Bytes, Io};
        induct Eq2(@A : Type) : (x : A, y : A) -> Type
        | refl(@z : A) : (z, z)
        end
        let sym2(@A : Type, @x : A, @y : A, p : Eq2()(x, y)) -> Eq2()(y, x) =
            match p : (s, t, q) => Eq2()(t, s)
            | refl(@z) => Eq2/refl()
            end;
        let pinned : Eq2(@Nat)(3, 3) = Eq2/refl();
        let proof : Eq2()(2, 2) = Eq2/refl();
        let inferred : Eq2()(2, 2) = sym2(proof);
        let entry() -> /std/Io({}) =
            match inferred : (_, _, _) => /std/Io({})
            | refl(@z) => let _ = /std/print(Nat/to_str(z))!; /std/Io/pure(())
            end;
        entry()
        "#;

    assert_eq!(run(source), b"2");
}

#[test]
fn implicit_inductive_type_param_rejects_explicit_spelling() {
    // With `@A` implicit, the explicit spelling queues `Nat` into the parameter call's explicit slots, of which there are none — an error rather than a silent reinterpretation. (`Eq2(@Nat)(2, 2)` is the pinned spelling.)
    let source = r#"
        use /std/{Nat, Io};
        induct Eq2(@A : Type) : (x : A, y : A) -> Type
        | refl(@z : A) : (z, z)
        end
        let bad : Eq2(Nat)(2, 2) = Eq2/refl();
        let entry() -> /std/Io({}) =
            let _ = /std/print("no")!;
            /std/Io/pure(());
        entry()
        "#;

    error(source);
}

// An `Eq/subst` whose motive contains `Eq()(_, _)` — whose `@A` is implicit — inserts that implicit when the motive is instantiated. Dropping it would leave `Eq` (a 3-telescope `@A, x, y`) applied to 2 args, which panics `reduce_apply` with a telescope arity mismatch.
#[test]
fn subst_motive_inserts_implicit_in_eq() {
    let source = r#"
        use /std/{Eq, Nat, Io};
        let g(n : Nat) -> Nat = n;
        let lemma(@a : Nat, @b : Nat, p : Eq()(a, b)) -> Eq()(g(a), g(b)) =
            Eq/subst((x) => Eq()(g(a), g(x)), p, Eq/refl());
        let _ = lemma;
        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// The flex-apply imitation rule: an implicit higher-kinded binder `@M` is inferred from an argument's concrete type — `?M(?A) ≡ List(Nat)` commits `?M := (A) => List(A)` and `?A := Nat` — as the explicit `apply_m(@List, l)` spelling states it.
#[test]
fn higher_kinded_implicit_infers_by_imitation() {
    let source = r#"
        use /std/{Nat, List, Str, Io};
        pub let apply_m(@M : (Type) -> Type, @A : Type, x : M(A)) -> M(A) = x;
        let l : List(Nat) = [1, 2];
        let k : List(Nat) = apply_m(l);
        /std/print(Nat/to_str(List/len(k)))
        "#;

    assert_eq!(run(source), b"2");
}

// Two applications of one global definition are unified by their spines before the head unfolds — the first-order approximation. `trim` is a fold that never names itself, so it is a `let` and reduction would unfold it: the left side steps to `combine(false, trim(x))`, the right to a fold stuck on `?t`, and nothing pins `?t` again. Comparing the spines first states the solution outright, as it does for a `rec`-defined head, whose application stays folded.
#[test]
fn an_implicit_solves_by_spine_agreement_before_the_head_unfolds() {
    let source = r#"
        use /std/{Nat, Bool, List, Eq, Str, Io};
        let combine(head : Bool, t : List(Bool)) -> List(Bool) =
            match t
            | [] => match head | true => [head] | false => [] end
            | [_, .._] => [head, ..t]
            end;
        let trim(bits : List(Bool)) -> List(Bool) =
            match bits | [] => [] | [head, ..tail]; ih => combine(head, ih) end;
        let through(@t : List(Bool), q : Eq()(trim(t), [])) -> Eq()(trim(t), []) = q;
        let probe(x : List(Bool), q : Eq()(trim([false, ..x]), [])) -> Eq()(trim([false, ..x]), []) = through(q);
        let entry() -> /std/Io({}) =
            let _ = /std/print("ok")!;
            /std/Io/pure(());
        entry()
        "#;

    assert_eq!(run(source), b"ok");
}

// Agreeing spines are sufficient, never necessary: a constant function's applications to different arguments are still equal, and the attempt's mismatch must fall through to the unfolding that decides them — with whatever the attempt committed rolled back.
#[test]
fn a_spine_mismatch_falls_through_to_unfolding() {
    let source = r#"
        use /std/{Nat, Eq, Str, Io};
        use /std/Bool/{Holds};
        let constant(n : Nat) -> Nat = 0;
        let same : Eq()(constant(2), constant(1)) = Eq/refl();
        let entry() -> /std/Io({}) =
            let _ = /std/print("ok")!;
            /std/Io/pure(());
        entry()
        "#;

    assert_eq!(run(source), b"ok");
}

#[test]
fn an_undetermined_value_implicit_is_reported_as_undetermined_not_undischarged() {
    // `n` is a `Nat`, not a proposition: nothing about it was ever an obligation, so the report says nothing determined it and shows its type, rather than claiming nothing discharged `Nat` — which is the wording a *bound* gets, and names a fault a reader cannot find here.
    let source = r#"
        use /std/{Nat};
        let pad(@n: Nat, x: Nat) -> Nat = x;
        /std/print(Nat/to_str(pad(1)))
        "#;

    let report = error(source);
    assert!(
        report.contains("implicit argument 'n' of 'pad' was not inferred")
            && report.contains("no argument or expected type determined it (its type is Nat)")
            && !report.contains("nothing discharged"),
        "the report should say nothing determined the value, got: {report}"
    );
}

/// A solution is committed as written where its reduct does not re-check. `reach` reduces to `k` plus `hop` inlined, and `hop`'s absurd arm types only because its own arm refines the variable `b`: inlined, its scrutinee's type is `Holds(0 < Bytes/len(b) - k)`, which the arm's equation on `Bytes/slice(…)` never reaches, so the reduct is refused at re-validation and `Eq/refl`'s `@x` is solved to `reach(b, k, @within, @here)` as written. Mutation-checked: committing the reduct alone refuses the program at `Eq/refl()`.
#[test]
fn a_solution_whose_reduct_does_not_recheck_is_committed_as_written() {
    let output = run(r#"
        use /std/{Bytes, Nat, Eq, print, proved};
        use /std/Bool/{True, Holds};

        let hop(@b: Bytes, some: Holds(0 < Bytes/len(b))) -> Nat =
            match b
            | x[] => match some end
            | x[_, .._] => 1
            end;

        let reach(b: Bytes, k: Nat, @within: Holds(k <= Bytes/len(b)), @here: Holds(k < Bytes/len(b))) -> Nat =
            k + hop(@Bytes/drop(b, k, @within), proved());

        pub let same(b: Bytes, k: Nat, @within: Holds(k <= Bytes/len(b)), @here: Holds(k < Bytes/len(b)))
            -> Eq()(reach(b, k, @within, @here), reach(b, k, @within, @here)) =
            Eq/refl();

        print(Nat/to_str(reach(x[1, 2], 0, @True/qed(), @True/qed())))
        "#);

    assert_eq!(output, b"1");
}

/// A solution that holds a call of a recursive function names the function: `Eq/refl`'s implicit is solved to `f(n).count`, as the row states it. A candidate arrives reduced, a stuck call headed by its member's group, and committed so the solution would hold a copy of the callee, which its re-validation elaborates again and the kernel checks again.
#[test]
fn a_solution_holding_a_call_of_a_recursive_function_names_the_function() {
    let elaborated = core_elab(
        r#"
        use /std/{Nat, Eq, Io};

        struct Q(n: Nat): Type {
            count: Nat,
        }

        let f(n: Nat) -> Q(n) =
            match 0 < n
            | true =>
                let inner = f(n / 2);
                Q { count = inner.count + 1 }
            | false => Q { count = 0 }
            end;

        pub let row(n: Nat) -> Eq()(f(n).count, f(n).count) = Eq/refl();

        Io/pure(())
        "#,
    );
    let row = elaborated
        .split("let /row")
        .nth(1)
        .expect("the row is printed");

    assert!(row.contains("refl.{Type; 0}(@Nat, @(f(n)).0)"), "{row}");
    assert!(!row.contains("rec "), "{row}");
}

/// A solution met against a projection off a local definition is that projection: `Eq/refl`'s implicit is solved to `p.a`, as the row states it, where reducing it opens the definition into the match `make`'s body is and projects off that, which a report would print and the kernel receive.
#[test]
fn a_solution_met_against_a_projection_off_a_local_definition_is_that_projection() {
    let elaborated = core_elab(
        r#"
        use /std/{Nat, Eq, Io};

        struct P: Type {
            a: Nat,
        }

        let make(n: Nat) -> P =
            match n
            | 0 => P { a = 0 }
            | m + 1 => P { a = m }
            end;

        pub let row(n: Nat) -> {} =
            let p = make(n);
            let _: Eq()(p.a, p.a) = Eq/refl();
            ();

        Io/pure(())
        "#,
    );
    let row = elaborated
        .split("let /row")
        .nth(1)
        .expect("the row is printed");

    assert!(row.contains("refl.{Type; 0}(@Nat, @(p).0)"), "{row}");
    assert!(!row.contains("Nat.fold"), "{row}");
}

/// A solution that holds a call of a recursive function is that call, where the function's body holds a proof the elaborator wrote. `of` leaves the bound of `of(n / 2)` to the elaborator, whose proof splits on the bound's decision and eliminates a contradiction under an elided motive, and `Eq/refl`'s implicit is solved to `of(n, @ok).count`. Committed as the copy of `of`'s group its reduct holds, the solution was re-validated by elaborating that group again, an inner goal came back in another spelling, and conversion unfolded the two groups side by side over arguments that grow until the budget was spent — so the budget here is one the program fits many times over and the regress does not.
#[test]
fn a_solution_holding_a_call_whose_body_holds_a_proof_the_elaborator_wrote_is_that_call() {
    typecheck_within(
        2_000_000,
        r#"
        use /std/{Nat, Eq, Io, proved};
        use /std/Bool/{Holds};

        struct Scale(n: Nat): Type {
            count: Nat,
            half: Nat,
            joined: Eq()(n, Nat/shl(2 * half + 1, count)),
        }

        let of(n: Nat, @ok: Holds(0 < n)) -> Scale(n) =
            match 0 < n
            | true =>
                match 0 < n % 2
                | true =>
                    Scale {
                        count = 0,
                        half = n / 2,
                        joined = Nat/le/antisym(n, 2 * (n / 2) + 1, proved(), proved()),
                    }
                | false =>
                    let inner = of(n / 2);
                    let even: Eq()(n, 2 * (n / 2)) =
                        Nat/le/antisym(n, 2 * (n / 2), proved(), proved());
                    Scale {
                        count = inner.count + 1,
                        half = inner.half,
                        joined = Eq/subst((q: Nat) => Eq()(n, 2 * q), inner.joined, even),
                    }
                end
            | false => match ok end
            end;

        pub let same(n: Nat, ok: Holds(0 < n)) -> Eq()(of(n, @ok).count + 1, 1 + of(n, @ok).count) =
            Eq/refl();

        pub let rebuilt(n: Nat, ok: Holds(0 < n), m: Nat, e: Eq()(m, n))
            -> Eq()(m, Nat/shl(2 * of(n, @ok).half + 1, of(n, @ok).count)) =
            Eq/trans(e, of(n, @ok).joined);

        Io/pure(())
        "#,
    )
    .expect("the two sides are one call");
}

/// A local recursive function elaborated again is itself. A local group has no name a solution could take, so `Eq/refl`'s implicit holds a copy of `of`'s group, and re-validating it elaborates the group again: each rebuilt match keeps the goal it holds, where the type it is checked at converts with it. Given the goal its arm specializes the second time, the copy is a second term to conversion, which holds the two groups one where their members convert under shared binders, and unfolds neither.
///
/// Mutation-checked: with a rebuilt match taking the type it is checked at, the program is still accepted, the copy and the group compared as the binders they are; with that comparison gone as well, it runs out of steps.
#[test]
fn a_local_recursive_function_elaborated_again_is_itself() {
    typecheck_within(
        2_000_000,
        r#"
        use /std/{Nat, Eq, Io, proved};
        use /std/Bool/{Holds};

        struct Scale(n: Nat): Type {
            count: Nat,
            half: Nat,
            joined: Eq()(n, Nat/shl(2 * half + 1, count)),
        }

        pub let same(n: Nat, ok: Holds(0 < n)) -> Nat =
            let of(n: Nat, @ok: Holds(0 < n)) -> Scale(n) =
                match 0 < n
                | true =>
                    match 0 < n % 2
                    | true =>
                        Scale {
                            count = 0,
                            half = n / 2,
                            joined =
                                Nat/le/antisym(n, 2 * (n / 2) + 1, proved(), proved()),
                        }
                    | false =>
                        let inner = of(n / 2);
                        let even: Eq()(n, 2 * (n / 2)) =
                            Nat/le/antisym(n, 2 * (n / 2), proved(), proved());
                        Scale {
                            count = inner.count + 1,
                            half = inner.half,
                            joined = Eq/subst((q: Nat) => Eq()(n, 2 * q), inner.joined, even),
                        }
                    end
                | false => match ok end
                end;
            let commuted: Eq()(of(n, @ok).count + 1, 1 + of(n, @ok).count) = Eq/refl();
            of(n, @ok).count;

        Io/pure(())
        "#,
    )
    .expect("the copy is the group");
}

/// A list's unknown tail is solved to what the other side has left once the written elements are paired: against a longer prefix, a tail that is a concatenation, and a closed literal, each of which has one solution. A constructor whose index is a cons meets all three wherever a proof builds it.
#[test]
fn a_lists_unknown_tail_is_solved_to_what_the_other_side_has_left() {
    let source = r#"
        use /std/{Nat, List, Eq, Str, Io};

        induct Swapped(@T : Type) : (List(T), List(T)) -> Prop
        | swap(@x : T, @y : T, @l : List(T)) : ([y, x, ..l], [x, y, ..l])
        end

        let tail_of(@T : Type, @h : T, @l : List(T), p : Eq()([h, ..l], [h, ..l])) -> List(T) =
            l;

        let longer_prefix(x : Nat, y : Nat, t : List(Nat)) -> List(Nat) =
            tail_of(Eq/refl(@_, @[y, x, ..t]));

        let two_tails(x : Nat, a : List(Nat), b : List(Nat)) -> List(Nat) =
            tail_of(Eq/refl(@_, @[x, ..a, ..b]));

        let literal(x : Nat, y : Nat) -> List(Nat) =
            tail_of(Eq/refl(@List(Nat), @[y, x]));

        let _past_a_concatenation(
            a : Nat,
            b : Nat,
            c : List(Nat),
            d : List(Nat),
        ) -> Swapped()([a, b, ..c, ..d], [b, a, ..c, ..d]) =
            Swapped/swap();

        let _of_a_closed_list : Swapped()([2, 1, 3], [1, 2, 3]) =
            Swapped/swap();

        let entry() -> /std/Io({}) =
            let lengths =
                List/map(
                    [longer_prefix(1, 2, [3]), two_tails(1, [2], [3]), literal(1, 2)],
                    (l : List(Nat)) => Nat/to_str(List/len(l)));
            let _ = /std/print(Str/join(" ", lengths))!;
            /std/Io/pure(());
        entry()
        "#;

    assert_eq!(run(source), b"2 2 1");
}

/// The split refuses what has no solution: a list of one element is no swap of two.
#[test]
fn a_list_shorter_than_its_constructors_index_is_refused() {
    let refusal = error(
        r#"
        use /std/{Nat, List, Io};

        induct Swapped(@T : Type) : (List(T), List(T)) -> Prop
        | swap(@x : T, @y : T, @l : List(T)) : ([y, x, ..l], [x, y, ..l])
        end

        let wrong : Swapped()([1], [1]) =
            Swapped/swap();

        /std/Io/pure(())
        "#,
    );

    assert!(refusal.contains("Swapped/swap"), "{refusal}");
}

/// A candidate rejected where its implicit was born is reported as one. The value `g`'s `Eq/refl()` leaves implicit is born outside the arm, and its candidate holds a proof of `y <= z` that checks under the arm's guard alone: the report names the implicit, shows the candidate and gives the bound that did not hold. The pair conversion was comparing, a `?` the reader did not write and the candidate, was not found apart, and read as two terms that differ. The verdict stands, and the declaration compiles with the binding inside the arm.
#[test]
fn a_candidate_rejected_where_its_implicit_was_born_is_reported_as_one() {
    let refusal = error(
        r#"
        use /std/{Eq, Nat, print};
        use /std/Bool/{Holds};

        struct Le(z: Nat): Type {
            low: Nat,
            @ok: Holds(low <= z),
        }

        let both(@A: Type, @v: A, a: Eq(@A)(v, v), b: Eq(@A)(v, v)) -> Nat = 0;

        let f(y: Nat, z: Nat) -> Nat =
            let g = Eq/refl();
            match y <= z
            | true => (let r: Eq(@Le(z))(Le { low = y }, Le { low = y }) = Eq/refl(); both(g, r))
            | false => 0
            end;

        print(Nat/to_str(f(1, 2)))
        "#,
    );

    assert!(
        refusal.contains(
            "the implicit argument 'z' of 'Eq/refl' cannot be Le { y, @ok = /std/Bool/True/qed() }\n  it does not check where the argument was left out:\n    type mismatch\n      inferred: /std/Bool/True\n      expected: Holds(y <= z)"
        ),
        "{refusal}"
    );

    let output = run(r#"
        use /std/{Eq, Nat, print};
        use /std/Bool/{Holds};

        struct Le(z: Nat): Type {
            low: Nat,
            @ok: Holds(low <= z),
        }

        let both(@A: Type, @v: A, a: Eq(@A)(v, v), b: Eq(@A)(v, v)) -> Nat = 0;

        let f(y: Nat, z: Nat) -> Nat =
            match y <= z
            | true => (let g = Eq/refl(); let r: Eq(@Le(z))(Le { low = y }, Le { low = y }) = Eq/refl(); both(g, r))
            | false => 0
            end;

        print(Nat/to_str(f(1, 2)))
        "#);

    assert_eq!(output, b"0");
}
