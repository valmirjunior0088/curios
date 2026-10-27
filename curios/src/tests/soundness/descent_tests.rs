//! The size-change obligation: what licenses a decrease — a dispatch or guard that rules out zero, a shrinking literal operand, a payload — and what does not.

use {super::test_support::*, crate::tests::run};

// The arithmetic decrease is licensed by the *arm*, not by the operator. `n - 1` is smaller than `n` only where `n` is known nonzero — `Nat` subtraction saturates, so `0 - 1` is `0` and the "decrease" is a fixed point — and a `Nat` dispatch's default arm establishes nonzero exactly when `0` is one of the enumerated cases. This fixture omits `0`, so nothing rules it out and the call may not be graded as descending.
//
// Read the wrong way the cost is a non-terminating *type*, which is the negative knot `a_type_level_rec_cannot_tie_the_negative_knot` above spells out in full: `Bad(0)` would unfold to `Bad(0)` forever while the classifier reported the group total, type formation would rest on a family that never forms, and (T) — whose whole job is that nothing reachable from a type position is partial — would have been satisfied by a false premise rather than by a fact.
//
// This side condition carries more weight than its size suggests. Across the corpus, the enclosing arms' refinements change thousands of call grades, and every change is an upgrade away from `Unknown` — to `Less` or `Same`, never a downgrade. The arm facts are what make the corpus's recursion provable at all, so the conditions under which an arm may establish one are the accepting direction throughout.
//
// Probed rather than closed: the fixture asserts the diagnostic so it cannot pass on an unrelated failure. Its control is `a_dispatch_that_enumerates_zero_licenses_the_decrease`, the same recursion with `0` enumerated, which must keep compiling — refusing it would take `/std/Nat/to_str` and every other bounded `Nat` descent with it.
#[test]
fn a_dispatch_that_does_not_enumerate_zero_licenses_no_decrease() {
    rejected_as_a_type(
        r#"
        use /std/{Nat};

        let Bad(n : Nat) -> Type =
            match n
            | 1 => {}
            | _ => Bad(n - 1)
            end;

        /std/print("unreachable")
        "#,
    );
}

// The control for the fixture above: with `0` enumerated, the default arm does establish nonzero, `n - 1` is a genuine decrease, and the same type-level recursion is accepted.
#[test]
fn a_dispatch_that_enumerates_zero_licenses_the_decrease() {
    let source = r#"
        use /std/{Nat};

        let Good(n : Nat) -> Type =
            match n
            | 0 => {}
            | _ => Good(n - 1)
            end;

        let held : Good(2) = ();

        /std/print("ok")
        "#;
    assert_eq!(run(source), b"ok");
}

// A `Nat` dispatch is not the only thing that can establish nonzero. A *boolean* arm does it too, when its scrutinee compares a tracked binder against a literal: the classifier reads the comparison, and the arm in which it holds either excludes zero or does not. `/std/Nat/to_str` descends on exactly that — the false arm of `n < 10` gives `n >= 10`, which is what licenses `to_str(n / 10)` — so the route is live throughout the corpus, where the pair above probes the dispatch route.
//
// Three spellings reach the same reader and all three must refuse, because each exercises a different piece of it. `n < 10` is already an intrinsic after elaboration, so it tests the relation table alone. `10 > n` arrives with its operands the other way round and is only readable once flipped, so it tests the flip — a flip that turned `n < 10` into `n > 10` would report nonzero for an arm that admits zero. `small(n)` is an ordinary definition, so it is readable only through the same bounded weak-head unfolding the shape reader uses, and an unfolding that lost track of which arm it was in would do the same. In every one the true arm gives `n < 10`, which admits zero, so `n - 1` is a fixed point at zero and the group does not descend.
//
// Read the wrong way the cost is the same as for the dispatch route and is spelled out there: a non-terminating type, type formation resting on a family that never forms, and (T) satisfied by a false premise.
//
// Probed rather than closed: each asserts the diagnostic so none can pass on an unrelated failure. The control is `a_boolean_guard_that_excludes_zero_licenses_the_decrease`, which must keep compiling in both spellings — refusing them would take `/std/Nat/to_str` with it.
#[test]
fn a_boolean_guard_that_does_not_exclude_zero_licenses_no_decrease() {
    // The intrinsic spelling: the relation table on its own.
    rejected_as_a_type(
        r#"
        use /std/{Nat};

        let Bad(n : Nat) -> Type =
            match n < 10
            | true => Bad(n - 1)
            | false => {}
            end;

        /std/print("unreachable")
        "#,
    );

    // The literal first, so the guard is readable only once its operands are exchanged.
    rejected_as_a_type(
        r#"
        use /std/{Nat};

        let Bad(n : Nat) -> Type =
            match 10 > n
            | true => Bad(n - 1)
            | false => {}
            end;

        /std/print("unreachable")
        "#,
    );

    // Behind a definition, so the guard is readable only after unfolding.
    rejected_as_a_type(
        r#"
        use /std/{Nat, Bool};

        let small(n : Nat) -> Bool = n < 10;

        let Bad(n : Nat) -> Type =
            match small(n)
            | true => Bad(n - 1)
            | false => {}
            end;

        /std/print("unreachable")
        "#,
    );
}

// The control for the fixture above, in both spellings the guard reader accepts. `n > 0` excludes zero whatever the literal, and `0 < n` is the same fact with its operands exchanged — so the flip is pinned in the accepting direction as well as the refusing one, which a rejection-only fixture cannot do.
#[test]
fn a_boolean_guard_that_excludes_zero_licenses_the_decrease() {
    let direct = r#"
        use /std/{Nat};

        let Good(n : Nat) -> Type =
            match n > 0
            | true => Good(n - 1)
            | false => {}
            end;

        let held : Good(2) = ();

        /std/print("ok")
        "#;
    assert_eq!(run(direct), b"ok");

    let flipped = r#"
        use /std/{Nat};

        let Good(n : Nat) -> Type =
            match 0 < n
            | true => Good(n - 1)
            | false => {}
            end;

        let held : Good(2) = ();

        /std/print("ok")
        "#;
    assert_eq!(run(flipped), b"ok");
}

// The nonzero fact is only half of what licenses an arithmetic decrease; the other half is that the *right* operand must be a literal, and it must be at or above the smallest value that makes the operation strictly decreasing — `2` for division, because `n / 1` is `n`, and `1` for subtraction, because `n - 0` is `n`. Read the wrong way each is a fixed point graded as a descent, at which point the cost is the one `a_dispatch_that_does_not_enumerate_zero_licenses_no_decrease` spells out in full: a non-terminating type, type formation resting on a family that never forms, and (T) satisfied by a false premise.
//
// Two spellings, because the clause has two halves and each fails independently. `n / 1` sits under the dispatch that *does* enumerate zero, so the nonzero condition is satisfied and only the magnitude bound stands between this program and acceptance — and `Nat/div` folds only when both operands are literals, so with a symbolic `n` the term really does reach the reader as a division by one rather than as `n` itself. `n / k` names a parameter instead of a literal, which is the half that must hold whatever `k` turns out to be, since nothing here rules out `1`.
//
// Probed rather than closed: each asserts the diagnostic so neither can pass on an unrelated failure. Its control is `a_division_by_a_literal_above_one_licenses_the_decrease` below, which must keep compiling — refusing every division, or every division whose divisor could not be read, would take `/std/Nat/to_str` with it.
#[test]
fn an_arithmetic_decrease_needs_a_literal_operand_that_shrinks() {
    // Divisor below the least value that decreases: `n / 1` is `n`.
    rejected_as_a_type(
        r#"
        use /std/{Nat};

        let Bad(n : Nat) -> Type =
            match n
            | 0 => {}
            | _ => Bad(n / 1)
            end;

        /std/print("unreachable")
        "#,
    );

    // A divisor that is not a literal at all, so no bound can be read off it.
    rejected_as_a_type(
        r#"
        use /std/{Nat};

        let Bad(n : Nat, k : Nat) -> Type =
            match n
            | 0 => {}
            | _ => Bad(n / k, k)
            end;

        /std/print("unreachable")
        "#,
    );
}

// The control for the fixture above: the same dispatch, the same operand shape, and a divisor of `2`, which does decrease. `/std/Nat/to_str` descends on exactly this, so a rule that stopped reading divisors would fail here rather than pass quietly.
#[test]
fn a_division_by_a_literal_above_one_licenses_the_decrease() {
    let source = r#"
        use /std/{Nat};

        let Good(n : Nat) -> Type =
            match n
            | 0 => {}
            | _ => Good(n / 2)
            end;

        let held : Good(4) = ();

        /std/print("ok")
        "#;
    assert_eq!(run(source), b"ok");
}

// A nonzero fact has the extent of the arm that established it. `n > 0` hands its *true* arm the fact and its *false* arm the opposite one, so a reader that recorded the fact for the whole match rather than for one arm would license the decrease precisely where `n` is zero — which is the fixed point the condition exists to exclude, and `Bad(0)` then unfolds to `Bad(0)` forever.
//
// `a_boolean_guard_that_does_not_exclude_zero_licenses_no_decrease` cannot see this: there *neither* arm establishes nonzero, so a leaked fact has nothing to leak. This is the sibling-arm bracket, not the relation table.
//
// Both operations, because the fact is read at one site and each arrives at it carrying its own least literal — a bracket that leaked would license both.
//
// Probed rather than closed: each asserts the diagnostic. Its control is `a_boolean_guard_that_excludes_zero_licenses_the_decrease` above, whose true arm must keep descending on the same guard.
#[test]
fn a_nonzero_fact_does_not_escape_the_arm_that_established_it() {
    rejected_as_a_type(
        r#"
        use /std/{Nat};

        let Bad(n : Nat) -> Type =
            match n > 0
            | true => {}
            | false => Bad(n - 1)
            end;

        /std/print("unreachable")
        "#,
    );

    rejected_as_a_type(
        r#"
        use /std/{Nat};

        let Bad(n : Nat) -> Type =
            match n > 0
            | true => {}
            | false => Bad(n / 2)
            end;

        /std/print("unreachable")
        "#,
    );
}

// The descent gate reads a declared type, and a declared type is a term like any other.
//
// A `rec` member that erasure deletes must descend, or assuming it at its own type certifies `rec f : False = f`. Which members those are is decided in two halves, both asked semantically: whether the type is a *proposition*, and whether it *yields a sort* (`curios_analysis::yields_a_sort`, by reduction). So `U` — an ordinary definition whose value is `Type` — counts, and `Bad` below, productive and not descending, is refused; read off the spelling, it would be assumed at a type that is `Type` in every sense but the syntactic one.
//
// Nothing is forged from it here: the whole-module obligation refuses the *use* of a partial type anyway, so what the gate guards is the local guarantee it exists to give — `check_group`'s own claim that a member erasure deletes has been shown to terminate before its body is checked against it. An alias is not a different claim, and reading the spelling would make it one.
//
// Its control is `an_aliased_sort_that_descends_is_still_accepted`, which keeps the same alias at a recursion that does descend: refusing every member declared through one would pass this and fail that.
#[test]
fn a_sort_reached_through_an_alias_still_needs_descent() {
    rejected_as_a_type(
        r#"
        use /std/{Nat, Option};

        let U : Type = Type;

        let wrap(A : U) -> U = Option(A);

        let Bad(n : Nat) -> U = wrap(Bad(n));

        /std/print("unreachable")
        "#,
    );
}

// The control for the fixture above: the same aliased sort at a recursion that descends is ordinary code and still compiles.
#[test]
fn an_aliased_sort_that_descends_is_still_accepted() {
    let source = r#"
        use /std/{Nat};

        let U : Type = Type;

        let Good(n : Nat) -> U =
            match n
            | 0 => {}
            | _ => Good(n - 1)
            end;

        let held : Good(2) = ();

        /std/print("ok")
        "#;
    assert_eq!(run(source), b"ok");
}

// The accessibility shape, which is the one well-founded recursion in a proof needs: the recursive call descends through `below(m, lt)`, an application of a function-typed constructor payload, rather than through a syntactically smaller subterm. A function-typed payload is a branching node whose children are its applications, so the application grades below the constructor for the reason the payload does, and the group descends on `acc`. The result is `Prop`-sorted, so this is (V) reading the verdict, and it is the shape every well-founded fixpoint takes.
#[test]
fn a_call_through_a_constructor_payload_descends() {
    let source = r#"
        use /std/{Nat};
        use /std/Bool/{Holds};

        induct Accessible(@A : Type, R : (A, A) -> Prop) : (A) -> Prop
        | intro(@x : A, below : (y : A, r : R(y, x)) -> Accessible(R)(y)) : (x)
        end

        let strong(
            P : (Nat) -> Prop,
            step : (n : Nat, ih : (m : Nat, lt : Holds(m < n)) -> P(m)) -> P(n),
            n : Nat,
            acc : Accessible((a : Nat, b : Nat) => Holds(a < b))(n),
        ) -> P(n) =
            match acc : (w, _) => P(w)
            | intro(@w, below) => step(w, (m, lt) => strong(P, step, m, below(m, lt)))
            end;

        /std/print("ok")
        "#;
    assert_eq!(run(source), b"ok");
}

// The control: the same call through a function that is *not* a constructor payload. `again` is a parameter, so nothing relates `again(n)` to anything smaller, the group does not descend, and (V) refuses the proof position. A rule that read every application of a binder as a decrease would accept this, and `loop(P, 0, (m) => m)` then proves any `P`.
#[test]
fn a_call_through_a_parameter_bound_function_does_not_descend() {
    rejected_as_a_proof(
        r#"
        use /std/{Nat};

        let loop(P : Prop, n : Nat, again : (m : Nat) -> Nat) -> P =
            loop(P, again(n), again);

        /std/print("unreachable")
        "#,
    );
}

// The library's recursion along `<` needs no accessibility: `/std/Nat/descend` recurses on a bound its measure fits under, so a proposition recursing along `<` is written once against the library and is total by the successor's descent. The proof is consumed in `List/get`'s erased bound, so nothing about it runs.
#[test]
fn the_library_recursion_on_a_bound_serves_a_proof() {
    let source = r#"
        use /std/{Nat, Str, Char, List};
        use /std/Bool/{True, Holds};
        let two_more(n: Nat) -> Holds(n < n + 2) =
            Nat/descend((k: Nat) => k, (k) => Holds(k < k + 2), (k, ih) => True/qed(), n);
        /std/print(Str/of_char(List/get(@Char, ['a', 'b', 'c'], 1, @two_more(1))))
        "#;
    assert_eq!(run(source), b"b");
}

// The same recursion at a motive that is a type depending on its argument: `pick(n)` has type `Shape(n)`, the unit type at zero and `Nat` above it. Written in a type, `pick(5)` puts `recurse` and its local loop under obligation (T), and the checkers reduce it on the bound to `4` with no proof read; the running program computes the same value with the bound's proofs erased.
#[test]
fn the_library_recursion_on_a_bound_computes_at_a_dependent_motive() {
    let source = r#"
        use /std/{Nat, Eq};
        let Shape(n: Nat) -> Type = match n | 0 => {} | _ + 1 => Nat end;
        let pick(n: Nat) -> Shape(n) =
            Nat/descend((k: Nat) => k, Shape, (k, _) => match k | 0 => () | kp + 1 => kp end, n);
        let _computed: Eq()(pick(5), 4) = Eq/refl();
        /std/print(Nat/to_str(pick(5)))
        "#;
    assert_eq!(run(source), b"4");
}

// A convoy written by hand around the recursive call: the arm generalizes the witness it descends on and applies the lambda back to it. The analysis grades the redex as its contractum, so `rest` is still read as the payload of `d` and the proof descends.
#[test]
fn a_hand_written_convoy_keeps_its_descent_visible() {
    let source = r#"
        use /std/{Nat, Byte, Bytes, Eq, Str};
        use /std/Str/{Scan, step};

        induct Utf8: (Scan, Bytes) -> Prop
        | stop(): (Scan/lead(), x[])
        | more(c: Byte, st: Scan, t: Bytes, rest: Utf8(step(c, st), t)): (st, x[c, ..t])
        end

        let dv(s: Scan, @b: Bytes, d: Utf8(s, b)) -> Eq()(0, 0) =
            match d
            | stop() => Eq/refl()
            | more(c, st, t, rest) => ((r: Utf8(step(c, st), t)) => dv(step(c, st), r))(rest)
            end;

        /std/print("ok")
        "#;
    assert_eq!(run(source), b"ok");
}

// The same descent through a `let` alias of the payload, which is a redex by another spelling.
#[test]
fn a_let_alias_keeps_its_descent_visible() {
    let source = r#"
        use /std/{Nat, Byte, Bytes, Eq, Str};
        use /std/Str/{Scan, step};

        induct Utf8: (Scan, Bytes) -> Prop
        | stop(): (Scan/lead(), x[])
        | more(c: Byte, st: Scan, t: Bytes, rest: Utf8(step(c, st), t)): (st, x[c, ..t])
        end

        let dv(s: Scan, @b: Bytes, d: Utf8(s, b)) -> Eq()(0, 0) =
            match d
            | stop() => Eq/refl()
            | more(c, st, t, rest) => let r = rest; dv(step(c, st), r)
            end;

        /std/print("ok")
        "#;
    assert_eq!(run(source), b"ok");
}

// A program a synthesized convoy would refuse: the goal's implicit is still a metavariable when the inner match is elaborated, its spine names every local, and a convoy generalizing the hypothesis `d` would hand the walk a lambda-bound copy it cannot read as below the parameter. At the ambient result there is no convoy: `rest` is the payload of `d`, and the proof descends.
#[test]
fn a_proof_over_a_dependent_hypothesis_descends() {
    let source = r#"
        use /std/{Nat, Byte, Bytes, Eq, Str};
        use /std/Str/{Scan, step};

        induct Utf8: (Scan, Bytes) -> Prop
        | stop(): (Scan/lead(), x[])
        | more(c: Byte, st: Scan, t: Bytes, rest: Utf8(step(c, st), t)): (st, x[c, ..t])
        end

        let dv(s : Scan, @b : Bytes, d : Utf8(s, b)) -> Eq()(0, 0) =
            match s
            | lead() => match d | stop() => Eq/refl() | more(c, _, t, rest) => dv(step(c, Scan/lead()), rest) end
            | cont(_, _, _) => match d | more(c, _, _, rest) => dv(step(c, s), rest) end
            | bad() => match d | more(c, _, _, rest) => dv(step(c, Scan/bad()), rest) end
            end;

        /std/print("ok")
        "#;
    assert_eq!(run(source), b"ok");
}

// A walk that halves a span descends on no list, so `List/balanced` descends on the span's length, every call on its quotient by two, which the termination check reads with no count beside it. What that buys is (T) for everything built on it: `Str/repeat` may stand in a type, which the equations state at the values the walk reaches. Each single carries the proof that its index is below `n`, and a join reads its parts through it, first and last included.
#[test]
fn a_halving_walk_descends_on_its_span_halved_and_stands_in_a_type() {
    let source = r#"
        use /std/{Str, Eq, List};

        let _in_a_type: Eq()(Str/repeat("ab", 2), "abab") = Eq/refl();
        let _summed: Eq()(List/balanced(5, 0, (i, _) => i + 1, (l, r) => l + r), 15) = Eq/refl();
        let _joined: Eq()(Str/join(",", ["a", "b", "c", "d", "e"]), "a,b,c,d,e") = Eq/refl();

        /std/print(Str/repeat("ab", 3))
        "#;
    assert_eq!(run(source), b"ababab");
}

// The subtraction half of `an_arithmetic_decrease_needs_a_literal_operand_that_shrinks`: `n - k` decreases only for a literal `k >= 1`, because `Nat` subtraction saturates and `n - 0` is `n` itself — a fixed point, not a descent. The division sibling pins `k >= 2`; nothing pinned subtraction's floor, so a reader that let `0` through would grade `Bad(n - 0)` as descending and admit a type that unfolds to itself forever. The control is `a_dispatch_that_enumerates_zero_licenses_the_decrease`, whose `Bad(n - 1)` is the same shape one above the floor.
#[test]
fn a_subtraction_by_zero_licenses_no_decrease() {
    rejected_as_a_type(
        r#"
        use /std/{Nat};

        let Bad(n : Nat) -> Type =
            match n
            | 0 => {}
            | _ => Bad(n - 0)
            end;

        /std/print("unreachable")
        "#,
    );
}

// A `choose` ladder reaches the same nonzero reader a headed boolean match does, by a different spelling: each condition arm desugars to a boolean match whose true branch is the arm and whose false branch is the rest of the ladder. The arm-scoping `a_nonzero_fact_does_not_escape_the_arm_that_established_it` holds for a `match` must survive that desugaring — a guard that admits zero (`n < 10`) licenses no decrease, and only one that excludes it (`n > 0`) does.
#[test]
fn a_choose_guard_that_does_not_exclude_zero_licenses_no_decrease() {
    rejected_as_a_type(
        r#"
        use /std/{Nat};

        let Bad(n : Nat) -> Type =
            choose
            | n < 10 => Bad(n - 1)
            | _ => {}
            end;

        /std/print("unreachable")
        "#,
    );
}

// The control for the fixture above: `n > 0` excludes zero through the same desugaring, so the ladder's true arm descends and the type is accepted.
#[test]
fn a_choose_guard_that_excludes_zero_licenses_the_decrease() {
    let source = r#"
        use /std/{Nat};

        let Good(n : Nat) -> Type =
            choose
            | n > 0 => Good(n - 1)
            | _ => {}
            end;

        let held : Good(2) = ();

        /std/print("ok")
        "#;
    assert_eq!(run(source), b"ok");
}

// The size-change closure's own verdict, at the source level: a recursion that hands its two arguments on exchanged does not descend, however one of them shrinks. `swap`'s first arm passes `(y, 0)` and its second `(k, y + 1)`; neither argument is below the caller's in the position it lands, and the swap's call matrix composed with itself is an idempotent that carries `Same` on its diagonal and no `Less` — a path that can repeat forever with nothing getting smaller — so the group is `Partial` and the proof `swap(3, 0)` is refused. The accepting direction of the same closure — two members that descend only across their composite cycle — is `/std/List/sort_by`'s `merge`, which `the_prelude_shapes_a_user_program_leans_on_still_elaborate` carries; this is the refusing direction the seed's "descent through an exchange of arguments" names.
#[test]
fn a_recursion_that_hands_its_arguments_on_exchanged_does_not_descend() {
    rejected(
        r#"
        use /std/{Nat};
        use /std/Bool/{False};

        let swap(x : Nat, y : Nat) -> False =
            match x
            | 0 => swap(y, 0)
            | k + 1 => swap(k, y + 1)
            end;

        /std/print("unreachable")
        "#,
    );
}
