//! What licenses a decrease and what does not: a dispatch or guard that rules out zero, a shrinking literal operand, a subtraction past a layer, a payload, an element below its list, and arguments handed on exchanged.

use {
    crate::tests::{rules::test_support::*, test_support::*},
    curios_analysis::Erased,
};

// The arithmetic decrease is licensed by the *arm*, not by the operator. `n - 1` is smaller than `n` only where `n` is known nonzero — `Nat` subtraction saturates, so `0 - 1` is `0` and the "decrease" is a fixed point — and the arm a `Nat` match falls to past its literals establishes nonzero exactly when `0` is one of them: the successor's arm where `0` is the only literal, a dispatch's default otherwise. This fixture omits `0`, so nothing rules it out and the call may not be graded as descending.
//
// Read the wrong way the cost is a non-terminating *type*, which is the negative knot `a_type_level_rec_cannot_tie_the_negative_knot` above spells out in full: `Down(0)` would unfold to `Down(0)` forever while the classifier reported the group total, type formation would rest on a family that never forms, and type totality — whose whole job is that nothing reachable from a type position is partial — would have been satisfied by a false premise rather than by a fact.
//
// This side condition carries more weight than its size suggests. Across the corpus, the enclosing arms' refinements change thousands of call grades, and every change is an upgrade away from `Unknown` — to `Less` or `Same`, never a downgrade. The arm facts are what make the corpus's recursion provable at all, so the conditions under which an arm may establish one are the accepting direction throughout.
//
// Probed rather than closed: the fixture asserts the diagnostic so it cannot pass on an unrelated failure. Its control is the same recursion with `0` enumerated, which must keep compiling — refusing it would take `/std/Nat/to_str` and every other bounded `Nat` descent with it.
#[test]
fn a_dispatch_that_does_not_enumerate_zero_licenses_no_decrease() {
    control(
        r#"
        use /std/{Nat};

        let Down(n : Nat) -> Type =
            match n
            | 0 => {}
            | _ => Down(n - 1)
            end;

        let held : Down(2) = ();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| 0 => {}", "| 1 => {}")])
    .by_elaborator_in("/Down", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Type,
                ..
            }
        )
    });
}

// A `Nat` dispatch is not the only thing that can establish nonzero. A *boolean* arm does it too, when its scrutinee compares a tracked binder against a literal: the classifier reads the comparison, and the arm in which it holds either excludes zero or does not. `/std/Nat/to_str` descends on exactly that — the false arm of `n < 10` gives `n >= 10`, which is what licenses `to_str(n / 10)` — so the route is live throughout the corpus, where the pair above probes the dispatch route.
//
// Three spellings reach the same reader and all three must refuse, because each exercises a different piece of it. `n < 10` is already an intrinsic after elaboration, so it tests the relation table alone. `10 > n` arrives with its operands the other way round and is only readable once flipped, so it tests the flip — a flip that turned `n < 10` into `n > 10` would report nonzero for an arm that admits zero. `guard(n)` is an ordinary definition, so it is readable only through the same bounded weak-head unfolding the shape reader uses, and an unfolding that lost track of which arm it was in would do the same. In every one the true arm gives `n < 10`, which admits zero, so `n - 1` is a fixed point at zero and the group does not descend.
//
// Read the wrong way the cost is the same as for the dispatch route and is spelled out there: a non-terminating type, type formation resting on a family that never forms, and type totality satisfied by a false premise.
//
// Probed rather than closed: each asserts the diagnostic so none can pass on an unrelated failure. Each is derived from a control whose guard excludes zero in the same spelling, `n > 0`, `0 < n` or a `guard(n)` that is `n > 0`, which must keep compiling — refusing them would take `/std/Nat/to_str` with it, and the flipped one pins the flip in the accepting direction as well as the refusing one.
#[test]
fn a_boolean_guard_that_does_not_exclude_zero_licenses_no_decrease() {
    control(
        r#"
        use /std/{Nat};

        let Down(n : Nat) -> Type =
            match n > 0
            | true => Down(n - 1)
            | false => {}
            end;

        let held : Down(2) = ();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("match n > 0", "match n < 10")])
    .by_elaborator_in("/Down", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Type,
                ..
            }
        )
    });
    control(
        r#"
        use /std/{Nat};

        let Down(n : Nat) -> Type =
            match 0 < n
            | true => Down(n - 1)
            | false => {}
            end;

        let held : Down(2) = ();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("match 0 < n", "match 10 > n")])
    .by_elaborator_in("/Down", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Type,
                ..
            }
        )
    });
    control(
        r#"
        use /std/{Nat, Bool};

        let guard(n : Nat) -> Bool = n > 0;

        let Down(n : Nat) -> Type =
            match guard(n)
            | true => Down(n - 1)
            | false => {}
            end;

        let held : Down(2) = ();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("= n > 0;", "= n < 10;")])
    .by_elaborator_in("/Down", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Type,
                ..
            }
        )
    });
}

// The nonzero fact is only half of what licenses an arithmetic decrease; the other half is that the *right* operand must be a literal, and it must be at or above the smallest value that makes the operation strictly decreasing — `2` for division, because `n / 1` is `n`, and `1` for subtraction, because `n - 0` is `n`. Read the wrong way each is a fixed point graded as a descent, at which point the cost is the one `a_dispatch_that_does_not_enumerate_zero_licenses_no_decrease` spells out in full: a non-terminating type, type formation resting on a family that never forms, and type totality satisfied by a false premise.
//
// Two spellings, because the clause has two halves and each fails independently. `n / 1` sits under the dispatch that *does* enumerate zero, so the nonzero condition is satisfied and only the magnitude bound stands between this program and acceptance — and `Nat/div` folds only when both operands are literals, so with a symbolic `n` the term really does reach the reader as a division by one rather than as `n` itself. `n / k` names a parameter instead of a literal, which is the half that must hold whatever `k` turns out to be, since nothing here rules out `1`.
//
// Probed rather than closed: each asserts the diagnostic so neither can pass on an unrelated failure. Each is derived from a control dividing by `2`, which must keep compiling — refusing every division, or every division whose divisor could not be read, would take `/std/Nat/to_str` with it.
#[test]
fn an_arithmetic_decrease_needs_a_literal_operand_that_shrinks() {
    control(
        r#"
        use /std/{Nat};

        let Down(n : Nat) -> Type =
            match n
            | 0 => {}
            | _ => Down(n / 2)
            end;

        let held : Down(4) = ();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Down(n / 2)", "Down(n / 1)")])
    .by_elaborator_in("/Down", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Type,
                ..
            }
        )
    });
    control(
        r#"
        use /std/{Nat};

        let Down(n : Nat, k : Nat) -> Type =
            match n
            | 0 => {}
            | _ => Down(n / 2, k)
            end;

        let held : Down(4, 2) = ();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Down(n / 2, k)", "Down(n / k, k)")])
    .by_elaborator_in("/Down", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Type,
                ..
            }
        )
    });
}

// A nonzero fact has the extent of the arm that established it. `n > 0` hands its *true* arm the fact and its *false* arm the opposite one, so a reader that recorded the fact for the whole match rather than for one arm would license the decrease precisely where `n` is zero — which is the fixed point the condition exists to exclude, and `Down(0)` then unfolds to `Down(0)` forever.
//
// `a_boolean_guard_that_does_not_exclude_zero_licenses_no_decrease` cannot see this: there *neither* arm establishes nonzero, so a leaked fact has nothing to leak. This is the sibling-arm bracket, not the relation table.
//
// Both operations, because the fact is read at one site and each arrives at it carrying its own least literal — a bracket that leaked would license both.
//
// Probed rather than closed: each asserts the diagnostic. Each is derived from a control whose true arm descends on the same guard, which must keep compiling.
#[test]
fn a_nonzero_fact_does_not_escape_the_arm_that_established_it() {
    control(
        r#"
        use /std/{Nat};

        let Down(n : Nat) -> Type =
            match n > 0
            | true => Down(n - 1)
            | false => {}
            end;

        let held : Down(2) = ();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "| true => Down(n - 1)\n            | false => {}",
        "| true => {}\n            | false => Down(n - 1)",
    )])
    .by_elaborator_in("/Down", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Type,
                ..
            }
        )
    });
    control(
        r#"
        use /std/{Nat};

        let Down(n : Nat) -> Type =
            match n > 0
            | true => Down(n / 2)
            | false => {}
            end;

        let held : Down(2) = ();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "| true => Down(n / 2)\n            | false => {}",
        "| true => {}\n            | false => Down(n / 2)",
    )])
    .by_elaborator_in("/Down", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Type,
                ..
            }
        )
    });
}

// The descent gate reads a declared type, and a declared type is a term like any other.
//
// A `rec` member that erasure deletes must descend, or assuming it at its own type certifies `rec f : False = f`. Which members those are is decided in two halves, both asked semantically: whether the type is a *proposition*, and whether it *yields a sort* (`curios_analysis::yields_a_sort_where`, by reduction). So `U` — an ordinary definition whose value is `Type` — counts, and `Down` below, productive and not descending, is refused; read off the spelling, it would be assumed at a type that is `Type` in every sense but the syntactic one.
//
// Nothing is forged from it here: the whole-module obligation refuses the *use* of a partial type anyway, so what the gate guards is the local guarantee it exists to give — `check_group`'s own claim that a member erasure deletes has been shown to terminate before its body is checked against it. An alias is not a different claim, and reading the spelling would make it one.
//
// Its control keeps the same alias at a recursion that does descend: refusing every member declared through one would pass the refusal and fail the control.
#[test]
fn a_sort_reached_through_an_alias_still_needs_descent() {
    control(
        r#"
        use /std/{Nat, Option};

        let U : Type = Type;

        let wrap(A : U) -> U = Option(A);

        let Down(n : Nat) -> U =
            match n
            | 0 => {}
            | _ => wrap(Down(n - 1))
            end;

        let held : Down(1) = Option/none();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("wrap(Down(n - 1))", "wrap(Down(n))")])
    .by_elaborator_in("/Down", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Type,
                ..
            }
        )
    });
}

// The accessibility shape, which is the one well-founded recursion in a proof needs: the recursive call descends through `below(m, lt)`, an application of a function-typed constructor payload, rather than through a syntactically smaller subterm. A function-typed payload is a branching node whose children are its applications, so the application grades below the constructor for the reason the payload does, and the group descends on `acc`. The result is `Prop`-sorted, so this is proof totality reading the verdict, and it is the shape every well-founded fixpoint takes.
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
            match acc
            | intro(@w, below) => step(w, (m, lt) => strong(P, step, m, below(m, lt)))
            end;

        /std/print("ok")
        "#;
    assert_eq!(run(source), b"ok");
}

// A walk that halves a span descends on no list, so `List/balanced` descends on the span's length, every call on its quotient by two, which the termination check reads with no count beside it. What that buys is type totality for everything built on it: `Str/repeat` may stand in a type, which the equations state at the values the walk reaches. Each single carries the proof that its index is below `n`, and a join reads its parts through it, first and last included.
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

// The subtraction half of `an_arithmetic_decrease_needs_a_literal_operand_that_shrinks`: `n - k` decreases only for a literal `k >= 1`, because `Nat` subtraction saturates and `n - 0` is `n` itself — a fixed point, not a descent. The division sibling pins `k >= 2`; nothing pinned subtraction's floor, so a reader that let `0` through would grade `Down(n - 0)` as descending and admit a type that unfolds to itself forever. The control is the same shape one above the floor, `n - 1`.
#[test]
fn a_subtraction_by_zero_licenses_no_decrease() {
    control(
        r#"
        use /std/{Nat};

        let Down(n : Nat) -> Type =
            match n
            | 0 => {}
            | _ => Down(n - 1)
            end;

        let held : Down(2) = ();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Down(n - 1)", "Down(n - 0)")])
    .by_elaborator_in("/Down", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Type,
                ..
            }
        )
    });
}

// A successor arm's subtraction past the one layer it peels. The certifier reads the arm under its case equation, `n = x + 1`, and `(x + 1) - 2` cancels to `x - 1` before the grader sees it, so what licenses the call there is that `x - 1` is at most `x`, below the `x + 1` the parameter stands for; the elaborator reads `n - 2` as written, below the successor `n` is. The refused sibling enumerates `1` instead, so nothing rules zero out and `n - 2` is at most `n` itself: a fixed point at zero.
#[test]
fn a_subtraction_past_the_layer_a_successor_arm_peels_descends() {
    control(
        r#"
        use /std/{Nat};

        let Down(n : Nat) -> Type =
            match n
            | 0 => {}
            | _ => Down(n - 2)
            end;

        let held : Down(4) = ();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| 0 => {}", "| 1 => {}")])
    .by_elaborator_in("/Down", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Type,
                ..
            }
        )
    });
}

// A match with no head reaches the same nonzero reader a headed boolean match does, by a different spelling: an arm whose one guard is a condition compiles to a boolean match whose true branch is the arm and whose false branch is the arms after it. The arm-scoping `a_nonzero_fact_does_not_escape_the_arm_that_established_it` holds for a headed `match` must survive that compilation — a guard that admits zero (`n < 10`) licenses no decrease, and only one that excludes it (`n > 0`) does. The control's guard is `n > 0`, which excludes zero through the same compilation.
#[test]
fn a_guard_with_no_head_that_does_not_exclude_zero_licenses_no_decrease() {
    control(
        r#"
        use /std/{Nat};

        let Down(n : Nat) -> Type =
            match
            | n > 0 => Down(n - 1)
            | _ => {}
            end;

        let held : Down(2) = ();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| n > 0 =>", "| n < 10 =>")])
    .by_elaborator_in("/Down", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Type,
                ..
            }
        )
    });
}

// The size-change closure's own verdict, at the source level: a recursion that hands its two arguments on exchanged does not descend, however one of them shrinks. `swap`'s first arm passes `(y, 0)` and its second `(k, y + 1)`; neither argument is below the caller's in the position it lands, and the swap's call matrix composed with itself is an idempotent that carries `Same` on its diagonal and no `Less` — a path that can repeat forever with nothing getting smaller — so the group is `Partial` and the proof `swap(3, 0)` is refused. The accepting direction of the same closure — two members that descend only across their composite cycle — is `/std/List/sort_by`'s `merge`, which `the_prelude_shapes_a_user_program_leans_on_still_elaborate` carries; this is the refusing direction the seed's "descent through an exchange of arguments" names. The control states `True` and descends on `x`, its base an answer rather than the exchange.
#[test]
fn a_recursion_that_hands_its_arguments_on_exchanged_does_not_descend() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True, False};

        let swap(x : Nat, y : Nat) -> True =
            match x
            | 0 => True/qed()
            | k + 1 => swap(k, y + 1)
            end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("-> True =", "-> False ="),
        ("| 0 => True/qed()", "| 0 => swap(y, 0)"),
    ])
    .by_elaborator_in("/swap", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/swap", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}

// `/sys/List/get` hands out one of its list's elements, below the list as a case of the list hands one out, so a recursion through the element at an index of a tree's children descends. The refusal reads the element out of a list holding the tree itself, which a run reaches as the same call again.
#[test]
fn an_element_read_at_an_index_is_below_its_list() {
    control(
        r#"
        use /std/{Nat, Bool, List};

        induct Rose: Type
        | rose(Nat, List(Rose))
        end

        let leftmost(t: Rose) -> Nat =
            match t
            | rose(n, cs) =>
                match 0 < List/len(cs)
                | true => leftmost(List/get(cs, 0))
                | false => n
                end
            end;

        let Small(t: Rose) -> Prop = Bool/Holds(leftmost(t) == 0);

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("leftmost(List/get(cs, 0))", "leftmost(List/get([t], 0))")])
    .by_elaborator_in("/Small", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Small", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}
