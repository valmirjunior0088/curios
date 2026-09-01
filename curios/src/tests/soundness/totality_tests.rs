//! The size-change obligation: what licenses a decrease, and the type-level loops that are diagnosed rather than aborted.

//! End-to-end coverage for the two totality obligations.
//!
//! Erasure deletes types and it deletes `Prop`-sorted proofs, and both must be total: a divergent type breaks type formation, a divergent proof proves anything. What erasure *keeps* may diverge freely, which is why every rejection here is of a position and never of a definition.
//!
//! The size lattice and the classifier are unit-tested in `curios-analysis/src/totality/tests.rs`; these check what a user can observe, through the prelude-replay path a real program takes — where the analysis sees only the user suffix and reads the prelude's verdicts back from the archive.
//!
//! Each rejection asserts the *diagnostic*, not merely that compilation failed. A soundness test that accepts any error is worthless: a typo in the fixture would pass it while the hole stayed open.

use crate::tests::run;

use super::test_support::*;

// The route no analysis of *value*-level partiality can see. The exploit has no value-level `rec`, no `exit`, and no negative occurrence in any declaration: a lambda, a constructor application, and one application. The knot is tied entirely by `rec Bad : Type`, which `check_positivity` never looks at, because a `rec` is neither an `induct` nor a `struct`.
#[test]
fn a_type_level_rec_cannot_tie_the_negative_knot() {
    rejected(&format!(
        r#"
        {SINK}

        let Bad : Type = Sink(Bad);

        let delta(x : Bad) -> /std/False =
            match x
            | sink(g) => g(x)
            end;

        let boom : /std/False = delta(Sink/sink(delta));

        /std/print("unreachable")
        "#
    ));
}

// The same knot behind a projection. A rule keyed on "the `rec` member is itself sort-valued" misses this: `P`'s type is a tuple, and only `P.0` is a type.
// The arithmetic decrease is licensed by the *arm*, not by the operator. `n - 1` is smaller than `n` only where `n` is known nonzero — `Nat` subtraction saturates, so `0 - 1` is `0` and the "decrease" is a fixed point — and a `Nat` dispatch's default arm establishes nonzero exactly when `0` is one of the enumerated cases. This fixture omits `0`, so nothing rules it out and the call may not be graded as descending.
//
// Read the wrong way the cost is a non-terminating *type*, which is the negative knot `a_type_level_rec_cannot_tie_the_negative_knot` above spells out in full: `Bad(0)` would unfold to `Bad(0)` forever while the classifier reported the group total, type formation would rest on a family that never forms, and (T) — whose whole job is that nothing reachable from a type position is partial — would have been satisfied by a false premise rather than by a fact.
//
// This side condition carries more weight than its size suggests. Instrumenting the classifier across the whole corpus counts 1365 call gradings and 4117 grade *changes* from the enclosing arms' refinements, every one of them an upgrade away from `Unknown` — 2810 to `Less` and 1307 to `Same`, and not a single downgrade. The arm facts are what make the corpus's recursion provable at all, so the conditions under which an arm may establish one are the accepting direction throughout.
//
// Probed rather than closed: the rule already held when this was written, and the fixture asserts the diagnostic so it cannot pass on an unrelated failure. Its control is `a_dispatch_that_enumerates_zero_licenses_the_decrease`, the same recursion with `0` enumerated, which must keep compiling — refusing it would take `/std/Nat/to_str` and every other bounded `Nat` descent with it.
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

// A `Nat` dispatch is not the only thing that can establish nonzero, and the other one had no fixture. A *boolean* arm does it too, when its scrutinee compares a tracked binder against a literal: the classifier reads the comparison, and the arm in which it holds either excludes zero or does not. `/std/Nat/to_str` descends on exactly that — the false arm of `n < 10` gives `n >= 10`, which is what licenses `to_str(n / 10)` — so the route is live throughout the corpus while the pair above probed only the dispatch route.
//
// Three spellings reach the same reader and all three must refuse, because each exercises a different piece of it. `n < 10` is already an intrinsic after elaboration, so it tests the relation table alone. `10 > n` arrives with its operands the other way round and is only readable once flipped, so it tests the flip — a flip that turned `n < 10` into `n > 10` would report nonzero for an arm that admits zero. `small(n)` is an ordinary definition, so it is readable only through the same bounded weak-head unfolding the shape reader uses, and an unfolding that lost track of which arm it was in would do the same. In every one the true arm gives `n < 10`, which admits zero, so `n - 1` is a fixed point at zero and the group does not descend.
//
// Read the wrong way the cost is the same as for the dispatch route and is spelled out there: a non-terminating type, type formation resting on a family that never forms, and (T) satisfied by a false premise.
//
// Probed rather than closed: all three already refused when this was written, and each asserts the diagnostic so none can pass on an unrelated failure. The control is `a_boolean_guard_that_excludes_zero_licenses_the_decrease`, which must keep compiling in both spellings — refusing them would take `/std/Nat/to_str` with it.
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

// The nonzero fact is only half of what licenses an arithmetic decrease, and the other half had no fixture at all: the *right* operand must be a literal, and it must be at or above the smallest value that makes the operation strictly decreasing — `2` for division, because `n / 1` is `n`, and `1` for subtraction, because `n - 0` is `n`. Read the wrong way each is a fixed point graded as a descent, at which point the cost is the one `a_dispatch_that_does_not_enumerate_zero_licenses_no_decrease` spells out in full: a non-terminating type, type formation resting on a family that never forms, and (T) satisfied by a false premise.
//
// Two spellings, because the clause has two halves and each fails independently. `n / 1` sits under the dispatch that *does* enumerate zero, so the nonzero condition is satisfied and only the magnitude bound stands between this program and acceptance — and `Nat/div` folds only when both operands are literals, so with a symbolic `n` the term really does reach the reader as a division by one rather than as `n` itself. `n / k` names a parameter instead of a literal, which is the half that must hold whatever `k` turns out to be, since nothing here rules out `1`.
//
// Probed rather than closed: both already refused when this was written, and each asserts the diagnostic so neither can pass on an unrelated failure. Its control is `a_division_by_a_literal_above_one_licenses_the_decrease` below, which must keep compiling — refusing every division, or every division whose divisor could not be read, would take `/std/Nat/to_str` with it.
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

// A nonzero fact has the extent of the arm that established it, and nothing had pinned that. `n > 0` hands its *true* arm the fact and its *false* arm the opposite one, so a reader that recorded the fact for the whole match rather than for one arm would license the decrease precisely where `n` is zero — which is the fixed point the condition exists to exclude, and `Bad(0)` then unfolds to `Bad(0)` forever.
//
// `a_boolean_guard_that_does_not_exclude_zero_licenses_no_decrease` cannot see this: there *neither* arm establishes nonzero, so a leaked fact has nothing to leak. This is the sibling-arm bracket, not the relation table.
//
// Both operations, because the fact is read at one site and each arrives at it carrying its own least literal — a bracket that leaked would license both.
//
// Probed rather than closed: both already refused when this was written, and each asserts the diagnostic. Its control is `a_boolean_guard_that_excludes_zero_licenses_the_decrease` above, whose true arm must keep descending on the same guard.
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

#[test]
fn a_partial_type_behind_a_projection_is_still_a_type() {
    rejected(&format!(
        r#"
        {SINK}

        let P : {{Type, /std/Nat}} = (Sink(P.0), 0);

        let delta(x : P.0) -> /std/False =
            match x
            | sink(g) => g(x)
            end;

        let boom : /std/False = delta(Sink/sink(delta));

        /std/print("unreachable")
        "#
    ));
}

// Why (T) has to be the aggressive reading. Nothing here is a partial type former: `Shape` is structurally recursive and total, and `inf` is an ordinary partial *value* of an ordinary data type. The dangerous type exists only because a total function was applied to a bottom argument, so a rule that looks at what a type-level eliminator scrutinizes does not see it.
#[test]
fn a_total_type_function_applied_to_a_partial_value_is_rejected() {
    rejected(&format!(
        r#"
        {SINK}

        induct F : pub Type
        | stop()
        | more(rest : F)
        end

        let Shape(f : F) -> Type =
            match f
            | stop() => /std/False
            | more(rest) => Sink(Shape(rest))
            end;

        let inf : F = F/more(inf);

        let delta(x : Shape(inf)) -> /std/False =
            match x
            | sink(g) => g(x)
            end;

        let boom : /std/False = delta(Sink/sink(delta));

        /std/print("unreachable")
        "#
    ));
}

// The entrypoint's trailing expression is not a top-level item — lowering leaves it in `Module::body`. An obligation seeded only from `items` sees nothing here. The only items below are `Sink` and `consume`, and neither is partial; the offending `rec` lives entirely inside the final term.
#[test]
fn the_entrypoint_expression_is_not_a_blind_spot() {
    rejected(&format!(
        r#"
        {SINK}

        let consume(u : {{}}) -> {{}} = u;

        consume(
            let forge(x : (let Bad : Type = Sink(Bad); Bad)) -> {{}} = ();
            ()
        )
        "#
    ));
}

// The shape that used to abort the compiler rather than reject the program. Reducing `Bad` rebuilds an arrow whose domain is `Bad`, so elaborating the first *use* overflowed the stack — long before any whole-module gate could run. This is what the local form at `rec` elaboration exists for, and the only claim it makes is that the compiler answers instead of dying.
#[test]
fn a_type_level_rec_through_an_arrow_is_diagnosed_not_aborted() {
    rejected(
        r#"
        let Bad : Type = (Bad) -> /std/False;

        let delta(x : Bad) -> /std/False = x(x);

        let boom : /std/False = delta(delta);

        /std/print("unreachable")
        "#,
    );
}

// The same shape as a *local* `rec`. `Item::Rec` and `Subterm::Rec` are separate elaboration paths, and the second one still aborted after the first was fixed.
#[test]
fn a_local_type_level_rec_through_an_arrow_is_diagnosed_too() {
    rejected(
        r#"
        let consume(u : {}) -> {} = u;

        consume(
            let forge(x : (let Bad : Type = (Bad) -> /std/False; Bad)) -> {} = ();
            ()
        )
        "#,
    );
}

// General recursion is untouched wherever erasure keeps it. `collatz` cannot pass any size-change checker — it recurses on a computed quotient, and whether it terminates at all is an open problem — and it runs anyway, because its result is a `Nat` the program actually consumes.
#[test]
fn a_partial_definition_stays_usable_in_a_program() {
    let source = r#"
        use /std/{Nat};

        let collatz(n : Nat, steps : Nat) -> Nat =
            choose
            | n <= 1 => steps
            | n % 2 == 0 => collatz(n / 2, steps + 1)
            | _ => collatz(3 * n + 1, steps + 1)
            end;

        /std/print(Nat/to_str(collatz(6, 0)))
        "#;
    assert_eq!(run(source), b"8");
}

// (T) is seeded two ways, because neither reaches what the other does. The walk finds types where they are *written* — annotations, motives, telescopes, and the body of any definition whose type ends in a sort. It has no case for an application's arguments, so a type handed to a function is written nowhere it looks. The settle records find those, because a term whose own type is a sort is a type wherever it appears.
//
// The argument form is the one that reaches this gate at all. Written as an annotation, `Shape(inf)` is *forced* while the binder elaborates and the step budget stops it there — fail-closed, but before any whole-module pass runs. Passed as a type argument it is never forced, so elaboration succeeds and only the gate is left. That form was accepted until the settle records were added.
//
// Nothing here is `Prop`-typed, so (V) cannot fire and (T) is on its own: `Shape` is total, descending structurally on `F`, and `inf` is an ordinary partial value of a data type. The whole-module gate runs post-zonk — after elaboration has already done the type-level reduction (T) exists to make safe. A type that reaches a partial definition and happens to be *productive* survives elaboration and the gate rejects it. One that is not productive spins until the step budget dies, and the program used to be refused for running out of a resource no amount of would have helped. Refusing the written type up front is what makes both shapes report the same thing, which is the thing that is actually wrong.
//
// `Shape(inf)` here reduces to itself with no progress; the productive sibling is `a_total_type_function_applied_to_a_partial_value_is_rejected`.
#[test]
fn a_non_productive_type_level_loop_is_diagnosed_not_exhausted() {
    rejected_as_a_type(&format!(
        "{SHAPE}\n let here(x : Shape(inf)) -> Nat = 0;\n\n 0"
    ));
}

// A struct *field* is the one written type position obligation (T)'s early net did not reach, and the three fixtures below are the two halves of that gap and its control.
//
// The net exists so a type reaching a partial definition is refused by name before elaboration forces it — the post-zonk gate runs after the reduction (T) exists to make safe, so by then a productive type has already spun. It is applied to a *definition's* written type, and every declaration position rides on some definition: an `induct` or `struct` parameter on the type former's own type, an `induct` payload on its constructor wrapper's, a `concept` method on the method wrapper's. A struct field rides on nothing — a field is projected, not constructed, so no wrapper's type mentions it — and so nothing looked at it until it had been elaborated.
//
// Neither half is a route to `False`; both fail closed, and that is why this is recorded as a coverage gap rather than as a forgery. What they broke is the claim `a_type_level_rec_through_an_arrow_is_diagnosed_not_aborted` makes for its own shape — that the compiler answers instead of dying — and the claim `a_non_productive_type_level_loop_is_diagnosed_not_exhausted` makes for its twin, that both shapes report the same thing. Both held for every spelling but this one.
//
// Verified while the hole was open, on the default main-thread stack with no `RUST_MIN_STACK`: the productive field aborted the compiler outright (`exit=134`, "has overflowed its stack"), and the non-productive field was refused with "reduction ran out of steps on: Shape(inf)", naming neither the definition at fault nor the reason. The same `Shape(inf)` written as an `induct` payload, an `induct` parameter, a `struct` parameter, or a `concept` method type was diagnosed by name in every case.
#[test]
fn a_productive_type_level_loop_in_a_struct_field_is_diagnosed_not_aborted() {
    rejected_as_a_type(&format!(
        "{PRODUCTIVE_SHAPE}\n struct Trap : pub Type {{ x : Shape(inf) }}\n\n ()"
    ));
}

#[test]
fn a_non_productive_type_level_loop_in_a_struct_field_is_diagnosed_too() {
    rejected_as_a_type(&format!(
        "{SHAPE}\n struct Trap : pub Type {{ x : Shape(inf) }}\n\n 0"
    ));
}

// The control. `Shape` applied to a *total* value is an ordinary type-level application and must keep working in a field, so a net that refused every struct field — or every field whose type mentions a `rec` — would fail here rather than pass quietly. It runs, so the field type really did reduce to `Nat`.
#[test]
fn a_struct_field_over_a_total_application_still_compiles() {
    let source = format!(
        "{SHAPE}\n struct Holder : pub Type {{ x : Shape(F/more(F/stop())) }}\n \
         let take(h : Holder) -> Nat = h.x;\n\n /std/print(Nat/to_str(take(Holder {{ x = 7 }})))"
    );
    assert_eq!(run(&source), b"7");
}

// The corpus shapes the design is keyed to, exercised through the replay path rather than the prelude build. `BigNat`'s arithmetic rests on `add/raw`, which descends on *either* of two `Bits` depending on the arm — a shape only match refinement can see — and `Fmt/print`'s result type is `format_type_with(parse(s))`, a type-level `rec` over a parsed format.
#[test]
fn the_prelude_shapes_a_user_program_leans_on_still_elaborate() {
    let source = r#"
        use /std/{BigNat, Fmt};

        Fmt/print("%-%")(BigNat/to_str(BigNat/of_nat(99999999) + BigNat/of_nat(1)))(3)
        "#;
    assert_eq!(run(source), b"100000000-3");
}

// The descent gate reads a declared type, and a declared type is a term like any other.
//
// A `rec` member that erasure deletes must descend, or assuming it at its own type certifies `rec f : False = f`. Which members those are was decided in two halves: whether the type is a *proposition*, asked semantically, and whether it *yields a sort*, read off the spelling. So `U` — an ordinary definition whose value is `Type` — reached neither. `Bad` below is productive and does not descend, and both checkers let it be assumed at a type that is `Type` in every sense but the syntactic one.
//
// Nothing was forged from it here, and that is the honest statement: the whole-module obligation still refuses the *use* of a partial type, so what the gate's omission costs is the local guarantee it exists to give — `check_group`'s own claim that a member erasure deletes has been shown to terminate before its body is checked against it. An alias is not a different claim, and reading the spelling made it one.
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

// The accessibility shape, which is the one well-founded recursion in a proof needs and the one the structural rung could not read: the recursive call descends through `below(m, lt)`, an application of a function-typed constructor payload, rather than through a syntactically smaller subterm. A function-typed payload is a branching node whose children are its applications, so the application grades below the constructor for the reason the payload does, and the group descends on `acc`. The result is `Prop`-sorted, so this is (V) reading the verdict, and it is the shape every well-founded fixpoint takes.
#[test]
fn a_call_through_a_constructor_payload_descends() {
    let source = r#"
        use /std/{Nat, True};

        induct Accessible(@A : Type, R : (A, A) -> Prop) : (A) -> Prop
        | intro(@x : A, below : (y : A, r : R(y, x)) -> Accessible(R, y)) : (x)
        end

        let strong(
            P : (Nat) -> Prop,
            step : (n : Nat, ih : (m : Nat, lt : Nat/Lt(m, n)) -> P(m)) -> P(n),
            n : Nat,
            acc : Accessible((a : Nat, b : Nat) => Nat/Lt(a, b), n),
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
