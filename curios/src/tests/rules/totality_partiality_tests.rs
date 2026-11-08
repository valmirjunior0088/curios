//! A partial definition's place: usable in a program and refused in a type, and the type-level loops diagnosed rather than aborted.

use {
    super::test_support::*, crate::test_support::*, curios_analysis::Erased, curios_core::Quality,
};

// The route no analysis of *value*-level partiality can see. The exploit has no value-level `rec`, no `exit`, and no negative occurrence in any declaration: a lambda, a constructor application, and one application. The knot is tied entirely by `rec Bad : Type`, which `check_positivity` never looks at, because a `rec` is neither an `induct` nor a `struct`. The control's alias is `Sink({})`, which ties nothing, and `delta` refutes a unit with it.
#[test]
fn a_type_level_rec_cannot_tie_the_negative_knot() {
    control(
        r#"
        induct Sink(A : Type) : pub Type
        | sink(f : (A) -> /std/Bool/False)
        end

        let Bad : Type = Sink({});

        let delta(x : Bad) -> /std/Bool/False =
            match x
            | sink(g) => g(())
            end;

        let boom = delta;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("Sink({});", "Sink(Bad);"),
        ("g(())", "g(x)"),
        (
            "let boom = delta;",
            "let boom : /std/Bool/False = delta(Sink/sink(delta));",
        ),
    ])
    .by_elaborator_in("/Bad", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    });
}

// The control's projection is `Sink({})`, which ties nothing.
#[test]
fn a_partial_type_behind_a_projection_is_still_a_type() {
    control(
        r#"
        induct Sink(A : Type) : pub Type
        | sink(f : (A) -> /std/Bool/False)
        end

        let P : {Type, /std/Nat} = (Sink({}), 0);

        let delta(x : P.0) -> /std/Bool/False =
            match x
            | sink(g) => g(())
            end;

        let boom = delta;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("(Sink({}), 0)", "(Sink(P.0), 0)"),
        ("g(())", "g(x)"),
        (
            "let boom = delta;",
            "let boom : /std/Bool/False = delta(Sink/sink(delta));",
        ),
    ])
    .by_elaborator_in("/P", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    });
}

// Why (T) has to be the aggressive reading. Nothing here is a partial type former: `Shape` is structurally recursive and total, and `inf` is an ordinary partial *value* of an ordinary data type. The dangerous type exists only because a total function was applied to a bottom argument, so a rule that looks at what a type-level eliminator scrutinizes does not see it. The control applies `Shape` to a total value.
#[test]
fn a_total_type_function_applied_to_a_partial_value_is_rejected() {
    control(
        r#"
        induct Sink(A : Type) : pub Type
        | sink(f : (A) -> /std/Bool/False)
        end

        induct F : pub Type
        | stop()
        | more(rest : F)
        end

        let Shape(f : F) -> Type =
            match f
            | stop() => {}
            | more(rest) => Sink(Shape(rest))
            end;

        let inf : F = F/more(inf);

        let delta(x : Shape(F/more(F/stop()))) -> /std/Bool/False =
            match x
            | sink(g) => g(())
            end;

        let boom = delta;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("Shape(F/more(F/stop()))", "Shape(inf)"),
        ("g(())", "g(x)"),
        (
            "let boom = delta;",
            "let boom : /std/Bool/False = delta(Sink/sink(delta));",
        ),
    ])
    .by_elaborator_in("/delta", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    });
}

// The entrypoint's trailing expression is not a top-level item — lowering leaves it in `Module::body`. An obligation seeded only from `items` sees nothing here. The only items below are `Sink` and `consume`, and neither is partial; the offending `rec` lives entirely inside the final term. The control's local alias is `Sink({})`, which ties nothing.
#[test]
fn the_entrypoint_expression_is_not_a_blind_spot() {
    control(
        r#"
        induct Sink(A : Type) : pub Type
        | sink(f : (A) -> /std/Bool/False)
        end

        /std/print(
            let forge(x : (let Bad : Type = Sink({}); Bad)) -> {} = ();
            "ok"
        )
        "#,
        b"ok",
    )
    .refused(&[("Sink({}); Bad", "Sink(Bad); Bad")])
    .by_elaborator_in("entry", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    });
}

// A shape that would abort the compiler rather than reject the program: reducing `Bad` rebuilds an arrow whose domain is `Bad`, so elaborating the first *use* would overflow the stack long before any whole-module gate could run. This is what the local form at `rec` elaboration exists for, and the only claim it makes is that the compiler answers instead of dying. The control's arrow is out of the unit.
#[test]
fn a_type_level_rec_through_an_arrow_is_diagnosed_not_aborted() {
    control(
        r#"
        let Bad : Prop = ({}) -> /std/Bool/False;

        let delta(x : Bad) -> /std/Bool/False = x(());

        let boom = delta;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("({}) -> /std/Bool/False;", "(Bad) -> /std/Bool/False;"),
        ("x(());", "x(x);"),
        (
            "let boom = delta;",
            "let boom : /std/Bool/False = delta(delta);",
        ),
    ])
    .by_elaborator_in("/Bad", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    });
}

// The same shape as a *local* `rec`. `Item::Rec` and `Subterm::Rec` are separate elaboration paths, and each needs the local form. The control's arrow is out of the unit.
#[test]
fn a_local_type_level_rec_through_an_arrow_is_diagnosed_too() {
    control(
        r#"
        /std/print(
            let forge(x : (let Bad : Prop = ({}) -> /std/Bool/False; Bad)) -> {} = ();
            "ok"
        )
        "#,
        b"ok",
    )
    .refused(&[(
        "({}) -> /std/Bool/False; Bad",
        "(Bad) -> /std/Bool/False; Bad",
    )])
    .by_elaborator_in("entry", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    });
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
// The argument form is the one that reaches this gate at all. Written as an annotation, `Shape(inf)` is *forced* while the binder elaborates and the step budget stops it there — fail-closed, but before any whole-module pass runs. Passed as a type argument it is never forced, so elaboration succeeds and only the gate is left, which the settle records are what reach.
//
// Nothing here is `Prop`-typed, so (V) cannot fire and (T) is on its own: `Shape` is total, descending structurally on `F`, and `inf` is an ordinary partial value of a data type. The whole-module gate runs post-zonk — after elaboration has already done the type-level reduction (T) exists to make safe. A type that reaches a partial definition and happens to be *productive* survives elaboration and the gate rejects it. One that is not productive would spin until the step budget dies and be refused for running out of a resource no amount of which would help. Refusing the written type up front is what makes both shapes report the same thing, which is the thing that is actually wrong.
//
// `Shape(inf)` here reduces to itself with no progress; the productive sibling is `a_total_type_function_applied_to_a_partial_value_is_rejected`. The control applies `Shape` to a total value.
#[test]
fn a_non_productive_type_level_loop_is_diagnosed_not_exhausted() {
    control(
        r#"
        use /std/{Nat};

        induct F : pub Type
        | stop()
        | more(rest : F)
        end

        let Shape(f : F) -> Type =
            match f
            | stop() => Nat
            | more(rest) => Shape(rest)
            end;

        let inf : F = F/more(inf);

        let here(x : Shape(F/stop())) -> Nat = 0;

        /std/print(Nat/to_str(here(7)))
        "#,
        b"0",
    )
    .refused(&[("here(x : Shape(F/stop()))", "here(x : Shape(inf))")])
    .by_elaborator_in("/here", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Type,
                ..
            }
        )
    });
}

// A struct *field* is the written type position obligation (T)'s early net reaches on its own, and the two fixtures below are its two halves, each derived from a control at a total application.
//
// The net exists so a type reaching a partial definition is refused by name before elaboration forces it — the post-zonk gate runs after the reduction (T) exists to make safe, so by then a productive type has already spun. It is applied to a *definition's* written type, and every declaration position rides on some definition: an `induct` or `struct` parameter on the type former's own type, an `induct` payload on its constructor wrapper's, a `concept` method on the method wrapper's. A struct field rides on nothing — a field is projected, not constructed, so no wrapper's type mentions it — so the net reads a field's written type itself.
//
// Neither half is a route to `False`; both fail closed, which is why this is coverage rather than a forgery. What they hold at a field is the claim `a_type_level_rec_through_an_arrow_is_diagnosed_not_aborted` makes for its own shape — that the compiler answers instead of dying — and the claim `a_non_productive_type_level_loop_is_diagnosed_not_exhausted` makes for its twin, that both shapes report the same thing.
//
// Without the net, on the default main-thread stack, the productive field aborts the compiler outright with a stack overflow, and the non-productive one is refused with "reduction ran out of steps on: Shape(inf)", naming neither the definition at fault nor the reason — where the same `Shape(inf)` written as an `induct` payload, an `induct` parameter, a `struct` parameter, or a `concept` method type is diagnosed by name. The control applies `Shape` to a total value.
#[test]
fn a_productive_type_level_loop_in_a_struct_field_is_diagnosed_not_aborted() {
    control(
        r#"
        induct Sink(A : Type) : pub Type
        | sink(f : (A) -> /std/Bool/False)
        end

        induct F : pub Type
        | stop()
        | more(rest : F)
        end

        let Shape(f : F) -> Type =
            match f
            | stop() => {}
            | more(rest) => Sink(Shape(rest))
            end;

        let inf : F = F/more(inf);

        struct Trap : pub Type { x : Shape(F/stop()) }

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("{ x : Shape(F/stop()) }", "{ x : Shape(inf) }")])
    .by_elaborator_in("/Trap", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Type,
                ..
            }
        )
    });
}

// The control applies `Shape` to a total value, which must keep working in a field: a net that refused every struct field, or every field whose type mentions a `rec`, would fail there rather than pass quietly, and it runs, so the field type really did reduce to `Nat`.
#[test]
fn a_non_productive_type_level_loop_in_a_struct_field_is_diagnosed_too() {
    control(
        r#"
        use /std/{Nat};

        induct F : pub Type
        | stop()
        | more(rest : F)
        end

        let Shape(f : F) -> Type =
            match f
            | stop() => Nat
            | more(rest) => Shape(rest)
            end;

        let inf : F = F/more(inf);

        struct Holder : pub Type { x : Shape(F/more(F/stop())) }

        let take(h : Holder) -> Nat = h.x;

        /std/print(Nat/to_str(take(Holder { x = 7 })))
        "#,
        b"7",
    )
    .refused(&[("{ x : Shape(F/more(F/stop())) }", "{ x : Shape(inf) }")])
    .by_elaborator_in("/Holder", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Type,
                ..
            }
        )
    });
}

// A prelude shape the design is keyed to, exercised through the replay path rather than the prelude build: `Fmt/print`'s result type is `format_type_with(parse(s))`, a type-level `rec` over a parsed format. The other, recursion that descends on *either* of two lists depending on the arm — a shape only match refinement can see — is `/std/List/sort_by`'s `merge`, which the prelude's own build exercises.
#[test]
fn the_prelude_shapes_a_user_program_leans_on_still_elaborate() {
    let source = r#"
        use /std/{Nat, Fmt};

        Fmt/print("%-%")(Nat/to_str(99999999 + 1))(3)
        "#;
    assert_eq!(run(source), b"100000000-3");
}

// The payload-child rule at a type-level recursion: `Bad` recurses on `g(0)`, an application of the constructor payload `g`, which the size order reads as a child of the `c(g)` it was projected from — so `Bad` is graded total. Its soundness rests entirely on (T): the only way `g(0)` can re-produce a `c(...)` that keeps `Bad` unfolding is for `g` to feed itself, which makes `g` non-descending, and a type reaching `g` is then refused. `loop` is exactly such a `g`, and the type of `held` reaches it; nothing in the totality grading refuses this — (T) does. `F` has a base case so that a total function can stand where `loop` does, which is the control.
#[test]
fn a_payload_application_that_reaches_a_partial_function_is_a_partial_type() {
    control(
        r#"
        use /std/{Nat};

        induct F : pub Type
        | c(g : (Nat) -> F)
        | z()
        end

        let Bad(x : F) -> Type =
            match x
            | c(g) => Bad(g(0))
            | z() => {}
            end;

        let loop : (Nat) -> F = (n) => F/c(loop);

        let held : Bad(F/c((n) => F/z())) = ();

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Bad(F/c((n) => F/z()))", "Bad(F/c(loop))")])
    .by_elaborator_in("/held", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Type,
                ..
            }
        )
    });
}

// The (V)-side mirror of `a_payload_application_that_reaches_a_partial_function_is_a_partial_type`: here the payload-child rule grades a *proof*-returning group total, and (V) — not the grading — is what keeps it from proving `False`. `drop` recurses on `f(0)`, an application of the constructor payload `f`, which the size order reads as a child of the `node(f)` it was projected from, so `drop` descends and is accepted at `(Fam) -> False`. That acceptance is sound only because every total `Fam` is a finite tree: the one inhabitant that would make `drop` loop is an infinite one, which no total definition builds. `grow` is that inhabitant, and it is non-descending — its self-call sits under a payload lambda, graded `Unknown` — so the proof `drop(grow(0))` is a proof position reaching `grow`, and (V) refuses it there. A grading that refused `drop` would be rejecting the well-founded recursion `a_call_through_a_constructor_payload_descends` turns on; the obligation, not the grade, carries this. The control declares the same three and applies none.
#[test]
fn a_payload_descent_is_sound_only_because_a_partial_producer_is_refused_as_a_proof() {
    let refusals = control(
        r#"
        use /std/{Nat};
        use /std/Bool/{False};

        induct Fam : pub Type
        | node(f : (Nat) -> Fam)
        end

        let drop(x : Fam) -> False =
            match x
            | node(f) => drop(f(0))
            end;

        let grow(n : Nat) -> Fam = Fam/node((m) => grow(m));

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("/std/print(\"ok\")", "/std/print(match drop(grow(0)) end)")]);
    refusals.by_elaborator_in("entry", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { erased: Erased::Proof, offender: Some(offender), .. } if *offender == global("/grow"))
    })
        .by_kernel_in("entry", |error| matches!(error, curios_cert::Error::NotTotal { erased: Erased::Proof, .. }));
    // The payload-descending group is accepted, not blamed.
    refusals.not_by_elaborator(|error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { offender: Some(offender), .. } if *offender == global("/drop"))
    });
}

// (T) across the unit boundary: a partial definition in one unit is partial to every unit that mounts it. Every (T) fixture above builds its partiality in-unit, so none exercises the replay path's load-bearing half — that a definition the prelude's own walk classified `Partial` is read back as partial by a user suffix that only mentions it. `Json/decode` is that definition: a parser no size-change checker can show terminates, whose `Partial` verdict lives in the archived record alone. `Bad(s)` is a type that computes through that parse, so its body is a type position reaching `Json/decode`; a gate that read the archive's verdict as `Total`, or did not consult it, would form `Bad` over a diverging type and satisfy (T) by a classification it never read. The control computes through `Option/to_result`, which the prelude's walk found total.
#[test]
fn a_type_reaching_a_partial_prelude_definition_is_rejected() {
    control(
        r#"
        use /std/{Nat, Json, Str, Option};

        let Bad(s : Str) -> Type =
            match Option/to_result(Option/some(s), 0)
            | success(_) => Nat
            | failure(_) => {}
            end;

        let _held(s : Str, _x : Bad(s)) -> Nat = 0;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Option/to_result(Option/some(s), 0)", "Json/decode(s)")])
    .by_elaborator_in("/_held", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Type,
                ..
            }
        )
    });
}

// The positions a program writes a proof or a type at, each reaching a definition not known to terminate: a `let` at a proposition, a structure's field, a case's payload, an implicit argument, a witness's method and the type a definition states. `curios_cert`'s `recheck::proposition_tests` puts the same positions to the kernel under this name, with the ones only Core text writes. The control states each position at `True` through `Claim`, its definitions answering at once.
#[test]
fn a_proof_or_type_that_may_not_terminate_is_refused_wherever_it_stands() {
    let refusals = control(
        r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, False, Holds};

        let Claim : Prop = True;

        struct Box: Type { p: Claim }
        induct Wit: Type | w(p: Claim) end
        concept Lawful(A: Type): Type { op(A) -> A, law(x: A) -> Claim }

        let spin(u: {}) -> Box = Box { p = True/qed() };
        let decide(u: {}) -> Bool = true;
        let take(@_: Claim) -> Nat = 0;

        pub let in_a_let: Nat = let _: Claim = spin(()).p; 0;
        pub let in_a_field: Box = Box { p = spin(()).p };
        pub let in_a_payload: Wit = Wit/w(spin(()).p);
        pub let in_an_implicit: Nat = take(@spin(()).p);
        satisfy Lawful(Nat) { op(x) = x, law(_) = spin(()).p }
        pub let in_a_signature(_: Holds(decide(()))) -> Nat = 0;
        pub let in_a_groups_tail: Nat = let h(u: {}) -> Box = Box { p = True/qed() }; let _: Claim = h(()).p; 0;
        pub let in_a_type_in_a_groups_tail: Nat = let d(u: {}) -> Bool = true; let f(_: Holds(d(()))) -> Nat = 0; 0;
        pub let in_a_lets_tail: Nat = let g: ({}) -> Box = (let h(u: {}) -> Box = Box { p = True/qed() }; h); let _: Claim = g(()).p; 0;

        /std/print(Nat/to_str(in_a_let + in_an_implicit + in_a_signature(True/qed())))
        "#,
        b"0",
    )
    .refused(&[
        ("let Claim : Prop = True;", "let Claim : Prop = False;"),
        ("let spin(u: {}) -> Box = Box { p = True/qed() };", "let spin(u: {}) -> Box = spin(u);"),
        ("let decide(u: {}) -> Bool = true;", "let decide(u: {}) -> Bool = decide(u);"),
        ("let h(u: {}) -> Box = Box { p = True/qed() }; let _", "let h(u: {}) -> Box = h(u); let _"),
        ("let d(u: {}) -> Bool = true;", "let d(u: {}) -> Bool = d(u);"),
        ("(let h(u: {}) -> Box = Box { p = True/qed() }; h)", "(let h(u: {}) -> Box = h(u); h)"),
    ]);

    for at in [
        "/in_a_let",
        "/in_a_field",
        "/in_a_payload",
        "/in_an_implicit",
        "/witness~/Lawful(Nat)",
    ] {
        refusals.by_elaborator_in(at, |error| {
            matches!(
                error,
                curios_elab::Error::PartialInErasedPosition {
                    erased: Erased::Proof,
                    offender: Some(offender),
                    ..
                } if *offender == global("/spin")
            )
        });
    }
    refusals.by_elaborator_in("/in_a_signature", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Type,
                offender: Some(offender),
                ..
            } if *offender == global("/decide")
        )
    });
    // A member named in its group's tail, or a binding whose value holds a group named in the binding's tail, stands there by its label, which is read as what it stands for, so the position holds the group.
    for (at, erased) in [
        ("/in_a_groups_tail", Erased::Proof),
        ("/in_a_type_in_a_groups_tail", Erased::Type),
        ("/in_a_lets_tail", Erased::Proof),
    ] {
        refusals.by_elaborator_in(at, |error| {
            matches!(
                error,
                curios_elab::Error::PartialInErasedPosition {
                    erased: refused,
                    offender: None,
                    ..
                } if *refused == erased
            )
        });
    }
}

// A group standing in a term of a declaration that leaves a sort binds none of its own: `loop` answers `A`, a proof where the caller states `Prop`, and it does not descend. The elaborator owes a value at a sort left to a caller the termination a proof owes; the kernel reads the group at each assignment with `spin`, and at `Prop` it is a proof's recursion. `curios-cert`'s `recheck::assignment_tests` puts the same declaration to the kernel under this name. Rename both or neither. The control declares `spin` at `Type`, where a value's recursion owes nothing.
#[test]
fn a_group_inside_a_term_is_judged_at_every_assignment_of_its_declarations_sorts() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{False};

        let spin(@A: Type, n: Nat) -> A =
            let loop(k: Nat) -> A = loop(k);
            loop(n);

        let boom = (n: Nat) => spin(@Nat, n);

        /std/print("")
        "#,
        b"",
    )
    .refused(&[
        ("@A: Type", "@A: Sort"),
        (
            "let boom = (n: Nat) => spin(@Nat, n);",
            "let boom: False = spin(0);",
        ),
    ])
    .by_elaborator_in("/spin", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_kernel_in("/spin", |error| {
        matches!(
            error,
            curios_cert::Error::AtSorts { sorts, refusal }
                if sorts[..] == [Quality::Prop]
                    && matches!(**refusal, curios_cert::Error::NotDescending { .. })
        )
    });
}

// A closed match whose one arm reads nothing is that arm with its scrutinee unread, and where the indices rule the arm out the kernel types none of it: so a call that never returns, at an instance no case builds, may be read only where nothing is erased, and the type and the proof that would read it are refused, while the elaborator types the arm a reader wrote. `curios_cert`'s `recheck::closed_computation_tests` puts the module to the kernel under this name. The control's call returns the case the arm names.
#[test]
fn a_ruled_out_arm_a_closed_match_takes_is_reached_only_through_a_partial_call() {
    let refusals = control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};

        induct F: (n: Nat) -> Type
        | c(): (0)
        end

        pub let loop(u: {}) -> F(0) = F/c();

        pub let T: Prop = match loop(()) | c() => True end;
        pub let typed: T = True/qed();
        pub let forged: True = match loop(()) | c() => True/qed() end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("use /std/Bool/{True};", "use /std/Bool/{True, False};"),
        ("-> F(0) = F/c();", "-> F(1) = loop(u);"),
        ("pub let forged: True", "pub let forged: False"),
    ]);

    for at in ["/T", "/typed"] {
        refusals.by_elaborator_in(at, |error| {
            matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
        });
    }
    refusals.by_elaborator_in("/forged", |error| {
        matches!(error, curios_elab::Error::TypeMismatch { .. })
    });
}
