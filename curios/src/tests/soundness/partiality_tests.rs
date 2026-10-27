//! A partial definition's place: usable in a program and refused in a type, and the type-level loops diagnosed rather than aborted.

use {super::test_support::*, crate::tests::run, curios_utilities::fill};

// The route no analysis of *value*-level partiality can see. The exploit has no value-level `rec`, no `exit`, and no negative occurrence in any declaration: a lambda, a constructor application, and one application. The knot is tied entirely by `rec Bad : Type`, which `check_positivity` never looks at, because a `rec` is neither an `induct` nor a `struct`.
#[test]
fn a_type_level_rec_cannot_tie_the_negative_knot() {
    rejected(&format!(
        r#"
        {SINK}

        let Bad : Type = Sink(Bad);

        let delta(x : Bad) -> /std/Bool/False =
            match x
            | sink(g) => g(x)
            end;

        let boom : /std/Bool/False = delta(Sink/sink(delta));

        /std/print("unreachable")
        "#
    ));
}

#[test]
fn a_partial_type_behind_a_projection_is_still_a_type() {
    rejected(&fill!(
        r#"
        ${SINK}

        let P : {Type, /std/Nat} = (Sink(P.0), 0);

        let delta(x : P.0) -> /std/Bool/False =
            match x
            | sink(g) => g(x)
            end;

        let boom : /std/Bool/False = delta(Sink/sink(delta));

        /std/print("unreachable")
        "#,
        SINK
    ));
}

// Why (T) has to be the aggressive reading. Nothing here is a partial type former: `Shape` is structurally recursive and total, and `inf` is an ordinary partial *value* of an ordinary data type. The dangerous type exists only because a total function was applied to a bottom argument, so a rule that looks at what a type-level eliminator scrutinizes does not see it.
#[test]
fn a_total_type_function_applied_to_a_partial_value_is_rejected() {
    rejected(&fill!(
        r#"
        ${SINK}

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

        let delta(x : Shape(inf)) -> /std/Bool/False =
            match x
            | sink(g) => g(x)
            end;

        let boom : /std/Bool/False = delta(Sink/sink(delta));

        /std/print("unreachable")
        "#,
        SINK
    ));
}

// The entrypoint's trailing expression is not a top-level item — lowering leaves it in `Module::body`. An obligation seeded only from `items` sees nothing here. The only items below are `Sink` and `consume`, and neither is partial; the offending `rec` lives entirely inside the final term.
#[test]
fn the_entrypoint_expression_is_not_a_blind_spot() {
    rejected(&fill!(
        r#"
        ${SINK}

        let consume(u : {}) -> {} = u;

        consume(
            let forge(x : (let Bad : Type = Sink(Bad); Bad)) -> {} = ();
            ()
        )
        "#,
        SINK
    ));
}

// A shape that would abort the compiler rather than reject the program: reducing `Bad` rebuilds an arrow whose domain is `Bad`, so elaborating the first *use* would overflow the stack long before any whole-module gate could run. This is what the local form at `rec` elaboration exists for, and the only claim it makes is that the compiler answers instead of dying.
#[test]
fn a_type_level_rec_through_an_arrow_is_diagnosed_not_aborted() {
    rejected(
        r#"
        let Bad : Prop = (Bad) -> /std/Bool/False;

        let delta(x : Bad) -> /std/Bool/False = x(x);

        let boom : /std/Bool/False = delta(delta);

        /std/print("unreachable")
        "#,
    );
}

// The same shape as a *local* `rec`. `Item::Rec` and `Subterm::Rec` are separate elaboration paths, and each needs the local form.
#[test]
fn a_local_type_level_rec_through_an_arrow_is_diagnosed_too() {
    rejected(
        r#"
        let consume(u : {}) -> {} = u;

        consume(
            let forge(x : (let Bad : Prop = (Bad) -> /std/Bool/False; Bad)) -> {} = ();
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
// The argument form is the one that reaches this gate at all. Written as an annotation, `Shape(inf)` is *forced* while the binder elaborates and the step budget stops it there — fail-closed, but before any whole-module pass runs. Passed as a type argument it is never forced, so elaboration succeeds and only the gate is left, which the settle records are what reach.
//
// Nothing here is `Prop`-typed, so (V) cannot fire and (T) is on its own: `Shape` is total, descending structurally on `F`, and `inf` is an ordinary partial value of a data type. The whole-module gate runs post-zonk — after elaboration has already done the type-level reduction (T) exists to make safe. A type that reaches a partial definition and happens to be *productive* survives elaboration and the gate rejects it. One that is not productive would spin until the step budget dies and be refused for running out of a resource no amount of which would help. Refusing the written type up front is what makes both shapes report the same thing, which is the thing that is actually wrong.
//
// `Shape(inf)` here reduces to itself with no progress; the productive sibling is `a_total_type_function_applied_to_a_partial_value_is_rejected`.
#[test]
fn a_non_productive_type_level_loop_is_diagnosed_not_exhausted() {
    rejected_as_a_type(&format!(
        "{SHAPE}\n let here(x : Shape(inf)) -> Nat = 0;\n\n 0"
    ));
}

// A struct *field* is the written type position obligation (T)'s early net reaches on its own, and the three fixtures below are its two halves and its control.
//
// The net exists so a type reaching a partial definition is refused by name before elaboration forces it — the post-zonk gate runs after the reduction (T) exists to make safe, so by then a productive type has already spun. It is applied to a *definition's* written type, and every declaration position rides on some definition: an `induct` or `struct` parameter on the type former's own type, an `induct` payload on its constructor wrapper's, a `concept` method on the method wrapper's. A struct field rides on nothing — a field is projected, not constructed, so no wrapper's type mentions it — so the net reads a field's written type itself.
//
// Neither half is a route to `False`; both fail closed, which is why this is coverage rather than a forgery. What they hold at a field is the claim `a_type_level_rec_through_an_arrow_is_diagnosed_not_aborted` makes for its own shape — that the compiler answers instead of dying — and the claim `a_non_productive_type_level_loop_is_diagnosed_not_exhausted` makes for its twin, that both shapes report the same thing.
//
// Without the net, on the default main-thread stack, the productive field aborts the compiler outright with a stack overflow, and the non-productive one is refused with "reduction ran out of steps on: Shape(inf)", naming neither the definition at fault nor the reason — where the same `Shape(inf)` written as an `induct` payload, an `induct` parameter, a `struct` parameter, or a `concept` method type is diagnosed by name.
#[test]
fn a_productive_type_level_loop_in_a_struct_field_is_diagnosed_not_aborted() {
    rejected_as_a_type(&fill!(
        "${PRODUCTIVE_SHAPE}\n struct Trap : pub Type { x : Shape(inf) }\n\n ()",
        PRODUCTIVE_SHAPE
    ));
}

#[test]
fn a_non_productive_type_level_loop_in_a_struct_field_is_diagnosed_too() {
    rejected_as_a_type(&fill!(
        "${SHAPE}\n struct Trap : pub Type { x : Shape(inf) }\n\n 0",
        SHAPE
    ));
}

// The control. `Shape` applied to a *total* value is an ordinary type-level application and must keep working in a field, so a net that refused every struct field — or every field whose type mentions a `rec` — would fail here rather than pass quietly. It runs, so the field type really did reduce to `Nat`.
#[test]
fn a_struct_field_over_a_total_application_still_compiles() {
    let source = fill!(
        "${SHAPE}\n struct Holder : pub Type { x : Shape(F/more(F/stop())) }\n \
         let take(h : Holder) -> Nat = h.x;\n\n /std/print(Nat/to_str(take(Holder { x = 7 })))",
        SHAPE
    );
    assert_eq!(run(&source), b"7");
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

// The payload-child rule at a type-level recursion: `Bad` recurses on `g(0)`, an application of the constructor payload `g`, which the size order reads as a child of the `c(g)` it was projected from — so `Bad` is graded total. Its soundness rests entirely on (T): the only way `g(0)` can re-produce a `c(...)` that keeps `Bad` unfolding is for `g` to feed itself, which makes `g` non-descending, and a type reaching `g` is then refused. `loop` is exactly such a `g`, and the type of `held` reaches it; nothing in the totality grading refuses this — (T) does.
#[test]
fn a_payload_application_that_reaches_a_partial_function_is_a_partial_type() {
    rejected_as_a_type(
        r#"
        use /std/{Nat};

        induct F : pub Type
        | c(g : (Nat) -> F)
        end

        let Bad(x : F) -> Type =
            match x
            | c(g) => Bad(g(0))
            end;

        let loop : (Nat) -> F = (n) => F/c(loop);

        let held : Bad(F/c(loop)) = ();

        /std/print("unreachable")
        "#,
    );
}

// The (V)-side mirror of `a_payload_application_that_reaches_a_partial_function_is_a_partial_type`: here the payload-child rule grades a *proof*-returning group total, and (V) — not the grading — is what keeps it from proving `False`. `drop` recurses on `f(0)`, an application of the constructor payload `f`, which the size order reads as a child of the `node(f)` it was projected from, so `drop` descends and is accepted at `(Fam) -> False`. That acceptance is sound only because every total `Fam` is a finite tree: the one inhabitant that would make `drop` loop is an infinite one, which no total definition builds. `grow` is that inhabitant, and it is non-descending — its self-call sits under a payload lambda, graded `Unknown` — so the proof `drop(grow(0))` is a proof position reaching `grow`, and (V) refuses it there. A grading that refused `drop` would be rejecting the well-founded recursion `a_call_through_a_constructor_payload_descends` turns on; the obligation, not the grade, carries this.
#[test]
fn a_payload_descent_is_sound_only_because_a_partial_producer_is_refused_as_a_proof() {
    let report = crate::tests::error(
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

        /std/print(match drop(grow(0)) end)
        "#,
    );
    assert!(
        report.contains("is a proof position") && report.contains("reaches 'grow'"),
        "expected (V) to refuse the proof for reaching the partial producer:\n{report}",
    );
    assert!(
        !report.contains("reaches 'drop'"),
        "the payload-descending group must be accepted, not blamed:\n{report}",
    );
}

// (T) across the unit boundary: a partial definition in one unit is partial to every unit that mounts it. Every (T) fixture above builds its partiality in-unit, so none exercises the replay path's load-bearing half — that a definition the prelude's own walk classified `Partial` is read back as partial by a user suffix that only mentions it. `Json/decode` is that definition: a parser no size-change checker can show terminates, whose `Partial` verdict lives in the archived record alone. `Bad(s)` is a type that computes through that parse, so its body is a type position reaching `Json/decode`; a gate that read the archive's verdict as `Total`, or did not consult it, would form `Bad` over a diverging type and satisfy (T) by a classification it never read.
#[test]
fn a_type_reaching_a_partial_prelude_definition_is_rejected() {
    rejected_as_a_type(
        r#"
        use /std/{Nat, Json, Str};

        let Bad(s : Str) -> Type =
            match Json/decode(s)
            | success(_) => Nat
            | failure(_) => {}
            end;

        let _held(s : Str, _x : Bad(s)) -> Nat = 0;

        /std/print("unreachable")
        "#,
    );
}
