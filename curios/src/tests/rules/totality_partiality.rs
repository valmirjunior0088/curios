//! A partial definition's place: usable in a program, kept folded where a checker reads a type, and refused in a proof; a type's own loops diagnosed rather than aborted.

use {
    super::test_support::*, crate::test_support::*, curios_analysis::Erased, curios_common::fill,
    curios_core::Quality,
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

// A term is a type where its type reduces to a sort, written as one or not: `_T`'s recursive call has type `_U(j + 1)`, which is no sort as written and is `Type` once `_U`'s match on a successor reduces, so the call is a type position, and one that does not descend is refused by the elaborator as the certifier refuses it. The control recurses on `j` under an arm that rules zero out, which descends.
#[test]
fn a_term_whose_type_reduces_to_a_sort_is_a_type_position() {
    control(
        r#"
        let _U(k: /std/Nat) -> Type = match k | 0 => Type | _ + 1 => Type end;

        let _T(k: /std/Nat) -> _U(k) =
            match k
            | 0 => /std/Nat
            | j + 1 => match j | 0 => /std/Nat | _ + 1 => _T(j) end
            end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("_T(j)", "_T(j + 1)")])
    .by_elaborator_in("/_T", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Type,
                ..
            }
        )
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

// A total type-level function applied to a partial value ties no knot. Nothing here is a partial type former: `Shape` is structurally recursive and total, and `inf` an ordinary partial *value* of an ordinary data type, `Unfounded`, which no checker unfolds. So `Shape(inf)` is a stuck application and never `Sink(Shape(inf))`, and the match `delta` would take apart finds no family to take it apart as. The control applies `Shape` to a total value.
#[test]
fn a_total_type_function_applied_to_a_partial_value_ties_no_knot() {
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
        matches!(error, curios_elab::Error::NotAInductType { .. })
    })
    .saying("`inf` is kept folded");
}

// The entrypoint's trailing expression is not a top-level item — lowering leaves it in `Module::body`. An obligation seeded only from `items` sees nothing here. The only items below are `Sink` and `consume`, and neither is partial; the offending `rec` lives entirely inside the final term. The control's local alias is `Sink({})`, which ties nothing.
#[test]
fn the_entrypoint_expression_is_not_a_blind_spot() {
    control(
        r#"
        induct Sink(A : Type) : pub Type
        | sink(f : (A) -> /std/Bool/False)
        end

        /std/print((() =>
            let forge(x : (() => let Bad : Type = Sink({}); Bad)()) -> {} = ();
            "ok")()
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
        /std/print((() =>
            let forge(x : (() => let Bad : Prop = ({}) -> /std/Bool/False; Bad)()) -> {} = ();
            "ok")()
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
            match
            | n <= 1 => steps
            | n % 2 == 0 => collatz(n / 2, steps + 1)
            | _ => collatz(3 * n + 1, steps + 1)
            end;

        /std/print(Nat/to_str(collatz(6, 0)))
        "#;
    assert_eq!(run(source), b"8");
}

// A type-level loop that is not productive stays folded and spends nothing. `Shape` is total, descending structurally on `F`, and `inf` an `Unfounded` value no checker unfolds, so `Shape(inf)` is a stuck application at once: `here`'s signature forms, and the call that hands it a `Nat` is refused as a mismatch naming `inf`, where an unfolding `inf` would spin until the budget was spent. Type totality is seeded two ways, written type positions and the terms elaboration settled at a sort, and neither has anything to say of it: the declaration writes no recursion of its own. The control applies `Shape` to a total value.
#[test]
fn a_non_productive_type_level_loop_stays_folded_not_exhausted() {
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
    .by_elaborator_in("entry", |error| {
        matches!(error, curios_elab::Error::TypeMismatch { .. })
    })
    .saying("`inf` is kept folded");
}

// A struct *field* is a written type position elaboration forces, and a type-level loop written there is no fault, the two fixtures below being its two halves. A productive field forms a structure nothing builds a value of, and the program runs, `Shape(inf)` with `inf` withheld being a stuck application; it runs with `inf` unfolding too, so the fold is not what this half rests on.
#[test]
fn a_productive_type_level_loop_in_a_struct_field_stays_folded() {
    let source = r#"
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

        struct Trap : pub Type { x : Shape(inf) }

        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// The non-productive half: a field at `Shape(inf)` forms, and the projection that reads it where a `Nat` is wanted is refused as a mismatch naming `inf`, which an unfolding `inf` would have spent the budget on instead. The control applies `Shape` to a total value, which must keep working in a field: it runs, so the field type really did reduce to `Nat`.
#[test]
fn a_non_productive_type_level_loop_in_a_struct_field_stays_folded_too() {
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
    .by_elaborator_in("/take", |error| {
        matches!(error, curios_elab::Error::TypeMismatch { .. })
    })
    .saying("`inf` is kept folded");
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

// The payload-child rule at a type-level recursion: `Bad` recurses on `g(0)`, an application of the constructor payload `g`, which the size order reads as a child of the `c(g)` it was projected from — so `Bad` is graded total. Its soundness rests on the fold: the only way `g(0)` can re-produce a `c(...)` that keeps `Bad` unfolding is for `g` to feed itself, which makes `g` a recursion that does not descend, and `loop` is exactly such a `g`, withheld from both checkers. So `Bad(F/c(loop))` unfolds once to `Bad(loop(0))` and stands, and `()` is no value of it. `F` has a base case so that a total function can stand where `loop` does, which is the control.
#[test]
fn a_payload_application_that_reaches_a_partial_function_stays_folded() {
    let held = control(
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
    );
    held.refused(&[("Bad(F/c((n) => F/z()))", "Bad(F/c(loop))")])
        .by_elaborator_in("/held", |error| {
            matches!(error, curios_elab::Error::NotATupleType { .. })
        })
        .saying("`loop` is kept folded");
    held.refused(&[("Bad(F/c((n) => F/z())) = ()", "Bad(F/c(loop)) = (n) => n")])
        .by_elaborator_in("/held", |error| {
            matches!(error, curios_elab::Error::NotAFunctionType { .. })
        })
        .saying("`loop` is kept folded");
}

// The proof-side mirror of `a_payload_application_that_reaches_a_partial_function_is_a_partial_type`: here the payload-child rule grades a *proof*-returning group total, and proof totality — not the grading — is what keeps it from proving `False`. `drop` recurses on `f(0)`, an application of the constructor payload `f`, which the size order reads as a child of the `node(f)` it was projected from, so `drop` descends and is accepted at `(Fam) -> False`. That acceptance is sound only because every total `Fam` is a finite tree: the one inhabitant that would make `drop` loop is an infinite one, which no total definition builds. `grow` is that inhabitant, and it is non-descending — its self-call sits under a payload lambda, graded `Unknown` — so the proof `drop(grow(0))` is a proof position reaching `grow`, and proof totality refuses it there. A grading that refused `drop` would be rejecting the well-founded recursion `a_call_through_a_constructor_payload_descends` turns on; the obligation, not the grade, carries this. The control declares the same three and applies none.
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
        .by_certifier_in("entry", |error| matches!(error, curios_cert::Error::NotTotal { erased: Erased::Proof, .. }));
    // The payload-descending group is accepted, not blamed.
    refusals.not_by_elaborator(|error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { offender: Some(offender), .. } if *offender == global("/drop"))
    });
}

// The fold across the unit boundary: a definition the prelude's walk filed `Unfounded` is withheld in every unit that mounts it, read off the archived record alone. `Json/decode` reaches such a parser, `/std/Json/decode/value`, so `Bad("1")`, which would be `Nat` were the parser unfolded, since `"1"` parses, stands as written, and `0` is no value of it; a unit that read the archive's parser by its body would accept the call. The refusal names the parser through `Bad`, the name its side stands on as written. The control computes through `Option/to_result`, which the prelude's walk found total.
#[test]
fn a_partial_prelude_definition_stays_folded_in_a_user_unit() {
    control(
        r#"
        use /std/{Nat, Json, Str, Option};

        let Bad(s : Str) -> Type =
            match Option/to_result(Option/some(s), 0)
            | success(_) => Nat
            | failure(_) => {}
            end;

        let held(s : Str, x : Bad(s)) -> Nat = 0;

        /std/print(Nat/to_str(held("1", 0)))
        "#,
        b"0",
    )
    .refused(&[("Option/to_result(Option/some(s), 0)", "Json/decode(s)")])
    .by_elaborator_in("entry", |error| {
        matches!(error, curios_elab::Error::TypeMismatch { .. })
    })
    .saying("`Bad` reaches `/std/Json/decode/value`, which is kept folded");
}

// The positions a program writes a proof or a type at, each reaching a definition not known to terminate: a `let` at a proposition, a structure's field, a case's payload, an implicit argument, a witness's method and the type a definition states. `curios_cert`'s `recheck::proposition_tests` puts the same positions to the certifier under this name, with the ones only Core text writes. The control states each position at `True` through `Claim`, its definitions answering at once.
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
        pub let in_a_lets_tail: Nat = let g: ({}) -> Box = (() => let h(u: {}) -> Box = Box { p = True/qed() }; h)(); let _: Claim = g(()).p; 0;

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
        ("(() => let h(u: {}) -> Box = Box { p = True/qed() }; h)()", "(() => let h(u: {}) -> Box = h(u); h)()"),
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
    // A signature naming a loop it never applies forms, `decide` withheld, and the call that hands it `qed` is refused, `Holds(decide(()))` standing as written.
    refusals
        .by_elaborator_in("entry", |error| {
            matches!(error, curios_elab::Error::TypeMismatch { .. })
        })
        .saying("`decide` is kept folded");
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

// A group standing in a term of a declaration that leaves a sort binds none of its own: `loop` answers `A`, a proof where the caller states `Prop`, and it does not descend. The elaborator owes a value at a sort left to a caller the termination a proof owes; the certifier reads the group at each assignment with `spin`, and at `Prop` it is a proof's recursion. `curios-cert`'s `recheck::assignment_tests` puts the same declaration to the certifier under this name. Rename both or neither. The control declares `spin` at `Type`, where a value's recursion owes nothing.
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
    .by_certifier_in("/spin", |error| {
        matches!(
            error,
            curios_cert::Error::AtSorts { sorts, refusal }
                if sorts[..] == [Quality::Prop]
                    && matches!(**refusal, curios_cert::Error::NotDescending { .. })
        )
    });
}

// A match on data is taken by its scrutinee's value, a match reading nothing of its scrutinee being one that states the proposition it eliminates, so an arm its indices rule out, which the certifier types none of, is reached by no reduction over a call that never returns: the type and the proof that would read it are refused, while the elaborator types the arm a reader wrote. `curios_cert`'s `recheck::closed_computation_tests` puts the module to the certifier under this name. The control's call returns the case the arm names.
#[test]
fn a_ruled_out_arm_of_a_closed_match_on_data_is_reached_by_no_reduction() {
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

    // `T` forms, a type naming a loop its own declaration does not write; `typed` and `forged` are refused, neither type reducing past `loop`.
    for at in ["/typed", "/forged"] {
        refusals.by_elaborator_in(at, |error| {
            matches!(error, curios_elab::Error::TypeMismatch { .. })
        });
    }
    refusals.not_by_elaborator(|error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    });
}

// A family's index type naming a loop forms, the loop withheld: `F`'s index is typed `Eq()(spin(0), 0)` in the refusal, which stands as written, `spin` never descending, so `f`'s target `Eq/refl()` is no proof of it and is refused as a mismatch naming `spin`. The control types the index at `fine(0)`, which answers.
#[test]
fn a_family_whose_index_type_names_a_loop_stays_folded() {
    control(
        r#"
        use /std/{Nat, Eq};

        let fine(n: Nat) -> Nat = n;
        let spin(n: Nat) -> Nat = spin(n);

        induct F: (p: Eq()(fine(0), 0)) -> Type
        | f(): (Eq/refl())
        end

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Eq()(fine(0), 0)", "Eq()(spin(0), 0)")])
    .by_elaborator_in("/F", |error| {
        matches!(error, curios_elab::Error::TypeMismatch { .. })
    })
    .saying("`spin` is kept folded");
}

// A proof standing as a match's scrutinee and naming a loop is refused by proof totality, by the loop's name, in both checkers: `spin` is `Unfounded`, so it is defined with no value, and the index unification the match asks of `Eq()(spin(0), spin(0))` reads `spin(0)` as it stands. Mutation-checked: with `spin` defined by its value, the elaborator unfolds `spin(0)` until the budget is spent and refuses the program for that. The control's lemma stands at a value that terminates.
#[test]
fn a_proof_scrutinee_naming_a_loop_is_refused_by_name_and_not_by_the_budget() {
    control(
        r#"
        use /std/{Nat, Eq};

        let fine(n: Nat) -> Nat = n;
        let spin(n: Nat) -> Nat = spin(n);

        let lemma(@n: Nat) -> Eq()(n, n) = Eq/refl();

        let v() -> Nat = match lemma(@fine(0)) | refl() => 0 end;

        /std/print(Nat/to_str(v()))
        "#,
        b"0",
    )
    .refused(&[("lemma(@fine(0))", "lemma(@spin(0))")])
    .by_elaborator_in("/v", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Proof,
                offender: Some(offender),
                ..
            } if *offender == global("/spin")
        )
    })
    .by_certifier_in("/v", |error| {
        matches!(
            error,
            curios_cert::Error::NotTotal {
                erased: Erased::Proof,
                ..
            }
        )
    });
}

// A type may name a loop it never applies: a finished declaration whose recursion does not descend unfolds to nothing in either checker, so a type naming it reaches nothing that loops, and type totality asks no more of it. `spin` is such a declaration, and so is the witness `Joint` resolves to for a pair of actions, whose method runs `Io/both`: a parameter type counting a list `spin` is mapped over, a pair's answers read through the witness's `Answers`, and a list of actions `spin` is mapped over counted through a function's result type, each formerly refused as a type reaching what may not terminate, are accepted, and run. Mutation-checked: reaching every name a type position mentions refuses each again.
#[test]
fn a_type_naming_a_loop_it_never_applies_is_accepted() {
    let preamble = r#"
        use /std/{Nat, List, Io, Vec};

        let spin(n: Nat) -> Io(Nat) = Io/bind(Io/yield(), (()) => spin(n));
    "#;
    for program in [
        r#"
        let take(v: Vec(Io(Nat), List/length(List/map([1, 2, 3], spin)))) -> Nat = Vec/length(v);

        /std/print(Nat/to_str(take(Vec/of_list(List/map([1, 2, 3], spin)))))
        "#,
        r#"
        concept Joint(T: Type): Type {
            Answers: Type,
            run(T) -> Io(Answers),
        }

        satisfy (@A: Type, @B: Type) => Joint({Io(A), Io(B)}) {
            Answers = {A, B},
            run(t) = Io/both(t.0, t.1),
        }

        let all_of(@T: Type, use Joint(T), actions: T) -> Io(Joint/Answers(@T)) =
            Joint/run(actions);

        let main() -> Io({}) =
            let (a, b) = all_of((Io/pure(1), Io/pure(2)))!;
            /std/print(Nat/to_str(a + b));

        main()
        "#,
        r#"
        let all_counted(@A: Type, actions: List(Io(A))) -> Io(Vec(A, List/length(actions))) =
            match actions
            | [] => Io/pure(Vec/nil())
            | [a, ..more] =>
                let x = a!;
                let rest = all_counted(more)!;
                Io/pure(Vec/cons(x, rest))
            end;

        let counted() -> Nat = Vec/length(Vec/of_list(List/map([1, 2, 3], spin)));

        let main() -> Io(Nat) =
            let answers = all_counted(List/map([4, 5, 6], (n: Nat) => Io/pure(n)))!;
            Io/pure(Vec/length(answers) + counted());

        /std/print(Nat/to_str(main()!))
        "#,
    ] {
        let source = fill!("${preamble}${program}", preamble, program);
        assert!(!run(&source).is_empty(), "{source}");
    }
}

// A type naming a loop is no fault once the loop is withheld, so what keeps a family from holding itself negatively behind one is positivity alone: `G` matches on a value of `F(1)`, which `loop` would be, and `B` holds itself as `G`'s second argument left of an arrow. `G(loop(()), B)` unfolds to a match stuck on `loop`, and positivity reads a stuck match as every arm it could reduce to, so `B`, which the arm puts left of the arrow, is refused. Mutation-checked: reading a stuck match's suppliers alone admits `B`. `curios-cert`'s `recheck::positivity_tests` puts the same declarations to the certifier under this name. Rename both or neither. The control hands `G` a type that is not `B`.
#[test]
fn a_family_behind_a_stuck_loop_is_held_to_positivity() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{False};

        induct F: (n: Nat) -> Type
        | c(): (0)
        end

        let loop(u: {}) -> F(1) = loop(u);

        let G(x: F(1), A: Type) -> Type = match x | c() => A end;

        induct B: Type
        | mk(f: (G(loop(()), Nat)) -> False)
        end

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("G(loop(()), Nat)", "G(loop(()), B)")])
    .by_elaborator_in("/B", |error| {
        matches!(error, curios_elab::Error::NotStrictlyPositive { .. })
    });
}

// A type may name a loop, and a proof standing in it may not: `T`'s body is a type, and the scrutinee of its match is a proof, `lemma` instantiated at `spin(0)`, which proof totality refuses by name wherever it stands. The control instantiates `lemma` at a value that terminates.
#[test]
fn a_proof_standing_in_a_type_and_naming_a_loop_is_refused() {
    control(
        r#"
        use /std/{Nat, Eq};
        use /std/Bool/{True};

        let fine(n: Nat) -> Nat = n;
        let spin(n: Nat) -> Nat = spin(n);

        let lemma(@n: Nat) -> Eq()(n, n) = Eq/refl();

        let T: Prop = match lemma(@fine(0)) | refl() => True end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("lemma(@fine(0))", "lemma(@spin(0))")])
    .by_elaborator_in("/T", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Proof,
                offender: Some(offender),
                ..
            } if *offender == global("/spin")
        )
    })
    .by_certifier_in("/T", |error| {
        matches!(
            error,
            curios_cert::Error::NotTotal {
                erased: Erased::Proof,
                ..
            }
        )
    });
}

// A family indexing a payload by a type-level function and instantiated at a loop ties no knot: `Trap(inf)`'s payload is `Shape(inf)`, a stuck application with `inf` withheld, so the match `delta` would take it apart by finds no family. `curios-cert`'s `recheck::elimination_tests` puts the same declarations to the certifier under this name. Rename both or neither. The control instantiates `Trap` at a value that terminates, whose payload is `Sink({})`.
#[test]
fn a_family_instantiated_at_a_loop_ties_no_knot() {
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

        induct Trap(f : F) : pub Type
        | mk(x : Shape(f))
        end

        let delta(t : Trap(F/more(F/stop()))) -> /std/Bool/False =
            match t
            | mk(x) => match x | sink(g) => g(()) end
            end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("Trap(F/more(F/stop()))", "Trap(inf)")])
    .by_elaborator_in("/delta", |error| {
        matches!(error, curios_elab::Error::NotAInductType { .. })
    });
}
