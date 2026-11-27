//! A partial definition at an erased position, however it is reached, and what does not count as descent.

use {
    super::test_support::*, crate::test_support::*, curios_analysis::Erased, curios_common::fill,
};

/// A proof read off a `Type`-sorted carrier `build` makes, reached four ways: directly, through a witness, through a higher-order argument and through a field. Each refusal has `build` call itself, the box at `False`; the certifier's local gate does not fire there — `Box` is neither a proposition nor a sort — so these are the class the erasure obligations in `curios-cert` exist for.
const BUILT_DIRECT: &str = r#"
    use /std/{Nat};
    use /std/Bool/{True};
    struct Box : pub Type { p : True }
    let build(n : Nat) -> Box => Box { p = True/qed() };
    let bad : True = build(0).p;
    /std/print("ok")
    "#;

const BUILT_THROUGH_WITNESS: &str = r#"
    use /std/{Nat};
    use /std/Bool/{True};
    struct Box : pub Type { p : True }
    concept Make(A : Type) : pub Type { make(A) -> Box, }
    let build(n : Nat) -> Box => Box { p = True/qed() };
    satisfy Make(Nat) { make(n) => build(n), }
    let bad : True = Make/make(0).p;
    /std/print("ok")
    "#;

const BUILT_HIGHER_ORDER: &str = r#"
    use /std/{Nat};
    use /std/Bool/{True};
    struct Box : pub Type { p : True }
    let build(n : Nat) -> Box => Box { p = True/qed() };
    let apply(f : (Nat) -> Box, n : Nat) -> Box => f(n);
    let bad : True = apply(build, 0).p;
    /std/print("ok")
    "#;

const BUILT_IN_FIELD: &str = r#"
    use /std/{Nat};
    use /std/Bool/{True};
    struct Box : pub Type { p : True }
    struct Holder : pub Type { run : (Nat) -> Box }
    let build(n : Nat) -> Box => Box { p = True/qed() };
    let holder : Holder = Holder { run = build };
    let bad : True = holder.run(0).p;
    /std/print("ok")
    "#;

/// A proof in a position the judgment *infers* rather than checks — a match scrutinee — inside a definition whose own type is relevant, so the body is not a proof position either. Both checkers record every settled node with the type it settled at, checked or inferred alike. In the refusal, `build` calling itself and the box at `False`, nothing is a checked position and the elimination conjures a `Nat` from a proof that never terminates, so a checker seeding only checked positions would accept it.
const BUILT_AT_AN_INFERRED_POSITION: &str = r#"
    use /std/{Nat};
    use /std/Bool/{True};
    struct Box : pub Type { p : True }
    let build(n : Nat) -> Box => Box { p = True/qed() };
    let conjured : Nat =
        match build(0).p | qed() => 7
        end;
    /std/print(Nat/to_str(conjured))
    "#;

/// A partial definition behind a `Type`-sorted carrier, reached four ways, and a diverging proof at a position the judgment infers rather than checks: each is a proof resting on something that may not terminate, however it is reached. Each control is the same proof read off a `build` that terminates.
#[test]
fn a_proof_resting_on_a_partial_definition_is_refused_however_it_is_reached() {
    let loops = ("Box { p = True/qed() }", "build(n)");
    let at_false = [("{True}", "{False}"), ("p : True }", "p : False }")];
    for (source, prints, at, edits) in [
        (
            BUILT_DIRECT,
            b"ok".as_slice(),
            "/bad",
            vec![
                at_false[0],
                at_false[1],
                ("bad : True", "bad : False"),
                loops,
            ],
        ),
        (
            BUILT_THROUGH_WITNESS,
            b"ok".as_slice(),
            "/bad",
            vec![
                at_false[0],
                at_false[1],
                ("bad : True", "bad : False"),
                loops,
            ],
        ),
        (
            BUILT_HIGHER_ORDER,
            b"ok".as_slice(),
            "/bad",
            vec![
                at_false[0],
                at_false[1],
                ("bad : True", "bad : False"),
                loops,
            ],
        ),
        (
            BUILT_IN_FIELD,
            b"ok".as_slice(),
            "/bad",
            vec![
                at_false[0],
                at_false[1],
                ("bad : True", "bad : False"),
                loops,
            ],
        ),
        (
            BUILT_AT_AN_INFERRED_POSITION,
            b"7".as_slice(),
            "/conjured",
            vec![at_false[0], at_false[1], loops, (" | qed() => 7", "")],
        ),
    ] {
        control(source, prints)
            .refused(&edits)
            .by_elaborator_in(at, |error| {
                matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
            })
            .by_certifier_in(at, |error| {
                matches!(error, curios_cert::Error::NotTotal { .. })
            });
    }
}

// A non-descending `rec` written *inline*, at a `Type`-sorted type so the certifier's local descent gate does not apply, inside a definition that is not itself a proof position. Nothing but the classification walk can see it: `make` has no name-level partiality to inherit and no proof-typed member to gate, so it is partial only if `locally_partial` descends into the `rec` group's member scopes rather than stopping at the node. The proof position is `bad`, which merely mentions `make`, refused as the evaluation cycle `make`'s term is, since `r` reads itself outside a lambda. The control binds `r` to a box it builds.
#[test]
fn a_recursion_written_inline_under_a_carrier_is_still_partial() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};
        struct Box : pub Type { p : True }
        let make : Box =
            let r : Box = Box { p = True/qed() };
            r;
        let bad : True = make.p;
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("{True}", "{False}"),
        ("p : True }", "p : False }"),
        ("bad : True", "bad : False"),
        ("let r : Box = Box { p = True/qed() };", "let r : Box = r;"),
    ])
    .by_elaborator_in("/bad", |error| {
        matches!(error, curios_elab::Error::EvaluatesItself { name } if *name == global("/make"))
    })
    .by_certifier_in("/bad", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// Type totality rather than proof totality: a *type* holding a loop its own declaration writes. A loop a finished declaration holds is withheld from both checkers, so a type naming it reaches nothing that loops; one written in the type itself has no verdict to be withheld by until its declaration is judged, and a type-level loop reties the negative knot strict positivity exists to forbid. The elaborator seeds this from written type positions; the certifier from its own typing, where the body of a definition whose type is a sort is checked against that sort. This row is the only coverage either seeding has for type totality. The control's `spin` hands its argument back.
#[test]
fn a_type_holding_a_loop_it_writes_is_refused() {
    control(
        r#"
        use /std/{Nat, Vec};
        let Sized(n : Nat) -> Type =>
            let spin(k : Nat) -> Nat => k;
            Vec(Nat, spin(n));
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("-> Nat => k;", "-> Nat => spin(k);")])
    .by_elaborator_in("/Sized", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/Sized", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// Saturating subtraction is not a descent: `n - 1` is `0` at `0`, so `bogus(0)` calls itself for ever. The declared result is a proposition, which obliges the group to descend, and an engine crediting `n - 1` as strictly smaller would certify a recursion that does not terminate, and `False` follows, erasure deleting the proof. The elaborator refuses through the reach obligation and the certifier through its own local gate: two rules, two crates, one program. The control subtracts in the arm that rules zero out, where `n - 1` is below `n`.
#[test]
fn saturating_subtraction_is_not_descent() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};
        let bogus(n : Nat) -> True => match n | 0 => True/qed() | _ => bogus(n - 1) end;
        let bad : True = bogus(0);
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("{True}", "{False}"),
        (
            "-> True => match n | 0 => True/qed() | _ => bogus(n - 1) end",
            "-> False => bogus(n - 1)",
        ),
        ("bad : True", "bad : False"),
    ])
    .by_elaborator_in("/bogus", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/bogus", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}

// Permuting the arguments is not a descent either: every call maps a parameter to a parameter, so each call matrix is full of `Same` entries and none is `Less`, and the swap composed with itself is the identity, so no cycle carries a strict decrease. The control swaps the arguments and decreases one, which descends across two calls.
#[test]
fn permuting_arguments_is_not_descent() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};
        let bogus(a : Nat, b : Nat) -> True => match a | 0 => True/qed() | k + 1 => bogus(b, k) end;
        let bad : True = bogus(0, 1);
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("{True}", "{False}"),
        (
            "-> True => match a | 0 => True/qed() | k + 1 => bogus(b, k) end",
            "-> False => bogus(b, a)",
        ),
        ("bad : True", "bad : False"),
    ])
    .by_elaborator_in("/bogus", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/bogus", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}

// A structure's field is read by projection, which takes no case of the value apart, so a function field applied off one grades below nothing: `f` would recur for ever along `l`'s `next`, a structure that hands itself back, and is refused as no descent wherever it is reached. The control is the same recursion over an inductive's payload, which is a descent.
#[test]
fn a_field_applied_off_a_structure_is_not_descent() {
    control(
        r#"
        use /std/Bool/{False};

        induct S: Type | mk(next: () -> S) end

        let l() -> S => S/mk(l);

        let f(x: S) -> False => match x | mk(next) => f(next()) end;

        let bad = f;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("induct S: Type | mk(next: () -> S) end", "struct S: Type { next: () -> S }"),
        ("S/mk(l)", "S { next = l }"),
        ("match x | mk(next) => f(next()) end", "f(x.next())"),
        ("let bad = f;", "let bad: False = f(l());"),
    ])
    .by_elaborator_in("/f", |error| matches!(error, curios_elab::Error::PartialInErasedPosition { offender: Some(offender), .. } if *offender == global("/f")))
    .by_certifier_in("/f", |error| matches!(error, curios_cert::Error::NotDescending { .. }));
}

/// A value that reads itself outside a lambda, reached by a proof: `w` hands itself to `each`, whose answer's method reads `o.inner`, and `r` proves by its method.
const KNOT: &str = r#"
        use /std/{List, print};
        use /std/Bool/{True};

        struct Inner(A: Type): Type {
            step: (A) -> True,
        }

        struct Outer(A: Type): Type {
            inner: Inner(A),
            more: Inner(List(A)),
        }

        induct Doc: Type
        | leaf()
        end

        let all(@T: Type, i: Inner(T), s: List(T)) -> True =>
            match s
            | [] => True/qed()
            | [x, ..rest] => match i.step(x) | qed() => all(i, rest) end
            end;

        let each(@T: Type, o: Outer(T)) -> Inner(List(T)) =>
            Inner { step(s) => all(o.inner, s) };

        let w: Outer(Doc) = Outer { inner = Inner { step(_) => True/qed() }, more = each(w) };

        print("ok")
        "#;

/// A value that reads itself outside a lambda is refused, where a proof reaches it, as the evaluation cycle it is, in erasure's words: erasure refuses the twin whose method answers a `Nat` in the same words, so the rule reads alike whichever stage meets it first. Mutation-checked: with `reads_itself` answering no, `r` is refused as reaching a definition not known to terminate.
#[test]
fn a_value_reading_itself_outside_a_lambda_is_refused_as_an_evaluation_cycle() {
    let proof = (
        r#"print("ok")"#,
        r#"let r: True = w.more.step([Doc/leaf()]);

        print("ok")"#,
    );
    control(KNOT, b"ok")
        .refused(&[proof])
        .by_elaborator_in("/r", |error| matches!(error, curios_elab::Error::EvaluatesItself { name } if *name == global("/w")));

    let report = error(&KNOT.replacen(proof.0, proof.1, 1));

    assert!(
        report.contains("evaluation cycle")
            && report.contains("`w` evaluates itself as it is initialized"),
        "{report}"
    );
}

// An application of a payload is a child of its constructor, which is true of a value that is a finite tree (`curios-analysis`'s `totality`), the premise the paradox of trees breaks: `down` descends by it and is admitted, and `x`, whose payload hands back `x` itself, is a tree with no bottom for the descent to reach. A recursive binding is not known to terminate, so the proof that reaches it is refused at `x`, and the descent is never asked of it. The control applies `down` to nothing in particular.
#[test]
fn a_value_that_is_its_own_child_is_not_known_to_terminate() {
    control(
        r#"
        use /std/Bool/{False};

        induct N: Type
        | s(p: () -> N)
        end

        let down(n: N) -> False => match n | s(p) => down(p()) end;

        let x: N = N/s(() => x);

        let bad = down;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("let bad = down;", "let bad: False = down(x);")])
    .by_elaborator_in("/bad", |error| matches!(error, curios_elab::Error::PartialInErasedPosition { offender: Some(offender), .. } if *offender == global("/x")))
    .by_certifier_in("/bad", |error| matches!(error, curios_cert::Error::NotTotal { .. }));
}

// The size order reads an arm's case for a `Prop`-sorted scrutinee as for any other (`refined_by_arm`), so a recursion descends on an inductive proposition's constructors, and a proof irrelevance holds equal to the scrutinee is then graded below it. What stands between that reading and a closed inhabitant of `False` is coverage, and this is the shape that asks for it: `Q`'s index is a proof, so irrelevance makes `Q(v)` and `Q(Wit/s(v))` one type and the recursive call on the payload is well typed, and `Q/nil()` inhabits `Q(Wit/s(Wit/triv()))` for the same reason — a proof of `Q` at the scrutinee's own type whose payload the descent reads as smaller.
//
// `drain` has no base case, so the forgery needs `nil`'s arm absent, and a proposition's two constructors never clash (`curios-analysis`'s `invert`, whose tag test fires only at a relevant family). So the arm is mandatory and has to inhabit `False`. The refusal is coverage's, not the size order's: a change that let a `Prop`-valued index excuse an omitted arm would hand this program the descent it already has. The control writes `nil`'s arm, at `True`.
#[test]
fn a_recursion_on_a_propositions_case_still_needs_the_arm_it_omits() {
    control(
        r#"
        use /std/Bool/{True, False};

        induct Wit: Prop
        | triv()
        | s(inner: Wit)
        end

        induct Q: (Wit) -> Prop
        | nil(): (Wit/triv())
        | cons(v: Wit, inner: Q(v)): (Wit/s(v))
        end

        let drain(v: Wit, q: Q(Wit/s(v))) -> True =>
            match q | nil() => True/qed()
            | cons(w, inner) => drain(w, inner)
            end;

        let bad: True = drain(Wit/triv(), Q/nil());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("{True, False}", "{False}"),
        ("-> True =>", "-> False =>"),
        ("match q | nil() => True/qed()", "match q"),
        ("bad: True", "bad: False"),
    ])
    .by_elaborator_in("/drain", |error| {
        matches!(
            error,
            curios_elab::Error::MatrixUncovered { indexed: true, .. }
        )
    });
}

/// A recursion on a proposition's own case, every arm recursing, at an index the declarations hold a value of: `base`'s arm is excused where the index tells `base` from `step`. Each refusal holds a proof there, where `P/base()` stands at `step`'s target by irrelevance, so `_bad`, written `f(P/base())`, is a closed term at `False` were `base`'s arm excused. The holes are the declarations the index needs, the index's type, and `base`'s and `step`'s targets.
const A_RECURSION_AT_AN_INDEX_HOLDING_A_PROOF: &str = r#"
    use /std/Bool/{False};

    ${declarations}

    induct P: (${index}) -> Prop
    | base(): (${base})
    | step(p: P(${step})): (${step})
    end

    let f(p: P(${step})) -> False =>
        match p
        | step(q) => f(q)
        end;

    let _bad = f;

    /std/print("ok")
    "#;

// `a_recursion_on_a_propositions_case_still_needs_the_arm_it_omits` at every shape a program can carry the proof in: a relevant family holding one, a list and a tuple of them, a type family over a proposition, a proposition above level zero, and one declared in a group. The size order reads `step(q)` as a descent in each, and coverage keeps `base`'s arm, since no clash is concluded between two of a proposition's constructors wherever in an index they meet. `curios-cert`'s `recheck::case_tests` puts the shapes to the certifier under this name, a stated instance and a type a definition spells among them. Each control holds a value where its refusal holds a proof.
#[test]
fn no_index_holding_a_proof_excuses_the_arm_a_recursion_on_a_propositions_case_omits() {
    let wit = r#"
    induct Wit: Type
    | a()
    | b()
    end
    "#;
    let boxed = r#"
    induct Wit: Type
    | a()
    | b()
    end

    induct Box: Type
    | box(w: Wit)
    end
    "#;
    let optional = r#"
    induct Wit: Type
    | a()
    | b()
    end

    induct Opt(A: Type): Type
    | none()
    | some(x: A)
    end
    "#;
    let high = r#"
    induct High: Type
    | ha(f: (A: Prop) -> (A) -> A)
    | hb()
    end
    "#;
    let grouped = r#"
    induct G: Type
    | ga()
    | gb(x: G2)
    and G2: Type
    | c(y: G)
    | d()
    end
    "#;

    let proof = ("induct Wit: Type", "induct Wit: Prop");
    for (declarations, index, base, step, edits) in [
        (
            boxed,
            "Box",
            "Box/box(Wit/a())",
            "Box/box(Wit/b())",
            vec![proof],
        ),
        (wit, "/std/List(Wit)", "[Wit/a()]", "[Wit/b()]", vec![proof]),
        (
            wit,
            "{Wit, Wit}",
            "(Wit/a(), Wit/a())",
            "(Wit/b(), Wit/b())",
            vec![proof],
        ),
        (
            optional,
            "Opt(Wit)",
            "Opt/some(Wit/a())",
            "Opt/some(Wit/b())",
            vec![proof, ("Opt(A: Type)", "Opt(A: Prop)")],
        ),
        (
            high,
            "High",
            "High/ha((_) => (x) => x)",
            "High/hb()",
            vec![("induct High: Type", "induct High: Prop")],
        ),
        (
            grouped,
            "G",
            "G/ga()",
            "G/gb(G2/d())",
            vec![
                ("induct G: Type", "induct G: Prop"),
                ("and G2: Type", "and G2: Prop"),
            ],
        ),
    ] {
        let mut edits = edits;
        edits.push(("let _bad = f;", "let _bad: False = f(P/base());"));
        control(
            &fill!(
                A_RECURSION_AT_AN_INDEX_HOLDING_A_PROOF,
                declarations,
                index,
                base,
                step
            ),
            b"ok",
        )
        .refused(&edits)
        .by_elaborator_in("/f", |error| {
            matches!(
                error,
                curios_elab::Error::MatrixUncovered { indexed: true, .. }
            )
        });
    }
}

// `S` is indexed by a proof of `W`, and its one payload is its index, so a match on an `S` proof fires with the proof unreduced and reads `v` off the index: `v` is `i`, no part of `s`, and the size order grades it below `s`. That reading is sound, a proof of `S(i)` ranking above `i` at its least derivation; what refuses the loop is that `S/mk(w)`, built around `h`'s own case, is nothing at or below `w`, so `h(base)`, which is `h(base)` for ever, starts no descending thread. `curios-cert`'s `recheck::recursion_tests` puts the module to the certifier under this name. The control's `W` has a case below another, and its `g` hands `h` its own index rather than a payload the cast reads, so the two descend.
#[test]
fn a_payload_a_cast_reads_off_a_proofs_index_is_no_descent() {
    control(
        r#"
        use /std/Bool/{False};

        induct W: pub Prop
        | up(w: W)
        end

        induct S: (W) -> pub Prop
        | mk(v: W): (v)
        end

        let g(i: W, s: S(i)) -> False =>
            h(i)
        and h(w: W) -> False =>
            match w
            | up(u) => g(u, S/mk(u))
            end;

        let _bad = h;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("| up(w: W)", "| base()"),
        (
            "            h(i)\n",
            "            match s\n            | mk(v) => h(v)\n            end\n",
        ),
        ("| up(u) => g(u, S/mk(u))", "| base() => g(w, S/mk(w))"),
        ("let _bad = h;", "let _bad: False = h(W/base());"),
    ])
    .by_elaborator_in("/g", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Proof,
                ..
            }
        )
    })
    .by_certifier_in("/g", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}

// A call is graded on what it passes as written, and an arm's index inversion concludes nothing the size order reads: in `mk(v)`, `i` is solved to `v` for typing, and `h(i)` hands `h` the `i` `g` was given, which `h` takes apart, so the two descend on it, as the elaborator reads them. Graded at the solution, `h(i)` would hand on `v`, a payload of `s` holding nothing of `i`, and the group would read as not descending. The refusal hands `g` back the `w` `h` was given, which descends on nothing.
#[test]
fn a_call_passing_a_variable_an_arm_solved_is_graded_on_the_variable() {
    control(
        r#"
        use /std/Bool/{False};

        induct W: pub Prop
        | up(w: W)
        end

        induct S: (W) -> pub Prop
        | mk(v: W): (v)
        end

        let g(i: W, s: S(i)) -> False =>
            match s
            | mk(v) => h(i)
            end
        and h(w: W) -> False =>
            match w
            | up(u) => g(u, S/mk(u))
            end;

        let _bad = h;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("| up(u) => g(u, S/mk(u))", "| up(u) => g(w, S/mk(w))")])
    .by_elaborator_in("/g", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Proof,
                ..
            }
        )
    })
    .by_certifier_in("/g", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}

// The one way a proof reaches a type is the cast, and a recursion graded below the proof it casts along must still descend to pass type totality: `m` is below `p`, `S/mk(m)` is nothing below `p`, and `m` stands for `n` only by the arm's index solution, which refines for typing alone. `curios-cert`'s `recheck::recursion_tests` puts the module to the certifier under this name. The control recurses on `n` itself.
#[test]
fn a_type_recursing_through_a_cast_on_the_index_it_reads_does_not_descend() {
    control(
        r#"
        use /std/{Nat};

        induct S: (Nat) -> Prop
        | mk(n: Nat): (n)
        end

        let X(n: Nat, p: S(n)) -> Type =>
            match n
            | 0 => Nat
            | k + 1 => X(k, S/mk(k))
            end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[(
        "match n\n            | 0 => Nat\n            | k + 1 => X(k, S/mk(k))",
        "match p\n            | mk(m) => X(m, S/mk(m))",
    )])
    .by_elaborator_in("/X", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Type,
                offender: None,
                ..
            }
        )
    });
}

// The field's accessibility routes all need a proof of accessibility where none exists, and `spin` is that recursion's honest half: over a reflexive relation nothing is accessible, `spin` descends through `below(w, qed)`, an application of `intro`'s payload, and so proves accessibility empty there. The proof it is handed is built by a recursion that hands itself back, which descends on nothing and is refused at the proof it stands at. The control's proof is honest, over the relation that relates nothing.
#[test]
fn an_accessibility_proof_built_by_self_reference_is_refused() {
    control(
        r#"
        use /std/Bool/{True, False};

        induct Acc(@A: Type, R: (A, A) -> Prop): (A) -> pub Prop
        | intro(@x: A, below: (y: A, r: R(y, x)) -> Acc(R)(y)): (x)
        end

        let spin(x: {}, a: Acc((p: {}, q: {}) => True)(x)) -> False =>
            match a
            | intro(@w, below) => spin(w, below(w, True/qed()))
            end;

        let forever(x: {}) -> Acc((p: {}, q: {}) => False)(x) =>
            Acc/intro((y, r) => match r end);

        let _bad = spin;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        (
            "Acc((p: {}, q: {}) => False)(x) =>\n            Acc/intro((y, r) => match r end);",
            "Acc((p: {}, q: {}) => True)(x) =>\n            Acc/intro((y, r) => forever(y));",
        ),
        (
            "let _bad = spin;",
            "let _bad: False = spin((), forever(()));",
        ),
    ])
    .by_elaborator_in("/forever", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Proof,
                ..
            }
        )
    })
    .by_certifier_in("/forever", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}

/// A proof standing on a partial term its elimination reads nothing of is refused too. `Pin(n + 1)` is a case every constructor's target clashes with, so the elimination enumerates no arm and reduction never takes the scrutinee apart — the shape where "a term left stuck admits nothing" has to be carried by proof totality, since `spin(n + 1)` is a closed term of an uninhabited type no rule reduces. The control eliminates an argument of that type instead, the ex falso it is.
#[test]
fn a_partial_inhabitant_of_an_impossible_index_proves_nothing() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{False};
        induct Pin : (n : Nat) -> pub Type
        | only() : (0)
        end
        let spin(n : Nat) -> Pin(n) => spin(n);
        let bad(n : Nat, pin : Pin(n + 1)) -> False =>
            match pin
            end;
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("bad(n : Nat, pin : Pin(n + 1))", "bad(n : Nat)"),
        ("match pin", "match spin(n + 1)"),
    ])
    .by_elaborator_in("/bad", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_certifier_in("/bad", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// A call whose argument is a variable the arm's index inversion solved is graded as written: `i` is `v` in `mk(v)`'s arm only by a conversion that proof irrelevance satisfies, so reading `h(i)` as `h(v)` is a reading of `s`'s payload and no descent of `i`. What refuses the loop is that `h(base)`, whose case hands `g` `S/mk(w)`, is nothing below `w`. The control's `W` has no base and `g` hands `h` its index directly.
#[test]
fn a_call_at_a_variable_an_index_inversion_solved_starts_no_descending_thread() {
    control(
        r#"
        use /std/Bool/{False};

        induct W: pub Prop
        | up(w: W)
        end

        induct S: (W) -> pub Prop
        | mk(v: W): (v)
        end

        let g(i: W, s: S(i)) -> False =>
            h(i)
        and h(w: W) -> False =>
            match w
            | up(u) => g(u, S/mk(u))
            end;

        let _bad = h;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("| up(w: W)", "| base()\n        | up(w: W)"),
        (
            "            h(i)\n",
            "            match s\n            | mk(v) => h(i)\n            end\n",
        ),
        (
            "| up(u) => g(u, S/mk(u))",
            "| base() => g(w, S/mk(w))\n            | up(u) => g(u, S/mk(u))",
        ),
        ("let _bad = h;", "let _bad: False = h(W/base());"),
    ])
    .by_elaborator_in("/g", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Proof,
                ..
            }
        )
    })
    .by_certifier_in("/g", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}

// An applied lambda's binder stands for the argument, so splitting it splits the argument and its predecessor is below the parameter; handing the successor back, `k + 1`, is the parameter itself and no descent. The control passes the predecessor.
#[test]
fn splitting_an_applied_lambdas_binder_grades_the_successor_as_no_descent() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};
        pub let below(n: Nat) -> True =>
            ((x: Nat) => match x | 0 => True/qed() | k + 1 => below(k) end)(n);
        /std/print("held")
        "#,
        b"held",
    )
    .refused(&[("below(k)", "below(k + 1)")])
    .by_elaborator_in("/below", |error| {
        matches!(
            error,
            curios_elab::Error::PartialInErasedPosition {
                erased: Erased::Proof,
                ..
            }
        )
    });
}

/// A concept whose law states its method, witnessed for a family whose `same` recurses on the payloads through the witness's own key, as a lawful derived witness of a recursive family does, and whose `sound` recurses on them too. The law's binder `h` states the literal's own `same`, so its type holds a copy of the method whose calls are typed before `h` is bound.
const A_LAW_THROUGH_ITS_OWN_WITNESS: &str = r#"
    use /std/{Bool, Eq, Show, print};
    concept Same(A : Type) : Type {
        same(A, A) -> Bool,
        sound(a : A, b : A, h : Bool/Holds(same(a, b))) -> Eq()(a, b),
    }
    induct Tree : Type
    | leaf()
    | node(Tree, Tree)
    end
    satisfy Same(Tree) {
        same(a, b) =>
            match a
            | leaf() => match b | leaf() => true | _ => false end
            | node(l, r) => match b | node(m, s) => Same/same(l, m) && Same/same(r, s) | _ => false end
            end,
        sound(a, b, h) =>
            match a
            | leaf() => match b | leaf() => Eq/refl() end
            | node(l, r) =>
                match b
                | node(m, s) =>
                    let left : Eq()(l, m) = Same/sound(l, m, Bool/and/first(Same/same(l, m), Same/same(r, s), h));
                    let right : Eq()(r, s) = Same/sound(r, s, Bool/and/second(Same/same(l, m), Same/same(r, s), h));
                    match left | refl() => match right | refl() => Eq/refl() end end
                end
            end,
    }
    let t : Tree = Tree/node(Tree/leaf(), Tree/leaf());
    print(Show/show(Same/same(t, t)))
    "#;

/// The same law for a family that holds its children in a `List`: `sound` recurses through `List`'s witness, handed the family's own dictionary, so its descent is read through what `List`'s law applies.
const A_LAW_THROUGH_A_LISTS_WITNESS: &str = r#"
    use /std/{Bool, Eq, List, Show, print};
    concept Same(A : Type) : Type {
        same(A, A) -> Bool,
        sound(a : A, b : A, h : Bool/Holds(same(a, b))) -> Eq()(a, b),
    }
    let list_same(@A : Type, use Same(A), a : List(A), b : List(A)) -> Bool =>
        match (a, b)
        | ([], []) => true
        | ([x, ..xs], [y, ..ys]) => Same/same(x, y) && list_same(xs, ys)
        | _ => false
        end;
    let list_sound(@A : Type, use Same(A), a : List(A), b : List(A), h : Bool/Holds(list_same(a, b))) -> Eq()(a, b) =>
        match a
        | [] => match b | [] => Eq/refl() end
        | [x, ..xs] =>
            match b
            | [y, ..ys] =>
                let head : Eq()(x, y) = Same/sound(x, y, Bool/and/first(Same/same(x, y), list_same(xs, ys), h));
                let tail : Eq()(xs, ys) = list_sound(xs, ys, Bool/and/second(Same/same(x, y), list_same(xs, ys), h));
                match head | refl() => match tail | refl() => Eq/refl() end end
            end
        end;
    satisfy (@A : Type, use Same(A)) -> Same(List(A)) {
        same = list_same,
        sound = list_sound,
    }
    induct Rose : Type
    | node(List(Rose))
    end
    satisfy Same(Rose) {
        same(a, b) => match a | node(xs) => match b | node(ys) => Same/same(xs, ys) end end,
        sound(a, b, h) =>
            match a
            | node(xs) =>
                match b
                | node(ys) =>
                    let inner : Eq()(xs, ys) = Same/sound(xs, ys, h);
                    match inner | refl() => Eq/refl() end
                end
            end,
    }
    let r : Rose = Rose/node([Rose/node([])]);
    print(Show/show(Same/same(r, r)))
    "#;

/// A law beside a method that recurses through its own witness descends in both checkers, directly and through a container's witness at the family's own dictionary: each grades the calls the law's binder type makes, through its copy of the method, against every parameter of the law. Mutation-checked: with the certifier grading such a call against the parameters bound where it stands, both controls are refused by the certifier alone. Each refusal reads the method and the law through the witness at the values they were handed.
#[test]
fn a_law_beside_a_method_recursing_through_its_own_witness_descends() {
    for (source, at, edits) in [
        (
            A_LAW_THROUGH_ITS_OWN_WITNESS,
            "/witness~/Same(/Tree)",
            vec![
                (
                    "Same/same(l, m) && Same/same(r, s) | _ => false",
                    "Same/same(a, b) | _ => false",
                ),
                (
                    "let left : Eq()(l, m) = Same/sound(l, m, Bool/and/first(Same/same(l, m), Same/same(r, s), h));\n",
                    "",
                ),
                (
                    "let right : Eq()(r, s) = Same/sound(r, s, Bool/and/second(Same/same(l, m), Same/same(r, s), h));\n",
                    "",
                ),
                (
                    "match left | refl() => match right | refl() => Eq/refl() end end",
                    "Same/sound(a, b, h)",
                ),
            ],
        ),
        (
            A_LAW_THROUGH_A_LISTS_WITNESS,
            "/witness~/Same(/Rose)",
            vec![
                (
                    "same(a, b) => match a | node(xs) => match b | node(ys) => Same/same(xs, ys) end end,",
                    "same(a, b) => Same/same(a, b),",
                ),
                ("let inner : Eq()(xs, ys) = Same/sound(xs, ys, h);\n", ""),
                (
                    "match inner | refl() => Eq/refl() end",
                    "Same/sound(a, b, h)",
                ),
            ],
        ),
    ] {
        control(source, b"true")
            .refused(&edits)
            .by_elaborator_in(at, |error| {
                matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
            })
            .by_certifier_in(at, |error| {
                matches!(
                    error,
                    curios_cert::Error::NotTotal { .. } | curios_cert::Error::NotDescending { .. }
                )
            });
    }
}
