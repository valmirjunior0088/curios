//! A partial definition at an erased position, however it is reached, and what does not count as descent.

use {super::test_support::*, curios_analysis::Erased, curios_common::fill};

/// A proof read off a `Type`-sorted carrier `build` makes, reached four ways: directly, through a witness, through a higher-order argument and through a field. Each refusal has `build` call itself, the box at `False`; the kernel's local gate does not fire there — `Box` is neither a proposition nor a sort — so these are the class the erasure obligations in `curios-cert` exist for.
const BUILT_DIRECT: &str = r#"
    use /std/{Nat};
    use /std/Bool/{True};
    struct Box : pub Type { p : True }
    let build(n : Nat) -> Box = Box { p = True/qed() };
    let bad : True = build(0).p;
    /std/print("ok")
    "#;

const BUILT_THROUGH_WITNESS: &str = r#"
    use /std/{Nat};
    use /std/Bool/{True};
    struct Box : pub Type { p : True }
    concept Make(A : Type) : pub Type { make(A) -> Box, }
    let build(n : Nat) -> Box = Box { p = True/qed() };
    satisfy Make(Nat) { make(n) = build(n), }
    let bad : True = Make/make(0).p;
    /std/print("ok")
    "#;

const BUILT_HIGHER_ORDER: &str = r#"
    use /std/{Nat};
    use /std/Bool/{True};
    struct Box : pub Type { p : True }
    let build(n : Nat) -> Box = Box { p = True/qed() };
    let apply(f : (Nat) -> Box, n : Nat) -> Box = f(n);
    let bad : True = apply(build, 0).p;
    /std/print("ok")
    "#;

const BUILT_IN_FIELD: &str = r#"
    use /std/{Nat};
    use /std/Bool/{True};
    struct Box : pub Type { p : True }
    struct Holder : pub Type { run : (Nat) -> Box }
    let build(n : Nat) -> Box = Box { p = True/qed() };
    let holder : Holder = Holder { run = build };
    let bad : True = holder.run(0).p;
    /std/print("ok")
    "#;

/// A proof in a position the judgment *infers* rather than checks — a match scrutinee — inside a definition whose own type is relevant, so the body is not a proof position either. Both checkers record every settled node with the type it settled at, checked or inferred alike. In the refusal, `build` calling itself and the box at `False`, nothing is a checked position and the elimination conjures a `Nat` from a proof that never terminates, so a checker seeding only checked positions would accept it.
const BUILT_AT_AN_INFERRED_POSITION: &str = r#"
    use /std/{Nat};
    use /std/Bool/{True};
    struct Box : pub Type { p : True }
    let build(n : Nat) -> Box = Box { p = True/qed() };
    let conjured : Nat =
        match build(0).p : (_) => Nat | qed() => 7
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
            .by_kernel_in(at, |error| {
                matches!(error, curios_cert::Error::NotTotal { .. })
            });
    }
}

// A non-descending `rec` written *inline*, at a `Type`-sorted type so the kernel's local descent gate does not apply, inside a definition that is not itself a proof position. Nothing but the classification walk can see it: `make` has no name-level partiality to inherit and no proof-typed member to gate, so it is partial only if `locally_partial` descends into the `rec` group's member scopes rather than stopping at the node. The proof position is `bad`, which merely mentions `make`. The control binds `r` to a box it builds.
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
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_kernel_in("/bad", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// Obligation (T) rather than (V): a *type* that reaches a definition not known to terminate. `spin` calling itself is legal — general recursion at a relevant type is the language's design — but a type mentioning it is not, because erasure deletes types too, and a type-level loop reties the negative knot strict positivity exists to forbid. The elaborator seeds this from written type positions; the kernel from its own typing, where the body of a definition whose type is a sort is checked against that sort. This row is the only coverage either seeding has for (T). The control's `spin` hands its argument back.
#[test]
fn a_type_reaching_a_partial_definition_is_refused() {
    control(
        r#"
        use /std/{Nat, Vec};
        let spin(n : Nat) -> Nat = n;
        let Sized(n : Nat) -> Type = Vec(Nat, spin(n));
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("-> Nat = n;", "-> Nat = spin(n);")])
    .by_elaborator_in("/Sized", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_kernel_in("/Sized", |error| {
        matches!(error, curios_cert::Error::NotTotal { .. })
    });
}

// Saturating subtraction is not a descent: `n - 1` is `0` at `0`, so `bogus(0)` calls itself for ever. The declared result is a proposition, which obliges the group to descend, and an engine crediting `n - 1` as strictly smaller would certify a recursion that does not terminate, and `False` follows, erasure deleting the proof. The elaborator refuses through the reach obligation and the kernel through its own local gate: two rules, two crates, one program. The control subtracts in the arm that rules zero out, where `n - 1` is below `n`.
#[test]
fn saturating_subtraction_is_not_descent() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True};
        let bogus(n : Nat) -> True = match n | 0 => True/qed() | _ => bogus(n - 1) end;
        let bad : True = bogus(0);
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("{True}", "{False}"),
        (
            "-> True = match n | 0 => True/qed() | _ => bogus(n - 1) end",
            "-> False = bogus(n - 1)",
        ),
        ("bad : True", "bad : False"),
    ])
    .by_elaborator_in("/bogus", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_kernel_in("/bogus", |error| {
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
        let bogus(a : Nat, b : Nat) -> True = match a | 0 => True/qed() | k + 1 => bogus(b, k) end;
        let bad : True = bogus(0, 1);
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("{True}", "{False}"),
        (
            "-> True = match a | 0 => True/qed() | k + 1 => bogus(b, k) end",
            "-> False = bogus(b, a)",
        ),
        ("bad : True", "bad : False"),
    ])
    .by_elaborator_in("/bogus", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_kernel_in("/bogus", |error| {
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

        let l() -> S = S/mk(l);

        let f(x: S) -> False = match x | mk(next) => f(next()) end;

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
    .by_kernel_in("/f", |error| matches!(error, curios_cert::Error::NotDescending { .. }));
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

        let down(n: N) -> False = match n | s(p) => down(p()) end;

        let x: N = N/s(() => x);

        let bad = down;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("let bad = down;", "let bad: False = down(x);")])
    .by_elaborator_in("/bad", |error| matches!(error, curios_elab::Error::PartialInErasedPosition { offender: Some(offender), .. } if *offender == global("/x")))
    .by_kernel_in("/bad", |error| matches!(error, curios_cert::Error::NotTotal { .. }));
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

        let drain(v: Wit, q: Q(Wit/s(v))) -> True =
            match q: (_, _) => True | nil() => True/qed()
            | cons(w, inner) => drain(w, inner)
            end;

        let bad: True = drain(Wit/triv(), Q/nil());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("{True, False}", "{False}"),
        ("-> True =", "-> False ="),
        ("(_, _) => True | nil() => True/qed()", "(_, _) => False"),
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

    let f(p: P(${step})) -> False =
        match p
        | step(q) => f(q)
        end;

    let _bad = f;

    /std/print("ok")
    "#;

// `a_recursion_on_a_propositions_case_still_needs_the_arm_it_omits` at every shape a program can carry the proof in: a relevant family holding one, a list and a tuple of them, a type family over a proposition, a proposition above level zero, and one declared in a group. The size order reads `step(q)` as a descent in each, and coverage keeps `base`'s arm, since no clash is concluded between two of a proposition's constructors wherever in an index they meet. `curios-cert`'s `recheck::case_tests` puts the shapes to the kernel under this name, a stated instance and a type a definition spells among them. Each control holds a value where its refusal holds a proof.
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

// `S` is indexed by a proof of `W`, and its one payload is its index, so a match on an `S` proof fires with the proof unreduced and reads `v` off the index: `v` is `i`, no part of `s`, and the size order grades it below `s`. That reading is sound, a proof of `S(i)` ranking above `i` at its least derivation; what refuses the loop is that `S/mk(w)`, built around `h`'s own case, is nothing at or below `w`, so `h(base)`, which is `h(base)` for ever, starts no descending thread. `curios-cert`'s `recheck::recursion_tests` puts the module to the kernel under this name. The control's `W` has a case below another, and its `g` hands `h` its own index rather than a payload the cast reads, so the two descend.
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

        let g(i: W, s: S(i)) -> False =
            h(i)
        and h(w: W) -> False =
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
    .by_kernel_in("/g", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}

// The one way a proof reaches a type is the cast, and a recursion graded below the proof it casts along must still descend to stand at (T): `m` is below `p`, `S/mk(m)` is nothing below `p`, and `m` stands for `n` only by the arm's index solution, which refines for typing alone. `curios-cert`'s `recheck::recursion_tests` puts the module to the kernel under this name. The control recurses on `n` itself.
#[test]
fn a_type_recursing_through_a_cast_on_the_index_it_reads_does_not_descend() {
    control(
        r#"
        use /std/{Nat};

        induct S: (Nat) -> Prop
        | mk(n: Nat): (n)
        end

        let X(n: Nat, p: S(n)) -> Type =
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

        let spin(x: {}, a: Acc((p: {}, q: {}) => True)(x)) -> False =
            match a
            | intro(@w, below) => spin(w, below(w, True/qed()))
            end;

        let forever(x: {}) -> Acc((p: {}, q: {}) => False)(x) =
            Acc/intro((y, r) => match r end);

        let _bad = spin;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        (
            "Acc((p: {}, q: {}) => False)(x) =\n            Acc/intro((y, r) => match r end);",
            "Acc((p: {}, q: {}) => True)(x) =\n            Acc/intro((y, r) => forever(y));",
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
    .by_kernel_in("/forever", |error| {
        matches!(error, curios_cert::Error::NotDescending { .. })
    });
}

/// A proof standing on a partial term its elimination reads nothing of is refused too. `Pin(n + 1)` is a case every constructor's target clashes with, so the elimination enumerates no arm and reduction never takes the scrutinee apart — the shape where "a term left stuck admits nothing" has to be carried by obligation (V), since `spin(n + 1)` is a closed term of an uninhabited type no rule reduces. The control eliminates an argument of that type instead, the ex falso it is.
#[test]
fn a_partial_inhabitant_of_an_impossible_index_proves_nothing() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{False};
        induct Pin : (n : Nat) -> pub Type
        | only() : (0)
        end
        let spin(n : Nat) -> Pin(n) = spin(n);
        let bad(n : Nat, pin : Pin(n + 1)) -> False =
            match pin : (i, p) => False
            end;
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("bad(n : Nat, pin : Pin(n + 1))", "bad(n : Nat)"),
        ("match pin :", "match spin(n + 1) :"),
    ])
    .by_elaborator_in("/bad", |error| {
        matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
    })
    .by_kernel_in("/bad", |error| {
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

        let g(i: W, s: S(i)) -> False =
            h(i)
        and h(w: W) -> False =
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
    .by_kernel_in("/g", |error| {
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
        pub let below(n: Nat) -> True =
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
