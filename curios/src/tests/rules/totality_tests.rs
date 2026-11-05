//! A partial definition at an erased position, however it is reached, and what does not count as descent.

use {super::test_support::*, curios_analysis::Erased, curios_common::fill};

/// A partial definition behind a `Type`-sorted carrier, reached four ways, and a diverging proof at a position the judgment infers rather than checks: each is a proof resting on something that may not terminate, however it is reached.
#[test]
fn a_proof_resting_on_a_partial_definition_is_refused_however_it_is_reached() {
    for (source, at) in [
        (PARTIAL_DIRECT, "/bad"),
        (PARTIAL_THROUGH_WITNESS, "/bad"),
        (PARTIAL_HIGHER_ORDER, "/bad"),
        (PARTIAL_IN_FIELD, "/bad"),
        (INFERRED_PROOF_POSITION, "/conjured"),
    ] {
        refused(source)
            .by_elaborator_in(at, |error| {
                matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
            })
            .by_kernel_in(at, |error| {
                matches!(error, curios_cert::Error::NotTotal { .. })
            });
    }
}

#[test]
fn a_recursion_written_inline_under_a_carrier_is_still_partial() {
    refused(INLINE_REC_UNDER_CARRIER)
        .by_elaborator_in("/bad", |error| {
            matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
        })
        .by_kernel_in("/bad", |error| {
            matches!(error, curios_cert::Error::NotTotal { .. })
        });
}

#[test]
fn a_type_reaching_a_partial_definition_is_refused() {
    refused(TYPE_REACHING_PARTIAL)
        .by_elaborator_in("/Sized", |error| {
            matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
        })
        .by_kernel_in("/Sized", |error| {
            matches!(error, curios_cert::Error::NotTotal { .. })
        });
}

#[test]
fn saturating_subtraction_is_not_descent() {
    refused(SATURATING_SUBTRACTION_IS_NOT_DESCENT)
        .by_elaborator_in("/bogus", |error| {
            matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
        })
        .by_kernel_in("/bogus", |error| {
            matches!(error, curios_cert::Error::NotDescending { .. })
        });
}

#[test]
fn permuting_arguments_is_not_descent() {
    refused(PERMUTING_ARGUMENTS_IS_NOT_DESCENT)
        .by_elaborator_in("/bogus", |error| {
            matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
        })
        .by_kernel_in("/bogus", |error| {
            matches!(error, curios_cert::Error::NotDescending { .. })
        });
}

// A structure's field is read by projection, which takes no case of the value apart, so a function field applied off one grades below nothing: `f` would recur for ever along `l`'s `next`, a structure that hands itself back, and is refused as no descent wherever it is reached.
#[test]
fn a_field_applied_off_a_structure_is_not_descent() {
    refused(r#"
        use /std/Bool/{False};

        struct S: Type { next: () -> S }

        let l() -> S = S { next = l };

        let f(x: S) -> False = f(x.next());

        let bad: False = f(l());

        /std/print("FORGED")
        "#).by_elaborator_in("/f", |error| matches!(error, curios_elab::Error::PartialInErasedPosition { offender: Some(offender), .. } if *offender == global("/f")))
        .by_kernel_in("/f", |error| matches!(error, curios_cert::Error::NotDescending { .. }));
}

// An application of a payload is a child of its constructor, which is true of a value that is a finite tree (`curios-analysis`'s `totality`), the premise the paradox of trees breaks: `down` descends by it and is admitted, and `x`, whose payload hands back `x` itself, is a tree with no bottom for the descent to reach. A recursive binding is not known to terminate, so the proof that reaches it is refused at `x`, and the descent is never asked of it.
#[test]
fn a_value_that_is_its_own_child_is_not_known_to_terminate() {
    refused(r#"
        use /std/Bool/{False};

        induct N: Type
        | s(p: () -> N)
        end

        let down(n: N) -> False = match n | s(p) => down(p()) end;

        let x: N = N/s(() => x);

        let bad: False = down(x);

        /std/print("FORGED")
        "#).by_elaborator_in("/bad", |error| matches!(error, curios_elab::Error::PartialInErasedPosition { offender: Some(offender), .. } if *offender == global("/x")))
        .by_kernel_in("/bad", |error| matches!(error, curios_cert::Error::NotTotal { .. }));
}

// The size order reads an arm's case for a `Prop`-sorted scrutinee as for any other (`refined_by_arm`), so a recursion descends on an inductive proposition's constructors, and a proof irrelevance holds equal to the scrutinee is then graded below it. What stands between that reading and a closed inhabitant of `False` is coverage, and this is the shape that asks for it: `Q`'s index is a proof, so irrelevance makes `Q(v)` and `Q(Wit/s(v))` one type and the recursive call on the payload is well typed, and `Q/nil()` inhabits `Q(Wit/s(Wit/triv()))` for the same reason — a proof of `Q` at the scrutinee's own type whose payload the descent reads as smaller.
//
// `drain` has no base case, so the forgery needs `nil`'s arm absent, and a proposition's two constructors never clash (`curios-analysis`'s `invert`, whose tag test fires only at a relevant family). So the arm is mandatory and has to inhabit `False`. The refusal is coverage's, not the size order's: a change that let a `Prop`-valued index excuse an omitted arm would hand this program the descent it already has.
#[test]
fn a_recursion_on_a_propositions_case_still_needs_the_arm_it_omits() {
    refused(
        r#"
        use /std/Bool/{False};

        induct Wit: Prop
        | triv()
        | s(inner: Wit)
        end

        induct Q: (Wit) -> Prop
        | nil(): (Wit/triv())
        | cons(v: Wit, inner: Q(v)): (Wit/s(v))
        end

        let drain(v: Wit, q: Q(Wit/s(v))) -> False =
            match q: (_, _) => False
            | cons(w, inner) => drain(w, inner)
            end;

        let bad: False = drain(Wit/triv(), Q/nil());

        /std/print("FORGED")
        "#,
    )
    .by_elaborator_in("/drain", |error| {
        matches!(
            error,
            curios_elab::Error::MatrixUncovered { indexed: true, .. }
        )
    });
}

/// A recursion on a proposition's own case, every arm recursing, at an index holding a proof: `P/base()` stands at `step`'s target by irrelevance, so `_bad` is a closed term at `False` were `base`'s arm excused. The holes are the declarations the index needs, the index's type, and `base`'s and `step`'s targets.
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

    let _bad: False = f(P/base());

    /std/print("FORGED")
    "#;

// `a_recursion_on_a_propositions_case_still_needs_the_arm_it_omits` at every shape a program can carry the proof in: a relevant family holding one, a list and a tuple of them, a type family over a proposition, a proposition above level zero, and one declared in a group. The size order reads `step(q)` as a descent in each, and coverage keeps `base`'s arm, since no clash is concluded between two of a proposition's constructors wherever in an index they meet. `curios-cert`'s `recheck::case_tests` puts the shapes to the kernel under this name, a stated instance and a type a definition spells among them.
#[test]
fn no_index_holding_a_proof_excuses_the_arm_a_recursion_on_a_propositions_case_omits() {
    let wit = r#"
    induct Wit: Prop
    | a()
    | b()
    end
    "#;
    let boxed = r#"
    induct Wit: Prop
    | a()
    | b()
    end

    induct Box: Type
    | box(w: Wit)
    end
    "#;
    let optional = r#"
    induct Wit: Prop
    | a()
    | b()
    end

    induct Opt(A: Prop): Type
    | none()
    | some(x: A)
    end
    "#;
    let high = r#"
    induct High: Prop
    | ha(f: (A: Prop) -> (A) -> A)
    | hb()
    end
    "#;
    let grouped = r#"
    induct G: Prop
    | ga()
    | gb(x: G2)
    and G2: Prop
    | c(y: G)
    | d()
    end
    "#;

    for (declarations, index, base, step) in [
        (boxed, "Box", "Box/box(Wit/a())", "Box/box(Wit/b())"),
        (wit, "/std/List(Wit)", "[Wit/a()]", "[Wit/b()]"),
        (
            wit,
            "{Wit, Wit}",
            "(Wit/a(), Wit/a())",
            "(Wit/b(), Wit/b())",
        ),
        (
            optional,
            "Opt(Wit)",
            "Opt/some(Wit/a())",
            "Opt/some(Wit/b())",
        ),
        (high, "High", "High/ha((_) => (x) => x)", "High/hb()"),
        (grouped, "G", "G/ga()", "G/gb(G2/d())"),
    ] {
        refused(&fill!(
            A_RECURSION_AT_AN_INDEX_HOLDING_A_PROOF,
            declarations,
            index,
            base,
            step
        ))
        .by_elaborator_in("/f", |error| {
            matches!(
                error,
                curios_elab::Error::MatrixUncovered { indexed: true, .. }
            )
        });
    }
}

// `S` is indexed by a proof of `W` and `W` holds an `S`, so a match on an `S` proof fires with the proof unreduced and reads `v` off the index: `v` is `i`, no part of `s`, and the size order grades it below `s`. That reading is sound, a proof of `S(i)` ranking above `i` at its least derivation; what refuses the loop is that `S/mk(w)`, built around `h`'s own case, is nothing at or below `w`, so `h(base)`, which is `h(base)` for ever, starts no descending thread. `curios-cert`'s `recheck::recursion_tests` puts the module to the kernel under this name.
#[test]
fn a_payload_a_cast_reads_off_a_proofs_index_is_no_descent() {
    refused(
        r#"
        use /std/Bool/{False};

        induct W: pub Prop
        | base()
        | wrap(x: W, s: S(x))
        and S: (W) -> pub Prop
        | mk(v: W): (v)
        end

        let g(i: W, s: S(i)) -> False =
            match s
            | mk(v) => h(v)
            end
        and h(w: W) -> False =
            match w
            | base() => g(w, S/mk(w))
            | wrap(x, s2) => g(x, s2)
            end;

        let _bad: False = h(W/base());

        /std/print("FORGED")
        "#,
    )
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

// The one way a proof reaches a type is the cast, and a recursion graded below the proof it casts along must still descend to stand at (T): `m` is below `p`, `S/mk(m)` is nothing below `p`, and `m` stands for `n` only by the arm's index solution, which refines for typing alone. `curios-cert`'s `recheck::recursion_tests` puts the module to the kernel under this name.
#[test]
fn a_type_recursing_through_a_cast_on_the_index_it_reads_does_not_descend() {
    refused(
        r#"
        use /std/{Nat};

        induct S: (Nat) -> Prop
        | mk(n: Nat): (n)
        end

        let X(n: Nat, p: S(n)) -> Type =
            match p
            | mk(m) => X(m, S/mk(m))
            end;

        /std/print("unreachable")
        "#,
    )
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

// The field's accessibility routes all need a proof of accessibility where none exists, and `spin` is that recursion's honest half: over a reflexive relation nothing is accessible, `spin` descends through `below(w, qed)`, an application of `intro`'s payload, and so proves accessibility empty there. The proof it is handed is built by a recursion that hands itself back, which descends on nothing and is refused at the proof it stands at.
#[test]
fn an_accessibility_proof_built_by_self_reference_is_refused() {
    refused(
        r#"
        use /std/Bool/{True, False};

        induct Acc(@A: Type, R: (A, A) -> Prop): (A) -> pub Prop
        | intro(@x: A, below: (y: A, r: R(y, x)) -> Acc(R)(y)): (x)
        end

        let spin(x: {}, a: Acc((p: {}, q: {}) => True)(x)) -> False =
            match a
            | intro(@w, below) => spin(w, below(w, True/qed()))
            end;

        let forever(x: {}) -> Acc((p: {}, q: {}) => True)(x) =
            Acc/intro((y, r) => forever(y));

        let _bad: False = spin((), forever(()));

        /std/print("FORGED")
        "#,
    )
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

/// A proof standing on a partial term its elimination reads nothing of is refused too.
#[test]
fn a_partial_inhabitant_of_an_impossible_index_proves_nothing() {
    refused(PARTIAL_UNDER_AN_EMPTY_ELIMINATION)
        .by_elaborator_in("/bad", |error| {
            matches!(error, curios_elab::Error::PartialInErasedPosition { .. })
        })
        .by_kernel_in("/bad", |error| {
            matches!(error, curios_cert::Error::NotTotal { .. })
        });
}
