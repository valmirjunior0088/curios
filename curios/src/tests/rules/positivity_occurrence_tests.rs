//! Where an occurrence stands under a declaration's own arrows: strictly positive, positive and not strictly, or negative, and an occurrence of another type, which an arrow leaves alone.

use {
    super::test_support::*, crate::tests::test_support::*, curios_core::Polarity,
    curios_elab::Error,
};

// The whole reason the gate exists. `Bad` is not the initial algebra of any functor — the payload is a function *out of* `Bad` — and admitting it hands back an eliminator that inhabits `False` in four lines with no recursion. The control's payload produces a `Bad`.
#[test]
fn a_negative_occurrence_is_rejected() {
    control(
        r#"
        use /std/Bool/{False};

        induct Bad : pub Type
        | c(f : (False) -> Bad)
        end

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("(False) -> Bad", "(Bad) -> False")])
    .by_elaborator_in("/Bad", |error| {
        matches!(
            error,
            Error::NotStrictlyPositive {
                polarity: Polarity::Neg,
                ..
            }
        )
    });
}

// `struct Waker : Type { () -> {} }` inside `/std/Pause`'s `park` payload: a genuine negative position, and sound only because `Waker` mentions neither member of the group. The check must reject on the *path back*, not on the mere presence of a negative occurrence.
#[test]
fn a_negative_occurrence_of_an_unrelated_type_is_admitted() {
    let source = r#"
        struct Waker : pub Type { () -> {} }

        induct Event : pub Type
        | idle()
        | park((Waker) -> {})
        end

        match Event/idle()
        | idle() => /std/print("unrelated")
        | park(_) => /std/print("no")
        end
        "#;
    assert_eq!(run(source), b"unrelated");
}

// Positive but not strictly positive: two arrows, so the sign flips back. This records `documentation/design/theory/strict-positivity-modulo-polarity.md`'s refusal of the merely positive as a test rather than as prose: a family that is its own double function space has no model as a set, so the merely-positive relaxation other systems allow is not available here. The control's payload holds `Bad2` right of both arrows.
#[test]
fn a_positive_but_not_strictly_positive_occurrence_is_rejected() {
    control(
        r#"
        use /std/Bool/{False};

        induct Bad2 : pub Type
        | c(f : ((False) -> False) -> Bad2)
        end

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("((False) -> False) -> Bad2", "((Bad2) -> False) -> False")])
    .by_elaborator_in("/Bad2", |error| {
        matches!(
            error,
            Error::NotStrictlyPositive {
                polarity: Polarity::Pos,
                ..
            }
        )
    });
}

// The rule most likely to be implemented wrongly. A nullary `() -> X` is an *empty* parameter list, so nothing flips and `X` stays strictly positive — a zero-argument function is a thunk of its result. Read as "an occurrence under an arrow is negative", this rejects every type that holds a thunk of itself.
#[test]
fn a_nullary_arrow_codomain_is_a_strictly_positive_payload() {
    let source = r#"
        use /std/{Nat};

        induct Susp(A : Type) : pub Type
        | now(A)
        | later(() -> Susp(A))
        end

        let delayed : Susp(Nat) = Susp/later(() => Susp/now(7));

        match delayed
        | now(_) => /std/print("no")
        | later(_) => /std/print("thunked")
        end
        "#;
    assert_eq!(run(source), b"thunked");
}

// A description of an `X` is the delayed `X` its thunk erasure makes it, so `X` under `Io` stays strictly positive — the same reading as `List`, and for a stronger reason: `bind` never hands back the result except inside another `Io`, and there is no eliminator that does. This is the rule a suspension whose continuation is computed by performing an effect rests on.
#[test]
fn an_io_payload_is_a_strictly_positive_occurrence() {
    let source = r#"
        use /std/{Nat, Io};

        induct Step(A : Type) : pub Type
        | done(A)
        | more(next : Io(Step(A)))
        end

        let machine : Step(Nat) = Step/more(Io/pure(Step/done(7)));

        match machine
        | done(_) => /std/print("no")
        | more(_) => /std/print("suspended")
        end
        "#;
    assert_eq!(run(source), b"suspended");
}

// A struct is checked on the same footing as an inductive: it is a nominal record, so a field that consumes the record it belongs to is the same unsoundness wearing different syntax. The control's field produces the record.
#[test]
fn a_negative_struct_field_is_rejected() {
    control(
        r#"
        use /std/Bool/{False};

        struct Consume : pub Type {
            run : (False) -> Consume,
        }

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[("(False) -> Consume", "(Consume) -> False")])
    .by_elaborator_in("/Consume", |error| {
        matches!(
            error,
            Error::NotStrictlyPositive {
                polarity: Polarity::Neg,
                ..
            }
        )
    });
}

// The other support the argument names, at a shape positivity's own probes do not spell: they run the negative and the double negative bare, through an `induct` parameter, through a `struct` parameter, through a type alias, under `List`, behind a type-level `match`, and at a higher-kinded parameter — never behind an anonymous Σ, which is the construct this row is about.
//
// The diagnostic is what makes this more than a repeat. It reads *positively, but not strictly* rather than *negatively*, which is the same verdict the bare spelling gets: the polarity lattice is computed through the tuple component rather than the component being answered opaquely, since an opaque answer would join to `Mixed` and refuse with the other message. Refusing a merely-`Pos` diagonal is precisely what keeps `℘℘` out, a family that is its own double function space having no model as a set, so this is the pairing the row rests on, checked where the row lives. The control holds `Bad` right of both arrows behind the same Σ.
#[test]
fn a_non_strict_occurrence_behind_a_record_is_still_refused() {
    control(
        r#"
        induct Bad : pub Type
        | mk(f : {((Prop) -> Prop) -> Bad})
        end

        /std/Io/pure(())
        "#,
        b"",
    )
    .refused(&[("((Prop) -> Prop) -> Bad", "((Bad) -> Prop) -> Prop")])
    .by_elaborator_in("/Bad", |error| {
        matches!(
            error,
            Error::NotStrictlyPositive {
                polarity: Polarity::Pos,
                ..
            }
        )
    });
}
