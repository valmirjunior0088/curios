//! A projection in size-change, put to the certifier alone as Core text no elaborator writes: a field applied is read no further, each claim beside the programs `curios`'s `tests::rules::totality_projection` puts to both checkers.

use {super::test_support::*, crate::*, curios_common::fill};

/// The payload of the 2026-10-08 ticket held in a structure's field: `/Fn`, declared at `Prop` level zero, holds a function over every proposition, `/Bad` holds an `/Fn`, and `bad` recurs on what the field returns of the scrutinee at `/Bad`, which at the identity is the scrutinee; closed with that recursion.
const A_FIELD_OVER_EVERY_PROPOSITION: &str = r#"
struct /Fn: pub Prop {
    f: (A: Prop) -> (x: A) -> A,
}

induct /Bad: pub Prop
| c(r: #{node = StructType} /Fn())
end

#{node = Rec}
(let bad: (b: #{node = InductType} /Bad()) -> /std/Bool/False =
    (b2: #{node = InductType} /Bad()) =>
        #{goal = /std/Bool/False}
        match b2 | c(r) => bad((#{field = Struct(/Fn, 0)} r.0)(#{node = InductType} /Bad())(b2)) end;
bad(#{node = Variant} /Bad/c(/Fn { (A: Prop) => (x: A) => x })))
"#;

/// A field applied is read no further, a record being no family whose function-typed fields sizing keeps below it, so the recursion does not descend even with the structure judged as it is declared; and the structure is sized above the propositions its field ranges over, so `/Fn` is refused where it is declared and `/Bad`, holding it, is none of them.
#[test]
fn a_field_applied_at_its_holders_family_is_not_below_the_holder() {
    let verdicts = core_text_refusals(&fill!(
        "${PROPOSITIONS}\n${A_FIELD_OVER_EVERY_PROPOSITION}",
        PROPOSITIONS,
        A_FIELD_OVER_EVERY_PROPOSITION
    ));

    assert!(
        refuses(&verdicts, Some("Fn"), |error| matches!(
            error,
            Error::Oversized { .. }
        )),
        "a structure whose field ranges over the propositions it is one of was admitted: {verdicts:?}",
    );
    assert!(
        refuses(&verdicts, None, |error| matches!(
            error,
            Error::NotDescending { .. }
        )),
        "a recursion on what a field applied returns was graded below the record holding it: {verdicts:?}",
    );
}
