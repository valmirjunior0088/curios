//! The large-elimination guard: a singleton whose index pins its payload eliminates into a type, and every other proposition with a constructor eliminates into a proposition alone.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_common::fill,
    curios_core::Term,
};

/// The elimination of a scrutinee at `${family}(0)` into `goal` by `arms`, opened in `certifier`, where the family is declared.
fn at_zero(certifier: &mut Certifier, family: &str, goal: &str, arms: &[&str]) -> Term {
    let [matched] = under(
        certifier,
        &fill!("subject: ${at}", at = family_at(family, "0")),
        &fill!("${matched},", matched = eliminating(goal, arms)),
    );

    matched
}

/// A proposition whose single constructor's payload is *pinned* by its index target — the `Eq`/`refl` shape. Matching `(z)` against a value recovers `z`, so eliminating tells a program nothing it did not already know, and the large elimination is admitted, Without this, `Eq/subst` is unstatable.
#[test]
fn a_singleton_whose_index_pins_its_payload_eliminates_into_a_type() {
    let mut certifier = certifier();
    declare(
        &mut certifier,
        "Pinned",
        "i: #NatType",
        "Prop",
        &["refl(z: #NatType): (z)"],
    );
    let matched = at_zero(&mut certifier, "Pinned", "#NatType", &["refl(z) => z"]);

    assert_eq!(infer(&mut certifier, &matched), Ok(term("#NatType")));
}

/// The same shape with a *non-injective* index target. `blur(a)` mentions `a`, but knowing `blur(a)` recovers nothing — `blur` need not be injective, and in the program this rule exists for it is the constant zero. So the payload is not pinned, the proposition carries something a program could read back, and eliminating it into a relevant type is refused.
///
/// Reading occurrence as determination is exactly the defect this rule exists to avoid: it admits this elimination, and a closed inhabitant of `False` follows from it.
#[test]
fn a_singleton_whose_index_merely_mentions_its_payload_does_not() {
    let mut certifier = certifier();
    opaque(&mut certifier, "blur", "(n: #NatType) -> #NatType");
    declare(
        &mut certifier,
        "Loose",
        "i: #NatType",
        "Prop",
        &["mk(a: #NatType): (/blur(a))"],
    );
    let matched = at_zero(&mut certifier, "Loose", "#NatType", &["mk(a) => a"]);

    assert_eq!(
        infer(&mut certifier, &matched),
        Err(Error::LargeElimination(named("Loose"))),
    );
}

/// The same shape with a payload that is *itself a type*. `mk(a : Type 0)` pins nothing — the index target is the constant `0` — so eliminating into `Type` hands a program the type the proposition was built with, while irrelevance says every inhabitant is the same one. `mk({})` and `mk(False)` are convertible at a `Prop`-sorted family, `refl` therefore proves `Eq()(mk({}), mk(False))`, congruence through this eliminator carries that to `Eq()({}, False)`, and `subst` turns `()` into a closed inhabitant of `False`.
///
/// A component whose *type is a universe* carries information however completely erasure deletes it: erasure governs what the runtime observes, while irrelevance is a claim about definitional equality, and conversion reads this component back in full. `curios-elab`'s `singleton_eliminable` requires each binder to be `Prop`-sorted or index-pinned and refuses every surface spelling, so only a hand-built derivation reaches this rule; `crate::recheck::proposition_tests::a_derivation_through_a_type_carrying_proposition_is_refused` holds the whole-module walk to it.
///
/// `a_singleton_whose_index_pins_its_payload_eliminates_into_a_type` is the control: a payload the targets do recover must stay eliminable, or `Eq/subst` becomes unstatable.
#[test]
fn a_singleton_carrying_a_type_does_not_eliminate_into_a_type() {
    let mut certifier = certifier();
    declare(
        &mut certifier,
        "TypeBox",
        "i: #NatType",
        "Prop",
        &["mk(a: Type): (0)"],
    );
    let matched = at_zero(&mut certifier, "TypeBox", "Type", &["mk(a) => a"]);

    assert_eq!(
        infer(&mut certifier, &matched),
        Err(Error::LargeElimination(named("TypeBox"))),
    );
}

/// The other half of the same clause: a payload whose type is `Prop` holds a proposition *as data* rather than proving one. `Sort::of(Prop)` is `Type 0`, so the component is informative for exactly the reason above — `mk(True)` and `mk(False)` are convertible at a `Prop`-sorted family while this eliminator tells them apart, and transporting a proof of `True` along the resulting equality inhabits `False`.
///
/// Worth its own fixture: a clause excusing universe-typed components would name a sort of either quality, and one that named `Type` alone would leave this open.
#[test]
fn a_singleton_carrying_a_proposition_does_not_eliminate_into_a_type() {
    let mut certifier = certifier();
    declare(
        &mut certifier,
        "PropBox",
        "i: #NatType",
        "Prop",
        &["mk(a: Prop): (0)"],
    );
    let matched = at_zero(&mut certifier, "PropBox", "Type", &["mk(a) => a"]);

    assert_eq!(
        infer(&mut certifier, &matched),
        Err(Error::LargeElimination(named("PropBox"))),
    );
}

/// A payload that is itself a proof is not excused. Irrelevance makes any two of them interchangeable, so nothing a program computes could tell which it was handed; but the arm is handed the proof the match was given, and a recursion along such a payload has a type reduce the lemma that built it. A proposition holding a proof no index pins is taken apart only into a proposition.
///
/// The distinction from the fixture above is what the payload is. There its type is `Prop` itself, so the payload *is* a proposition carried as data; here its type is a family declared at `Prop`, so the payload inhabits one. Neither is a payload an index pins, which is the one thing the guard asks.
#[test]
fn a_singleton_carrying_a_proof_does_not_eliminate_into_a_type() {
    let mut certifier = certifier();
    declare(
        &mut certifier,
        "Held",
        "i: #NatType",
        "Prop",
        &["qed(): (0)"],
    );
    declare(
        &mut certifier,
        "ProofBox",
        "i: #NatType",
        "Prop",
        &[&fill!("mk(p: ${held}): (0)", held = family_at("Held", "0"))],
    );
    let matched = at_zero(&mut certifier, "ProofBox", "#NatType", &["mk(p) => 0"]);

    assert_eq!(
        infer(&mut certifier, &matched),
        Err(Error::LargeElimination(named("ProofBox"))),
    );
}

/// An empty proposition eliminates into anything: there is nothing to have received, so nothing to extract.
#[test]
fn an_empty_proposition_eliminates_into_a_type() {
    let mut certifier = certifier();
    declare(&mut certifier, "Absurd", "i: #NatType", "Prop", &[]);
    let matched = at_zero(&mut certifier, "Absurd", "#NatType", &[]);

    assert_eq!(infer(&mut certifier, &matched), Ok(term("#NatType")));
}

/// `Two : (Nat) -> Prop | here : (0) | there : (0)`.
const TWO: [&str; 2] = ["here(): (0)", "there(): (0)"];

/// Two constructors mean the value says *which*, which is information a relevant result could branch on.
#[test]
fn a_proposition_with_two_constructors_does_not_eliminate_into_a_type() {
    let mut certifier = certifier();
    declare(&mut certifier, "Two", "i: #NatType", "Prop", &TWO);
    let matched = at_zero(
        &mut certifier,
        "Two",
        "#NatType",
        &["here() => 1", "there() => 2"],
    );

    assert_eq!(
        infer(&mut certifier, &matched),
        Err(Error::LargeElimination(named("Two"))),
    );
}

/// The guard is about *relevance*, not about propositions as such: eliminating a proposition into another proposition is always fine, since irrelevance makes the result indistinguishable either way.
#[test]
fn a_proposition_eliminates_into_a_proposition_however_many_constructors() {
    let mut certifier = certifier();
    declare(
        &mut certifier,
        "Target",
        "i: #NatType",
        "Prop",
        &["only(): (0)"],
    );
    declare(&mut certifier, "Two", "i: #NatType", "Prop", &TWO);
    let target = family_at("Target", "0");
    let proof = "#{node = Variant} /Target/only()";
    let matched = at_zero(
        &mut certifier,
        "Two",
        &target,
        &[
            &fill!("here() => ${proof}", proof),
            &fill!("there() => ${proof}", proof),
        ],
    );

    assert_eq!(infer(&mut certifier, &matched), Ok(term(&target)));
}
