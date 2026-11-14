//! An inductive declaration: its payloads sized against the family's level, the self-applications the known paradoxes need, and a uniform parameter's slack.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_common::Qualifier,
    curios_core::Global,
};

/// `induct Bad : Type 0 | mk(x : Type 0)` contains the universe it lives in. This is the fixture no surface program can spell — levels have no syntax — and the clause the item walk cannot check, because it computes each signature's sort and compares it to nothing.
#[test]
fn a_payload_at_the_familys_own_level_is_refused() {
    let mut certifier = certifier();
    let declaration = family(&mut certifier, "Type", "Type");

    assert!(matches!(
        check_induct_decl(&mut certifier, &declaration),
        Err(Error::Oversized { .. }),
    ));
}

/// `Box : Type 1 | mk(x : Type 0)` is the stratification working: the payload sorts one level below the family.
#[test]
fn a_payload_below_the_familys_level_is_admitted() {
    let mut certifier = certifier();
    let declaration = family(&mut certifier, "#{level = 1} Type", "Type");

    assert_eq!(check_induct_decl(&mut certifier, &declaration), Ok(()));
}

/// A `Prop`-sorted family is sized as a `Type`-sorted one is: `induct Box : Prop | mk(x : Type 0)` holds a payload a level above it. Left unsized, a payload quantifying over the sort its family sits in is applied at the family, and the application reads as a descent.
#[test]
fn a_proposition_family_is_sized_as_a_type_family_is() {
    let mut certifier = certifier();
    let declaration = family(&mut certifier, "Prop", "Type");

    assert!(matches!(
        check_induct_decl(&mut certifier, &declaration),
        Err(Error::Oversized { .. }),
    ));
}

/// `Box : Prop 1 | mk(x : Type 0)`: the payload fits under a proposition at its own level.
#[test]
fn a_payload_below_a_propositions_level_is_admitted() {
    let mut certifier = certifier();
    let declaration = family(&mut certifier, "#{level = 1} Prop", "Type");

    assert_eq!(check_induct_decl(&mut certifier, &declaration), Ok(()));
}

/// A proof counts at its proposition's level, in a data family as in a proposition: `induct Box : Type 0 | mk(x : (A : Prop) -> A)` holds a proof of a proposition one level up. Exempt, a data family squashes a large proposition into the sort it ranges over.
#[test]
fn a_proof_payload_counts_at_its_propositions_level() {
    let mut certifier = certifier();
    let declaration = family(&mut certifier, "Type", LARGE_PROPOSITION);

    assert!(matches!(
        check_induct_decl(&mut certifier, &declaration),
        Err(Error::Oversized { .. }),
    ));
}

/// The control: the same proof fits under a family one level up.
#[test]
fn a_proof_payload_below_the_familys_level_is_admitted() {
    let mut certifier = certifier();
    let declaration = family(&mut certifier, "#{level = 1} Type", LARGE_PROPOSITION);

    assert_eq!(check_induct_decl(&mut certifier, &declaration), Ok(()));
}

/// An intrinsic carrier of propositions is data at their level, and a list of the ground propositions themselves sits where they do, a level above them: `induct Box : Prop | mk(x : List(Prop))` holds a code for every proposition of its own level, the carrier Coquand's paradox of trees needs, and is refused as `mk(x : Prop)` is. `Box : Prop 1` holds it, which is the control.
///
/// Mutation-checked at `sort_of_intrinsic`: a carrier pinned at level zero admits the small proposition.
#[test]
fn a_proposition_holding_a_list_of_its_own_propositions_is_refused() {
    let codes = "#ListType(Prop)";

    let mut certifier = certifier();
    let declaration = family(&mut certifier, "Prop", codes);
    assert!(matches!(
        check_induct_decl(&mut certifier, &declaration),
        Err(Error::Oversized { .. }),
    ));

    let mut certifier = self::certifier();
    let declaration = family(&mut certifier, "#{level = 1} Prop", codes);
    assert_eq!(check_induct_decl(&mut certifier, &declaration), Ok(()));
}

/// Russell's paradox through a large inductive, the certifier's reading of the clash the elaborator refuses at `R: V = V/set(Regular, …)`. `V : Type 1 | set(A : Type 0, f : (A) -> V)` is a set of the values of any `Type 0`, a valid declaration since the payload fits below it; and `V/set(V, …)`, the set whose members are `V`s, hands `V` itself — a `Type 1` — to the `A : Type 0` field, which asks a type to sit strictly below itself. The declaration stands and the self-application is refused, where `curios`'s `tests::universes::hierarchy_tests` puts the paradox's own `R` to the elaborator.
#[test]
fn a_large_inductive_holding_its_own_type_is_refused_at_the_self_application() {
    let mut certifier = certifier();
    let declaration = family_holding_a_type(&mut certifier, "#{level = 1} Type", "Type");

    assert_eq!(check_induct_decl(&mut certifier, &declaration), Ok(()));
    assert!(matches!(
        infer(&mut certifier, &term(HOLDING_ITSELF)),
        Err(Error::Mismatch { .. }),
    ));
}

/// Coquand's paradox of trees, the same clash one sort down, which the elaborator refuses at `t: T = T/node(T, …)`. `T : Prop 1 | node(A : Prop 0, f : (A) -> T)` is a tree branching over any proposition, a valid declaration; and `T/node(T, …)` hands `T` itself — a `Prop 1` — to the `A : Prop 0` field, asking a proposition to sit strictly below itself. The declaration stands and the self-application is refused, as its `Type` twin above is.
#[test]
fn a_tree_over_every_proposition_is_refused_at_the_self_application() {
    let mut certifier = certifier();
    let declaration = family_holding_a_type(&mut certifier, "#{level = 1} Prop", "Prop");

    assert_eq!(check_induct_decl(&mut certifier, &declaration), Ok(()));
    assert!(matches!(
        infer(&mut certifier, &term(HOLDING_ITSELF)),
        Err(Error::Mismatch { .. }),
    ));
}

/// Hurkens' impredicative pair, the certifier's reading of the clash the elaborator refuses at `Packed: Prop = {P: Prop, p: P}`. A strong sum of a proposition and its proof holds a `Prop`, so it sits a level above the propositions it ranges over: the certifier sizes `{P: Prop, p: P}` as a `Type`, never a `Prop`, so the carrier Girard's paradox needs one sort down is none of the propositions it would pack. `carries_information` says so, which is what refuses the definition where its sort is stated `Prop`.
#[test]
fn an_impredicative_pair_of_a_proposition_and_its_proof_is_a_type() {
    let mut certifier = certifier();
    let pair = term("{P: Prop, p: P}");

    assert!(matches!(Sort::of(&mut certifier, &pair), Ok(Sort::Type(_))));
}

/// A uniform parameter gets one rung of slack: a family at `Type 0` may take a `T : Type` parameter — the parameter's domain sorts at `1 ≤ 0 + 1` — while the same domain as a *payload* is refused above.
#[test]
fn a_uniform_parameter_has_one_rung_of_slack() {
    let mut certifier = certifier();
    declared(
        &mut certifier,
        "induct /Vec(T: Type): pub Type\n| mk(x: T)\nend",
    );
    let declaration = certifier
        .induct_decl(&Global::Authored(Qualifier::from(["Vec"])))
        .expect("the family is declared")
        .clone();

    assert_eq!(check_induct_decl(&mut certifier, &declaration), Ok(()));
}
