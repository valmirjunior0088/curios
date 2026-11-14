//! What the certifier states of a definition it judges: a proof, which unfolds to nothing, and the sort a type-valued definition's value inhabits, each by its own reading.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_common::{Qualifier, fill},
    curios_core::{DefinitionKind, Global, UniverseContext, Universes},
};

/// A definition written at a proposition is stated a proof, checked as any other and brought into scope with nothing to unfold to: its body is its own check's to read, and no judgment's after it. One the compiler generated at the same statement is stated none, and unfolds.
#[test]
fn a_definition_written_at_a_proposition_is_a_proof_and_unfolds_to_nothing() {
    for (kind, proof) in [
        (DefinitionKind::Authored, true),
        (DefinitionKind::Witness, false),
    ] {
        let mut certifier = certifier();
        family(&mut certifier, "Prop", "{}");
        let statement = fam_type();
        let body = term("#{node = Variant} /Fam/mk(())");

        let stated = check_definition(
            &mut certifier,
            &lemma(),
            &kind,
            &statement,
            &body,
            &UniverseContext::empty(),
        )
        .expect("the definition checks");
        assert_eq!(stated.proof, proof, "{kind:?}");
        assert_eq!(
            certifier.value_at(&lemma(), &Universes::default()),
            (!proof).then_some(&body)
        );
    }
}

/// A definition at a type that is no proposition is stated no proof: a number that unfolded to nothing would be one no judgment could compute with, so `2 : Nat` unfolds.
#[test]
fn a_definition_at_no_proposition_is_no_proof() {
    let mut certifier = certifier();
    let two = term("2");

    let stated = check_definition(
        &mut certifier,
        &lemma(),
        &DefinitionKind::Authored,
        &term("#NatType"),
        &two,
        &UniverseContext::empty(),
    )
    .expect("the definition checks");
    assert!(!stated.proof);
    assert_eq!(
        certifier.value_at(&lemma(), &Universes::default()),
        Some(&two)
    );
}

/// A definition whose type is written as a sort is stated the sort its value inhabits, its level among it: `Nat` inhabits `Type 0`, and the value `Type 0` inhabits `Type 1`. The level is read with nothing run: a list's sort is its element's as a lookup classifies it, the lookup reads the stated sort, and `List(lemma)` sits at `Type 1` over elements that are the small types themselves.
#[test]
fn a_type_valued_definition_is_stated_the_sort_its_value_inhabits() {
    let at = |certifier: &mut Certifier, type_: &str, value: &str| {
        check_definition(
            certifier,
            &lemma(),
            &DefinitionKind::Authored,
            &term(type_),
            &term(value),
            &UniverseContext::empty(),
        )
        .expect("the definition checks")
        .value_sort
    };

    assert_eq!(at(&mut certifier(), "Type", "#NatType"), Some(term("Type")));

    let mut certifier = certifier();
    assert_eq!(
        at(&mut certifier, "#{level = 1} Type", "Type"),
        Some(term("#{level = 1} Type"))
    );
    assert_eq!(
        infer(&mut certifier, &term("#ListType(/lemma)")),
        Ok(term("#{level = 1} Type")),
    );
}

/// A statement that reduces to a proposition is typed at `Prop` as written, and its proof is one: the mark is read off the sort the statement is typed at, with the statement unreduced. `((T: Prop) => T)(Fam)` is typed at `Prop` and is `Fam`. The same redex over `Type` is no statement of a proposition at all: no proposition stands where a type is wanted.
#[test]
fn a_definition_at_a_statement_that_reduces_to_a_proposition_is_a_proof() {
    let mut certifier = certifier();
    family(&mut certifier, "Prop", "{}");
    let over = |sort: &str| {
        term(&fill!(
            "((T: ${sort}) => T)(#{node = InductType} /Fam())",
            sort
        ))
    };
    let body = term("#{node = Variant} /Fam/mk(())");
    assert!(matches!(
        check_definition(
            &mut certifier,
            &Global::Authored(Qualifier::from(["at_a_type"])),
            &DefinitionKind::Authored,
            &over("Type"),
            &body,
            &UniverseContext::empty(),
        ),
        Err(Error::Mismatch { .. }),
    ));

    let stated = check_definition(
        &mut certifier,
        &lemma(),
        &DefinitionKind::Authored,
        &over("Prop"),
        &body,
        &UniverseContext::empty(),
    )
    .expect("the definition checks");
    assert!(stated.proof);
    assert_eq!(certifier.value_at(&lemma(), &Universes::default()), None);
}
