//! What the certifier states of a definition it judges: a proof, which unfolds to nothing, and the sort a type-valued definition's value inhabits, each by its own reading.

use {
    super::test_support::*,
    crate::*,
    curios_common::Qualifier,
    curios_core::{
        DefinitionKind, Free, Global, Intrinsic, Level, Nat, Term, UniverseContext, Universes,
    },
};

/// A definition written at a proposition is stated a proof, checked as any other and brought into scope with nothing to unfold to: its body is its own check's to read, and no judgment's after it. One the compiler generated at the same statement is stated none, and unfolds.
#[test]
fn a_definition_written_at_a_proposition_is_a_proof_and_unfolds_to_nothing() {
    for (kind, proof) in [
        (DefinitionKind::Authored, true),
        (DefinitionKind::Witness, false),
    ] {
        let mut certifier = certifier();
        family(&mut certifier, Term::prop(), Term::tuple_type_unit());
        let statement = Term::induct_type(fam(), Vec::<Term>::new(), Vec::<Term>::new());
        let body = Term::variant(
            fam(),
            Vec::<Term>::new(),
            "mk",
            [Term::tuple(Vec::<Term>::new())],
        );

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
    let two = Term::intrinsic(Intrinsic::Nat(Nat::new(2usize)));

    let stated = check_definition(
        &mut certifier,
        &lemma(),
        &DefinitionKind::Authored,
        &Term::intrinsic(Intrinsic::NatType),
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
    let one = Level::zero().succ().expect("level zero has a successor");
    let at = |certifier: &mut Certifier, type_: Term, value: Term| {
        check_definition(
            certifier,
            &lemma(),
            &DefinitionKind::Authored,
            &type_,
            &value,
            &UniverseContext::empty(),
        )
        .expect("the definition checks")
        .value_sort
    };

    assert_eq!(
        at(
            &mut certifier(),
            Term::type_ground(),
            Term::intrinsic(Intrinsic::NatType)
        ),
        Some(Term::type_ground())
    );

    let mut certifier = certifier();
    assert_eq!(
        at(
            &mut certifier,
            Term::type_at(one.clone()),
            Term::type_ground()
        ),
        Some(Term::type_at(one.clone()))
    );
    assert_eq!(
        infer(
            &mut certifier,
            &Term::intrinsic(Intrinsic::list_type(Term::instance(
                lemma(),
                Universes::default()
            )))
        ),
        Ok(Term::type_at(one)),
    );
}

/// A statement that reduces to a proposition is typed at `Prop` as written, and its proof is one: the mark is read off the sort the statement is typed at, with the statement unreduced. `((T: Prop) => T)(Fam)` is typed at `Prop` and is `Fam`. The same redex over `Type` is no statement of a proposition at all: no proposition stands where a type is wanted.
#[test]
fn a_definition_at_a_statement_that_reduces_to_a_proposition_is_a_proof() {
    let mut certifier = certifier();
    family(&mut certifier, Term::prop(), Term::tuple_type_unit());
    let t = Free::local(1, Some("T"));
    let over = |sort: Term| {
        Term::apply(
            Term::func([(t, sort)], Term::free_var(&t)),
            [Term::induct_type(
                fam(),
                Vec::<Term>::new(),
                Vec::<Term>::new(),
            )],
        )
    };
    let body = Term::variant(
        fam(),
        Vec::<Term>::new(),
        "mk",
        [Term::tuple(Vec::<Term>::new())],
    );
    assert!(matches!(
        check_definition(
            &mut certifier,
            &Global::Authored(Qualifier::from(["at_a_type"])),
            &DefinitionKind::Authored,
            &over(Term::type_ground()),
            &body,
            &UniverseContext::empty(),
        ),
        Err(Error::Mismatch { .. }),
    ));

    let stated = check_definition(
        &mut certifier,
        &lemma(),
        &DefinitionKind::Authored,
        &over(Term::prop()),
        &body,
        &UniverseContext::empty(),
    )
    .expect("the definition checks");
    assert!(stated.proof);
    assert_eq!(certifier.value_at(&lemma(), &Universes::default()), None);
}
