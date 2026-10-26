//! A definition's mark and its stated sort: a proof unfolding to nothing, and a value sort held to the sort and level its value inhabits.

use {
    super::test_support::*,
    crate::{Error, Kernel, check_definition, infer},
    curios_core::{Free, Intrinsic, Level, Nat, Term, UniverseContext},
};

/// A definition marked a proof is checked as any other and brought into scope with nothing to unfold to: its body is its own check's to read, and no judgment's after it. Left unmarked, the same definition unfolds.
#[test]
fn a_definition_marked_a_proof_unfolds_to_nothing() {
    for proof in [true, false] {
        let mut kernel = kernel();
        family(&mut kernel, Term::prop(), Term::tuple_type_unit());
        let statement = Term::induct_type(fam(), Vec::<Term>::new(), Vec::<Term>::new());
        let body = Term::variant(
            fam(),
            Vec::<Term>::new(),
            "mk",
            [Term::tuple(Vec::<Term>::new())],
        );

        assert_eq!(
            check_definition(
                &mut kernel,
                &lemma(),
                &statement,
                &body,
                proof,
                None,
                &UniverseContext::empty(),
            ),
            Ok(()),
        );
        assert_eq!(kernel.value(&lemma()), (!proof).then_some(&body));
    }
}

/// The mark is held to the sort of the type it is stated at. A number that unfolded to nothing would be one no judgment could compute with, so a definition marked a proof at `Nat` is refused.
#[test]
fn a_definition_marked_a_proof_at_no_proposition_is_refused() {
    let mut kernel = kernel();

    assert!(matches!(
        check_definition(
            &mut kernel,
            &lemma(),
            &Term::intrinsic(Intrinsic::NatType),
            &Term::intrinsic(Intrinsic::Nat(Nat::new(2usize))),
            true,
            None,
            &UniverseContext::empty(),
        ),
        Err(Error::NoProof(_)),
    ));
}

/// The sort a definition states its value inhabits is held to the sort the value has: `Nat` stated a proposition is refused, and stated at `Type` admitted, so no judgment reads a sort off the name that its reduct denies.
#[test]
fn a_definition_stating_its_value_at_a_sort_it_does_not_inhabit_is_refused() {
    let at = |stated: Term| {
        check_definition(
            &mut kernel(),
            &lemma(),
            &Term::type_ground(),
            &Term::intrinsic(Intrinsic::NatType),
            false,
            Some(&stated),
            &UniverseContext::empty(),
        )
    };

    assert!(matches!(at(Term::prop()), Err(Error::ValueSort { .. })));
    assert_eq!(at(Term::type_ground()), Ok(()));
}

/// The sort a definition states its value inhabits is held to its level as well as to its quality: the value `Type 0` inhabits `Type 1`, and stated at `Type 0` it is refused. The level is read with nothing run: a list's sort is its element's as a lookup classifies it, the lookup reads the stated sort, and stated a level low `List(lemma)` would sit at `Type 0` over elements that are the small types themselves, a universe holding a code for itself. Stated at `Type 1`, the list sits there.
///
/// Mutation-checked: with `hold_value_sort` comparing the two sorts' qualities alone, `Type 0` stated is admitted.
#[test]
fn a_definition_stating_its_value_a_level_below_the_one_it_inhabits_is_refused() {
    let one = Level::zero().succ().expect("level zero has a successor");
    let at = |kernel: &mut Kernel, stated: Term| {
        check_definition(
            kernel,
            &lemma(),
            &Term::type_at(one.clone()),
            &Term::type_ground(),
            false,
            Some(&stated),
            &UniverseContext::empty(),
        )
    };

    assert!(matches!(
        at(&mut kernel(), Term::type_ground()),
        Err(Error::ValueSort { .. })
    ));

    let mut kernel = kernel();
    assert_eq!(at(&mut kernel, Term::type_at(one.clone())), Ok(()));
    assert_eq!(
        infer(
            &mut kernel,
            &Term::intrinsic(Intrinsic::list_type(Term::free_var(&lemma())))
        ),
        Ok(Term::type_at(one)),
    );
}

/// A statement that reduces to a proposition is typed at `Prop` as written, and its proof is one: the mark is read off the sort the statement is typed at, with the statement unreduced. `((T: Prop) => T)(Fam)` is typed at `Prop` and is `Fam`. The same redex over `Type` is no statement of a proposition at all: no proposition stands where a type is wanted.
#[test]
fn a_definition_marked_a_proof_at_a_statement_that_reduces_to_a_proposition_is_admitted() {
    let mut kernel = kernel();
    family(&mut kernel, Term::prop(), Term::tuple_type_unit());
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
    assert!(matches!(
        check_definition(
            &mut kernel,
            &Free::local(2, Some("at_a_type")),
            &over(Term::type_ground()),
            &Term::variant(
                fam(),
                Vec::<Term>::new(),
                "mk",
                [Term::tuple(Vec::<Term>::new())],
            ),
            true,
            None,
            &UniverseContext::empty(),
        ),
        Err(Error::Mismatch { .. }),
    ));
    let statement = Term::apply(
        Term::func([(t, Term::prop())], Term::free_var(&t)),
        [Term::induct_type(
            fam(),
            Vec::<Term>::new(),
            Vec::<Term>::new(),
        )],
    );
    let body = Term::variant(
        fam(),
        Vec::<Term>::new(),
        "mk",
        [Term::tuple(Vec::<Term>::new())],
    );

    assert_eq!(
        check_definition(
            &mut kernel,
            &lemma(),
            &statement,
            &body,
            true,
            None,
            &UniverseContext::empty(),
        ),
        Ok(()),
    );
    assert_eq!(kernel.value(&lemma()), None);
}
