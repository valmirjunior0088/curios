//! Remembered sorts: a shared type classified once a node, and a sort that does not outlive the equations, arm, type or binder it was read under.

use {
    super::test_support::*,
    crate::*,
    curios_analysis::{Erased, test_support::*},
    curios_core::{Free, Intrinsic, Level, Nat, Term, UniverseContext, Universes},
};

/// A record of two fields at one type, `depth` deep: a graph of `depth + 1` nodes whose tree has `2^depth` fields.
fn doubled(base: Term, depth: usize) -> Term {
    (0..depth).fold(base, |type_, level| {
        let first = Free::local(2 * level as u64 + 100, None);
        let second = Free::local(2 * level as u64 + 101, None);

        Term::tuple_type([(first, type_.clone()), (second, type_)])
    })
}

/// A sort is remembered per type, so a record sixty levels deep whose two fields are one type each — a tree no walk per path finishes — is classified in the size of its graph, over a closed base and over a local alike. At a depth the uncached certifier affords, both give the same sort.
///
/// Mutation-checked: with no remembered sort answered, neither sixty-level row finishes.
#[test]
fn a_shared_type_is_classified_once_per_node() {
    let nat = Term::intrinsic(Intrinsic::NatType);
    assert_eq!(
        Sort::of(&mut certifier(), &doubled(nat.clone(), 60)),
        Ok(Sort::Type(Level::zero())),
    );

    let hypothesis = binder(0, "A");
    let mut over_a_local = certifier();
    over_a_local.assume(&hypothesis, &Term::type_ground());
    assert_eq!(
        Sort::of(&mut over_a_local, &doubled(Term::free_var(&hypothesis), 60)),
        Ok(Sort::Type(Level::zero())),
    );

    let mut uncached = Certifier::uncached(100_000, SYNTAX);
    assert_eq!(
        Sort::of(&mut certifier(), &doubled(nat.clone(), 8)),
        Sort::of(&mut uncached, &doubled(nat, 8)),
    );
}

/// A remembered sort lives as long as the equations it was taken under: a type whose universe is a scrutinee is a proposition in the arm that makes it `Prop`, data in the arm that makes it `Type`, and unclassified outside both — and the uncached certifier agrees on all three.
///
/// Mutation-checked: leaving the scoped sorts standing where an equation moves answers the second arm `Prop`.
#[test]
fn a_remembered_sort_does_not_outlive_the_equations_it_was_taken_under() {
    let sequence = |certifier: &mut Certifier| {
        let universe = binder(0, "u");
        let hypothesis = binder(1, "A");
        certifier.assume(
            &universe,
            &Term::type_at(Level::zero().succ().expect("one")),
        );
        certifier.assume(&hypothesis, &Term::free_var(&universe));
        let type_ = Term::free_var(&hypothesis);

        let under = |certifier: &mut Certifier, value: Term| {
            certifier.scoped(|certifier| {
                certifier
                    .refine(Term::free_var(&universe), value)
                    .expect("the equation records");
                Sort::of(certifier, &type_)
            })
        };

        [
            under(certifier, Term::prop()),
            under(certifier, Term::type_ground()),
            Sort::of(certifier, &type_),
        ]
    };

    let cached = sequence(&mut certifier());

    assert_eq!(cached[0], Ok(Sort::Prop(Level::zero())));
    assert_eq!(cached[1], Ok(Sort::Type(Level::zero())));
    assert!(matches!(cached[2], Err(Error::NotASort(_))));
    assert_eq!(cached, sequence(&mut Certifier::uncached(100_000, SYNTAX)));
}

/// What a position is recorded as lives as its type's sort does: a term at a type one arm's equation makes a proposition is a proof in that arm and nothing the obligations constrain in the arm that makes its type data, whichever is checked first.
///
/// Mutation-checked: leaving the remembered halves standing where an equation moves records the second arm's position as a proof too.
#[test]
fn a_position_is_classified_under_its_own_arm() {
    let mut certifier = certifier();
    let universe = binder(0, "u");
    let hypothesis = binder(1, "A");
    let inhabitant = binder(2, "a");
    certifier.assume(
        &universe,
        &Term::type_at(Level::zero().succ().expect("one")),
    );
    certifier.assume(&hypothesis, &Term::free_var(&universe));
    certifier.assume(&inhabitant, &Term::free_var(&hypothesis));

    let recorded = [Term::prop(), Term::type_ground()].map(|value| {
        certifier.scoped(|certifier| {
            certifier
                .refine(Term::free_var(&universe), value)
                .expect("the equation records");
            certifier.record_checked(&Term::free_var(&inhabitant), &Term::free_var(&hypothesis))
        })
    });
    let (positions, failure) = certifier.take_checked();

    assert_eq!(recorded, [Some(0), None]);
    assert_eq!(
        positions
            .iter()
            .map(|position| position.erased)
            .collect::<Vec<_>>(),
        [Erased::Proof],
    );
    assert!(failure.is_none());
}

/// Nor does it outlive the type a local stands at: an arm re-assumes a local at its specialized type, and a neutral's sort is read off its binder, so the sort remembered before the arm does not answer inside it, nor the arm's after it.
///
/// Mutation-checked: with `Certifier::assume` leaving the tables alone where it re-types a binder, the shadowed read answers `Type`.
#[test]
fn a_remembered_sort_does_not_outlive_the_type_its_local_stands_at() {
    let mut certifier = certifier();
    let hypothesis = binder(0, "h");
    let type_ = Term::free_var(&hypothesis);

    certifier.assume(&hypothesis, &Term::type_ground());
    assert_eq!(
        Sort::of(&mut certifier, &type_),
        Ok(Sort::Type(Level::zero()))
    );

    let shadowed = certifier.scoped(|certifier| {
        certifier.assume(&hypothesis, &Term::prop());
        Sort::of(certifier, &type_)
    });

    assert_eq!(shadowed, Ok(Sort::Prop(Level::zero())));
    assert_eq!(
        Sort::of(&mut certifier, &type_),
        Ok(Sort::Type(Level::zero()))
    );
}

/// Nor its binder. A name handed in from outside can be assumed again once its first binder is closed, at another type, and a sort read under the first does not answer for the second: the table files each answer with the binders it was read under, and asks whether they stand.
///
/// Mutation-checked: answering a scoped sort without asking whether its binders stand classifies the second `h` at `Type`.
#[test]
fn a_remembered_sort_does_not_outlive_its_binder() {
    let mut certifier = certifier();
    let hypothesis = binder(0, "h");
    let type_ = Term::free_var(&hypothesis);

    let first = certifier.scoped(|certifier| {
        certifier.assume(&hypothesis, &Term::type_ground());
        Sort::of(certifier, &type_)
    });
    let second = certifier.scoped(|certifier| {
        certifier.assume(&hypothesis, &Term::prop());
        Sort::of(certifier, &type_)
    });

    assert_eq!(first, Ok(Sort::Type(Level::zero())));
    assert_eq!(second, Ok(Sort::Prop(Level::zero())));
}

/// A local-free type an arm's equation makes a proposition, beside that equation's scrutinee. `g: (P) -> Nat` has no body, the type is `(x: P) -> match g(x) < 10 | true => Q | false => Nat`, which names no local, and the scrutinee is `g(p) < 10` over a proof `p` in scope. Under its equation `g(x) < 10` is that scrutinee to the certifier's conversion, the two calls differing in a proof, so the type is a proposition inside the arm and data outside it.
fn family_over_a_proof(certifier: &mut Certifier) -> (Term, Term) {
    let proposition = declare(certifier, "P", Term::prop());
    let other = declare(certifier, "Q", Term::prop());
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let g = nominal("g");
    certifier
        .declare(
            &g,
            &Term::func_type([(binder(90, "x"), proposition.clone())], nat_type.clone()),
            &UniverseContext::default(),
        )
        .expect("a name is declared once");
    let p = binder(1, "p");
    certifier.assume(&p, &proposition);
    let guard = |proof: &Free| {
        Term::intrinsic(Intrinsic::nat_lt(
            Term::apply(
                Term::instance(g, Universes::default()),
                [Term::free_var(proof)],
            ),
            Term::intrinsic(Intrinsic::Nat(Nat::new(10usize))),
        ))
    };
    let x = binder(91, "x");
    let family = Term::func_type(
        [(x, proposition)],
        Term::bool_match(guard(&x), None, Term::type_ground(), nat_type, other),
    );
    assert!(!family.has_local_free());

    (family, guard(&p))
}

/// Nor does a local-free type's: its sort is the declaration's only where no case equation is in force, since an equation answers what the certifier's conversion holds equal to its scrutinee, and a term naming only a proof the classification opened can be that ([`family_over_a_proof`]). Both orders are run, and the uncached certifier agrees.
///
/// Mutation-checked both ways: with such a sort read as the declaration's under an equation, the answer from before the arm is handed back inside it, and with it filed there, the arm's answer is handed back after the arm.
#[test]
fn a_local_free_types_sort_does_not_outlive_the_equations_it_was_read_under() {
    let orders = |inside_first: bool, mut certifier: Certifier| {
        let (family, scrutinee) = family_over_a_proof(&mut certifier);
        let is_prop =
            |certifier: &mut Certifier| Sort::of(certifier, &family).map(|sort| sort.is_prop());

        let before = (!inside_first).then(|| is_prop(&mut certifier));
        let inside = certifier.scoped(|certifier| {
            certifier
                .refine(scrutinee, Term::intrinsic(Intrinsic::Bool(true)))
                .expect("the equation records");
            is_prop(certifier)
        });
        let after = is_prop(&mut certifier);

        (before, inside, after)
    };

    for inside_first in [false, true] {
        let cached = orders(inside_first, certifier());
        assert_eq!(
            cached,
            ((!inside_first).then_some(Ok(false)), Ok(true), Ok(false))
        );
        assert_eq!(
            cached,
            orders(inside_first, Certifier::uncached(100_000, SYNTAX))
        );
    }
}

/// A position at such a type is classified under its own arm as well: a term at it is a proof inside the arm and nothing the obligations constrain outside it, whichever is recorded first.
///
/// Mutation-checked both ways: with the half read as the declaration's under an equation, the position inside the arm goes unrecorded in the first order, and with it filed there, the term is recorded as a proof after the arm in the second.
#[test]
fn a_position_at_a_local_free_type_is_classified_under_its_own_arm() {
    for inside_first in [false, true] {
        let mut certifier = certifier();
        let (family, scrutinee) = family_over_a_proof(&mut certifier);
        let inhabitant = binder(2, "f");
        certifier.assume(&inhabitant, &family);
        let record = |certifier: &mut Certifier| {
            certifier.record_checked(&Term::free_var(&inhabitant), &family)
        };

        let before = (!inside_first).then(|| record(&mut certifier));
        let inside = certifier.scoped(|certifier| {
            certifier
                .refine(scrutinee, Term::intrinsic(Intrinsic::Bool(true)))
                .expect("the equation records");
            record(certifier)
        });
        let after = record(&mut certifier);
        let (positions, failure) = certifier.take_checked();

        assert_eq!(
            (before, inside, after),
            ((!inside_first).then_some(None), Some(0), None)
        );
        assert_eq!(
            positions
                .iter()
                .map(|position| position.erased)
                .collect::<Vec<_>>(),
            [Erased::Proof],
        );
        assert!(failure.is_none());
    }
}

/// A sort is remembered by the reduction that read it: what plain reduction classified is not answered where a judgment asks, nor a judgment's where plain reduction does. A sort is read through reducts, and a judgment's reduction may ask the certifier's conversion where plain reduction asks nothing, so each files its own.
///
/// Mutation-checked both ways: with the table read as a judgment's whichever reduction asks, plain reduction finds nothing of what it filed, and with the answer filed there whichever reduction read it, a judgment is handed plain reduction's.
#[test]
fn a_sort_is_remembered_by_the_reduction_that_read_it() {
    let mut certifier = certifier();
    let read = declare(&mut certifier, "Read", Term::type_ground());
    let judged = declare(&mut certifier, "Judged", Term::prop());

    certifier
        .plainly(|certifier| Sort::of(certifier, &read))
        .expect("classifies");
    assert!(certifier.sort_hit(&read).is_none());
    assert!(
        certifier
            .plainly(|certifier| certifier.sort_hit(&read))
            .is_some()
    );

    Sort::of(&mut certifier, &judged).expect("classifies");
    assert!(
        certifier
            .plainly(|certifier| certifier.sort_hit(&judged))
            .is_none()
    );
    assert!(certifier.sort_hit(&judged).is_some());
}
