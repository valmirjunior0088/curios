//! Remembered sorts: a shared type classified once a node, and a sort that does not outlive the equations, arm, type or binder it was read under.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_analysis::{Erased, test_support::*},
    curios_common::fill,
    curios_core::{Level, Term, UniverseContext},
};

/// A record of two fields at one type, `depth` deep over `base`, written with each level's type named where it is met again: a graph of `depth + 1` nodes whose tree has `2^depth` fields.
fn doubled(base: &str, depth: usize) -> String {
    (0..depth).fold(base.to_string(), |type_, level| {
        fill!(
            "{a: #{share = ${level}} (${type_}), b: #{shared = ${level}} _}",
            level,
            type_
        )
    })
}

/// A sort is remembered per type, so a record sixty levels deep whose two fields are one type each — a tree no walk per path finishes — is classified in the size of its graph, over a closed base and over a local alike. At a depth the uncached certifier affords, both give the same sort.
///
/// Mutation-checked: with no remembered sort answered, neither sixty-level row finishes.
#[test]
fn a_shared_type_is_classified_once_per_node() {
    assert_eq!(
        Sort::of(&mut certifier(), &term(&doubled("#NatType", 60))),
        Ok(Sort::Type(Level::zero())),
    );

    let mut over_a_local = certifier();
    let (_, over) = opened(
        &mut over_a_local,
        &fill!("(A: Type) => ${doubled}", doubled = doubled("A", 60)),
    );
    assert_eq!(
        Sort::of(&mut over_a_local, &over),
        Ok(Sort::Type(Level::zero())),
    );

    let mut uncached = Certifier::uncached(100_000, SYNTAX);
    assert_eq!(
        Sort::of(&mut certifier(), &term(&doubled("#NatType", 8))),
        Sort::of(&mut uncached, &term(&doubled("#NatType", 8))),
    );
}

/// A remembered sort lives as long as the equations it was taken under: a type whose universe is a scrutinee is a proposition in the arm that makes it `Prop`, data in the arm that makes it `Type`, and unclassified outside both — and the uncached certifier agrees on all three.
///
/// Mutation-checked: leaving the scoped sorts standing where an equation moves answers the second arm `Prop`.
#[test]
fn a_remembered_sort_does_not_outlive_the_equations_it_was_taken_under() {
    let sequence = |certifier: &mut Certifier| {
        let (locals, type_) = opened(certifier, "(u: #{level = 1} Type, A: u) => A");
        let universe = Term::free_var(&locals[0]);

        let under = |certifier: &mut Certifier, value: &str| {
            certifier.scoped(|certifier| {
                certifier
                    .refine(universe.clone(), term(value))
                    .expect("the equation records");
                Sort::of(certifier, &type_)
            })
        };

        [
            under(certifier, "Prop"),
            under(certifier, "Type"),
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
    let (locals, inhabitant) = opened(&mut certifier, "(u: #{level = 1} Type, A: u, a: A) => a");
    let (universe, hypothesis) = (Term::free_var(&locals[0]), Term::free_var(&locals[1]));

    let recorded = ["Prop", "Type"].map(|value| {
        certifier.scoped(|certifier| {
            certifier
                .refine(universe.clone(), term(value))
                .expect("the equation records");
            certifier.record_checked(&inhabitant, &hypothesis)
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
    let (locals, type_) = opened(&mut certifier, "(h: Type) => h");
    assert_eq!(
        Sort::of(&mut certifier, &type_),
        Ok(Sort::Type(Level::zero()))
    );

    let shadowed = certifier.scoped(|certifier| {
        certifier.assume(&locals[0], &term("Prop"));
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
        certifier.assume(&hypothesis, &term("Type"));
        Sort::of(certifier, &type_)
    });
    let second = certifier.scoped(|certifier| {
        certifier.assume(&hypothesis, &term("Prop"));
        Sort::of(certifier, &type_)
    });

    assert_eq!(first, Ok(Sort::Type(Level::zero())));
    assert_eq!(second, Ok(Sort::Prop(Level::zero())));
}

/// A local-free type an arm's equation makes a proposition, beside that equation's scrutinee. `g: (P) -> Nat` has no body, the type is `(x: P) -> match g(x) < 10 | true => Q | false => Nat`, which names no local, and the scrutinee is `g(p) < 10` over a proof `p` in scope. Under its equation `g(x) < 10` is that scrutinee to the certifier's conversion, the two calls differing in a proof, so the type is a proposition inside the arm and data outside it.
fn family_over_a_proof(certifier: &mut Certifier) -> (Term, Term) {
    declared(
        certifier,
        "induct /P: pub Prop\nend\n\ninduct /Q: pub Prop\nend",
    );
    certifier
        .declare(
            &nominal("g"),
            &term("(x: #{node = InductType} /P()) -> #NatType"),
            &UniverseContext::default(),
        )
        .expect("a name is declared once");
    let (_, scrutinee) = opened(
        certifier,
        "(p: #{node = InductType} /P()) => #NatLt(/g(p), 10)",
    );
    let family = term(
        "(x: #{node = InductType} /P()) -> #{result = Ambient(Type)} match #NatLt(/g(x), 10) | false => #NatType | true => #{node = InductType} /Q() end",
    );
    assert!(!family.has_local_free());

    (family, scrutinee)
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
                .refine(scrutinee, term("true"))
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
        let inhabitant = certifier.fresh(Some("f"));
        certifier.assume(&inhabitant, &family);
        let record = |certifier: &mut Certifier| {
            certifier.record_checked(&Term::free_var(&inhabitant), &family)
        };

        let before = (!inside_first).then(|| record(&mut certifier));
        let inside = certifier.scoped(|certifier| {
            certifier
                .refine(scrutinee, term("true"))
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
    declared(
        &mut certifier,
        "induct /Read: pub Type\nend\n\ninduct /Judged: pub Prop\nend",
    );
    let (read, judged) = (
        term("#{node = InductType} /Read()"),
        term("#{node = InductType} /Judged()"),
    );

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
