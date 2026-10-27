//! Remembered sorts: a shared type classified once a node, and a sort that does not outlive the equations, arm, type or binder it was read under.

use {
    super::test_support::{binder, declare, kernel, nominal},
    crate::{Error, Kernel, Sort},
    curios_analysis::{Erased, test_support::SYNTAX},
    curios_core::{Free, Intrinsic, Level, Nat, Term, UniverseContext},
};

/// A record of two fields at one type, `depth` deep: a graph of `depth + 1` nodes whose tree has `2^depth` fields.
fn doubled(base: Term, depth: usize) -> Term {
    (0..depth).fold(base, |type_, level| {
        let first = Free::local(2 * level as u64 + 100, None);
        let second = Free::local(2 * level as u64 + 101, None);

        Term::tuple_type([(first, type_.clone()), (second, type_)])
    })
}

/// A sort is remembered per type, so a record sixty levels deep whose two fields are one type each — a tree no walk per path finishes — is classified in the size of its graph, over a closed base and over a local alike. At a depth the uncached kernel affords, both give the same sort.
///
/// Mutation-checked: with no remembered sort answered, neither sixty-level row finishes.
#[test]
fn a_shared_type_is_classified_once_per_node() {
    let nat = Term::intrinsic(Intrinsic::NatType);
    assert_eq!(
        Sort::of(&mut kernel(), &doubled(nat.clone(), 60)),
        Ok(Sort::Type(Level::zero())),
    );

    let hypothesis = binder(0, "A");
    let mut over_a_local = kernel();
    over_a_local.assume(&hypothesis, &Term::type_ground());
    assert_eq!(
        Sort::of(&mut over_a_local, &doubled(Term::free_var(&hypothesis), 60)),
        Ok(Sort::Type(Level::zero())),
    );

    let mut uncached = Kernel::uncached(100_000, SYNTAX);
    assert_eq!(
        Sort::of(&mut kernel(), &doubled(nat.clone(), 8)),
        Sort::of(&mut uncached, &doubled(nat, 8)),
    );
}

/// A remembered sort lives as long as the equations it was taken under: a type whose universe is a scrutinee is a proposition in the arm that makes it `Prop`, data in the arm that makes it `Type`, and unclassified outside both — and the uncached kernel agrees on all three.
///
/// Mutation-checked: leaving the scoped sorts standing where an equation moves answers the second arm `Prop`.
#[test]
fn a_remembered_sort_does_not_outlive_the_equations_it_was_taken_under() {
    let sequence = |kernel: &mut Kernel| {
        let universe = binder(0, "u");
        let hypothesis = binder(1, "A");
        kernel.assume(
            &universe,
            &Term::type_at(Level::zero().succ().expect("one")),
        );
        kernel.assume(&hypothesis, &Term::free_var(&universe));
        let type_ = Term::free_var(&hypothesis);

        let under = |kernel: &mut Kernel, value: Term| {
            kernel.scoped(|kernel| {
                kernel
                    .refine(Term::free_var(&universe), value)
                    .expect("the equation records");
                Sort::of(kernel, &type_)
            })
        };

        [
            under(kernel, Term::prop()),
            under(kernel, Term::type_ground()),
            Sort::of(kernel, &type_),
        ]
    };

    let cached = sequence(&mut kernel());

    assert_eq!(cached[0], Ok(Sort::Prop(Level::zero())));
    assert_eq!(cached[1], Ok(Sort::Type(Level::zero())));
    assert!(matches!(cached[2], Err(Error::NotASort(_))));
    assert_eq!(cached, sequence(&mut Kernel::uncached(100_000, SYNTAX)));
}

/// What a position is recorded as lives as its type's sort does: a term at a type one arm's equation makes a proposition is a proof in that arm and nothing the obligations constrain in the arm that makes its type data, whichever is checked first.
///
/// Mutation-checked: leaving the remembered halves standing where an equation moves records the second arm's position as a proof too.
#[test]
fn a_position_is_classified_under_its_own_arm() {
    let mut kernel = kernel();
    let universe = binder(0, "u");
    let hypothesis = binder(1, "A");
    let inhabitant = binder(2, "a");
    kernel.assume(
        &universe,
        &Term::type_at(Level::zero().succ().expect("one")),
    );
    kernel.assume(&hypothesis, &Term::free_var(&universe));
    kernel.assume(&inhabitant, &Term::free_var(&hypothesis));

    let recorded = [Term::prop(), Term::type_ground()].map(|value| {
        kernel.scoped(|kernel| {
            kernel
                .refine(Term::free_var(&universe), value)
                .expect("the equation records");
            kernel.record_checked(&Term::free_var(&inhabitant), &Term::free_var(&hypothesis))
        })
    });
    let (positions, failure) = kernel.take_checked();

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
/// Mutation-checked: with `Kernel::assume` leaving the tables alone where it re-types a binder, the shadowed read answers `Type`.
#[test]
fn a_remembered_sort_does_not_outlive_the_type_its_local_stands_at() {
    let mut kernel = kernel();
    let hypothesis = binder(0, "h");
    let type_ = Term::free_var(&hypothesis);

    kernel.assume(&hypothesis, &Term::type_ground());
    assert_eq!(Sort::of(&mut kernel, &type_), Ok(Sort::Type(Level::zero())));

    let shadowed = kernel.scoped(|kernel| {
        kernel.assume(&hypothesis, &Term::prop());
        Sort::of(kernel, &type_)
    });

    assert_eq!(shadowed, Ok(Sort::Prop(Level::zero())));
    assert_eq!(Sort::of(&mut kernel, &type_), Ok(Sort::Type(Level::zero())));
}

/// Nor its binder. A name handed in from outside can be assumed again once its first binder is closed, at another type, and a sort read under the first does not answer for the second: the table files each answer with the binders it was read under, and asks whether they stand.
///
/// Mutation-checked: answering a scoped sort without asking whether its binders stand classifies the second `h` at `Type`.
#[test]
fn a_remembered_sort_does_not_outlive_its_binder() {
    let mut kernel = kernel();
    let hypothesis = binder(0, "h");
    let type_ = Term::free_var(&hypothesis);

    let first = kernel.scoped(|kernel| {
        kernel.assume(&hypothesis, &Term::type_ground());
        Sort::of(kernel, &type_)
    });
    let second = kernel.scoped(|kernel| {
        kernel.assume(&hypothesis, &Term::prop());
        Sort::of(kernel, &type_)
    });

    assert_eq!(first, Ok(Sort::Type(Level::zero())));
    assert_eq!(second, Ok(Sort::Prop(Level::zero())));
}

/// A local-free type an arm's equation makes a proposition, beside that equation's scrutinee. `g: (P) -> Nat` has no body, the type is `(x: P) -> match g(x) < 10 | true => Q | false => Nat`, which names no local, and the scrutinee is `g(p) < 10` over a proof `p` in scope. Under its equation `g(x) < 10` is that scrutinee to the kernel's conversion, the two calls differing in a proof, so the type is a proposition inside the arm and data outside it.
fn family_over_a_proof(kernel: &mut Kernel) -> (Term, Term) {
    let proposition = declare(kernel, "P", Term::prop());
    let other = declare(kernel, "Q", Term::prop());
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let g = Free::from(&nominal("g"));
    kernel
        .declare(
            &g,
            &Term::func_type([(binder(90, "x"), proposition.clone())], nat_type.clone()),
            &UniverseContext::default(),
        )
        .expect("a name is declared once");
    let p = binder(1, "p");
    kernel.assume(&p, &proposition);
    let guard = |proof: &Free| {
        Term::intrinsic(Intrinsic::nat_lt(
            Term::apply(Term::free_var(&g), [Term::free_var(proof)]),
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

/// Nor does a local-free type's: its sort is the declaration's only where no case equation is in force, since an equation answers what the kernel's conversion holds equal to its scrutinee, and a term naming only a proof the classification opened can be that ([`family_over_a_proof`]). Both orders are run, and the uncached kernel agrees.
///
/// Mutation-checked both ways: with such a sort read as the declaration's under an equation, the answer from before the arm is handed back inside it, and with it filed there, the arm's answer is handed back after the arm.
#[test]
fn a_local_free_types_sort_does_not_outlive_the_equations_it_was_read_under() {
    let orders = |inside_first: bool, mut kernel: Kernel| {
        let (family, scrutinee) = family_over_a_proof(&mut kernel);
        let is_prop = |kernel: &mut Kernel| Sort::of(kernel, &family).map(|sort| sort.is_prop());

        let before = (!inside_first).then(|| is_prop(&mut kernel));
        let inside = kernel.scoped(|kernel| {
            kernel
                .refine(scrutinee, Term::intrinsic(Intrinsic::Bool(true)))
                .expect("the equation records");
            is_prop(kernel)
        });
        let after = is_prop(&mut kernel);

        (before, inside, after)
    };

    for inside_first in [false, true] {
        let cached = orders(inside_first, kernel());
        assert_eq!(
            cached,
            ((!inside_first).then_some(Ok(false)), Ok(true), Ok(false))
        );
        assert_eq!(
            cached,
            orders(inside_first, Kernel::uncached(100_000, SYNTAX))
        );
    }
}

/// A position at such a type is classified under its own arm as well: a term at it is a proof inside the arm and nothing the obligations constrain outside it, whichever is recorded first.
///
/// Mutation-checked both ways: with the half read as the declaration's under an equation, the position inside the arm goes unrecorded in the first order, and with it filed there, the term is recorded as a proof after the arm in the second.
#[test]
fn a_position_at_a_local_free_type_is_classified_under_its_own_arm() {
    for inside_first in [false, true] {
        let mut kernel = kernel();
        let (family, scrutinee) = family_over_a_proof(&mut kernel);
        let inhabitant = binder(2, "f");
        kernel.assume(&inhabitant, &family);
        let record =
            |kernel: &mut Kernel| kernel.record_checked(&Term::free_var(&inhabitant), &family);

        let before = (!inside_first).then(|| record(&mut kernel));
        let inside = kernel.scoped(|kernel| {
            kernel
                .refine(scrutinee, Term::intrinsic(Intrinsic::Bool(true)))
                .expect("the equation records");
            record(kernel)
        });
        let after = record(&mut kernel);
        let (positions, failure) = kernel.take_checked();

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

/// A sort is remembered by the reduction that read it: what plain reduction classified is not answered where a judgment asks, nor a judgment's where plain reduction does. A sort is read through reducts, and a judgment's reduction may ask the kernel's conversion where plain reduction asks nothing, so each files its own.
///
/// Mutation-checked both ways: with the table read as a judgment's whichever reduction asks, plain reduction finds nothing of what it filed, and with the answer filed there whichever reduction read it, a judgment is handed plain reduction's.
#[test]
fn a_sort_is_remembered_by_the_reduction_that_read_it() {
    let mut kernel = kernel();
    let read = declare(&mut kernel, "Read", Term::type_ground());
    let judged = declare(&mut kernel, "Judged", Term::prop());

    kernel
        .plainly(|kernel| Sort::of(kernel, &read))
        .expect("classifies");
    assert!(kernel.sort_hit(&read).is_none());
    assert!(kernel.plainly(|kernel| kernel.sort_hit(&read)).is_some());

    Sort::of(&mut kernel, &judged).expect("classifies");
    assert!(kernel.plainly(|kernel| kernel.sort_hit(&judged)).is_none());
    assert!(kernel.sort_hit(&judged).is_some());
}
