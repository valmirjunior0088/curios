//! A block's end restricts the metavariables it leaves unsolved: what [`Context::block_restriction`] plans, and what it leaves to be read by value.
//!
//! A block's label is a binder of every metavariable born in the block and an argument of its spine. Where the block ends nothing binds the label, so a metavariable the block's type still holds is solved to a stand-in over the binders before the block's own, at its type with each label read as its value.

use {
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{Free, Intrinsic, MetavarId, MetavarOrigin, Nat, Subterm, Term},
    std::rc::Rc,
};

fn context() -> Context<'static> {
    Context::with_default_budget(SYNTAX)
}

fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

fn two() -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(2usize)))
}

/// A metavariable born where the context stands, at `result`, with the occurrence that reads it there.
fn born(context: &mut Context, id: usize, result: Term) -> Rc<Vec<Term>> {
    let (telescope, spine) = context.identity_snapshot();
    context.birth_metavar(MetavarId(id), telescope, result);
    spine
}

/// A family over `Nat` assumed at the top level, for a type that names a label.
fn family(context: &mut Context) -> Free {
    let family = context.fresh(Some("F"));
    let index = context.fresh(Some("i"));
    context.assume(
        &family,
        &Term::func_type([(index, nat_type())], Term::type_ground()),
    );
    family
}

/// The stand-in is born over the binders before the block's labels and at the metavariable's type with the label read as its value: `?0 : F(n)` over `d` and `n`, where `n` is the block's and stands for `2`, becomes `?1 : F(2)` over `d` alone, applied to `d`.
#[test]
fn a_metavariable_is_restricted_to_the_binders_before_the_blocks_labels() {
    let mut context = context();
    let family = family(&mut context);
    let d = context.fresh(Some("d"));
    let n = context.fresh(Some("n"));

    context.with_frame(|context| {
        context.assume(&d, &nat_type());
        context.with_frame(|context| {
            context.define_assuming(&n, &nat_type(), &two(), None);
            let result = Term::apply(Term::free_var(&family), [Term::free_var(&n)]);
            let spine = born(context, 0, result);
            let values = [(n, two())];

            let restriction = context
                .block_restriction(MetavarId(0), &MetavarOrigin::Hole, &[spine], &values)
                .expect("a metavariable born in the block's frame is restricted");
            context.restrict_metavar(MetavarId(0), (MetavarOrigin::Hole, None), restriction);

            let Some(Subterm::Metavar(stand_in)) =
                context.metavar_solution(MetavarId(0)).map(|term| &**term)
            else {
                panic!("the metavariable is solved to its stand-in");
            };
            assert_eq!(*stand_in.spine, vec![Term::free_var(&d)]);
            let entry = context
                .metavar_entry(stand_in.id)
                .expect("the stand-in has a birth record");
            assert_eq!(*entry.telescope, vec![(d, nat_type())]);
            assert_eq!(entry.result, Term::apply(Term::free_var(&family), [two()]));
        });
    });
}

/// A metavariable born under a binder of the block's tail is not restricted: that binder may be typed by a label, and so may the refinements it was born under, which the stand-in would take as they stand.
#[test]
fn a_metavariable_born_under_a_binder_of_the_tail_is_not_restricted() {
    let mut context = context();
    let n = context.fresh(Some("n"));
    let y = context.fresh(Some("y"));

    context.with_frame(|context| {
        context.define_assuming(&n, &nat_type(), &two(), None);
        context.with_frame(|context| {
            context.assume(&y, &nat_type());
            let spine = born(context, 0, nat_type());
            let values = [(n, two())];

            assert!(
                context
                    .block_restriction(MetavarId(0), &MetavarOrigin::Hole, &[spine], &values)
                    .is_none()
            );
        });
    });
}

/// An occurrence that is not the metavariable read where it was born is not restricted: a value is spelled in the birth's names only through an occurrence that names each binder as itself.
#[test]
fn an_occurrence_read_away_from_its_birth_is_not_restricted() {
    let mut context = context();
    let d = context.fresh(Some("d"));
    let n = context.fresh(Some("n"));

    context.with_frame(|context| {
        context.assume(&d, &nat_type());
        context.define_assuming(&n, &nat_type(), &two(), None);
        let _ = born(context, 0, nat_type());
        let away = Rc::new(vec![two(), Term::free_var(&n)]);
        let values = [(n, two())];

        assert!(
            context
                .block_restriction(MetavarId(0), &MetavarOrigin::Hole, &[away], &values)
                .is_none()
        );
    });
}

/// A hole something else fills is not restricted: a written goal reports by its identity, where it was born, and the label in its spine is read by value where it stands.
#[test]
fn a_hole_something_else_fills_is_not_restricted() {
    let mut context = context();
    let n = context.fresh(Some("n"));

    context.with_frame(|context| {
        context.define_assuming(&n, &nat_type(), &two(), None);
        let spine = born(context, 0, nat_type());
        let values = [(n, two())];

        assert!(
            context
                .block_restriction(MetavarId(0), &MetavarOrigin::Goal, &[spine], &values)
                .is_none()
        );
    });
}

/// A metavariable whose type still holds an unsolved one naming a label waits for that one: restricted first, its stand-in names no label, and the type is read by value with nothing of a label in a spine.
#[test]
fn a_metavariable_waits_for_the_one_its_type_holds() {
    let mut context = context();
    let n = context.fresh(Some("n"));

    context.with_frame(|context| {
        context.define_assuming(&n, &nat_type(), &two(), None);
        let type_spine = born(context, 0, Term::type_ground());
        let type_ = Term::metavar_birthed(0, MetavarOrigin::Hole, (*type_spine).clone());
        let occurrences = [born(context, 1, type_)];
        let values = [(n, two())];

        assert!(
            context
                .block_restriction(MetavarId(1), &MetavarOrigin::Hole, &occurrences, &values)
                .is_none()
        );

        let restriction = context
            .block_restriction(MetavarId(0), &MetavarOrigin::Hole, &[type_spine], &values)
            .expect("the type's metavariable is restricted first");
        context.restrict_metavar(MetavarId(0), (MetavarOrigin::Hole, None), restriction);

        assert!(
            context
                .block_restriction(MetavarId(1), &MetavarOrigin::Hole, &occurrences, &values)
                .is_some()
        );
    });
}
