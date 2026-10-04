//! The sort a type inhabits, remembered: what the table saves, and what ends it.

use {
    crate::{Context, Sort},
    curios_analysis::test_support::SYNTAX,
    curios_core::{Free, Intrinsic, Level, Term},
};

/// A record of two fields at one type, `depth` deep: a graph of `depth + 1` nodes whose tree has `2^depth` fields.
fn doubled(base: Term, depth: u32) -> Term {
    (0..depth).fold(base, |type_, level| {
        let first = Free::local(2 * level + 100, None);
        let second = Free::local(2 * level + 101, None);

        Term::tuple_type([(first, type_.clone()), (second, type_)])
    })
}

/// A sort is remembered per type while the context stands as it did, so a record sixty levels deep whose two fields are one type each — a tree no walk per path finishes — is classified in the size of its graph, over a closed base and over a local alike.
///
/// Mutation-checked: with no remembered sort answered, neither row finishes.
#[test]
fn a_shared_type_is_classified_once_per_node() {
    let ground = Sort::Type(Level::zero());
    let mut context = Context::with_default_budget(SYNTAX);
    let nat = Term::intrinsic(Intrinsic::NatType);
    assert_eq!(
        Sort::of(&mut context, &doubled(nat, 60)).expect("classifies"),
        ground,
    );

    let hypothesis = Free::local(0, Some("A"));
    context.assume(&hypothesis, &Term::type_ground());
    assert_eq!(
        Sort::of(&mut context, &doubled(Term::free_var(&hypothesis), 60)).expect("classifies"),
        ground,
    );
}

/// A remembered sort does not outlive the type its local stands at, on either side of the frame that re-types it: assuming a local is a stamped write, which empties the table at the next probe, and leaving a frame closes its binders and empties it there.
///
/// Mutation-checked: with the stamps unread the shadowed probe answers `Type`, and with a frame's exit leaving the table alone the probe after it answers `Prop`.
#[test]
fn a_remembered_sort_does_not_outlive_the_type_its_local_stands_at() {
    let mut context = Context::with_default_budget(SYNTAX);
    let hypothesis = Free::local(0, Some("h"));
    let type_ = Term::free_var(&hypothesis);

    context.assume(&hypothesis, &Term::type_ground());
    let before = Sort::of(&mut context, &type_).expect("classifies");
    let shadowed = context.with_frame(|context| {
        context.assume(&hypothesis, &Term::prop());
        Sort::of(context, &type_).expect("classifies")
    });
    let after = Sort::of(&mut context, &type_).expect("classifies");

    assert_eq!(before, Sort::Type(Level::zero()));
    assert_eq!(shadowed, Sort::Prop);
    assert_eq!(after, Sort::Type(Level::zero()));
}

/// Nor a definition landing. A fresh definition stamps nothing — it is the one ambient fact a pure run may read — and a type that was stuck on the name is classified again once the name unfolds.
///
/// Mutation-checked: with the table left alone where a fresh definition lands, the second probe answers `Type`.
#[test]
fn a_remembered_sort_does_not_outlive_a_definition_landing() {
    let mut context = Context::with_default_budget(SYNTAX);
    let proposition = Free::local(0, Some("P"));
    let alias = Free::local(1, Some("T"));
    context.assume(&proposition, &Term::prop());
    let type_ = Term::free_var(&alias);

    let stuck = Sort::of(&mut context, &type_).expect("classifies");
    context.define(&alias, &Term::free_var(&proposition), None);
    let unfolded = Sort::of(&mut context, &type_).expect("classifies");

    assert_eq!(stuck, Sort::Type(Level::zero()));
    assert_eq!(unfolded, Sort::Prop);
}

/// A remembered sort lives as long as the budget does, as a reduct does: across a restore a type pays for its classification again, so a sort one declaration filed never spares the next the reduction it rests on.
///
/// Mutation-checked: with the table kept where the reducts are cleared, the type after the boundary is classified for nothing.
#[test]
fn restoring_the_budget_forgets_a_remembered_sort() {
    let mut context = Context::with_default_budget(SYNTAX);
    let x = Free::local(0, Some("x"));
    let type_ = Term::apply(
        Term::func([(x, Term::type_ground())], Term::free_var(&x)),
        [Term::intrinsic(Intrinsic::NatType)],
    );

    Sort::of(&mut context, &type_).expect("classifies");
    let first = context.consumed().units();
    Sort::of(&mut context, &type_).expect("classifies");
    let second = context.consumed().units() - first;
    context.restore_budget();
    Sort::of(&mut context, &type_).expect("classifies");
    let after_boundary = context.consumed().units();

    assert!(first > 0, "classifying it reduces it");
    assert_eq!(second, 0);
    assert_eq!(after_boundary, first);
}

/// A type naming a binder its walk opened is read off the walk and never the table: the binder's type travels beside the walk, where no stamp sees it, so one name opened at two types by two walks is classified at each.
///
/// Mutation-checked: with such a type remembered, the second walk answers `Type`.
#[test]
fn a_type_naming_a_binder_its_walk_opened_is_not_remembered() {
    let mut context = Context::with_default_budget(SYNTAX);
    let binder = Free::local(0, Some("x"));
    let type_ = Term::free_var(&binder);

    let at_a_type = Sort::of_in(
        &mut context,
        &mut vec![(binder, Term::type_ground())],
        &type_,
    )
    .expect("classifies");
    let at_a_proposition =
        Sort::of_in(&mut context, &mut vec![(binder, Term::prop())], &type_).expect("classifies");

    assert_eq!(at_a_type, Sort::Type(Level::zero()));
    assert_eq!(at_a_proposition, Sort::Prop);
}
