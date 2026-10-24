//! A spine's facts are its entries', however it was built, and only a spine built as the identity is known for one.

use {
    super::*,
    crate::{Global, Intrinsic, MetavarId, MetavarOrigin, Nat, Var},
    curios_utilities::Qualifier,
    std::hash::DefaultHasher,
};

fn local(id: u64, name: &str) -> Free {
    Free::local(id, Some(name))
}

fn hash_of(spine: &MetavarSpine) -> u64 {
    let mut hasher = DefaultHasher::new();
    spine.hash(&mut hasher);
    hasher.finish()
}

#[test]
fn a_spine_holds_the_facts_of_its_entries() {
    let entries = vec![
        Term::free_var(&local(4, "a")),
        Term::var(Var::bound(2)),
        Term::free_var(&local(1, "b")),
        Term::intrinsic(Intrinsic::Nat(Nat::new(3usize))),
    ];
    let spine = MetavarSpine::from(entries.clone());

    assert_eq!(spine.reach(), 3);
    assert_eq!(spine.newest_local(), Some(4));
    assert!(!spine.has_transient());
    assert!(!spine.has_group());
    assert!(!spine.has_universe_meta());

    // Rewriting the one entry that reached past the spine and the newest local leaves the older local the newest.
    let rewritten = spine
        .rewritten(|entry| {
            (entry.newest_local() == Some(4) || entry.reach() > 0)
                .then(|| Term::free_var(&Free::Global(Global::Authored(Qualifier::from(["g"])))))
        })
        .expect("two entries were rewritten");
    assert_eq!(rewritten.reach(), 0);
    assert_eq!(rewritten.newest_local(), Some(1));
    assert_eq!(rewritten.len(), 4);
}

#[test]
fn two_spines_of_one_list_of_entries_are_one_spine() {
    let names = [local(0, "a"), local(1, "b"), local(2, "c")];
    let identity = MetavarSpine::identity_over(&names);
    let listed = MetavarSpine::from(names.iter().map(Term::free_var).collect::<Vec<_>>());

    assert_eq!(identity, listed);
    assert_eq!(hash_of(&identity), hash_of(&listed));
    assert!(identity.is_identity());
    assert!(!listed.is_identity());

    let reordered = MetavarSpine::from(
        [&names[1], &names[0], &names[2]]
            .into_iter()
            .map(Term::free_var)
            .collect::<Vec<_>>(),
    );
    assert_ne!(identity, reordered);
}

#[test]
fn only_a_walk_that_rewrites_an_entry_loses_the_identity() {
    let names = [local(0, "a"), local(1, "b")];
    let identity = MetavarSpine::identity_over(&names);

    assert!(identity.rewritten(|_| None).is_none());
    let rewritten = identity
        .rewritten(|entry| (*entry == Term::free_var(&names[1])).then(|| Term::var(Var::bound(0))))
        .expect("one entry was rewritten");
    assert!(!rewritten.is_identity());
    assert_eq!(identity.len(), 2);
    assert!(identity.is_identity());
    assert!(MetavarSpine::from(Vec::new()).is_identity());
}

/// A spine's names are its entries', each counted by the entries holding it: rewriting one entry that held a name leaves it where another entry still does, and takes it out where none does.
#[test]
fn a_spine_holds_the_names_of_its_entries() {
    let (a, b) = (local(1, "a"), local(2, "b"));
    let g = Free::Global(Global::Authored(Qualifier::from(["g"])));
    let spine = MetavarSpine::from(vec![
        Term::free_var(&a),
        Term::apply(Term::free_var(&g), [Term::free_var(&a)]),
        Term::free_var(&b),
    ]);
    let names = |spine: &MetavarSpine| spine.free_vars().iter().copied().collect::<Vec<_>>();
    assert_eq!(names(&spine), [g, a, b]);

    let rewritten = spine
        .rewritten(|entry| (*entry == Term::free_var(&a)).then(|| Term::var(Var::bound(0))))
        .expect("an entry was rewritten");
    assert_eq!(names(&rewritten), [g, a, b]);

    let rewritten = rewritten
        .rewritten(|entry| (*entry == Term::free_var(&b)).then(|| Term::var(Var::bound(1))))
        .expect("an entry was rewritten");
    assert_eq!(names(&rewritten), [g, a]);
    assert!(rewritten.mentions_any(&Names::new([a, b])));
    assert!(!rewritten.mentions_any(&Names::new([b])));
}

/// A metavariable's free variables are its spine's names, the one set the spine keeps, read without reading an entry.
#[test]
fn a_metavariables_set_is_its_spines() {
    let spine = MetavarSpine::identity_over(&[local(0, "a"), local(1, "b"), local(2, "c")]);
    let hole = Term::metavar_birthed(MetavarId(5), MetavarOrigin::Hole, spine.clone());

    assert!(hole.free_vars().shares(&spine.free_vars()));
    assert!(
        Term::tuple([hole.clone(), Term::free_var(&local(1, "b"))])
            .free_vars()
            .shares(&spine.free_vars())
    );
}
