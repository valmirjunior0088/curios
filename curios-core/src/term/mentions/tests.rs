use {super::*, crate::*, curios_common::Qualifier};

/// Every empty set is one allocation, however it was come to.
#[test]
fn empty_sets_share_one_canonical_set() {
    assert!(Mentions::empty().shares(&Mentions::union([])));
    assert!(Mentions::union([Mentions::empty(), Mentions::empty()]).shares(&Mentions::empty()));
}

/// A union with one carrying operand is that operand, shared: what lets a chain of links above one free occurrence hold one set.
#[test]
fn a_union_of_one_carrier_shares_it() {
    let carrier = Mentions::single(Mention::Local(Free::local(0, Some("x"))));

    assert!(
        Mentions::union([Mentions::empty(), carrier.clone(), Mentions::empty()]).shares(&carrier)
    );
}

#[test]
fn a_union_of_two_carriers_holds_both_in_order() {
    let x = Mention::Local(Free::local(0, Some("x")));
    let y = Mention::Local(Free::local(1, Some("y")));

    assert_eq!(
        Mentions::union([Mentions::single(y), Mentions::single(x)])
            .iter()
            .copied()
            .collect::<Vec<_>>(),
        [x, y],
    );
}

/// A union whose smaller operands add nothing to the largest is the largest itself, in whichever order they come, and one that adds a name holds every name of both.
#[test]
fn a_union_keeps_the_largest_where_the_others_add_nothing() {
    let names = (0..10)
        .map(|index| Mention::Local(Free::local(index, None)))
        .collect::<Vec<_>>();
    let largest = Mentions::union(names.iter().map(|name| Mentions::single(*name)));
    let within = Mentions::union([Mentions::single(names[3]), Mentions::single(names[7])]);

    assert!(Mentions::union([within.clone(), largest.clone()]).shares(&largest));
    assert!(Mentions::union([largest.clone(), within]).shares(&largest));

    let grown = Mentions::union([
        largest.clone(),
        Mentions::single(Mention::Local(Free::local(42, None))),
    ]);
    assert_eq!(grown.len(), 11);
    assert!(!grown.shares(&largest));
}

/// The locals are read from the least of them, past every declaration, and the declarations before the first local.
#[test]
fn locals_are_read_past_the_declarations() {
    let declaration = Global::Authored(Qualifier::from(["g"]));
    let local = Free::local(7, Some("l"));
    let set = Mentions::union([
        Mentions::single(Mention::Local(local)),
        Mentions::single(Mention::Declaration(declaration)),
    ]);

    assert_eq!(
        set.iter().copied().collect::<Vec<_>>(),
        [Mention::Declaration(declaration), Mention::Local(local)]
    );
    assert_eq!(set.locals().copied().collect::<Vec<_>>(), [local]);
    assert_eq!(
        set.declarations().copied().collect::<Vec<_>>(),
        [declaration]
    );
}

#[test]
fn a_filled_cache_answers_membership() {
    let name = Mention::Local(Free::local(0, Some("x")));
    let cache = MentionCache::default();
    cache.fill(Mentions::single(name));

    assert!(cache.is_filled());
    assert!(cache.contains(&name));
    assert_eq!(cache.get().unwrap().len(), 1);
}

/// A local is answered by the newest local a term keeps wherever that decides — none held newer, or it the newest — and by the set otherwise, a newest local past its packed width included.
#[test]
fn a_local_is_answered_by_the_newest_local_first() {
    let older = Free::local(1, Some("a"));
    let newer = Free::local(2, Some("b"));
    let newest = Free::local(3, Some("c"));
    let wide = Free::local(1 << 40, Some("w"));
    let pair =
        |first: &Free, second: &Free| Term::tuple([Term::free_var(first), Term::free_var(second)]);

    assert!(!pair(&older, &newer).mentions_free(&newest));
    assert!(pair(&older, &newer).mentions_free(&newer));
    assert!(pair(&older, &newer).mentions_free(&older));
    assert!(!pair(&newer, &newest).mentions_free(&older));
    assert!(pair(&older, &wide).mentions_free(&wide));
    assert!(pair(&older, &wide).mentions_free(&older));
    assert!(!pair(&older, &wide).mentions_free(&newer));
}

/// Several names are answered as none where every one is a local newer than the newest a term keeps, and otherwise by the two sets; a declaration among them leaves the sets to answer.
#[test]
fn any_of_several_names_is_answered_by_their_oldest_local_first() {
    let older = Free::local(1, Some("a"));
    let newer = Free::local(2, Some("b"));
    let newest = Free::local(3, Some("c"));
    let global = Global::Authored(Qualifier::from(["g"]));
    let declaration = Mention::Declaration(global);

    assert!(!Term::free_var(&older).mentions_any(&Names::new([newer, newest])));
    assert!(Term::free_var(&newer).mentions_any(&Names::new([newer, newest])));
    let declared = Term::instance(global, Universes::default());
    assert!(declared.mentions_any(&Names::new([declaration, newest.into()])));
    assert!(!Term::free_var(&older).mentions_any(&Names::new([declaration, newest.into()])));
    assert!(!Term::free_var(&older).mentions_any(&Names::new(Vec::<Mention>::new())));
}
