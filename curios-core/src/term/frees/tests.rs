use {super::*, crate::Term, curios_utilities::Qualifier};

/// Every empty set is one allocation, however it was come to.
#[test]
fn empty_sets_share_one_canonical_set() {
    assert!(Frees::empty().shares(&Frees::union([])));
    assert!(Frees::union([Frees::empty(), Frees::empty()]).shares(&Frees::empty()));
}

/// A union with one carrying operand is that operand, shared: what lets a chain of links above one free occurrence hold one set.
#[test]
fn a_union_of_one_carrier_shares_it() {
    let carrier = Frees::single(Free::local(0, Some("x")));

    assert!(Frees::union([Frees::empty(), carrier.clone(), Frees::empty()]).shares(&carrier));
}

#[test]
fn a_union_of_two_carriers_holds_both_in_order() {
    let x = Free::local(0, Some("x"));
    let y = Free::local(1, Some("y"));

    assert_eq!(
        Frees::union([Frees::single(y), Frees::single(x)])
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
        .map(|index| Free::local(index, None))
        .collect::<Vec<_>>();
    let largest = Frees::union(names.iter().map(|name| Frees::single(*name)));
    let within = Frees::union([Frees::single(names[3]), Frees::single(names[7])]);

    assert!(Frees::union([within.clone(), largest.clone()]).shares(&largest));
    assert!(Frees::union([largest.clone(), within]).shares(&largest));

    let grown = Frees::union([largest.clone(), Frees::single(Free::local(42, None))]);
    assert_eq!(grown.len(), 11);
    assert!(!grown.shares(&largest));
}

/// The locals are read from the least of them, past every global.
#[test]
fn locals_are_read_past_the_globals() {
    let global = Free::global(Qualifier::from(["g"]));
    let local = Free::local(7, Some("l"));
    let set = Frees::union([Frees::single(local), Frees::single(global)]);

    assert_eq!(set.iter().copied().collect::<Vec<_>>(), [global, local]);
    assert_eq!(set.locals().copied().collect::<Vec<_>>(), [local]);
}

#[test]
fn a_filled_cache_answers_membership() {
    let name = Free::local(0, Some("x"));
    let cache = FreeCache::default();
    cache.fill(Frees::single(name));

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

/// Several names are answered as none where every one is a local newer than the newest a term keeps, and otherwise by the two sets; a global among them leaves the sets to answer.
#[test]
fn any_of_several_names_is_answered_by_their_oldest_local_first() {
    let older = Free::local(1, Some("a"));
    let newer = Free::local(2, Some("b"));
    let newest = Free::local(3, Some("c"));
    let global = Free::global(Qualifier::from(["g"]));

    assert!(!Term::free_var(&older).mentions_any(&Names::new([newer, newest])));
    assert!(Term::free_var(&newer).mentions_any(&Names::new([newer, newest])));
    assert!(Term::free_var(&global).mentions_any(&Names::new([global, newest])));
    assert!(!Term::free_var(&older).mentions_any(&Names::new([global, newest])));
    assert!(!Term::free_var(&older).mentions_any(&Names::new([])));
}
