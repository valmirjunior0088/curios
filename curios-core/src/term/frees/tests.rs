use {super::*, crate::Term, curios_utilities::Qualifier};

#[test]
fn empty_fills_share_one_canonical_set() {
    let first = FreeCache::default();
    let second = FreeCache::default();
    first.fill(FreeVars::Owned(BTreeSet::new()));
    second.fill(FreeVars::Owned(BTreeSet::new()));

    assert!(Rc::ptr_eq(first.get().unwrap(), second.get().unwrap()));
}

#[test]
fn nonempty_fills_keep_their_own_set() {
    let name = Free::local(0, Some("x"));
    let cache = FreeCache::default();
    cache.fill(FreeVars::Owned(BTreeSet::from([name])));

    assert!(cache.is_filled());
    assert!(cache.contains(&name));
    assert_eq!(cache.get().unwrap().len(), 1);
}

#[test]
fn shared_fills_keep_the_carrier_allocation() {
    let name = Free::local(0, Some("x"));
    let carrier = Rc::new(BTreeSet::from([name]));
    let cache = FreeCache::default();
    cache.fill(FreeVars::Shared(Rc::clone(&carrier)));

    assert!(Rc::ptr_eq(cache.get().unwrap(), &carrier));
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
