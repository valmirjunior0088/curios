use super::*;

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
    cache.fill(FreeVars::Owned(BTreeSet::from([name.clone()])));

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
