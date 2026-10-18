use super::*;

#[test]
fn starts_unfilled() {
    let cache = ScalarCache::default();

    assert!(!cache.is_filled());
    assert!(cache.get().is_none());
}

#[test]
fn round_trips_every_field() {
    let cache = ScalarCache::default();
    cache.fill(Scalars {
        reach: 123_456,
        newest_local: Some(41),
        has_metavar: false,
        has_transient: true,
        has_universe_meta: true,
        has_universe_data: false,
        has_group: false,
        hash: u64::MAX,
    });

    assert!(cache.is_filled());
    let read = cache.get().unwrap();
    assert_eq!(read.reach, 123_456);
    assert_eq!(read.newest_local, Some(41));
    assert!(!read.has_metavar);
    assert!(read.has_transient);
    assert!(read.has_universe_meta);
    assert!(!read.has_universe_data);
    assert!(!read.has_group);
    assert_eq!(read.hash, u64::MAX);
}

/// A zero hash and a zero reach are legitimate filled values: validity comes from the filled bit, never from a sentinel.
#[test]
fn zero_values_read_back_as_filled() {
    let cache = ScalarCache::default();
    cache.fill(Scalars {
        reach: 0,
        newest_local: None,
        has_metavar: true,
        has_transient: false,
        has_universe_meta: false,
        has_universe_data: true,
        has_group: true,
        hash: 0,
    });

    assert!(cache.is_filled());
    let read = cache.get().unwrap();
    assert_eq!(read.reach, 0);
    assert_eq!(read.newest_local, None);
    assert!(read.has_metavar);
    assert!(!read.has_transient);
    assert!(!read.has_universe_meta);
    assert!(read.has_universe_data);
    assert!(read.has_group);
    assert_eq!(read.hash, 0);
}

/// `reach` shares its word with the newest local, the flags and the filled bit, so its widest value has to read back with every flag set beside it — a shift that overlapped would show up here and nowhere else.
#[test]
fn the_widest_reach_reads_back_beside_every_flag() {
    let widest_reach = usize::try_from((1u64 << REACH_BITS) - 1).unwrap_or(usize::MAX);
    let cache = ScalarCache::default();
    cache.fill(Scalars {
        reach: widest_reach,
        newest_local: Some(41),
        has_metavar: true,
        has_transient: true,
        has_universe_meta: true,
        has_universe_data: true,
        has_group: true,
        hash: 7,
    });

    let read = cache.get().unwrap();
    assert_eq!(read.reach, widest_reach);
    assert_eq!(read.newest_local, Some(41));
    assert!(read.has_metavar && read.has_transient);
    assert!(read.has_universe_meta && read.has_universe_data);
}

/// A reach at the packed width is refused, in every build: packed unmasked it would read back truncated, an understated reach, which a pruning traversal trusts to pass over a subterm.
#[test]
#[should_panic(expected = "reach exceeds its packed width")]
fn a_reach_at_the_packed_width_is_refused() {
    let Ok(reach) = usize::try_from(1u64 << REACH_BITS) else {
        panic!("reach exceeds its packed width: no reach this host can spell is as wide");
    };

    ScalarCache::default().fill(Scalars {
        reach,
        newest_local: None,
        has_metavar: false,
        has_transient: false,
        has_universe_meta: false,
        has_universe_data: false,
        has_group: false,
        hash: 0,
    });
}

/// The newest local's widest packed index reads back as itself, and one past it as newer than any binder: never as an older one, which a capture would trust to pass over a subterm that holds the binder.
#[test]
fn a_newest_local_past_its_width_reads_back_as_newer_than_any() {
    let widest = NEWEST_PAST - 2;
    let fill = |newest_local| {
        let cache = ScalarCache::default();
        cache.fill(Scalars {
            reach: (1 << REACH_BITS) - 1,
            newest_local,
            has_metavar: true,
            has_transient: true,
            has_universe_meta: true,
            has_universe_data: true,
            has_group: true,
            hash: 0,
        });
        cache.get().unwrap()
    };

    assert_eq!(fill(Some(widest)).newest_local, Some(widest));
    assert_eq!(fill(Some(widest + 1)).newest_local, Some(u64::MAX));
    assert_eq!(fill(Some(u64::MAX)).newest_local, Some(u64::MAX));
    let read = fill(Some(0));
    assert_eq!(read.newest_local, Some(0));
    assert_eq!(read.reach, (1 << REACH_BITS) - 1);
    assert!(read.has_metavar && read.has_group);
}
