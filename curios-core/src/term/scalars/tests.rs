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
        has_local_free: true,
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
    assert!(read.has_local_free);
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
        has_local_free: false,
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
    assert!(!read.has_local_free);
    assert!(read.has_metavar);
    assert!(!read.has_transient);
    assert!(!read.has_universe_meta);
    assert!(read.has_universe_data);
    assert!(read.has_group);
    assert_eq!(read.hash, 0);
}

/// `reach` shares its word with the flags and the filled bit, so its widest value has to read back with every flag set beside it — a shift that overlapped would show up here and nowhere else.
#[test]
fn the_widest_reach_reads_back_beside_every_flag() {
    let widest_reach = usize::try_from((1u64 << REACH_BITS) - 1).unwrap_or(usize::MAX);
    let cache = ScalarCache::default();
    cache.fill(Scalars {
        reach: widest_reach,
        has_local_free: true,
        has_metavar: true,
        has_transient: true,
        has_universe_meta: true,
        has_universe_data: true,
        has_group: true,
        hash: 7,
    });

    let read = cache.get().unwrap();
    assert_eq!(read.reach, widest_reach);
    assert!(read.has_local_free && read.has_metavar && read.has_transient);
    assert!(read.has_universe_meta && read.has_universe_data);
}
