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
        has_universe_meta: true,
        has_universe_data: false,
        hash: u64::MAX,
    });

    assert!(cache.is_filled());
    let read = cache.get().unwrap();
    assert_eq!(read.reach, 123_456);
    assert!(read.has_local_free);
    assert!(!read.has_metavar);
    assert!(read.has_universe_meta);
    assert!(!read.has_universe_data);
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
        has_universe_meta: false,
        has_universe_data: true,
        hash: 0,
    });

    assert!(cache.is_filled());
    let read = cache.get().unwrap();
    assert_eq!(read.reach, 0);
    assert!(!read.has_local_free);
    assert!(read.has_metavar);
    assert!(!read.has_universe_meta);
    assert!(read.has_universe_data);
    assert_eq!(read.hash, 0);
}

#[test]
fn reach_keeps_its_widest_packed_value() {
    let widest = usize::try_from((1u64 << 59) - 1).unwrap_or(usize::MAX);
    let cache = ScalarCache::default();
    cache.fill(Scalars {
        reach: widest,
        has_local_free: true,
        has_metavar: true,
        has_universe_meta: true,
        has_universe_data: true,
        hash: 7,
    });

    assert_eq!(cache.get().unwrap().reach, widest);
}
