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
        footprint: 987_654,
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
    assert_eq!(read.footprint, 987_654);
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
        footprint: 0,
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
    assert_eq!(read.footprint, 0);
    assert_eq!(read.hash, 0);
}

/// The two packed figures share one word, so each has to keep its own widest value with the other at *its* widest — a shift that overlapped would show up here and nowhere else.
#[test]
fn reach_and_footprint_keep_their_widest_packed_values() {
    let widest_reach = usize::try_from((1u64 << REACH_BITS) - 1).unwrap_or(usize::MAX);
    let cache = ScalarCache::default();
    cache.fill(Scalars {
        reach: widest_reach,
        has_local_free: true,
        has_metavar: true,
        has_transient: true,
        has_universe_meta: true,
        has_universe_data: true,
        footprint: FOOTPRINT_MAX,
        hash: 7,
    });

    let read = cache.get().unwrap();
    assert_eq!(read.reach, widest_reach);
    assert_eq!(read.footprint, FOOTPRINT_MAX);
}

/// A footprint past its field is *clamped*, not wrapped — so an unmeasurably large term reads as "at least the maximum", which stops a retention insertion rather than admitting one. Wrapping would report a huge term as a tiny one, which is the direction that loses the bound.
#[test]
fn an_oversized_footprint_clamps_rather_than_wrapping() {
    let cache = ScalarCache::default();
    cache.fill(Scalars {
        reach: 0,
        has_local_free: false,
        has_metavar: false,
        has_transient: false,
        has_universe_meta: false,
        has_universe_data: false,
        footprint: u64::MAX,
        hash: 0,
    });

    assert_eq!(cache.get().unwrap().footprint, FOOTPRINT_MAX);
}
