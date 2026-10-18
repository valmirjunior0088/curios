//! The persistent maps against the standard ones: the same answers over the same inserts, and a version untouched by what is inserted into another.

use {
    super::*,
    std::collections::{BTreeMap, HashMap},
};

/// A seeded sequence of draws, so every run of a test meets the same operations.
struct Draws(u64);

impl Draws {
    fn next(&mut self, below: u64) -> u64 {
        self.0 = self
            .0
            .wrapping_mul(6_364_136_223_846_793_005)
            .wrapping_add(1_442_695_040_888_963_407);
        (self.0 >> 33) % below
    }
}

#[test]
fn a_persistent_map_answers_as_a_hash_map_does() {
    for seed in 0..8 {
        let mut draws = Draws(seed);
        let mut persistent = Persistent::new();
        let mut standard = HashMap::new();
        for step in 0..2_000 {
            let key = draws.next(300);
            match draws.next(4) {
                0 => assert_eq!(
                    persistent.remove(&key),
                    standard.remove(&key),
                    "seed {seed}"
                ),
                _ => {
                    persistent.insert(key, step);
                    standard.insert(key, step);
                }
            }
            let probe = draws.next(400);
            assert_eq!(persistent.get(&probe), standard.get(&probe), "seed {seed}");
        }
        assert_eq!(persistent.len(), standard.len());
        let mut entries = persistent
            .iter()
            .map(|(key, value)| (*key, *value))
            .collect::<Vec<_>>();
        entries.sort_unstable();
        let mut expected = standard.into_iter().collect::<Vec<_>>();
        expected.sort_unstable();
        assert_eq!(entries, expected, "seed {seed}");
    }
}

#[test]
fn an_ordered_map_answers_as_a_btree_map_does_in_its_order() {
    for seed in 0..8 {
        let mut draws = Draws(seed);
        let mut ordered = Ordered::new();
        let mut standard = BTreeMap::new();
        for step in 0..2_000 {
            let position = draws.next(600) as i64 - 300;
            match draws.next(4) {
                0 => assert_eq!(ordered.remove(position), standard.remove(&position)),
                _ => {
                    ordered.insert(position, step);
                    standard.insert(position, step);
                }
            }
            assert_eq!(ordered.get(position), standard.get(&position));
        }
        assert_eq!(ordered.len(), standard.len());
        assert_eq!(
            ordered
                .iter()
                .map(|(position, value)| (position, *value))
                .collect::<Vec<_>>(),
            standard.into_iter().collect::<Vec<_>>(),
            "seed {seed}"
        );
    }
}

/// Keys that all hash alike are held together and told apart by their equality.
#[derive(Clone, PartialEq, Eq)]
struct Colliding(u32);

impl Hash for Colliding {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u8(0);
    }
}

#[test]
fn keys_of_one_hash_are_told_apart_by_their_equality() {
    let mut map = Persistent::new();
    for key in 0..50 {
        map.insert(Colliding(key), key);
    }
    map.insert(Colliding(7), 700);

    assert_eq!(map.len(), 50);
    assert_eq!(map.get(&Colliding(7)), Some(&700));
    assert_eq!(map.get(&Colliding(8)), Some(&8));
    assert_eq!(map.get(&Colliding(50)), None);

    assert_eq!(map.remove(&Colliding(8)), Some(8));
    assert_eq!(map.remove(&Colliding(8)), None);
    assert_eq!(map.len(), 49);
    assert_eq!(map.get(&Colliding(9)), Some(&9));
}

#[test]
fn a_version_is_untouched_by_what_another_writes() {
    let mut first = Persistent::new();
    let mut positions = Ordered::new();
    for key in 0..100u32 {
        first.insert(key, key);
        positions.insert(i64::from(key), key);
    }

    let mut second = first.clone();
    let mut later = positions.clone();
    for key in 50..150u32 {
        second.insert(key, key + 1_000);
        later.insert(i64::from(key) - 200, key);
    }
    second.remove(&10);
    later.remove(10);

    assert_eq!(first.len(), 100);
    assert_eq!(first.get(&10), Some(&10));
    assert_eq!(second.get(&10), None);
    assert_eq!(positions.get(10), Some(&10));
    assert_eq!(later.get(10), None);
    assert_eq!(first.get(&60), Some(&60));
    assert_eq!(first.get(&120), None);
    assert_eq!(second.get(&60), Some(&1_060));
    assert_eq!(second.get(&120), Some(&1_120));
    assert_eq!(positions.len(), 100);
    assert_eq!(positions.iter().next(), Some((0, &0)));
    assert_eq!(later.iter().next(), Some((-150, &50)));
}

#[test]
fn a_sorted_map_answers_as_a_btree_map_does_in_its_order() {
    for seed in 0..8 {
        let mut draws = Draws(seed);
        let mut sorted = Sorted::new();
        let mut standard = BTreeMap::new();
        for step in 0..2_000 {
            let key = format!("k{}", draws.next(300));
            match draws.next(4) {
                0 => assert_eq!(sorted.remove(&key), standard.remove(&key), "seed {seed}"),
                _ => {
                    sorted.insert(key.clone(), step);
                    standard.insert(key, step);
                }
            }
        }
        assert_eq!(sorted.len(), standard.len());
        assert!(
            sorted.iter().eq(standard.iter()),
            "seed {seed}: the entries in the keys' order"
        );
    }
}

#[test]
fn a_sorted_version_is_untouched_by_what_another_takes() {
    let mut first = Sorted::new();
    for key in ["a", "b", "c"] {
        first.insert(key, key.len());
    }
    let mut second = first.clone();
    second.insert("d", 1);
    second.remove(&"a");

    assert!(first.iter().map(|(key, _)| *key).eq(["a", "b", "c"]));
    assert!(second.iter().map(|(key, _)| *key).eq(["b", "c", "d"]));
}

#[test]
fn an_ordered_map_names_the_positions_on_either_side_of_one() {
    let mut ordered = Ordered::new();
    for position in [-40, 0, 10, 30] {
        ordered.insert(position, position);
    }

    assert_eq!(ordered.before(10), Some((0, &0)));
    assert_eq!(ordered.after(10), Some((30, &30)));
    assert_eq!(ordered.before(5), Some((0, &0)));
    assert_eq!(ordered.after(5), Some((10, &10)));
    assert_eq!(ordered.before(-40), None);
    assert_eq!(ordered.after(30), None);
    assert_eq!(ordered.before(i64::MAX), Some((30, &30)));
}

/// A map read in key order, archived beside a field of its own.
#[curios_archive::archived]
struct Stored {
    #[archived_with(InKeyOrder)]
    map: Sorted<u32, String>,
}

/// A map read in key order is archived as its entries in that order and restored to the same entries, whatever order they were inserted in.
#[test]
fn a_sorted_map_is_restored_from_its_archive_in_key_order() {
    let mut map = Sorted::new();
    for key in [5, 1, 9, 3] {
        map.insert(key, key.to_string());
    }
    let bytes = curios_archive::to_bytes(&Stored { map }).expect("a map archives");
    let restored = curios_archive::from_bytes::<Stored>(&bytes).expect("a map restores");

    assert_eq!(
        restored
            .map
            .iter()
            .map(|(key, value)| (*key, value.clone()))
            .collect::<Vec<_>>(),
        [1, 3, 5, 9].map(|key| (key, key.to_string())),
    );
}
