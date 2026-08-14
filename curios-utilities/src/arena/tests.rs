use super::*;

// Hand-rolled rather than `id!`-minted: the macro's `pub` items trip `unreachable_pub` inside a test module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct ProbeId(u32);

impl std::fmt::Display for ProbeId {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "~probe{}", self.0)
    }
}

impl ArenaId for ProbeId {
    fn from_index(index: usize) -> Self {
        Self(u32::try_from(index).expect("ProbeId identity space exhausted"))
    }

    fn index(self) -> usize {
        self.0 as usize
    }
}

#[test]
fn mint_and_get_round_trip() {
    let mut arena: Arena<ProbeId, &str> = Arena::new();
    let first = arena.mint("first");
    let second = arena.mint("second");

    assert_eq!(first.index(), 0);
    assert_eq!(second.index(), 1);
    assert_eq!(arena.get(first), Some(&"first"));
    assert_eq!(arena.get(second), Some(&"second"));
    assert_eq!(arena.len(), 2);
    assert_eq!(arena.live_count(), 2);
    assert_eq!(arena.tombstone_count(), 0);
}

#[test]
fn reserve_then_define_fills_the_slot_once() {
    let mut arena: Arena<ProbeId, u32> = Arena::new();
    let reserved = arena.reserve();

    assert_eq!(arena.get(reserved), None);
    arena.define(reserved, 7);
    assert_eq!(arena.get(reserved), Some(&7));
}

#[test]
#[should_panic(expected = "is already defined")]
fn define_refuses_a_live_slot() {
    let mut arena: Arena<ProbeId, u32> = Arena::new();
    let id = arena.mint(1);
    arena.define(id, 2);
}

#[test]
#[should_panic(expected = "is not live")]
fn set_refuses_a_reserved_slot() {
    let mut arena: Arena<ProbeId, u32> = Arena::new();
    let id = arena.reserve();
    arena.set(id, 2);
}

#[test]
fn remove_tombstones_without_moving_identities() {
    let mut arena: Arena<ProbeId, u32> = Arena::new();
    let first = arena.mint(1);
    let second = arena.mint(2);

    assert_eq!(arena.remove(first), Some(1));
    assert_eq!(arena.get(first), None);
    assert_eq!(arena.get(second), Some(&2));
    assert_eq!(arena.len(), 2);
    assert_eq!(arena.tombstone_count(), 1);

    // The identity space keeps growing past the tombstone; nothing is reused.
    let third = arena.mint(3);
    assert_eq!(third.index(), 2);
}

#[test]
fn retain_keeps_exactly_the_named_ids() {
    let mut arena: Arena<ProbeId, u32> = Arena::new();
    let first = arena.mint(1);
    let second = arena.mint(2);
    let third = arena.mint(3);

    arena.retain(&std::collections::BTreeSet::from([first, third]));

    assert_eq!(arena.get(first), Some(&1));
    assert_eq!(arena.get(second), None);
    assert_eq!(arena.get(third), Some(&3));
    assert_eq!(
        arena
            .iter_live()
            .map(|(_, value)| *value)
            .collect::<Vec<_>>(),
        vec![1, 3]
    );
    assert_eq!(arena.live_ids().collect::<Vec<_>>(), vec![first, third]);
}
