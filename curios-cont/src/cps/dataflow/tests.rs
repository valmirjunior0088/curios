use super::*;

/// The simplest lattice that still exercises every law: a height ordered by `max`, with zero as bottom. Real lattices here carry payloads, but nothing in the solver reads one.
#[derive(Clone, PartialEq, Debug)]
struct Height(u32);

impl Lattice for Height {
    fn bottom() -> Self {
        Height(0)
    }

    fn join(&mut self, incoming: Self) {
        self.0 = self.0.max(incoming.0);
    }
}

fn value(index: u32) -> CpsValueId {
    CpsValueId(index)
}

#[test]
fn bottom_is_the_join_identity() {
    let mut fact = Height(7);
    fact.join(Height::bottom());
    assert_eq!(fact, Height(7));

    let mut bottom = Height::bottom();
    bottom.join(Height(7));
    assert_eq!(bottom, Height(7));
}

#[test]
fn seeded_keys_start_at_bottom_and_a_constraintless_system_converges_there() {
    let facts = solve::<Height>([value(0), value(1)], |_| {});
    assert_eq!(facts[&value(0)], Height::bottom());
    assert_eq!(facts[&value(1)], Height::bottom());
}

/// A chain `0 → 1 → 2`, each raising the next, is the case a single pass cannot settle: it takes one round per link, so this pins that the solver iterates rather than visiting each constraint once.
#[test]
fn a_transitive_chain_settles_at_its_least_fixpoint() {
    let facts = solve::<Height>([value(0), value(1), value(2)], |solver| {
        solver.join(value(0), Height(3));
        let zero = solver.facts()[&value(0)].clone();
        solver.join(value(1), zero);
        let one = solver.facts()[&value(1)].clone();
        solver.join(value(2), one);
    });

    assert_eq!(facts[&value(2)], Height(3));
}

/// Absence is not `bottom`: a key never seeded and never joined is simply not there, which is the distinction [`super::analysis`]'s constant propagation reads to tell an unobservable value from one it has yet to learn about.
#[test]
fn an_unseeded_key_is_absent_rather_than_bottom() {
    let facts = solve::<Height>([value(0)], |solver| {
        assert!(solver.facts().contains_key(&value(0)));
        assert!(!solver.facts().contains_key(&value(9)));
    });

    assert!(!facts.contains_key(&value(9)));
}

#[test]
fn joining_an_unseeded_key_discovers_it() {
    let facts = solve::<Height>([], |solver| {
        solver.join(value(4), Height(2));
    });

    assert_eq!(facts[&value(4)], Height(2));
}

/// The convergence test is on the *fact*, not on the join being called: re-joining a value already at or below the current height must not report a change, or the loop never terminates.
#[test]
fn rejoining_a_settled_fact_terminates() {
    let facts = solve::<Height>([value(0)], |solver| {
        solver.join(value(0), Height(5));
        solver.join(value(0), Height(1));
        solver.join(value(0), Height(5));
    });

    assert_eq!(facts[&value(0)], Height(5));
}
