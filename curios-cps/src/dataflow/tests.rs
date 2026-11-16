use {super::*, std::cell::Cell};

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

fn value(index: u32) -> ValueId {
    ValueId(index)
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
fn seeded_keys_start_at_bottom_and_a_ruleless_system_converges_there() {
    let facts = Solver::<Height>::solve([value(0), value(1)], |_| {});
    assert_eq!(facts[&value(0)], Height::bottom());
    assert_eq!(facts[&value(1)], Height::bottom());
}

/// A chain `0 → 1 → 2` stated last link first is the case running each rule once cannot settle: it pins that a rule runs again where a fact it read moved.
#[test]
fn a_transitive_chain_settles_at_its_least_fixpoint() {
    let facts = Solver::<Height>::solve([value(0), value(1), value(2)], |rules| {
        rules.add(|solver| {
            let one = solver.fact(&value(1)).cloned().expect("seeded");
            solver.join(value(2), one);
        });
        rules.add(|solver| {
            let zero = solver.fact(&value(0)).cloned().expect("seeded");
            solver.join(value(1), zero);
        });
        rules.add(|solver| solver.join(value(0), Height(3)));
    });

    assert_eq!(facts[&value(2)], Height(3));
}

/// A chain stated against the order its facts flow runs each of its rules once more than it settles, where running every rule until none changes a fact runs the whole chain once for each of its links.
///
/// Mutation-checked: with every rule queued again at any move, the chain's rules run the square of its length.
#[test]
fn a_rule_runs_again_only_where_a_fact_it_read_moved() {
    let length = 100;
    let runs = Cell::new(0);
    let facts = Solver::<Height>::solve((0..=length).map(value), |rules| {
        for link in (0..length).rev() {
            let runs = &runs;
            rules.add(move |solver| {
                runs.set(runs.get() + 1);
                let incoming = solver.fact(&value(link)).cloned().expect("seeded");
                solver.join(value(link + 1), incoming);
            });
        }
        rules.add(|solver| solver.join(value(0), Height(1)));
    });

    assert_eq!(facts[&value(length)], Height(1));
    assert!(
        runs.get() <= 2 * length,
        "the chain's rules ran {} times",
        runs.get()
    );
}

/// Absence is not `bottom`: a key never seeded and never joined is simply not there, which is the distinction [`super::analysis`]'s constant propagation reads to tell an unobservable value from one it has yet to learn about.
#[test]
fn an_unseeded_key_is_absent_rather_than_bottom() {
    let facts = Solver::<Height>::solve([value(0)], |rules| {
        rules.add(|solver| {
            assert!(solver.fact(&value(0)).is_some());
            assert!(solver.fact(&value(9)).is_none());
        });
    });

    assert!(!facts.contains_key(&value(9)));
}

#[test]
fn distinct_key_kinds_propagate_without_aliasing_or_requiring_copy() {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
    enum Key {
        Value(String),
        Return(String, usize),
    }

    let value = || Key::Value("f".into());
    let returned = |position| Key::Return("f".into(), position);
    let facts = Solver::<Height, Key>::solve([value(), returned(0), returned(1)], |rules| {
        rules.add(move |solver| {
            let forwarded = solver.fact(&value()).cloned().expect("seeded");
            solver.join(returned(1), forwarded);
            let incoming = solver.fact(&returned(0)).cloned().expect("seeded");
            solver.join(value(), incoming);
            solver.join(returned(0), Height(7));
        });
    });

    assert_eq!(facts[&value()], Height(7));
    assert_eq!(facts[&returned(1)], Height(7));
    assert!(!facts.contains_key(&returned(2)));
}

#[test]
fn joining_an_unseeded_key_discovers_it() {
    let facts = Solver::<Height>::solve([], |rules| {
        rules.add(|solver| solver.join(value(4), Height(2)));
    });

    assert_eq!(facts[&value(4)], Height(2));
}

/// The convergence test is on the *fact*, not on the join being called: a rule that reads its own target and joins it at or below the height it holds must not be queued again, or the solve never ends.
#[test]
fn rejoining_a_settled_fact_terminates() {
    let facts = Solver::<Height>::solve([value(0)], |rules| {
        rules.add(|solver| {
            let _ = solver.fact(&value(0));
            solver.join(value(0), Height(5));
            solver.join(value(0), Height(1));
            solver.join(value(0), Height(5));
        });
    });

    assert_eq!(facts[&value(0)], Height(5));
}

/// A set of members joined by union, which answers the members a join gained, as a client over sets answers.
#[derive(Clone, PartialEq, Debug, Default)]
struct Members(BTreeSet<u32>);

thread_local! {
    /// How many members the gains of a solve were handed, together.
    static HANDED: Cell<usize> = const { Cell::new(0) };
}

impl Lattice for Members {
    fn bottom() -> Self {
        Members::default()
    }

    fn join(&mut self, incoming: Self) {
        self.0.extend(incoming.0);
    }

    fn gain(&mut self, incoming: &Self) -> Option<Self> {
        HANDED.with(|handed| handed.set(handed.get() + incoming.0.len()));
        let gained = incoming
            .0
            .difference(&self.0)
            .copied()
            .collect::<BTreeSet<_>>();
        self.0.extend(&gained);
        (!gained.is_empty()).then_some(Members(gained))
    }
}

fn members(items: impl IntoIterator<Item = u32>) -> Members {
    Members(items.into_iter().collect())
}

/// Members sent into the head of a chain of copies one at a time each cross every copy once, however many the chain holds already.
///
/// Mutation-checked: with a copy handing on the whole fact its source holds, the chain is handed the square of the members' count.
#[test]
fn a_copy_hands_on_only_what_its_source_gained() {
    let (length, count) = (20, 50);
    HANDED.with(|handed| handed.set(0));
    let facts = Solver::<Members>::solve([], |rules| {
        rules.add(move |solver| {
            for link in 0..length {
                solver.copy(value(link), value(link + 1));
            }
            solver.join(value(0), members([0]));
        });
        // Each member reaching the tail sends the next one in at the head.
        rules.watch(value(length), move |solver, gained| {
            let next = gained.0.last().expect("a gain holds a member") + 1;
            if next < count {
                solver.join(value(0), members([next]));
            }
        });
    });

    assert_eq!(facts[&value(length)], members(0..count));
    let handed = HANDED.with(Cell::get);
    assert!(
        handed <= 2 * (length + 1) as usize * count as usize,
        "the chain was handed {handed} members"
    );
}

/// A watch is handed all its key comes to hold, each member once, though members reach the key by two routes.
#[test]
fn a_watch_is_handed_each_member_once() {
    let handed = RefCell::new(Vec::new());
    let facts = Solver::<Members>::solve([value(0)], |rules| {
        rules.add(|solver| {
            solver.copy(value(1), value(0));
            solver.copy(value(2), value(0));
            solver.join(value(1), members([1, 2]));
            solver.join(value(2), members([2, 3]));
        });
        rules.watch(value(0), |_, gained| {
            handed.borrow_mut().extend(gained.0.iter().copied());
        });
    });

    let mut handed = handed.into_inner();
    handed.sort_unstable();
    assert_eq!(handed, [1, 2, 3]);
    assert_eq!(facts[&value(0)], members([1, 2, 3]));
}

/// A copy stated once its source holds members hands them all on, and every gain of the source after them.
#[test]
fn a_copy_stated_late_hands_on_what_its_source_holds() {
    let facts = Solver::<Members>::solve([], |rules| {
        rules.add(|solver| solver.join(value(0), members([1, 2])));
        rules.watch(value(0), |solver, _| solver.copy(value(0), value(1)));
        rules.watch(value(1), |solver, gained| {
            if !gained.0.contains(&3) {
                solver.join(value(0), members([3]));
            }
        });
    });

    assert_eq!(facts[&value(1)], members([1, 2, 3]));
}

/// A copy inserts its target as a join does, and a source nothing wrote stays absent.
#[test]
fn a_copy_from_an_absent_key_inserts_its_target_at_bottom() {
    let facts = Solver::<Members>::solve([], |rules| {
        rules.add(|solver| solver.copy(value(9), value(1)));
    });

    assert_eq!(facts[&value(1)], Members::bottom());
    assert!(!facts.contains_key(&value(9)));
}

/// What a copy hands on moves its target as a join would, so a rule that read the target runs again.
#[test]
fn a_rule_reading_a_copied_key_runs_again_where_the_copy_moves_it() {
    let facts = Solver::<Members>::solve([value(1)], |rules| {
        rules.add(|solver| {
            let held = solver.fact(&value(1)).cloned().expect("seeded");
            solver.join(value(2), held);
        });
        rules.add(|solver| {
            solver.copy(value(0), value(1));
            solver.join(value(0), members([5]));
        });
    });

    assert_eq!(facts[&value(2)], members([5]));
}
