//! How the fold takes a baseline through the cache seam: asked for on a miss alone, offered what the fold's assembler offers and taken only by a cache, announced as a recompile, and handed to `put` as any unit is.

use {
    super::test_support::{mounted, unit_of},
    crate::{After, Cache, DEFAULT_STEP_BUDGET, Earlier, Progress, compile_units},
    curios_prelude::with_prelude,
    curios_text::{SYNTAX, UnitSource},
    curios_unit::{Predecessors, Unit},
    std::cell::Cell,
};

const BASE: &str = "use /std/{Nat};\n\npub let answer: Nat = 42;\n";

/// A cache that hits with `hit`, answers a miss with `baseline` or else with whatever it was offered, and records what it is handed.
struct Stub {
    hit: Option<Unit>,
    baseline: Option<Unit>,
    /// How many units `put` was handed.
    put: Cell<usize>,
}

impl Cache for Stub {
    fn get(&self, _: &UnitSource<'_>) -> Option<Unit> {
        self.hit.clone()
    }

    fn baseline(&self, _: &UnitSource<'_>, offered: Option<&Unit>) -> Option<Earlier> {
        assert!(
            self.hit.is_none(),
            "a baseline is asked for on a miss alone"
        );
        self.baseline
            .clone()
            .or_else(|| offered.cloned())
            .map(|unit| Earlier {
                unit,
                after: After::Scope,
            })
    }

    fn put(&self, _: &UnitSource<'_>, _: &Unit) {
        self.put.set(self.put.get() + 1);
    }
}

/// What the fold reported for the one unit, folded through `cache`.
fn folded(cache: &dyn Cache) -> Vec<String> {
    folded_all(cache, &[("lib", BASE)])
}

/// What the fold reported for `units`, each a prefix and the source mounted at it, folded through `cache` in the order given with nothing offered.
fn folded_all(cache: &dyn Cache, units: &[(&str, &str)]) -> Vec<String> {
    folded_offered(Some(cache), units, None)
}

/// [`folded_all`], through a cache or none, with `offered` offered for the first unit alone.
fn folded_offered(
    cache: Option<&dyn Cache>,
    units: &[(&str, &str)],
    offered: Option<&Unit>,
) -> Vec<String> {
    let modules = units
        .iter()
        .map(|(prefix, source)| mounted(prefix, source))
        .collect::<Vec<_>>();
    let sources = modules
        .iter()
        .enumerate()
        .map(|(index, modules)| (UnitSource::mounted(modules), offered.filter(|_| index == 0)))
        .collect::<Vec<_>>();
    let mut events = Vec::new();

    with_prelude(|prelude| {
        compile_units(
            DEFAULT_STEP_BUDGET,
            Predecessors::over(prelude),
            &SYNTAX,
            &sources,
            cache,
            |progress| {
                events.push(match progress {
                    Progress::Compiling(prefix) => format!("compiling {}", prefix.join()),
                    Progress::Recompiling(prefix) => format!("recompiling {}", prefix.join()),
                    Progress::Reused(prefix) => format!("reused {}", prefix.join()),
                    Progress::Compiled => "compiled".to_string(),
                    Progress::Entry => "entry".to_string(),
                });
            },
        )
    })
    .expect("the units compile");

    events
}

#[test]
fn a_hit_is_reused_before_a_baseline_is_asked_for() {
    let stub = Stub {
        hit: Some(unit_of(BASE)),
        baseline: None,
        put: Cell::new(0),
    };

    assert_eq!(folded(&stub), ["reused /lib"]);
    assert_eq!(stub.put.get(), 0, "a hit is not handed back");
}

#[test]
fn a_miss_with_a_baseline_is_announced_as_a_recompile_and_handed_to_put() {
    let stub = Stub {
        hit: None,
        baseline: Some(unit_of(BASE)),
        put: Cell::new(0),
    };

    assert_eq!(folded(&stub), ["recompiling /lib", "compiled"]);
    assert_eq!(
        stub.put.get(),
        1,
        "what was compiled over a baseline is handed over like any unit"
    );
}

#[test]
fn a_miss_without_a_baseline_compiles_whole() {
    let stub = Stub {
        hit: None,
        baseline: None,
        put: Cell::new(0),
    };

    assert_eq!(folded(&stub), ["compiling /lib", "compiled"]);
    assert_eq!(stub.put.get(), 1);
}

/// An offered baseline is the cache's to take, and the fold takes nothing without one: a fold with no cache compiles every unit whole whatever it was offered.
#[test]
fn an_offered_baseline_is_taken_by_a_cache_and_by_nothing_else() {
    let offered = unit_of(BASE);
    let taking = Stub {
        hit: None,
        baseline: None,
        put: Cell::new(0),
    };

    assert_eq!(
        folded_offered(Some(&taking), &[("lib", BASE)], Some(&offered)),
        ["recompiling /lib", "compiled"]
    );
    assert_eq!(
        folded_offered(None, &[("lib", BASE)], Some(&offered)),
        ["compiling /lib", "compiled"],
        "no cache, no taker"
    );
}
