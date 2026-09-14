//! How the fold takes a baseline through the cache seam: asked for on a miss alone, announced as a recompile, and handed to `put` as any unit is.

use {
    super::test_support::{mounted, unit_of},
    crate::{Cache, DEFAULT_STEP_BUDGET, Progress, compile_units},
    curios_prelude::{SYNTAX, with_prelude},
    curios_text::UnitSource,
    curios_unit::{Prefix, Unit},
    std::cell::RefCell,
};

const BASE: &str = "use /std/{Nat};\n\npub let answer: Nat = 42;\n";

/// A cache that hits with `hit`, offers `baseline` on a miss, and counts what it is handed.
struct Stub {
    hit: Option<Unit>,
    baseline: Option<Unit>,
    put: RefCell<usize>,
}

impl Cache for Stub {
    fn get(&self, _: &UnitSource<'_>) -> Option<Unit> {
        self.hit.clone()
    }

    fn baseline(&self, _: &UnitSource<'_>, offered: Option<Unit>) -> Option<Unit> {
        assert!(
            self.hit.is_none(),
            "a baseline is asked for on a miss alone"
        );
        self.baseline.clone().or(offered)
    }

    fn put(&self, _: &UnitSource<'_>, _: &Unit) {
        *self.put.borrow_mut() += 1;
    }
}

/// What the fold reported for the one unit, folded through `cache`.
fn folded(cache: &dyn Cache) -> Vec<String> {
    let modules = mounted("lib", BASE);
    let mut events = Vec::new();

    with_prelude(|prelude| {
        compile_units(
            DEFAULT_STEP_BUDGET,
            Prefix::over(prelude),
            &SYNTAX,
            &[UnitSource::mounted(&modules)],
            Some(cache),
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
    .expect("the unit compiles");

    events
}

#[test]
fn a_hit_is_reused_before_a_baseline_is_asked_for() {
    let stub = Stub {
        hit: Some(unit_of(BASE)),
        baseline: None,
        put: RefCell::new(0),
    };

    assert_eq!(folded(&stub), ["reused /lib"]);
    assert_eq!(*stub.put.borrow(), 0, "a hit is not handed back");
}

#[test]
fn a_miss_with_a_baseline_is_announced_as_a_recompile_and_handed_to_put() {
    let stub = Stub {
        hit: None,
        baseline: Some(unit_of(BASE)),
        put: RefCell::new(0),
    };

    assert_eq!(folded(&stub), ["recompiling /lib", "compiled"]);
    assert_eq!(
        *stub.put.borrow(),
        1,
        "what was compiled over a baseline is placed like any unit"
    );
}

#[test]
fn a_miss_without_a_baseline_compiles_whole() {
    let stub = Stub {
        hit: None,
        baseline: None,
        put: RefCell::new(0),
    };

    assert_eq!(folded(&stub), ["compiling /lib", "compiled"]);
    assert_eq!(*stub.put.borrow(), 1);
}
