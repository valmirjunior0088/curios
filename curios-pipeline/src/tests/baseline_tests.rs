//! How the fold takes a baseline through the cache seam: asked for on a miss alone, offered what the fold's assembler offers and taken only by a cache, announced as a recompile, and handed to `put` as any unit is.

use {
    super::test_support::{compile, entrypoint_of, mounted, reuses_body, unit_of},
    crate::{
        After, Cache, DEFAULT_STEP_BUDGET, Earlier, Progress, compile_entrypoint, compile_units,
    },
    curios_prelude::with_prelude,
    curios_text::{RootSource, SYNTAX, UnitSource},
    curios_unit::{Predecessors, Unit},
    std::cell::{Cell, RefCell},
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

/// A cache as a session is one: it never hits, keeps the last unit it was handed, and offers it as the baseline of the next compile, and it counts what it was asked.
#[derive(Default)]
struct Keeping {
    kept: RefCell<Option<Unit>>,
    asked: Cell<usize>,
}

impl Cache for Keeping {
    fn get(&self, _: &UnitSource<'_>) -> Option<Unit> {
        self.asked.set(self.asked.get() + 1);
        None
    }

    fn baseline(&self, _: &UnitSource<'_>, _: Option<&Unit>) -> Option<Earlier> {
        self.kept.borrow().clone().map(|unit| Earlier {
            unit,
            after: After::Scope,
        })
    }

    fn put(&self, _: &UnitSource<'_>, unit: &Unit) {
        *self.kept.borrow_mut() = Some(unit.clone());
    }
}

/// A program of two declarations and a final term that reads both.
const PROGRAM: &str = "use /std/{Nat};

let double(n: Nat) -> Nat = n + n;

let seven: Nat = 7;

/std/print(/std/Nat/to_str(double(seven)))
";

/// `source` compiled as a program against the prelude, its declarations taken from `cache` and handed to it, to the emitted module as text.
fn program_through(source: &str, cache: &dyn Cache) -> String {
    let entrypoint = entrypoint_of(source);

    with_prelude(|prelude| {
        compile_entrypoint(
            DEFAULT_STEP_BUDGET,
            Predecessors::over(prelude),
            &SYNTAX,
            &UnitSource::entry(&entrypoint, &RootSource::none()),
            Some(cache),
            |_| {},
        )
    })
    .map(|(module, _foreigns)| module.to_string())
    .expect("the program compiles")
}

/// A program's declarations are a unit: asked of the cache, handed to it where they were compiled, and compiled over the one it kept. `double`'s body is edited, so it is elaborated again; `seven`, which the edit does not reach, is the declaration the kept unit holds; and the program is the one a compile through no cache emits.
///
/// Mutation-checked: with the entry compiled whole whatever the cache offers, `seven` is elaborated again.
#[test]
fn a_programs_declarations_are_compiled_over_the_unit_a_cache_kept() {
    let edited = PROGRAM.replace("n + n", "n + n + 0");
    let cache = Keeping::default();

    program_through(PROGRAM, &cache);
    assert_eq!(cache.asked.get(), 1, "the entry's unit was not asked for");
    let first = cache.kept.borrow().clone().expect("the entry's unit");

    let emitted = program_through(&edited, &cache);
    let second = cache.kept.borrow().clone().expect("the entry's unit");

    assert!(!reuses_body(&first, &second, "double"));
    assert!(
        reuses_body(&first, &second, "seven"),
        "a declaration the edit does not reach was elaborated again"
    );
    assert_eq!(
        emitted,
        compile(&edited).expect("the program compiles").to_string()
    );
}

/// Where a cache hands a program's unit back, nothing of the unit is compiled, the term that closes the program is compiled over it, and the program is the one a compile through no cache emits.
#[test]
fn a_program_whose_unit_a_cache_hands_back_is_the_program_compiled_whole() {
    let kept = Keeping::default();
    program_through(PROGRAM, &kept);
    let cache = Stub {
        hit: kept.kept.borrow().clone(),
        baseline: None,
        put: Cell::new(0),
    };

    let emitted = program_through(PROGRAM, &cache);

    assert_eq!(
        cache.put.get(),
        0,
        "a unit a cache handed back was handed to it again"
    );
    assert_eq!(
        emitted,
        compile(PROGRAM).expect("the program compiles").to_string()
    );
}

/// A program holding a refused declaration reports the refusal and what its final term says beside it, in one batch, as it did when a program was compiled whole: the term rides in the walk of the declarations.
#[test]
fn a_refused_declaration_is_reported_beside_what_the_final_term_says() {
    let source = "use /std/{Nat};

let wrong: Nat = \"a\";

/std/print(7)
";
    let cache = Keeping::default();
    let entrypoint = entrypoint_of(source);

    let refusal = with_prelude(|prelude| {
        compile_entrypoint(
            DEFAULT_STEP_BUDGET,
            Predecessors::over(prelude),
            &SYNTAX,
            &UnitSource::entry(&entrypoint, &RootSource::none()),
            Some(&cache),
            |_| {},
        )
    })
    .map(|_| ())
    .expect_err("neither the declaration nor the final term checks");

    assert_eq!(refusal.reports().len(), 2, "{refusal}");
    assert!(
        cache.kept.borrow().is_none(),
        "a refused program's unit was kept"
    );
}
