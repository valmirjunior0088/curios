//! Compiling a program end to end and reading the answer back: the harness every case in these suites asserts through.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use {
    crate::*,
    curios_core::{Item, Module},
    curios_elab::{Context, Resumed, erase_program},
    curios_prelude::with_prelude,
    curios_text::{Entrypoint, LintKind, RootSource, SYNTAX, UnitSource},
    curios_unit::{Predecessors, Unit},
    curios_utilities::{RootKind, test_support::Temporary},
    std::fs,
};

/// A fixture's entrypoint. Every fixture is a program, so its final term describes doing something and yielding nothing: one whose subject is a term binds it in a `let` at the type under test and closes with `Io/pure(())`.
pub(super) fn entrypoint_of(source: &str) -> Entrypoint {
    source.parse::<Entrypoint>().unwrap()
}

pub(super) fn compile(source: &str) -> Result<curios_wasm::Module, String> {
    let entrypoint = entrypoint_of(source);

    compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |_| {},
    )
    .map(|(module, _foreigns)| module)
    .map_err(String::from)
}

pub(super) fn compile_printed_stages(source: &str) -> Result<(String, String), String> {
    let entrypoint = entrypoint_of(source);
    let mut ersd = String::new();
    let mut cont = String::new();

    compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| match stage {
            Stage::Ersd(stage) => ersd = format!("{stage}"),
            Stage::Cont(stage) => cont = format!("{stage}"),
            _ => {}
        },
    )?;

    Ok((ersd, cont))
}

/// Whether a report spells a metavariable by its id — `?2659` — which no report should.
pub(super) fn mentions_metavar_id(report: &str) -> bool {
    report
        .chars()
        .zip(report.chars().skip(1))
        .any(|(a, b)| a == '?' && b.is_ascii_digit())
}

// --- A: typecheck-only (stop after zonk, no lowering) ---------------------

pub(super) fn typecheck(source: &str) -> Result<(), String> {
    let entrypoint = entrypoint_of(source);
    with_prelude(|prelude| {
        crate::elaborate_and_zonk(
            DEFAULT_STEP_BUDGET,
            Predecessors::over(prelude),
            &SYNTAX,
            &entrypoint,
            &RootSource::none(),
            crate::EntryTail::Authored,
            &mut |_| {},
        )
    })
    .map(|_| ())
    .map_err(String::from)
}

/// Elaborate `source` to its meta-free Core program and erase it as `compile_entrypoint` does — the archived erased prelude replayed, the program's own items erased onto it and its entry sealed — short of marking its functions' termination flags, which takes the kernel's records and is `erase_checked`'s.
///
/// Replayed rather than erased fresh: a compiled module carries the entry's own items alone, so a from-scratch erasure of it would leave every prelude name unbound. Replaying is also the path production takes, so what these tests exercise is what actually runs; erasing the prelude fresh is `erase_unit`'s job at archive-build time, where a failure panics the build.
pub(super) fn erase_to_ersd(source: &str) -> curios_ersd::Module {
    let entrypoint = entrypoint_of(source);
    let (program, _foreigns, _records) = with_prelude(|prelude| {
        crate::elaborate_and_zonk(
            DEFAULT_STEP_BUDGET,
            Predecessors::over(prelude),
            &SYNTAX,
            &entrypoint,
            &RootSource::none(),
            crate::EntryTail::Authored,
            &mut |_| {},
        )
    })
    .unwrap();
    let program = curios_core::Zonked::project(&program).expect("the elaborated program is zonked");
    with_prelude(|prelude| {
        let predecessors = Predecessors::over(prelude);
        erase_program(
            &mut Context::with_default_budget(SYNTAX),
            Resumed::of(&predecessors.cores(), predecessors.arena()),
            &program,
        )
    })
    .expect("the elaborated program erases into a verified erased module")
    .into_module()
}

// --- The unit boundary ----------------------------------------------------
//
// These tests come in a pair. A unit boundary is not packaging: it is where coherence is enforced, so the same three declarations are *refused* across units and *accepted* across modules of one unit. Either half alone proves nothing — the first could pass because the fixture is malformed, the second because the rule never ran.

/// Compile `sources` as units in order, then `entrypoint` as the entry against all of them.
pub(crate) fn compile_with_units(
    sources: &[(&str, &str)],
    entrypoint: &str,
) -> Result<curios_wasm::Module, String> {
    let parsed = sources
        .iter()
        .map(|(prefix, source)| {
            let mut modules = curios_text::RootSource::supplied();
            modules.insert_root(
                prefix,
                curios_utilities::RootKind::Ordinary,
                source
                    .parse::<curios_text::Module>()
                    .expect("a unit parses"),
            );
            modules
        })
        .collect::<Vec<_>>();
    let entry = entrypoint_of(entrypoint);

    with_prelude(|prelude| {
        let sources = parsed
            .iter()
            .map(|modules| (curios_text::UnitSource::mounted(modules), None))
            .collect::<Vec<_>>();
        let produced = compile_units(
            DEFAULT_STEP_BUDGET,
            Predecessors::over(prelude),
            &SYNTAX,
            &sources,
            None,
            |_| {},
        )?;
        let predecessors = prelude
            .iter()
            .copied()
            .chain(produced.iter())
            .collect::<Vec<_>>();

        compile_entrypoint(
            DEFAULT_STEP_BUDGET,
            Predecessors::over(&predecessors),
            &SYNTAX,
            &entry,
            &RootSource::none(),
            |_| {},
        )
        .map(|(module, _foreigns)| module)
    })
    .map_err(String::from)
}

// --- A unit over a baseline ------------------------------------------------

/// `source` as the modules of a unit mounted at `prefix`, supplied already parsed.
pub(super) fn mounted(prefix: &str, source: &str) -> RootSource {
    let mut modules = RootSource::supplied();
    modules.insert_root(
        prefix,
        RootKind::Ordinary,
        source
            .parse::<curios_text::Module>()
            .expect("a unit parses"),
    );

    modules
}

/// `source` written to a file and mounted at `prefix`, so it is read the way the compiler reads any module — the parser resynchronizing past an item it cannot read.
///
/// [`mounted`] is the strict `FromStr` spelling, which refuses such a text whole: right for a fixture written to compile, where a typo should fail at the fixture rather than as a diagnostic about a program nobody wrote, and wrong for one written *not* to. The guard comes back with the source because the file has to outlive every read of it.
pub(super) fn written(prefix: &str, source: &str) -> (Temporary, RootSource) {
    let directory = Temporary::new("pipeline", prefix);
    let header = directory.join("lib.crs");
    fs::create_dir_all(&*directory).expect("a fixture directory");
    fs::write(&header, source).expect("a fixture is written");

    let modules = RootSource::mounted(prefix, RootKind::Ordinary, header, directory.to_path_buf());

    (directory, modules)
}

/// `source` compiled whole as the unit `/lib`, against the prelude.
pub(super) fn unit_of(source: &str) -> Unit {
    compile_modules(&mounted("lib", source)).expect("the unit compiles")
}

/// The binders a unit's `unused-binder` lints name, once elaboration has credited them.
pub(super) fn unused_binders(unit: &Unit) -> Vec<String> {
    unit.text()
        .lints()
        .iter()
        .filter(|lint| lint.kind == LintKind::UnusedBinder)
        .map(|lint| lint.report.message.clone())
        .collect()
}

/// `source` compiled as the unit `/lib` over `baseline`, against the prelude.
pub(super) fn recompile_over(source: &str, baseline: &Unit) -> Result<Unit, String> {
    recompile_modules(&mounted("lib", source), baseline)
}

/// A whole compile of modules already supplied, handing back what it said rather than expecting it to compile — which is what a fixture written *not* to compile needs, and what lets its answer be held against the recompile's.
pub(super) fn compile_modules(modules: &RootSource) -> Result<Unit, String> {
    with_prelude(|prelude| {
        compile_units(
            DEFAULT_STEP_BUDGET,
            Predecessors::over(prelude),
            &SYNTAX,
            &[(UnitSource::mounted(modules), None)],
            None,
            |_| {},
        )
    })
    .map_err(String::from)
    .map(|mut units| units.pop().expect("one unit was compiled"))
}

/// Every unit `sources` holds, compiled in order against the prelude, each after the ones before it.
pub(super) fn compile_in_order(sources: &[&RootSource]) -> Vec<Unit> {
    with_prelude(|prelude| {
        compile_units(
            DEFAULT_STEP_BUDGET,
            Predecessors::over(prelude),
            &SYNTAX,
            &sources
                .iter()
                .map(|modules| (UnitSource::mounted(modules), None))
                .collect::<Vec<_>>(),
            None,
            |_| {},
        )
    })
    .expect("the units compile")
}

/// [`recompile_over`] over modules already supplied; the recompiling half of [`compile_modules`]'s differential.
pub(super) fn recompile_modules(modules: &RootSource, baseline: &Unit) -> Result<Unit, String> {
    with_prelude(|prelude| {
        compile_unit_over(
            DEFAULT_STEP_BUDGET,
            Predecessors::over(prelude),
            &SYNTAX,
            &UnitSource::mounted(modules),
            baseline,
        )
    })
    .map_err(String::from)
}

/// The differential predicate: the two elaborated modules agree item by item in order, on every registry entry, marker and the entry.
pub(super) fn assert_modules_agree(whole: &Module, incremental: &Module) {
    assert_eq!(
        whole.items.len(),
        incremental.items.len(),
        "the two compiles hold different item counts"
    );
    for (expected, actual) in whole.items.iter().zip(&incremental.items) {
        assert_eq!(
            expected.describe(),
            actual.describe(),
            "the item order differs"
        );
        assert_eq!(expected, actual, "{} differs", expected.describe());
    }
    assert_eq!(whole.mounts, incremental.mounts);
    assert_eq!(whole.induct_decls, incremental.induct_decls);
    assert_eq!(whole.struct_decls, incremental.struct_decls);
    assert_eq!(whole.concepts, incremental.concepts);
    assert_eq!(whole.witnesses, incremental.witnesses);
    assert_eq!(whole.tests, incremental.tests);
}

/// Whether `unit` holds the very allocation `baseline` holds for the body of the `let` named `name` — which nothing but reuse can produce, since every elaboration builds its own terms.
pub(super) fn reuses_body(baseline: &Unit, unit: &Unit, name: &str) -> bool {
    let body = |unit: &Unit| {
        unit.core()
            .items
            .iter()
            .find(|item| {
                item.declared_names()
                    .first()
                    .is_some_and(|declared| declared.symbol().ends_with(&format!("/{name}")))
            })
            .and_then(|item| match item {
                Item::Let(definition) => Some(definition.body.clone()),
                Item::Rec(_) => None,
            })
            .unwrap_or_else(|| panic!("{name} is a let item of the unit"))
    };
    let (before, after) = (body(baseline), body(unit));

    std::ptr::eq(&*before, &*after)
}
