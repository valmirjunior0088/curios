//! Compiling a program end to end and reading the answer back: the harness every case in these suites asserts through.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use {
    crate::*,
    curios_elab::{Context, Resumed, erase_unit},
    curios_prelude::{SYNTAX, with_prelude},
    curios_text::{Entrypoint, RootSource},
    curios_unit::Prefix,
    std::slice::from_ref,
};

/// A fixture's entrypoint, stating its own type when the fixture is a bare *term* rather than a program.
///
/// A program's tail describes doing something and yielding nothing, so an entrypoint carrying no type is checked against `Io({})` (`elaborate_and_zonk`). Most fixtures here are terms — they end in the `Nat` or `List` the feature under test produces — and stating the type is exactly the embedder path that contract leaves open, so each keeps compiling the term it was written to compile rather than acquiring a tail that would change what it measures.
pub(super) fn with_entrypoint_type(source: &str, type_: Option<&str>) -> Entrypoint {
    let entrypoint = source.parse::<Entrypoint>().unwrap();

    match type_ {
        Some(type_) => entrypoint.with_type(type_.parse().unwrap()),
        None => entrypoint,
    }
}

pub(super) fn compile(source: &str, type_: Option<&str>) -> Result<curios_wasm::Module, String> {
    let entrypoint = with_entrypoint_type(source, type_);

    compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |_| {},
    )
    .map(|(module, _foreigns)| module)
    .map_err(String::from)
}

pub(super) fn compile_printed_stages(
    source: &str,
    type_: Option<&str>,
) -> Result<(String, String), String> {
    let entrypoint = with_entrypoint_type(source, type_);
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

pub(super) fn typecheck(source: &str, type_: Option<&str>) -> Result<(), String> {
    let entrypoint = with_entrypoint_type(source, type_);
    with_prelude(|prelude| {
        crate::elaborate_and_zonk(
            DEFAULT_STEP_BUDGET,
            Prefix::over(from_ref(&prelude)),
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

/// Elaborate `source` to its meta-free Core module and erase it exactly as `compile_entrypoint` does — the archived erased prelude replayed, the entry's own items erased onto it.
///
/// It used to erase *fresh*, passing the whole module to `erase_module`, which worked only because a compiled module carried the prelude spliced into its items. It no longer does, and a from-scratch erasure of the entry alone leaves every prelude name unbound. Replaying is also the path production takes, so what these tests exercise is what actually runs; erasing the prelude fresh is `erase_unit`'s job at archive-build time, where a failure panics the build.
pub(super) fn erase_to_ersd(source: &str, type_: Option<&str>) -> curios_ersd::Module {
    let entrypoint = with_entrypoint_type(source, type_);
    let (module, core_type, _foreigns) = with_prelude(|prelude| {
        crate::elaborate_and_zonk(
            DEFAULT_STEP_BUDGET,
            Prefix::over(from_ref(&prelude)),
            &SYNTAX,
            &entrypoint,
            &RootSource::none(),
            crate::EntryTail::Authored,
            &mut |_| {},
        )
    })
    .unwrap();
    let module = curios_core::Zonked::project(&module).expect("the elaborated module is zonked");
    with_prelude(|prelude| {
        erase_unit(
            &mut Context::with_default_budget(SYNTAX),
            Resumed::of(from_ref(&prelude.core()), prelude.arena()),
            &module,
            Some(&core_type),
        )
    })
    .expect("the elaborated module erases into a verified erased module")
    .into_module()
}

// --- The unit boundary ----------------------------------------------------
//
// These are the tests the specification insists come in a pair. A unit boundary is not packaging: it is where coherence is enforced, so the same three declarations are *refused* across units and *accepted* across modules of one unit. Either half alone proves nothing — the first could pass because the fixture is malformed, the second because the rule never ran.

/// Compile `sources` as units in order, then `entrypoint` as the entry against all of them.
pub(super) fn compile_with_units(
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
    let entry = with_entrypoint_type(entrypoint, None);

    with_prelude(|prelude| {
        let sources = parsed
            .iter()
            .map(curios_text::UnitSource::mounted)
            .collect::<Vec<_>>();
        let produced = compile_units(
            DEFAULT_STEP_BUDGET,
            Prefix::over(from_ref(&prelude)),
            &SYNTAX,
            &sources,
            None,
            |_| {},
        )?;
        let scope = std::iter::once(prelude)
            .chain(produced.iter())
            .collect::<Vec<_>>();

        compile_entrypoint(
            DEFAULT_STEP_BUDGET,
            Prefix::over(&scope),
            &SYNTAX,
            &entry,
            &RootSource::none(),
            |_| {},
        )
        .map(|(module, _foreigns)| module)
    })
    .map_err(String::from)
}
