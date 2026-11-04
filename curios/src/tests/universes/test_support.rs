//! What a program's declarations elaborate to, read by more than one theme.

use {
    curios_core::{Item, Program, Variance},
    curios_elab::{Context, Established, Tail, elaborate_and_zonk_entry},
    curios_module::{RootSource, SYNTAX, UnitSource, into_core_entry},
    curios_pipeline::{DEFAULT_BUDGET, test_support::*},
    curios_prelude::with_prelude,
    curios_surface::{Entrypoint, Spellable},
    std::collections::BTreeMap,
};

/// Every definition's finalized universe parameter count, keyed by the name its item describes itself with.
pub(super) fn universe_parameters(source: &str) -> BTreeMap<String, usize> {
    let entrypoint = source.parse::<Entrypoint>().expect("the fixture parses");
    let program: Program = typecheck_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none())
        .expect("the fixture type-checks")
        .program;

    program
        .definitions
        .items
        .iter()
        .flat_map(|item| {
            let described = item.describe();
            item.definitions().into_iter().map(move |definition| {
                (
                    described.clone(),
                    definition.universe_context.parameter_count,
                )
            })
        })
        .collect()
}

/// How many sorts every definition leaves to its caller, keyed as [`universe_parameters`] keys it.
pub(super) fn sort_parameters(source: &str) -> BTreeMap<String, usize> {
    let entrypoint = source.parse::<Entrypoint>().expect("the fixture parses");
    let program: Program = typecheck_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none())
        .expect("the fixture type-checks")
        .program;

    program
        .definitions
        .items
        .iter()
        .flat_map(|item| {
            let described = item.describe();
            item.definitions().into_iter().map(move |definition| {
                (described.clone(), definition.universe_context.quality_count)
            })
        })
        .collect()
}

/// Every inductive family's variance vector as its unit carries it, keyed by the family's name: `*` for an irrelevant level and `=` for an invariant one.
pub(super) fn family_variances(source: &str) -> BTreeMap<String, String> {
    let entrypoint = source.parse::<Entrypoint>().expect("the fixture parses");
    let program: Program = typecheck_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none())
        .expect("the fixture type-checks")
        .program;

    program
        .definitions
        .induct_decls()
        .iter()
        .map(|(name, declaration)| {
            let spelled = declaration
                .variances
                .iter()
                .map(|variance| match variance {
                    Variance::Irrelevant => '*',
                    Variance::Invariant => '=',
                })
                .collect();
            (name.faithful().to_string(), spelled)
        })
        .collect()
}

/// Every definition's finalized type as it prints, universe instances included, keyed as [`universe_parameters`] keys it.
pub(super) fn signatures(source: &str) -> BTreeMap<String, String> {
    let entrypoint = source.parse::<Entrypoint>().expect("the fixture parses");
    let program: Program = typecheck_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none())
        .expect("the fixture type-checks")
        .program;

    program
        .definitions
        .items
        .iter()
        .flat_map(|item| {
            let described = item.describe();
            item.definitions()
                .into_iter()
                .map(move |definition| (described.clone(), definition.type_.faithful().to_string()))
        })
        .collect()
}

/// Every non-recursive definition's variance vector as its unit files it ([`curios_elab::Vectors`]), keyed as [`universe_parameters`] keys it: `*` for an irrelevant level and `=` for an invariant one, empty for a definition that takes none. A vector holding no irrelevant level is filed as none, and spelled as every level invariant.
///
/// The program's declarations are lowered and elaborated as the unit a compilation takes them as, over the prelude's roots, since the vectors are the unit's and no program carries them.
pub(super) fn definition_variances(source: &str) -> BTreeMap<String, String> {
    let entrypoint = source.parse::<Entrypoint>().expect("the fixture parses");

    with_prelude(|prelude| {
        let texts = prelude.iter().map(|unit| unit.text()).collect::<Vec<_>>();
        let (lowered, entry) = into_core_entry(
            &UnitSource::entry(&entrypoint, &RootSource::none()),
            &texts,
            &SYNTAX,
            None,
        )
        .expect("the fixture lowers");
        let placed = prelude
            .iter()
            .map(|unit| (unit.core(), unit.places(), unit.mounts(), unit.traces()))
            .collect::<Vec<_>>();
        let established = Established::placed(&placed);
        let mut context = Context::new(DEFAULT_BUDGET, SYNTAX);
        context.set_imports(lowered.imports().clone());
        let (definitions, _, traces) = elaborate_and_zonk_entry(
            &mut context,
            &established,
            lowered.core(),
            lowered.mounts(),
            lowered.minted(),
            Tail::Entry(&entry),
        )
        .expect("the fixture elaborates");

        definitions
            .items
            .iter()
            .filter_map(|item| match item {
                Item::Let(definition) => Some((item.describe(), definition)),
                Item::Rec(_) => None,
            })
            .map(|(described, definition)| {
                let spelled = match traces.classified(&definition.name) {
                    Some(classified) if !classified.vectors.variances.is_empty() => classified
                        .vectors
                        .variances
                        .iter()
                        .map(|variance| match variance {
                            Variance::Irrelevant => '*',
                            Variance::Invariant => '=',
                        })
                        .collect(),
                    _ => "=".repeat(definition.universe_context.parameter_count),
                };
                (described, spelled)
            })
            .collect()
    })
}
