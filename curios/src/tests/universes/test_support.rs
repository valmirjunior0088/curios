//! What a program's declarations elaborate to, read by more than one theme.

use {
    curios_core::{Item, Program, Variance},
    curios_pipeline::{DEFAULT_STEP_BUDGET, typecheck_with_prelude},
    curios_text::{Entrypoint, RootSource},
    std::collections::BTreeMap,
};

/// Every definition's finalized universe parameter count, keyed by the name its item describes itself with.
pub(super) fn universe_parameters(source: &str) -> BTreeMap<String, usize> {
    let entrypoint = source.parse::<Entrypoint>().expect("the fixture parses");
    let program: Program =
        typecheck_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none())
            .expect("the fixture type-checks")
            .program;

    program
        .module
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
    let program: Program =
        typecheck_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none())
            .expect("the fixture type-checks")
            .program;

    program
        .module
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
    let program: Program =
        typecheck_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none())
            .expect("the fixture type-checks")
            .program;

    program
        .module
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
            (name.to_string(), spelled)
        })
        .collect()
}

/// Every definition's finalized type as it prints, universe instances included, keyed as [`universe_parameters`] keys it.
pub(super) fn signatures(source: &str) -> BTreeMap<String, String> {
    let entrypoint = source.parse::<Entrypoint>().expect("the fixture parses");
    let program: Program =
        typecheck_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none())
            .expect("the fixture type-checks")
            .program;

    program
        .module
        .items
        .iter()
        .flat_map(|item| {
            let described = item.describe();
            item.definitions()
                .into_iter()
                .map(move |definition| (described.clone(), definition.type_.to_string()))
        })
        .collect()
}

/// Every non-recursive definition's variance vector as its unit carries it, keyed as [`universe_parameters`] keys it: `*` for an irrelevant level and `=` for an invariant one, empty for a definition that takes none.
pub(super) fn definition_variances(source: &str) -> BTreeMap<String, String> {
    let entrypoint = source.parse::<Entrypoint>().expect("the fixture parses");
    let program: Program =
        typecheck_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none())
            .expect("the fixture type-checks")
            .program;

    program
        .module
        .items
        .iter()
        .filter_map(|item| match item {
            Item::Let(definition) => Some((item.describe(), definition)),
            Item::Rec(_) => None,
        })
        .map(|(described, definition)| {
            let spelled = definition
                .variances
                .iter()
                .map(|variance| match variance {
                    Variance::Irrelevant => '*',
                    Variance::Invariant => '=',
                })
                .collect();
            (described, spelled)
        })
        .collect()
}
