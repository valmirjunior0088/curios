//! What a program's declarations elaborate to, read by more than one theme.

use {
    curios_core::{Program, Variance},
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

/// Every inductive family's variance vector as its unit carries it, keyed by the family's name: `*` for an irrelevant level and `=` for an invariant one.
pub(super) fn family_variances(source: &str) -> BTreeMap<String, String> {
    let entrypoint = source.parse::<Entrypoint>().expect("the fixture parses");
    let program: Program =
        typecheck_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none())
            .expect("the fixture type-checks")
            .program;

    program
        .module
        .induct_decls
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
