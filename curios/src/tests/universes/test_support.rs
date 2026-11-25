//! What a program's declarations elaborate to, read by more than one theme.

use {
    curios_core::{Global, Item, Program, Term, UniverseContext, Variance},
    curios_elab::{Context, DEFAULT_BUDGET, Established, Tail, elaborate_and_zonk_entry},
    curios_module::{RootSource, SYNTAX, UnitSource, into_core_entry},
    curios_pipeline::test_support::*,
    curios_prelude::with_prelude,
    curios_surface::{Entrypoint, Spellable},
    std::collections::BTreeMap,
};

/// Each name `item` defines or declares a former under, with the scheme it is generalized under and its type: a former's as its entry makes it.
fn declared(item: &Item) -> Vec<(Global, UniverseContext, Term)> {
    let definitions = item.definitions().into_iter().map(|definition| {
        (
            definition.name,
            definition.universe_context,
            definition.type_,
        )
    });
    let formers = item
        .formers()
        .into_iter()
        .map(|former| (former.name, former.universe_context, former.type_));

    definitions.chain(formers).collect()
}

/// Every definition's and former's finalized universe parameter count, keyed by the name its item describes itself with.
pub fn universe_parameters(source: &str) -> BTreeMap<String, usize> {
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
            declared(item)
                .into_iter()
                .map(move |(_, context, _)| (described.clone(), context.parameter_count))
        })
        .collect()
}

/// How many sorts every definition leaves to its caller, keyed as [`universe_parameters`] keys it.
pub fn sort_parameters(source: &str) -> BTreeMap<String, usize> {
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
            declared(item)
                .into_iter()
                .map(move |(_, context, _)| (described.clone(), context.quality_count))
        })
        .collect()
}

/// Every family's variance vector as its unit carries it, an `induct`'s and a `struct`'s, keyed by the family's name: `*` for an irrelevant level and `=` for an invariant one.
pub fn family_variances(source: &str) -> BTreeMap<String, String> {
    let entrypoint = source.parse::<Entrypoint>().expect("the fixture parses");
    let program: Program = typecheck_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none())
        .expect("the fixture type-checks")
        .program;
    let spelled = |variances: &[Variance]| {
        variances
            .iter()
            .map(|variance| match variance {
                Variance::Irrelevant => '*',
                Variance::Invariant => '=',
            })
            .collect::<String>()
    };

    let inducts = program
        .definitions
        .induct_decls()
        .into_iter()
        .map(|(name, declaration)| {
            (
                name.faithful().to_string(),
                spelled(declaration.variances()),
            )
        });
    let structs = program
        .definitions
        .struct_decls()
        .into_iter()
        .map(|(name, declaration)| (name.faithful().to_string(), spelled(&declaration.variances)));
    inducts.chain(structs).collect()
}

/// Every definition's finalized type as it prints, universe instances included, keyed as [`universe_parameters`] keys it.
pub fn signatures(source: &str) -> BTreeMap<String, String> {
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
            declared(item)
                .into_iter()
                .map(move |(_, _, type_)| (described.clone(), type_.faithful().to_string()))
        })
        .collect()
}

/// Every definition's and every former's variance vector as its unit files it ([`curios_elab::Vectors`]), a recursive group's members each its own, keyed by the declaration as [`universe_parameters`] keys a `let`: `*` for an irrelevant level and `=` for an invariant one, empty for a declaration that takes none. A vector holding no irrelevant level is filed as none, and spelled as every level invariant.
///
/// The program's declarations are lowered and elaborated as the unit a compilation takes them as, over the prelude's roots, since the vectors are the unit's and no program carries them.
pub fn definition_variances(source: &str) -> BTreeMap<String, String> {
    let entrypoint = source.parse::<Entrypoint>().expect("the fixture parses");

    with_prelude(|prelude| {
        let texts = prelude
            .iter()
            .map(|unit| unit.text().expect("a program's unit holds its lowering"))
            .collect::<Vec<_>>();
        let (lowered, entry) = into_core_entry(
            &UnitSource::entry(&entrypoint, &RootSource::none()),
            &texts,
            &SYNTAX,
            None,
        )
        .expect("the fixture lowers");
        let placed = prelude
            .iter()
            .map(|unit| {
                (
                    unit.core(),
                    unit.places(),
                    unit.mounts(),
                    unit.classifications(),
                )
            })
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
            .flat_map(|item| {
                let defined = item.definitions().into_iter().map(|definition| {
                    (
                        Item::Let(Box::new(definition.clone())).describe(),
                        definition.name,
                        definition.universe_context,
                    )
                });
                let formers = item.formers().into_iter().map(|former| {
                    (
                        former.name.faithful().to_string(),
                        former.name,
                        former.universe_context,
                    )
                });
                defined.chain(formers).collect::<Vec<_>>()
            })
            .map(|(described, name, context)| {
                let spelled = match traces.classified(&name) {
                    Some(classified) if !classified.vectors.variances.is_empty() => classified
                        .vectors
                        .variances
                        .iter()
                        .map(|variance| match variance {
                            Variance::Irrelevant => '*',
                            Variance::Invariant => '=',
                        })
                        .collect(),
                    _ => "=".repeat(context.parameter_count),
                };
                (described, spelled)
            })
            .collect()
    })
}
