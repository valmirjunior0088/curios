//! The elaborator's driver for the shared variance analysis: when it runs, and where its vectors live.
//!
//! The analysis itself lives in `curios-analysis/src/variance.rs` (see that module for the rule and its rationale) and is run by both checkers. It runs here twice, for one reason each. When a family's group finalizes, because a later item of the same unit may compare two of its instances: that vector is kept on the context's registry entry, and it is the one the unit carries. And over the zonked module, where the telescopes are final and meta-free: that run writes nothing and holds the carried vector to what the declarations say.

use {
    super::{Context, Error, zonk_solved_term_metas},
    curios_analysis::{Declarations, VarianceVectors, variance_vectors},
    curios_core::{Global, InductDecl, InductParam, Module, StructDecl, Variance},
    std::collections::BTreeMap,
};

/// Record each of `names`' variance vectors on its registry entry: the families of one group, analyzed together as that group finalizes, so a later item that compares two of their instances reads them.
///
/// Every earlier family's vector answers from the registry. A solved metavariable is spliced in first; one still unsolved is read as mentioning every level, its solution being free to. A reduction the budget refuses reads the same way, and is no error here: the vector it leaves claims less.
pub(crate) fn record_variances(context: &mut Context, names: &[Global]) {
    curios_profile::profile!("record_variances");
    let mut inducts = BTreeMap::new();
    let mut structs = BTreeMap::new();
    for name in names {
        if let Some(declaration) = context.induct_decl(name).cloned() {
            let spliced = InductDecl {
                arity: zonk_solved_term_metas(context, &declaration.arity),
                constructors: declaration
                    .constructors
                    .iter()
                    .map(|(tag, constructor)| {
                        (
                            tag.clone(),
                            InductParam::new(
                                zonk_solved_term_metas(context, &constructor.telescope),
                                constructor.plicities().to_vec(),
                            ),
                        )
                    })
                    .collect(),
                ..declaration
            };
            inducts.insert(*name, spliced);
        } else if let Some(declaration) = context.struct_decl(name).cloned() {
            let spliced = StructDecl {
                arity: zonk_solved_term_metas(context, &declaration.arity),
                ..declaration
            };
            structs.insert(*name, spliced);
        }
    }

    let VarianceVectors { vectors, .. } =
        variance_vectors(context, Declarations::of(&inducts, &structs));
    for (name, vector) in vectors {
        if let Some(declaration) = context.induct_decl(&name).cloned() {
            context.update_induct(
                &name,
                InductDecl {
                    variances: vector,
                    ..declaration
                },
            );
        } else if let Some(declaration) = context.struct_decl(&name).cloned() {
            context.update_struct(
                &name,
                StructDecl {
                    variances: vector,
                    ..declaration
                },
            );
        }
    }
}

/// Recompute every vector of `module` over its zonked declarations and hold the one each entry carries to it.
///
/// `module` is exactly the declaration set to analyze, and anything outside it answers from this context's registry, for the reason [`check_positivity`](super::check_positivity) gives. The carried vector is the one taken as the family's group finalized, and it stays: the unit's later items were compared under it, so a stored unit hands a later compile the reading its own items had and a reused family is read as a re-elaborated one is. What this run decides is that it claims no irrelevance the final telescopes deny. It may claim less, a telescope having held a metavariable since solved, and that family is counted rather than rewritten.
pub fn check_variance(context: &mut Context, module: &Module) -> Result<(), Error> {
    curios_profile::profile!("check_variance");
    let VarianceVectors { vectors, .. } = variance_vectors(
        context,
        Declarations::of(&module.induct_decls, &module.struct_decls),
    );

    let carried = |name: &Global| {
        module
            .induct_decls
            .get(name)
            .map(|declaration| &declaration.variances)
            .or_else(|| {
                module
                    .struct_decls
                    .get(name)
                    .map(|declaration| &declaration.variances)
            })
    };

    for (name, settled) in &vectors {
        let Some(carried) = carried(name) else {
            continue;
        };
        let denied = carried.iter().zip(settled).position(|(carried, settled)| {
            *carried == Variance::Irrelevant && *settled == Variance::Invariant
        });
        if let Some(level) = denied {
            return Err(Error::UniverseInvariant(format!(
                "{name}: universe level {level} is carried as irrelevant, and its declaration mentions it"
            )));
        }
    }
    curios_profile::sample!(
        "variance::read_short_at_finalization",
        vectors
            .iter()
            .filter(|(name, settled)| carried(name).is_some_and(|carried| carried != *settled))
            .count()
    );

    Ok(())
}
