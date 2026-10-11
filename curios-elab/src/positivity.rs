//! The elaborator's driver for the shared strict-positivity analysis.
//!
//! The analysis itself lives in `curios-analysis/src/positivity.rs` (see that module for the rule and its rationale) and is run by both checkers; what belongs here is the driving: which declarations are analyzed (the ones a declaration declares, as it is published), where the vectors are persisted (the registry entries, which a stored unit carries), and how a refusal is rendered (an [`Error`] naming the offending part).

use {
    super::{Context, Error, Published},
    curios_analysis::{Declarations, PositivityRefusal, positivity_vectors},
    curios_core::{InductDecl, StructDecl},
    std::collections::BTreeMap,
};

/// Reject the `induct` and `struct` entries `published` declares where they are not strictly positive, and record each one's parameter polarities on its registry entry.
///
/// Runs on a published declaration, so the telescopes the analysis reads are final and meta-free, and over that declaration's entries alone: a group's formers are one item, so the families that name one another are analyzed together. Anything the walk reaches outside them belongs to a declaration that finished before this one — a predecessor unit's, or one of this unit that this one reads — whose vector was written as it was published and answers from this context's registry. Sound because a finished declaration cannot mention this one, so no cycle crosses the boundary.
///
/// The vector is written here and nowhere else: no pass over a unit decides it again, and a recompile leaves a reused declaration's as its baseline published it.
pub(crate) fn decide_polarities(context: &mut Context, published: &Published) -> Result<(), Error> {
    if published.induct_decls.is_empty() && published.struct_decls.is_empty() {
        return Ok(());
    }

    curios_profile::profile!("check_positivity");
    let inducts = published
        .induct_decls
        .iter()
        .cloned()
        .collect::<BTreeMap<_, _>>();
    let structs = published
        .struct_decls
        .iter()
        .cloned()
        .collect::<BTreeMap<_, _>>();
    // These are this declaration's own; every other was analyzed as it was published, and its vector rides its registry entry.
    let vectors =
        positivity_vectors(context, Declarations::of(&inducts, &structs)).map_err(|refusal| {
            match refusal {
                // Placed at the offending part's type as written, where the published telescope kept its position.
                PositivityRefusal::NotPositive(refusal) => {
                    let written = refusal.type_.extent();

                    Error::not_strictly_positive(
                        refusal.name,
                        refusal.part,
                        refusal.type_,
                        refusal.polarity,
                    )
                    .at_opt(written)
                }
                PositivityRefusal::Exhausted { name, error } => error.in_declaration([name]),
            }
        })?;

    for (name, vector) in vectors {
        if let Some(declaration) = inducts.get(&name) {
            context.update_induct(
                &name,
                InductDecl {
                    polarities: vector,
                    ..declaration.clone()
                },
            );
        } else if let Some(declaration) = structs.get(&name) {
            context.update_struct(
                &name,
                StructDecl {
                    polarities: vector,
                    ..declaration.clone()
                },
            );
        }
    }

    Ok(())
}
