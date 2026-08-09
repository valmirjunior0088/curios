//! The elaborator's driver for the shared strict-positivity analysis.
//!
//! The analysis itself lives in `curios-cert` (see that module for the rule and its rationale) and is run by both checkers; what belongs here is the driving: which declarations are analyzed (this module's — the whole program at archive build, the user suffix at a replay), where the vectors are persisted (the registry entries, riding the prelude archive), and how a refusal is rendered (a spanned [`Error`] naming the offending part).

use {
    super::{Context, Error},
    curios_cert::positivity_vectors,
    curios_core::Module,
};

/// Reject every `induct` and `struct` declaration in `module` that is not strictly positive, and record each surviving declaration's parameter polarities on its registry entry.
///
/// Runs on zonked Core, so the telescopes the analysis reads are final and meta-free. `module` is exactly the declaration set to analyze; anything the walk reaches outside it is a replayed prelude declaration, whose vector was computed once at archive-build time and answers from this context's registry — sound because prelude items cannot mention user code, so no cycle crosses the boundary.
pub fn check_positivity(context: &mut Context, module: &mut Module) -> Result<(), Error> {
    curios_profile::profile!("check_positivity");
    let vectors = positivity_vectors(
        context,
        curios_cert::Declarations::of(&module.induct_decls, &module.struct_decls),
        // At a replay these are the entry's own; the prelude was analyzed when it was elaborated, and its vectors ride the archive.
        curios_cert::Coverage::Partial,
    )
    .map_err(|refusal| {
        Error::not_strictly_positive(
            refusal.name.symbol(),
            refusal.part,
            refusal.type_,
            refusal.polarity,
        )
    })?;

    for (name, vector) in vectors {
        if let Some(declaration) = module.induct_decls.get_mut(&name) {
            declaration.polarities = vector;
        } else if let Some(declaration) = module.struct_decls.get_mut(&name) {
            declaration.polarities = vector;
        }
    }

    Ok(())
}
