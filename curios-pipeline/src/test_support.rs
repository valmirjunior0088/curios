//! The puts: a program, or a module, handed to the checkers with the fixed prelude in scope, for the soundness board and the suites that judge what it judges.
//!
//! Behind the `test-support` feature, so no build that ships holds it, and compiled for this crate's own tests.

use {
    crate::{
        Examined, Refused, Typechecked, examine_entrypoint, recheck, recheck_measured,
        typecheck_entrypoint,
    },
    curios_module::SYNTAX,
    curios_prelude::with_prelude,
    curios_unit::Predecessors,
};

/// Lower and type-check `entrypoint` against the fixed prelude, stopping short of the kernel. See [`typecheck_entrypoint`].
pub fn typecheck_with_prelude(
    budget: u64,
    entrypoint: &curios_surface::Entrypoint,
    loader: &curios_module::RootSource,
) -> Result<Typechecked, Refused> {
    with_prelude(|prelude| {
        typecheck_entrypoint(
            budget,
            Predecessors::over(prelude),
            &SYNTAX,
            entrypoint,
            loader,
        )
    })
}

/// Put `entrypoint` to both checkers against the fixed prelude and hand back what each said. See [`examine_entrypoint`].
pub fn examine_with_prelude(
    budget: u64,
    entrypoint: &curios_surface::Entrypoint,
    loader: &curios_module::RootSource,
) -> Result<Examined, Refused> {
    with_prelude(|prelude| {
        examine_entrypoint(
            budget,
            Predecessors::over(prelude),
            &SYNTAX,
            entrypoint,
            loader,
        )
    })
}

/// Put `program` to the independent kernel with the fixed prelude in scope, handing the walk's own kernel to `read` for a measurement to take what it wants of it. See [`recheck_measured`].
pub fn recheck_with_prelude_measured<T>(
    program: &curios_core::Zonked<curios_core::Program>,
    budget: u64,
    read: impl FnOnce(&curios_cert::Kernel<'_>) -> T,
) -> (Vec<curios_cert::Verdict>, T) {
    with_prelude(|prelude| {
        recheck_measured(program, budget, Predecessors::over(prelude), &SYNTAX, read)
    })
}

/// Put `program` to the independent kernel with the fixed prelude in scope. See [`recheck`].
pub fn recheck_with_prelude(
    program: &curios_core::Zonked<curios_core::Program>,
    budget: u64,
) -> Vec<curios_cert::Verdict> {
    with_prelude(|prelude| recheck(program, budget, Predecessors::over(prelude), &SYNTAX))
}
