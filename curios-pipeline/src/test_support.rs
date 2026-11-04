//! The puts: a program, or a module, handed to the checkers with the fixed prelude in scope, the scope Core text is read over to make such a module, and a unit read from Core text compiled where a program's is lowered and elaborated, for the soundness board and the suites that judge what it judges.
//!
//! Behind the `test-support` feature, so no build that ships holds it, and compiled for this crate's own tests.

use {
    crate::{
        Earlier, Examined, Over, Refused, Typechecked, examine_entrypoint, globals_after, recheck,
        recheck_measured, recheck_over, typecheck_entrypoint,
    },
    curios_abi::ForeignStore,
    curios_cert::{Verdict, certify_definitions},
    curios_common::Mount,
    curios_core::{Definitions, Places, Zonked, ZonkedRefusal},
    curios_erase::{Erasures, erase_unit},
    curios_module::SYNTAX,
    curios_prelude::with_prelude,
    curios_surface::FormScope,
    curios_unit::{Predecessors, Unit},
};

/// Why a unit read from Core text was not made: its module carries what elaboration would have solved, the kernel refused it, or it could not be erased. Each held by pointer, a refusal being the large case of what a compile answers.
#[derive(Debug)]
pub enum Unmade {
    Unfinished(Box<ZonkedRefusal>),
    Kernel(Box<Verdict>),
    Erased(String),
}

/// Compile a unit read from Core text, `read` being the module and the rows the reader made of its text: what stands where a program's unit holds its lowering and its elaboration, judged and erased as any unit is from there, over `predecessors`, and claiming `mounts`. Over `baseline`, the unit the store offers for the text's earlier spelling, the kernel is handed its module and record whole and adopts each verdict that still stands, the units in scope it was not compiled after being moved, as a recompile over a baseline is judged ([`recheck_over`]).
pub fn compile_read(
    budget: u64,
    predecessors: Predecessors<'_>,
    mounts: Vec<Mount>,
    (definitions, foreigns): (Definitions, ForeignStore),
    baseline: Option<&Earlier>,
) -> Result<Unit, Unmade> {
    let core =
        Zonked::project(&definitions).map_err(|refusal| Unmade::Unfinished(Box::new(refusal)))?;
    let moved = predecessors
        .units()
        .iter()
        .map(|unit| baseline.is_some_and(|earlier| earlier.after.moved(unit)))
        .collect::<Vec<_>>();
    let globals = globals_after(predecessors, &moved);
    let places = Places::of(core.as_definitions());
    let (rechecked, mut kernel) = match baseline {
        Some(earlier) => recheck_over(
            &core,
            &places,
            budget,
            &globals,
            Over {
                baseline: &earlier.unit,
                moved: &moved,
                candidates: earlier.candidates.as_deref(),
            },
            &SYNTAX,
        ),
        None => certify_definitions(&core, budget, &globals, SYNTAX),
    };
    let certification = rechecked.certification;
    if let Some(verdict) = rechecked.verdicts.into_iter().next() {
        return Err(Unmade::Kernel(Box::new(verdict)));
    }
    // The unit is the module the kernel stated, so every mark it carries is the walk's own.
    let core = core.map(|_| rechecked.definitions);

    let cores = predecessors.cores();
    let scope_places = predecessors.places();
    let certifications = predecessors.certifications();
    let erased = predecessors.erased();
    let fixed = predecessors.fixed();
    let scope = Erasures::of(&cores, &scope_places, &certifications, &erased).fixed(&fixed);
    let anf = erase_unit(&mut kernel, scope, &core)
        .map_err(|error| Unmade::Erased(format!("{error:?}")))?;

    Ok(Unit::read(
        mounts,
        foreigns,
        core.into_definitions(),
        anf,
        certification,
    ))
}

/// The scope Core text is read over with the fixed prelude in front of it: the entries the prelude's formers carry and the foreign rows its declarations state, as a unit's lowering is handed those of the units before it.
pub fn prelude_form_scope() -> FormScope {
    with_prelude(|prelude| FormScope::over(SYNTAX, prelude.iter().map(|unit| unit.core())))
}

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
