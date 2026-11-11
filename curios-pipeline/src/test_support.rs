//! The puts: a program, or a module, handed to the checkers with the fixed prelude in scope, the scope Core text is read over to make such a module, and a unit read from Core text compiled where a program's is lowered and elaborated, for the soundness board and the suites that judge what it judges. Beside them, what a compile answers that only those suites read.
//!
//! Behind the `test-support` feature, so no build that ships holds it, and compiled for this crate's own tests.

use {
    crate::{
        Earlier, EntryTail, Examined, Obligation, Over, Refused, elaborate_entry,
        examine_entrypoint, globals, globals_after, lower_entry, recheck, recheck_over,
    },
    curios_abi::ForeignStore,
    curios_cert::{Certifier, Verdict, certify_definitions, certify_program},
    curios_common::{Mount, SyntaxRegistry},
    curios_core::{Consumption, Definitions, Places, Program, Zonked, ZonkedRefusal},
    curios_elab::{Context, Established},
    curios_erase::{Erasures, erase_unit},
    curios_module::RootSource,
    curios_module::SYNTAX,
    curios_prelude::with_prelude,
    curios_surface::Entrypoint,
    curios_surface::FormScope,
    curios_unit::{Predecessors, Unit},
};

/// [`recheck`], handing the walk's own certifier to `read` for a measurement to take what it wants of it rather than only its verdicts. See `curios_cert::certify_program`.
///
/// A reader rather than the certifier handed back: the certifier reads its scope where the units hold it, so it lives no longer than the environment assembled here.
pub fn recheck_measured<T>(
    program: &curios_core::Zonked<Program>,
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    read: impl FnOnce(&Certifier<'_>) -> T,
) -> (Vec<Verdict>, T) {
    let globals = globals(predecessors);
    let (rechecked, certifier) = certify_program(program, budget, &globals, *syntax);

    (rechecked.verdicts, read(&certifier))
}

/// A program type-checked and not yet put to the certifier.
#[derive(Debug)]
pub struct Typechecked {
    pub program: Program,
    /// The (T) and (V) obligations the elaborator decided over the program, reported rather than raised. Empty when it carries none.
    pub obligations: Vec<Obligation>,
    /// What the heaviest declaration consumed: the units and peak depth `DEFAULT_BUDGET` is set against, and the only figures that say whether a program's cost is depth or work. A measurement's, which nothing in the compiler reads.
    pub consumption: Consumption,
}

/// Lower and type-check `entrypoint` as a compilation does, stopping short of the certifier, with the erasure obligations reported rather than raised.
///
/// The elaborated program comes back even when this stage's own (T)/(V) verdicts refuse it, which is what lets one program be put to *both* checkers: `curios-cert` decides the same two obligations independently, and a program only this side refuses would otherwise yield no module for the certifier to judge — leaving the most consequential disagreement, the trusted base resting on an elaborator-only analysis, unobservable. What the program's tail is checked against is the compile path's, the entry's `Io({})` contract, which both read off `stated`; what differs is that the program is elaborated whole here, its declarations and its final term as one module, where a compilation takes the declarations as the unit they are.
///
/// Each refusal carries the elaborator's error as it raised it beside the diagnostic a compilation prints, since rendering is where a consumer that matches the error's variant would lose it. A caller that wants the certifier's opinion on the result puts it to [`recheck`], which supplies the same environment `compile_entrypoint` does rather than re-walking the standard library.
pub fn typecheck_entrypoint(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    entrypoint: &Entrypoint,
    loader: &RootSource,
) -> Result<Typechecked, Refused> {
    let lowered =
        lower_entry(predecessors, syntax, entrypoint, loader, &mut |_| {}).map_err(|reported| {
            Refused {
                error: None,
                reported,
            }
        })?;

    let established = Established::placed(&predecessors.placed());
    let mut context = Context::new(budget, *syntax);
    let (program, obligations) = elaborate_entry(
        &mut context,
        &established,
        predecessors,
        lowered,
        EntryTail::Program,
        &mut |_| {},
    )?;

    Ok(Typechecked {
        program,
        obligations,
        consumption: context.heaviest_declaration(),
    })
}

/// Why a unit read from Core text was not made: its module carries what elaboration would have solved, the certifier refused it, or it could not be erased. Each held by pointer, a refusal being the large case of what a compile answers.
#[derive(Debug)]
pub enum Unmade {
    Unfinished(Box<ZonkedRefusal>),
    Certifier(Box<Verdict>),
    Erased(String),
}

/// Compile a unit read from Core text, `read` being the module and the rows the reader made of its text: what stands where a program's unit holds its lowering and its elaboration, judged and erased as any unit is from there, over `predecessors`, and claiming `mounts`. Over `baseline`, the unit the store offers for the text's earlier spelling, the certifier is handed its module and record whole and adopts each verdict that still stands, the units in scope it was not compiled after being moved, as a recompile over a baseline is judged (`recheck_over`).
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
    let (rechecked, mut certifier) = match baseline {
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
        return Err(Unmade::Certifier(Box::new(verdict)));
    }
    // The unit is the module the certifier stated, so every mark it carries is the walk's own.
    let core = core.map(|_| rechecked.definitions);

    let cores = predecessors.cores();
    let scope_places = predecessors.places();
    let certifications = predecessors.certifications();
    let erased = predecessors.erased();
    let fixed = predecessors.fixed();
    let scope = Erasures::of(&cores, &scope_places, &certifications, &erased).fixed(&fixed);
    let anf = erase_unit(&mut certifier, scope, &core)
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

/// Lower and type-check `entrypoint` against the fixed prelude, stopping short of the certifier. See [`typecheck_entrypoint`].
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

/// Put `entrypoint` to both checkers at what `tail` judges it at, against the fixed prelude, and hand back what each said. See [`examine_entrypoint`].
pub fn examine_with_prelude(
    budget: u64,
    entrypoint: &curios_surface::Entrypoint,
    loader: &curios_module::RootSource,
    tail: EntryTail,
) -> Result<Examined, Refused> {
    with_prelude(|prelude| {
        examine_entrypoint(
            budget,
            Predecessors::over(prelude),
            &SYNTAX,
            entrypoint,
            loader,
            tail,
        )
    })
}

/// Put `program` to the independent certifier with the fixed prelude in scope, handing the walk's own certifier to `read` for a measurement to take what it wants of it. See [`recheck_measured`].
pub fn recheck_with_prelude_measured<T>(
    program: &curios_core::Zonked<curios_core::Program>,
    budget: u64,
    read: impl FnOnce(&curios_cert::Certifier<'_>) -> T,
) -> (Vec<curios_cert::Verdict>, T) {
    with_prelude(|prelude| {
        recheck_measured(program, budget, Predecessors::over(prelude), &SYNTAX, read)
    })
}

/// Put `program` to the independent certifier with the fixed prelude in scope. See [`recheck`].
pub fn recheck_with_prelude(
    program: &curios_core::Zonked<curios_core::Program>,
    budget: u64,
) -> Vec<curios_cert::Verdict> {
    with_prelude(|prelude| recheck(program, budget, Predecessors::over(prelude), &SYNTAX))
}
