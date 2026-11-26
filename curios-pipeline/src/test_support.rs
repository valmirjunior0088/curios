//! The puts: a program handed to the checkers with the fixed prelude in scope, and the scope Curios/Core is read over in front of it, for the suites that judge what the checkers judge. Beside them, what a compile answers that only those suites read.
//!
//! Behind the `test-support` feature, so no build that ships holds it, and compiled for this crate's own tests.

mod standard;
pub use standard::{Edit, declarations, edits, elaborated_over, recompiled_over};

mod stored;
pub use stored::{asked_stored, lowering_stored, traces_stored};

use {
    crate::{
        EntryTail, Examined, Obligation, Refused, elaborate_entry, examine_entrypoint, globals,
        lower_entry,
    },
    curios_cert::{Certifier, Refusal, certify_program},
    curios_common::SyntaxRegistry,
    curios_core::{Consumption, Program},
    curios_elab::{Context, Established},
    curios_module::RootSource,
    curios_module::SYNTAX,
    curios_prelude::with_prelude,
    curios_surface::Entrypoint,
    curios_surface::FormScope,
    curios_unit::Predecessors,
};

/// [`recheck`], handing the walk's own certifier to `read` for a measurement to take what it wants of it rather than only its verdicts. See `curios_cert::certify_program`.
///
/// A reader rather than the certifier handed back: the certifier reads its scope where the units hold it, so it lives no longer than the environment assembled here.
pub fn recheck_measured<T>(
    program: &Program,
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    read: impl FnOnce(&Certifier<'_>) -> T,
) -> (Vec<Refusal>, T) {
    let globals = globals(predecessors);
    let (rechecked, certifier) = certify_program(program, budget, &globals, *syntax);

    (rechecked.into_refusals(), read(&certifier))
}

/// Put `program` to the independent certifier with `predecessors` already in scope, so only what its units do not already answer for is judged — their own items resting on the verdict recorded when each was built: the compile path's rechecking, in the environment [`globals`] assembles for it, for a suite that puts a program whole.
pub fn recheck(
    program: &Program,
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
) -> Vec<Refusal> {
    certify_program(program, budget, &globals(predecessors), *syntax)
        .0
        .into_refusals()
}

/// A program type-checked and not yet put to the certifier.
#[derive(Debug)]
pub struct Typechecked {
    pub program: Program,
    /// Type totality and proof totality as the elaborator decided them over the program, reported rather than raised. Empty when it carries none.
    pub obligations: Vec<Obligation>,
    /// What the heaviest declaration consumed: the units and peak depth `DEFAULT_BUDGET` is set against, and the only figures that say whether a program's cost is depth or work. A measurement's, which nothing in the compiler reads.
    pub consumption: Consumption,
}

/// Lower and type-check `entrypoint` as a compilation does, stopping short of the certifier, with the erasure obligations reported rather than raised.
///
/// The elaborated program comes back even when this stage's own type and proof totality refuse it, which is what lets one program be put to *both* checkers: `curios-cert` decides the same two obligations independently, and a program only this side refuses would otherwise yield no module for the certifier to judge — leaving the most consequential disagreement, the trusted base resting on an elaborator-only analysis, unobservable. What the program's tail is checked against is the compile path's, the entry's `Io({})` contract, which both read off `stated`; what differs is that the program is elaborated whole here, its declarations and its final term as one module, where a compilation takes the declarations as the unit they are.
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

/// The scope Curios/Core is read over with the fixed prelude in front of it: the entries the prelude's items hold and the foreign rows its declarations state, as a unit's lowering is handed those of the units before it.
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
    program: &curios_core::Program,
    budget: u64,
    read: impl FnOnce(&curios_cert::Certifier<'_>) -> T,
) -> (Vec<curios_cert::Refusal>, T) {
    with_prelude(|prelude| {
        recheck_measured(program, budget, Predecessors::over(prelude), &SYNTAX, read)
    })
}

/// Put `program` to the independent certifier with the fixed prelude in scope. See [`recheck`].
pub fn recheck_with_prelude(
    program: &curios_core::Program,
    budget: u64,
) -> Vec<curios_cert::Refusal> {
    with_prelude(|prelude| recheck(program, budget, Predecessors::over(prelude), &SYNTAX))
}
