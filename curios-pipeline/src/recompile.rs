//! One unit compiled over a baseline: lowered whole, walked whole, and each declaration kept as the baseline holds it where everything its elaboration asked there is answered as it was, with every other elaborated and judged; and each erased form kept where what its erasure asked is.
//!
//! **A baseline is a [`Unit`] compiled from an earlier text of the same sources**, handed in by whatever the fold consults; nothing here knows where it came from. Which of its declarations stand is decided by no one stage for another. The lowering keeps a declaration whose text and resolutions stand (`PreparedText::kept_from`). The elaborator keeps one of those where every question its elaboration put to the declarations around it is answered as it was, asked again in order as the unit is walked (`curios_elab::Traces`), so an edit elaborates again what read the part it changed and nothing behind that: a declaration elaborated again that came out as it was moves none of its readers. The kernel is handed the baseline whole and adopts a verdict by the reads its own walk filed ([Cached verdicts](../../documentation/design/soundness/cached-verdicts.md)). Erasure keeps the erased form of a declaration the elaborator kept where every question its erasure put is answered as it was, of the declarations around it and of the erased forms it read a binding in (`curios_elab::ErasedBaseline`).
//!
//! **A declaration lowered again is elaborated again.** Nothing compares two lowerings: an edit that leaves a declaration's meaning alone costs that declaration's elaboration, and what it comes to is compared part by part with what the baseline holds, by whatever read it.

use {
    crate::{CompileError, anchors_of, globals, kernel_refusal, with_broken},
    curios_cert::{Baseline, Rechecked, certify_module_over},
    curios_core::{Module, Zonked},
    curios_elab::{
        Context, ErasedBaseline, Erasures, Established, Recompile, Recompiled,
        elaborate_and_zonk_unit_over, erase_unit_over,
    },
    curios_text::{UnitSource, into_core_unit_over},
    curios_unit::{Predecessors, Uncertified, Unit},
    curios_utilities::SyntaxRegistry,
    std::{collections::BTreeSet, rc::Rc},
};

/// Compile one unit against `predecessors` over `baseline`: lower whole, then elaborate, judge and erase what does not stand.
///
/// The result is a unit like [`compile_unit`](crate::compile_unit)'s — the same lowering, a core module holding every item in the order a module is stored in, an erased form for each — differing in which items were elaborated, judged and erased now and which were taken from the baseline. An item's erased form is its own, in identities of its own, so one taken from the baseline is the one a whole compile makes.
pub fn compile_unit_over(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    source: &UnitSource<'_>,
    baseline: &Unit,
) -> Result<Unit, CompileError> {
    curios_profile::profile!("compile_unit_over");
    let text = predecessors.text();
    let cores = predecessors.cores();

    let mut lowered = into_core_unit_over(source, &text, syntax, baseline.text())
        .map_err(|error| CompileError::Failure(vec![error.report()]))?;

    // A mount says how every name of the unit is spelled, so a unit whose mounts moved keeps nothing.
    let kept = match baseline.core().mounts == lowered.core().mounts {
        true => lowered.kept_from(baseline.text()),
        false => BTreeSet::new(),
    };

    // The baseline is the scope's last layer, answering for each declaration from where the walk keeps it.
    let established =
        Established::placed(&predecessors.placed()).keeping(baseline.core(), baseline.places());
    let mut context = Context::new(budget, *syntax);
    context.set_imports(lowered.imports().clone());
    context.set_broken(lowered.broken_names());
    let Recompiled {
        module: core,
        traces,
        kept,
    } = with_broken(
        lowered.broken(),
        elaborate_and_zonk_unit_over(
            &mut context,
            &established,
            Recompile {
                baseline: baseline.core(),
                places: baseline.places(),
                traces: baseline.traces(),
                lowered: &kept,
            },
            lowered.core(),
            lowered.minted(),
        )
        .map_err(|error| {
            let spellings = Rc::new(lowered.spellings(&text));
            CompileError::of(&error, |member| {
                member.reports_with_hints(
                    lowered.core(),
                    &cores,
                    syntax,
                    lowered.unbound(),
                    &spellings,
                    &anchors_of(&lowered, predecessors),
                )
            })
        }),
    )?;
    // What the proofs written now read, and what the baseline's read in the declarations kept from it.
    lowered.credit(&context.credited());
    lowered.credit_reused(baseline.text(), |name| kept.contains(name));

    let core =
        Zonked::project(&core).map_err(|refusal| CompileError::failure(refusal.to_string()))?;

    // The kernel is told nothing of what was kept: it is handed the baseline whole and decides which of its verdicts stand, so its record covers every declaration of the new text and none it dropped.
    let rechecked = recheck_over(&core, budget, predecessors, baseline, syntax);
    if let Some(verdict) = rechecked.verdicts.first() {
        return Err(kernel_refusal(verdict, core.as_module(), &cores, syntax));
    }
    let certification = rechecked.certification;

    // Erasure decides what it keeps by its own record: an erased form stands where the elaborator kept its declaration and what its erasure asked is answered as it was.
    let views = predecessors.projected();
    let erased = predecessors.erased();
    let ersd = erase_unit_over(
        budget,
        *syntax,
        Erasures::of(&cores, &erased).keeping(&views),
        &core,
        ErasedBaseline {
            core: baseline.core(),
            places: baseline.places(),
            erased: baseline.erased(),
            kept: &kept,
        },
    )
    .map_err(|error| {
        CompileError::Failure(error.reports_with(
            core.as_module(),
            &cores,
            syntax,
            &anchors_of(&lowered, predecessors),
        ))
    })?;

    Ok(Uncertified::new(lowered, core.into_module(), ersd, traces).certified(certification))
}

/// [`recheck`](crate::recheck) for a unit compiled over `baseline`: the walk is handed the units in scope as its environment and the baseline's module and record whole, and adopts each of the baseline's verdicts that still stands, by the reads its own walk filed (`curios_cert::Baseline`).
///
/// Nothing here says which declarations are kept. A declaration elaborated again that came out as the baseline holds it, reading nothing that moved, is adopted like one never touched, and one the elaborator kept is judged where what the kernel's own walk read for it has changed: the verdicts that are filed rest on the certifier's record, and on one claim of the fold's, that `baseline` was compiled after these `predecessors`, which is what a cache offers one by ([`Cache::baseline`](crate::Cache::baseline)).
pub(crate) fn recheck_over(
    module: &Zonked<Module>,
    budget: u64,
    predecessors: Predecessors<'_>,
    baseline: &Unit,
    syntax: &SyntaxRegistry,
) -> Rechecked {
    certify_module_over(
        module,
        budget,
        &globals(predecessors),
        Baseline {
            module: baseline.core(),
            places: baseline.places(),
            certification: baseline.certification(),
        },
        *syntax,
    )
}
