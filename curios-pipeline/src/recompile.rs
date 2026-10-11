//! One unit compiled over a baseline: lowered whole, walked whole, and each declaration kept as the baseline holds it where everything its elaboration asked there is answered as it was, with every other elaborated and judged; and each erased form kept where what its erasure asked is.
//!
//! **A baseline is a [`Unit`] compiled from an earlier text of the same sources**, handed in by whatever the fold consults; nothing here knows where it came from. Which of its declarations stand is decided by no one stage for another. The lowering keeps a declaration whose text and resolutions stand (`PreparedText::kept_from`). The elaborator keeps one of those where every question its elaboration put to the declarations around it is answered as it was, asked again in order as the unit is walked (`curios_elab::Traces`), so an edit elaborates again what read the part it changed and nothing behind that: a declaration elaborated again that came out as it was moves none of its readers. The kernel is handed the baseline whole and adopts a verdict by the reads its own walk filed ([Cached verdicts](../../documentation/design/soundness/cached-verdicts.md)). Erasure keeps the erased form of a declaration the elaborator kept where every question its erasure put is answered as it was, of the declarations around it and of the erased forms it read a binding in (`curios_elab::ErasedBaseline`).
//!
//! **A declaration lowered again is elaborated again.** Nothing compares two lowerings: an edit that leaves a declaration's meaning alone costs that declaration's elaboration, and what it comes to is compared part by part with what the baseline holds, by whatever read it.

use {
    crate::{
        Closing, CompileError, elaborator_refusal, globals_after, kernel_refusal, with_broken,
    },
    curios_cert::{Baseline, Rechecked, certify_module_over},
    curios_core::{Entrypoint, Module, Zonked},
    curios_elab::{
        Context, ErasedBaseline, Erasures, Established, Recompile, Recompiled,
        elaborate_and_zonk_entry_over, elaborate_and_zonk_unit_over, erase_unit_over,
    },
    curios_text::{PreparedText, UnitSource, into_core_unit_over},
    curios_unit::{Predecessors, Uncertified, Unit},
    curios_utilities::SyntaxRegistry,
    std::collections::BTreeSet,
};

/// Compile one unit against `predecessors` over `baseline`: lower whole, then elaborate, judge and erase what does not stand.
///
/// The result is a unit like [`compile_unit`](crate::compile_unit)'s — the same lowering, a core module holding every item in the order a module is stored in, an erased form for each — differing in which items were elaborated, judged and erased now and which were taken from the baseline. An item's erased form is its own, in identities of its own, so one taken from the baseline is the one a whole compile makes.
///
/// **`moved` says which predecessors the baseline was not compiled after**, one for each in their order: a unit whose contents changed since, or one the baseline's compile was never handed. What the baseline read of a unit that did not move stands unasked, at every stage; what it read of one that moved is asked again by the digest each stage recorded of the part read (`curios_elab::Foreign`, `curios_core::Abroad`). Whoever offers the baseline says which is which, and where it cannot say, every one moved.
pub fn compile_unit_over(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    source: &UnitSource<'_>,
    baseline: &Unit,
    moved: &[bool],
) -> Result<Unit, CompileError> {
    curios_profile::profile!("compile_unit_over");
    let lowered = into_core_unit_over(source, &predecessors.text(), syntax, baseline.text())
        .map_err(|error| CompileError::Failure(vec![error.report()]))?;

    compile_lowered_over(budget, predecessors, syntax, lowered, baseline, moved, None)
        .map(|(unit, _)| unit)
}

/// [`compile_unit_over`] from the unit's lowering on, which was made over `baseline`'s: elaborate, judge and erase what does not stand. With `closing`, the unit is a program's declarations and the term that closes the program is elaborated in their walk and handed back beside the unit ([`Closing`]).
pub(crate) fn compile_lowered_over(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    mut lowered: PreparedText,
    baseline: &Unit,
    moved: &[bool],
    closing: Option<Closing<'_>>,
) -> Result<(Unit, Option<Entrypoint>), CompileError> {
    let cores = predecessors.cores();
    let (tail, mut elaborated, mut refused) = match closing {
        Some(Closing {
            tail,
            elaborated,
            refused,
        }) => (Some(tail), Some(elaborated), Some(refused)),
        None => (None, None, None),
    };

    // A mount says how every name of the unit is spelled, so a unit whose mounts moved keeps nothing.
    let kept = match baseline.core().mounts == lowered.core().mounts {
        true => lowered.kept_from(baseline.text()),
        false => BTreeSet::new(),
    };

    // The baseline is the scope's last layer, answering for each declaration from where the walk keeps it.
    let fixed = predecessors.fixed();
    let established = Established::placed(&predecessors.placed())
        .fixed(&fixed)
        .after(moved)
        .keeping(baseline.core(), baseline.places());
    let mut context = Context::new(budget, *syntax);
    context.set_imports(lowered.imports().clone());
    context.set_broken(lowered.broken_names());
    let recompile = Recompile {
        baseline: baseline.core(),
        places: baseline.places(),
        traces: baseline.traces(),
        lowered: &kept,
    };
    let walked = match tail {
        Some(tail) => elaborate_and_zonk_entry_over(
            &mut context,
            &established,
            recompile,
            lowered.core(),
            lowered.minted(),
            tail,
        ),
        None => elaborate_and_zonk_unit_over(
            &mut context,
            &established,
            recompile,
            lowered.core(),
            lowered.minted(),
        )
        .map(|recompiled| (recompiled, None)),
    };
    let walked = with_broken(
        lowered.broken(),
        walked.map_err(|error| {
            elaborator_refusal(
                &error,
                lowered.core(),
                &predecessors.lexicons(syntax),
                &lowered,
                predecessors,
                syntax,
            )
        }),
    );
    // What the proofs written now read, whatever elaboration said: a lint is no part of its verdict.
    lowered.credit(&context.credited());
    let mut refuse = |lowered: &PreparedText, error: CompileError| {
        if let Some(refused) = &mut refused {
            refused(lowered);
        }

        error
    };
    let (
        Recompiled {
            module: core,
            traces,
            kept,
        },
        entry,
    ) = walked.map_err(|error| refuse(&lowered, error))?;
    // And what the baseline's read in the declarations kept from it.
    lowered.credit_reused(baseline.text(), |name| kept.contains(name));
    if let (Some(elaborated), Some(entry)) = (&mut elaborated, &entry) {
        elaborated(&core, entry);
    }

    let core = Zonked::project(&core)
        .map_err(|refusal| refuse(&lowered, CompileError::failure(refusal.to_string())))?;

    // The kernel is told nothing of what was kept: it is handed the baseline whole and decides which of its verdicts stand, so its record covers every declaration of the new text and none it dropped.
    let rechecked = recheck_over(&core, budget, predecessors, baseline, moved, syntax);
    if let Some(verdict) = rechecked.verdicts.first() {
        let refusal = kernel_refusal(
            verdict,
            core.as_module(),
            &predecessors.lexicons(syntax),
            syntax,
        );
        return Err(refuse(&lowered, refusal));
    }
    let certification = rechecked.certification;

    // Erasure decides what it keeps by its own record: an erased form stands where the elaborator kept its declaration and what its erasure asked is answered as it was.
    let views = predecessors.projected();
    let erased = predecessors.erased();
    let ersd = erase_unit_over(
        budget,
        *syntax,
        Erasures::of(&cores, &erased)
            .keeping(&views)
            .fixed(&fixed)
            .after(moved),
        &core,
        ErasedBaseline {
            core: baseline.core(),
            places: baseline.places(),
            erased: baseline.erased(),
            kept: &kept,
        },
    )
    .map_err(|error| {
        let refusal = elaborator_refusal(
            &error,
            core.as_module(),
            &predecessors.lexicons(syntax),
            &lowered,
            predecessors,
            syntax,
        );

        refuse(&lowered, refusal)
    })?;

    Ok((
        Uncertified::new(lowered, core.into_module(), ersd, traces).certified(certification),
        entry,
    ))
}

/// [`recheck`](crate::recheck) for a unit compiled over `baseline`: the walk is handed the units in scope as its environment and the baseline's module and record whole, and adopts each of the baseline's verdicts that still stands, by the reads its own walk filed (`curios_cert::Baseline`).
///
/// Nothing here says which declarations are kept. A declaration elaborated again that came out as the baseline holds it, reading nothing that moved, is adopted like one never touched, and one the elaborator kept is judged where what the kernel's own walk read for it has changed: the verdicts that are filed rest on the certifier's record, and on one claim of the fold's, which of these `predecessors` are the units `baseline` was compiled after, `moved` naming the others ([`Cache::baseline`](crate::Cache::baseline)).
pub(crate) fn recheck_over(
    module: &Zonked<Module>,
    budget: u64,
    predecessors: Predecessors<'_>,
    baseline: &Unit,
    moved: &[bool],
    syntax: &SyntaxRegistry,
) -> Rechecked {
    certify_module_over(
        module,
        budget,
        &globals_after(predecessors, moved),
        Baseline {
            module: baseline.core(),
            places: baseline.places(),
            certification: baseline.certification(),
        },
        *syntax,
    )
}
