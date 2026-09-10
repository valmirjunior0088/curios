//! The certifying half of the fixed prelude's build.
//!
//! `curios-prelude-archive` produced an image. This restores it, walks every item with the independent kernel, and panics on the first refusal — so this crate compiles only if the kernel accepted the whole prelude, and nothing can reach the prelude except through a crate that compiled.
//!
//! That is the verdict, and it is a build artifact rather than a recorded claim: exactly what Coq's `.vok` is, an otherwise-empty file whose existence means the proofs checked. There is nothing to serialize here and nothing for a later pass to believe.

use {
    curios_cert::{Globals, recheck_module_verdicts},
    curios_core::Zonked,
};

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    // The *restored* images, not the values the producing script held before serializing them. Those differ — hash-consing and the round trip sit between them — and it is the restored ones every compilation actually uses, so they are the ones worth certifying.
    curios_prelude_archive::with_prelude(|prelude| {
        // Built up as the fold goes rather than taken from `Globals::default()`: `/std` names `/sys`, so judging it against an empty environment would refuse every intrinsic carrier it wraps. The assembler `curios-pipeline` offers is unreachable from here — a build script that reached the compiler boundary would pull the whole pipeline into the prelude's own build — so the environment is mounted by hand, which is two lines and says exactly what the fold order means.
        let mut globals = Globals::default();
        let mut items = 0;

        for unit in prelude {
            let core = unit.core();
            // The restored image must be meta-free before the kernel walks it; projecting here is also what validates that claim about the archive at the same boundary that certifies its items.
            let zonked = Zonked::project(core).unwrap_or_else(|refusal| {
                panic!("a restored prelude root is not zonked: {refusal}")
            });
            let refusals = recheck_module_verdicts(
                &zonked,
                curios_prelude_archive::DEFAULT_STEP_BUDGET,
                &globals,
                curios_prelude_archive::SYNTAX,
            );

            if let Some(verdict) = refusals.first() {
                // The name, not only the error: a `KernelError` renders terms and sorts and never the top-level item it came from, so without this the one diagnostic this crate exists to produce points nowhere in a prelude of a thousand items.
                let name = match &verdict.name {
                    Some(name) => format!("{name}"),
                    None => "<entrypoint>".to_owned(),
                };

                panic!(
                    // A refusal count over a corpus size, not a subset of it: the walk pushes a verdict per *failing pass*, and its passes run over declarations and over each definition of a `rec` group, so the count can exceed the item count outright.
                    "fixed prelude failed the kernel: {} refusals over {} items, first: {name} — {}",
                    refusals.len(),
                    core.items.len(),
                    verdict.error,
                );
            }

            globals.mount(core, unit.binder_floor());
            items += core.items.len();
        }

        // Plain stdout rather than `cargo:warning=`, for the same reason as the archive's metric — and because the verdict is not this line. The panic above is: a refusal fails the build, so a build that finished has already certified. What this adds is the count, which changes only when the prelude does.
        println!("fixed prelude certified: {items} items accepted by the kernel");
    });
}
