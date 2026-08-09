//! The certifying half of the fixed prelude's build.
//!
//! `curios-prelude-archive` produced an image. This restores it, walks every item with the independent kernel, and panics on the first refusal — so this crate compiles only if the kernel accepted the whole prelude, and nothing can reach the prelude except through a crate that compiled.
//!
//! That is the verdict, and it is a build artifact rather than a recorded claim: exactly what Coq's `.vok` is, an otherwise-empty file whose existence means the proofs checked. There is nothing to serialize here and nothing for a later pass to believe.

use curios_cert::{Globals, recheck_module_verdicts};

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    // The *restored* image, not the value the producing script held before serializing it. Those differ — hash-consing and the round trip sit between them — and it is the restored one every compilation actually uses, so it is the one worth certifying.
    curios_prelude_archive::with_prelude(|prelude| {
        let core = prelude.core();
        let refusals = recheck_module_verdicts(
            core,
            curios_prelude_archive::DEFAULT_STEP_BUDGET,
            &Globals::default(),
        );

        if let Some(verdict) = refusals.first() {
            panic!(
                "fixed prelude failed the kernel: {} of {} items refused, first: {}",
                refusals.len(),
                core.items.len(),
                verdict.error,
            );
        }

        println!(
            "cargo:warning=fixed prelude certified: {} items accepted by the kernel",
            core.items.len(),
        );
    });
}
