//! The certifying half of the fixed prelude's build.
//!
//! `curios-prelude-archive` produced an image. This restores it, walks every item with the independent kernel, and panics on the first refusal — so this crate compiles only if the kernel accepted the whole prelude, and nothing can reach the prelude except through a crate that compiled.
//!
//! That is the verdict, and it is a build artifact rather than a recorded claim: exactly what Coq's `.vok` is, an otherwise-empty file whose existence means the proofs checked. What it does file is the one thing a later walk reads of it: the certifier's record of each root's definitions — their totality, closed over what each mentions — at `certification.rkyv` under `OUT_DIR`, with which the crate certifies the units it restores.

use {
    curios_cert::{Globals, Rechecked, certify_module},
    curios_core::{Certification, Zonked},
    std::{fs, path::PathBuf},
};

fn main() {
    // Under the `profile` feature certification runs under a record stream filed beside this crate, as the archive's build files the elaboration's beside its own, so the two halves of the prelude's build are read by one instrument.
    #[cfg(feature = "profile")]
    curios_profile::trace_build_script(certify);
    #[cfg(not(feature = "profile"))]
    certify();
}

fn certify() {
    println!("cargo:rerun-if-changed=build.rs");

    // The *restored* images, not the values the producing script held before serializing them. Those differ — hash-consing and the round trip sit between them — and it is the restored ones every compilation actually uses, so they are the ones worth certifying.
    curios_prelude_archive::with_prelude(|prelude| {
        // Built up as the fold goes rather than taken from `Globals::default()`: `/std` names `/sys`, so judging it against an empty environment would refuse every intrinsic carrier it wraps. The assembler `curios-pipeline` offers is unreachable from here — a build script that reached the compiler boundary would pull the whole pipeline into the prelude's own build — so the environment is mounted by hand, which is two lines and says exactly what the fold order means.
        let mut globals = Globals::default();
        let mut items = 0;
        let mut certifications = Vec::new();

        for unit in prelude {
            let core = unit.core();
            // The restored image must be meta-free before the kernel walks it; projecting here is also what validates that claim about the archive at the same boundary that certifies its items.
            let zonked = Zonked::project(core).unwrap_or_else(|refusal| {
                panic!("a restored prelude root is not zonked: {refusal}")
            });
            let Rechecked {
                verdicts: refusals,
                certification,
            } = certify_module(
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

            // With the record this walk just made, so `/std`'s walk reads the certifier's classification of `/sys` rather than classifying it again.
            globals.mount(core, &certification);
            items += core.items.len();
            certifications.push(certification);
        }

        file_certifications(&certifications);

        // Plain stdout rather than `cargo:warning=`, for the same reason as the archive's metric — and because the verdict is not this line. The panic above is: a refusal fails the build, so a build that finished has already certified. What this adds is the count, which changes only when the prelude does.
        println!("fixed prelude certified: {items} items accepted by the kernel");
    });
}

/// File the roots' records, in the fold's order, where the crate's own restoration reads them: under `OUT_DIR`, as the images are, since the crate is their one reader.
fn file_certifications(certifications: &Vec<Certification>) {
    let out = PathBuf::from(
        std::env::var_os("OUT_DIR").expect("Cargo runs a build script with `OUT_DIR` set"),
    );

    let bytes = curios_archive::to_bytes(certifications)
        .unwrap_or_else(|error| panic!("the prelude's certification failed to serialize: {error}"));
    let path = out.join("certification.rkyv");
    fs::write(&path, &*bytes)
        .unwrap_or_else(|error| panic!("failed to write {}: {error}", path.display()));
}
