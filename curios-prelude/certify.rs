//! The certifying half of the fixed prelude's build: the restored images walked by the independent kernel, each root against the roots before it, and the certifier's record of them filed.
//!
//! A module of two programs that stand beside it. `build.rs` is the one Cargo runs, into the crate's `OUT_DIR`; `profile.rs` runs the same work under a recorder for whoever asks, into a directory of its own.

use {
    curios_cert::{Globals, Rechecked, certify_module},
    curios_core::{Certification, Zonked},
    curios_elab::DEFAULT_STEP_BUDGET,
    curios_text::SYNTAX,
    std::{fs, path::Path},
};

/// Certify the prelude and file the certifier's record of each root in `out`. Panics on the first refusal: a prelude the kernel does not accept is no prelude.
pub(crate) fn certify(out: &Path) {
    // The *restored* images, not the values the producing script held before serializing them. Those differ — hash-consing and the round trip sit between them — and it is the restored ones every compilation actually uses, so they are the ones worth certifying.
    curios_prelude_archive::with_prelude(|prelude| {
        let mut items = 0;
        let mut certifications: Vec<Certification> = Vec::new();

        for unit in prelude {
            let core = unit.core();
            // The restored image must be meta-free before the kernel walks it; projecting here is also what validates that claim about the archive at the same boundary that certifies its items.
            let zonked = Zonked::project(core).unwrap_or_else(|refusal| {
                panic!("a restored prelude root is not zonked: {refusal}")
            });
            let Rechecked {
                verdicts: refusals,
                certification,
            } = {
                // The roots before this one rather than `Globals::default()`: `/std` names `/sys`, so judging it against an empty environment would refuse every intrinsic carrier it wraps. Each with the record its walk just made, so `/std`'s walk reads the certifier's classification of `/sys` rather than classifying it again. The assembler `curios-pipeline` offers is unreachable from here — a build script that reached the compiler boundary would pull the whole pipeline into the prelude's own build — so the environment is mounted by hand, which is three lines and says exactly what the fold order means.
                let mut globals = Globals::default();
                for (earlier, certification) in prelude.iter().zip(&certifications) {
                    globals.mount(earlier.core(), earlier.places(), certification);
                }
                // Each is the compiler's own, of which no read is recorded.
                globals.standing(
                    &vec![true; certifications.len()],
                    &vec![false; certifications.len()],
                );

                certify_module(&zonked, DEFAULT_STEP_BUDGET, &globals, SYNTAX)
            };

            if let Some(verdict) = refusals.first() {
                // The name, not only the error: a `curios_cert::Error` renders terms and sorts and never the top-level item it came from, so without this the one diagnostic this crate exists to produce points nowhere in a prelude of a thousand items.
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

            items += core.items.len();
            certifications.push(certification);
        }

        file_certifications(out, &certifications);

        // Plain stdout rather than `cargo:warning=`, for the same reason as the archive's metric — and because the verdict is not this line. The panic above is: a refusal fails the build, so a build that finished has already certified. What this adds is the count, which changes only when the prelude does.
        println!("fixed prelude certified: {items} items accepted by the kernel");
    });
}

/// File the roots' records, in the fold's order, in `out`: `certification.rkyv`, and beside it what judging each definition read.
fn file_certifications(out: &Path, certifications: &Vec<Certification>) {
    let bytes = curios_archive::to_bytes(certifications)
        .unwrap_or_else(|error| panic!("the prelude's certification failed to serialize: {error}"));
    let path = out.join("certification.rkyv");
    fs::write(&path, &*bytes)
        .unwrap_or_else(|error| panic!("failed to write {}: {error}", path.display()));

    // What judging each definition read, each root's behind its length: a record is filed without them, and only a walk over a root as a baseline reads them.
    let reads = certifications
        .iter()
        .map(Certification::reads_bytes)
        .collect::<Result<Vec<_>, _>>()
        .unwrap_or_else(|error| panic!("the prelude's reads failed to serialize: {error}"));
    let path = out.join("reads.rkyv");
    fs::write(&path, curios_archive::joined(&reads))
        .unwrap_or_else(|error| panic!("failed to write {}: {error}", path.display()));
}
