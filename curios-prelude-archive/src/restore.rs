//! One-time validation and per-thread restoration of the generated image.

use {
    crate::{ArchivedPreludeArchive, PreludeArchive, SCHEMA},
    curios_unit::Unit,
    std::{cell::LazyCell, sync::OnceLock},
};

const BYTES: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/prelude.rkyv"));
const EXPECTED_FINGERPRINT: &str = env!("CURIOS_PRELUDE_FINGERPRINT");

static ARCHIVE: OnceLock<Result<&'static ArchivedPreludeArchive, String>> = OnceLock::new();

thread_local! {
    /// The reusable, restored fixed prelude, as the unit it is. Held behind [`with_prelude`] so callers cannot mutate compiler-global state between invocations.
    static PRELUDE: LazyCell<Unit> = LazyCell::new(|| {
        let image = restore_archive();

        Unit::new(
            image.prepared,
            image.core,
            image.ersd,
            image.binder_floor,
        )
    });
}

fn validate_archive() -> Result<&'static ArchivedPreludeArchive, String> {
    curios_profile::profile!("validate_archive");
    validate_bytes(BYTES, SCHEMA, EXPECTED_FINGERPRINT)
}

/// Validate through the zero-copy view — bytecheck for structural validity, then the header fields — without deserializing the image, whose full restoration is per-thread and on demand.
///
/// There is deliberately no content-digest check. The archive reaches this function through `include_bytes!`, so it is a constant in the executable's own read-only data, and a digest computed by `build.rs` would be another constant from the same build: hashing one to compare it against the other could not fail except under corruption of our own image, and would cover only the prelude rather than any of the compiler's code. It could not detect a stale or substituted archive, because there is no runtime file to be stale or substituted. Bytecheck below is the part that does real work — it is what makes handing out references into these bytes sound — and the schema and fingerprint reject an image from any other build.
fn validate_bytes<'bytes>(
    bytes: &'bytes [u8],
    schema: u32,
    fingerprint: &str,
) -> Result<&'bytes ArchivedPreludeArchive, String> {
    let image = curios_archive::access::<ArchivedPreludeArchive>(bytes)
        .map_err(|error| format!("invalid archived prelude: {error}"))?;

    if image.schema.to_native() != schema {
        return Err(format!(
            "archived prelude schema mismatch: expected {schema}, found {}",
            image.schema
        ));
    }

    if hex(&image.fingerprint) != fingerprint {
        return Err("archived prelude source fingerprint mismatch".into());
    }

    Ok(image)
}

fn archived() -> &'static ArchivedPreludeArchive {
    match ARCHIVE.get_or_init(validate_archive) {
        Ok(archived) => archived,
        Err(error) => panic!("{error}"),
    }
}

/// Deserialize the validated image.
///
/// The universe invariants are *not* re-checked here. They are asserted once by `build.rs`, on the value it is about to serialize, and [`validate_bytes`] establishes that these bytes are exactly the bytes written from that value: bytecheck confirms the archived graph is structurally sound, and the schema and source fingerprint reject an image from any other build. Walking the whole standard library again per compilation to re-derive an answer already settled cost ~175 ms of a ~680 ms release compile of a one-line program.
fn restore_archive() -> PreludeArchive {
    curios_profile::profile!("restore_archive");
    curios_archive::deserialize::<PreludeArchive>(archived())
        .unwrap_or_else(|error| panic!("validated archived prelude failed to restore: {error}"))
}

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|byte| format!("{byte:02x}")).collect()
}

/// Borrow this thread's reusable restored prelude unit.
pub fn with_prelude<R>(use_prelude: impl FnOnce(&Unit) -> R) -> R {
    curios_profile::profile!("with_prelude");
    PRELUDE.with(|prelude| use_prelude(prelude))
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        crate::SYNTAX,
        curios_cert::{
            Globals, KernelError, recheck_module_measured, recheck_module_verdicts,
            recheck_module_verdicts_uncached,
        },
        curios_core::Global,
        curios_core::Item,
        curios_core::Term,
        curios_core::derived_binder_floor,
        curios_elab::{Context, DEFAULT_STEP_BUDGET, ErasedArena, Resumed, erase_unit},
        std::{
            collections::{BTreeMap, BTreeSet},
            thread,
            time::Instant,
        },
    };

    #[test]
    fn embedded_archive_validates() {
        validate_archive().unwrap();
    }

    /// The declarations a literal expands into *per byte* are monomorphic, so no occurrence mints universe metavariables in the data's length.
    ///
    /// A literal's type is `Str` at a single level. It used to expand into one constructor application per byte, and when those carried universe parameters every application instantiated fresh levels — a declaration's level count grew with literal *length* and elaboration went quartic in it: a 500-byte literal took 50s in release and put `long_str_literal_compiles_on_the_default_test_stack` beyond any test budget. Pinning the per-byte targets at zero is what keeps that linear.
    ///
    /// **The bridge changed what is pinned, and why.** A literal now emits `of_scan_eq(b, refl_scan(b))` — once, whatever the length — so those two are free to be universe-polymorphic: they cost one level per *literal*, not one per byte, and the quartic behaviour this guards against cannot arise from an `O(1)` emission. They route through `/std/Eq`, which is `Eq(@A : Type)`, so they do carry a parameter.
    ///
    /// A monomorphic equality at `Scan` would have kept them pinned, and was tried and rejected: it works only while nothing consumes the proof, and the moment a third-party proof does, it needs transport, congruence, symmetry and transitivity restated at `Scan` — a second equality to maintain. It also broke erasure outright, at 333 tests.
    ///
    /// So what stays pinned is what a literal still emits per byte: nothing. The type itself is checked because a polymorphic `Str` would put a level on every literal value.
    #[test]
    fn string_literal_machinery_is_monomorphic() {
        // Selected by name because these are exactly the hidden lowering targets `curios-prelude/src/syntax.rs` registers that are emitted more than once per literal; nothing infers behavior from the spelling. `of_scan_eq` and `refl_scan` are deliberately absent — see above.
        let lowering_targets = ["/syn/Str", "/syn/Str/scan_from", "/syn/Char"];

        with_prelude(|prelude| {
            let mut parameters = std::collections::BTreeMap::new();
            for item in &prelude.core().items {
                match item {
                    Item::Let(definition) => {
                        parameters.insert(
                            definition.name.symbol(),
                            definition.universe_context.parameter_count,
                        );
                    }
                    Item::Rec(rec) => {
                        for definition in rec.definitions() {
                            parameters.insert(
                                definition.name.symbol(),
                                definition.universe_context.parameter_count,
                            );
                        }
                    }
                }
            }

            let mut checked = 0;
            for target in lowering_targets {
                let Some(count) = parameters.get(target) else {
                    continue;
                };
                checked += 1;
                assert_eq!(
                    *count, 0,
                    "{target} is universe-polymorphic; every literal byte will mint levels"
                );
            }
            // Without this the test passes vacuously if `/syn` is renamed.
            assert!(
                checked >= lowering_targets.len() / 2,
                "found only {checked} of the expected lowering targets; \
                 the `/syn` names this pins have moved"
            );
        });
    }

    #[test]
    fn truncated_archive_is_rejected() {
        let truncated = &BYTES[..BYTES.len() / 2];
        assert!(validate_bytes(truncated, SCHEMA, EXPECTED_FINGERPRINT).is_err());
    }

    #[test]
    fn schema_and_fingerprint_are_checked() {
        assert!(validate_bytes(BYTES, SCHEMA + 1, EXPECTED_FINGERPRINT).is_err());
        assert!(validate_bytes(BYTES, SCHEMA, "not-the-build-fingerprint").is_err());
    }

    #[test]
    fn ersd_clones_are_fresh() {
        with_prelude(|prelude| {
            let first = prelude.arena();
            assert!(!first.is_empty());
            drop(first);
            assert!(!prelude.arena().is_empty());
        });
    }

    #[test]
    fn every_syntax_target_is_present_after_restore() {
        with_prelude(|prelude| {
            let names = prelude
                .core()
                .items
                .iter()
                .flat_map(Item::declared_names)
                .cloned()
                .collect::<BTreeSet<_>>();
            for target in SYNTAX.targets() {
                assert!(
                    names.contains(&Global::Authored(target.qualifier())),
                    "missing syntax target {}",
                    target.symbol()
                );
            }
        });
    }

    #[test]
    fn every_registered_concept_declares_its_method_after_restore() {
        with_prelude(|prelude| {
            for target in SYNTAX.concept_fields() {
                let concept = prelude
                    .core()
                    .concepts
                    .get(&Global::Authored(target.concept.qualifier()))
                    .unwrap_or_else(|| panic!("missing concept {}", target.concept.symbol()));
                assert!(
                    concept.fields.iter().any(|field| field == target.field),
                    "concept {} does not declare {}",
                    target.concept.symbol(),
                    target.field
                );
            }
        });
    }

    /// A term's printed head, clipped — enough to tell one refusal's shape from another's without pasting a standard-library type into a tally.
    fn head(term: &Term) -> String {
        let rendered = format!("{term}");
        let rendered = rendered.split_whitespace().collect::<Vec<_>>().join(" ");

        match rendered.char_indices().nth(44) {
            Some((cut, _)) => format!("{}…", &rendered[..cut]),
            None => rendered,
        }
    }

    /// The class a refusal is tallied under.
    ///
    /// Deliberately mechanical: the variant, plus for a mismatch the two sides' printed heads. Naming classes like "index inversion" here would be inventing categories from a heuristic, which is how this project's wrong answers get made — the point of the tally is to let the categories fall out of it.
    fn class(error: &KernelError) -> String {
        match error {
            KernelError::Mismatch { inferred, expected } => {
                format!("Mismatch  {}  vs  {}", head(inferred), head(expected))
            }
            other => {
                let rendered = format!("{other:?}");

                rendered
                    .split(['(', ' ', '{'])
                    .next()
                    .unwrap_or("?")
                    .to_string()
            }
        }
    }

    /// Every item the kernel refuses across the whole fixed prelude, tallied by class.
    ///
    /// Not an assertion — a measurement, run on demand. `recheck_module` stops at the first refusal and so says nothing about what lies past it; this walks to the end with each verdict independent of the others (see `recheck_module_verdicts`), which is what makes the classes countable rather than discovered one build at a time.
    ///
    /// # Why it lives here
    ///
    /// It used to sit in `curios/src/tests/kernel.rs` and reach the prelude by compiling a fixture, because a compiled module carried the standard library spliced in front of the user's items. It no longer does, so the subject has to be named directly — and the restored image is what the subject *is*. Walking it from an empty environment is not the path production takes, which is exactly why this is an inventory and not an assertion: what asserts the prelude is acceptable is `curios-prelude`'s own build script, which runs this same walk and panics on the first refusal.
    ///
    /// # It used to abort in a debug build, and that was the defect
    ///
    /// Judgment depth once scaled with a `Str` literal's *length* — 103 nested judgments at 40 bytes, 324 at 160, 494 at 640 — because a literal is one certified-UTF-8 link per scalar and `infer`/`check` descended two frames per link. At roughly 21.5KiB of stack per level in a debug build that exhausted a 2MiB thread partway through `/std/Toml`, and no reduction budget could prevent it: a budget bounds steps, and depth is not steps.
    ///
    /// `infer` now defers the child obligations of an application, a constructor, and a record onto a stack rather than descending into them, so depth is bounded by written nesting. Both profiles complete, and both report the same verdicts — which is the check that this was a restructuring rather than a change of rule.
    ///
    /// The measurement that found it is worth keeping: a backtrace at depth 300 showed the stack was 300 `infer` and 298 `check` frames and *nothing else*, which retired an earlier diagnosis naming four functions that were never on it. Stack size is not something to hide behind either: raising `RUST_MIN_STACK` would have concealed this rather than fixed it.
    ///
    /// An abort rather than a tally is a finding, not noise: nothing here is wrapped in a catch, because a kernel that aborts is a kernel to fix.
    #[test]
    #[ignore = "inventory: measures where the kernel disagrees rather than asserting"]
    fn kernel_disagreements() {
        with_prelude(|prelude| {
            let verdicts =
                recheck_module_verdicts(prelude.core(), DEFAULT_STEP_BUDGET, &Globals::default());

            let mut tally: BTreeMap<String, usize> = BTreeMap::new();
            for verdict in &verdicts {
                *tally.entry(class(&verdict.error)).or_default() += 1;
            }

            println!(
                "\n=== {} of {} prelude items refused ===",
                verdicts.len(),
                prelude.core().items.len()
            );
            for (class, count) in &tally {
                println!("  {count:>4}  {class}");
            }
            for verdict in &verdicts {
                let name = match &verdict.name {
                    Some(name) => format!("{name}"),
                    None => "<entrypoint>".to_string(),
                };
                println!("        {name}  —  {}", class(&verdict.error));
            }
        });
    }

    /// Memoization is an evaluation strategy exactly as long as switching it off changes no *semantic* verdict. This runs the whole-prelude walk both ways and requires the verdict lists identical — the same instrument that validated the defunctionalized judgment (identical counts across profiles).
    ///
    /// **The semantic half is what this is for, rather than what it happens to cover.** A term-keyed memo hit spends nothing, so an uncached walk spends at least as much as a cached one and the two may reach different *exhaustion* points; `curios-cert`'s `spend` module argues why that is the whole of what the design gives up. Comparing verdicts at one budget, over a corpus where nothing exhausts, is therefore exactly the surviving property — acceptance, and refusals that are not exhaustion, are budget-independent and must agree. Asserting equal exhaustion points here would be asserting the design that was replaced.
    ///
    /// The prelude is the subject because the property needs a large body of real terms to mean anything, and this is the only one the workspace has. That it walks from an empty environment rather than production's is beside the point here: what is under test is the memo, which both walks use.
    #[test]
    #[ignore = "parity: runs the whole-prelude walk twice, the second time uncached"]
    fn kernel_memo_parity() {
        with_prelude(|prelude| {
            assert_eq!(
                recheck_module_verdicts(prelude.core(), DEFAULT_STEP_BUDGET, &Globals::default()),
                recheck_module_verdicts_uncached(
                    prelude.core(),
                    DEFAULT_STEP_BUDGET,
                    &Globals::default()
                ),
            );
        });
    }

    /// Every figure the projects specification's appendix leans on, taken over the stored image.
    ///
    /// # Why it is in-tree
    ///
    /// That appendix exists because two items in a predecessor document were designed against unattributed numbers and both were wrong — a 471 ms eager restore that is 34.4 ms, and an estimated 60–70 s saving on an operation that takes 11.8 s. Its own remedy was to record the date, the profile, and how to retake each figure; but "how to retake it" was a throwaway probe nobody kept, so retaking meant writing it again, and a figure nobody can cheaply reproduce is a figure that quietly decays into a claim about history. This is that probe, kept.
    ///
    /// # How to read it
    ///
    /// Numbers are only comparable to the recorded ones in `--release`, because these are the figures taken over the *stored* image and a debug build measures a different program:
    ///
    /// ```sh
    /// cargo test --release --package curios-prelude-archive -- --ignored --nocapture stored_prelude_measurements
    /// ```
    ///
    /// It asserts nothing and cannot fail — a measurement that fails is a measurement with an opinion. What it does not cover is stated where it prints: the elaboration figures come from the build script's own `profile.tsv` and need no probe, and the witness inventory is a term walk this does not do.
    ///
    /// # What it last printed
    ///
    /// Taken **2026-08-10**, **release**, immediately after the erased arena gained its compaction pass. Kept here rather than in a document that cites this test, so a number cannot drift from the thing that would check it.
    ///
    /// | What | Measured | Against |
    /// | --- | --- | --- |
    /// | Cold restore — bytecheck, then deserializing the prepared Text state, the Core and the erased prefix | 34.1–35.3 ms | 34.4 ms — confirmed |
    /// | Erased-prefix clone, taken once per compile | 2.0–2.1 ms, mean of 100 | 1.4 ms — see below |
    /// | Re-erasing one whole unit over the stored Core | 661–682 ms | 608 ms — up, partly spread |
    /// | Certifying one whole unit, from an empty environment | 11.9–12.0 s, 0 refusals | 11.8 s — confirmed |
    ///
    /// Shape, same run: 1091 items and 1107 definitions; 75 witnesses; 31 inductives, 47 structures, 14 concepts; `derived_binder_floor` **0** against a lowering watermark of 6748.
    ///
    /// **Ranges, not points**, because those are two runs and the spread between them is part of what the figure is: the re-erasure alone moved 21 ms between consecutive readings, which is half of what separates it from the number it replaces. Nobody should read 608 → 682 as a 12% regression on that evidence.
    ///
    /// **The clone's old figure was a single sample, and that was the defect.** Taken once the same way it read 3.7 ms, which would have looked like a 2.6× regression from the compaction pass; averaged over 100 it is 2.0 ms. At one-to-four milliseconds the noise band swallows the signal, which is why this one is averaged and the others are not. The 1.4 ms it replaces was never wrong so much as never repeated.
    ///
    /// **The certification figure was owed a retake and got one.** It was taken before the kernel's level substitution changed, and the kernel calls it — so the doubt was right to raise, and the answer is that nothing moved. Roughly twelve seconds is what a whole-unit certification costs, and it is the number the caching half is designed against.
    ///
    /// # Certification, retaken 2026-08-15
    ///
    /// A partial retake, of the certification row alone, taken on both sides of the kernel's `whnf`/`forced` memo hits becoming free — which came with those two tables cleared at every declaration boundary, so 1107 clearings over this walk is what needed checking. **6.2 s before, 6.1 s after**, 0 refusals both times, and `kernel_memo_parity` passing on both sides. The clearing costs nothing measurable.
    ///
    /// **It also says the twelve seconds above is stale by roughly a factor of two, for reasons that are not this.** Both readings here are of the unchanged kernel and both are near six seconds. Nothing in that change could halve a whole-unit walk — it only ever *reduces* what a judgment spends, and this corpus exhausts nowhere — so the drift belongs to something between 2026-08-10 and now that the row above has not been re-run against. The rest of the table is not retaken and is not corrected on the strength of one row.
    #[test]
    #[ignore = "measurement: reports timings over the stored image rather than asserting"]
    fn stored_prelude_measurements() {
        // A restore happens once per thread, so a *cold* one needs a thread that has not had one — measuring it on this thread would measure a thread-local read.
        let cold = thread::spawn(|| {
            let start = Instant::now();
            with_prelude(|_| ());

            start.elapsed()
        })
        .join()
        .expect("the restoring thread");

        with_prelude(|prelude| {
            let core = prelude.core();

            // Averaged, because a single shot at this magnitude is not a measurement: the other three take long enough that one sample says something, and this one does not.
            const CLONES: u32 = 100;
            let start = Instant::now();
            for _ in 0..CLONES {
                drop(prelude.arena());
            }
            let clone = start.elapsed() / CLONES;

            let mut erasure_context = Context::new(DEFAULT_STEP_BUDGET, SYNTAX);
            let start = Instant::now();
            erase_unit(
                &mut erasure_context,
                Resumed::of(&[], ErasedArena::default()),
                core,
                None,
            )
            .expect("the stored prelude re-erases");
            let erasure = start.elapsed();

            let start = Instant::now();
            let (verdicts, kernel) =
                recheck_module_measured(core, DEFAULT_STEP_BUDGET, &Globals::default());
            let certification = start.elapsed();
            let retained = kernel.retained();
            let heaviest = kernel.heaviest_declaration();

            let definitions: usize = core
                .items
                .iter()
                .map(|item| match item {
                    Item::Let(_) => 1,
                    Item::Rec(rec) => rec.definitions().len(),
                })
                .sum();

            println!("\n=== timings, over the stored image ===");
            println!("  cold restore                 {:>10.1?}", cold);
            println!(
                "  erased-prefix clone          {:>10.1?}   (mean of {CLONES})",
                clone
            );
            println!("  re-erasing one whole unit    {:>10.1?}", erasure);
            println!(
                "  certifying one whole unit    {:>10.1?}  ({} refusals)",
                certification,
                verdicts.len()
            );
            println!(
                "  ...retaining                 {retained:>10} units   (elaborator side, over the re-erasure: {})",
                erasure_context.retained()
            );
            println!(
                "  ...heaviest declaration      {:>10} units   (depth {}, costing {} of them)",
                heaviest.units(),
                heaviest.peak_depth(),
                heaviest.frame_units()
            );

            println!("\n=== shape ===");
            println!("  items                        {:>10}", core.items.len());
            println!("  definitions                  {definitions:>10}");
            println!(
                "  witnesses                    {:>10}",
                core.witnesses.len()
            );
            println!(
                "  inductives                   {:>10}",
                core.induct_decls.len()
            );
            println!(
                "  structures                   {:>10}",
                core.struct_decls.len()
            );
            println!("  concepts                     {:>10}", core.concepts.len());
            println!(
                "  derived binder floor         {:>10}   (lowering watermark {})",
                derived_binder_floor(core),
                core.binder_floor
            );

            println!(
                "\nNot measured here: the elaboration figures, which the build script already writes to `OUT_DIR/profile.tsv` (retake with `cargo build --release --package curios-prelude --features profile`), and the witness inventory B1 needs, which is a term walk.\n"
            );
        });
    }
}
