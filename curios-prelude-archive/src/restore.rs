//! One-time validation and per-thread restoration of the generated image.

use {
    crate::{ArchivedPreludeArchive, PreludeArchive, SCHEMA},
    curios_core::Module,
    curios_core::Term,
    curios_elab::ErasedPrelude,
    curios_text::PreparedPrelude,
    std::{cell::LazyCell, sync::OnceLock},
};

const BYTES: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/prelude.rkyv"));
const EXPECTED_FINGERPRINT: &str = env!("CURIOS_PRELUDE_FINGERPRINT");

static ARCHIVE: OnceLock<Result<&'static ArchivedPreludeArchive, String>> = OnceLock::new();

/// The reusable, restored fixed prelude. Fields stay private so callers cannot mutate compiler-global state between invocations.
pub struct Prelude {
    prepared: PreparedPrelude,
    core: Module,
    binder_floor: usize,
    body_type: Term,
    ersd: ErasedPrelude,
}

impl Prelude {
    pub fn prepared(&self) -> &PreparedPrelude {
        &self.prepared
    }

    pub fn core(&self) -> &Module {
        &self.core
    }

    /// The binder floor derived over [`core`](Self::core) when this image was built.
    pub fn binder_floor(&self) -> usize {
        self.binder_floor
    }

    pub fn body_type(&self) -> &Term {
        &self.body_type
    }

    /// The arena prelude prefix — the erased module and environment production replay resumes over. Returned as an owned clone because replay consumes it by value, so a compile's mutation of its copy can never poison a later one.
    pub fn ersd(&self) -> ErasedPrelude {
        self.ersd.clone()
    }
}

thread_local! {
    static PRELUDE: LazyCell<Prelude> = LazyCell::new(|| {
        let image = restore_archive();

        Prelude {
            prepared: image.prepared,
            core: image.core,
            binder_floor: image.binder_floor,
            body_type: image.body_type,
            ersd: image.ersd,
        }
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
    let image = curios_archive::rkyv::access::<
        ArchivedPreludeArchive,
        curios_archive::rkyv::rancor::Error,
    >(bytes)
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
    curios_archive::rkyv::deserialize::<PreludeArchive, curios_archive::rkyv::rancor::Error>(
        archived(),
    )
    .unwrap_or_else(|error| panic!("validated archived prelude failed to restore: {error}"))
}

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|byte| format!("{byte:02x}")).collect()
}

/// Borrow this thread's reusable restored Text/Core prelude.
pub fn with_prelude<R>(use_prelude: impl FnOnce(&Prelude) -> R) -> R {
    curios_profile::profile!("with_prelude");
    PRELUDE.with(|prelude| use_prelude(prelude))
}

#[cfg(test)]
mod tests {
    use {
        super::*,
        crate::SYNTAX,
        curios_cert::{
            Globals, KernelError, recheck_module_verdicts, recheck_module_verdicts_uncached,
        },
        curios_core::Global,
        curios_core::Item,
        curios_elab::DEFAULT_STEP_BUDGET,
        std::collections::{BTreeMap, BTreeSet},
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
            let first = prelude.ersd();
            assert!(!first.is_empty());
            drop(first);
            assert!(!prelude.ersd().is_empty());
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

    /// Memoization is an evaluation strategy exactly as long as switching it off changes nothing. This runs the whole-prelude walk both ways and requires the verdict lists identical — the same instrument that validated the defunctionalized judgment (identical counts across profiles).
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
}
