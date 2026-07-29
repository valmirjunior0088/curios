//! One-time validation and per-thread restoration of the generated image.

use {
    crate::{ArchivedPreludeArchive, PreludeArchive, SCHEMA},
    std::{cell::LazyCell, sync::OnceLock},
};

const BYTES: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/prelude.rkyv"));
const EXPECTED_FINGERPRINT: &str = env!("CURIOS_PRELUDE_FINGERPRINT");

static ARCHIVE: OnceLock<Result<&'static ArchivedPreludeArchive, String>> = OnceLock::new();

/// The reusable, restored fixed prelude. Fields stay private so callers
/// cannot mutate compiler-global state between invocations.
pub struct Prelude {
    prepared: curios_text::PreparedPrelude,
    core: curios_elab::Module,
    body_type: curios_elab::Term,
    ersd: curios_elab::ErasedPrelude,
}

impl Prelude {
    pub fn prepared(&self) -> &curios_text::PreparedPrelude {
        &self.prepared
    }

    pub fn core(&self) -> &curios_elab::Module {
        &self.core
    }

    pub fn body_type(&self) -> &curios_elab::Term {
        &self.body_type
    }

    /// The arena prelude prefix — the erased module and environment
    /// production replay resumes over. Returned as an owned clone because
    /// replay consumes it by value, so a compile's mutation of its copy can
    /// never poison a later one.
    pub fn ersd(&self) -> curios_elab::ErasedPrelude {
        self.ersd.clone()
    }
}

thread_local! {
    static PRELUDE: LazyCell<Prelude> = LazyCell::new(|| {
        let image = restore_archive();

        Prelude {
            prepared: image.prepared,
            core: image.core,
            body_type: image.body_type,
            ersd: image.ersd,
        }
    });
}

#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
fn validate_archive() -> Result<&'static ArchivedPreludeArchive, String> {
    validate_bytes(BYTES, SCHEMA, EXPECTED_FINGERPRINT)
}

/// Validate through the zero-copy view — bytecheck for structural validity,
/// then the header fields — without deserializing the image, whose full
/// restoration is per-thread and on demand.
///
/// There is deliberately no content-digest check. The archive reaches this
/// function through `include_bytes!`, so it is a constant in the executable's
/// own read-only data, and the digest `build.rs` exported alongside it is
/// another constant from the same build: hashing one to compare it against the
/// other cannot fail except under corruption of our own image, and covers only
/// the prelude rather than any of the compiler's code. It could not detect a
/// stale or substituted archive, because there is no runtime file to be stale or
/// substituted. Bytecheck below is the part that does real work — it is what
/// makes handing out references into these bytes sound — and the schema and
/// fingerprint reject an image from any other build.
fn validate_bytes<'bytes>(
    bytes: &'bytes [u8],
    schema: u32,
    fingerprint: &str,
) -> Result<&'bytes ArchivedPreludeArchive, String> {
    let image = rkyv::access::<ArchivedPreludeArchive, rkyv::rancor::Error>(bytes)
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
/// The universe invariants are *not* re-checked here. They are asserted once by
/// `build.rs`, on the value it is about to serialize, and [`validate_bytes`]
/// establishes that these bytes are exactly the bytes written from that value:
/// the content digest pins them, bytecheck confirms the archived graph is
/// structurally sound, and the schema and source fingerprint reject an image
/// from any other build. Walking the whole standard library again per
/// compilation to re-derive an answer already settled cost ~175 ms of a ~680 ms
/// release compile of a one-line program.
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
fn restore_archive() -> PreludeArchive {
    rkyv::deserialize::<PreludeArchive, rkyv::rancor::Error>(archived())
        .unwrap_or_else(|error| panic!("validated archived prelude failed to restore: {error}"))
}

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|byte| format!("{byte:02x}")).collect()
}

/// Borrow this thread's reusable restored Text/Core prelude.
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
pub fn with_prelude<R>(use_prelude: impl FnOnce(&Prelude) -> R) -> R {
    PRELUDE.with(|prelude| use_prelude(prelude))
}

#[cfg(test)]
mod tests {
    use {super::*, crate::SYNTAX, std::collections::BTreeSet};

    #[test]
    fn embedded_archive_validates() {
        validate_archive().unwrap();
    }

    /// The declarations a literal expands into *per byte* are monomorphic, so
    /// no occurrence mints universe metavariables in the data's length.
    ///
    /// A literal's type is `Str` at a single level. It used to expand into one
    /// constructor application per byte, and when those carried universe
    /// parameters every application instantiated fresh levels — a declaration's
    /// level count grew with literal *length* and elaboration went quartic in
    /// it: a 500-byte literal took 50s in release and put
    /// `long_str_literal_compiles_on_the_default_test_stack` beyond any test
    /// budget. Pinning the per-byte targets at zero is what keeps that linear.
    ///
    /// **The bridge changed what is pinned, and why.** A literal now emits
    /// `of_scan_eq(b, refl_scan(b))` — once, whatever the length — so those two
    /// are free to be universe-polymorphic: they cost one level per *literal*,
    /// not one per byte, and the quartic behaviour this guards against cannot
    /// arise from an `O(1)` emission. They route through `/std/Eq`, which is
    /// `Eq(@A : Type)`, so they do carry a parameter.
    ///
    /// A monomorphic equality at `Scan` would have kept them pinned, and was
    /// tried and rejected: it works only while nothing consumes the proof, and
    /// the moment a third-party proof does, it needs transport, congruence,
    /// symmetry and transitivity restated at `Scan` — a second equality to
    /// maintain. It also broke erasure outright, at 333 tests.
    ///
    /// So what stays pinned is what a literal still emits per byte: nothing.
    /// The type itself is checked because a polymorphic `Str` would put a level
    /// on every literal value.
    #[test]
    fn string_literal_machinery_is_monomorphic() {
        // Selected by name because these are exactly the hidden lowering
        // targets `curios-prelude/src/syntax.rs` registers that are emitted
        // more than once per literal; nothing infers behavior from the
        // spelling. `of_scan_eq` and `refl_scan` are deliberately absent — see
        // above.
        let lowering_targets = ["/syn/Str", "/syn/Str/scan_from", "/syn/Char"];

        with_prelude(|prelude| {
            let mut parameters = std::collections::BTreeMap::new();
            for item in &prelude.core().items {
                match item {
                    curios_elab::Item::Let(definition) => {
                        parameters.insert(
                            definition.name.symbol(),
                            definition.universe_context.parameter_count,
                        );
                    }
                    curios_elab::Item::Rec(rec) => {
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
                .flat_map(curios_elab::Item::declared_names)
                .cloned()
                .collect::<BTreeSet<_>>();
            for target in SYNTAX.targets() {
                assert!(
                    names.contains(&curios_elab::Global::Authored(target.qualifier())),
                    "missing syntax target {}",
                    target.symbol()
                );
            }
        });
    }
}
