//! One-time validation and per-thread restoration of the generated image.

use {
    crate::{ArchivedPreludeArchive, PreludeArchive, SCHEMA},
    sha2::{Digest, Sha256},
    std::{cell::LazyCell, sync::OnceLock},
};

const BYTES: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/prelude.rkyv"));
const EXPECTED_FINGERPRINT: &str = env!("CURIOS_PRELUDE_FINGERPRINT");
const EXPECTED_ARCHIVE_DIGEST: &str = env!("CURIOS_PRELUDE_ARCHIVE_DIGEST");

static ARCHIVE: OnceLock<Result<&'static ArchivedPreludeArchive, String>> = OnceLock::new();

/// The reusable, restored fixed prelude. Fields stay private so callers
/// cannot mutate compiler-global state between invocations.
pub struct Prelude {
    prepared: curios_text::PreparedPrelude,
    core: curios_core::Module,
    body_type: curios_core::Term,
    ersd: curios_core::ErasedPrelude,
}

impl Prelude {
    pub fn prepared(&self) -> &curios_text::PreparedPrelude {
        &self.prepared
    }

    pub fn core(&self) -> &curios_core::Module {
        &self.core
    }

    pub fn body_type(&self) -> &curios_core::Term {
        &self.body_type
    }

    /// The arena prelude prefix — the erased module and environment
    /// production replay resumes over. Returned as an owned clone because
    /// replay consumes it by value, so a compile's mutation of its copy can
    /// never poison a later one.
    pub fn ersd(&self) -> curios_core::ErasedPrelude {
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
    validate_bytes(BYTES, SCHEMA, EXPECTED_FINGERPRINT, EXPECTED_ARCHIVE_DIGEST)
}

/// Validate through the zero-copy view — bytecheck for structural validity,
/// then the header fields — without deserializing the image, whose full
/// restoration is per-thread and on demand.
fn validate_bytes<'bytes>(
    bytes: &'bytes [u8],
    schema: u32,
    fingerprint: &str,
    archive_digest: &str,
) -> Result<&'bytes ArchivedPreludeArchive, String> {
    if hex(&Sha256::digest(bytes)) != archive_digest {
        return Err("archived prelude content digest mismatch".into());
    }
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

    #[test]
    fn truncated_archive_is_rejected() {
        let truncated = &BYTES[..BYTES.len() / 2];
        assert!(
            validate_bytes(
                truncated,
                SCHEMA,
                EXPECTED_FINGERPRINT,
                EXPECTED_ARCHIVE_DIGEST
            )
            .is_err()
        );
    }

    #[test]
    fn schema_and_fingerprint_are_checked() {
        assert!(
            validate_bytes(
                BYTES,
                SCHEMA + 1,
                EXPECTED_FINGERPRINT,
                EXPECTED_ARCHIVE_DIGEST
            )
            .is_err()
        );
        assert!(
            validate_bytes(
                BYTES,
                SCHEMA,
                "not-the-build-fingerprint",
                EXPECTED_ARCHIVE_DIGEST
            )
            .is_err()
        );
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
                .flat_map(curios_core::Item::declared_names)
                .collect::<BTreeSet<_>>();
            for target in SYNTAX.targets() {
                assert!(names.contains(target), "missing syntax target {target}");
            }
        });
    }
}
