//! One-time validation and per-thread restoration of the generated images: two units, `/sys` then `/std`, each framed by the build script as the cabinet frames a unit — a record of what the root was compiled from, ahead of the index of the certified [`Unit`] — beside one pack of the blobs the index names, as a cabinet keeps them beside an object.

use {
    curios_archive::{Bytes, parted},
    curios_common::Qualifier,
    curios_document::Documentation,
    curios_unit::{
        ArchivedRecord, ArchivedUnit, Pack, Record, Unit, apart_parts, halves, segments,
    },
    std::{cell::LazyCell, sync::OnceLock},
};

// From `OUT_DIR`, where the build script files them, by the names it sets for each root's place in the fold (`curios_prelude_build::image_files`).
//
// Two images rather than one paired file, because each is framed as a unit the cabinet keeps is: a pair would be a second framing beside the one `curios-unit` defines, and the fold that consumes these wants them separately anyway.
const SYS_BYTES: &[u8] = include_bytes!(concat!(
    env!("OUT_DIR"),
    "/",
    env!("CURIOS_PRELUDE_IMAGE_0")
));
const STD_BYTES: &[u8] = include_bytes!(concat!(
    env!("OUT_DIR"),
    "/",
    env!("CURIOS_PRELUDE_IMAGE_1")
));

// The blobs each image's index names, one pack beside each, as a cabinet keeps an object's packs beside its unit file.
const SYS_PACK: &[u8] =
    include_bytes!(concat!(env!("OUT_DIR"), "/", env!("CURIOS_PRELUDE_PACK_0")));
const STD_PACK: &[u8] =
    include_bytes!(concat!(env!("OUT_DIR"), "/", env!("CURIOS_PRELUDE_PACK_1")));

/// The documentation record of each root, in the fold's order: an image of its own, which nothing that compiles reads.
const DOCUMENTATION_BYTES: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/documentation.rkyv"));

/// The prelude's roots in dependency order, as the build script folded them, each by the word the build script names it by (`curios_prelude_build::prelude_roots`): `/sys` is lowered against nothing and `/std` against `/sys`, so restoring them in any other order would hand a successor a scope its own names were never resolved in.
const ROOTS: [(&str, &[u8], &[u8]); 2] = [
    (env!("CURIOS_PRELUDE_ROOT_0"), SYS_BYTES, SYS_PACK),
    (env!("CURIOS_PRELUDE_ROOT_1"), STD_BYTES, STD_PACK),
];

/// One image validated: its record and its unit, each as the zero-copy view bytecheck confirmed, what the unit stores apart from itself as the bytes it is, which nothing here reads, and the pack of the blobs its index names.
type Validated = Result<
    (
        &'static ArchivedRecord,
        &'static ArchivedUnit,
        &'static [u8],
        &'static [u8],
    ),
    String,
>;

static ARCHIVES: OnceLock<[Validated; 2]> = OnceLock::new();

thread_local! {
    /// The reusable, restored fixed prelude: each root's certified unit, in dependency order. Held behind [`with_prelude`] so callers cannot mutate compiler-global state between invocations.
    static PRELUDE: LazyCell<[Unit; 2]> =
        LazyCell::new(|| restore_archives().map(|(_, unit)| unit));
}

fn validate_archives() -> [Validated; 2] {
    curios_common::profile!("validate_archives");
    ROOTS.map(|(root, bytes, pack)| validate_bytes(root, bytes, pack))
}

/// Validate through the zero-copy view — bytecheck for structural validity — without deserializing the image, whose full restoration is per-thread and on demand. The image is framed as a stored unit is, so it is split at the one framing `curios-unit` states before either half is checked.
///
/// There is deliberately no content-digest check. The archive reaches this function through `include_bytes!`, so it is a constant in the executable's own read-only data, and a digest computed by `build.rs` would be another constant from the same build: hashing one to compare it against the other could not fail except under corruption of our own image, and would cover only the prelude rather than any of the compiler's code. It could not detect a stale or substituted archive, because there is no runtime file to be stale or substituted. Bytecheck is what does real work here — it is what makes handing out references into these bytes sound. Nor is there an image of other sources to reject: the bytes are made, or taken from the images the checkout keeps under their key, by the build of the sources that compiles this line, so a schema or source fingerprint beside them could only ever compare those sources against themselves.
///
/// The record is not checked against anything here either, for the same reason: nothing in this process is about to believe a verdict on its strength, nor reads it at all. It is carried because the image is framed exactly as an object is, and this crate's tests hold it to the tree.
fn validate_bytes(root: &str, bytes: &'static [u8], pack: &'static [u8]) -> Validated {
    let (record, artifact) =
        segments(bytes).ok_or_else(|| format!("invalid archived /{root}: not a stored unit"))?;
    let (unit, apart) = halves(artifact)
        .ok_or_else(|| format!("invalid archived /{root}: not a unit's artifact"))?;
    // Read lazily, so its layout is confirmed here: an image cut short anywhere is refused where it is opened. What the unit stores apart from itself comes first, then what the certifier's record does.
    parted(&Bytes::from(apart))
        .filter(|parts| parts.len() == 3)
        .and_then(|parts| apart_parts(&parts[0]))
        .ok_or_else(|| {
            format!("invalid archived /{root}: not what a certified unit stores apart")
        })?;
    Pack::read(Bytes::from(pack))
        .ok_or_else(|| format!("invalid archived /{root}: its blobs are no pack"))?;
    let record = curios_archive::access::<ArchivedRecord>(record)
        .map_err(|error| format!("invalid archived /{root} record: {error}"))?;
    let unit = curios_archive::access::<ArchivedUnit>(unit)
        .map_err(|error| format!("invalid archived /{root}: {error}"))?;

    Ok((record, unit, apart, pack))
}

fn archived() -> &'static [Validated; 2] {
    ARCHIVES.get_or_init(validate_archives)
}

/// Deserialize the validated images: each root's record beside its unit.
///
/// The universe invariants are *not* re-checked here, nor is anything certified again. Both are settled once by the build, on the values it is about to serialize, and these bytes are exactly the bytes written from those values — constants of the same build, whose structure `validate_bytes` confirms. Walking the whole standard library again per compilation would re-derive an answer already settled.
fn restore_archives() -> [(Record, Unit); 2] {
    curios_common::profile!("restore_archives");
    let mut units = archived()
        .iter()
        .zip(ROOTS)
        .map(|(archived, (root, _, _))| {
            let (record, unit, apart, pack) = match archived {
                Ok(archived) => *archived,
                Err(error) => panic!("{error}"),
            };
            let pack = Pack::read(Bytes::from(pack)).expect("a validated image's blobs are a pack");

            (
                curios_archive::deserialize::<Record>(record).unwrap_or_else(|error| {
                    panic!("validated archived /{root} record failed to restore: {error}")
                }),
                // What the unit stores apart from itself, and each blob it names, stays the slice of the executable it is: a compilation reads a lowered item where it names its declaration, and the baseline half where it lowers over this root.
                curios_archive::deserialize::<Unit>(unit)
                    .unwrap_or_else(|error| {
                        panic!("validated archived /{root} failed to restore: {error}")
                    })
                    .with_apart(Bytes::from(apart), &|digest| pack.get(digest))
                    .unwrap_or_else(|error| {
                        panic!("archived /{root} holds nothing stored apart: {error}")
                    }),
            )
        });

    [
        units.next().expect("the prelude has two roots"),
        units.next().expect("the prelude has two roots"),
    ]
}

/// The documentation record of each of the prelude's roots, in the fold's order: what `document --std` renders, and what a unit adopting a root's declarations reads them out of.
///
/// Apart from [`with_prelude`], because no unit carries a record: a compilation restores none, and a command that documents restores these and no unit for them. Restored on each call and kept by nothing here, since a process documents once.
pub fn documentation() -> Vec<Documentation> {
    curios_common::profile!("restore_documentation");
    curios_archive::from_bytes::<Vec<Documentation>>(DOCUMENTATION_BYTES)
        .unwrap_or_else(|error| panic!("the prelude's documentation failed to restore: {error}"))
}

/// The prefix each of the prelude's roots claims, in the fold's order — `/sys`, then `/std` — read off the words the build script names the roots by, so a caller asking which root a unit takes the place of restores none of them. A root claims the one prefix its word spells, which the crate's tests hold to the mounts of the unit restored for it and to the registry's roots.
pub fn root_prefixes() -> [Qualifier; 2] {
    ROOTS.map(|(root, _, _)| Qualifier::from([root]))
}

/// Borrow this thread's restored prelude, certified, as the ordered predecessors every compilation starts from — `/sys` then `/std`, each unit carrying the certifier's record of its definitions.
///
/// A slice rather than one unit, and in dependency order rather than any: what `Predecessors::over` takes is exactly this, so a product puts the whole prelude in scope by handing it along instead of deciding how its roots compose.
pub fn with_prelude<R>(use_prelude: impl FnOnce(&[&Unit]) -> R) -> R {
    curios_common::profile!("with_prelude");
    PRELUDE.with(|prelude| {
        let [sys, std] = &**prelude;
        use_prelude(&[sys, std])
    })
}

#[cfg(test)]
mod certifier_tests;
#[cfg(test)]
mod declaration_tests;
#[cfg(test)]
mod image_tests;
