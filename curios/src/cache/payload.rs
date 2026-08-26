//! What an invocation consults before compiling a program it has already compiled.
//!
//! **The same address-and-record split the unit family states for itself, one level up.** A slot names a place — this executable, by this compiler, after this chain, for this engine — and carries no file contents, so a source edit changes what the slot is verified against rather than where it is looked for. What the payload was made from rides in a [`Record`] beside it: the entry file, every file the entry's loader read, what each predecessor contained, and the payload's own digest.
//!
//! **Taking a payload from here is believing a whole compilation, judgment included.** The entry's kernel recheck is skipped on the strength of the record exactly as a reused unit's is, and the argument for that is [Reused payloads](../../../documentation/soundness/admission-without-judgment/reused-payloads.md), which extends [Cached verdicts](../../../documentation/soundness/admission-without-judgment/cached-verdicts.md) rather than restating it.
//!
//! **Two things the unit family has no counterpart for.** The entry's own header is never read through a loader — the caller parsed it before any resolution happened — so it is recorded separately, and from the text that was parsed rather than from a re-read of the path: re-reading races an edit landing between the parse and the digest, which records newer text against an older artifact, the one direction that admits stale. And the payload is machine code, so the engine that will run it joins the address; see `curios_runtime::engine_compatibility`.

#[cfg(test)]
mod tests;

use {
    super::{Placed, RECORD, Verdicts, chained, digested, read_within, replace},
    curios_package::{Fingerprint, digest, payload_slot},
    curios_text::{RootSource, UnitSource},
    std::{fs, io, path::Path, sync::LazyLock},
};

/// The file a stored payload is written as, inside its slot.
///
/// Named for what it is rather than for the stage that produced it: what sits here is exactly the byte string a bundled executable carries and `curios_runtime::run_bytes` deserializes.
const STORED: &str = "payload.cwasm";

/// What a payload is filed for: the executable it builds, and the entry it builds from.
///
/// One bundle rather than five parameters, because the five are one subject. The two names decide the *address* and the three entry-side facts decide the *record*, which is the same split the whole scheme rests on. The engine is not among them: it is a fact about this machine rather than about the program, so `engine` answers for it once and no caller can get it wrong.
pub struct Program<'a> {
    /// The package declaring the executable. Its name and the executable's are together the one identity in a compilation that cannot collide, which is why the address carries them instead of a path.
    pub package: &'a str,
    /// The executable, by the name its manifest row gives it.
    pub executable: &'a str,
    /// The `.crs` file the program is compiled from.
    pub entry: &'a Path,
    /// The text that was parsed from it — the parsed text, never a re-read.
    pub text: &'a str,
    /// The loader the entry's own `mod` declarations resolve through, and afterwards the log of what they read.
    pub loader: &'a RootSource,
}

/// What a stored payload must still be true of to be believed.
///
/// Every field is a fact the compilation depended on and the address deliberately does not carry. Verification is all of them or nothing: a record that cannot be read, or that disagrees anywhere, is a miss.
// `always`: a product that reads and writes archives unconditionally has no `archive` feature for a `cfg_attr` to gate on.
#[curios_archive::archived(always)]
struct Record {
    /// The entry file, by canonical path and the digest of the text that was parsed from it.
    ///
    /// Apart from `reads` because no loader ever reads it, and the *path* is checked for equality rather than containment: the asking invocation knows exactly which file it opened, so nothing weaker is called for. That check is this family's counterpart of the unit record's containment clause — without it, two projects each declaring a package and an executable of one name would address one slot and be handed each other's program.
    entry: (String, String),
    /// Each file the entry's loader read, by canonical path, with the digest of the text that was parsed from it.
    reads: Vec<(String, String)>,
    /// What each predecessor contained, in fold order — the digest of the bytes its own unit slot holds.
    predecessors: Vec<String>,
    /// The payload's own digest, so a truncated or corrupted artifact is a miss here rather than a failure to deserialize at run time.
    payload: String,
}

impl Verdicts {
    /// The payload filed for `program` after `units`, when everything it was made from still agrees.
    ///
    /// **Decidable without deserializing anything.** The units are verified through `Verdicts::chain` — the same slot and record checks the fold makes, minus the decode a payload hit has no use for — and the payload itself is bytes to hand on, not a structure to read. A stale unit is a miss by construction, since it is about to recompile into bytes no record could match.
    ///
    /// Consulting this does not enter the fold's chain: a hit means no fold runs at all, and a miss leaves the fold to place its own units as it always did.
    pub fn payload_get(&self, program: &Program<'_>, units: &[UnitSource<'_>]) -> Option<Vec<u8>> {
        let placed = self.chain(units)?;
        let directory = self.store.payload(&slot(self, program, &placed)?);

        // Read before either is judged, and both before anything is believed: a slot missing one of the two files is a half-written store to ignore.
        let recorded = fs::read(directory.join(RECORD)).ok()?;
        let bytes = fs::read(directory.join(STORED)).ok()?;

        // A stored record that will not read back is a store to ignore, never a compile to fail.
        let record = curios_archive::from_bytes::<Record>(&recorded).ok()?;

        match agrees(program, &record, &placed, &bytes) {
            true => Some(bytes),
            false => None,
        }
    }

    /// File `bytes` as `program`'s payload, against the chain the fold just placed over `units`.
    ///
    /// Called after the fold, which is what makes `Verdicts::placed` the right chain to record: it holds what every unit of *this* compilation was filed as, whether it was reused or compiled. Best effort, exactly as a unit's write is — a store that cannot be written costs the next invocation the work it would have saved and nothing else, and the refusal is kept for a caller to report rather than raised here.
    ///
    /// **`units` is taken so this can refuse a chain with a gap in it, and it is the same slice [`Verdicts::payload_get`] probes with.** The two halves derived their chain by different rules until they were made to take one input: the probe builds it with `Verdicts::chain`, which refuses any unit `Verdicts::slot` declines, while this read whatever the fold happened to place — and a unit the fold could not place is simply absent from that. Filing under the shorter chain writes a slot addressed by a prefix no probe will ever compute: not a stale answer, but a directory the store grows and nothing reads, forever. Withholding the record is free, since the compilation it came from is correct either way.
    pub fn payload_put(&self, program: &Program<'_>, units: &[UnitSource<'_>], bytes: &[u8]) {
        let placed = self.placed.borrow();

        if placed.len() != units.len() {
            return;
        }

        let Some(slot) = slot(self, program, &placed) else {
            return;
        };

        let record = Record {
            entry: (canonical(program.entry), digest(program.text.as_bytes())),
            reads: digested(program.loader.reads()),
            predecessors: placed
                .iter()
                .map(|placed| placed.contained.clone())
                .collect(),
            payload: digest(bytes),
        };

        let filed = curios_archive::to_bytes(&record)
            .map_err(io::Error::other)
            .and_then(|record| replace(&self.store.payload(&slot), STORED, bytes, &record));

        if let Err(error) = filed {
            self.refused.borrow_mut().get_or_insert(error.to_string());
        }
    }
}

/// The slot `program` is filed in after `placed`, or `None` when it may not be filed at all — which is when the compiler cannot identify itself, and for the reason [`Verdicts::compiler`] gives.
fn slot(verdicts: &Verdicts, program: &Program<'_>, placed: &[Placed]) -> Option<String> {
    let compiler = verdicts.compiler.as_ref()?;
    let predecessors = placed
        .iter()
        .map(|placed| placed.slot.clone())
        .collect::<Vec<_>>();

    Some(payload_slot(
        compiler,
        &predecessors,
        program.package,
        program.executable,
        engine(),
    ))
}

/// What decides whether a payload compiled on this machine runs on another, as a key part.
///
/// **The payload is the store's first machine-dependent artifact.** A unit is a judgment and travels wherever the compiler binary does; this is machine code Cranelift emitted for the host's ISA — the one input neither the compiler digest nor any recorded source file covers. `curios-runtime` describes it, this turns the description into a digest, and `curios-package` files under it, so the crate that names wasmtime and the crate that names `sha2` stay apart.
///
/// Computed once: it is a property of the process, and the engine it asks is itself built once.
fn engine() -> &'static str {
    static ENGINE: LazyLock<String> = LazyLock::new(|| {
        let mut fingerprint = Fingerprint::new();
        curios_runtime::engine_compatibility(&mut fingerprint);

        fingerprint.hex()
    });

    &ENGINE
}

/// Whether `record` still describes the world: the payload is intact, the chain is what it was, the entry is the same file holding the same text, and every file its loader read still holds the text it was read as.
///
/// All of it or nothing, and in cheapest-first order — the two digests decide most misses before any file is opened.
fn agrees(program: &Program<'_>, record: &Record, placed: &[Placed], bytes: &[u8]) -> bool {
    record.payload == digest(bytes)
        && chained(&record.predecessors, placed)
        && record.entry == (canonical(program.entry), digest(program.text.as_bytes()))
        && read_within(&program.loader.directories(), &record.reads)
}

/// A path as a record spells it: canonical, so the same file reached through a relative invocation and an absolute one is one entry.
///
/// A path that will not canonicalize is spelled as given rather than dropped, which mirrors what the loader records. The consequence is the unit family's: moving a project invalidates its records, costing one recompile before they are rewritten.
fn canonical(path: &Path) -> String {
    path.canonicalize()
        .unwrap_or_else(|_| path.to_path_buf())
        .to_string_lossy()
        .into_owned()
}
