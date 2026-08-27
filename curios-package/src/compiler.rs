//! Which compiler produced a verdict.
//!
//! A cached verdict is keyed by two things: the terms it was reached about, and the compiler that reached it. This module is the second half.
//!
//! **Why the binary's contents, and not its version.** A version string does not move while a compiler is being developed, so keying on it would let a verdict outlive the kernel that reached it — the direction that *admits*, and the only one of the two that is a soundness question. GHC, Coq and Lean all tie their interface files to a version, which works because their users receive releases; it does not work for the machine the compiler is being written on.
//!
//! **Why not a per-build nonce.** It is trivially sound and it throws away every cache hit a reproducible rebuild should have kept, which for a compiler whose certification costs twelve seconds a unit is most of the point. A digest moves exactly when the binary does and no more.
//!
//! **Why not a fingerprint over chosen sources.** That is a list somebody maintains, and the failure of forgetting an entry is silent admission. There is nothing to forget here.
//!
//! **Why it is memoized.** Digesting a compiler binary costs more per compile than the certification it saves — 0.37s for a 570 MiB debug build, which a language server would pay on every keystroke — so the digest is recorded beside the store against a stamp of the binary, and recomputed only when the stamp moves. The common path is one `stat`.
//!
//! **What the stamp identifies, and why size and modification time are not enough.** Those two alone are what `sccache` uses, and they are forgeable by the ordinary means of moving a binary about: `cp -p`, `rsync -a`, `tar -x` and a restored artifact cache all preserve a modification time, so two builds that happen to agree on length would share a stamp and the second would be handed the first's digest — a verdict believed on behalf of a compiler that never reached it, which is the one direction that admits. The stamp therefore also carries the inode and the *status change* time, which the kernel writes on every modification and no copying tool can set. What is identified is the file this compiler is running from, not merely how large it is and when someone says it was written.

#[cfg(test)]
mod tests;

use {
    crate::Store,
    sha2::{Digest, Sha256},
    std::{
        fs::{self, File},
        io::Read,
        os::unix::fs::MetadataExt,
        path::Path,
        time::UNIX_EPOCH,
    },
};

/// The identity of the compiler running now, or `None` when it cannot identify itself.
///
/// `None` is an answer, and the answer is *do not cache*: a verdict recorded under an identity nobody can reproduce is a verdict that would later be believed on behalf of a different compiler. Failing closed here costs a re-certification; failing open costs the perimeter.
///
/// The identity is the *running* binary, which is exactly right for the compiler and coarse for an embedder: linked into somebody's application, the running binary is their application, so their rebuild invalidates verdicts this compiler would still stand behind. That is over-invalidation — the safe direction — and it is not fixable from here, because nothing can point at the compiler-shaped part of another program.
pub fn compiler(store: &Store) -> Option<String> {
    let executable = std::env::current_exe().ok()?;
    let metadata = fs::metadata(&executable).ok()?;
    let stamp = format!(
        "{} {} {} {} {}.{}",
        metadata.len(),
        metadata
            .modified()
            .ok()?
            .duration_since(UNIX_EPOCH)
            .ok()?
            .as_nanos(),
        metadata.dev(),
        metadata.ino(),
        metadata.ctime(),
        metadata.ctime_nsec(),
    );

    let record = store.compiler();
    if let Ok(recorded) = fs::read_to_string(&record)
        && let Some((seen, digest)) = recorded.rsplit_once(' ')
        && seen == stamp
    {
        return Some(digest.to_string());
    }

    let digest = digest(&executable)?;

    // Best-effort: a store that cannot be written costs the next compile another digest, and nothing else. It must not cost the verdict.
    if let Some(parent) = record.parent() {
        let _ = fs::create_dir_all(parent);
    }
    let _ = fs::write(&record, format!("{stamp} {digest}"));

    Some(digest)
}

/// SHA-256 over a file, read in chunks rather than into memory — a compiler binary is large enough that the difference matters and small enough that reading it once per compiler does not.
fn digest(path: &Path) -> Option<String> {
    let mut file = File::open(path).ok()?;
    let mut hasher = Sha256::new();
    let mut buffer = [0_u8; 64 * 1024];

    loop {
        match file.read(&mut buffer).ok()? {
            0 => break,
            read => hasher.update(&buffer[..read]),
        }
    }

    Some(
        hasher
            .finalize()
            .iter()
            .map(|byte| format!("{byte:02x}"))
            .collect(),
    )
}
