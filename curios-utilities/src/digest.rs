//! SHA-256 as the workspace's one content digest: the hex digest of some bytes, the [`Digest`] a store names an [`Object`] by, and a [`Hasher`] that finishes into one.
//!
//! This crate is the authority for `sha2`, on the rule that one crate names one external concern. Every digest a record is checked against, every store key and every tree hash is spelled through here, and every part of a key is framed the one way [`Fingerprint::feed`] states — so nothing above this crate names the algorithm, and no two keys can disagree about how their parts are separated.

#[cfg(test)]
mod tests;

use {
    curios_archive::Bytes,
    sha2::{Digest as _, Sha256},
    std::{fmt, hash::Hasher},
};

/// The digest of `bytes`, for a caller checking one thing against a record of it.
///
/// Deliberately not `curios-package`'s `TreeHash`: that answers what a delivered tree *is*, and carries a scheme prefix because it is written into manifests and compared across machines. This answers whether some bytes are the bytes something was made from, is never published, and would be a lie in the other's spelling.
pub fn digest(bytes: &[u8]) -> String {
    let mut fingerprint = Fingerprint::new();
    fingerprint.write(bytes);

    fingerprint.hex()
}

/// The digest of some bytes as its thirty-two bytes, for a caller that keeps many and compares them: what [`digest`] spells in hex, and what a store names an object by.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Digest([u8; 32]);

impl Digest {
    /// The digest of `bytes`.
    pub fn of(bytes: &[u8]) -> Self {
        Self(Sha256::digest(bytes).into())
    }

    /// The digest whose thirty-two bytes are `bytes`, as a table that keeps digests spells them.
    pub fn from_bytes(bytes: [u8; 32]) -> Self {
        Self(bytes)
    }

    /// The thirty-two bytes of this digest.
    pub fn as_bytes(&self) -> &[u8; 32] {
        &self.0
    }

    /// This digest in the hex [`digest`] spells it in.
    pub fn hex(&self) -> String {
        self.0.iter().map(|byte| format!("{byte:02x}")).collect()
    }
}

impl fmt::Debug for Digest {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "{}", &self.hex()[..12])
    }
}

/// A value as a store keeps it, beside the digest it is found by: the bytes it was stored as, or the bytes of several values stored together with which of them it is. A value restored from a store keeps the object it was read from, so storing it again digests nothing.
#[derive(Debug, Clone)]
pub struct Object {
    pub digest: Digest,
    pub bytes: Bytes,
    /// Which of the values `bytes` holds together this one is: `None` where they hold it alone.
    pub member: Option<u32>,
}

impl Object {
    /// `bytes` as an object holding one value alone, digested now.
    pub fn of(bytes: impl Into<Bytes>) -> Self {
        let bytes = bytes.into();

        Self {
            digest: Digest::of(bytes.as_slice()),
            bytes,
            member: None,
        }
    }
}

/// A [`Hasher`] that finishes into a digest, for a key part whose producer speaks `std::hash` and whose consumer speaks SHA-256.
///
/// One part of the payload address is not a string anybody here can build: the engine compatibility stamp, which only `curios-runtime` can describe and which it hands over by *writing into* a hasher rather than by returning a digest — keeping `sha2` out of that crate and `wasmtime` out of the one that assembles the address. This is the adapter between the two vocabularies, and it lives beside [`digest`] because what it produces is one.
///
/// [`Hasher::finish`] is not how a value is taken out of this. It has to exist, and a `u64` is not what a store key is spelled in, so it answers with the leading eight bytes of the digest so far and [`Fingerprint::hex`] is what a caller uses. Nothing in `std::hash` calls `finish` on the caller's behalf — `Hash::hash` only ever writes — so the narrow answer is never the one that reaches a key.
pub struct Fingerprint(Sha256);

impl Fingerprint {
    /// A fingerprint with nothing folded into it yet.
    pub fn new() -> Self {
        Self(Sha256::new())
    }

    /// One length-framed part of a key: the part's byte length as a little-endian `u64`, then its bytes.
    ///
    /// The frame is what keeps two sequences of parts from sharing a spelling — `ab` then `c` and `a` then `bc` feed identical bytes without it, and two different keys would collide. Every store key and the tree hash frame their parts this way, and this is the one place the frame is spelled.
    pub fn feed(&mut self, part: impl AsRef<[u8]>) {
        let part = part.as_ref();

        self.0.update((part.len() as u64).to_le_bytes());
        self.0.update(part);
    }

    /// Everything written so far, as the hex digest a key part is spelled in.
    pub fn hex(self) -> String {
        self.0
            .finalize()
            .iter()
            .map(|byte| format!("{byte:02x}"))
            .collect()
    }
}

impl Default for Fingerprint {
    fn default() -> Self {
        Self::new()
    }
}

impl Hasher for Fingerprint {
    fn write(&mut self, bytes: &[u8]) {
        self.0.update(bytes);
    }

    fn finish(&self) -> u64 {
        let digest = self.0.clone().finalize();

        u64::from_le_bytes(digest[..8].try_into().expect("a digest is 32 bytes wide"))
    }
}
