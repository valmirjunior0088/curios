//! What a compile made of a declaration, kept for a later compile of the same input: a candidate, a record of the store named by the facet it is, the input it was made from and what it holds ([`candidate_name`]), so a store finds every candidate filed for one input together ([`Candidates`]).
//!
//! **Many for one input.** A declaration made again from an input it was once made from is made from whatever it read then, which may stand or not: so a candidate holds what its making read, each part by the digest it had, and a store keeps every candidate filed for an input until no slot has named its unit for the time a collection keeps an object, so an edit undone, a branch checked out again and a bisection find what was made of the text they bring back.

use {
    crate::{Digest, Object},
    std::fmt,
};

/// The facet a candidate is, what a stage made of a declaration: what keeps the candidates two stages made of one input apart.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Facet {
    Elaboration,
    Erasure,
}

impl Facet {
    /// The byte a candidate's name is made with.
    fn tag(self) -> u8 {
        match self {
            Facet::Elaboration => 1,
            Facet::Erasure => 2,
        }
    }
}

impl fmt::Debug for Facet {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(match self {
            Facet::Elaboration => "elaboration",
            Facet::Erasure => "erasure",
        })
    }
}

/// How many bytes of a candidate's name its facet and its input decide.
pub const CANDIDATE_PREFIX: usize = 16;

/// The bytes every name of a candidate of `facet` made from `input` opens with.
pub fn candidate_prefix(facet: Facet, input: &Digest) -> [u8; CANDIDATE_PREFIX] {
    let mut keyed = vec![facet.tag()];
    keyed.extend_from_slice(input.as_bytes());

    Digest::of(&keyed).as_bytes()[..CANDIDATE_PREFIX]
        .try_into()
        .expect("a digest is longer than a candidate's prefix")
}

/// What a store names a candidate of `facet` made from `input` by, `bytes` being what it is stored as alone: the facet and the input, which it shares with every candidate made of them, and then the digest of what it holds, which tells it from them.
pub fn candidate_name(facet: Facet, input: &Digest, bytes: &[u8]) -> Digest {
    let mut name = [0; 32];
    name[..CANDIDATE_PREFIX].copy_from_slice(&candidate_prefix(facet, input));
    name[CANDIDATE_PREFIX..]
        .copy_from_slice(&Digest::of(bytes).as_bytes()[..32 - CANDIDATE_PREFIX]);

    Digest::from_bytes(name)
}

/// What a store holds for a compile to look among: the candidates filed for an input, and any object by its name.
pub trait Candidates {
    /// Every candidate of `facet` made from `input` that the store holds, in no stated order.
    fn candidates(&self, facet: Facet, input: &Digest) -> Vec<Object>;

    /// The object named `digest`, where the store holds it.
    fn object(&self, digest: &Digest) -> Option<Object>;
}
