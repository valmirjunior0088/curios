//! What judging definitions read, as a run of the store holds it: every name once, and each definition's reads holding the places of the names it read, a member of the run.
//!
//! **Each definition's reads an object.** What judging a definition read is stored as an object of its own, named by the digest of its run alone, and the reads a filing writes are the members of one run, so a name two judgments read is written and restored once.

#[cfg(test)]
mod tests;

use {
    crate::{Global, Reads},
    curios_utilities::Digest,
    std::collections::HashMap,
};

/// What judging a definition read, as a run holds it: the definition's name, and what it read.
pub type ReadEntry = (Global, Reads);

/// What judging definitions read, as a run of the store holds it: every name once, and each definition's reads holding places among them.
#[curios_archive::archived]
pub struct ReadTable {
    names: Vec<Global>,
    definitions: Vec<TabledReads>,
}

/// One definition's reads as a [`ReadTable`] holds them: the place of its name, and the places of the names whose signatures and bodies its judgment read.
#[curios_archive::archived]
struct TabledReads {
    name: u32,
    signatures: Vec<u32>,
    bodies: Vec<u32>,
}

impl ReadTable {
    /// The kind a pack's entry names for a member of a run of reads ([`curios_utilities::Member`]).
    pub const KIND: u8 = 3;

    /// The bytes of a run of `entries`, each a definition's name and what judging it read, in their order.
    pub fn run<'r>(
        entries: impl IntoIterator<Item = (&'r Global, &'r Reads)>,
    ) -> Result<Vec<u8>, String> {
        let mut names = Vec::new();
        let mut places = HashMap::new();
        let mut place = |name: &Global| {
            *places.entry(*name).or_insert_with(|| {
                names.push(*name);
                u32::try_from(names.len() - 1).expect("a table is counted in a u32")
            })
        };
        let definitions = entries
            .into_iter()
            .map(|(name, reads)| TabledReads {
                name: place(name),
                signatures: reads.signatures.iter().map(&mut place).collect(),
                bodies: reads.bodies.iter().map(&mut place).collect(),
            })
            .collect();

        curios_archive::to_bytes(&ReadTable { names, definitions }).map(|bytes| bytes.to_vec())
    }

    /// What the reads `reads` of the definition `name` are named by among the objects of a store: the digest of a run of them alone.
    pub fn identity(name: &Global, reads: &Reads) -> Result<Digest, String> {
        Self::run([(name, reads)]).map(|bytes| Digest::of(&bytes))
    }

    /// The entries of the run `bytes` holds ([`ReadTable::run`]), in their order. `Err` is bytes that hold no run of reads.
    pub fn read(bytes: &[u8]) -> Result<Vec<ReadEntry>, String> {
        let ReadTable { names, definitions } = curios_archive::from_bytes::<ReadTable>(bytes)?;
        let named = |place: u32| {
            usize::try_from(place)
                .ok()
                .and_then(|place| names.get(place))
                .copied()
                .ok_or_else(|| "a stored read names a place its table does not".to_string())
        };

        definitions
            .into_iter()
            .map(|definition| {
                Ok((
                    named(definition.name)?,
                    Reads {
                        signatures: definition
                            .signatures
                            .into_iter()
                            .map(named)
                            .collect::<Result<_, _>>()?,
                        bodies: definition
                            .bodies
                            .into_iter()
                            .map(named)
                            .collect::<Result<_, _>>()?,
                    },
                ))
            })
            .collect()
    }
}
