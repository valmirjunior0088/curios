//! What judging definitions read, as a run of the store holds it: every name once, and each definition's reads holding the places of the names it read, a member of the run.
//!
//! **Each definition's reads an object.** What judging a definition read is stored as an object of its own, named by its run alone or, completed, by what it judged ([`crate::ReadRecord::digest`]), and the reads a filing writes are the members of one run, so a name two judgments read is written and restored once.

#[cfg(test)]
mod tests;

use {
    crate::{Concluded, Global, Polarity, Reads, Totality, Variance},
    curios_utilities::Digest,
    std::collections::HashMap,
};

/// What judging a definition read, as a run holds it: the definition's name, what it read, and what a filing completed it with.
pub type ReadEntry = (Global, Reads, Option<Concluded>);

/// What judging a definition read, as a run is written from: the definition's name, what it read, and what a filing completed it with, each borrowed.
pub type ReadsEntry<'r> = (&'r Global, &'r Reads, Option<&'r Concluded>);

/// What judging definitions read, as a run of the store holds it: every name once, and each definition's reads holding places among them.
#[curios_archive::archived]
pub struct ReadTable {
    names: Vec<Global>,
    definitions: Vec<TabledReads>,
}

/// One definition's reads as a [`ReadTable`] holds them: the place of its name, the places of the names whose signatures and bodies its judgment read, and what a filing completed them with.
#[curios_archive::archived]
struct TabledReads {
    name: u32,
    signatures: Vec<u32>,
    bodies: Vec<u32>,
    made: Option<TabledJudged>,
}

/// What a filing completed a definition's reads with ([`Concluded`]), as a [`ReadTable`] holds it.
#[curios_archive::archived]
struct TabledJudged {
    input: [u8; 32],
    signatures: Vec<Option<Option<[u8; 32]>>>,
    bodies: Vec<Option<Option<[u8; 32]>>>,
    totality: Totality,
    vectors: Option<(Vec<Polarity>, Vec<Variance>)>,
}

impl TabledReads {
    /// The entry this holds, each place read through `named`.
    fn entry(self, named: impl Fn(u32) -> Result<Global, String>) -> Result<ReadEntry, String> {
        let reads = Reads {
            signatures: self
                .signatures
                .into_iter()
                .map(&named)
                .collect::<Result<_, _>>()?,
            bodies: self
                .bodies
                .into_iter()
                .map(&named)
                .collect::<Result<_, _>>()?,
        };
        let made = self.made.map(|made| Concluded {
            input: Digest::from_bytes(made.input),
            signatures: made.signatures,
            bodies: made.bodies,
            totality: made.totality,
            vectors: made.vectors,
        });

        Ok((named(self.name)?, reads, made))
    }
}

impl ReadTable {
    /// The kind a pack's entry names for a member of a run of reads ([`curios_utilities::Member`]).
    pub const KIND: u8 = 3;

    /// The bytes of a run of `entries`, each a definition's name, what judging it read and what a filing completed that with, in their order.
    pub fn run<'r>(entries: impl IntoIterator<Item = ReadsEntry<'r>>) -> Result<Vec<u8>, String> {
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
            .map(|(name, reads, made)| TabledReads {
                name: place(name),
                signatures: reads.signatures.iter().map(&mut place).collect(),
                bodies: reads.bodies.iter().map(&mut place).collect(),
                made: made.map(|made| TabledJudged {
                    input: *made.input.as_bytes(),
                    signatures: made.signatures.clone(),
                    bodies: made.bodies.clone(),
                    totality: made.totality,
                    vectors: made.vectors.clone(),
                }),
            })
            .collect();

        curios_archive::to_bytes(&ReadTable { names, definitions }).map(|bytes| bytes.to_vec())
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
            .map(|definition| definition.entry(named))
            .collect()
    }

    /// The entry the run `bytes` holds as its member `member`, the run checked once and nothing of it restored but the member and the names it holds: what a walk reads of a record a store found among those of a run.
    pub fn member(bytes: &[u8], member: u32) -> Result<ReadEntry, String> {
        let table = curios_archive::access::<ArchivedReadTable>(bytes)?;
        let tabled = usize::try_from(member)
            .ok()
            .and_then(|member| table.definitions.get(member))
            .ok_or_else(|| format!("a run of reads holds no member {member}"))?;

        curios_archive::deserialize::<TabledReads>(tabled)?.entry(|place| {
            let name = usize::try_from(place)
                .ok()
                .and_then(|place| table.names.get(place))
                .ok_or("a stored read names a place its table does not")?;

            curios_archive::deserialize::<Global>(name)
        })
    }
}
