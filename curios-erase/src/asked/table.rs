//! What erasing items read, as a run of the store holds it: every name once, and each item's record holding the places of the names it read, a member of the run.
//!
//! **Each item's record an object.** What erasing an item read is stored as an object of its own, named by its run alone or, completed, by the item it erased ([`AskedRecord::digest`](crate::AskedRecord::digest)), and the records a filing writes are the members of one run, so a name two erasures read is written and restored once.

use {
    super::{Asked, Made, Part, Read},
    curios_utilities::Digest,
    std::collections::HashMap,
};

/// An item's record as a run holds it: the name the item declares first, what erasing it read, and what a filing completed it with.
pub type AskedEntry = (curios_core::Global, Asked, Option<Made>);

/// What erasing items read, as a run of the store holds it: every name once, and each item's record holding places among them.
#[curios_archive::archived]
pub struct AskedTable {
    names: Vec<curios_core::Global>,
    items: Vec<TabledAsked>,
}

/// One item's record as an [`AskedTable`] holds it: the place of the name it declares first, the place of each name it read a part of with the part, and what a filing completed it with.
#[curios_archive::archived]
struct TabledAsked {
    name: u32,
    read: Vec<(u32, Part)>,
    made: Option<TabledMade>,
}

/// What a filing completed a record with ([`Made`]), as an [`AskedTable`] holds it.
#[curios_archive::archived]
struct TabledMade {
    input: [u8; 32],
    reads: Vec<Read>,
    forms: [u8; 32],
}

impl TabledAsked {
    /// The entry this holds, each place read through `named`.
    fn entry(
        self,
        named: impl Fn(u32) -> Result<curios_core::Global, String>,
    ) -> Result<AskedEntry, String> {
        let mut asked = Asked::default();
        for (place, part) in self.read {
            asked.note(named(place)?, part);
        }
        let made = self.made.map(|made| Made {
            input: Digest::from_bytes(made.input),
            reads: made.reads,
            forms: Digest::from_bytes(made.forms),
        });

        Ok((named(self.name)?, asked, made))
    }
}

impl AskedTable {
    /// The kind a pack's entry names for a member of a run of erasures' records ([`curios_utilities::Member`]).
    pub const KIND: u8 = 4;

    /// The bytes of a run of `entries`, each the name an item declares first, what erasing it read and what a filing completed that with, in their order.
    pub fn run<'a>(
        entries: impl IntoIterator<Item = (&'a curios_core::Global, &'a Asked, Option<&'a Made>)>,
    ) -> Result<Vec<u8>, String> {
        let mut names = Vec::new();
        let mut places = HashMap::new();
        let mut place = |name: &curios_core::Global| {
            *places.entry(*name).or_insert_with(|| {
                names.push(*name);
                u32::try_from(names.len() - 1).expect("a table is counted in a u32")
            })
        };
        let items = entries
            .into_iter()
            .map(|(name, asked, made)| TabledAsked {
                name: place(name),
                read: asked
                    .reads()
                    .map(|(name, part)| (place(&name), part))
                    .collect(),
                made: made.map(|made| TabledMade {
                    input: *made.input.as_bytes(),
                    reads: made.reads.clone(),
                    forms: *made.forms.as_bytes(),
                }),
            })
            .collect();

        curios_archive::to_bytes(&AskedTable { names, items }).map(|bytes| bytes.to_vec())
    }

    /// The entries of the run `bytes` holds ([`AskedTable::run`]), in their order. `Err` is bytes that hold no run of erasures' records.
    pub fn read(bytes: &[u8]) -> Result<Vec<AskedEntry>, String> {
        let AskedTable { names, items } = curios_archive::from_bytes::<AskedTable>(bytes)?;
        let named = |place: u32| {
            usize::try_from(place)
                .ok()
                .and_then(|place| names.get(place))
                .copied()
                .ok_or_else(|| "a stored erasure names a place its table does not".to_string())
        };

        items.into_iter().map(|item| item.entry(named)).collect()
    }

    /// The entry the run `bytes` holds as its member `member`, the run checked once and nothing of it restored but the member and the names it holds: what an erasure reads of a record a store found among those of a run.
    pub fn member(bytes: &[u8], member: u32) -> Result<AskedEntry, String> {
        let table = curios_archive::access::<ArchivedAskedTable>(bytes)?;
        let tabled = usize::try_from(member)
            .ok()
            .and_then(|member| table.items.get(member))
            .ok_or_else(|| format!("a run of erasures holds no member {member}"))?;

        curios_archive::deserialize::<TabledAsked>(tabled)?.entry(|place| {
            let name = usize::try_from(place)
                .ok()
                .and_then(|place| table.names.get(place))
                .ok_or("a stored erasure names a place its table does not")?;

            curios_archive::deserialize::<curios_core::Global>(name)
        })
    }
}
