//! What erasing each item of a unit read of the declarations around it, kept with the unit: the record a later erasure of the same unit keeps an item's erased forms by.
//!
//! **Erasure's own record.** An item's erased forms are a function of the item and of what erasing it read: of a declaration around it, its signature and its body as the kernel that certified the unit read them, and its kind where erasure reads the definition off its item; of another item's erased forms, how a name is bound in them, or the forms whole where an evaluation ran them ([`Part`]). Each part is noted once as the item is erased ([`Asked`]), and the unit keeps each item's record as its baseline half, with the digest of each part read of a declaration it does not hold ([`AskedItems`]).
//!
//! **A part is digested by whoever reads it.** A signature and a body by the certifier's own digest ([`curios_cert::part_digest`]), so what a recompile compares of a declaration is what the kernel compares of it; a kind, a binding and forms whole by erasure's ([`declared_digest`], [`erased_digest`]).
//!
//! **Completed where it is filed, and found by the item it erased.** A store's filing completes each record with the item's identity, the digest of each part read and the digest of the item's forms ([`Made`]), and names it by those, as the certifier's records are named ([`curios_utilities::candidate_name`]), so a later erasure of an item of that identity takes its forms out of the store where every part read digests now as it did. A constructive trace, which "additionally stores the resulting value" (Mokhov, Mitchell and Peyton Jones, [Build Systems à la Carte](https://www.microsoft.com/en-us/research/wp-content/uploads/2018/03/build-systems.pdf), section 4.2.3), the value held by its identity in the store beside it.

mod table;
pub use table::{AskedEntry, AskedTable};

mod by;
pub use by::AskedBy;

use {
    crate::{ErasedUnit, declared},
    curios_utilities::{
        Digest, Facet, InKeyOrder, Object, Records, Sorted, candidate_name, digest,
    },
    std::{
        cell::OnceCell,
        collections::{BTreeSet, HashMap},
        ops::Deref,
        rc::Rc,
    },
};

/// A part of a declaration that erasing an item read.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived]
pub enum Part {
    /// Its signature as the kernel reads one: its type, its universe context, the registry entry its former carries, its totality and the entry's vectors.
    Signature,
    /// What it unfolds to, as the kernel reads it.
    Body,
    /// What it is declared as and what it was generated from, read off its item.
    Kind,
    /// How it is bound in the erased forms of the item declaring it, with the row it declares there.
    Binding,
    /// The erased forms of the item declaring it, whole, as an evaluation runs them.
    Forms,
}

impl Part {
    /// Every part, in the order a record holds them.
    pub const ALL: [Part; 5] = [
        Part::Signature,
        Part::Body,
        Part::Kind,
        Part::Binding,
        Part::Forms,
    ];
}

/// A part of the declaration of a name: what a record notes a read as.
pub type NamedPart = (curios_core::Global, Part);

/// What erasing an item's forms read: each part of each declaration around it, once, in the order of the names and then of the parts, which is the order a completed record digests them in.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Asked {
    read: BTreeSet<(curios_core::Global, Part)>,
}

impl Asked {
    /// Note `part` of `name` as read.
    pub(super) fn note(&mut self, name: curios_core::Global, part: Part) {
        self.read.insert((name, part));
    }

    /// Each part read, in order.
    pub fn reads(&self) -> impl Iterator<Item = (curios_core::Global, Part)> + '_ {
        self.read.iter().copied()
    }

    /// How many parts were read.
    pub fn len(&self) -> usize {
        self.read.len()
    }

    /// Whether nothing was read.
    pub fn is_empty(&self) -> bool {
        self.read.is_empty()
    }
}

/// What a completed record holds of a read: `None` where nothing is recorded of it, a part of a fixed unit's, which a store's address names; and otherwise the digest of the part, `None` within where the name holds no such part.
pub type Read = Option<Option<[u8; 32]>>;

/// What a filing completes an item's record with: what the item was, what each of its reads read and what it erased to. See the module documentation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Made {
    /// The identity of the item erased ([`curios_core::ItemRun::identity`]).
    pub input: Digest,
    /// What each read read, in the record's order.
    pub reads: Vec<Read>,
    /// The digest of the item's erased forms, as they are stored.
    pub forms: Digest,
}

/// An item's record as a unit holds it: what erasing it read, and once a filing completed it, what it was erased from and to (`Made`). Shared with every version that keeps it, beside the digest it is stored under.
#[derive(Debug)]
pub struct AskedRecord {
    asked: Asked,
    /// What a filing completed it with: unset until one did, `None` where nothing completes it.
    made: OnceCell<Option<Made>>,
    /// The digest it is stored under, kept once it is complete: what a record not yet completed is stored under moves where a filing completes it.
    digest: OnceCell<Digest>,
}

impl AskedRecord {
    /// The record of an erasure that read `asked`, which no filing has completed.
    pub fn new(asked: Asked) -> Self {
        Self {
            asked,
            made: OnceCell::new(),
            digest: OnceCell::new(),
        }
    }

    /// The record a store held under `digest`: complete where it was filed with `made`, and otherwise left for a filing to complete.
    pub fn restored(asked: Asked, made: Option<Made>, digest: Digest) -> Self {
        let record = Self::new(asked);
        if let Some(made) = made {
            let _ = record.made.set(Some(made));
            let _ = record.digest.set(digest);
        }

        record
    }

    /// What the item was erased from and to, and what each read read, where a filing completed the record with it.
    pub fn made(&self) -> Option<&Made> {
        self.made.get()?.as_ref()
    }

    /// Whether a filing has completed the record, with what the item made or with nothing.
    pub fn completed(&self) -> bool {
        self.made.get().is_some()
    }

    /// Complete the record with `made`, where nothing has: the record of every version that holds it. `None` is a record nothing completes.
    pub fn complete(&self, made: Option<Made>) {
        let _ = self.made.set(made);
    }

    /// The digest a store names this record of the item declaring `name` first by: where it is complete, the candidate's name, which a store finds it among the records filed for the item by ([`candidate_name`]), and otherwise the digest of its run alone.
    pub fn digest(&self, name: &curios_core::Global) -> Result<Digest, String> {
        if let Some(digest) = self.digest.get() {
            return Ok(*digest);
        }
        let run = AskedTable::run([(name, &self.asked, self.made())])?;
        let digest = match self.made() {
            Some(made) => candidate_name(Facet::Erasure, &made.input, &run),
            None => Digest::of(&run),
        };
        if self.completed() {
            let _ = self.digest.set(digest);
        }

        Ok(digest)
    }
}

/// A record reads as what the erasure read.
impl Deref for AskedRecord {
    type Target = Asked;

    fn deref(&self) -> &Asked {
        &self.asked
    }
}

/// An item's record as a unit holds it: shared with every version that keeps it.
pub type HeldAsked = Rc<AskedRecord>;

/// What erasing each of a unit's items read, by the first name the item declares, and what the items read of declarations the unit does not hold: the unit's baseline half. See the module documentation.
///
/// **A version of the baseline's.** A unit erased over a baseline holds its baseline's records with the record of each item it erased again filed in place of the one it replaces and each item it dropped taken out: the table is persistent and read back in the order of its names, so a record kept is shared, never copied.
#[derive(Debug, Clone, Default)]
#[curios_archive::archived]
pub struct AskedItems {
    /// **Stored apart, an object for each**, and handed back where the records are restored ([`Records`]): what is archived with the unit is the rest.
    #[archived_with(curios_archive::Skip)]
    items: Sorted<curios_core::Global, HeldAsked>,
    /// What the items read of declarations the unit does not hold, another unit's or no one's: each part with the digest it had, `None` where the name held no such part.
    ///
    /// **What a read of another unit stands on.** A record holds no answer for a part, the declaration being the one to hold it; of a declaration of the unit that is the baseline's own, and of one of another unit it is this digest. A unit erased over a baseline asks nothing of a unit that is the one the baseline was erased after, and of any other asks each part again by what it digests to now. One table for a unit, every item of one erasure having read one scope: a part is digested once however many items read it.
    #[archived_with(InKeyOrder)]
    abroad: Sorted<(curios_core::Global, Part), Option<String>>,
}

impl AskedItems {
    /// File `record` as the item declaring `name` first's: another unit's record of it, held where that unit holds it, or one a store held.
    pub(super) fn share(&mut self, name: curios_core::Global, record: HeldAsked) {
        self.items.insert(name, record);
    }

    /// Take out the record held under `name`, where one is: an item no longer declaring it first.
    pub(super) fn forget(&mut self, name: &curios_core::Global) {
        self.items.remove(name);
    }

    /// The record of the item declaring `name` first, as the unit holds it.
    pub(super) fn held(&self, name: &curios_core::Global) -> Option<&HeldAsked> {
        self.items.get(name)
    }

    /// Each item's record as the unit holds it, in the order of the names the items declare first: what the unit stores an object for.
    pub fn records(&self) -> impl Iterator<Item = (&curios_core::Global, &HeldAsked)> {
        self.items.iter()
    }

    /// The digest recorded of `part` of `name`, a declaration the unit does not hold: `None` where nothing read it, `Some(None)` where the name held no such part.
    pub(super) fn abroad(&self, name: &curios_core::Global, part: Part) -> Option<&Option<String>> {
        self.abroad.get(&(*name, part))
    }

    /// Record `digest` as what `part` of `name` had, in place of what was recorded of it.
    pub(super) fn record_abroad(
        &mut self,
        (name, part): (curios_core::Global, Part),
        digest: Option<String>,
    ) {
        self.abroad.insert((name, part), digest);
    }

    /// Hold what `other` recorded of declarations its unit did not hold, in place of what this recorded: where a revision of the baseline's table starts.
    pub(super) fn read_abroad_of(&mut self, other: &AskedItems) {
        self.abroad = other.abroad.clone();
    }

    /// Take out what was recorded of `part` of `name`: no item reads it now.
    pub(super) fn forget_abroad(&mut self, (name, part): (curios_core::Global, Part)) {
        self.abroad.remove(&(name, part));
    }
}

/// A unit's records restored out of what it stores of them: the rest, archived with the unit, and each item's record a member of a run ([`AskedTable`]), restored beside the digest it was found by. A run several records lie in is read once.
impl Records for AskedItems {
    fn restored(table: &[u8], entries: &[Object]) -> Result<Self, String> {
        let mut items = curios_archive::from_bytes::<AskedItems>(table)?;
        let mut runs = HashMap::<(usize, usize), Vec<Option<AskedEntry>>>::new();
        for object in entries {
            let member = object
                .member
                .ok_or("what erasing an item read is stored as a member of a run")?;
            let bytes = object.bytes.as_slice();
            let run = match runs.entry((bytes.as_ptr() as usize, bytes.len())) {
                std::collections::hash_map::Entry::Occupied(read) => read.into_mut(),
                std::collections::hash_map::Entry::Vacant(unread) => {
                    unread.insert(AskedTable::read(bytes)?.into_iter().map(Some).collect())
                }
            };
            let (name, asked, made) = usize::try_from(member.index)
                .ok()
                .and_then(|index| run.get_mut(index))
                .and_then(Option::take)
                .ok_or_else(|| format!("a run of erasures holds no member {}", member.index))?;
            items.items.insert(
                name,
                Rc::new(AskedRecord::restored(asked, made, object.digest)),
            );
        }

        Ok(items)
    }
}

/// The digest of `part` of `name`, a declaration of the unit `module` is, held where `places` says and concluded as `certification` records: a signature or a body by the certifier's own digest ([`curios_cert::part_digest`]), and a kind by erasure's, what the definition is declared as and what it was generated from, nothing of an entry. `Ok(None)` where the unit declares no such part, a binding and forms whole being read off its erased forms instead.
pub(super) fn declared_digest(
    module: &curios_core::Module,
    places: &curios_core::Places,
    certification: &curios_core::Certification,
    (name, part): (curios_core::Global, Part),
) -> Result<Option<String>, String> {
    match part {
        Part::Signature => curios_cert::part_digest(module, places, certification, &name, false),
        Part::Body => curios_cert::part_digest(module, places, certification, &name, true),
        Part::Kind => declared(module, places, &name)
            .map(|definition| {
                let kind = (definition.kind.declared(), definition.kind.generated_from());

                curios_archive::to_bytes(&kind).map(|bytes| digest(&bytes))
            })
            .transpose(),
        Part::Binding | Part::Forms => Ok(None),
    }
}

/// The digest of what an item naming `name` reads under `part` of the erased forms `unit` holds at `place`, which declare it: a binding as [`ErasedForms::read_digest`](crate::ErasedForms::read_digest) reads it, and the forms whole as `unit` stores them.
pub(super) fn erased_digest(
    unit: &ErasedUnit,
    place: usize,
    (name, part): (curios_core::Global, Part),
) -> Result<Option<String>, String> {
    match part {
        Part::Binding => unit.item(place).read_digest(&name).map(Some),
        Part::Forms => unit.form_digest(place).map(Some),
        Part::Signature | Part::Body | Part::Kind => Ok(None),
    }
}

/// The thirty-two bytes a digest spells.
pub(super) fn bytes_of(digest: &str) -> Option<[u8; 32]> {
    Digest::from_hex(digest).map(|digest| *digest.as_bytes())
}
