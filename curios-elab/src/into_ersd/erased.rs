//! What an item erases to, what a unit's items erase to together, and the scope both are read through.
//!
//! **An item's erased form is its own.** An item erases into a fragment, a [`curios_ersd::Module`] whose identities count from zero for it, so what a unit stores for a declaration says nothing of the declarations erased before it, and an edit to one renumbers no other. A name another item binds is not an identity of the fragment but an *import* of it, known by that name ([`ErasedItem::values`], [`ErasedItem::functions`]), and so is the row of a structure or an inductive another item declares: the fragment holds a schema of its widths, known by the declaration it stands for.
//!
//! **A unit holds its items' erased forms and no arena**, which is what lets a unit compiled over a baseline erase the items it redoes and take every other's as it stands, and what leaves nothing for two units to be ordered by.
//!
//! **A unit keeps what erasing each item asked.** An item's erased form is a function of its elaborated declaration and of what its erasure reads of the declarations around it: their types and registry entries, through the doors an elaboration reads them by, and how each name it uses is bound in the erased form of the item that declares it. Each is noted as the item is erased (`Traced`), and the unit keeps each item's trace as its baseline half: a unit erased over this one keeps an erased form where everything its erasure asked is answered as it was, and nothing compiled after the unit reads the traces.
//!
//! **A scope is the units' own, borrowed.** Erasing an item reads another's erased form for one thing, how its name is bound — to nothing at run time, to a constant, to a value or to a function of some arity and interface — and reads it where the name is first used.

use {
    super::{Binding, Environment, Projection},
    crate::{Answered, Foreign, Question, Traces, part_digest},
    curios_archive::{Bytes, Deferred, joined, parted},
    curios_core::{Extent, Free, Global, Item, Module, Places, Trace},
    curios_utilities::digest,
    std::{
        borrow::Cow,
        collections::{BTreeMap, BTreeSet},
        rc::Rc,
    },
};

/// One item's erased form — see the module documentation.
#[derive(Debug, Clone)]
#[curios_archive::archived(recursive)]
pub struct ErasedItem {
    #[archived_omit_bounds]
    module: curios_ersd::Module,
    /// What each name the item declares is bound to, in the fragment's identities, and the fragment's schemas by the declaration each is the row of, the ones it declares told from the ones it imports.
    environment: Environment,
    /// The name each value the fragment imports stands for, in the fragment's order.
    values: Vec<Global>,
    /// The name each function the fragment imports stands for, in the fragment's order.
    functions: Vec<Global>,
    /// What applying each function a declared name is bound to does ([`curios_ersd::Module::interface`]), read once here, where the fragment is final, since every item that imports the name asks.
    interfaces: BTreeMap<curios_ersd::FunctionId, curios_ersd::Interface>,
    /// The values whose initializers read themselves while nothing else of the fragment reads them ([`curios_ersd::Module::unforced`]), each with the name a refusal calls it by and where its initializer is written. Nothing here forces one; an item that reads one would, and is refused.
    unforced: BTreeMap<curios_ersd::ValueId, (String, Option<Extent>)>,
    /// The names bound to what an evaluation of the item computed, where that is another atom than the one the item bound them to. A function one of them is bound to is a closure the item computed and no definition's own, so what a record says of the definition is not said of it.
    computed: BTreeSet<Global>,
}

/// How a name is bound, as an item that uses it reads it.
#[derive(PartialEq)]
pub(super) enum Export<'a> {
    /// To nothing at run time: a proof or a type.
    Dropped,
    /// To a constant, which is read where the name is used.
    Constant(&'a curios_ersd::Constant),
    /// To a value its item computes.
    Value,
    /// To a value whose own initializer reads it and nothing else of its item does: reading it is the cycle forcing meets first, under the name `member`, written at `extent`.
    Unforced {
        member: &'a str,
        extent: Option<&'a Extent>,
    },
    /// To a function of `arity` parameters.
    Function {
        arity: usize,
        interface: curios_ersd::Interface,
    },
}

impl ErasedItem {
    /// `extents` holds where each computed member's initializer is written, by the identity it was minted as.
    pub(super) fn new(
        module: curios_ersd::Module,
        environment: Environment,
        values: Vec<Global>,
        functions: Vec<Global>,
        extents: &BTreeMap<curios_ersd::ValueId, Extent>,
    ) -> Self {
        let interfaces = environment
            .bindings()
            .filter_map(|(_, binding)| match binding {
                Binding::Atom(curios_ersd::Atom::Function(function)) => {
                    Some((function, module.interface(function)))
                }
                _ => None,
            })
            .collect();
        let unforced = module
            .unforced()
            .into_iter()
            .map(|step| (step.value, (step.member, extents.get(&step.value).cloned())))
            .collect();

        Self {
            module,
            environment,
            values,
            functions,
            interfaces,
            unforced,
            computed: BTreeSet::new(),
        }
    }

    /// This erased form with `names` as the names bound to what an evaluation of the item computed.
    pub(super) fn computing(mut self, names: BTreeSet<Global>) -> Self {
        self.computed = names;
        self
    }

    /// Whether `name` is bound to what an evaluation of the item computed.
    pub(super) fn computed(&self, name: &Global) -> bool {
        self.computed.contains(name)
    }

    /// The fragment.
    pub(super) fn module(&self) -> &curios_ersd::Module {
        &self.module
    }

    /// The bindings of the names the item declares, and its schema maps.
    pub(super) fn environment(&self) -> &Environment {
        &self.environment
    }

    /// The name each imported value of the fragment stands for, in the fragment's order.
    pub(super) fn values(&self) -> &[Global] {
        &self.values
    }

    /// The name each imported function of the fragment stands for, in the fragment's order.
    pub(super) fn functions(&self) -> &[Global] {
        &self.functions
    }

    /// Where the initializer of each value that reads itself is written, where it is written anywhere, by the value's identity in the fragment.
    pub(super) fn extents(&self) -> impl Iterator<Item = (curios_ersd::ValueId, &Extent)> {
        self.unforced
            .iter()
            .filter_map(|(value, (_, extent))| Some((*value, extent.as_ref()?)))
    }

    /// Whether an item that names `name` reads this erased form and `other` alike: the name bound the same way, and the row it declares, where it declares one, of the same shape under the same mask. What an importing item holds of a row is no more ([`ErasedItem::struct_row_read`], [`ErasedItem::induct_row_read`]).
    pub(super) fn read_alike(&self, other: &ErasedItem, name: &Global) -> bool {
        self.export(name) == other.export(name)
            && self.struct_row_read(name) == other.struct_row_read(name)
            && self.induct_row_read(name) == other.induct_row_read(name)
    }

    /// The digest of what an item that names `name` reads of this erased form: what [`ErasedItem::read_alike`] compares, archived. What a unit records of a binding it read in another unit's erased form, and what the binding is asked again by.
    pub(super) fn read_digest(&self, name: &Global) -> Result<String, String> {
        let export = self.export(name).map(|export| match export {
            Export::Dropped => (0u8, None, None, None),
            Export::Constant(constant) => (1, Some(constant.clone()), None, None),
            Export::Value => (2, None, None, None),
            Export::Unforced { member, extent } => {
                (3, None, Some((member.to_string(), extent.cloned())), None)
            }
            Export::Function { arity, interface } => {
                (4, None, None, Some((arity as u64, interface)))
            }
        });
        let record = self.struct_row_read(name).map(|(schema, mask)| {
            (
                schema
                    .map(|(name, width, shared)| (name.map(str::to_string), width as u64, shared)),
                mask.clone(),
            )
        });
        let family = self.induct_row_read(name).map(|(name, constructors)| {
            (
                name.map(str::to_string),
                constructors
                    .into_iter()
                    .map(|(name, width, mask)| {
                        (name.map(str::to_string), width as u64, mask.clone())
                    })
                    .collect::<Vec<_>>(),
            )
        });

        curios_archive::to_bytes(&(export, record, family)).map(|bytes| digest(&bytes))
    }

    /// What an item importing the structure `name` reads of the row this item declares for it: its mask, and of its schema the name, the width and whether it is shared.
    fn struct_row_read(&self, name: &Global) -> Option<StructRowRead<'_>> {
        let declared = self.environment.declared_struct_row(name)?;
        let schema = declared.schema.map(|schema| {
            let held = self
                .module
                .product(schema)
                .expect("a declared row's schema is its fragment's");

            (held.debug_name.as_deref(), held.width(), held.shared)
        });

        Some((schema, &declared.mask))
    }

    /// What an item importing the inductive `name` reads of the row this item declares for it: the family's name, and each constructor's name, width and mask.
    fn induct_row_read(&self, name: &Global) -> Option<InductRowRead<'_>> {
        let declared = self.environment.declared_induct_row(name)?;
        let family = self
            .module
            .family(declared.family)
            .expect("a declared row's family is its fragment's");
        let constructors = declared
            .constructors
            .iter()
            .map(|constructor| {
                let held = self
                    .module
                    .constructor(constructor.id)
                    .expect("a declared row's constructor is its fragment's");

                (held.debug_name.as_deref(), held.width(), &constructor.mask)
            })
            .collect();

        Some((family.debug_name.as_deref(), constructors))
    }

    /// How the item binds `name`, where it declares it.
    pub(super) fn export(&self, name: &Global) -> Option<Export<'_>> {
        Some(match self.environment.lookup(&Free::from(name))? {
            Binding::Dropped => Export::Dropped,
            Binding::Atom(curios_ersd::Atom::Constant(constant)) => Export::Constant(
                self.module
                    .constant(constant)
                    .expect("an exported constant is the fragment's"),
            ),
            Binding::Atom(curios_ersd::Atom::Value(value)) => match self.unforced.get(&value) {
                Some((member, extent)) => Export::Unforced {
                    member,
                    extent: extent.as_ref(),
                },
                None => Export::Value,
            },
            Binding::Atom(curios_ersd::Atom::Function(function)) => Export::Function {
                arity: self
                    .module
                    .function(function)
                    .expect("an exported function is the fragment's")
                    .params
                    .len(),
                interface: self.interfaces[&function].clone(),
            },
        })
    }
}

/// A schema's name, its width and whether it is shared, where a structure's row has one, and the row's mask.
type StructRowRead<'a> = (Option<(Option<&'a str>, usize, bool)>, &'a Vec<bool>);

/// A family's name, and each of its constructors' name, width and mask.
type InductRowRead<'a> = (
    Option<&'a str>,
    Vec<(Option<&'a str>, usize, &'a Vec<bool>)>,
);

/// A unit's items' erased forms, in the order the unit stores its items.
///
/// Each is held by a handle of its own, the form or the bytes it was stored as until something reads it: a unit compiled over a baseline keeps the baseline's own handle for every item it does not erase again, so it copies none and reads none, and where it is filed that form's bytes are the ones it was restored from.
///
/// **What the unit is restored with is its index**, which name each item declares and which items compute: the forms are stored beside it, one on its own for each ([`ErasedUnit::items_bytes`]), since erasing a unit reads the forms of the names its items use and a program the forms it links.
#[derive(Debug, Clone, Default)]
#[curios_archive::archived]
pub struct ErasedUnit {
    #[archived_with(curios_archive::Skip)]
    items: Vec<Rc<Deferred<ErasedItem>>>,
    /// The item that declares each name.
    declared: BTreeMap<Global, u32>,
    /// The items whose evaluation at initialization computes ([`curios_ersd::Module::computes`]), ascending: what a program links whether or not it names them, since whether one is observable is the program's to say.
    computing: Vec<u32>,
    /// What erasing each item asked of the declarations around it, by the first name the item declares — see the module documentation.
    ///
    /// **The unit's baseline half**, as a lowered unit keeps its own: stored beside what the unit is for its successors ([`ErasedUnit::baseline_bytes`]), handed back as bytes where the unit is restored ([`ErasedUnit::with_baseline`]), and read out of them when an erasure over the unit first asks.
    #[archived_with(curios_archive::Skip)]
    asked: Deferred<Traces>,
}

impl ErasedUnit {
    /// Whether this holds no erased item — the freshness probe the archive tests use.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Add `item`'s erased form, made here, after those of the items before it.
    pub(super) fn push(&mut self, item: &Item, erased: ErasedItem) {
        let computes = erased.module.computes();
        self.keep(
            item,
            Held {
                erased: Rc::new(Deferred::held(erased)),
                computes,
            },
        );
    }

    /// Add `item`'s erased form as another unit holds it, after those of the items before it. The form is not read: whether it computes is the holder's to say.
    pub(super) fn keep(&mut self, item: &Item, held: Held) {
        let place = u32::try_from(self.items.len()).expect("a unit's items are counted in a u32");
        for name in item.declared_names() {
            self.declared.insert(*name, place);
        }
        if held.computes {
            self.computing.push(place);
        }
        self.items.push(held.erased);
    }

    /// The erased form of the item at `place`, read out of its bytes now where the unit was restored and nothing has read it before.
    pub(super) fn item(&self, place: usize) -> &ErasedItem {
        self.items[place]
            .get()
            .unwrap_or_else(|error| panic!("a unit's erased form failed to restore: {error}"))
    }

    /// The erased forms, in item order, each read as [`ErasedUnit::item`] reads it.
    pub(super) fn forms(&self) -> impl Iterator<Item = &ErasedItem> {
        (0..self.items.len()).map(|place| self.item(place))
    }

    /// Where the item declaring `name` sits.
    pub(super) fn place(&self, name: &Global) -> Option<usize> {
        self.declared.get(name).map(|place| *place as usize)
    }

    /// The erased form of the item declaring `name`.
    pub(super) fn find(&self, name: &Global) -> Option<&ErasedItem> {
        self.place(name).map(|place| self.item(place))
    }

    /// The erased form of the item declaring `name`, as this unit holds it, with whether it computes: what a unit compiled over this one keeps, reading nothing.
    pub(super) fn held(&self, name: &Global) -> Option<Held> {
        let place = self.place(name)?;
        let at = u32::try_from(place).expect("a unit's items are counted in a u32");

        Some(Held {
            erased: Rc::clone(&self.items[place]),
            computes: self.computing.binary_search(&at).is_ok(),
        })
    }

    /// Whether the item declaring `name` holds one erased form in this unit and in `other`: the one handle, or two forms stored as the same bytes.
    pub(super) fn form_alike(&self, other: &ErasedUnit, name: &Global) -> bool {
        let (Some(this), Some(that)) = (self.place(name), other.place(name)) else {
            return false;
        };
        let (this, that) = (&self.items[this], &other.items[that]);
        if Rc::ptr_eq(this, that) {
            return true;
        }

        matches!((this.bytes(), that.bytes()), (Ok(this), Ok(that)) if this == that)
    }

    /// The digest of the erased form of the item at `place`, whole, as it is stored.
    fn form_digest(&self, place: usize) -> Result<String, String> {
        self.items[place].bytes().map(|bytes| digest(&bytes))
    }

    /// The places of the items that compute at initialization, ascending.
    pub(super) fn computing(&self) -> impl Iterator<Item = usize> {
        self.computing.iter().map(|place| *place as usize)
    }

    /// This unit with `asked` as what erasing each of its items asked.
    pub(super) fn asking(mut self, asked: Traces) -> Self {
        self.asked = Deferred::held(asked);
        self
    }

    /// What erasing each item asked, read out of the unit's baseline half now, where the unit was restored and no erasure over it has asked before.
    fn traces(&self) -> &Traces {
        self.asked
            .get()
            .unwrap_or_else(|error| panic!("an erased unit's traces failed to restore: {error}"))
    }

    /// What erasing the item declaring `name` first asked, as the unit holds it.
    pub(super) fn asked(&self, name: &Global) -> Option<&Rc<Trace<Question, Answered>>> {
        self.traces().held(name)
    }

    /// What erasing the unit's items read of declarations the unit does not hold, each part and each binding by the digest it had.
    pub fn foreign(&self) -> &Rc<Foreign> {
        self.traces().foreign()
    }

    /// How many items' erasures hold a trace.
    pub(super) fn asked_len(&self) -> usize {
        self.traces().len()
    }

    /// The bytes of this unit's baseline half: what erasing each of its items asked, stored apart from what the unit is for its successors.
    pub fn baseline_bytes(&self) -> Result<Cow<'_, [u8]>, String> {
        self.asked.bytes()
    }

    /// This unit with `bytes` as its baseline half, read when an erasure over the unit first asks: what a unit restored without one is given, since restoring it leaves the half empty.
    pub fn with_baseline(mut self, bytes: impl Into<Bytes>) -> Self {
        self.asked = Deferred::stored(bytes);
        self
    }

    /// The bytes of this unit's erased forms, each stored on its own behind how many they are and how long each is ([`joined`]): the ones a form was restored from, and otherwise its archive, made now.
    pub fn items_bytes(&self) -> Result<Vec<u8>, String> {
        self.items
            .iter()
            .map(|erased| erased.bytes())
            .collect::<Result<Vec<_>, _>>()
            .map(|parts| joined(&parts))
    }

    /// This unit with `bytes` as its erased forms ([`ErasedUnit::items_bytes`]), each read when something first asks for it: what a restored unit is given, since restoring it leaves it with none. `Err` is bytes that hold no such forms, or not one for each item the unit's index names.
    pub fn with_items(mut self, bytes: impl Into<Bytes>) -> Result<Self, String> {
        let items = parted(&bytes.into()).ok_or("not a unit's erased forms")?;
        let named = |place: &u32| (*place as usize) < items.len();
        if !self.declared.values().all(named) || !self.computing.iter().all(named) {
            return Err("a unit's erased forms are fewer than its index names".to_string());
        }

        self.items = items
            .into_iter()
            .map(|bytes| Rc::new(Deferred::stored(bytes)))
            .collect();

        Ok(self)
    }
}

/// An item's erased form as a unit holds it, with whether evaluating it at initialization computes: what another unit keeps of it without reading it.
#[derive(Debug, Clone)]
pub(super) struct Held {
    erased: Rc<Deferred<ErasedItem>>,
    computes: bool,
}

/// The units in scope, as what erasing another reads: the Core each was erased from, and its items' erased forms.
///
/// The two halves describe the same units, in dependency order, and pairing them in one value is the point: as two parameters nothing would say they correspond. The Core is read through, each declaration out of its universes when it is first read (`Projected`), so the types the unit's erasure reads reduce through it; the erased forms answer how each name is bound.
#[derive(Clone, Copy, Default)]
pub struct Erasures<'a> {
    /// Universes are taken as already validated — `curios-prelude` validates the archive as it restores, which is where untrusted bytes become a `Module`, and the value is immutable from then on.
    cores: &'a [&'a Module],
    /// What each unit keeps for erasure, where the caller holds the units and not their modules alone: where it holds each name, and what has been read of it out of its universes. `None` keys each module for this erasure.
    kept: Option<&'a [(&'a Places, &'a Projection)]>,
    erased: &'a [&'a ErasedUnit],
    /// Which units are other than the ones the baseline was erased after ([`Erasures::after`]): none where nothing says.
    moved: Option<&'a [bool]>,
    /// Which units are the compiler's own ([`Erasures::fixed`]): none where nothing says.
    fixed: Option<&'a [bool]>,
}

impl<'a> Erasures<'a> {
    /// Nothing in scope: the unit is erased against itself alone.
    pub fn nothing() -> Self {
        Self::default()
    }

    /// The units `cores` were elaborated to and `erased` are the erased forms of, in dependency order.
    pub fn of(cores: &'a [&'a Module], erased: &'a [&'a ErasedUnit]) -> Self {
        assert_eq!(
            cores.len(),
            erased.len(),
            "a scope pairs each unit's Core with its erased forms",
        );

        Self {
            cores,
            kept: None,
            erased,
            moved: None,
            fixed: None,
        }
    }

    /// These units with `moved` saying of each, in the same order, whether it is other than the one the baseline was erased after: what `Established::after` says of elaboration's scope.
    pub fn after(self, moved: &'a [bool]) -> Self {
        assert_eq!(
            self.cores.len(),
            moved.len(),
            "what moved is said of each unit in scope",
        );

        Self {
            moved: Some(moved),
            ..self
        }
    }

    /// These units with `fixed` saying of each, in the same order, whether it is the compiler's own, of which no read is recorded: what `Established::fixed` says of elaboration's scope.
    pub fn fixed(self, fixed: &'a [bool]) -> Self {
        assert_eq!(
            self.cores.len(),
            fixed.len(),
            "what is fixed is said of each unit in scope",
        );

        Self {
            fixed: Some(fixed),
            ..self
        }
    }

    /// Whether every unit in scope is the one the baseline was erased after.
    pub(super) fn settled(&self) -> bool {
        !self.moved.is_some_and(|moved| moved.contains(&true))
    }

    /// Whether what a baseline's erasure read of `name`, which the unit being erased does not declare, stands unasked: every unit in scope is the one the baseline was erased after, or the one that declares `name` is.
    pub(super) fn unmoved(&self, name: &Global) -> bool {
        let Some(moved) = self.moved.filter(|moved| moved.contains(&true)) else {
            return true;
        };

        self.place(name).is_some_and(|(unit, _)| !moved[unit])
    }

    /// Whether the unit at `place` is the compiler's own, of which no read is recorded.
    pub(super) fn is_fixed(&self, place: usize) -> bool {
        self.fixed.is_some_and(|fixed| fixed[place])
    }

    /// Whether a read of `name`, which the unit being erased does not declare, is one to record: every one but a read of a fixed unit's declaration.
    pub(super) fn recorded(&self, name: &Global) -> bool {
        self.place(name)
            .is_none_or(|(unit, _)| !self.is_fixed(unit))
    }

    /// The digest of what `question` asks of the declaration a unit in scope holds under its name: a part of its elaborated declaration ([`part_digest`]), how an item naming it reads its erased form ([`ErasedItem::read_digest`]), or that form whole. `Ok(None)` where no unit declares the name or the one that does holds no such part; `Err` where a part is asked of a scope read without its units' places.
    pub(super) fn part_digest(&self, question: &Question) -> Result<Option<String>, String> {
        let Some(name) = question.subject() else {
            return Ok(None);
        };
        let Some((unit, place)) = self.place(name) else {
            return Ok(None);
        };

        match question {
            Question::Erased(_) => self.erased[unit].item(place).read_digest(name).map(Some),
            Question::Ran(_) => self.erased[unit].form_digest(place).map(Some),
            part => match self.kept(unit) {
                Some((places, _)) => part_digest(self.cores[unit], places, part),
                None => Err("a scope read without its units' places digests no part".to_string()),
            },
        }
    }

    /// These units read by what each keeps for erasure, `kept`, in the same order: nothing of a unit is keyed for the erasure, and what it projects of one stays projected.
    pub fn keeping(self, kept: &'a [(&'a Places, &'a Projection)]) -> Self {
        assert_eq!(
            self.cores.len(),
            kept.len(),
            "a scope pairs each unit's Core with what the unit keeps for erasure",
        );

        Self {
            kept: Some(kept),
            ..self
        }
    }

    /// What the unit at `place` keeps for erasure, where the scope holds it.
    pub(super) fn kept(&self, place: usize) -> Option<(&'a Places, &'a Projection)> {
        self.kept.map(|kept| kept[place])
    }

    /// The Core each unit was erased from, in dependency order.
    pub(super) fn cores(&self) -> &'a [&'a Module] {
        self.cores
    }

    /// The units' erased forms, in dependency order.
    pub(super) fn units(&self) -> &'a [&'a ErasedUnit] {
        self.erased
    }

    /// The unit and the place of the item declaring `name`, where a unit in scope declares it.
    pub(super) fn place(&self, name: &Global) -> Option<(usize, usize)> {
        self.erased
            .iter()
            .enumerate()
            .find_map(|(unit, erased)| Some((unit, erased.place(name)?)))
    }

    /// The erased form of the item declaring `name`, where a unit in scope declares it.
    pub(super) fn find(&self, name: &Global) -> Option<&'a ErasedItem> {
        self.erased.iter().find_map(|erased| erased.find(name))
    }
}
