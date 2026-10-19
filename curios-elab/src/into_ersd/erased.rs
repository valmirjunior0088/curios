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
    crate::{Foreign, HeldTrace, Question, Readers, Traces, part_digest},
    curios_archive::{Bytes, Deferred},
    curios_core::{Free, Global, Item, Module, Places},
    curios_utilities::{Digest, Object, Ordered, Persistent, Recorded, digest},
    std::{
        borrow::Borrow,
        cell::OnceCell,
        collections::{BTreeMap, BTreeSet, HashMap},
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
    /// The values whose initializers read themselves while nothing else of the fragment reads them ([`curios_ersd::Module::unforced`]). Nothing here forces one; an item that reads one would, and is refused where it reads it.
    ///
    /// **No position.** Where such a value is written is its declaration's, which a report finds by the name; a form holding it would be one thing erased from an item in hand and another from the same item restored from a store, which holds no position.
    unforced: BTreeSet<curios_ersd::ValueId>,
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
    /// To a value whose own initializer reads it and nothing else of its item does: reading it is the cycle forcing meets first.
    Unforced,
    /// To a function of `arity` parameters.
    Function {
        arity: usize,
        interface: curios_ersd::Interface,
    },
}

impl ErasedItem {
    pub(super) fn new(
        module: curios_ersd::Module,
        environment: Environment,
        values: Vec<Global>,
        functions: Vec<Global>,
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
            .map(|step| step.value)
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

    /// Whether an item that names `name` reads this erased form and `other` alike: the name bound the same way, and the row it declares, where it declares one, of the same shape under the same mask. What an importing item holds of a row is no more ([`ErasedItem::struct_row_read`], [`ErasedItem::induct_row_read`]).
    pub(super) fn read_alike(&self, other: &ErasedItem, name: &Global) -> bool {
        self.export(name) == other.export(name)
            && self.struct_row_read(name) == other.struct_row_read(name)
            && self.induct_row_read(name) == other.induct_row_read(name)
    }

    /// The digest of what an item that names `name` reads of this erased form: what [`ErasedItem::read_alike`] compares, archived. What a unit records of a binding it read in another unit's erased form, and what the binding is asked again by.
    pub(crate) fn read_digest(&self, name: &Global) -> Result<String, String> {
        let export = self.export(name).map(|export| match export {
            Export::Dropped => (0u8, None, None),
            Export::Constant(constant) => (1, Some(constant.clone()), None),
            Export::Value => (2, None, None),
            Export::Unforced => (3, None, None),
            Export::Function { arity, interface } => (4, None, Some((arity as u64, interface))),
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
            Binding::Atom(curios_ersd::Atom::Value(value)) => {
                match self.unforced.contains(&value) {
                    true => Export::Unforced,
                    false => Export::Value,
                }
            }
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

/// A unit's erased forms by place, which item declares each name, and which items compute: what the unit is restored with, the forms stored beside it.
///
/// **By the place the unit's module holds each item at** ([`curios_core::Items::places`]), in maps that are persistent: a unit erased over a baseline whose module is a version of the baseline's is the baseline's index with the forms of what it erased again placed again. What is stored names an item by its rank among the unit's items, read off where the unit is filed ([`Ranking`]).
#[derive(Debug, Clone, Default)]
pub struct ErasedIndex {
    /// Each item's erased form, by its place.
    items: Ordered<Rc<Form>>,
    /// The place of the item that declares each name.
    declared: Persistent<Global, i64>,
    /// The places of the items whose evaluation at initialization computes ([`curios_ersd::Module::computes`]): what a program links whether or not it names them, since whether one is observable is the program's to say.
    computing: Ordered<()>,
}

/// An erased unit's index as it is stored: each name by the rank of the item declaring it among the unit's items, and the ranks of the items that compute, ascending.
#[curios_archive::archived]
pub struct RankedIndex {
    declared: BTreeMap<Global, u32>,
    computing: Vec<u32>,
}

/// How an erased unit's index is archived: as a [`RankedIndex`], each place given as its item's rank. A unit restored holds its items at their ranks.
pub struct Ranking;

/// The field adapter: `#[archived_with(Ranked)]`.
pub type Ranked = curios_archive::Via<Ranking>;

impl curios_archive::Proxy<ErasedIndex> for Ranking {
    type Archivable = RankedIndex;

    fn to_archivable(index: &ErasedIndex) -> impl Borrow<RankedIndex> {
        let ranks = index
            .items
            .iter()
            .enumerate()
            .map(|(rank, (at, _))| (at, rank))
            .collect::<HashMap<_, _>>();
        // An index restored with no forms beside it holds its items at their ranks.
        let rank = |at: i64| {
            let rank = ranks
                .get(&at)
                .copied()
                .unwrap_or_else(|| usize::try_from(at).expect("a place is not negative"));
            u32::try_from(rank).expect("a unit's items are counted in a u32")
        };

        RankedIndex {
            declared: index
                .declared
                .iter()
                .map(|(name, at)| (*name, rank(*at)))
                .collect(),
            computing: index.computing.iter().map(|(at, _)| rank(at)).collect(),
        }
    }

    fn from_archivable(stored: RankedIndex) -> Result<ErasedIndex, String> {
        let mut index = ErasedIndex::default();
        for (name, rank) in stored.declared {
            index.declared.insert(name, i64::from(rank));
        }
        for rank in stored.computing {
            index.computing.insert(i64::from(rank), ());
        }

        Ok(index)
    }
}

/// A place as an index's key.
fn position(place: usize) -> i64 {
    i64::try_from(place).expect("a place is counted in an i64")
}

/// The place an index's key is.
fn place_of(position: i64) -> usize {
    usize::try_from(position).expect("a place is not negative")
}

/// A unit's items' erased forms, by the place the unit's module holds each item at ([`ErasedIndex`]).
///
/// Each is held by a handle of its own, the form or the bytes it was stored as until something reads it: a unit compiled over a baseline keeps the baseline's own handle for every item it does not erase again, so it copies none and reads none, and where it is filed that form's bytes are the ones it was restored from.
///
/// **What the unit is restored with is its index**, which name each item declares and which items compute: the forms are stored beside it, each an object of its own ([`ErasedUnit::items_objects`]), since erasing a unit reads the forms of the names its items use and a program the forms it links.
#[derive(Debug, Clone, Default)]
#[curios_archive::archived]
pub struct ErasedUnit {
    #[archived_with(Ranked)]
    index: ErasedIndex,
    /// What erasing each item asked of the declarations around it, by the first name the item declares — see the module documentation.
    ///
    /// **The unit's baseline half**, as a lowered unit keeps its own: stored beside what the unit is for its successors, each item's trace an object ([`ErasedUnit::recorded`]), handed back where the unit is restored ([`ErasedUnit::with_baseline`]), and read when an erasure over the unit first asks.
    #[archived_with(curios_archive::Skip)]
    asked: Recorded<Traces>,
    /// Who asked what, of what erasing each item asked, read backwards the first time an erasure over the unit asks (`Readers`).
    #[archived_with(curios_archive::Skip)]
    readers: OnceCell<Readers>,
}

impl ErasedUnit {
    /// Whether this holds no erased item — the freshness probe the archive tests use.
    pub fn is_empty(&self) -> bool {
        self.index.items.is_empty()
    }

    /// This unit's index with none of its traces: what a unit erased over this one starts from, where its module is a version of this one's.
    pub(super) fn version(&self) -> ErasedUnit {
        ErasedUnit {
            index: self.index.clone(),
            ..ErasedUnit::default()
        }
    }

    /// `item`'s erased form, made here, at `place`, in place of what was held there.
    pub(super) fn push(&mut self, place: usize, item: &Item, erased: ErasedItem) {
        let computes = erased.module.computes();
        self.keep(
            place,
            item,
            Held {
                erased: Rc::new(Form {
                    erased: Deferred::held(erased),
                    digest: None,
                }),
                computes,
            },
        );
    }

    /// `item`'s erased form as another unit holds it, at `place`, in place of what was held there. The form is not read: whether it computes is the holder's to say.
    pub(super) fn keep(&mut self, place: usize, item: &Item, held: Held) {
        let at = position(place);
        for name in item.declared_names() {
            self.index.declared.insert(*name, at);
        }
        match held.computes {
            true => self.index.computing.insert(at, ()),
            false => {
                self.index.computing.remove(at);
            }
        }
        self.index.items.insert(at, held.erased);
    }

    /// Take out the erased form held at `place`, and each of `names` declared there: an item the unit no longer holds.
    pub(super) fn forget(&mut self, place: usize, names: &[&Global]) {
        let at = position(place);
        self.index.items.remove(at);
        self.index.computing.remove(at);
        for name in names {
            if self.index.declared.get(name) == Some(&at) {
                self.index.declared.remove(name);
            }
        }
    }

    /// The erased form of the item at `place`, read out of its bytes now where the unit was restored and nothing has read it before.
    pub(crate) fn item(&self, place: usize) -> &ErasedItem {
        self.form(place)
            .erased
            .get()
            .unwrap_or_else(|error| panic!("a unit's erased form failed to restore: {error}"))
    }

    /// The handle of the erased form at `place`, which the unit holds.
    fn form(&self, place: usize) -> &Rc<Form> {
        self.index
            .items
            .get(position(place))
            .expect("a place the unit holds a form at")
    }

    /// The erased forms, in item order, each read as [`ErasedUnit::item`] reads it.
    pub(super) fn forms(&self) -> impl Iterator<Item = &ErasedItem> {
        self.index.items.iter().map(|(_, form)| {
            form.erased
                .get()
                .unwrap_or_else(|error| panic!("a unit's erased form failed to restore: {error}"))
        })
    }

    /// Where the item declaring `name` sits.
    pub(crate) fn place(&self, name: &Global) -> Option<usize> {
        self.index.declared.get(name).map(|at| place_of(*at))
    }

    /// The erased form of the item declaring `name`.
    pub(super) fn find(&self, name: &Global) -> Option<&ErasedItem> {
        self.place(name).map(|place| self.item(place))
    }

    /// The erased form of the item declaring `name`, as this unit holds it, with whether it computes: what a unit compiled over this one keeps, reading nothing.
    pub(super) fn held(&self, name: &Global) -> Option<Held> {
        let place = self.place(name)?;

        Some(Held {
            erased: Rc::clone(self.form(place)),
            computes: self.index.computing.get(position(place)).is_some(),
        })
    }

    /// Whether the item declaring `name` holds one erased form in this unit and in `other`: the one handle, or two forms stored as the same bytes, told by their digests where both were restored.
    pub(super) fn form_alike(&self, other: &ErasedUnit, name: &Global) -> bool {
        let (Some(this), Some(that)) = (self.place(name), other.place(name)) else {
            return false;
        };
        let (this, that) = (self.form(this), other.form(that));
        if Rc::ptr_eq(this, that) {
            return true;
        }

        match (this.digest, that.digest) {
            (Some(this), Some(that)) => this == that,
            _ => matches!(
                (this.erased.bytes(), that.erased.bytes()),
                (Ok(this), Ok(that)) if this == that
            ),
        }
    }

    /// The digest of the erased form of the item at `place`, whole, as it is stored.
    pub(crate) fn form_digest(&self, place: usize) -> Result<String, String> {
        self.form(place).object().map(|object| object.digest.hex())
    }

    /// The places of the items that compute at initialization, ascending.
    pub(super) fn computing(&self) -> impl Iterator<Item = usize> {
        self.index.computing.iter().map(|(at, _)| place_of(at))
    }

    /// This unit with `asked` as what erasing each of its items asked.
    pub(super) fn asking(mut self, asked: Traces) -> Self {
        self.asked = Recorded::held(asked);
        self
    }

    /// What erasing each item asked, read out of the unit's baseline half now, where the unit was restored and no erasure over it has asked before.
    pub(super) fn traces(&self) -> &Traces {
        self.asked
            .get()
            .unwrap_or_else(|error| panic!("an erased unit's traces failed to restore: {error}"))
    }

    /// Who asked what, of what erasing each item asked, read backwards the first time it is asked: what an erasure over the unit finds the items an edit could reach by.
    pub(super) fn readers(&self) -> &Readers {
        self.readers.get_or_init(|| Readers::of(self.traces()))
    }

    /// This unit holding `readers` as who asked what of its traces: the baseline's, with the edges of every item erased again or no longer held replaced.
    pub(super) fn reading(self, readers: Readers) -> Self {
        let held = OnceCell::new();
        let _ = held.set(readers);

        Self {
            readers: held,
            ..self
        }
    }

    /// What erasing the item declaring `name` first asked, as the unit holds it.
    pub(super) fn asked(&self, name: &Global) -> Option<&HeldTrace> {
        self.traces().held(name)
    }

    /// What erasing the unit's items read of declarations the unit does not hold, each part and each binding by the digest it had.
    pub fn foreign(&self) -> &Rc<Foreign> {
        self.traces().foreign()
    }

    /// This unit's baseline half: what erasing each of its items asked, as the unit holds it, which nothing compiled after the unit reads, and which it stores apart from what it is for its successors, each item's trace an object.
    pub fn recorded(&self) -> &Recorded<Traces> {
        &self.asked
    }

    /// This unit with `table` and `entries` as its baseline half ([`ErasedUnit::recorded`]), read when an erasure over the unit first asks: what a unit restored without one is given, since restoring it leaves the half empty.
    pub fn with_baseline(mut self, table: impl Into<Bytes>, entries: Vec<Object>) -> Self {
        self.asked = Recorded::stored(table, entries);
        self
    }

    /// This unit's erased forms, each an object stored on its own, in the order of the items: the one a form was restored from, and otherwise its archive, made and digested now.
    pub fn items_objects(&self) -> Result<Vec<Object>, String> {
        self.index
            .items
            .iter()
            .map(|(_, form)| form.object())
            .collect()
    }

    /// This unit with `objects` as its erased forms ([`ErasedUnit::items_objects`]), each read when something first asks for it: what a restored unit is given, since restoring it leaves it with none. `Err` is not one for each item the unit's index names, or one stored with others, since a form is stored alone.
    pub fn with_items(mut self, items: Vec<Object>) -> Result<Self, String> {
        if items.iter().any(|object| object.member.is_some()) {
            return Err("an erased form is stored alone".to_string());
        }
        let named = |at: i64| usize::try_from(at).is_ok_and(|place| place < items.len());
        if !self.index.declared.iter().all(|(_, at)| named(*at))
            || !self.index.computing.iter().all(|(at, _)| named(at))
        {
            return Err("a unit's erased forms are fewer than its index names".to_string());
        }

        let mut forms = Ordered::new();
        for (place, object) in items.into_iter().enumerate() {
            forms.insert(
                position(place),
                Rc::new(Form {
                    erased: Deferred::stored(object.bytes),
                    digest: Some(object.digest),
                }),
            );
        }
        self.index.items = forms;

        Ok(self)
    }
}

#[cfg(feature = "test-support")]
impl ErasedUnit {
    /// [`declared_families`](crate::test_support::declared_families)' answer: each family read off the fragment of the item that declares it, a field at a family naming it.
    pub(crate) fn declared_families(&self) -> Vec<crate::test_support::ErasedFamily> {
        self.forms()
            .flat_map(|form| {
                let module = form.module();

                form.environment().declared_induct_rows().map(move |row| {
                    let family = module
                        .family(row.family)
                        .expect("a declared row's family is its fragment's");

                    crate::test_support::ErasedFamily {
                        name: family.debug_name.clone().unwrap_or_default(),
                        constructors: family
                            .constructors
                            .iter()
                            .map(|&id| {
                                module
                                    .constructor(id)
                                    .expect("a declared row's constructor is its fragment's")
                                    .fields
                                    .iter()
                                    .map(|field| crate::test_support::ErasedField {
                                        shape: field.shape,
                                        family: match field.shape {
                                            curios_ersd::FieldShape::Family(family) => module
                                                .family(family)
                                                .and_then(|family| family.debug_name.clone()),
                                            _ => None,
                                        },
                                    })
                                    .collect()
                            })
                            .collect(),
                    }
                })
            })
            .collect()
    }
}

/// An item's erased form as a unit holds it, with whether evaluating it at initialization computes: what another unit keeps of it without reading it.
#[derive(Debug, Clone)]
pub(super) struct Held {
    erased: Rc<Form>,
    computes: bool,
}

impl Held {
    /// The erased form `object` holds, as a store keeps it, with whether it computes: what a unit takes from a candidate, stored again as the bytes it was read from.
    pub(super) fn stored(object: Object) -> Result<Held, String> {
        let erased = Rc::new(Form {
            erased: Deferred::stored(object.bytes),
            digest: Some(object.digest),
        });
        let computes = erased.erased.get()?.module.computes();

        Ok(Held { erased, computes })
    }
}

/// An item's erased form, in hand or read out of the object it was stored as, whose digest it keeps: what it is stored as again and compared by, digesting nothing.
#[derive(Debug)]
struct Form {
    erased: Deferred<ErasedItem>,
    /// The digest of the object the form was restored from, where it was.
    digest: Option<Digest>,
}

impl Form {
    /// The object this form is stored as: the one it was restored from, and otherwise its archive, made and digested now.
    fn object(&self) -> Result<Object, String> {
        match (self.digest, self.erased.as_stored()) {
            (Some(digest), Some(bytes)) => Ok(Object {
                digest,
                bytes: bytes.clone(),
                member: None,
            }),
            _ => self
                .erased
                .bytes()
                .map(|bytes| Object::of(bytes.into_owned())),
        }
    }
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
