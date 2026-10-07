//! What an item erases to, what a unit's items erase to together, and the scope both are read through.
//!
//! **An item's erased form is its own.** An item erases into a fragment, a [`curios_ersd::Module`] whose identities count from zero for it, so what a unit stores for a declaration says nothing of the declarations erased before it, and an edit to one renumbers no other. A name another item binds is not an identity of the fragment but an *import* of it, known by that name ([`ErasedItem::values`], [`ErasedItem::functions`]), and a schema the fragment constructs or matches is the fragment's copy, known by the declaration it is the row of.
//!
//! **A unit holds its items' erased forms and no arena**, which is what lets a unit compiled over a baseline erase the items it redoes and take every other's as it stands, and what leaves nothing for two units to be ordered by.
//!
//! **A scope is the units' own, borrowed.** Erasing an item reads another's erased form for one thing, how its name is bound — to nothing at run time, to a constant, to a value or to a function of some arity and interface — and reads it where the name is first used.

use {
    super::{Binding, Environment, UniverseErased},
    curios_core::{Free, Global, Item, Module},
    curios_utilities::Span,
    std::collections::BTreeMap,
};

/// One item's erased form — see the module documentation.
#[derive(Debug, Clone)]
#[curios_archive::archived(recursive)]
pub struct ErasedItem {
    #[archived_omit_bounds]
    module: curios_ersd::Module,
    /// What each name the item declares is bound to, in the fragment's identities, and the fragment's schemas by the declaration each is the row of.
    environment: Environment,
    /// The name each value the fragment imports stands for, in the fragment's order.
    values: Vec<Global>,
    /// The name each function the fragment imports stands for, in the fragment's order.
    functions: Vec<Global>,
    /// What applying each function a declared name is bound to does ([`curios_ersd::Module::interface`]), read once here, where the fragment is final, since every item that imports the name asks.
    interfaces: BTreeMap<curios_ersd::FunctionId, curios_ersd::Interface>,
    /// The values whose initializers read themselves while nothing else of the fragment reads them ([`curios_ersd::Module::unforced`]), each with the name a refusal calls it by and where its initializer is written. Nothing here forces one; an item that reads one would, and is refused.
    unforced: BTreeMap<curios_ersd::ValueId, (String, Option<Span>)>,
}

/// How a name is bound, as an item that uses it reads it.
pub(super) enum Export<'a> {
    /// To nothing at run time: a proof or a type.
    Dropped,
    /// To a constant, which is read where the name is used.
    Constant(&'a curios_ersd::Constant),
    /// To a value its item computes.
    Value,
    /// To a value whose own initializer reads it and nothing else of its item does: reading it is the cycle forcing meets first, under the name `member`, written at `span`.
    Unforced {
        member: &'a str,
        span: Option<&'a Span>,
    },
    /// To a function of `arity` parameters.
    Function {
        arity: usize,
        interface: curios_ersd::Interface,
    },
}

impl ErasedItem {
    /// `spans` holds where each computed member's initializer is written, by the identity it was minted as.
    pub(super) fn new(
        module: curios_ersd::Module,
        environment: Environment,
        values: Vec<Global>,
        functions: Vec<Global>,
        spans: &BTreeMap<curios_ersd::ValueId, Span>,
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
            .map(|step| (step.value, (step.member, spans.get(&step.value).cloned())))
            .collect();

        Self {
            module,
            environment,
            values,
            functions,
            interfaces,
            unforced,
        }
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
                Some((member, span)) => Export::Unforced {
                    member,
                    span: span.as_ref(),
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

/// A unit's items' erased forms, in the order the unit stores its items.
#[derive(Debug, Clone, Default)]
#[curios_archive::archived(recursive)]
pub struct ErasedUnit {
    #[archived_omit_bounds]
    items: Vec<ErasedItem>,
    /// The item that declares each name.
    declared: BTreeMap<Global, u32>,
    /// The items whose evaluation at initialization computes ([`curios_ersd::Module::computes`]), ascending: what a program links whether or not it names them, since whether one is observable is the program's to say.
    computing: Vec<u32>,
}

impl ErasedUnit {
    /// Whether this holds no erased item — the freshness probe the archive tests use.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// Add `item`'s erased form after those of the items before it.
    pub(super) fn push(&mut self, item: &Item, erased: ErasedItem) {
        let place = u32::try_from(self.items.len()).expect("a unit's items are counted in a u32");
        for name in item.declared_names() {
            self.declared.insert(*name, place);
        }
        if erased.module.computes() {
            self.computing.push(place);
        }
        self.items.push(erased);
    }

    /// The erased forms, in item order.
    pub(super) fn items(&self) -> &[ErasedItem] {
        &self.items
    }

    /// Where the item declaring `name` sits.
    pub(super) fn place(&self, name: &Global) -> Option<usize> {
        self.declared.get(name).map(|place| *place as usize)
    }

    /// The erased form of the item declaring `name`.
    pub(super) fn find(&self, name: &Global) -> Option<&ErasedItem> {
        self.place(name).map(|place| &self.items[place])
    }

    /// The places of the items that compute at initialization, ascending.
    pub(super) fn computing(&self) -> impl Iterator<Item = usize> {
        self.computing.iter().map(|place| *place as usize)
    }
}

/// The units in scope, as what erasing another reads: the Core each was erased from, and its items' erased forms.
///
/// The two halves describe the same units, in dependency order, and pairing them in one value is the point: as two parameters nothing would say they correspond. The Core is re-seeded so the unit's own re-derived types reduce through it; the erased forms answer how each name is bound.
#[derive(Clone, Copy, Default)]
pub struct Erasures<'a> {
    /// Universes are taken as already validated — `curios-prelude` validates the archive as it restores, which is where untrusted bytes become a `Module`, and the value is immutable from then on.
    cores: &'a [&'a Module],
    erased: &'a [&'a ErasedUnit],
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

        Self { cores, erased }
    }

    pub(super) fn projected_cores(&self) -> Vec<Module> {
        self.cores
            .iter()
            .map(|core| UniverseErased::<Module>::project_validated(core).into_inner())
            .collect()
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
