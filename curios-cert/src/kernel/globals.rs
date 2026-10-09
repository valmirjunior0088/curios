//! What is in scope beyond the walk: top-level definitions, and the nominal registry.
//!
//! This is the one component that exists to answer with something other than the term in hand, which is exactly what a definition store is for. Everything about it is therefore stated rather than assumed: a definition unfolds through a bare occurrence only when it is monomorphic, the registry is data no judgment in this crate types, and a name is written once, every way of writing one refusing a name already held.
//!
//! # Two ways to reach a declaration, and they are not the same question
//!
//! [`Environment::induct_decl`] and [`Environment::struct_decl`] hand back the raw entry. That is correct for the *shared analyses* — positivity walks a declaration's telescopes, inversion reads its `result_sort` — because they analyze the declaration itself and there is no occurrence involved.
//!
//! It is **not** the way a typing rule should reach one. A rule consulting a declaration *for an occurrence* is asking what that occurrence is, and the answer is only meaningful once the occurrence's universe instance and its parameter and index counts have been checked against what the declaration declares. Reading the raw entry and opening its arity at whatever the occurrence carried would let `Telescope::open` abort the walk instead of refusing the item. Those rules go through the checked handle instead.
//!
//! # What a walk starts from is read where it stands
//!
//! [`Globals`] is the units in scope, which is how an already-certified module reaches a later walk: its definitions at their declared types with their real bodies, the entries its type formers carry, and the certifier's record of its definitions' totality and of its families' polarity vectors. Nothing of a unit is brought in. A name is answered by reference into the unit that holds it, by the places the unit keeps for its names (`curios_core::Places`), so what a walk costs of its scope is what its judgments read. A caller that has units hands them to [`recheck_module_verdicts`](crate::recheck_module_verdicts) and the walk judges the module it is handed against them. A name is one declaration: a module is refused for a name a unit here declares, whatever it declares under it (`Globals::in_scope`), and nothing about that question identifies an item by where it sits.
//!
//! # What a walk reads is three layers
//!
//! An [`Environment`] is what one walk reads a top-level name from: what the walk has written itself, then the declarations of a baseline whose verdict it adopted, each read where the baseline holds it, then the units it was seeded with. Only the first is written, and a write is refused for a name any of the three holds.

use {
    super::Error,
    crate::Baseline,
    curios_core::{
        Certification, Free, Global, InductDecl, Item, Module, Place, Places, Polarity, StructDecl,
        Term, Totality, UniverseContext,
    },
    std::collections::{BTreeMap, HashMap, HashSet},
};

/// A top-level name's entry as a judgment reads it: what it is, and what it unfolds to if anything.
///
/// The universe context is not decoration. A definition with universe parameters is *not* unfoldable through a bare occurrence, because such an occurrence denotes no particular instance; it reduces only through an [`Instance`](curios_core::Instance) that says which one.
#[derive(Clone, Copy)]
pub(crate) struct Held<'a> {
    pub(crate) type_: &'a Term,
    /// `None` for something with a type and no body: a definition marked a proof ([`curios_core::Definition::proof`]), whose body its own check read and no judgment after it does, and a fixture's opaque head.
    pub(crate) value: Option<&'a Term>,
    pub(crate) universes: &'a UniverseContext,
}

/// What `module` holds `name` at, where it defines it, by the place its unit keeps for the name: a `let` at its declared type with its body, a group's member at its opened type with its folded selection, and either with nothing to unfold to where it is marked a proof.
///
/// One function, read wherever a finished module answers for a name — a unit in scope, a baseline — so no two readers can hold an item two ways.
pub(crate) fn held<'a>(module: &'a Module, places: &'a Places, name: &Global) -> Option<Held<'a>> {
    match places.place(name)? {
        Place::Let(item) => match &module.items[item as usize] {
            Item::Let(definition) => Some(Held {
                type_: &definition.type_,
                value: (!definition.proof).then_some(&definition.body),
                universes: &definition.universe_context,
            }),
            Item::Rec(_) => unreachable!("a definition's place is a definition's"),
        },
        Place::Member { item, member, slot } => match &module.items[item as usize] {
            Item::Rec(rec) => {
                let (type_, body) = places.member(rec, member, slot);

                Some(Held {
                    type_,
                    value: (!rec.definitions[member as usize].proof).then_some(body),
                    universes: rec.group.universe_context(),
                })
            }
            Item::Let(_) => unreachable!("a member's place is a group's"),
        },
    }
}

/// Whether two `induct` registry entries are one declaration to the certifier: equal in everything it reads of one.
///
/// That is the declaration's terms — its universe context, its arity, its constructors and its result sort — and its variance vector, which a walk answers from for a family of a unit in scope. The polarity vector an entry carries is elaboration's record, which no judgment here reads: the one an environment answers a family's polarity from is the certifier's own, filed by the walk that accepted the entry. The owning module and the representation's privacy are the elaborator's to read. The entry is taken apart in full, so a field added to it is classified here or does not compile.
pub(crate) fn induct_alike(this: &InductDecl, that: &InductDecl) -> bool {
    let InductDecl {
        universe_context,
        arity,
        constructors,
        result_sort,
        module: _,
        rep_public: _,
        polarities: _,
        variances,
    } = this;

    *universe_context == that.universe_context
        && *arity == that.arity
        && *constructors == that.constructors
        && *result_sort == that.result_sort
        && *variances == that.variances
}

/// [`induct_alike`] for two `struct` registry entries, whose fields its arity ends in.
pub(crate) fn struct_alike(this: &StructDecl, that: &StructDecl) -> bool {
    let StructDecl {
        universe_context,
        arity,
        result_sort,
        module: _,
        rep_public: _,
        polarities: _,
        variances,
    } = this;

    *universe_context == that.universe_context
        && *arity == that.arity
        && *result_sort == that.result_sort
        && *variances == that.variances
}

/// One unit in scope, read where it stands: its module, the places its unit keeps for the module's names, and the record the certifier's walk over it filed.
#[derive(Clone, Copy)]
struct Mounted<'s> {
    module: &'s Module,
    places: &'s Places,
    certification: &'s Certification,
    /// Whether `certification` covers `module`. Where it does, its classifications are the environment's; where it does not — one naming fewer definitions than the unit holds, an empty one included — a walk classifies the unit's items for itself, from their terms.
    covered: bool,
    /// Whether the unit is the compiler's own, of which no read is recorded ([`Globals::standing`]).
    fixed: bool,
    /// Whether the unit is other than the one a baseline's walk read ([`Globals::standing`]).
    moved: bool,
}

impl<'s> Mounted<'s> {
    fn held(&self, name: &Global) -> Option<Held<'s>> {
        held(self.module, self.places, name)
    }

    /// Whether this unit declares `name`. An entry is held by its type former, so the names a unit declares are its items'.
    fn holds(&self, name: &Global) -> bool {
        self.places.place(name).is_some()
    }

    /// How many items this unit holds: what says which of two units is the smaller to ask the other of.
    fn size(&self) -> usize {
        self.module.items.len()
    }

    /// Whether this unit and `other` hold a name in common, asked of the smaller of the two.
    fn collides(&self, other: &Self) -> bool {
        let (smaller, larger) = match self.size() <= other.size() {
            true => (self, other),
            false => (other, self),
        };
        let items = &smaller.module.items;
        (0..items.len()).any(|place| {
            items
                .names(place)
                .into_iter()
                .any(|name| larger.holds(name))
        })
    }

    /// The polarity vector this unit's record files for the registry entry it holds under `name`.
    fn polarities(&self, name: &Global) -> Option<&'s [Polarity]> {
        self.certification
            .polarities(name)
            .filter(|_| self.module.nominal(self.places, name).is_some())
    }
}

/// The units in scope before a walk, each read where it stands.
#[derive(Default)]
pub struct Globals<'s> {
    mounted: Vec<Mounted<'s>>,
}

impl<'s> Globals<'s> {
    /// Put the unit `module` in scope — its definitions at their declared types with their real bodies, a proof's withheld, and the entries its type formers carry — read by the `places` its unit keeps for it, with the record the certifier's walk over it filed.
    ///
    /// Where `certification` covers `module`, its classifications are the environment's; where it does not, `module`'s items are a walk's to classify. Each registry entry answers its polarity from the vector the record files for it, and one it files none for is a walk's to compute. An environment built by hand, with no walk behind it, passes an empty record, which covers nothing a unit declares.
    ///
    /// A definition stands here exactly as a refused item stands in a walk's environment: at its declared type, with its real body, unjudged. That is deliberate and it is the whole meaning of this type — an environment records what is in scope, and whether the recording is warranted is the caller's question, answered before it ever built one.
    ///
    /// Names are disjoint by mount, so no two units hold one — asserted rather than reported, where a walk's own write of a held name is reported to it: a collision here is a driver that mounted one name twice, which is a construction bug and not a program's fault.
    pub fn mount(
        &mut self,
        module: &'s Module,
        places: &'s Places,
        certification: &'s Certification,
    ) {
        let mounted = Mounted {
            module,
            places,
            certification,
            covered: certification.covers(module),
            fixed: false,
            moved: false,
        };
        assert!(
            !self
                .mounted
                .iter()
                .any(|earlier| earlier.collides(&mounted)),
            "a mounted unit redeclared a name already in scope"
        );
        self.mounted.push(mounted);
    }

    /// Say of each unit mounted so far, in the order they were mounted, whether it is the compiler's own (`fixed`), and whether it is other than the unit the walk of a baseline read (`moved`): one whose contents changed since, or one that walk was never handed.
    ///
    /// **What a walk over a baseline does with each.** A read of a unit that did not move stands as the baseline's record has it. A read of one that moved stands where the part read digests now to what that record holds of it, by the certifier's own digest of the terms in its scope and of its verdict on them (`Baseline`). Nothing is recorded of a fixed unit: a baseline is offered by the compiler that made it, so a constant of the compiler is the one every baseline's walk read.
    pub fn standing(&mut self, fixed: &[bool], moved: &[bool]) {
        assert!(
            fixed.len() == self.mounted.len() && moved.len() == self.mounted.len(),
            "what is fixed and what moved are said of each unit mounted"
        );
        for (mounted, (fixed, moved)) in self.mounted.iter_mut().zip(fixed.iter().zip(moved)) {
            mounted.fixed = *fixed;
            mounted.moved = *moved;
        }
    }

    /// Whether what a baseline's walk read of `name`, which a unit here declares, stands unasked: the unit is the one that walk read.
    pub(crate) fn unmoved(&self, name: &Global) -> bool {
        self.mounted
            .iter()
            .find(|mounted| mounted.holds(name))
            .is_some_and(|mounted| !mounted.moved)
    }

    /// Whether a read of `name`, which a unit here declares, is one a walk records: every one but a read of a fixed unit.
    #[cfg(feature = "archive")]
    pub(crate) fn recorded(&self, name: &Global) -> bool {
        self.mounted
            .iter()
            .find(|mounted| mounted.holds(name))
            .is_some_and(|mounted| !mounted.fixed)
    }

    /// Whether `name` is a definition in scope here that is *not* known to terminate, closed transitively already, as the certifier's record of the unit that holds it classifies it.
    ///
    /// Obligations (T) and (V) close over what an erased position reaches, and what it reaches runs out of this environment as readily as out of the module being walked.
    ///
    /// It is read off [`Certification`], which only this crate's walk makes, and never off the totality elaboration stamps on a [`Definition`](curios_core::Definition): a verdict arriving here is one this crate reached about those exact terms. A record answers for the unit it was filed with and for no name beside it, and only where it covers that unit.
    pub(crate) fn partial(&self, name: &Global) -> bool {
        self.mounted.iter().any(|mounted| {
            mounted.covered
                && mounted.certification.totality(name) == Some(Totality::Partial)
                && mounted.places.place(name).is_some()
        })
    }

    /// The items of every unit in scope here with a record that does not cover it — one naming fewer definitions than the unit holds, an empty one included. A walk classifies these for itself, from their terms.
    pub(crate) fn unclassified(&self) -> impl Iterator<Item = &'s Item> + '_ {
        self.mounted
            .iter()
            .filter(|mounted| !mounted.covered)
            .flat_map(|mounted| &mounted.module.items)
    }

    /// The registry entries in scope here that no record files a vector for, as the two maps strict positivity is handed: a walk computes these for itself before it judges anything.
    fn unvectored(&self) -> (BTreeMap<Global, InductDecl>, BTreeMap<Global, StructDecl>) {
        let mut inducts = BTreeMap::new();
        let mut structs = BTreeMap::new();
        for mounted in &self.mounted {
            // Which names declare a nominal is what each place of the unit says, so the record is asked of each with no entry read, and an entry is read where the record files no vector for it.
            let unfiled = mounted
                .module
                .formers()
                .into_iter()
                .filter(|(name, _)| mounted.certification.polarities(name).is_none());
            for (name, _) in unfiled {
                let Some(nominal) = mounted.module.nominal(mounted.places, &name) else {
                    continue;
                };
                if let Some(declaration) = nominal.inductive() {
                    inducts.insert(name, declaration.clone());
                }
                if let Some(record) = nominal.record() {
                    structs.insert(name, record.clone());
                }
            }
        }

        (inducts, structs)
    }

    /// Whether a unit here declares `name`.
    ///
    /// A `Global` names one declaration: a module that declares a name a unit in scope does is refused for it, whatever it declares there.
    pub(crate) fn in_scope(&self, name: &Global) -> bool {
        self.mounted.iter().any(|mounted| mounted.holds(name))
    }

    /// What the unit that defines `name` holds it at.
    pub(crate) fn held(&self, name: &Global) -> Option<Held<'s>> {
        self.mounted.iter().find_map(|mounted| mounted.held(name))
    }

    pub(crate) fn induct_decl(&self, name: &Global) -> Option<&'s InductDecl> {
        self.mounted
            .iter()
            .find_map(|mounted| mounted.module.induct_decl(mounted.places, name))
    }

    pub(crate) fn struct_decl(&self, name: &Global) -> Option<&'s StructDecl> {
        self.mounted
            .iter()
            .find_map(|mounted| mounted.module.struct_decl(mounted.places, name))
    }

    /// The polarity vector the record of the unit holding the registry entry `name` files for it.
    pub(crate) fn polarities(&self, name: &Global) -> Option<&'s [Polarity]> {
        self.mounted
            .iter()
            .find_map(|mounted| mounted.polarities(name))
    }
}

/// A top-level name a walk wrote: what it is, and what it unfolds to if anything.
struct Definition {
    type_: Term,
    value: Option<Term>,
    universes: UniverseContext,
}

/// What one walk reads a top-level name from: what it has written, then the declarations of a baseline it adopted, then the units it was seeded with.
#[derive(Default)]
pub(super) struct Environment<'s> {
    /// The units in scope when the walk began, which it reads and never writes.
    scope: Option<&'s Globals<'s>>,
    /// The baseline whose verdicts the walk adopts, where it walks over one.
    baseline: Option<Baseline<'s>>,
    /// The names whose verdict the walk adopted: each is read where the baseline holds it, which is the declaration the walk was handed under that name, term for term.
    adopted: HashSet<Global>,
    definitions: HashMap<Free, Definition>,
    inducts: BTreeMap<Global, InductDecl>,
    structs: BTreeMap<Global, StructDecl>,
    /// The polarity vector the walk filed for each registry entry it accepted or adopted, and for each entry of its scope no record files one for: what strict positivity composes through where a family it analyzes mentions this one. The vector an entry carries is elaboration's, which nothing here reads; an entry with no vector filed answers `Mixed`.
    vectors: BTreeMap<Global, Vec<Polarity>>,
}

impl<'s> Environment<'s> {
    /// Start from `scope` — the units an earlier walk established — rather than from nothing. Nothing of it is brought in.
    pub(super) fn seed(&mut self, scope: &'s Globals<'s>) {
        self.scope = Some(scope);
    }

    /// Read the declarations this walk adopts where `baseline` holds them.
    pub(super) fn over(&mut self, baseline: Baseline<'s>) {
        self.baseline = Some(baseline);
    }

    /// What `name` is held at: as the walk wrote it, as the baseline it adopted it from holds it, or as a unit in scope does.
    fn held(&self, name: &Free) -> Option<Held<'_>> {
        if let Some(definition) = self.definitions.get(name) {
            return Some(Held {
                type_: &definition.type_,
                value: definition.value.as_ref(),
                universes: &definition.universes,
            });
        }

        let name = name.as_global()?;
        let adopted = self
            .baseline
            .filter(|_| self.adopted.contains(name))
            .and_then(|baseline| held(baseline.module, baseline.places, name));

        adopted.or_else(|| self.scope.and_then(|scope| scope.held(name)))
    }

    /// Record `name` at `type_`, generalized over `universes`, with `value` as its body where it has one — or refuse it, where a definition already stands under `name`.
    ///
    /// **Written once.** A judgment reads what a name means when it is made, and a remembered reduct is a function of the definitions it unfolded. An entry written over a held name would leave both stating what the environment no longer holds, so the write is refused and neither is ever revisited: the memos, a sibling component, need to know nothing of this one.
    pub(super) fn insert(
        &mut self,
        name: &Free,
        type_: &Term,
        value: Option<&Term>,
        universes: &UniverseContext,
    ) -> Result<(), Error> {
        if self.held(name).is_some() {
            return Err(Error::Redeclared(*name));
        }

        self.definitions.insert(
            *name,
            Definition {
                type_: type_.clone(),
                value: value.cloned(),
                universes: universes.clone(),
            },
        );

        Ok(())
    }

    /// Take `name` as the baseline holds it — or refuse it, where a definition already stands under `name` or the baseline holds none: written once, as [`Environment::insert`] writes.
    pub(super) fn adopt(&mut self, name: &Global) -> Result<(), Error> {
        let held = self
            .baseline
            .and_then(|baseline| held(baseline.module, baseline.places, name));
        if held.is_none() || self.held(&Free::from(name)).is_some() {
            return Err(Error::Redeclared(Free::from(name)));
        }

        self.adopted.insert(*name);

        Ok(())
    }

    /// Register an `induct` registry entry under `name`, or refuse it where one already stands there: written once, as [`Environment::insert`] writes a definition.
    pub(super) fn declare_induct(
        &mut self,
        name: &Global,
        declaration: &InductDecl,
    ) -> Result<(), Error> {
        if self.induct_decl(name).is_some() {
            return Err(Error::Redeclared(Free::from(name)));
        }

        self.inducts.insert(*name, declaration.clone());

        Ok(())
    }

    /// [`Environment::declare_induct`] for a `struct` registry entry.
    pub(super) fn declare_struct(
        &mut self,
        name: &Global,
        declaration: &StructDecl,
    ) -> Result<(), Error> {
        if self.struct_decl(name).is_some() {
            return Err(Error::Redeclared(Free::from(name)));
        }

        self.structs.insert(*name, declaration.clone());

        Ok(())
    }

    /// The registry entries of the scope this walk was seeded with that no record files a polarity vector for and the walk has filed none for either.
    pub(super) fn unvectored(
        &self,
    ) -> (BTreeMap<Global, InductDecl>, BTreeMap<Global, StructDecl>) {
        let (mut inducts, mut structs) = self.scope.map(Globals::unvectored).unwrap_or_default();
        inducts.retain(|name, _| !self.vectors.contains_key(name));
        structs.retain(|name, _| !self.vectors.contains_key(name));

        (inducts, structs)
    }

    /// File `vector` as the polarity vector of the registry entry under `name`: what the walk in progress computed for it, or adopted, written as it accepts the entry and read from then on by every judgment that composes through the family.
    pub(super) fn file_polarities(&mut self, name: &Global, vector: Vec<Polarity>) {
        self.vectors.insert(*name, vector);
    }

    /// How the family `name` uses its parameter at `index`, by the vector this walk filed for it or the record of its unit files: [`Polarity::Mixed`] where neither holds one, and past the end of one.
    pub(super) fn polarity(&self, name: &Global, index: usize) -> Polarity {
        self.vectors
            .get(name)
            .map(Vec::as_slice)
            .or_else(|| self.scope.and_then(|scope| scope.polarities(name)))
            .and_then(|vector| vector.get(index).copied())
            .unwrap_or(Polarity::Mixed)
    }

    /// An `induct` registry entry, as data. See the module documentation on why a typing rule wants the checked handle instead.
    pub(super) fn induct_decl(&self, name: &Global) -> Option<&InductDecl> {
        self.inducts
            .get(name)
            .or_else(|| self.scope.and_then(|scope| scope.induct_decl(name)))
    }

    /// A `struct` registry entry, as data. See the module documentation on why a typing rule wants the checked handle instead.
    pub(super) fn struct_decl(&self, name: &Global) -> Option<&StructDecl> {
        self.structs
            .get(name)
            .or_else(|| self.scope.and_then(|scope| scope.struct_decl(name)))
    }

    /// The universe scheme `name` was generalized under, for a use that states its own instance.
    pub(super) fn scheme_of(&self, name: &Free) -> Option<(&Term, &UniverseContext)> {
        self.held(name).map(|held| (held.type_, held.universes))
    }

    /// What `name` unfolds to through a bare occurrence.
    ///
    /// A definition with universe parameters is withheld: see [`Held`].
    pub(super) fn value(&self, name: &Free) -> Option<&Term> {
        self.held(name)
            .filter(|held| held.universes.parameter_count == 0)
            .and_then(|held| held.value)
    }

    /// What `name` unfolds to at a *stated* universe instance, which is the one position a polymorphic definition may be unfolded from.
    pub(super) fn value_at(&self, name: &Free) -> Option<&Term> {
        self.held(name).and_then(|held| held.value)
    }
}
