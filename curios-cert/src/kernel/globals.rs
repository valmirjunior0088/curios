//! What is in scope beyond the walk: top-level definitions, and the nominal registry.
//!
//! This is the one component that exists to answer with something other than the term in hand, which is exactly what a definition store is for. Everything about it is therefore stated rather than assumed: a definition unfolds through a bare occurrence only when it is monomorphic, the registry is data no judgment in this crate types, and a name is written once, every way of writing one refusing a name already held.
//!
//! # Two ways to reach a declaration, and they are not the same question
//!
//! [`Globals::induct_decl`] and [`Globals::struct_decl`] hand back the raw entry. That is correct for the *shared analyses* — positivity walks a declaration's telescopes, inversion reads its `result_sort` — because they analyze the declaration itself and there is no occurrence involved.
//!
//! It is **not** the way a typing rule should reach one. A rule consulting a declaration *for an occurrence* is asking what that occurrence is, and the answer is only meaningful once the occurrence's universe instance and its parameter and index counts have been checked against what the declaration declares. Reading the raw entry and opening its arity at whatever the occurrence carried would let `Telescope::open` abort the walk instead of refusing the item. Those rules go through the checked handle instead.
//!
//! # It is also what a walk starts from
//!
//! [`Globals::of`] builds this from a whole module, which is how an already-certified module reaches a later walk: its definitions at their declared types with their real bodies, its nominal registry, and the certifier's record of its definitions' totality and of its families' polarity vectors. A caller that has one hands it to [`recheck_module_verdicts`](crate::recheck_module_verdicts) and the walk judges what the environment does not already hold. A name is one declaration: a declaration the module carries is the environment's own where its namespace holds the same terms under its name, new where no namespace holds the name, and refused otherwise — which is what the standing questions below answer, and nothing about them identifies an item by where it sits.

use {
    super::Error,
    curios_core::{
        Certification, Free, Global, InductDecl, Item, Module, Polarity, StructDecl, Term,
        Totality, UniverseContext,
    },
    std::collections::{BTreeMap, BTreeSet, HashMap, HashSet},
};

/// A top-level name's entry: what it is, and what it unfolds to if anything.
///
/// The universe context is not decoration. A definition with universe parameters is *not* unfoldable through a bare occurrence, because such an occurrence denotes no particular instance; it reduces only through an [`Instance`](curios_core::Instance) that says which one.
#[derive(Clone, PartialEq)]
struct Definition {
    type_: Term,
    /// `None` for something with a type and no body — a `foreign` declaration, or a name deliberately kept opaque.
    value: Option<Term>,
    universes: UniverseContext,
}

/// What `item` puts in scope: each name it declares with its entry — a `let` at its declared type with its body, a group member at its opened type with its folded selection.
///
/// One function, read where an environment is built and where a declaration is compared with the one an environment holds, so the two cannot record an item two ways.
fn entries(item: &Item) -> Vec<(Free, Definition)> {
    match item {
        Item::Let(definition) => vec![(
            Free::from(&definition.name),
            Definition {
                type_: definition.type_.clone(),
                value: Some(definition.body.clone()),
                universes: definition.universe_context.clone(),
            },
        )],
        Item::Rec(rec) => {
            let universes = rec.group.universe_context();

            item.declared_names()
                .into_iter()
                .enumerate()
                .map(|(member, name)| {
                    (
                        Free::from(name),
                        Definition {
                            type_: rec.group.member_type(member),
                            value: Some(Term::rec_proj(rec.group.clone(), member)),
                            universes: universes.clone(),
                        },
                    )
                })
                .collect()
        }
    }
}

/// Where an environment stands toward a declaration a module carries.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Standing {
    /// No namespace holds its name: the walk judges it.
    New,
    /// The environment holds this very declaration, which the walk that built the environment judged.
    Own,
    /// Its name is held, and it is not the environment's own: a second meaning for the name.
    Redeclared,
}

/// Whether two `induct` registry entries are one declaration to the certifier: equal in everything it reads of one.
///
/// That is the declaration's terms — its universe context, its arity, its constructors and its result sort — and its variance vector, which a walk answers from for a family of a unit in scope. The polarity vector an entry carries is elaboration's record, which no judgment here reads: the one an environment holds beside its entry is the certifier's own, filed by the walk that accepted the entry. The owning module and the representation's privacy are the elaborator's to read. The entry is taken apart in full, so a field added to it is classified here or does not compile.
fn induct_alike(this: &InductDecl, that: &InductDecl) -> bool {
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
fn struct_alike(this: &StructDecl, that: &StructDecl) -> bool {
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

#[derive(Default, Clone)]
pub struct Globals {
    definitions: HashMap<Free, Definition>,
    /// Each entry with the polarity vector the certifier computed for it in the place of the one it was handed in with, which is elaboration's: what strict positivity composes through where a family it analyzes mentions this one. An entry no walk has filed a vector for holds none, and answers `Mixed`.
    inducts: BTreeMap<Global, InductDecl>,
    structs: BTreeMap<Global, StructDecl>,
    /// The entries of every unit mounted here whose record files no vector for them. A walk computes these for itself before it judges anything, as it classifies [`Globals::unclassified`] items.
    unvectored: BTreeSet<Global>,
    /// Concept names only. No judgment in this crate reads a concept's resolution metadata, so what is held is exactly what [`Globals::in_scope`] needs to answer for the namespace.
    concepts: HashSet<Global>,
    /// The names in scope here that are *not* known to terminate, closed transitively already, as the certifier's record of each unit mounted here classifies them.
    ///
    /// Obligations (T) and (V) close over what an erased position reaches, and what it reaches runs out of this environment as readily as out of the module being walked. Held as the non-total set rather than a flag per definition because that is what a walk seeds from.
    ///
    /// It is read off [`Certification`], which only this crate's walk makes, and never off the totality elaboration stamps on a [`Definition`](curios_core::Definition): a verdict arriving here is one this crate reached about those exact terms.
    partial: BTreeSet<Global>,
    /// The items of every unit mounted here with a record that does not cover it — one naming fewer definitions than the unit holds, an empty one included. A walk classifies these for itself, from their terms, beside the non-total set it seeds its closure from.
    unclassified: Vec<Item>,
}

impl Globals {
    /// Everything `module` puts in scope: its definitions at their declared types with their real bodies, and its nominal registry.
    ///
    /// `certification` is the record the certifier's walk over `module` filed with it: where it covers `module`, its classifications are the environment's; where it does not, `module`'s items are held for a walk to classify. Each registry entry takes the vector the record files for it, and one it files none for is held for a walk to compute. An environment built by hand, with no walk behind it, passes an empty record, which covers nothing a unit declares.
    ///
    /// A definition enters here exactly as a refused item enters a walk's environment: at its declared type, with its real body, unjudged. That is deliberate and it is the whole meaning of this type — an environment records what is in scope, and whether the recording is warranted is the caller's question, answered before it ever built one.
    pub fn of(module: &Module, certification: &Certification) -> Self {
        // Asserted rather than reported, as `mount` asserts: an environment is built from a unit a walk certified, which declares each name once, so a second entry under one is the builder's fault.
        let mut definitions = HashMap::new();
        for (name, entry) in module.items.iter().flat_map(entries) {
            assert!(
                definitions.insert(name, entry).is_none(),
                "a unit put in scope declares one name twice"
            );
        }

        // Restricted to `module`'s own definitions rather than every name the record classifies: a baseline's record mounted beside the items an item-level recompile reused also classifies the items it is re-judging, and a stale `Partial` seeded for one of those would outlive the walk that reclassifies it.
        let (partial, unclassified) = match certification.covers(module) {
            true => (
                module
                    .items
                    .iter()
                    .flat_map(Item::definitions)
                    .filter(|definition| {
                        certification.totality(&definition.name) != Some(Totality::Total)
                    })
                    .map(|definition| definition.name)
                    .collect(),
                Vec::new(),
            ),
            false => (BTreeSet::new(), module.items.clone()),
        };

        // The vector an entry carries is left behind: the one held here is the record's, or none.
        let mut unvectored = BTreeSet::new();
        let mut filed = |name: &Global| match certification.polarities(name) {
            Some(vector) => vector.to_vec(),
            None => {
                unvectored.insert(*name);
                Vec::new()
            }
        };
        let inducts = module
            .induct_decls
            .iter()
            .map(|(name, declaration)| {
                let declaration = InductDecl {
                    polarities: filed(name),
                    ..declaration.clone()
                };
                (*name, declaration)
            })
            .collect();
        let structs = module
            .struct_decls
            .iter()
            .map(|(name, declaration)| {
                let declaration = StructDecl {
                    polarities: filed(name),
                    ..declaration.clone()
                };
                (*name, declaration)
            })
            .collect();

        Self {
            definitions,
            inducts,
            structs,
            concepts: module.concepts.keys().cloned().collect(),
            partial,
            unclassified,
            unvectored,
        }
    }

    /// Add everything a second `module` puts in scope, with the record its walk filed, as [`Globals::of`] reads them.
    ///
    /// For a compilation whose scope is several units. Names are disjoint by mount, so this writes each once — asserted in every namespace rather than reported, where `Globals::insert` reports to a walk: a collision here is a driver that mounted one name twice, which is a construction bug and not a program's fault.
    pub fn mount(&mut self, module: &Module, certification: &Certification) {
        const COLLISION: &str = "a mounted unit redeclared a name already in scope";
        let added = Self::of(module, certification);

        for (name, definition) in added.definitions {
            assert!(
                self.definitions.insert(name, definition).is_none(),
                "{COLLISION}"
            );
        }
        for (name, declaration) in added.inducts {
            assert!(
                self.inducts.insert(name, declaration).is_none(),
                "{COLLISION}"
            );
        }
        for (name, declaration) in added.structs {
            assert!(
                self.structs.insert(name, declaration).is_none(),
                "{COLLISION}"
            );
        }
        for name in added.concepts {
            assert!(self.concepts.insert(name), "{COLLISION}");
        }
        self.partial.extend(added.partial);
        self.unclassified.extend(added.unclassified);
        self.unvectored.extend(added.unvectored);
    }

    /// The names in scope here that are not known to terminate. See the field.
    pub(crate) fn partial(&self) -> &BTreeSet<Global> {
        &self.partial
    }

    /// The items in scope here no covering record classifies. See the field.
    pub(crate) fn unclassified(&self) -> &[Item] {
        &self.unclassified
    }

    /// The registry entries in scope here that no record files a vector for, as the two maps strict positivity is handed. See the field.
    pub(super) fn unvectored(
        &self,
    ) -> (BTreeMap<Global, InductDecl>, BTreeMap<Global, StructDecl>) {
        let held = |name: &&Global| self.unvectored.contains(*name);

        (
            self.inducts
                .iter()
                .filter(|(name, _)| held(name))
                .map(|(name, declaration)| (*name, declaration.clone()))
                .collect(),
            self.structs
                .iter()
                .filter(|(name, _)| held(name))
                .map(|(name, declaration)| (*name, declaration.clone()))
                .collect(),
        )
    }

    /// File `vector` as the polarity vector of the registry entry under `name`: what the walk in progress computed for it, written once, as it accepts the entry, and read from then on by every judgment that composes through the family.
    pub(super) fn file_polarities(&mut self, name: &Global, vector: Vec<Polarity>) {
        if let Some(declaration) = self.inducts.get_mut(name) {
            declaration.polarities = vector;
        } else if let Some(declaration) = self.structs.get_mut(name) {
            declaration.polarities = vector;
        }
        self.unvectored.remove(name);
    }

    /// Whether any namespace here holds `name`.
    ///
    /// One predicate across four namespaces, because a `Global` names one declaration. It is never what makes a declaration the environment's own: each namespace answers that for itself, by the terms it holds. It tells a new declaration from one whose name is taken elsewhere, and, of a module nothing of which is redeclared, which names stand for the environment's own.
    pub(crate) fn in_scope(&self, name: &Global) -> bool {
        self.definitions.contains_key(&Free::from(name))
            || self.inducts.contains_key(name)
            || self.structs.contains_key(name)
            || self.concepts.contains(name)
    }

    /// Where `item` stands: the environment's own when a definition entry stands under every name it declares and each equals what the item puts in scope — its type, its body and its universe context, a group member's body being its folded selection; new when no namespace holds any of its names; and redeclared otherwise, which is an unequal entry, a group held in part, or a name held only in another namespace.
    ///
    /// Elaboration's stamps take no part: this holds none, and nothing here reads one. An item declaring nothing is new, so that passing over stays the direction that needs the argument.
    pub(crate) fn standing_of_item(&self, item: &Item) -> Standing {
        let names = item.declared_names();
        let held = names
            .iter()
            .filter(|name| self.definitions.contains_key(&Free::from(**name)))
            .count();

        if held == 0 {
            return match names.into_iter().any(|name| self.in_scope(name)) {
                true => Standing::Redeclared,
                false => Standing::New,
            };
        }

        let own = held == names.len()
            && entries(item)
                .iter()
                .all(|(name, entry)| self.definitions.get(name) == Some(entry));

        match own {
            true => Standing::Own,
            false => Standing::Redeclared,
        }
    }

    /// Where an `induct` registry entry stands under `name`: the environment's own when its `induct` registry holds the same declaration, by everything the certifier reads of one; new when no namespace holds the name; and redeclared otherwise. Elaboration's stamps take no part, as they take none in an item's standing.
    pub(crate) fn standing_of_induct(&self, name: &Global, declaration: &InductDecl) -> Standing {
        self.standing_in(&self.inducts, name, declaration, induct_alike)
    }

    /// [`Globals::standing_of_induct`] for a `struct` registry entry.
    pub(crate) fn standing_of_struct(&self, name: &Global, declaration: &StructDecl) -> Standing {
        self.standing_in(&self.structs, name, declaration, struct_alike)
    }

    /// Where `declaration` stands under `name` in `registry`, the one of its kind, `alike` saying whether two of its entries are one declaration: a registry answers for its own entries and for no other namespace's.
    fn standing_in<D>(
        &self,
        registry: &BTreeMap<Global, D>,
        name: &Global,
        declaration: &D,
        alike: fn(&D, &D) -> bool,
    ) -> Standing {
        match registry.get(name) {
            Some(held) if alike(held, declaration) => Standing::Own,
            Some(_) => Standing::Redeclared,
            None if self.in_scope(name) => Standing::Redeclared,
            None => Standing::New,
        }
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
        if self.definitions.contains_key(name) {
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

    /// Register an `induct` registry entry under `name`, or refuse it where one already stands there: written once, as [`Globals::insert`] writes a definition.
    ///
    /// The polarity vector the entry carries is left at the door. It is elaboration's, and the one held here is the certifier's own, which the walk files once it has computed it ([`Globals::file_polarities`]); until then the family answers `Mixed`.
    pub(super) fn declare_induct(
        &mut self,
        name: &Global,
        declaration: &InductDecl,
    ) -> Result<(), Error> {
        if self.inducts.contains_key(name) {
            return Err(Error::Redeclared(Free::from(name)));
        }

        self.inducts.insert(
            *name,
            InductDecl {
                polarities: Vec::new(),
                ..declaration.clone()
            },
        );

        Ok(())
    }

    /// [`Globals::declare_induct`] for a `struct` registry entry.
    pub(super) fn declare_struct(
        &mut self,
        name: &Global,
        declaration: &StructDecl,
    ) -> Result<(), Error> {
        if self.structs.contains_key(name) {
            return Err(Error::Redeclared(Free::from(name)));
        }

        self.structs.insert(
            *name,
            StructDecl {
                polarities: Vec::new(),
                ..declaration.clone()
            },
        );

        Ok(())
    }

    /// An `induct` registry entry, as data. See the module documentation on why a typing rule wants the checked handle instead.
    pub(super) fn induct_decl(&self, name: &Global) -> Option<&InductDecl> {
        self.inducts.get(name)
    }

    /// A `struct` registry entry, as data. See the module documentation on why a typing rule wants the checked handle instead.
    pub(super) fn struct_decl(&self, name: &Global) -> Option<&StructDecl> {
        self.structs.get(name)
    }

    /// The universe scheme `name` was generalized under, for a use that states its own instance.
    pub(super) fn scheme_of(&self, name: &Free) -> Option<(&Term, &UniverseContext)> {
        self.definitions
            .get(name)
            .map(|entry| (&entry.type_, &entry.universes))
    }

    /// What `name` unfolds to through a bare occurrence.
    ///
    /// A definition with universe parameters is withheld: see [`Definition`].
    pub(super) fn value(&self, name: &Free) -> Option<&Term> {
        self.definitions
            .get(name)
            .filter(|definition| definition.universes.parameter_count == 0)
            .and_then(|definition| definition.value.as_ref())
    }

    /// What `name` unfolds to at a *stated* universe instance, which is the one position a polymorphic definition may be unfolded from.
    pub(super) fn value_at(&self, name: &Free) -> Option<&Term> {
        self.definitions
            .get(name)
            .and_then(|definition| definition.value.as_ref())
    }
}
