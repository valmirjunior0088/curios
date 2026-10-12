//! What elaboration starts from: the units already elaborated, read where they stand.
//!
//! Elaboration of a compilation unit happens against everything already in scope, and that scope is not a *part* of the unit: it is a set of finished modules. [`Established`] is those modules as a declaration reads them — a name's type, its body, a registry entry, the witness at a key — each answered by reference into the unit that holds it (`InScope`).
//!
//! **Nothing of a scope is brought in.** A context is handed the scope and reads through to it where its own stores miss, so what a compile costs of its scope is what its declarations read: no definition is defined again, no registry entry cloned, no witness registered again by reducing a signature its own unit already keyed. A unit in scope answers for its names by the places its own unit keeps for it (`curios_core::Places`), made once for as long as the unit is held.
//!
//! **A baseline is one more layer.** A unit compiled over a baseline reads the declarations it keeps where the baseline holds them, above its scope and beneath its own stores. Which it keeps is decided as its declarations are asked for, so the layer answers for none of them at first and for each from where the walk keeps it ([`Established::keeping`], [`Established::keep`]).
//!
//! **A witness in scope is found by its name**, which states its concept and its key (`curios_core::WitnessId`): a unit's witnesses are an ordered set of names, so the witnesses at a concept and a key are the names from there on.

use {
    super::{Defined, Error, InScope, Question, part_digest},
    curios_core::{
        ConceptDecl, Global, InductDecl, Item, Module, Place, Places, RecGroup, RecItem,
        StructDecl, WitnessId, WitnessKey,
    },
    curios_utilities::Qualifier,
    std::{cell::RefCell, collections::HashMap},
};

/// The places of one unit's names: its unit's own, or made here for a module nothing keeps them with.
#[derive(Debug)]
enum Placed<'a> {
    Kept(&'a Places),
    Made(Places),
}

/// One unit in scope, answering for its names.
#[derive(Debug)]
struct Standing<'a> {
    module: &'a Module,
    places: Placed<'a>,
    /// Which of its items this unit answers for, by each one's place among them: the items of a baseline that a walk has kept so far. `None` for a unit in scope, which answers for every name it holds.
    kept: Option<RefCell<Vec<bool>>>,
    /// Whether this unit is other than the one the baseline was compiled after ([`Established::after`]).
    moved: bool,
    /// Whether this unit is the compiler's own ([`Established::fixed`]).
    fixed: bool,
}

impl<'a> Standing<'a> {
    fn places(&self) -> &Places {
        match &self.places {
            Placed::Kept(places) => places,
            Placed::Made(places) => places,
        }
    }

    /// Whether this unit answers for `name`, where it holds it.
    fn answers(&self, name: &Global) -> bool {
        let Some(kept) = &self.kept else {
            return true;
        };

        self.places()
            .place(name)
            .is_some_and(|place| kept.borrow()[place.item()])
    }

    /// Answer for the item declaring `name` from here on.
    fn keep(&self, name: &Global) {
        if let (Some(kept), Some(place)) = (&self.kept, self.places().place(name)) {
            kept.borrow_mut()[place.item()] = true;
        }
    }

    /// The group the item at `item` is.
    fn group(&self, item: usize) -> &'a RecItem {
        match &self.module.items[item] {
            Item::Rec(rec) => rec,
            Item::Let(_) => unreachable!("a member's place is a group's"),
        }
    }

    fn definition(&self, name: &Global) -> Option<Defined<'_>> {
        if !self.answers(name) {
            return None;
        }

        match self.places().place(name)? {
            Place::Let(item) => {
                let Item::Let(definition) = &self.module.items[item as usize] else {
                    unreachable!("a definition's place is a definition's");
                };

                Some(Defined {
                    type_: &definition.type_,
                    body: (!definition.proof).then_some(&definition.body),
                    kind: &definition.kind,
                    universe_context: &definition.universe_context,
                    totality: definition.totality,
                    variances: &definition.variances,
                    island: definition.island,
                })
            }
            Place::Member { item, member, slot } => {
                let rec = self.group(item as usize);
                let definition = &rec.definitions[member as usize];
                let (type_, body) = self.places().member(rec, member, slot);

                Some(Defined {
                    type_,
                    body: (!definition.proof).then_some(body),
                    kind: &definition.kind,
                    universe_context: rec.group.universe_context(),
                    totality: definition.totality,
                    // A group's member files no vector where it is published, so it answers none here.
                    variances: &[],
                    island: definition.island,
                })
            }
        }
    }

    fn induct_decl(&self, name: &Global) -> Option<&'a InductDecl> {
        self.module
            .induct_decl(self.places(), name)
            .filter(|_| self.answers(name))
    }

    fn struct_decl(&self, name: &Global) -> Option<&'a StructDecl> {
        self.module
            .struct_decl(self.places(), name)
            .filter(|_| self.answers(name))
    }

    fn concept(&self, name: &Global) -> Option<&'a ConceptDecl> {
        self.module
            .concept(self.places(), name)
            .filter(|_| self.answers(name))
    }

    /// The witnesses this unit answers for, in the order of their names: by concept, then by key.
    fn witnesses(&self) -> impl Iterator<Item = &'a Global> + '_ {
        self.module
            .witnesses
            .iter()
            .filter(|name| self.answers(name))
    }

    /// The first witness this unit answers for at `(concept, key)`: its names are ordered by concept and key, so the ones declared there are the names from there on.
    fn witness(&self, concept: &Qualifier, key: &WitnessKey) -> Option<Global> {
        let from = Global::Witness(WitnessId::Spelled {
            concept: *concept,
            key: *key,
            module: Qualifier::empty(),
            place: 0,
        });

        self.module
            .witnesses
            .range(from..)
            .take_while(|name| spelled_at(name) == Some((Global::Authored(*concept), *key)))
            .find(|name| self.answers(name))
            .copied()
    }

    /// Each group this unit answers for, with its members' names in member order.
    fn groups(&self) -> impl Iterator<Item = (RecGroup, Vec<Global>)> + '_ {
        let items = &self.module.items;

        (0..items.len())
            .filter(|place| items.is_group(*place))
            .filter_map(|place| match &items[place] {
                Item::Rec(rec) => {
                    let names = member_names(rec);

                    names
                        .iter()
                        .all(|name| self.answers(name))
                        .then(|| (rec.group.clone(), names))
                }
                Item::Let(_) => None,
            })
    }

    fn member_names(&self, group: &RecGroup) -> Option<Vec<Global>> {
        let names = member_names(self.group(self.places().group(self.module, group)?));

        names.iter().all(|name| self.answers(name)).then_some(names)
    }
}

/// The names `rec`'s members are defined under, in member order.
fn member_names(rec: &RecItem) -> Vec<Global> {
    rec.definitions
        .iter()
        .map(|definition| definition.name)
        .collect()
}

/// The concept and the key a witness's name states.
fn spelled_at(name: &Global) -> Option<(Global, WitnessKey)> {
    match name {
        Global::Witness(id) => id.spelled(),
        Global::Authored(_) => None,
    }
}

/// The modules already elaborated, as scope for elaborating another.
#[derive(Debug, Default)]
pub struct Established<'a> {
    /// The units already elaborated, in dependency order, and last, where a unit is compiled over one, the baseline whose declarations it keeps. Empty for a from-scratch elaboration, where the unit defines every name it mentions.
    units: Vec<Standing<'a>>,
    /// The units in scope, in dependency order: never the baseline, which is the unit being compiled.
    modules: Vec<&'a Module>,
    /// Whether any unit in scope is other than the one the baseline was compiled after.
    moved: bool,
}

impl<'a> Established<'a> {
    /// Nothing in scope: the unit is elaborated whole, against itself alone.
    pub fn nothing() -> Self {
        Self::default()
    }

    /// Everything `modules` established, in dependency order, as scope: each read by places made here, for a module no unit holds.
    pub fn over(modules: &[&'a Module]) -> Self {
        Self {
            units: modules
                .iter()
                .map(|module| Standing {
                    module,
                    places: Placed::Made(Places::of(module)),
                    kept: None,
                    moved: false,
                    fixed: false,
                })
                .collect(),
            modules: modules.to_vec(),
            moved: false,
        }
    }

    /// Everything `units` established, in dependency order, as scope: each module read by the places its unit keeps for it.
    pub fn placed(units: &[(&'a Module, &'a Places)]) -> Self {
        Self {
            units: units
                .iter()
                .map(|(module, places)| Standing {
                    module,
                    places: Placed::Kept(places),
                    kept: None,
                    moved: false,
                    fixed: false,
                })
                .collect(),
            modules: units.iter().map(|(module, _)| *module).collect(),
            moved: false,
        }
    }

    /// This scope with `fixed` saying of each unit, in the order they stand, whether it is the compiler's own: a constant of the executable, which is one thing under one compiler. Asked before a baseline is laid over the scope.
    ///
    /// **Nothing is recorded of a read of a fixed unit.** A baseline is offered by the compiler that made it, a store's slot and a session's entry naming it alike, so a fixed unit is the one every baseline was compiled after and no read of it is asked again.
    pub fn fixed(mut self, fixed: &[bool]) -> Self {
        assert_eq!(
            fixed.len(),
            self.units.len(),
            "what is fixed is said of each unit in scope, before a baseline is laid over them"
        );
        for (unit, fixed) in self.units.iter_mut().zip(fixed) {
            unit.fixed = *fixed;
        }

        self
    }

    /// This scope as one a baseline was not compiled after, unit for unit: `moved` says of each unit, in the order they stand, whether it is other than the one the baseline was compiled after — one whose contents changed since, or one the baseline's compile was never handed. Asked before the baseline is laid over the scope ([`Established::keeping`]).
    ///
    /// **A unit that did not move answers for every read of it.** What a kept declaration asked of one stands unasked. What it asked of a unit that moved, or of a name no unit in scope declares where one moved, is asked again by its digest (`Foreign`).
    pub fn after(mut self, moved: &[bool]) -> Self {
        assert_eq!(
            moved.len(),
            self.units.len(),
            "what moved is said of each unit in scope, before a baseline is laid over them"
        );
        for (unit, moved) in self.units.iter_mut().zip(moved) {
            unit.moved = *moved;
        }
        self.moved = moved.contains(&true);

        self
    }

    /// This scope with `baseline` above it: the unit being compiled, as an earlier text of it elaborated, answering for the declarations a walk keeps of it and for none until it keeps one (`Established::keep`). Nothing of it is brought in: a kept declaration is read where the baseline holds it, as a scope's is.
    pub fn keeping(mut self, baseline: &'a Module, places: &'a Places) -> Self {
        self.units.push(Standing {
            module: baseline,
            places: Placed::Kept(places),
            kept: Some(RefCell::new(vec![false; baseline.items.len()])),
            moved: false,
            fixed: false,
        });

        self
    }

    /// Keep the baseline's declaration declaring `names`: the baseline answers for them from here on. Asked by the walk, where it finds that everything the declaration read stands.
    pub(crate) fn keep(&self, names: &[&Global]) {
        for unit in &self.units {
            for name in names {
                unit.keep(name);
            }
        }
    }

    /// The modules in scope, in dependency order.
    pub fn modules(&self) -> &[&'a Module] {
        &self.modules
    }

    /// The unit in scope that declares `name`: never the baseline, which is the unit being compiled.
    fn declaring(&self, name: &Global) -> Option<&Standing<'a>> {
        self.units
            .iter()
            .filter(|unit| unit.kept.is_none())
            .find(|unit| unit.places().place(name).is_some())
    }

    /// Whether every unit in scope is the one the baseline was compiled after.
    pub(crate) fn settled(&self) -> bool {
        !self.moved
    }

    /// Whether what a baseline read of `name`, which the unit being compiled does not declare, stands unasked: every unit in scope is the one the baseline was compiled after, or the one that declares `name` is. A name no unit declares stands only in the first case, since a unit that moved may have declared it.
    pub(crate) fn unmoved(&self, name: &Global) -> bool {
        !self.moved || self.declaring(name).is_some_and(|unit| !unit.moved)
    }

    /// Whether a read of `name`, which the unit being compiled does not declare, is one to record: every one but a read of a fixed unit's declaration.
    pub(crate) fn recorded(&self, name: &Global) -> bool {
        self.declaring(name).is_none_or(|unit| !unit.fixed)
    }

    /// The digest of the part `question` asks, of the declaration a unit in scope holds under its name ([`part_digest`]): `Ok(None)` where no unit declares the name, or the one that does holds no such part.
    pub(crate) fn part_digest(&self, question: &Question) -> Result<Option<String>, String> {
        let Some(unit) = question.subject().and_then(|name| self.declaring(name)) else {
            return Ok(None);
        };

        part_digest(unit.module, unit.places(), question)
    }

    /// Refuse a scope two of whose units declare a witness at one concept and one key.
    ///
    /// Each unit was held to the orphan rule where it was compiled, which is what keeps two units from declaring one witness; a scope is still asked, since its units arrive from a store and a program resolved through either of two would mean two things. The names say it, so nothing is reduced.
    pub(crate) fn coherent(&self) -> Result<(), Error> {
        let mut declared = HashMap::<(Global, WitnessKey), (Global, Qualifier)>::new();
        for unit in &self.units {
            for name in unit.witnesses() {
                let (Some((concept, key)), Global::Witness(id)) = (spelled_at(name), name) else {
                    continue;
                };
                if let Some(first) = declared.insert((concept, key), (*name, *id.module()))
                    && first.1 != *id.module()
                {
                    return Err(Error::duplicate_witness(concept, key, first, *id.module()));
                }
            }
        }

        Ok(())
    }

    /// The last unit answering `asked`: a later unit's answer over an earlier one's, which mount disjointness makes the only one.
    fn find<'s, T>(&'s self, asked: impl Fn(&'s Standing<'a>) -> Option<T>) -> Option<T> {
        self.units.iter().rev().find_map(asked)
    }
}

impl InScope for Established<'_> {
    fn definition(&self, name: &Global) -> Option<Defined<'_>> {
        self.find(|unit| unit.definition(name))
    }

    fn induct_decl(&self, name: &Global) -> Option<&InductDecl> {
        self.find(|unit| unit.induct_decl(name))
    }

    fn struct_decl(&self, name: &Global) -> Option<&StructDecl> {
        self.find(|unit| unit.struct_decl(name))
    }

    fn concept(&self, name: &Global) -> Option<&ConceptDecl> {
        self.find(|unit| unit.concept(name))
    }

    fn witness(&self, concept: &Global, key: &WitnessKey) -> Option<Global> {
        let concept = concept.qualifier()?;

        self.find(|unit| unit.witness(concept, key))
    }

    fn witnesses(&self) -> Vec<(Global, WitnessKey, Global)> {
        self.units
            .iter()
            .flat_map(Standing::witnesses)
            .filter_map(|name| {
                let (concept, key) = spelled_at(name)?;

                Some((concept, key, *name))
            })
            .collect()
    }

    fn groups(&self) -> Vec<(RecGroup, Vec<Global>)> {
        self.units.iter().flat_map(Standing::groups).collect()
    }

    fn member_names(&self, group: &RecGroup) -> Option<Vec<Global>> {
        self.find(|unit| unit.member_names(group))
    }
}
