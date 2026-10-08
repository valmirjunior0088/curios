//! What elaboration starts from: the units already elaborated, read where they stand.
//!
//! Elaboration of a compilation unit happens against everything already in scope, and that scope is not a *part* of the unit: it is a set of finished modules. [`Established`] is those modules as a declaration reads them — a name's type, its body, a registry entry, the witness at a key — each answered by reference into the unit that holds it (`InScope`).
//!
//! **Nothing of a scope is brought in.** A context is handed the scope and reads through to it where its own stores miss, so what a compile costs of its scope is what its declarations read: no definition is defined again, no registry entry cloned, no witness registered again by reducing a signature its own unit already keyed. A unit in scope answers for its names by an index made once for the compile, a place for each name and nothing copied.
//!
//! **A witness in scope is found by its name**, which states its concept and its key (`curios_core::WitnessId`): a unit's witnesses are an ordered set of names, so the witness at a concept and a key is the first name from there on.

use {
    super::{Context, Defined, Error, InScope, recorded_totality, register_witness},
    curios_core::{
        ConceptDecl, Free, Global, InductDecl, Item, Module, RecGroup, StructDecl, Term, WitnessId,
        WitnessKey,
    },
    curios_utilities::Qualifier,
    std::{cell::OnceCell, collections::HashMap},
};

/// Where a unit holds a name: an item that is a definition, or a member of an item that is a group.
#[derive(Debug, Clone, Copy)]
enum Place {
    Let(u32),
    Member { item: u32, member: u32, slot: u32 },
}

/// One unit in scope, answering for its names.
#[derive(Debug)]
struct Standing<'a> {
    module: &'a Module,
    places: HashMap<Global, Place>,
    /// A group's member is read by its type and by its projection out of the group, which are terms the group does not hold as such: each is made the first time its member is read, and kept for the compile.
    members: Vec<OnceCell<(Term, Term)>>,
    /// The item each of the unit's groups is, made the first time a report asks which names a group's members go by.
    groups: OnceCell<HashMap<RecGroup, u32>>,
}

impl<'a> Standing<'a> {
    fn of(module: &'a Module) -> Self {
        let mut places = HashMap::with_capacity(module.items.len());
        let mut slots = 0;
        for (item, declared) in module.items.iter().enumerate() {
            let item = u32::try_from(item).expect("a unit's items are counted in a u32");
            match declared {
                Item::Let(definition) => {
                    places.insert(definition.name, Place::Let(item));
                }
                Item::Rec(rec) => {
                    for (member, definition) in rec.definitions.iter().enumerate() {
                        let member =
                            u32::try_from(member).expect("a group's members are counted in a u32");
                        places.insert(
                            definition.name,
                            Place::Member {
                                item,
                                member,
                                slot: slots,
                            },
                        );
                        slots += 1;
                    }
                }
            }
        }

        Self {
            module,
            places,
            members: (0..slots).map(|_| OnceCell::new()).collect(),
            groups: OnceCell::new(),
        }
    }

    fn definition(&self, name: &Global) -> Option<Defined<'_>> {
        match *self.places.get(name)? {
            Place::Let(item) => {
                let Item::Let(definition) = &self.module.items[item as usize] else {
                    unreachable!("a definition's place is a definition's");
                };

                Some(Defined {
                    type_: &definition.type_,
                    body: &definition.body,
                    kind: &definition.kind,
                    universe_context: &definition.universe_context,
                    totality: definition.totality,
                    variances: &definition.variances,
                    island: definition.island,
                })
            }
            Place::Member { item, member, slot } => {
                let Item::Rec(rec) = &self.module.items[item as usize] else {
                    unreachable!("a member's place is a group's");
                };
                let index = member as usize;
                let definition = &rec.definitions[index];
                let (type_, body) = self.members[slot as usize].get_or_init(|| {
                    (
                        rec.group.member_type(index),
                        Term::rec_proj(rec.group.clone(), index),
                    )
                });

                Some(Defined {
                    type_,
                    body,
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

    /// The first witness this unit declares at `(concept, key)`: its names are ordered by concept and key, so it is the first from there on.
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
            .next()
            .filter(|name| spelled_at(name) == Some((Global::Authored(*concept), *key)))
            .copied()
    }

    fn member_names(&self, group: &RecGroup) -> Option<Vec<Global>> {
        let groups = self.groups.get_or_init(|| {
            self.module
                .items
                .iter()
                .enumerate()
                .filter_map(|(item, declared)| match declared {
                    Item::Rec(rec) => Some((
                        rec.group.clone(),
                        u32::try_from(item).expect("a unit's items are counted in a u32"),
                    )),
                    Item::Let(_) => None,
                })
                .collect()
        });
        let Item::Rec(rec) = &self.module.items[*groups.get(group)? as usize] else {
            unreachable!("a group's place is a group's");
        };

        Some(
            rec.definitions
                .iter()
                .map(|definition| definition.name)
                .collect(),
        )
    }
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
    /// The units already elaborated, in dependency order. Empty for a from-scratch elaboration, where the unit defines every name it mentions.
    units: Vec<Standing<'a>>,
    modules: Vec<&'a Module>,
}

impl<'a> Established<'a> {
    /// Nothing in scope: the unit is elaborated whole, against itself alone.
    pub fn nothing() -> Self {
        Self::default()
    }

    /// Everything `modules` established, in dependency order, as scope.
    pub fn over(modules: &[&'a Module]) -> Self {
        Self {
            units: modules.iter().map(|module| Standing::of(module)).collect(),
            modules: modules.to_vec(),
        }
    }

    /// The modules in scope, in dependency order.
    pub fn modules(&self) -> &[&'a Module] {
        &self.modules
    }

    /// Refuse a scope two of whose units declare a witness at one concept and one key.
    ///
    /// Each unit was held to the orphan rule where it was compiled, which is what keeps two units from declaring one witness; a scope is still asked, since its units arrive from a store and a program resolved through either of two would mean two things. The names say it, so nothing is reduced.
    pub(crate) fn coherent(&self) -> Result<(), Error> {
        let mut declared = HashMap::<(Global, WitnessKey), Qualifier>::new();
        for unit in &self.units {
            for name in &unit.module.witnesses {
                let (Some((concept, key)), Global::Witness(id)) = (spelled_at(name), name) else {
                    continue;
                };
                if let Some(first) = declared.insert((concept, key), *id.module())
                    && first != *id.module()
                {
                    return Err(Error::duplicate_witness(concept, key, first, *id.module()));
                }
            }
        }

        Ok(())
    }

    /// The last unit answering `asked`: a later unit's answer over an earlier one's, which mount disjointness makes the only one.
    fn find<T>(&self, asked: impl Fn(&Standing<'a>) -> Option<T>) -> Option<T> {
        self.units.iter().rev().find_map(asked)
    }
}

/// The registries, mounts and recorded totality of the items a recompile reuses, into `context`'s own stores before any item is checked.
///
/// Those items are the unit's own and arrive elaborated, in a module made for this compile, so no view of a scope answers for them: they stand in the context's stores as the items elaborated beside them will. An inductive's type-constructor and value-constructor definitions reference their own registry entry, and `elaborate_struct`/`elaborate_proj` consult the struct registry, so these go in ahead of the unit's own; the totality flags were settled when the baseline was compiled, which is what lets a written type be refused against them before it reduces.
pub(crate) fn seed_reused(context: &mut Context, reused: &Module) -> Result<(), Error> {
    context.mount(&reused.mounts);
    context.seed_totality(&recorded_totality(reused));
    for (name, induct_decl) in &reused.induct_decls {
        context.register_induct(name, induct_decl.clone())?;
    }
    for (name, struct_decl) in &reused.struct_decls {
        context.register_struct(name, struct_decl.clone())?;
    }
    for (name, concept) in &reused.concepts {
        context.register_concept(name, concept.clone())?;
    }

    Ok(())
}

/// The definitions of the items a recompile reuses, into the persistent base frame.
///
/// `define_assuming_scheme` reproduces exactly the state `elaborate_module_let`/`_rec` leave behind — assume the type, define the body — with no re-checking, because these terms are already elaborated, and a definition's variance vector is filed as its publication files it. A witness additionally registers its already-elaborated signature, since the witness table is per-elaboration state and is not carried on a module.
///
/// Registering reduces the signature to its key, so each registration spends a budget restored for it: what a baseline costs to bring in is spent on no declaration's budget.
pub(crate) fn replay_reused(context: &mut Context, reused: &Module) -> Result<(), Error> {
    for item in &reused.items {
        match item {
            Item::Let(def) => {
                context.define_assuming_scheme(
                    &Free::from(&def.name),
                    &def.type_,
                    &def.body,
                    Some(&def.kind),
                    def.universe_context.clone(),
                );
                context.record_definition_variances(def);
                if reused.witnesses.contains(&def.name) {
                    context.restore_budget();
                    register_witness(
                        context,
                        &def.name,
                        &def.type_,
                        def.universe_context.clone(),
                        &def.island,
                    )?;
                }
            }
            Item::Rec(rec) => {
                for (index, definition) in rec.definitions.iter().enumerate() {
                    let name = Free::from(&definition.name);
                    context.assume(&name, &rec.group.member_type(index));
                    context.set_assumption_universe_context(
                        &name,
                        rec.group.universe_context().clone(),
                    );
                    context.define(
                        &name,
                        &Term::rec_proj(rec.group.clone(), index),
                        Some(&definition.kind),
                    );
                    // A witness that resolves through its own entry is lowered as a group of one, so the table is rebuilt from this arm as well.
                    if reused.witnesses.contains(&definition.name) {
                        context.restore_budget();
                        register_witness(
                            context,
                            &definition.name,
                            &rec.group.member_type(index),
                            rec.group.universe_context().clone(),
                            &definition.island,
                        )?;
                    }
                }
            }
        }
    }

    Ok(())
}

impl InScope for Established<'_> {
    fn definition(&self, name: &Global) -> Option<Defined<'_>> {
        self.units
            .iter()
            .rev()
            .find_map(|unit| unit.definition(name))
    }

    fn induct_decl(&self, name: &Global) -> Option<&InductDecl> {
        self.find(|unit| unit.module.induct_decls.get(name))
    }

    fn struct_decl(&self, name: &Global) -> Option<&StructDecl> {
        self.find(|unit| unit.module.struct_decls.get(name))
    }

    fn concept(&self, name: &Global) -> Option<&ConceptDecl> {
        self.find(|unit| unit.module.concepts.get(name))
    }

    fn witness(&self, concept: &Global, key: &WitnessKey) -> Option<Global> {
        let concept = concept.qualifier()?;

        self.find(|unit| unit.witness(concept, key))
    }

    fn witnesses(&self) -> Vec<(Global, WitnessKey, Global)> {
        self.units
            .iter()
            .flat_map(|unit| &unit.module.witnesses)
            .filter_map(|name| {
                let (concept, key) = spelled_at(name)?;

                Some((concept, key, *name))
            })
            .collect()
    }

    fn groups(&self) -> Vec<(RecGroup, Vec<Global>)> {
        self.units
            .iter()
            .flat_map(|unit| &unit.module.items)
            .filter_map(|item| match item {
                Item::Rec(rec) => Some((
                    rec.group.clone(),
                    rec.definitions
                        .iter()
                        .map(|definition| definition.name)
                        .collect(),
                )),
                Item::Let(_) => None,
            })
            .collect()
    }

    fn member_names(&self, group: &RecGroup) -> Option<Vec<Global>> {
        self.find(|unit| unit.member_names(group))
    }
}
