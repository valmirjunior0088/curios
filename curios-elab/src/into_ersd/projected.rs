//! The units an item's erasure reads, each declaration out of its universes the first time it is read.
//!
//! Erasure reads the types its walk is directed by through a reader of its own, and what that reader holds of the unit and of its scope is their *universe-erased* form: universe arguments have no runtime identity, and projecting them out once keeps reduction from specializing a polymorphic body again at every use. [`Projected`] is that form as a scope the reader reads through (`InScope`): a declaration is projected when it is first read, or when it is erased, and never otherwise. An item erased over a unit reads what it reads, and a unit none of whose items is erased again projects nothing.
//!
//! **The one door a global comes in by.** The reader starts empty and is handed this view and nothing else, so every definition and registry entry it reads is out of its universes by construction, which the walk relies on: no `Instance` reaches it.

use {
    crate::{Defined, InScope},
    curios_core::{
        ConceptDecl, Definition, Global, InductDecl, InductParam, Item, Module, RecGroup,
        StructDecl, Term, WitnessKey, project_erased_universes,
    },
    std::{cell::OnceCell, collections::HashMap},
};

/// One item out of its universes: the item as erasure walks it, and, for a group, each member as another item reads it.
#[derive(Debug)]
struct Erasable {
    item: Item,
    /// A group's members opened against their names, each with the projection it unfolds to. Empty for a definition, which is read by its own fields.
    members: Vec<(Definition, Term)>,
}

impl Erasable {
    fn of(item: &Item) -> Self {
        match item {
            Item::Let(definition) => Self {
                item: Item::Let(project_definition(definition)),
                members: Vec::new(),
            },
            // Projected in place. Opening the group and closing it again rebuilds every node twice over and discards every memoized derivation on the way, where the rewrite itself is uniform under the group's own binders and needs neither.
            Item::Rec(rec) => {
                let projected = rec.projected();
                let members = projected
                    .definitions()
                    .into_iter()
                    .enumerate()
                    .map(|(index, definition)| {
                        (definition, Term::rec_proj(projected.group.clone(), index))
                    })
                    .collect();

                Self {
                    item: Item::Rec(projected),
                    members,
                }
            }
        }
    }
}

fn project_definition(definition: &Definition) -> Definition {
    Definition {
        name: definition.name,
        kind: definition.kind.clone(),
        universe_context: Default::default(),
        island: definition.island,
        // Carried as every field is, and read by nothing below: the erased functions' termination flags are marked from the certifier's record where the linked program meets it (see `curios_ersd::Function::total`), never from this stamp.
        totality: definition.totality,
        proof: definition.proof,
        variances: Vec::new(),
        type_: project_erased_universes(&definition.type_),
        body: project_erased_universes(&definition.body),
    }
}

fn project_induct(declaration: &InductDecl) -> InductDecl {
    InductDecl {
        universe_context: Default::default(),
        arity: project_erased_universes(&declaration.arity),
        constructors: declaration
            .constructors
            .iter()
            .map(|(tag, constructor)| {
                (
                    tag.clone(),
                    InductParam {
                        telescope: project_erased_universes(&constructor.telescope),
                    },
                )
            })
            .collect(),
        result_sort: project_erased_universes(&declaration.result_sort),
        module: declaration.module,
        rep_public: declaration.rep_public,
        // Polarity and variance are elaboration's, projected out here alongside the universe context.
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

fn project_struct(declaration: &StructDecl) -> StructDecl {
    StructDecl {
        universe_context: Default::default(),
        arity: project_erased_universes(&declaration.arity),
        result_sort: project_erased_universes(&declaration.result_sort),
        module: declaration.module,
        rep_public: declaration.rep_public,
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

/// One unit as erasure reads it.
#[derive(Debug)]
struct Unit<'a> {
    module: &'a Module,
    /// Where the unit holds each name: its item, and its place among the item's members.
    places: HashMap<Global, (u32, u32)>,
    items: Vec<OnceCell<Erasable>>,
    inducts: HashMap<Global, OnceCell<InductDecl>>,
    structs: HashMap<Global, OnceCell<StructDecl>>,
}

impl<'a> Unit<'a> {
    fn of(module: &'a Module) -> Self {
        let mut places = HashMap::with_capacity(module.items.len());
        for place in 0..module.items.len() {
            let item = u32::try_from(place).expect("a unit's items are counted in a u32");
            for (member, name) in module.items.names(place).into_iter().enumerate() {
                let member = u32::try_from(member).expect("a group's members are counted in a u32");
                places.insert(*name, (item, member));
            }
        }

        Self {
            module,
            places,
            items: (0..module.items.len()).map(|_| OnceCell::new()).collect(),
            inducts: module
                .induct_decls
                .keys()
                .map(|name| (*name, OnceCell::new()))
                .collect(),
            structs: module
                .struct_decls
                .keys()
                .map(|name| (*name, OnceCell::new()))
                .collect(),
        }
    }

    /// The item at `place`, out of its universes.
    fn item(&self, place: usize) -> &Erasable {
        self.items[place].get_or_init(|| Erasable::of(&self.module.items[place]))
    }

    fn definition(&self, name: &Global) -> Option<Defined<'_>> {
        let (item, member) = *self.places.get(name)?;
        let erasable = self.item(item as usize);
        let (definition, body, universe_context) = match &erasable.item {
            Item::Let(definition) => (definition, &definition.body, &definition.universe_context),
            Item::Rec(rec) => {
                let (definition, projection) = &erasable.members[member as usize];

                (definition, projection, rec.group.universe_context())
            }
        };

        Some(Defined {
            type_: &definition.type_,
            body: (!definition.proof).then_some(body),
            kind: &definition.kind,
            universe_context,
            totality: definition.totality,
            // Out of its universes a definition has no level to be irrelevant in.
            variances: &[],
            island: definition.island,
        })
    }
}

/// The unit being erased and the units in its scope, read out of their universes.
#[derive(Debug)]
pub(crate) struct Projected<'a> {
    /// The scope's units in dependency order, then the unit being erased.
    units: Vec<Unit<'a>>,
}

impl<'a> Projected<'a> {
    /// `scope`'s units, then `unit`, the one being erased.
    pub(crate) fn over(scope: &[&'a Module], unit: &'a Module) -> Self {
        Self {
            units: scope.iter().copied().chain([unit]).map(Unit::of).collect(),
        }
    }

    /// The item at `place` of the unit being erased, out of its universes: what its erasure walks.
    pub(crate) fn own(&self, place: usize) -> &Item {
        &self
            .units
            .last()
            .expect("the unit being erased is the view's last")
            .item(place)
            .item
    }
}

impl InScope for Projected<'_> {
    fn definition(&self, name: &Global) -> Option<Defined<'_>> {
        self.units
            .iter()
            .rev()
            .find_map(|unit| unit.definition(name))
    }

    fn induct_decl(&self, name: &Global) -> Option<&InductDecl> {
        self.units.iter().rev().find_map(|unit| {
            let slot = unit.inducts.get(name)?;

            Some(slot.get_or_init(|| project_induct(&unit.module.induct_decls[name])))
        })
    }

    fn struct_decl(&self, name: &Global) -> Option<&StructDecl> {
        self.units.iter().rev().find_map(|unit| {
            let slot = unit.structs.get(name)?;

            Some(slot.get_or_init(|| project_struct(&unit.module.struct_decls[name])))
        })
    }

    // Erasure resolves nothing and reports through no survey: a concept and a witness are read by elaboration alone.
    fn concept(&self, _: &Global) -> Option<&ConceptDecl> {
        None
    }

    fn witness(&self, _: &Global, _: &WitnessKey) -> Option<Global> {
        None
    }

    fn witnesses(&self) -> Vec<(Global, WitnessKey, Global)> {
        Vec::new()
    }

    fn groups(&self) -> Vec<(RecGroup, Vec<Global>)> {
        Vec::new()
    }

    fn member_names(&self, _: &RecGroup) -> Option<Vec<Global>> {
        None
    }
}
