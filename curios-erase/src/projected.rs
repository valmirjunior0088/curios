//! The units an item's erasure reads, each declaration out of its universes the first time it is read.
//!
//! Erasure reads the types its walk is directed by through a reader of its own, and what that reader holds of the unit and of its scope is their *universe-erased* form: universe arguments have no runtime identity, and projecting them out once keeps reduction from specializing a polymorphic body again at every use. [`Projected`] is that form as a scope the reader reads through (`InScope`): a declaration is projected when it is first read, or when it is erased, and never otherwise. An item erased over a unit reads what it reads, and a unit none of whose items is erased again projects nothing.
//!
//! **What a unit has been read out of its universes is the unit's to keep** ([`Projection`]). A unit in scope is read by the places it keeps for its names and projected into cells it keeps beside them, so nothing of it is keyed for a compile, and a declaration one erasure projected is projected for every erasure after it for as long as the unit is held.
//!
//! **The one door a global comes in by.** The reader starts empty and is handed this view and nothing else, so every definition and registry entry it reads is out of its universes by construction, which the walk relies on: no `Instance` reaches it.

use {
    crate::Erasures,
    curios_elab::{Defined, InScope},
    curios_utilities::Persistent,
    std::{cell::OnceCell, rc::Rc},
};

/// One item out of its universes: the item as erasure walks it, and, for a group, each member as another item reads it.
#[derive(Debug)]
struct Erasable {
    item: curios_core::Item,
    /// A group's members opened against their names, each with the projection it unfolds to. Empty for a definition, which is read by its own fields.
    members: Vec<(curios_core::Definition, curios_core::Term)>,
}

impl Erasable {
    fn of(item: &curios_core::Item) -> Self {
        match item {
            curios_core::Item::Let(definition) => Self {
                item: curios_core::Item::Let(Box::new(project_definition(definition))),
                members: Vec::new(),
            },
            // Projected in place. Opening the group and closing it again rebuilds every node twice over and discards every memoized derivation on the way, where the rewrite itself is uniform under the group's own binders and needs neither.
            curios_core::Item::Rec(rec) => {
                let projected = rec.projected();
                let members = projected
                    .definitions()
                    .into_iter()
                    .enumerate()
                    .map(|(index, definition)| {
                        (
                            definition,
                            curios_core::Term::rec_proj(projected.group.clone(), index),
                        )
                    })
                    .collect();

                Self {
                    item: curios_core::Item::Rec(projected),
                    members,
                }
            }
        }
    }
}

fn project_definition(definition: &curios_core::Definition) -> curios_core::Definition {
    curios_core::Definition {
        name: definition.name,
        kind: definition.kind.clone(),
        universe_context: Default::default(),
        island: definition.island,
        // Carried as every field is, and read by nothing below: the erased functions' termination flags are marked from the certifier's record where the linked program meets it (see `curios_anf::Function::total`), never from this stamp.
        totality: definition.totality,
        proof: definition.proof,
        variances: Vec::new(),
        field_variances: Vec::new(),
        parameter_field: None,
        // Read by erasure for its quality alone: every level is one here.
        value_sort: definition
            .value_sort
            .as_ref()
            .map(curios_core::project_erased_universes),
        type_: curios_core::project_erased_universes(&definition.type_),
        body: curios_core::project_erased_universes(&definition.body),
    }
}

fn project_induct(declaration: &curios_core::InductDecl) -> curios_core::InductDecl {
    curios_core::InductDecl {
        universe_context: Default::default(),
        arity: curios_core::project_erased_universes(&declaration.arity),
        constructors: declaration
            .constructors
            .iter()
            .map(|(tag, constructor)| {
                (
                    tag.clone(),
                    curios_core::InductParam {
                        telescope: curios_core::project_erased_universes(&constructor.telescope),
                    },
                )
            })
            .collect(),
        result_sort: curios_core::project_erased_universes(&declaration.result_sort),
        module: declaration.module,
        rep_public: declaration.rep_public,
        // Polarity and variance are elaboration's, projected out here alongside the universe context.
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

fn project_struct(declaration: &curios_core::StructDecl) -> curios_core::StructDecl {
    curios_core::StructDecl {
        universe_context: Default::default(),
        arity: curios_core::project_erased_universes(&declaration.arity),
        result_sort: curios_core::project_erased_universes(&declaration.result_sort),
        module: declaration.module,
        rep_public: declaration.rep_public,
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

/// What erasure has read of one unit out of its universes: a cell for each of its items and each of its registry entries, filled the first time one is read.
///
/// **Kept with the unit, as where it holds each name is.** A unit in scope of many compilations is asked for the same declarations by each, so what one projected the next finds, and making the cells is paid once for a unit and not once for a compile. It holds no reference to the module it is of: every read hands it the module, which is its unit's.
///
/// **A version of its baseline's where its unit was erased over one.** The cells are shared and held in persistent maps, so the projection of a module that is a version of its baseline's is the baseline's with a cell made anew at each place the version changed (`Projection::revised`), what was read of every other item standing.
#[derive(Debug, Clone, Default)]
pub struct Projection {
    /// By the item's place ([`curios_core::Items::places`]).
    items: Persistent<usize, Rc<OnceCell<Erasable>>>,
    inducts: Persistent<curios_core::Global, Rc<OnceCell<curios_core::InductDecl>>>,
    structs: Persistent<curios_core::Global, Rc<OnceCell<curios_core::StructDecl>>>,
}

impl Projection {
    /// The cells of `module`, none filled.
    pub fn of(module: &curios_core::Module) -> Self {
        let mut projection = Self::default();
        for place in module.items.places() {
            projection.cells(module, place);
        }

        projection
    }

    /// This projection, of an earlier module, as the projection of `module`, a version of it that `changes` says what it changed of ([`curios_core::Items::changed_since`]): a cell made anew for each item and each registry entry at a place it changed, and none for one it took out.
    pub(super) fn revised(
        &self,
        earlier: &curios_core::Module,
        module: &curios_core::Module,
        changes: &curios_core::Changes,
    ) -> Self {
        let mut projection = self.clone();
        for place in changes.replaced.iter().chain(&changes.removed) {
            projection.items.remove(place);
            for name in earlier.items.names(*place) {
                projection.inducts.remove(name);
                projection.structs.remove(name);
            }
        }
        for place in changes.replaced.iter().chain(&changes.inserted) {
            projection.cells(module, *place);
        }

        projection
    }

    /// A cell for the item at `place` of `module`, and for each registry entry it declares. Which names declare a nominal is what each place says, so a cell is made for each with no entry read.
    fn cells(&mut self, module: &curios_core::Module, place: usize) {
        self.items.insert(place, Rc::default());
        let names = module.items.names(place).into_iter().copied();
        for (name, declared) in names.zip(module.items.declared(place)) {
            match declared {
                curios_core::Declared::Inductive => self.inducts.insert(name, Rc::default()),
                declared if declared.is_former() => self.structs.insert(name, Rc::default()),
                _ => {}
            }
        }
    }

    /// The item at `place` of `module`, out of its universes.
    fn item(&self, module: &curios_core::Module, place: usize) -> &Erasable {
        self.items
            .get(&place)
            .expect("a place the module holds an item at")
            .get_or_init(|| Erasable::of(&module.items[place]))
    }
}

/// What a unit keeps for erasure, or the same made for one erasure: where the caller holds a bare module and no unit, and for the unit being erased, which no unit holds yet.
#[derive(Debug)]
enum Held<'a, T> {
    Kept(&'a T),
    Made(Box<T>),
}

impl<T> std::ops::Deref for Held<'_, T> {
    type Target = T;

    fn deref(&self) -> &T {
        match self {
            Held::Kept(kept) => kept,
            Held::Made(made) => made,
        }
    }
}

/// One unit as erasure reads it: its module, where it holds each name, and what has been read of it out of its universes.
#[derive(Debug)]
struct Unit<'a> {
    module: &'a curios_core::Module,
    places: Held<'a, curios_core::Places>,
    projection: Held<'a, Projection>,
}

impl<'a> Unit<'a> {
    /// `module` with what is made of it for this erasure alone.
    fn made(module: &'a curios_core::Module) -> Self {
        Self {
            module,
            places: Held::Made(Box::new(curios_core::Places::of(module))),
            projection: Held::Made(Box::new(Projection::of(module))),
        }
    }

    fn definition(&self, name: &curios_core::Global) -> Option<Defined<'_>> {
        let place = self.places.place(name)?;
        let member = match place {
            curios_core::Place::Let(_) => 0,
            curios_core::Place::Member { member, .. } => member,
        };
        let erasable = self.projection.item(self.module, place.item());
        let (definition, body, universe_context) = match &erasable.item {
            curios_core::Item::Let(definition) => (
                definition.as_ref(),
                &definition.body,
                &definition.universe_context,
            ),
            curios_core::Item::Rec(rec) => {
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
            field_variances: &[],
            parameter_field: None,
            value_sort: definition.value_sort.as_ref(),
            island: definition.island,
        })
    }

    fn induct_decl(&self, name: &curios_core::Global) -> Option<&curios_core::InductDecl> {
        let slot = self.projection.inducts.get(name)?;

        Some(slot.get_or_init(|| {
            let declaration = self
                .module
                .induct_decl(&self.places, name)
                .expect("a name declared as an inductive's former carries its entry");

            project_induct(declaration)
        }))
    }

    fn struct_decl(&self, name: &curios_core::Global) -> Option<&curios_core::StructDecl> {
        let slot = self.projection.structs.get(name)?;

        Some(slot.get_or_init(|| {
            let declaration = self
                .module
                .struct_decl(&self.places, name)
                .expect("a name declared as a record's former carries its entry");

            project_struct(declaration)
        }))
    }
}

/// The unit being erased and the units in its scope, read out of their universes.
#[derive(Debug)]
pub struct Projected<'a> {
    /// The scope's units in dependency order, then the unit being erased.
    units: Vec<Unit<'a>>,
}

impl<'a> Projected<'a> {
    /// `scope`'s units, each read by what its unit keeps for erasure where the scope holds that, then `unit`, the one being erased.
    pub fn over(scope: Erasures<'a>, unit: &'a curios_core::Module) -> Self {
        Self::with(scope, Unit::made(unit))
    }

    /// The units of `scope` and `unit`, whose module is held at `places` and has been read out of its universes as `projection` holds it: a unit erased over a baseline, whose places are its module's own and whose projection is a version of its baseline's.
    pub fn revised(
        scope: Erasures<'a>,
        unit: &'a curios_core::Module,
        places: &'a curios_core::Places,
        projection: Projection,
    ) -> Self {
        Self::with(
            scope,
            Unit {
                module: unit,
                places: Held::Kept(places),
                projection: Held::Made(Box::new(projection)),
            },
        )
    }

    /// What erasure read of the unit being erased out of its universes: what that unit keeps, a later erasure over it finding it as this one left it.
    pub fn into_projection(mut self) -> Projection {
        let own = self
            .units
            .pop()
            .expect("the unit being erased is the view's last");

        match own.projection {
            Held::Made(projection) => *projection,
            Held::Kept(projection) => projection.clone(),
        }
    }

    /// The units of `scope`, each read by what its unit keeps for it where it keeps it, with `own` last.
    fn with(scope: Erasures<'a>, own: Unit<'a>) -> Self {
        let units = scope
            .cores()
            .iter()
            .enumerate()
            .map(|(place, module)| match scope.kept(place) {
                Some((places, projection)) => Unit {
                    module,
                    places: Held::Kept(places),
                    projection: Held::Kept(projection),
                },
                None => Unit::made(module),
            })
            .chain([own])
            .collect();

        Self { units }
    }

    /// The item at `place` of the unit being erased, out of its universes: what its erasure walks.
    pub fn own(&self, place: usize) -> &curios_core::Item {
        let own = self
            .units
            .last()
            .expect("the unit being erased is the view's last");

        &own.projection.item(own.module, place).item
    }
}

impl InScope for Projected<'_> {
    fn definition(&self, name: &curios_core::Global) -> Option<Defined<'_>> {
        self.units
            .iter()
            .rev()
            .find_map(|unit| unit.definition(name))
    }

    fn induct_decl(&self, name: &curios_core::Global) -> Option<&curios_core::InductDecl> {
        self.units
            .iter()
            .rev()
            .find_map(|unit| unit.induct_decl(name))
    }

    fn struct_decl(&self, name: &curios_core::Global) -> Option<&curios_core::StructDecl> {
        self.units
            .iter()
            .rev()
            .find_map(|unit| unit.struct_decl(name))
    }

    // Erasure resolves nothing and reports through no survey: a concept and a witness are read by elaboration alone.
    fn concept(&self, _: &curios_core::Global) -> Option<&curios_core::ConceptDecl> {
        None
    }

    fn witness(
        &self,
        _: &curios_core::Global,
        _: &curios_core::WitnessKey,
    ) -> Option<curios_core::Global> {
        None
    }

    fn witnesses(
        &self,
    ) -> Vec<(
        curios_core::Global,
        curios_core::WitnessKey,
        curios_core::Global,
    )> {
        Vec::new()
    }

    fn groups(&self) -> Vec<(curios_core::RecGroup, Vec<curios_core::Global>)> {
        Vec::new()
    }

    fn member_names(&self, _: &curios_core::RecGroup) -> Option<Vec<curios_core::Global>> {
        None
    }
}
