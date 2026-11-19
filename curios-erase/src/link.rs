//! Linking a program: the erased forms it reaches, appended to one module in the order their items evaluate.
//!
//! **A program links what it reaches.** Its own items, its entry, and of its scope the items those import, transitively. An item of the scope the program never names is no part of the module the back half lowers, so a program is not handed a library to drop.
//!
//! **And what initializes observably, whether or not it is named.** A top-level value whose evaluation is observable runs before the entry in every program its unit is in scope of, which is the language's meaning and not this pass's to change. What one computes was computed where its unit was erased, wherever that could be finished, and where it could not, whether what is left is observable was decided there too, by the rule a program's prune keeps an item by (`evaluate`); here every item whose initialization is observable is linked with what it reaches. One that still computes and is not observable is linked by a program that names it, as any item is, and by no other, whose prune would drop it.
//!
//! **The order is the scope's.** Units in dependency order, each one's items in the order it stores them, then the program's own and its entry: every item after the items it imports from, which is the order their statements evaluate in.
//!
//! **A schema is its declaration's.** The item that declares a structure or an inductive registers its row, and a fragment that imports the row is linked after that item's rows are, its schema resolved to the one the item registered. Only the rows: a declaration a program constructs or matches values of, and names nowhere else, contributes its schemas and none of what its own item erased to ([`curios_anf::Module::link_rows`]). An anonymous row is declared by no item: the first fragment linked that holds one of a width registers it, and every later one is resolved to it.
//!
//! **An evaluation holds a row as its readers do.** An item evaluated where it is erased runs in a module of what it reaches ([`crate::evaluate`]), whose evaluator reads no row, so that module links no declaration's rows: a row a fragment imports and no linked item declares is the module's import, at the count or width the fragment's import states and with the constructors the fragments name ([`curios_anf::Resolution::imports_rows`]), and an evaluation pays for what its fragments name of a row, never for the row.

use {
    crate::{
        AtSorts, Binding, Environment, ErasedItem, ErasedNames, ErasedProgram, ErasedUnit,
        Erasures, Error, Export, Origin, Sorts,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// A program under construction: the module, and where each name and each schema linked so far landed, by the name and the sorts of the form that declares it.
#[derive(Default)]
pub struct Linking {
    module: curios_anf::Module,
    bound: BTreeMap<AtSorts, curios_anf::Atom>,
    /// The name and sorts each value and each function a linked form bound is declared under: the first to bind it, which is the form that defines it, since a form that hands another's name on is linked after it.
    values: BTreeMap<curios_anf::ValueId, AtSorts>,
    functions: BTreeMap<curios_anf::FunctionId, AtSorts>,
    /// Each function of a linked form's evaluation, by the names its item declares: what the certifier's verdict on those names speaks of ([`curios_anf::Function::total`]).
    evaluating: BTreeMap<curios_anf::FunctionId, Vec<curios_core::Global>>,
    structs: BTreeMap<AtSorts, curios_anf::ProductId>,
    families: BTreeMap<AtSorts, curios_anf::FamilyId>,
    tuples: BTreeMap<usize, curios_anf::ProductId>,
    /// Whether a row a fragment imports and no linked item declares is the module's import, where a program links every row it names from its declaration: an evaluation's.
    imports_rows: bool,
}

impl Linking {
    /// A linking whose module holds each row a fragment imports and no linked item declares as an import: an evaluation's, which reads no row.
    pub fn importing_rows() -> Self {
        Self {
            imports_rows: true,
            ..Self::default()
        }
    }

    /// Append what `reached` takes of `units`, in the order of the units and, of each, in the order it stores its items, which puts a declaration ahead of whatever imports its rows. A form reached for its rows alone is linked only where the module does not import them.
    pub fn add_reached(&mut self, units: &[&ErasedUnit], reached: &Reached) {
        for (unit, (items, rows)) in units.iter().zip(reached.items.iter().zip(&reached.rows)) {
            for &(place, form) in items.union(rows) {
                let (erased, sorts) = unit.item(place).form(form);
                match (items.contains(&(place, form)), self.imports_rows) {
                    (true, _) => {
                        self.add(erased, &sorts);
                    }
                    (false, true) => {}
                    (false, false) => self.add_rows(erased, &sorts),
                }
            }
        }
    }

    /// Append one form's fragment, which stands for its item at each of `sorts`, its imports standing for where the forms that bind them landed, and hand back where its identities landed.
    pub fn add(&mut self, erased: &ErasedItem, sorts: &[Sorts]) -> curios_anf::Linked {
        let bound = |(name, sorts): &AtSorts| {
            *self.bound.get(&(*name, *sorts)).unwrap_or_else(|| {
                panic!(
                    "{}{sorts} is imported before the form that binds it is linked",
                    name.faithful()
                )
            })
        };
        let resolution = curios_anf::Resolution {
            values: erased.values().iter().map(bound).collect(),
            functions: erased
                .functions()
                .iter()
                .map(|name| match bound(name) {
                    curios_anf::Atom::Function(function) => function,
                    atom => panic!(
                        "{}{} is imported as a function and bound to {atom:?}",
                        name.0.faithful(),
                        name.1
                    ),
                })
                .collect(),
            ..self.held(erased, sorts)
        };

        let linked = self.module.link(erased.module(), &resolution);

        // Every function the fragment defines itself is its item's, and one of the item's evaluation is what a verdict on the names the item declares speaks of: a function of its term, of an intrinsic's loop, or of a closure computing it produced.
        let fragment = erased.module();
        let imported = fragment
            .imports()
            .functions()
            .iter()
            .map(|import| import.function)
            .collect::<BTreeSet<_>>();
        let declared = erased
            .environment()
            .bindings()
            .map(|(name, _)| *name)
            .collect::<Vec<_>>();
        for function in fragment.function_ids() {
            let evaluates = fragment
                .function(function)
                .is_some_and(|definition| definition.role == curios_anf::Role::Evaluation);
            if evaluates && !imported.contains(&function) {
                self.evaluating
                    .insert(linked.function(function), declared.clone());
            }
        }

        self.hold(erased, sorts, &linked);
        for (name, binding) in erased.environment().bindings() {
            if let Binding::Atom(atom) = binding {
                let atom = linked.atom(atom);
                for &at in sorts {
                    self.bound.insert((*name, at), atom);
                }
                let first = (*name, sorts[0]);
                match atom {
                    curios_anf::Atom::Value(value) => {
                        self.values.entry(value).or_insert(first);
                    }
                    curios_anf::Atom::Function(function) => {
                        self.functions.entry(function).or_insert(first);
                    }
                    curios_anf::Atom::Constant(_) => {}
                }
            }
        }

        linked
    }

    /// Register one form's schemas and nothing else of its fragment: the rows a linked fragment imports from a declaration the program never reaches.
    fn add_rows(&mut self, erased: &ErasedItem, sorts: &[Sorts]) {
        let resolution = self.held(erased, sorts);
        let linked = self.module.link_rows(erased.module(), &resolution);

        self.hold(erased, sorts, &linked);
    }

    /// Which of `erased`'s schemas the program already holds, each with where: a row by the declaration and sorts it is the row of — one the form declares at the first of `sorts`, which it stands for — and an anonymous one by its width. A module that imports rows holds one no linked form declares as an import, which linking registers.
    fn held(&self, erased: &ErasedItem, sorts: &[Sorts]) -> curios_anf::Resolution {
        let environment = erased.environment();
        for name in environment.imported_rows() {
            // A newtype's row has no schema, so nothing of it is resolved.
            let held = match environment.imported_struct_row(*name) {
                Some(schema) => schema.is_none() || self.structs.contains_key(name),
                None => self.families.contains_key(name),
            };
            assert!(
                held || self.imports_rows,
                "the row of {}{} is imported before the rows of the form that declares it are linked",
                name.0.faithful(),
                name.1
            );
        }

        let mut resolution = curios_anf::Resolution {
            imports_rows: self.imports_rows,
            ..Default::default()
        };
        for (origin, name, schema) in environment.struct_schemas() {
            if let Some(&held) = self.structs.get(&linked_as(origin, name, sorts)) {
                resolution.products.insert(schema, held);
            }
        }
        for (width, schema) in environment.tuple_schemas() {
            if let Some(&held) = self.tuples.get(&width) {
                resolution.products.insert(schema, held);
            }
        }
        for (origin, name, family) in environment.families() {
            if let Some(&held) = self.families.get(&linked_as(origin, name, sorts)) {
                resolution.families.insert(family, held);
            }
        }

        resolution
    }

    /// Record where each schema of `erased` landed, for the fragments linked after it: a row it declares under each of `sorts`, the assignments the form stands for.
    fn hold(&mut self, erased: &ErasedItem, sorts: &[Sorts], linked: &curios_anf::Linked) {
        let environment = erased.environment();
        for (origin, name, schema) in environment.struct_schemas() {
            for name in linked_under(origin, name, sorts) {
                self.structs.entry(name).or_insert(linked.product(schema));
            }
        }
        for (width, schema) in environment.tuple_schemas() {
            self.tuples.entry(width).or_insert(linked.product(schema));
        }
        for (origin, name, family) in environment.families() {
            for name in linked_under(origin, name, sorts) {
                self.families.entry(name).or_insert(linked.family(family));
            }
        }
    }

    pub fn mark(&self) -> curios_anf::Mark {
        self.module.mark()
    }

    /// Mark total every function of a linked form's evaluation that `certification` classifies so, as a program's are ([`ErasedProgram::mark_total`]).
    pub fn mark_total(&mut self, certification: &curios_core::Certification) {
        mark_evaluations_total(&mut self.module, &self.evaluating, certification);
    }

    /// The names whose totality marking the module reads: those of every linked item with a function of its evaluation.
    pub fn evaluated_names(&self) -> BTreeSet<curios_core::Global> {
        self.evaluating.values().flatten().copied().collect()
    }

    /// Whether a prune would keep what the module holds past `mark` for its effect ([`curios_anf::observable_since`]): asked once its functions are marked, as a program's prune asks.
    pub fn observable_since(&self, mark: &curios_anf::Mark) -> bool {
        curios_anf::observable_since(&self.module, mark)
    }

    /// Run what the module holds past `mark` ([`curios_anf::evaluate_since`]), answering whether that finished.
    pub fn evaluate_since(&mut self, mark: &curios_anf::Mark) -> bool {
        curios_anf::evaluate_since(&mut self.module, mark)
    }

    /// What the module holds past `mark`, as the erased form of the item `erased` was linked there as, at `landed`: `None` where that still computes, or names what no item declares under a name.
    pub fn taken(
        &self,
        units: &[&ErasedUnit],
        mark: &curios_anf::Mark,
        erased: &ErasedItem,
        landed: &curios_anf::Linked,
    ) -> Option<ErasedItem> {
        let declaring =
            |(name, sorts): &AtSorts| units.iter().find_map(|unit| unit.find(name, *sorts));
        let exported = |at: &AtSorts| declaring(at)?.export(&at.0);

        // What each name is bound to now: where the value it bound came to be an alias, the atom that names.
        let bound = |atom: curios_anf::Atom| self.module.aliased(mark, landed.atom(atom));
        let exports = erased
            .environment()
            .bindings()
            .filter_map(|(_, binding)| match binding {
                Binding::Atom(atom) => Some(bound(atom)),
                Binding::Dropped => None,
            })
            .collect::<Vec<_>>();
        let taken = self.module.taken(
            mark,
            &exports,
            |value| {
                self.values
                    .get(&value)
                    .is_some_and(|name| matches!(exported(name), Some(Export::Value)))
            },
            |function| match exported(self.functions.get(&function)?)? {
                Export::Function { interface, .. } => Some(interface),
                _ => None,
            },
        )?;
        // Held to what an erased fragment is held to where it is built, since this is the form that is stored.
        taken.module.verify_fragment().ok()?;
        if taken.module.computes() {
            return None;
        }

        let mut environment = Environment::default();
        for (name, binding) in erased.environment().bindings() {
            match binding {
                Binding::Atom(atom) => {
                    environment.bind(name, taken.landed.atom(bound(atom)));
                }
                Binding::Dropped => environment.bind_dropped(name),
            }
        }

        // Every schema the form names is one a linked item holds: a declaration's row, by its name, or the anonymous row of a width.
        let structs: BTreeMap<_, _> = self
            .structs
            .iter()
            .map(|(name, schema)| (*schema, *name))
            .collect();
        let tuples: BTreeMap<_, _> = self
            .tuples
            .iter()
            .map(|(width, schema)| (*schema, *width))
            .collect();
        let families: BTreeMap<_, _> = self
            .families
            .iter()
            .map(|(name, family)| (*family, *name))
            .collect();
        for (held, schema) in taken.landed.products() {
            match structs.get(&held) {
                Some(name) => environment.import_struct_row(*name, Some(schema)),
                None => environment.register_tuple_schema(*tuples.get(&held)?, schema),
            }
        }
        let positions = taken.module.imports().constructors();
        for (held, family) in taken.landed.families() {
            let name = *families.get(&held)?;
            environment.import_induct_family(name, family);
            for constructor in &taken.module.family(family)?.constructors {
                environment.import_induct_constructor(
                    name,
                    *positions.get(constructor)? as usize,
                    *constructor,
                );
            }
        }

        let values = taken
            .values
            .iter()
            .map(|value| self.values.get(value).copied())
            .collect::<Option<Vec<_>>>()?;
        let functions = taken
            .functions
            .iter()
            .map(|function| self.functions.get(function).copied())
            .collect::<Option<Vec<_>>>()?;
        Some(ErasedItem::new(
            taken.module,
            environment,
            values,
            functions,
        ))
    }
}

/// The name and sorts a row of `erased` stands under in a program: one it declares, at no sorts, under the assignment the form stands for first; one it imports, as it imports it.
fn linked_as(origin: Origin, (name, sorts): AtSorts, assignments: &[Sorts]) -> AtSorts {
    match origin {
        Origin::Declared => (name, assignments[0]),
        Origin::Imported => (name, sorts),
    }
}

/// Every name and sorts a row of a form standing for `assignments` is linked under: one it declares under each, one it imports as it imports it.
fn linked_under(origin: Origin, (name, sorts): AtSorts, assignments: &[Sorts]) -> Vec<AtSorts> {
    match origin {
        Origin::Declared => assignments.iter().map(|at| (name, *at)).collect(),
        Origin::Imported => vec![(name, sorts)],
    }
}

/// What a program takes of each unit it is linked from, as the places of its items and which of each item's forms, ascending.
pub struct Reached {
    /// The forms it links.
    items: Vec<BTreeSet<(usize, usize)>>,
    /// The forms whose rows it links: those declaring the rows a linked form imports, with theirs.
    rows: Vec<BTreeSet<(usize, usize)>>,
}

impl Reached {
    pub fn items(&self) -> &[BTreeSet<(usize, usize)>] {
        &self.items
    }
}

/// The unit, the place of the item and which of its forms declares each of `names` at its sorts that one of `units` declares. A name none declares is the program's own, and every form of the program is linked.
fn places<'n>(
    units: &[&ErasedUnit],
    names: impl Iterator<Item = &'n AtSorts>,
) -> Vec<(usize, (usize, usize))> {
    names
        .filter_map(|(name, sorts)| {
            units.iter().enumerate().find_map(|(unit, erased)| {
                let place = erased.place(name)?;
                let form = erased.item(place).index_at(*sorts)?;

                Some((unit, (place, form)))
            })
        })
        .collect()
}

/// Mark total in `module` each function `evaluating` holds whose item's every declared definition `certification` classifies `Total`. A group's members are judged as one, so a function of the group's item is marked only where all of them are total; a name the record does not classify, another unit's, leaves its function as it was, for that unit's record to mark.
pub fn mark_evaluations_total(
    module: &mut curios_anf::Module,
    evaluating: &BTreeMap<curios_anf::FunctionId, Vec<curios_core::Global>>,
    certification: &curios_core::Certification,
) {
    let total = evaluating
        .iter()
        .filter(|(_, names)| {
            !names.is_empty()
                && names.iter().all(|name| {
                    certification
                        .totality(name)
                        .is_some_and(|totality| totality.is_total())
                })
        })
        .map(|(function, _)| *function)
        .collect::<Vec<_>>();
    for id in total {
        module.mark_total(id);
    }
}

/// What `roots` take of each of `units`: the items they import, transitively, and, where `observable`, every item whose initialization is observable with what it imports; and, of the declarations whose rows any of those import, the rows.
pub fn reached<'a>(
    units: &[&ErasedUnit],
    roots: impl Iterator<Item = &'a ErasedItem>,
    observable: bool,
) -> Reached {
    let mut reached = Reached {
        items: vec![BTreeSet::new(); units.len()],
        rows: vec![BTreeSet::new(); units.len()],
    };
    let mut items = Vec::new();
    let mut rows = Vec::new();

    for root in roots {
        items.extend(places(units, root.values().iter().chain(root.functions())));
        rows.extend(places(units, root.environment().imported_rows()));
    }
    if observable {
        for (unit, erased) in units.iter().enumerate() {
            for place in erased.observable() {
                let forms = erased.item(place).forms().count();
                items.extend((0..forms).map(|form| (unit, (place, form))));
            }
        }
    }
    while let Some((unit, at)) = items.pop() {
        if reached.items[unit].insert(at) {
            let (erased, _) = units[unit].item(at.0).form(at.1);
            items.extend(places(
                units,
                erased.values().iter().chain(erased.functions()),
            ));
            rows.extend(places(units, erased.environment().imported_rows()));
        }
    }
    // A row's fields name other rows, which the declaring fragment imports in its turn.
    while let Some((unit, at)) = rows.pop() {
        if reached.rows[unit].insert(at) {
            let (erased, _) = units[unit].item(at.0).form(at.1);
            rows.extend(places(units, erased.environment().imported_rows()));
        }
    }

    reached
}

/// Link a program: `own`, its module's erased items, and `entry`, its entry's, with what they reach of `scope` — verified whole, as the module the back half lowers.
pub fn link(
    scope: Erasures<'_>,
    own: &ErasedUnit,
    entry: &ErasedItem,
) -> Result<ErasedProgram, Error> {
    curios_profile::profile!("link_program");
    let reached = reached(
        scope.units(),
        own.items().flat_map(|item| item.forms()).chain([entry]),
        true,
    );
    curios_profile::sample!(
        "link::observable",
        scope
            .units()
            .iter()
            .map(|unit| unit.observable().count())
            .sum::<usize>()
    );
    curios_profile::sample!(
        "link::reached",
        reached.items.iter().map(BTreeSet::len).sum::<usize>()
    );

    let mut linking = Linking::default();
    linking.add_reached(scope.units(), &reached);
    for item in own.items() {
        for (erased, sorts) in item.distinct() {
            linking.add(erased, &sorts);
        }
    }
    linking.add(entry, &[Sorts::NONE]);

    let Linking {
        module, evaluating, ..
    } = linking;
    // Every fragment was held to the recursion rules where it was erased: its own groups by its verification, what it applies of another through that function's interface, and a read of a value whose initializer reads itself where the read was erased. So nothing here is expected to be refused, and a refusal would name nothing an extent was kept for.
    module.verify().map_err(|error| {
        Error::refused_by_verifier(error, &BTreeMap::new(), &ErasedNames::default())
    })?;

    Ok(ErasedProgram::new(module, evaluating))
}
