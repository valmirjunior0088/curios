//! An item evaluated where it is erased.
//!
//! **A top-level value is computed where its unit is compiled.** An item whose top-level statements apply something erases to a fragment that computes when a program starts, so every program its unit is in scope of would link it, with what it reaches, to run it or to find that nothing observes it. What such a statement computes is the item's and what it reaches to say, and no program's. So it is computed here, once: the fragment is linked last into a module of what it reaches, the closed applications among its items are folded as a program's are ([`curios_ersd::evaluate_since`]), and what is left is taken back out as the item's erased form ([`curios_ersd::Module::taken`]). A form that computes nothing is linked by the programs that name it and by no other.
//!
//! **A name is bound to what its item computed.** A finished statement is an alias of its result, and the name the statement bound is bound to that result: a constant, which an item that names it reads in place; a function, which one applies as it applies any; or a value. So a closure one item computed is a function another can import by the first one's name, and what a later item's evaluation computes over it names it so.
//!
//! **An item is left as it was erased where evaluating it does not finish.** A statement that performs an effect stays the call it was, which the language runs when a program starts. One the evaluator gives up on, for fuel, depth or size, is tried again by each program's own rounds. And a result that names what another item binds under no name has nothing to import it by. Each leaves an item that still computes, which a program links as it links any such.
//!
//! **What evaluating it read is every erased form it was linked after, whole.** An erased form is otherwise read for how a name is bound; an evaluation runs what the items it reaches define. So the item's trace names each of them (`Question::Ran`), whether or not the evaluation finished, since another form of one may be what lets it finish: an item is erased and evaluated again where one of them is another form. The module it ran in holds those items and no other, so the list is what was linked. Nothing is recorded of a fixed unit's, which is the compiler's own.
//!
//! **An item that declares a row is not evaluated.** What is taken back out holds a schema as an import holds one, and a declared row is its fields.

use {
    super::{
        Binding, ConstructorRow, Environment, ErasedItem, ErasedUnit, Erasures, Export, FamilyRow,
        Linking, ProductRow, reached,
    },
    curios_core::{Free, Global},
    std::collections::BTreeMap,
};

/// What evaluating an item where it is erased came to.
pub(super) struct Evaluation {
    /// The item's erased form with what its top-level statements compute in their place, where that leaves it computing nothing.
    pub(super) erased: Option<ErasedItem>,
    /// A name of each item the evaluation was linked after, of a unit that is not fixed.
    pub(super) ran: Vec<Global>,
}

/// Evaluate `erased`, the erased form of the next item of `unit`, over what it reaches of `unit` and of `scope`. `None` for an item that computes nothing as it was erased, or that declares a row: there is nothing to evaluate, and nothing is read.
pub(super) fn evaluate(
    scope: Erasures<'_>,
    unit: &ErasedUnit,
    erased: &ErasedItem,
) -> Option<Evaluation> {
    if !erased.module().computes() || erased.environment().declares_rows() {
        return None;
    }
    curios_profile::profile!("evaluate_item");

    let units: Vec<&ErasedUnit> = scope.units().iter().copied().chain([unit]).collect();
    let reached = reached(&units, [erased].into_iter(), false);
    let ran = units
        .iter()
        .zip(&reached.items)
        .enumerate()
        .filter(|(place, _)| *place >= scope.units().len() || !scope.is_fixed(*place))
        .flat_map(|(_, (unit, items))| items.iter().map(|&place| unit.item(place)))
        .map(|item| {
            let (name, _) = item
                .environment()
                .bindings()
                .next()
                .expect("an item's erased form binds the names it declares");

            *name
        })
        .collect::<Vec<_>>();
    curios_profile::sample!(
        "evaluate::item::linked",
        reached.items.iter().map(|items| items.len()).sum::<usize>()
    );

    let mut linking = Linking::default();
    linking.add_reached(&units, &reached);
    let mark = linking.module.mark();
    let landed = linking.add(erased);
    let erased = curios_ersd::evaluate_since(&mut linking.module, &mark)
        .then(|| taken(&linking, &units, &mark, erased, &landed))
        .flatten();
    curios_profile::sample!("evaluate::item::finished", erased.is_some() as usize);

    Some(Evaluation { erased, ran })
}

/// What `linking`'s module holds past `mark`, as the erased form of the item `erased` was linked there as, at `landed`: `None` where that still computes, or names what no item declares under a name.
fn taken(
    linking: &Linking,
    units: &[&ErasedUnit],
    mark: &curios_ersd::Mark,
    erased: &ErasedItem,
    landed: &curios_ersd::Linked,
) -> Option<ErasedItem> {
    let declaring = |name: &Global| units.iter().find_map(|unit| unit.find(name));
    let exported = |name: &Global| declaring(name)?.export(name);

    // What each name is bound to now: where the value it bound came to be an alias, the atom that names.
    let bound = |atom: curios_ersd::Atom| linking.module.aliased(mark, landed.atom(atom));
    let exports = erased
        .environment()
        .bindings()
        .filter_map(|(_, binding)| match binding {
            Binding::Atom(atom) => Some(bound(atom)),
            Binding::Dropped => None,
        })
        .collect::<Vec<_>>();
    let taken = linking.module.taken(
        mark,
        &exports,
        |value| {
            linking
                .values
                .get(&value)
                .is_some_and(|name| matches!(exported(name), Some(Export::Value)))
        },
        |function| match exported(linking.functions.get(&function)?)? {
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
                environment.bind(&Free::from(name), taken.landed.atom(bound(atom)));
            }
            Binding::Dropped => environment.bind_dropped(&Free::from(name)),
        }
    }

    // Every schema the form names is one a linked item holds: a declaration's row, by its name, or the anonymous row of a width.
    let structs: BTreeMap<_, _> = linking
        .structs
        .iter()
        .map(|(name, schema)| (*schema, name))
        .collect();
    let tuples: BTreeMap<_, _> = linking
        .tuples
        .iter()
        .map(|(width, schema)| (*schema, *width))
        .collect();
    let families: BTreeMap<_, _> = linking
        .families
        .iter()
        .map(|(name, family)| (*family, name))
        .collect();
    for (held, schema) in taken.landed.products() {
        match structs.get(&held) {
            Some(name) => {
                let declared = declaring(name)?.environment().declared_struct_row(name)?;
                environment.import_struct_row(
                    name,
                    ProductRow {
                        schema: Some(schema),
                        mask: declared.mask.clone(),
                    },
                );
            }
            None => environment.register_tuple_schema(*tuples.get(&held)?, schema),
        }
    }
    for (held, family) in taken.landed.families() {
        let name = families.get(&held)?;
        let declared = declaring(name)?.environment().declared_induct_row(name)?;
        let constructors = taken
            .module
            .family(family)?
            .constructors
            .iter()
            .zip(&declared.constructors)
            .map(|(&id, row)| ConstructorRow {
                id,
                mask: row.mask.clone(),
            })
            .collect();
        environment.import_induct_row(
            name,
            FamilyRow {
                family,
                constructors,
            },
        );
    }

    let values = taken
        .values
        .iter()
        .map(|value| linking.values.get(value).copied())
        .collect::<Option<Vec<_>>>()?;
    let functions = taken
        .functions
        .iter()
        .map(|function| linking.functions.get(function).copied())
        .collect::<Option<Vec<_>>>()?;
    let extents = erased
        .extents()
        .filter_map(|(value, extent)| {
            match taken
                .landed
                .atom(landed.atom(curios_ersd::Atom::Value(value)))
            {
                curios_ersd::Atom::Value(value) => Some((value, *extent)),
                _ => None,
            }
        })
        .collect();

    let computed = erased
        .environment()
        .bindings()
        .filter_map(|(name, binding)| match binding {
            Binding::Atom(atom) if bound(atom) != landed.atom(atom) => Some(*name),
            _ => None,
        })
        .collect();

    Some(
        ErasedItem::new(taken.module, environment, values, functions, &extents).computing(computed),
    )
}
