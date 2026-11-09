//! What hoists to an interned global and what stays inline.

use {
    crate::{
        EmissionBody, EmissionModule, hoist_consts, lower, structurize, test_support::*, value_id,
        value_name,
    },
    std::collections::BTreeSet,
};

#[test]
fn constant_tuples_hoist_to_one_interned_global() {
    let wat = wat(&constant_tuple_pair());
    // One construction in the start function serves both bindings; the projection reads it back through the const global.
    assert_eq!(count(&wat, "struct.new $tuple/1"), 1);
    assert_contains(&wat, "global.set $const/");
    assert_contains(&wat, "global.get $const/");
}

#[test]
fn runtime_tuples_stay_inline() {
    let wat = wat(&runtime_tuple());
    assert_contains(&wat, "struct.new $tuple/1");
    assert_absent(&wat, "const/");
}

#[test]
fn constant_bytes_literals_hoist_into_a_start_initialized_global() {
    let wat = wat(&bytes_len());
    assert_contains(&wat, "global.set $const/");
    assert_contains(&wat, "global.get $const/");
    // The program's one constant; a refusal's sentence is built by its helper from a `refusal/…` segment and is not counted here.
    assert_eq!(count(&wat, "array.new_data $bytes $const/"), 1);
}

/// A scalar past the i31 allocates its magnitude, so it hoists as a `Flt` does, and its limbs come from a data segment the start function reads — which is also where the aggregate over it is built.
#[test]
fn big_scalars_and_their_aggregates_hoist_into_start_initialized_globals() {
    let wat = wat(&big_tuple());
    assert_contains(&wat, "array.new_data $words $const/");
    assert_contains(&wat, "struct.new $big");
    assert_contains(&wat, "global.set $const/");
    assert_eq!(refusals_besides_the_exit(&wat), 0);
}

/// Every binding hoisting interns is constant data as `curios_cps::Module::constant_data` states it: the values a rebuild in place never writes into, since one interned global serves every use of an equal constant. A name structurization mints for itself is no value of the module, and is not asked about.
#[test]
fn every_hoisted_binding_is_constant_data() {
    let mut asked = 0;
    for module in [
        constant_tuple_pair(),
        runtime_tuple(),
        tuple_project(),
        bytes_len(),
        list_len(),
        big_tuple(),
    ] {
        let representation = curios_cps::representation(&module);
        let mut structured = structurize(&lower(&module, &representation.results));
        let before = bound(&mut structured);
        hoist_consts(&mut structured);
        let hoisted = before
            .difference(&bound(&mut structured))
            .cloned()
            .collect::<BTreeSet<_>>();
        let named = |values: &mut dyn Iterator<Item = curios_cps::ValueId>| {
            values
                .map(|value| value_name(value_id(value)).as_string())
                .collect::<BTreeSet<_>>()
        };
        let constant = named(&mut module.constant_data().into_iter());
        let values = named(&mut module.value_ids());
        let stray = hoisted
            .iter()
            .filter(|name| values.contains(*name) && !constant.contains(*name))
            .collect::<Vec<_>>();
        assert!(
            stray.is_empty(),
            "{stray:?} hoisted beside constant data {constant:?}"
        );
        asked += hoisted.iter().filter(|name| values.contains(*name)).count();
    }
    assert!(asked > 0, "no fixture hoists a value of its module");
}

/// The names every region of `structured` binds.
fn bound(structured: &mut EmissionModule) -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    for region in structured.regions_mut() {
        collect(region, &mut names);
    }
    names
}

fn collect(region: &EmissionBody, names: &mut BTreeSet<String>) {
    for (name, _) in &region.values {
        names.insert(name.as_string());
    }
    for (_, block) in &region.blocks {
        collect(&block.region, names);
    }
}
