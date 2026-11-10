//! Constant-data hoisting: intern every body binding whose value is statically constant as a module const, so codegen materialises it once in a wasm global instead of allocating it at every execution.
//!
//! `Flt`, `Bytes`, a `Nat` or `Int` past the i31, and the aggregates allocate on every construction, so every constant one hoists; a small `Nat` or `Int` is an i31 immediate that costs nothing inline, so it hoists only on demand — when a hoisted aggregate names it, because const emission resolves aggregate elements through the const table. Interning is structural, so equal constants share one global module-wide, and the const vector stays in discovery order, which is dependency order for the start function's initialisation. Use sites need only a rename to the interned name: value resolution already falls back to the const table when a name is neither a closure field nor a local.

use {
    super::{
        EmissionArg, EmissionBody, EmissionData, EmissionModule, EmissionValue, EmissionValueName,
    },
    curios_num::{Binary, Integer, Natural},
    std::collections::HashMap,
};

/// A structural identity for constant data, with element names already canonicalized to interned const names — so two aggregates of equal shape and equal constant elements collide however they were spelled.
#[derive(Clone, PartialEq, Eq, Hash)]
enum ConstKey {
    Nat(Natural),
    Int(Integer),
    Flt(u64),
    Bytes(Binary),
    List(Vec<String>),
    Tuple(Vec<String>),
    /// A nominal row is its identity and its canonicalized slots, a filler keying as `None` — kept apart from `Tuple` because the two materialise at different heap types, so a structurally identical row is not the same constant.
    Row(usize, Vec<Option<String>>),
    /// A closure is its target plus its canonicalized captures: with the code field an ordinary table index, a const-captured closure is a constant aggregate like any `Tuple`, materialized once per instantiation instead of per construction.
    Clsr(String, Vec<String>),
}

#[derive(Default)]
struct ConstInterner {
    consts: Vec<(EmissionValueName, EmissionData)>,
    named: HashMap<ConstKey, EmissionValueName>,
}

impl ConstInterner {
    fn intern(&mut self, key: ConstKey, data: EmissionData) -> EmissionValueName {
        if let Some(name) = self.named.get(&key) {
            return name.clone();
        }
        let name = EmissionValueName::from(format!("const/{}", self.consts.len()));
        self.consts.push((name.clone(), data));
        self.named.insert(key, name.clone());
        name
    }
}

/// The per-function hoisting state: bindings already renamed to interned consts, and the kept scalar bindings still available for demand-hoisting. Names are unique within a function, so one flat map serves the whole region tree.
#[derive(Default)]
struct FunctionConsts {
    renames: HashMap<EmissionValueName, EmissionValueName>,
    scalars: HashMap<EmissionValueName, (ConstKey, EmissionData)>,
}

pub fn hoist_consts(module: &mut EmissionModule) {
    let mut interner = ConstInterner::default();
    for region in module.regions_mut() {
        hoist_region(region, &mut interner);
    }
    module.set_consts(interner.consts);
}

/// Hoist one function's region tree: collect and intern its constants, then drop the hoisted bindings and rename every surviving occurrence. Two phases because a scalar may be demanded by an aggregate bound after uses of the scalar were already walked.
fn hoist_region(region: &mut EmissionBody, interner: &mut ConstInterner) {
    let mut consts = FunctionConsts::default();
    collect_consts(region, interner, &mut consts);
    region.rename_values(&consts.renames);
}

fn collect_consts(body: &EmissionBody, interner: &mut ConstInterner, consts: &mut FunctionConsts) {
    for (name, value) in body.values() {
        let EmissionValue::Pure(data) = value else {
            continue;
        };
        match data {
            EmissionData::Nat(value) => {
                let key = ConstKey::Nat(value.clone());
                match curios_cps::nat_is_small(value) {
                    true => {
                        consts.scalars.insert(name.clone(), (key, data.clone()));
                    }
                    false => {
                        let interned = interner.intern(key, data.clone());
                        consts.renames.insert(name.clone(), interned);
                    }
                }
            }
            EmissionData::Int(value) => {
                let key = ConstKey::Int(value.clone());
                match curios_cps::int_is_small(value) {
                    true => {
                        consts.scalars.insert(name.clone(), (key, data.clone()));
                    }
                    false => {
                        let interned = interner.intern(key, data.clone());
                        consts.renames.insert(name.clone(), interned);
                    }
                }
            }
            EmissionData::Flt(value) => {
                let interned = interner.intern(ConstKey::Flt(value.to_bits()), data.clone());
                consts.renames.insert(name.clone(), interned);
            }
            EmissionData::Bytes(value) => {
                let interned = interner.intern(ConstKey::Bytes(value.clone()), data.clone());
                consts.renames.insert(name.clone(), interned);
            }
            EmissionData::Tuple(elems) => {
                if let Some(canonical) = intern_elements(elems, interner, consts) {
                    let key =
                        ConstKey::Tuple(canonical.iter().map(|name| name.as_string()).collect());
                    let interned = interner.intern(key, EmissionData::Tuple(canonical));
                    consts.renames.insert(name.clone(), interned);
                }
            }
            EmissionData::List(elems) => {
                if let Some(canonical) = intern_elements(elems, interner, consts) {
                    let key =
                        ConstKey::List(canonical.iter().map(|name| name.as_string()).collect());
                    let interned = interner.intern(key, EmissionData::List(canonical));
                    consts.renames.insert(name.clone(), interned);
                }
            }
            EmissionData::Row(row, slots) => {
                if let Some(canonical) = intern_slots(slots, interner, consts) {
                    let key = ConstKey::Row(
                        row.index(),
                        canonical
                            .iter()
                            .map(|slot| match slot {
                                EmissionArg::Value(name) => Some(name.as_string()),
                                EmissionArg::Filler => None,
                            })
                            .collect(),
                    );
                    let interned = interner.intern(key, EmissionData::Row(*row, canonical));
                    consts.renames.insert(name.clone(), interned);
                }
            }
            EmissionData::Closure(target, fields) => {
                if let Some(canonical) = intern_elements(fields, interner, consts) {
                    let key = ConstKey::Clsr(
                        target.as_string(),
                        canonical.iter().map(|name| name.as_string()).collect(),
                    );
                    let interned =
                        interner.intern(key, EmissionData::Closure(target.clone(), canonical));
                    consts.renames.insert(name.clone(), interned);
                }
            }
        }
    }
    for (_, block) in body.blocks() {
        collect_consts(&block.region, interner, consts);
    }
}

/// The [`intern_elements`] rule over a variant's slots: a filler names nothing, so it is constant by construction and carries through unresolved.
fn intern_slots(
    slots: &[EmissionArg],
    interner: &mut ConstInterner,
    consts: &mut FunctionConsts,
) -> Option<Vec<EmissionArg>> {
    let named: Vec<EmissionValueName> = slots
        .iter()
        .filter_map(|slot| match slot {
            EmissionArg::Value(name) => Some(name.clone()),
            EmissionArg::Filler => None,
        })
        .collect();
    let mut canonical = intern_elements(&named, interner, consts)?.into_iter();
    Some(
        slots
            .iter()
            .map(|slot| match slot {
                EmissionArg::Value(_) => {
                    EmissionArg::Value(canonical.next().expect("one canonical name per named slot"))
                }
                EmissionArg::Filler => EmissionArg::Filler,
            })
            .collect(),
    )
}

/// Resolve every element of an aggregate to its interned const name, demand-hoisting kept scalars, or report the aggregate non-constant. Checks all elements before interning any, so a failing aggregate hoists no scalar.
fn intern_elements(
    elems: &[EmissionValueName],
    interner: &mut ConstInterner,
    consts: &mut FunctionConsts,
) -> Option<Vec<EmissionValueName>> {
    if elems
        .iter()
        .any(|elem| !consts.renames.contains_key(elem) && !consts.scalars.contains_key(elem))
    {
        return None;
    }
    let canonical = elems
        .iter()
        .map(|elem| {
            if let Some(interned) = consts.renames.get(elem) {
                return interned.clone();
            }
            let (key, data) = consts.scalars.get(elem).unwrap().clone();
            let interned = interner.intern(key, data);
            consts.renames.insert(elem.clone(), interned.clone());
            interned
        })
        .collect();
    Some(canonical)
}
