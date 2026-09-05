//! Constant-data hoisting: intern every body binding whose value is statically constant as a module const, so codegen materialises it once in a wasm global instead of allocating it at every execution.
//!
//! `Flt`, `Bin`, and the aggregates allocate on every construction, so every constant one hoists; a `Nat`/`Int` scalar is an i31 immediate that costs nothing inline, so it hoists only on demand — when a hoisted aggregate names it, because const emission resolves aggregate elements through the const table. Interning is structural, so equal constants share one global module-wide, and the const vector stays in discovery order, which is dependency order for the start function's initialisation. Use sites need only a rename to the interned name: value resolution already falls back to the const table when a name is neither a closure field nor a local.

use {
    super::{
        EmissionArg, EmissionBody, EmissionCallTarget, EmissionCellTarget, EmissionCode,
        EmissionData, EmissionHostTarget, EmissionModule, EmissionTail, EmissionValue,
        EmissionValueName, RowLoad,
    },
    curios_utilities::{Grain, PackedBin},
    std::collections::HashMap,
};

/// A structural identity for constant data, with element names already canonicalized to interned const names — so two aggregates of equal shape and equal constant elements collide however they were spelled.
#[derive(Clone, PartialEq, Eq, Hash)]
enum ConstKey {
    Nat(u32),
    Int(i32),
    Flt(u32),
    /// The [`PackedBin`] itself, which carries its logical length. Packing alone underdetermines a bit-grain value — `b[1]` and `b[1, 0]` pack identically — and a key built from packed bytes interned them into one constant, whose emitted length was whichever literal arrived first.
    Bin(Grain, PackedBin),
    List(Vec<String>),
    Tuple(Vec<String>),
    /// A nominal row is its identity, its load mode and its canonicalized slots, a filler keying as `None` — kept apart from `Tuple` because the two materialise at different heap types, so a structurally identical row is not the same constant. The load mode rides the key so two rows that would emit different loads can never share one global, whether or not a tolerant one can reach here.
    Row(usize, RowLoad, Vec<Option<String>>),
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

pub(crate) fn hoist_consts(module: &mut EmissionModule) {
    let mut interner = ConstInterner::default();
    for (_, clsr) in &mut module.clsrs {
        hoist_region(&mut clsr.region, &mut interner);
    }
    for (_, func) in &mut module.funcs {
        hoist_region(&mut func.region, &mut interner);
    }
    module.consts = interner.consts;
}

/// Hoist one function's region tree: collect and intern its constants, then drop the hoisted bindings and rename every surviving occurrence. Two phases because a scalar may be demanded by an aggregate bound after uses of the scalar were already walked.
fn hoist_region(region: &mut EmissionBody, interner: &mut ConstInterner) {
    let mut consts = FunctionConsts::default();
    collect_consts(region, interner, &mut consts);
    apply_renames(region, &consts.renames);
}

fn collect_consts(body: &EmissionBody, interner: &mut ConstInterner, consts: &mut FunctionConsts) {
    for (name, value) in &body.values {
        let EmissionValue::Pure(data) = value else {
            continue;
        };
        match data {
            // An out-of-range scalar materialises as a trap, which is no constant instruction and must stay at its execution point rather than fail validation or trap at instantiation — so it is never a candidate, which also keeps every aggregate over it inline.
            EmissionData::Nat(value) => {
                if value >> 31 == 0 {
                    consts
                        .scalars
                        .insert(name.clone(), (ConstKey::Nat(*value), data.clone()));
                }
            }
            EmissionData::Int(value) => {
                if value >> 30 == value >> 31 {
                    consts
                        .scalars
                        .insert(name.clone(), (ConstKey::Int(*value), data.clone()));
                }
            }
            EmissionData::Flt(value) => {
                let interned = interner.intern(ConstKey::Flt(value.to_bits()), data.clone());
                consts.renames.insert(name.clone(), interned);
            }
            EmissionData::Bin(grain, value) => {
                let interned = interner.intern(ConstKey::Bin(*grain, value.clone()), data.clone());
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
            // A tolerant rebuild is never a constant: its slots are field parameters, which `intern_slots` declines, so the load mode has no key to join.
            EmissionData::Row(row, slots, load) => {
                if let Some(canonical) = intern_slots(slots, interner, consts) {
                    let key = ConstKey::Row(
                        row.index(),
                        *load,
                        canonical
                            .iter()
                            .map(|slot| match slot {
                                EmissionArg::Value(name) => Some(name.as_string()),
                                EmissionArg::Filler => None,
                            })
                            .collect(),
                    );
                    let interned = interner.intern(key, EmissionData::Row(*row, canonical, *load));
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
    for (_, block) in &body.blocks {
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

fn apply_renames(body: &mut EmissionBody, renames: &HashMap<EmissionValueName, EmissionValueName>) {
    body.values.retain(|(name, _)| !renames.contains_key(name));
    for (_, value) in &mut body.values {
        rename_value(value, renames);
    }
    rename_tail(&mut body.tail, renames);
    for (_, block) in &mut body.blocks {
        apply_renames(&mut block.region, renames);
    }
}

fn rename_name(
    name: &mut EmissionValueName,
    renames: &HashMap<EmissionValueName, EmissionValueName>,
) {
    if let Some(interned) = renames.get(name) {
        *name = interned.clone();
    }
}

fn rename_names(
    names: &mut [EmissionValueName],
    renames: &HashMap<EmissionValueName, EmissionValueName>,
) {
    for name in names {
        rename_name(name, renames);
    }
}

fn rename_jump_args(
    args: &mut [EmissionArg],
    renames: &HashMap<EmissionValueName, EmissionValueName>,
) {
    for arg in args {
        // A filler names nothing, so there is nothing to intern it against.
        if let EmissionArg::Value(name) = arg {
            rename_name(name, renames);
        }
    }
}

fn rename_value(
    value: &mut EmissionValue,
    renames: &HashMap<EmissionValueName, EmissionValueName>,
) {
    match value {
        EmissionValue::Pure(data) => match data {
            EmissionData::Nat(_)
            | EmissionData::Int(_)
            | EmissionData::Flt(_)
            | EmissionData::Bin(_, _) => {}
            EmissionData::List(elems) | EmissionData::Tuple(elems) => rename_names(elems, renames),
            EmissionData::Row(_, slots, _) => rename_jump_args(slots, renames),
            EmissionData::Closure(_, fields) => rename_names(fields, renames),
        },
        EmissionValue::Eval(code) => match code {
            EmissionCode::Intrinsic(_, args) => rename_names(args, renames),
            EmissionCode::ListMap(list, mapper) => {
                rename_name(list, renames);
                rename_name(mapper, renames);
            }
        },
    }
}

fn rename_tail(tail: &mut EmissionTail, renames: &HashMap<EmissionValueName, EmissionValueName>) {
    match tail {
        EmissionTail::Jump(target) => rename_jump_args(&mut target.params, renames),
        EmissionTail::Match(target) => {
            rename_name(&mut target.operand, renames);
            for target in target.cases.values_mut().chain(target.default.iter_mut()) {
                rename_jump_args(&mut target.params, renames);
            }
        }
        EmissionTail::Call(EmissionCallTarget::Direct { params, .. }) => {
            rename_names(params, renames)
        }
        EmissionTail::Call(EmissionCallTarget::Indirect { target, params, .. }) => {
            rename_name(target, renames);
            rename_names(params, renames);
        }
        EmissionTail::Host(EmissionHostTarget::Foreign { operands, .. }) => {
            rename_names(operands, renames)
        }
        EmissionTail::Host(EmissionHostTarget::Exit { code }) => rename_name(code, renames),
        EmissionTail::Cell(EmissionCellTarget::New { init, .. }) => rename_name(init, renames),
        EmissionTail::Cell(EmissionCellTarget::Reserve { .. }) => {}
        EmissionTail::Cell(EmissionCellTarget::Set { cell, value, .. }) => {
            rename_name(cell, renames);
            rename_name(value, renames);
        }
        EmissionTail::Cell(EmissionCellTarget::Get { cell, .. }) => rename_name(cell, renames),
        EmissionTail::Unreachable => {}
    }
}
