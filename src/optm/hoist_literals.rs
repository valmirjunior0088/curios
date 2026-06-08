use {
    super::*,
    std::collections::{HashMap, HashSet},
};

/// Literal hoisting: lift compile-time-constant values out of bodies into shared
/// module consts.
///
/// An inline `Value::Pure(Data::…)` aggregate rebuilds its allocation **every time
/// the binding executes** — `array.new_data` for a `Bin`, `array.new_fixed` for an
/// `Arr`, `struct.new` for a `Tpl`/`Clsr` (`cont/to_wasm/expr_emitter.rs`). The same
/// value emitted as a module const runs that allocation **once** in the `start`
/// function and stashes it in a global, so every use is a cheap shared `global.get`
/// (`cont/to_wasm/module_emitter.rs`). For a constant in a loop, a closure body, or
/// any hot path, that turns a per-execution allocation into a one-time init.
///
/// This hoists every **bytestring** and every **closed aggregate/closure** — an
/// `Arr`/`Tpl`/`Clsr` all of whose elements (or captures) are themselves constants,
/// transitively. Closedness is the constraint because a const aggregate's elements
/// are emitted as `global.get` of other consts (`cont/to_wasm/context.rs`), with no
/// inline-scalar path: hoisting one aggregate lifts its whole constant sub-DAG —
/// scalar and `Bin` leaves included — into consts. Leaves are minted before the
/// aggregates that name them, so the emitted const table is in the dependency order
/// a wasm global initializer requires (a `global.get` may only read an earlier
/// global). Standalone scalars are *not* hoisted (an inline `i31` is cheaper than a
/// `global.get`); a scalar becomes a const only as the leaf of a hoisted aggregate.
///
/// Each distinct constant becomes one const named `lit$<kind>$<n>` (the `lit$`
/// prefix cannot collide with the lowerer's `v…`/`b…` body names), deduplicated
/// structurally across the whole module, and each hoisted binding is rewritten to a
/// `Value::Alias` of it. Copy propagation then collapses the alias into its uses and
/// dead-code elimination reclaims any const left unreferenced (its const→const and
/// const→closure edges are what keep a hoisted aggregate's leaves alive) — so this
/// leans on the same alias-then-propagate idiom as inlining and jump threading.
///
/// Soundness: these values are immutable (the IR has no field-store, the invariant
/// [`constant_folding`] relies on) and nothing compares them by reference identity
/// (`BinEql` is bytewise), so sharing one instance across every use — and across
/// bodies, via dedup — is observationally identical to rebuilding it.
pub fn hoist_literals(module: &mut Module) {
    let mut interner = Interner::default();

    // Collect against the (immutably borrowed) bodies, recording per body which
    // bindings to rewrite; the interner accumulates the new consts it owns.
    let mut func_rewrites = Vec::with_capacity(module.funcs().len());
    for (_, func) in module.funcs() {
        func_rewrites.push(collect_body(&func.region, &mut interner));
    }
    let mut clsr_rewrites = Vec::with_capacity(module.clsrs().len());
    for (_, clsr) in module.clsrs() {
        clsr_rewrites.push(collect_body(&clsr.region, &mut interner));
    }

    if interner.consts.is_empty() {
        return;
    }

    // Leaves were interned before the aggregates that name them, so this preserves
    // the dependency order a wasm global initializer requires.
    for (name, data) in interner.consts {
        module.add_const(name, data);
    }

    for ((_, func), rewrites) in module.funcs_mut().iter_mut().zip(&func_rewrites) {
        rewrite_body(&mut func.region, rewrites);
    }
    for ((_, clsr), rewrites) in module.clsrs_mut().iter_mut().zip(&clsr_rewrites) {
        rewrite_body(&mut clsr.region, rewrites);
    }
}

// --- Per-body collection ----------------------------------------------------

/// Hoist every entry point in one body, returning the bindings to rewrite to an
/// alias of their hoisted const.
fn collect_body(region: &Region, interner: &mut Interner) -> HashMap<ValueName, ValueName> {
    let preallocs = prealloc_names(region);

    // Every constant binding, by name — the lookup `lower` follows through. A
    // prealloc'd name is a `rec` closure's backpatched fill: inherently recursive,
    // so it is never a constant and never an entry point.
    let mut defs: HashMap<ValueName, &Data> = HashMap::new();
    collect_defs(region, &preallocs, &mut defs);

    let mut rewrites = HashMap::new();
    let mut memo: HashMap<ValueName, ValueName> = HashMap::new();
    collect_entry_points(
        region,
        &preallocs,
        &defs,
        interner,
        &mut memo,
        &mut rewrites,
    );
    rewrites
}

fn prealloc_names(region: &Region) -> HashSet<ValueName> {
    let mut names = HashSet::new();
    collect_prealloc_names(region, &mut names);
    names
}

fn collect_prealloc_names(region: &Region, names: &mut HashSet<ValueName>) {
    for (name, _) in &region.preallocs {
        names.insert(name.clone());
    }
    for (_, block) in &region.blocks {
        collect_prealloc_names(&block.region, names);
    }
}

fn collect_defs<'a>(
    region: &'a Region,
    preallocs: &HashSet<ValueName>,
    defs: &mut HashMap<ValueName, &'a Data>,
) {
    for (name, value) in &region.values {
        if let Value::Pure(data) = value
            && !preallocs.contains(name)
        {
            defs.insert(name.clone(), data);
        }
    }

    for (_, block) in &region.blocks {
        collect_defs(&block.region, preallocs, defs);
    }
}

/// Attempt to hoist every `Bin` or aggregate binding (standalone scalars are left
/// inline). A non-closed aggregate fails to lower and is simply skipped.
fn collect_entry_points(
    region: &Region,
    preallocs: &HashSet<ValueName>,
    defs: &HashMap<ValueName, &Data>,
    interner: &mut Interner,
    memo: &mut HashMap<ValueName, ValueName>,
    rewrites: &mut HashMap<ValueName, ValueName>,
) {
    for (name, value) in &region.values {
        if let Value::Pure(data) = value
            && !preallocs.contains(name)
            && is_entry_point(data)
            && let Some(const_name) = lower(name, defs, interner, memo)
        {
            rewrites.insert(name.clone(), const_name);
        }
    }

    for (_, block) in &region.blocks {
        collect_entry_points(&block.region, preallocs, defs, interner, memo, rewrites);
    }
}

/// A binding worth hoisting on its own: a bytestring or an aggregate. Scalars are
/// hoisted only as the leaves of a containing aggregate, never standalone.
fn is_entry_point(data: &Data) -> bool {
    matches!(
        data,
        Data::Bin(_) | Data::Arr(_) | Data::Tpl(_) | Data::Clsr(..)
    )
}

// --- Lowering ---------------------------------------------------------------

/// Lower the constant bound to `name` into an interned const, returning its name.
/// `None` if `name` is not a constant binding (a runtime value, parameter, or block
/// parameter) or sits on a reference cycle (a self-capturing recursive closure) —
/// either of which makes any enclosing aggregate non-closed.
fn lower(
    name: &ValueName,
    defs: &HashMap<ValueName, &Data>,
    interner: &mut Interner,
    memo: &mut HashMap<ValueName, ValueName>,
) -> Option<ValueName> {
    lower_named(name, defs, interner, memo, &mut HashSet::new())
}

fn lower_named(
    name: &ValueName,
    defs: &HashMap<ValueName, &Data>,
    interner: &mut Interner,
    memo: &mut HashMap<ValueName, ValueName>,
    stack: &mut HashSet<ValueName>,
) -> Option<ValueName> {
    if let Some(const_name) = memo.get(name) {
        return Some(const_name.clone());
    }
    if !stack.insert(name.clone()) {
        return None; // cycle
    }

    let result = defs
        .get(name)
        .copied()
        .and_then(|data| lower_data(data, defs, interner, memo, stack));

    stack.remove(name);

    if let Some(const_name) = &result {
        memo.insert(name.clone(), const_name.clone());
    }
    result
}

fn lower_data(
    data: &Data,
    defs: &HashMap<ValueName, &Data>,
    interner: &mut Interner,
    memo: &mut HashMap<ValueName, ValueName>,
    stack: &mut HashSet<ValueName>,
) -> Option<ValueName> {
    let (key, emit) = match data {
        Data::Nat(value) => (ConstKey::Nat(*value), Data::Nat(*value)),
        Data::Int(value) => (ConstKey::Int(*value), Data::Int(*value)),
        Data::Flt(value) => (ConstKey::Flt(value.to_bits()), Data::Flt(*value)),
        Data::Bin(bytes) => (ConstKey::Bin(bytes.clone()), Data::Bin(bytes.clone())),
        Data::Arr(elems) => {
            let lowered = lower_children(elems, defs, interner, memo, stack)?;
            (ConstKey::Arr(lowered.clone()), Data::Arr(lowered))
        }
        Data::Tpl(elems) => {
            let lowered = lower_children(elems, defs, interner, memo, stack)?;
            (ConstKey::Tpl(lowered.clone()), Data::Tpl(lowered))
        }
        Data::Clsr(clsr, captures) => {
            let lowered = lower_children(captures, defs, interner, memo, stack)?;
            (
                ConstKey::Clsr(clsr.clone(), lowered.clone()),
                Data::Clsr(clsr.clone(), lowered),
            )
        }
    };

    Some(interner.intern(key, emit))
}

/// Lower every child to a const name, failing the whole aggregate if any child is
/// not a constant.
fn lower_children(
    names: &[ValueName],
    defs: &HashMap<ValueName, &Data>,
    interner: &mut Interner,
    memo: &mut HashMap<ValueName, ValueName>,
    stack: &mut HashSet<ValueName>,
) -> Option<Vec<ValueName>> {
    names
        .iter()
        .map(|name| lower_named(name, defs, interner, memo, stack))
        .collect()
}

// --- Interner ---------------------------------------------------------------

/// A hashable identity for a constant: structurally equal constants intern to one
/// const. `Flt` is keyed by its bit pattern (`f32` is not `Eq`/`Hash`); the
/// aggregate variants hold their already-lowered child const names.
#[derive(PartialEq, Eq, Hash)]
enum ConstKey {
    Nat(u32),
    Int(i32),
    Flt(u32),
    Bin(Vec<u8>),
    Arr(Vec<ValueName>),
    Tpl(Vec<ValueName>),
    Clsr(ClsrName, Vec<ValueName>),
}

/// Mints one const per distinct constant, module-wide. `consts` is in mint order;
/// because a child is always interned before its parent, that order is topological
/// (leaves first) — the order wasm global initializers require.
#[derive(Default)]
struct Interner {
    dedup: HashMap<ConstKey, ValueName>,
    consts: Vec<(ValueName, Data)>,
    counters: HashMap<&'static str, usize>,
}

impl Interner {
    fn intern(&mut self, key: ConstKey, data: Data) -> ValueName {
        if let Some(name) = self.dedup.get(&key) {
            return name.clone();
        }

        let kind = kind_of(&data);
        let index = self.counters.entry(kind).or_insert(0);
        let name = ValueName::from(format!("lit${kind}${index}"));
        *index += 1;

        self.dedup.insert(key, name.clone());
        self.consts.push((name.clone(), data));
        name
    }
}

fn kind_of(data: &Data) -> &'static str {
    match data {
        Data::Nat(_) => "nat",
        Data::Int(_) => "int",
        Data::Flt(_) => "flt",
        Data::Bin(_) => "bin",
        Data::Arr(_) => "arr",
        Data::Tpl(_) => "tpl",
        Data::Clsr(..) => "clsr",
    }
}

// --- Rewrite ----------------------------------------------------------------

/// Replace each hoisted binding in the tree with an alias of its const.
fn rewrite_body(region: &mut Region, rewrites: &HashMap<ValueName, ValueName>) {
    for (name, value) in &mut region.values {
        if let Some(const_name) = rewrites.get(name) {
            *value = Value::Alias(const_name.clone());
        }
    }

    for (_, block) in &mut region.blocks {
        rewrite_body(&mut block.region, rewrites);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(name: &str) -> ValueName {
        ValueName::from(name)
    }

    fn b(name: &str) -> BlockName {
        BlockName::from(name)
    }

    fn region(values: Vec<(ValueName, Value)>, blocks: Vec<(BlockName, Block)>) -> Region {
        Region {
            preallocs: vec![],
            values,
            blocks,
            tail: Tail::Jump(JumpTarget {
                target: BlockName::from("rm"),
                params: vec![],
            }),
        }
    }

    fn main_func(region: Region) -> Module {
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![],
                resume: BlockName::from("rm"),
                region,
            },
        );
        module.set_entry(FuncName::from("main"));
        module
    }

    fn main_region(module: &Module) -> &Region {
        &module.funcs()[0].1.region
    }

    fn const_names(module: &Module) -> Vec<String> {
        module
            .consts()
            .iter()
            .map(|(name, _)| name.as_string())
            .collect()
    }

    fn alias(value: &Value) -> &ValueName {
        match value {
            Value::Alias(source) => source,
            other => panic!("expected an alias, got {other:?}"),
        }
    }

    // --- Bytestrings (phase-1 behavior, preserved) --------------------------

    #[test]
    fn hoists_distinct_bytestrings_and_dedups_identical_ones() {
        // v0 and v2 share bytes; v1 is distinct → two consts, v0 and v2 alias one.
        let mut module = main_func(region(
            vec![
                (v("v0"), Value::Pure(Data::Bin(vec![1, 2, 3]))),
                (v("v1"), Value::Pure(Data::Bin(vec![9]))),
                (v("v2"), Value::Pure(Data::Bin(vec![1, 2, 3]))),
            ],
            vec![],
        ));

        hoist_literals(&mut module);

        let consts: Vec<(String, Data)> = module
            .consts()
            .iter()
            .map(|(name, data)| (name.as_string(), data.clone()))
            .collect();
        assert_eq!(consts.len(), 2);
        assert_eq!(consts[0].0, "lit$bin$0");
        assert!(matches!(&consts[0].1, Data::Bin(bytes) if bytes == &vec![1, 2, 3]));
        assert_eq!(consts[1].0, "lit$bin$1");
        assert!(matches!(&consts[1].1, Data::Bin(bytes) if bytes == &vec![9]));

        let values = &main_region(&module).values;
        assert_eq!(alias(&values[0].1), &v("lit$bin$0"));
        assert_eq!(alias(&values[1].1), &v("lit$bin$1"));
        assert_eq!(alias(&values[2].1), &v("lit$bin$0"));
    }

    #[test]
    fn hoists_bytestrings_inside_nested_blocks() {
        let inner = Block {
            params: vec![],
            region: region(vec![(v("w"), Value::Pure(Data::Bin(vec![7, 7])))], vec![]),
        };
        let mut module = main_func(region(vec![], vec![(b("blk"), inner)]));

        hoist_literals(&mut module);

        assert_eq!(module.consts().len(), 1);
        let (_, block) = &main_region(&module).blocks[0];
        assert_eq!(alias(&block.region.values[0].1), &v("lit$bin$0"));
    }

    #[test]
    fn leaves_bodies_without_constants_untouched() {
        // A standalone scalar is not an entry point — it stays inline.
        let mut module = main_func(region(vec![(v("v0"), Value::Pure(Data::Nat(1)))], vec![]));

        hoist_literals(&mut module);

        assert!(module.consts().is_empty());
        assert!(matches!(
            &main_region(&module).values[0].1,
            Value::Pure(Data::Nat(1))
        ));
    }

    // --- Closed aggregates and closures -------------------------------------

    #[test]
    fn hoists_closed_tuple_lifting_scalar_leaves_in_dependency_order() {
        // a=1; b=2; t=(a,b) → consts lit$nat$0, lit$nat$1, lit$tpl$0 (leaves first).
        let mut module = main_func(region(
            vec![
                (v("a"), Value::Pure(Data::Nat(1))),
                (v("b"), Value::Pure(Data::Nat(2))),
                (v("t"), Value::Pure(Data::Tpl(vec![v("a"), v("b")]))),
            ],
            vec![],
        ));

        hoist_literals(&mut module);

        assert_eq!(
            const_names(&module),
            vec!["lit$nat$0", "lit$nat$1", "lit$tpl$0"]
        );
        match &module.consts()[2].1 {
            Data::Tpl(elems) => assert_eq!(elems, &vec![v("lit$nat$0"), v("lit$nat$1")]),
            other => panic!("expected tuple const, got {other:?}"),
        }

        // The tuple binding aliases the const; the scalar bindings stay inline.
        let values = &main_region(&module).values;
        assert!(matches!(&values[0].1, Value::Pure(Data::Nat(1))));
        assert!(matches!(&values[1].1, Value::Pure(Data::Nat(2))));
        assert_eq!(alias(&values[2].1), &v("lit$tpl$0"));
    }

    #[test]
    fn hoists_nested_closed_aggregate_in_dependency_order() {
        // a=7; inner=[a]; t=(inner) → scalar, then array, then tuple.
        let mut module = main_func(region(
            vec![
                (v("a"), Value::Pure(Data::Nat(7))),
                (v("inner"), Value::Pure(Data::Arr(vec![v("a")]))),
                (v("t"), Value::Pure(Data::Tpl(vec![v("inner")]))),
            ],
            vec![],
        ));

        hoist_literals(&mut module);

        assert_eq!(
            const_names(&module),
            vec!["lit$nat$0", "lit$arr$0", "lit$tpl$0"]
        );
        let values = &main_region(&module).values;
        assert_eq!(alias(&values[1].1), &v("lit$arr$0"));
        assert_eq!(alias(&values[2].1), &v("lit$tpl$0"));
    }

    #[test]
    fn hoists_closed_closure_capturing_a_constant() {
        let mut module = main_func(region(
            vec![
                (v("bias"), Value::Pure(Data::Int(5))),
                (
                    v("k"),
                    Value::Pure(Data::Clsr(ClsrName::from("c"), vec![v("bias")])),
                ),
            ],
            vec![],
        ));

        hoist_literals(&mut module);

        assert_eq!(const_names(&module), vec!["lit$int$0", "lit$clsr$0"]);
        match &module.consts()[1].1 {
            Data::Clsr(clsr, captures) => {
                assert_eq!(clsr.as_str(), "c");
                assert_eq!(captures, &vec![v("lit$int$0")]);
            }
            other => panic!("expected closure const, got {other:?}"),
        }
        assert_eq!(alias(&main_region(&module).values[1].1), &v("lit$clsr$0"));
    }

    #[test]
    fn leaves_an_aggregate_with_a_runtime_element_inline() {
        // The parameter `p` is not a constant, so `(p,)` is not closed.
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![v("p").into()],
                resume: BlockName::from("rm"),
                region: region(vec![(v("t"), Value::Pure(Data::Tpl(vec![v("p")])))], vec![]),
            },
        );
        module.set_entry(FuncName::from("main"));

        hoist_literals(&mut module);

        assert!(module.consts().is_empty());
        assert!(matches!(
            &main_region(&module).values[0].1,
            Value::Pure(Data::Tpl(_))
        ));
    }

    #[test]
    fn leaves_a_self_capturing_recursive_closure_inline() {
        // `rec` is prealloc'd and its fill captures itself — never a constant.
        let mut module = main_func(Region {
            preallocs: vec![(v("rec"), Prealloc::Clsr(ClsrName::from("c")))],
            values: vec![(
                v("rec"),
                Value::Pure(Data::Clsr(ClsrName::from("c"), vec![v("rec")])),
            )],
            blocks: vec![],
            tail: Tail::Jump(JumpTarget {
                target: BlockName::from("rm"),
                params: vec![v("rec")],
            }),
        });

        hoist_literals(&mut module);

        assert!(module.consts().is_empty());
        assert!(matches!(
            &main_region(&module).values[0].1,
            Value::Pure(Data::Clsr(..))
        ));
    }

    #[test]
    fn dedups_identical_closed_tuples_across_bodies() {
        let body = || {
            region(
                vec![
                    (v("a"), Value::Pure(Data::Nat(1))),
                    (v("t"), Value::Pure(Data::Tpl(vec![v("a")]))),
                ],
                vec![],
            )
        };

        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![],
                resume: BlockName::from("rm"),
                region: body(),
            },
        );
        module.add_func(
            FuncName::from("other"),
            Func {
                params: vec![],
                resume: BlockName::from("rm"),
                region: body(),
            },
        );
        module.set_entry(FuncName::from("main"));

        hoist_literals(&mut module);

        // One scalar const and one tuple const, shared by both bodies.
        assert_eq!(const_names(&module), vec!["lit$nat$0", "lit$tpl$0"]);
    }
}
