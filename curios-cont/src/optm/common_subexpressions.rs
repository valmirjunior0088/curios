use {
    super::*,
    std::collections::{HashMap, HashSet},
    std::mem::{Discriminant, discriminant},
};

/// Common-subexpression elimination: rebind a duplicate pure computation as an
/// alias of its first occurrence.
///
/// Copy propagation only collapses spelled-out renames, and constant folding
/// only acts when operands are literal — two `NatAdd(x, 1)` over the same
/// non-literal `x`, or two identical `Data::Tpl` constructions, each stay as
/// separate bindings (and, for aggregates, separate allocations). This pass
/// value-numbers every `Value::Eval` and every aggregate `Value::Pure` along
/// each region path: a binding whose key matches an earlier binding in the same
/// region or an ancestor becomes `Value::Alias(first)`, and the pipeline's next
/// copy-propagation round rewrites its uses and drops it — the same
/// leave-an-alias protocol inlining uses for parameter bindings.
///
/// Soundness leans on the two IR laws:
///
/// - *Scope is execution order.* A region's `values` run before control enters
///   any of its `blocks`, so an earlier binding on the current path has always
///   been evaluated when the duplicate would have run — including the trapping
///   ops (`NatDiv`, `ArrGet`): if the first occurrence traps, the duplicate was
///   never reached, and if it didn't, the duplicate can't.
/// - *Values are pure.* Effects live only at tail position, and every `Code`
///   op except `ArrMap` is deterministic in its operands. `ArrMap` invokes a
///   closure — whose region may end in an effectful tail — so it is never
///   keyed. Aggregates are immutable and the language has no pointer equality,
///   so deduplicating a `Tpl`/`Arr`/`Clsr` construction is unobservable;
///   prealloc shells are the one exception (their identity matters while their
///   captures are backpatched) and are skipped on both sides of a match.
///
/// Sibling blocks never see each other's entries: the key scope is a stack,
/// truncated when a block's subtree is left, so only the shared ancestor
/// prefix survives into the next arm — a match arm cannot alias into a
/// computation of a sibling arm that may never run.
pub fn eliminate_common_subexpressions(module: &mut Module) {
    for (_, func) in module.funcs_mut() {
        dedup_tree(&mut func.region);
    }
    for (_, clsr) in module.clsrs_mut() {
        dedup_tree(&mut clsr.region);
    }
}

/// The value-number of one binding: the op (or data kind) plus its operands,
/// canonicalized through already-known aliases so chains dedup in one pass.
#[derive(Clone, PartialEq, Eq, Hash)]
enum Key {
    /// A `Value::Eval`: the `Code` variant, its canonical operand names (sorted
    /// when the op is commutative), and the `TplGet` index (zero elsewhere).
    Eval(Discriminant<Code>, Vec<ValueName>, usize),
    /// An aggregate `Value::Pure`: the `Data` variant, the closure it names (if
    /// any), and its canonical element names.
    Data(Discriminant<Data>, Option<ClsrName>, Vec<ValueName>),
}

/// The keys visible on the current region path: a map for lookup plus the
/// insertion stack that lets a block's additions be popped on exit. A key is
/// only ever inserted when absent (a duplicate becomes an alias instead), so
/// popping never has a shadowed entry to restore.
#[derive(Default)]
struct Scope {
    map: HashMap<Key, ValueName>,
    inserted: Vec<Key>,
}

impl Scope {
    fn insert(&mut self, key: Key, name: ValueName) {
        self.map.insert(key.clone(), name);
        self.inserted.push(key);
    }

    fn truncate(&mut self, mark: usize) {
        for key in self.inserted.drain(mark..) {
            self.map.remove(&key);
        }
    }
}

fn dedup_tree(region: &mut Region) {
    let preallocs = collect_preallocs(region);
    let mut scope = Scope::default();
    let mut resolved = HashMap::new();

    dedup_region(region, &preallocs, &mut scope, &mut resolved);
}

/// Every prealloc shell name in the tree. Names are unique per body, so one
/// flat set is enough to recognize a shell at any depth.
fn collect_preallocs(region: &Region) -> HashSet<ValueName> {
    let mut shells = HashSet::new();
    collect_preallocs_into(region, &mut shells);

    shells
}

fn collect_preallocs_into(region: &Region, shells: &mut HashSet<ValueName>) {
    for (name, _) in &region.preallocs {
        shells.insert(name.clone());
    }

    for (_, block) in &region.blocks {
        collect_preallocs_into(&block.region, shells);
    }
}

fn dedup_region(
    region: &mut Region,
    preallocs: &HashSet<ValueName>,
    scope: &mut Scope,
    resolved: &mut HashMap<ValueName, ValueName>,
) {
    let mark = scope.inserted.len();

    for (name, value) in &mut region.values {
        match value {
            Value::Alias(source) => {
                let source = canonical(source, resolved);
                resolved.insert(name.clone(), source);
            }
            _ => {
                let Some(key) = value_key(value, preallocs, resolved) else {
                    continue;
                };

                if preallocs.contains(name) {
                    continue;
                }

                match scope.map.get(&key) {
                    Some(first) => {
                        resolved.insert(name.clone(), first.clone());
                        *value = Value::Alias(first.clone());
                    }
                    None => scope.insert(key, name.clone()),
                }
            }
        }
    }

    // Each block truncates its own additions on exit, so by the time a sibling
    // is entered the scope holds exactly the ancestor prefix again.
    for (_, block) in &mut region.blocks {
        dedup_region(&mut block.region, preallocs, scope, resolved);
    }

    scope.truncate(mark);
}

/// The binding's value-number, or `None` when the value must not participate:
/// an `ArrMap` (its closure may be effectful), a scalar or bytestring literal
/// (constant folding and literal hoisting own those), or an aggregate that
/// names a prealloc shell.
fn value_key(
    value: &Value,
    preallocs: &HashSet<ValueName>,
    resolved: &HashMap<ValueName, ValueName>,
) -> Option<Key> {
    match value {
        Value::Alias(_) => None,
        Value::Eval(code) => code_key(code, resolved),
        Value::Pure(data) => data_key(data, preallocs, resolved),
    }
}

fn code_key(code: &Code, resolved: &HashMap<ValueName, ValueName>) -> Option<Key> {
    if matches!(code, Code::ArrMap(..)) {
        return None;
    }

    let mut operands = operands(code, resolved);

    if is_commutative(code) {
        operands.sort();
    }

    let index = match code {
        Code::TplGet(_, index) => *index,
        _ => 0,
    };

    Some(Key::Eval(discriminant(code), operands, index))
}

fn data_key(
    data: &Data,
    preallocs: &HashSet<ValueName>,
    resolved: &HashMap<ValueName, ValueName>,
) -> Option<Key> {
    let (clsr, elems) = match data {
        // Scalars and bytestrings are already interned by constant folding and
        // literal hoisting; keying them here would only duplicate that work.
        Data::Nat(_) | Data::Int(_) | Data::Flt(_) | Data::Bin(_) => return None,
        Data::Arr(elems) | Data::Tpl(elems) => (None, elems),
        Data::Clsr(clsr, captures) => (Some(clsr.clone()), captures),
    };

    let elems: Vec<ValueName> = elems.iter().map(|name| canonical(name, resolved)).collect();

    if elems.iter().any(|name| preallocs.contains(name)) {
        return None;
    }

    Some(Key::Data(discriminant(data), clsr, elems))
}

/// A code op's operand names in order, each resolved to its ultimate source.
fn operands(code: &Code, resolved: &HashMap<ValueName, ValueName>) -> Vec<ValueName> {
    struct Collect<'a> {
        resolved: &'a HashMap<ValueName, ValueName>,
        operands: Vec<ValueName>,
    }

    impl Sink for Collect<'_> {
        fn value_use(&mut self, name: &ValueName) {
            self.operands.push(canonical(name, self.resolved));
        }
    }

    let mut collect = Collect {
        resolved,
        operands: Vec::new(),
    };
    walk_value_uses(&Value::Eval(code.clone()), &mut collect);

    collect.operands
}

fn canonical(name: &ValueName, resolved: &HashMap<ValueName, ValueName>) -> ValueName {
    resolved.get(name).unwrap_or(name).clone()
}

/// The integer ops whose operand order is irrelevant. Float ops are excluded
/// wholesale (identical-order duplicates still key equal), as is `BinEql` —
/// symmetric, but left to keep the whitelist purely scalar.
fn is_commutative(code: &Code) -> bool {
    matches!(
        code,
        Code::NatEql(..)
            | Code::NatNeq(..)
            | Code::NatAdd(..)
            | Code::NatMul(..)
            | Code::NatAnd(..)
            | Code::NatOr(..)
            | Code::NatXor(..)
            | Code::IntEql(..)
            | Code::IntNeq(..)
            | Code::IntAdd(..)
            | Code::IntMul(..)
            | Code::IntAnd(..)
            | Code::IntOr(..)
            | Code::IntXor(..)
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(name: &str) -> ValueName {
        ValueName::from(name)
    }

    fn func(region: Region) -> Module {
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![],
                resume: BlockName::from("b0"),
                region,
            },
        );
        module
    }

    fn jump(target: &str, args: Vec<ValueName>) -> Tail {
        Tail::Jump(JumpTarget {
            target: BlockName::from(target),
            params: args,
        })
    }

    fn region(values: Vec<(ValueName, Value)>, tail: Tail) -> Region {
        Region {
            preallocs: vec![],
            values,
            blocks: vec![],
            tail,
        }
    }

    fn main_region(module: &Module) -> &Region {
        &module.funcs()[0].1.region
    }

    fn assert_alias(region: &Region, index: usize, source: &str) {
        match &region.values[index].1 {
            Value::Alias(name) => assert_eq!(name, &v(source)),
            other => panic!("expected alias, got {other:?}"),
        }
    }

    #[test]
    fn dedups_identical_evals_in_straight_line() {
        // v2 = add(v0, v1); v3 = add(v1, v0)  ==>  v3 = v2 (commutative)
        let mut module = func(region(
            vec![
                (v("v0"), Value::Pure(Data::Nat(1))),
                (v("v1"), Value::Pure(Data::Nat(2))),
                (v("v2"), Value::Eval(Code::NatAdd(v("v0"), v("v1")))),
                (v("v3"), Value::Eval(Code::NatAdd(v("v1"), v("v0")))),
            ],
            jump("b0", vec![v("v2"), v("v3")]),
        ));

        eliminate_common_subexpressions(&mut module);

        assert_alias(main_region(&module), 3, "v2");
    }

    #[test]
    fn keeps_noncommutative_operand_order_distinct() {
        // sub(v0, v1) and sub(v1, v0) are different computations.
        let mut module = func(region(
            vec![
                (v("v0"), Value::Pure(Data::Nat(1))),
                (v("v1"), Value::Pure(Data::Nat(2))),
                (v("v2"), Value::Eval(Code::NatSub(v("v0"), v("v1")))),
                (v("v3"), Value::Eval(Code::NatSub(v("v1"), v("v0")))),
            ],
            jump("b0", vec![v("v2"), v("v3")]),
        ));

        eliminate_common_subexpressions(&mut module);

        assert!(matches!(
            &main_region(&module).values[3].1,
            Value::Eval(Code::NatSub(..))
        ));
    }

    #[test]
    fn dedups_through_alias_chains() {
        // v2 = v0; v3 = mul(v0, v1); v4 = mul(v2, v1)  ==>  v4 = v3
        let mut module = func(region(
            vec![
                (v("v0"), Value::Pure(Data::Nat(3))),
                (v("v1"), Value::Pure(Data::Nat(4))),
                (v("v2"), Value::Alias(v("v0"))),
                (v("v3"), Value::Eval(Code::NatMul(v("v0"), v("v1")))),
                (v("v4"), Value::Eval(Code::NatMul(v("v2"), v("v1")))),
            ],
            jump("b0", vec![v("v3"), v("v4")]),
        ));

        eliminate_common_subexpressions(&mut module);

        assert_alias(main_region(&module), 4, "v3");
    }

    #[test]
    fn dedups_block_computation_against_ancestor() {
        // The ancestor's add is in scope (and already evaluated) inside b1.
        let mut module = func(Region {
            preallocs: vec![],
            values: vec![
                (v("v0"), Value::Pure(Data::Nat(1))),
                (v("v1"), Value::Eval(Code::NatAdd(v("v0"), v("v0")))),
            ],
            blocks: vec![(
                BlockName::from("b1"),
                Block {
                    params: vec![],
                    region: region(
                        vec![(v("v2"), Value::Eval(Code::NatAdd(v("v0"), v("v0"))))],
                        jump("b0", vec![v("v2")]),
                    ),
                },
            )],
            tail: jump("b1", vec![]),
        });

        eliminate_common_subexpressions(&mut module);

        let block = &main_region(&module).blocks[0].1.region;
        assert_alias(block, 0, "v1");
    }

    #[test]
    fn does_not_dedup_across_sibling_arms() {
        // b1 and b2 are match arms: only one runs, so b2's add must not alias
        // into b1's.
        let arm = |value_name: &str| Block {
            params: vec![],
            region: region(
                vec![(v(value_name), Value::Eval(Code::NatAdd(v("v0"), v("v0"))))],
                jump("b0", vec![v(value_name)]),
            ),
        };

        let mut module = func(Region {
            preallocs: vec![],
            values: vec![(v("v0"), Value::Pure(Data::Nat(1)))],
            blocks: vec![
                (BlockName::from("b1"), arm("v1")),
                (BlockName::from("b2"), arm("v2")),
            ],
            tail: Tail::Match(MatchTarget {
                operand: v("v0"),
                cases: [
                    (
                        0,
                        JumpTarget {
                            target: BlockName::from("b1"),
                            params: vec![],
                        },
                    ),
                    (
                        1,
                        JumpTarget {
                            target: BlockName::from("b2"),
                            params: vec![],
                        },
                    ),
                ]
                .into(),
                default: None,
            }),
        });

        eliminate_common_subexpressions(&mut module);

        let arms = &main_region(&module).blocks;
        assert!(matches!(
            &arms[0].1.region.values[0].1,
            Value::Eval(Code::NatAdd(..))
        ));
        assert!(matches!(
            &arms[1].1.region.values[0].1,
            Value::Eval(Code::NatAdd(..))
        ));
    }

    #[test]
    fn dedups_identical_aggregate_constructions() {
        // Two identical tuples allocate twice; the second becomes an alias.
        let mut module = func(region(
            vec![
                (v("v0"), Value::Pure(Data::Nat(1))),
                (v("v1"), Value::Pure(Data::Tpl(vec![v("v0"), v("v0")]))),
                (v("v2"), Value::Pure(Data::Tpl(vec![v("v0"), v("v0")]))),
            ],
            jump("b0", vec![v("v1"), v("v2")]),
        ));

        eliminate_common_subexpressions(&mut module);

        assert_alias(main_region(&module), 2, "v1");
    }

    #[test]
    fn skips_prealloc_shells() {
        // A backpatched shell's identity matters mid-construction: neither the
        // shell binding nor an aggregate naming it participates.
        let clsr = ClsrName::from("c0");
        let mut module = func(Region {
            preallocs: vec![(v("v0"), clsr.clone())],
            values: vec![
                (v("v0"), Value::Pure(Data::Clsr(clsr.clone(), vec![]))),
                (v("v1"), Value::Pure(Data::Clsr(clsr.clone(), vec![]))),
                (v("v2"), Value::Pure(Data::Tpl(vec![v("v0")]))),
                (v("v3"), Value::Pure(Data::Tpl(vec![v("v0")]))),
            ],
            blocks: vec![],
            tail: jump("b0", vec![v("v1"), v("v2"), v("v3")]),
        });

        eliminate_common_subexpressions(&mut module);

        let region = main_region(&module);
        // v0 is a shell: not a source, not rewritten.
        assert!(matches!(&region.values[0].1, Value::Pure(Data::Clsr(..))));
        assert!(matches!(&region.values[1].1, Value::Pure(Data::Clsr(..))));
        // Aggregates naming the shell stay distinct too.
        assert!(matches!(&region.values[3].1, Value::Pure(Data::Tpl(..))));
    }

    #[test]
    fn never_keys_arr_map() {
        // ArrMap invokes a closure whose region may end in an effectful tail.
        let mut module = func(region(
            vec![
                (v("v0"), Value::Pure(Data::Arr(vec![]))),
                (
                    v("v1"),
                    Value::Pure(Data::Clsr(ClsrName::from("c0"), vec![])),
                ),
                (v("v2"), Value::Eval(Code::ArrMap(v("v0"), v("v1")))),
                (v("v3"), Value::Eval(Code::ArrMap(v("v0"), v("v1")))),
            ],
            jump("b0", vec![v("v2"), v("v3")]),
        ));

        eliminate_common_subexpressions(&mut module);

        assert!(matches!(
            &main_region(&module).values[3].1,
            Value::Eval(Code::ArrMap(..))
        ));
    }
}
