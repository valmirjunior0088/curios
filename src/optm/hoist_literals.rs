use {
    super::*,
    std::collections::{HashMap, HashSet},
};

/// Literal hoisting: lift bytestring literals out of bodies into module consts.
///
/// An inline `Value::Pure(Data::Bin(bytes))` lowers to `array.new_data` — a fresh
/// GC array allocation that copies the whole segment **every time the binding
/// executes** (`cont/to_wasm/expr_emitter.rs`). The same literal emitted as a
/// module const instead runs that allocation **once** in the `start` function and
/// stashes it in a global, so every use is a cheap shared `global.get`
/// (`cont/to_wasm/module_emitter.rs`). For a bytestring in a loop, a closure body,
/// or any hot path, that turns a per-execution O(n) copy into a one-time init.
///
/// This is sound because `Bin` is immutable (the IR has no field-store, the same
/// invariant [`constant_folding`] relies on) and nothing compares it by reference
/// identity (`BinEql` is bytewise). So sharing one instance across every use — and
/// across bodies, via dedup — is observationally identical to rebuilding it.
///
/// Each distinct byte sequence becomes one const named `lit$bin$<n>` (the `lit$`
/// prefix cannot collide with the lowerer's `v…`/`b…` body names), and each in-body
/// binding is rewritten to a `Value::Alias` of it. Copy propagation then collapses
/// the alias into its uses and dead-code elimination reclaims any const left
/// unreferenced — so this leans on the same alias-then-propagate idiom as inlining
/// and jump threading rather than rewriting use sites itself.
///
/// Only `Bin` is hoisted here. Closed `Arr`/`Tpl`/`Clsr` aggregates are also
/// hoistable in principle (they would introduce const-to-const references, which
/// the module sweep already follows), but are left for a later cut.
pub fn hoist_literals(module: &mut Module) {
    // Distinct bytestrings in first-seen order, so the minted const names — and the
    // emitted const table — are deterministic.
    let mut distinct: Vec<Vec<u8>> = Vec::new();
    let mut seen: HashSet<Vec<u8>> = HashSet::new();

    for (_, func) in module.funcs() {
        collect_bins(&func.region, &mut distinct, &mut seen);
    }
    for (_, clsr) in module.clsrs() {
        collect_bins(&clsr.region, &mut distinct, &mut seen);
    }

    if distinct.is_empty() {
        return;
    }

    // Mint one const per distinct bytestring, then rewrite every binding of it to an
    // alias of that const.
    let names: HashMap<Vec<u8>, ValueName> = distinct
        .iter()
        .enumerate()
        .map(|(index, bytes)| (bytes.clone(), const_name(index)))
        .collect();

    for (index, bytes) in distinct.into_iter().enumerate() {
        module.add_const(const_name(index), Data::Bin(bytes));
    }

    for (_, func) in module.funcs_mut() {
        rewrite_bins(&mut func.region, &names);
    }
    for (_, clsr) in module.clsrs_mut() {
        rewrite_bins(&mut clsr.region, &names);
    }
}

fn const_name(index: usize) -> ValueName {
    ValueName::from(format!("lit$bin${index}"))
}

/// Record every distinct `Bin` literal bound in the tree, in first-seen order.
fn collect_bins(region: &Region, distinct: &mut Vec<Vec<u8>>, seen: &mut HashSet<Vec<u8>>) {
    for (_, value) in &region.values {
        if let Value::Pure(Data::Bin(bytes)) = value
            && seen.insert(bytes.clone())
        {
            distinct.push(bytes.clone());
        }
    }

    for (_, block) in &region.blocks {
        collect_bins(&block.region, distinct, seen);
    }
}

/// Replace every `Bin` literal binding in the tree with an alias of its hoisted const.
fn rewrite_bins(region: &mut Region, names: &HashMap<Vec<u8>, ValueName>) {
    for (_, value) in &mut region.values {
        if let Value::Pure(Data::Bin(bytes)) = value {
            let name = names
                .get(bytes)
                .expect("every bytestring was interned")
                .clone();
            *value = Value::Alias(name);
        }
    }

    for (_, block) in &mut region.blocks {
        rewrite_bins(&mut block.region, names);
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

    fn alias(value: &Value) -> &ValueName {
        match value {
            Value::Alias(source) => source,
            other => panic!("expected an alias, got {other:?}"),
        }
    }

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

        // Two consts, carrying the distinct byte sequences in first-seen order.
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

        // The bindings are now aliases; the two identical ones share one const.
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
    fn leaves_bodies_without_bytestrings_untouched() {
        let mut module = main_func(region(vec![(v("v0"), Value::Pure(Data::Nat(1)))], vec![]));

        hoist_literals(&mut module);

        assert!(module.consts().is_empty());
        assert!(matches!(
            &main_region(&module).values[0].1,
            Value::Pure(Data::Nat(1))
        ));
    }
}
