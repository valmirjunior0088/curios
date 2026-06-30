use {
    super::*,
    std::collections::{HashMap, HashSet},
};

/// Copy propagation: eliminate `let x = y` renames.
///
/// Every `Value::Alias(source)` binding is a copy. This pass rewrites each use of
/// the aliased name to its ultimate source (following chains `a = b`, `b = c` to
/// `c`) and then drops the now-dead alias bindings, leaving downstream passes to
/// see a value's real identity instead of a rename.
///
/// Names are only unique within a single function or closure body (the lowerer
/// restarts its counter per body), so resolution runs per region tree, never
/// across the whole module.
pub fn propagate_copies(module: &mut Module) {
    for (_, func) in module.funcs_mut() {
        propagate_in_tree(&mut func.region);
    }
    for (_, clsr) in module.clsrs_mut() {
        propagate_in_tree(&mut clsr.region);
    }
}

fn propagate_in_tree(region: &mut Region) {
    let aliases = resolve(collect_aliases(region));

    if aliases.is_empty() {
        return;
    }

    walk_region_mut(region, &mut Substitute { aliases: &aliases });
    drop_aliases(region);
}

/// Gather the raw `name -> source` map of every alias binding in the tree.
fn collect_aliases(region: &Region) -> HashMap<ValueName, ValueName> {
    let mut aliases = HashMap::new();
    collect_aliases_into(region, &mut aliases);
    aliases
}

fn collect_aliases_into(region: &Region, aliases: &mut HashMap<ValueName, ValueName>) {
    for (name, value) in &region.values {
        if let Value::Alias(source) = value {
            aliases.insert(name.clone(), source.clone());
        }
    }

    for (_, block) in &region.blocks {
        collect_aliases_into(&block.region, aliases);
    }
}

/// Collapse alias chains so each name maps directly to its ultimate non-alias
/// source. A self-referential chain (which should never arise in practice) is
/// broken at the repeated name rather than looping forever.
fn resolve(raw: HashMap<ValueName, ValueName>) -> HashMap<ValueName, ValueName> {
    let mut resolved = HashMap::with_capacity(raw.len());

    for key in raw.keys() {
        let mut current = key.clone();
        let mut seen = HashSet::new();
        seen.insert(current.clone());

        while let Some(next) = raw.get(&current) {
            if !seen.insert(next.clone()) {
                break;
            }
            current = next.clone();
        }

        resolved.insert(key.clone(), current);
    }

    resolved
}

struct Substitute<'a> {
    aliases: &'a HashMap<ValueName, ValueName>,
}

impl SinkMut for Substitute<'_> {
    fn value_use(&mut self, name: &mut ValueName) {
        if let Some(source) = self.aliases.get(name) {
            *name = source.clone();
        }
    }
}

/// Remove every alias binding in the tree. After substitution none of them is
/// referenced, so this cannot change behavior.
fn drop_aliases(region: &mut Region) {
    region
        .values
        .retain(|(_, value)| !matches!(value, Value::Alias(_)));

    for (_, block) in &mut region.blocks {
        drop_aliases(&mut block.region);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(name: &str) -> ValueName {
        ValueName::from(name)
    }

    fn main_func(values: Vec<(ValueName, Value)>, tail: Tail) -> Module {
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![],
                resume: BlockName::from("b0"),
                region: Region {
                    preallocs: vec![],
                    values,
                    blocks: vec![],
                    tail,
                },
            },
        );
        module
    }

    fn jump(args: Vec<ValueName>) -> Tail {
        Tail::Jump(JumpTarget {
            target: BlockName::from("b0"),
            params: args,
        })
    }

    fn main_region(module: &Module) -> &Region {
        &module.funcs()[0].1.region
    }

    #[test]
    fn resolves_chain_and_rewrites_tail_use() {
        // v0 = 1; v1 = v0; v2 = v1; return v2  ==>  v0 = 1; return v0
        let mut module = main_func(
            vec![
                (v("v0"), Value::Pure(Data::Nat(1))),
                (v("v1"), Value::Alias(v("v0"))),
                (v("v2"), Value::Alias(v("v1"))),
            ],
            jump(vec![v("v2")]),
        );

        propagate_copies(&mut module);

        let region = main_region(&module);
        assert_eq!(region.values.len(), 1);
        assert_eq!(region.values[0].0, v("v0"));
        match &region.tail {
            Tail::Jump(target) => assert_eq!(target.params, vec![v("v0")]),
            _ => panic!("expected jump tail"),
        }
    }

    #[test]
    fn rewrites_uses_inside_code_operands() {
        // v0 = 2; v1 = v0; v2 = add(v1, v1)  ==>  v0 = 2; v2 = add(v0, v0)
        let mut module = main_func(
            vec![
                (v("v0"), Value::Pure(Data::Nat(2))),
                (v("v1"), Value::Alias(v("v0"))),
                (v("v2"), Value::Eval(Code::NatAdd(v("v1"), v("v1")))),
            ],
            jump(vec![v("v2")]),
        );

        propagate_copies(&mut module);

        let region = main_region(&module);
        let names: Vec<_> = region.values.iter().map(|(n, _)| n.clone()).collect();
        assert_eq!(names, vec![v("v0"), v("v2")]);
        match &region.values[1].1 {
            Value::Eval(Code::NatAdd(a, b)) => {
                assert_eq!((a, b), (&v("v0"), &v("v0")));
            }
            _ => panic!("expected NatAdd"),
        }
    }

    #[test]
    fn leaves_alias_free_regions_untouched() {
        let mut module = main_func(
            vec![(v("v0"), Value::Pure(Data::Nat(1)))],
            jump(vec![v("v0")]),
        );

        propagate_copies(&mut module);

        assert_eq!(main_region(&module).values.len(), 1);
    }
}
