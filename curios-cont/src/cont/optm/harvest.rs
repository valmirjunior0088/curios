use {super::*, std::collections::HashSet};

/// Every `ValueName` used (in operand position) anywhere in a region tree.
pub fn value_uses(region: &Region) -> HashSet<ValueName> {
    let mut uses = Uses(HashSet::new());
    walk_region(region, &mut uses);

    uses.0
}

struct Uses(HashSet<ValueName>);

impl Sink for Uses {
    fn value_use(&mut self, name: &ValueName) {
        self.0.insert(name.clone());
    }
}

/// The functions, closures, and value names referenced by a region tree —
/// everything needed to decide what a region keeps alive.
#[derive(Default)]
pub struct Refs {
    pub funcs: HashSet<FuncName>,
    pub clsrs: HashSet<ClsrName>,
    pub values: HashSet<ValueName>,
}

/// Harvest all three reference kinds in a single walk.
pub fn region_refs(region: &Region) -> Refs {
    let mut refs = Refs::default();
    walk_region(region, &mut refs);

    refs
}

/// The value and closure references held by a single `Data` — used to follow the
/// edges out of a module-level const (a const aggregate can name other consts; a
/// const `Data::Clsr` names a closure), which a region walk never reaches.
pub fn data_refs(data: &Data) -> Refs {
    let mut refs = Refs::default();
    walk_data_refs(data, &mut refs);

    refs
}

impl Sink for Refs {
    fn value_use(&mut self, name: &ValueName) {
        self.values.insert(name.clone());
    }

    fn clsr_ref(&mut self, name: &ClsrName) {
        self.clsrs.insert(name.clone());
    }

    fn func_ref(&mut self, name: &FuncName) {
        self.funcs.insert(name.clone());
    }
}

/// Every value name *bound* in a region tree: prealloc, value, and
/// block-parameter binders. The walker deliberately skips binder positions, so
/// this recursion is spelled out by hand — once, here, for every pass that
/// clones a subtree and must separate its bound names from its free ones
/// (a free name is an enclosing-scope or module-const reference and must not
/// be renamed).
pub fn bound_values(region: &Region) -> HashSet<ValueName> {
    let mut bound = HashSet::new();
    collect_bound_values(region, &mut bound);

    bound
}

fn collect_bound_values(region: &Region, bound: &mut HashSet<ValueName>) {
    for (name, _) in &region.preallocs {
        bound.insert(name.clone());
    }

    for (name, _) in &region.values {
        bound.insert(name.clone());
    }

    for (_, block) in &region.blocks {
        for param in &block.params {
            bound.insert(param.clone());
        }

        collect_bound_values(&block.region, bound);
    }
}

/// Every block name *defined* in a region tree — the block-name mirror of
/// [`bound_values`]. A block reference not in this set points outside the
/// tree (an enclosing-scope block or a resume sentinel).
pub fn bound_blocks(region: &Region) -> HashSet<BlockName> {
    let mut bound = HashSet::new();
    collect_bound_blocks(region, &mut bound);

    bound
}

fn collect_bound_blocks(region: &Region, bound: &mut HashSet<BlockName>) {
    for (name, block) in &region.blocks {
        bound.insert(name.clone());
        collect_bound_blocks(&block.region, bound);
    }
}
