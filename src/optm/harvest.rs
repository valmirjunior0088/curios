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
