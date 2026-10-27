//! Copy plans a suite asks for with no limit on their extent, which the optimizer never plans.

use {
    super::CopyPlan,
    crate::{ContinuationId, FunctionId, Module},
    std::collections::BTreeSet,
};

impl CopyPlan {
    pub(crate) fn functions(module: &Module, functions: BTreeSet<FunctionId>) -> Self {
        Self::new(module, functions, BTreeSet::new())
    }

    pub(crate) fn continuation(module: &Module, continuation: ContinuationId) -> Self {
        Self::new(module, BTreeSet::new(), BTreeSet::from([continuation]))
    }

    fn new(
        module: &Module,
        functions: BTreeSet<FunctionId>,
        continuations: BTreeSet<ContinuationId>,
    ) -> Self {
        Self::limited(module, functions, continuations, usize::MAX)
            .expect("an unlimited copy extent fits")
    }
}
