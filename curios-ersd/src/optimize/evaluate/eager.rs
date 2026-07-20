//! Proving eager computed groups pure so pruning can drop the dead ones.
//!
//! When an eager recursive group only *builds closures* — its members
//! partially apply a higher-order combinator like `Fmt/go_with` — its eager
//! evaluation performs no observable effect, yet the first-order effect
//! [`Summary`](crate::Summary) cannot see that: applying a computed closure
//! value is conservatively unknown, hence observable, so the whole dead group
//! (and every function it keeps alive) survives pruning. This pass *runs*
//! each group with the interpreter: a group whose members all force to values
//! with no effect is proven pure, and the prune skips its observability
//! seeding — reachability alone decides whether it survives.

use {
    super::interpret::Evaluator,
    crate::{Analysis, Module, Statement, StatementId},
    std::collections::BTreeSet,
};

/// The item statements whose eager evaluation the interpreter proves performs
/// no effect.
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
pub(crate) fn prove_eager_groups_pure(module: &Module) -> BTreeSet<StatementId> {
    let analysis = Analysis::analyze(module);
    let mut evaluator = Evaluator::new(module, &analysis);
    let mut pure = BTreeSet::new();
    for &item in module.items() {
        let Some(Statement::Rec { group }) = module.statement(item) else {
            continue;
        };
        let Some(group) = module.rec_group(*group) else {
            continue;
        };
        // A function-only group is already inert; only a computed group can
        // be a higher-order observable.
        if group.values.is_empty() {
            continue;
        }
        let values = group.values.clone();
        if evaluator.evaluate_computed_group(&values).is_some() {
            pure.insert(item);
        }
    }
    pure
}
