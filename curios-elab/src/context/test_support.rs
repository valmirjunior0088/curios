//! What a suite asks of a context and no judgment does: what the declaration being elaborated has spent, and a context whose caches clear every table at every boundary; and, for this crate's tests alone, the fixtures the context's suites share.
//!
//! Compiled for this crate's tests and behind the `test-support` feature, so no build that ships holds it.

use {
    super::{Caches, Context},
    curios_core::Consumption,
    curios_utilities::SyntaxRegistry,
};

#[cfg(test)]
use {
    crate::*,
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Declarations, Definition, DefinitionKind, Global, HeadKey, Intrinsic, Item, Module, Term,
        Totality, UniverseContext, WitnessKey,
    },
    curios_utilities::Qualifier,
    std::collections::BTreeSet,
};

impl Context<'_> {
    /// A context whose caches clear every table at every boundary, as they did before an answer was kept for as long as what it read stands. Exists for one purpose: asserting that keeping an answer longer moves no elaborated term (`curios-pipeline`'s `std_memo_census`), as `curios-cert`'s `Kernel::uncached` asserts of the kernel's memos that switching them off moves no verdict.
    pub fn wholesale(budget: u64, syntax: SyntaxRegistry) -> Self {
        let mut context = Self::new(budget, syntax);
        context.typing.caches = Caches::wholesale();

        context
    }

    /// What the declaration being elaborated has consumed so far (`Spend::consumed`), which the measurements read between two judgments.
    pub fn consumed(&self) -> Consumption {
        self.typing.spend.consumed()
    }
}

#[cfg(test)]
pub fn context() -> Context<'static> {
    Context::with_default_budget(SYNTAX)
}

#[cfg(test)]
pub fn named(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

/// The declarations of a unit that writes `names` in order, each a definition of its own.
#[cfg(test)]
pub fn written(names: &[Global]) -> Declarations {
    let items = names
        .iter()
        .map(|name| {
            Item::Let(Box::new(Definition {
                name: *name,
                kind: DefinitionKind::Authored,
                universe_context: UniverseContext::empty(),
                island: Qualifier::empty(),
                totality: Totality::default(),
                proof: false,
                variances: Vec::new(),
                field_variances: Vec::new(),
                parameter_field: None,
                value_sort: None,
                type_: Term::type_ground(),
                body: Term::type_ground(),
            }))
        })
        .collect::<Vec<_>>();

    Declarations::of(&Module {
        items: items.into(),
        ..Module::default()
    })
}

/// A unit that writes `a` and then `b`, neither elaborated.
#[cfg(test)]
pub fn unit_of_two(context: &mut Context) -> [Global; 2] {
    let [a, b] = [named("a"), named("b")];
    context.begin_unit(written(&[a, b]), BTreeSet::from([a, b]));

    [a, b]
}

/// A witness the unit declares and has not elaborated, spelled at `Show(Nat)` at the place `position`.
#[cfg(test)]
pub fn declare(context: &mut Context, path: &str, position: usize) -> Global {
    let name = named(path);
    context.declare_witness(
        name,
        Declared {
            spelled: Some((named("Show"), WitnessKey::from([HeadKey::Nat]))),
            module: Qualifier::empty(),
            position,
        },
    );

    name
}

#[cfg(test)]
pub fn nat() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}
