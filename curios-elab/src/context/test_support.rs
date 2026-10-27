//! What a suite reads of a context and no judgment does, what the declaration being elaborated has spent; and, for this crate's tests alone, the fixtures the context's suites share.
//!
//! Compiled for this crate's tests and behind the `test-support` feature, so no build that ships holds it.

use {super::Context, curios_core::Consumption};

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
