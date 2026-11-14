//! Fixtures the certifier's inference suites share: the certifier they start from, and the declarations they infer against.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use {crate::*, curios_analysis::test_support::*};

pub fn certifier() -> Certifier<'static> {
    Certifier::new(100_000, SYNTAX)
}

/// `Boxed(A: Type) { value: A }`, a structure with one parameter and one field at that parameter's type — the shape an occurrence can state the wrong parameter count for.
pub const BOXED: &str =
    "#{polarities = [Strict]}\nstruct /Boxed(A: Type): pub Type {\n    value: A,\n}";

/// `{t: Type, x: t}`, a dependent Σ whose second entry's type *is* its first component.
///
/// The dependency is the whole point. Inferring the components of a literal independently yields `(Type, Nat)`, a telescope binding nothing, and no conversion relates that to this one — so a rule that reaches this expectation only through inference cannot accept any inhabitant of it.
pub const DEPENDENT_PAIR: &str = "{t: Type, x: t}";
