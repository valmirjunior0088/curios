//! Fixtures the kernel's inference suites share: a kernel above every binder they use, and the declarations they infer against.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use {
    crate::Kernel,
    curios_core::{
        Free, Global, Intrinsic, Level, Nat, Polarity, StructDecl, Telescope, Term, UniverseContext,
    },
    curios_utilities::Qualifier,
};

pub(super) fn kernel() -> Kernel {
    let mut kernel = Kernel::new(100_000, crate::SYNTAX);
    kernel.set_local_floor(1_000);
    kernel
}

pub(super) fn binder(index: u32, hint: &str) -> Free {
    Free::local(index, Some(hint))
}

pub(super) fn nat(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}

pub(super) fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

pub(super) fn bool_type() -> Term {
    Term::intrinsic(Intrinsic::BoolType)
}

pub(super) fn one() -> Level {
    Level::zero().succ().expect("level zero has a successor")
}

/// A structure with one parameter and one field at that parameter's type — the shape an occurrence can state the wrong parameter count for.
pub(super) fn parameterized_struct(kernel: &mut Kernel) -> Global {
    let name = Global::Authored(Qualifier::from(["Boxed"]));
    let param = binder(0, "A");
    let field = binder(1, "value");

    let fields = Telescope::build([(field, Term::free_var(&param))], ());
    let declaration = StructDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::build([(param, Term::type_ground())], fields),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: vec![Polarity::Strict],
    };
    kernel.declare_struct(&name, &declaration);

    name
}

/// A dependent Σ, for the two fixtures below: `(t : Type, x : t)`, whose second entry's type *is* its first component.
///
/// The dependency is the whole point. Inferring the components of a literal independently yields `(Type, Nat)`, a telescope binding nothing, and no conversion relates that to this one — so a rule that reaches this expectation only through inference cannot accept any inhabitant of it.
pub(super) fn dependent_pair_type() -> Term {
    let t = binder(40, "t");
    let x = binder(41, "x");

    Term::tuple_type([(t.clone(), Term::type_ground()), (x, Term::free_var(&t))])
}
