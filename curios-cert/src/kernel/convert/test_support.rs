//! Fixtures the kernel's conversion suites share: a kernel above every binder they use, and the declarations they compare at.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across `convert`, and nothing outside it.

use {
    crate::Kernel,
    curios_core::{Free, Global, InductDecl, Intrinsic, Nat, Telescope, Term, UniverseContext},
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

/// A nominal family at a stated sort — the only way to obtain a base proposition, since the registry is what says a nominal type is one.
pub(super) fn declare(kernel: &mut Kernel, path: &str, result_sort: Term) -> Term {
    let name = Global::Authored(Qualifier::from([path]));

    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::done(())),
            constructors: Vec::new(),
            result_sort,
            module: Qualifier::from([path]),
            rep_public: true,
            polarities: Vec::new(),
        },
    );

    Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new())
}

/// `rec m : Type = (m) -> codomain; m`, optionally carrying a second unused member so that two such groups are not structurally equal and must take a delta step to be compared.
pub(super) fn equirecursive(member: Free, param: Free, codomain: Term, padded: bool) -> Term {
    let body = Term::func_type([(param, Term::free_var(&member))], codomain);
    let mut items = vec![(member.clone(), Term::type_ground(), body)];

    if padded {
        items.push((binder(99, "unused"), Term::type_ground(), nat_type()));
    }

    Term::rec(items, Term::free_var(&member))
}

/// `induct Wit(P : <param_sort>) : (p : P)` — a family whose index type *is* its own parameter.
///
/// `induct_type_args` compares two index actuals at the type the declaration's telescope assigns, opened at the left instance's preceding actuals, so here the index pair is compared at whatever term stands in the parameter position. That makes the parameter's assumed type the whole of what decides the index goal, which is exactly the question a binder opened at a stand-in answers with the stand-in.
pub(super) fn declare_indexed(kernel: &mut Kernel, path: &str, param_sort: Term) -> Global {
    let name = Global::Authored(Qualifier::from([path]));
    let param = binder(90, "P");

    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::build(
                [(param.clone(), param_sort)],
                Telescope::build([(binder(91, "p"), Term::free_var(&param))], ()),
            ),
            constructors: Vec::new(),
            result_sort: Term::type_ground(),
            module: Qualifier::from([path]),
            rep_public: true,
            polarities: Vec::new(),
        },
    );

    name
}
