//! What the sort suites share: a kernel, a name, a binder, and a declaration at a stated sort.

use {
    crate::Kernel,
    curios_analysis::test_support::SYNTAX,
    curios_core::{Free, Global, InductDecl, Telescope, Term, UniverseContext},
    curios_utilities::Qualifier,
};

pub fn kernel() -> Kernel<'static> {
    Kernel::new(100_000, SYNTAX)
}

pub fn nominal(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

/// A nominal family with no constructors, declared at `result_sort`. Enough to give the tests below a base type at a known sort — the registry is what says whether a nominal type is a proposition, so there is no way to build one without it.
pub fn declare(kernel: &mut Kernel, path: &str, result_sort: Term) -> Term {
    let name = nominal(path);

    kernel
        .declare_induct(
            &name,
            &InductDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::done(Telescope::done(())),
                constructors: Vec::new().into(),
                result_sort,
                module: Qualifier::from([path]),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )
        .expect("a name is declared once");

    Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new())
}

pub fn binder(index: u32, hint: &str) -> Free {
    Free::local(u64::from(index), Some(hint))
}
