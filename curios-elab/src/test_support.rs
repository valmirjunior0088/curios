//! Scaffolding for tests that print a finished term as a report does, and the discharged bound a fold's suite stands in.
//!
//! Behind the `test-support` feature, so no build that ships holds it, and compiled for this crate's own tests, which share it.

use {
    crate::{Context, Established},
    curios_core::{Free, Term},
    curios_utilities::SyntaxRegistry,
};

/// `term`, read over the units `established` holds, as a report spells it for a reader standing at the top of the declaration it belongs to: each witness resolution would put back there left as an open slot, each method projected off one called as a program calls it (`written`). What a report settles where it is made, a test that prints a finished declaration settles here.
pub fn written_for_reader(
    established: &Established<'_>,
    syntax: SyntaxRegistry,
    term: &Term,
) -> Term {
    let mut context = Context::with_default_budget(syntax);
    context.read_through(established);

    crate::written(&mut context, term)
}

/// A stand-in for a discharged bound. Reduction never inspects one — proof irrelevance makes its value unobservable, and the suites that take it are about the fold laws rather than the obligation.
pub fn qed() -> Term {
    Term::free_var(&Free::local(9_999, Some("qed")))
}
