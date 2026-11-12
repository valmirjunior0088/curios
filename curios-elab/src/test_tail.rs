//! The synthesized tail a unit compiled as its own test program ends in.

use {
    crate::Context,
    curios_core::{Global, Intrinsic, Term, str_literal, syn_call},
};

/// The synthesized tail of a unit compiled as its own test program: `Test/main([("path", thunk), …])` over `tests` in the order they come, which is the order the runner runs them in, each pair the test's path as its `Global` renders it and the declaration itself, which is already the `() -> Test({})` thunk its lowering built. The list's element type is one fresh hole minted here, solved bidirectionally from `Test/main`'s parameter exactly as a written literal's would be.
///
/// **There is nothing to decide here, and that is the point.** A test takes no parameters, so it makes no claim about instantiations it has not been given and there is no discharge to choose: the tail pairs each declaration with its path and stops.
///
/// Built by the elaborator after the unit's items have been defined and before the entry is checked ([`Tail::Tests`](crate::Tail)), which is where the elaborated definitions it schedules are in reach.
pub fn test_program_tail(context: &mut Context, tests: &[Global]) -> Term {
    let syntax = context.syntax();
    let element_hole = context.mint_metavar();
    let items = tests
        .iter()
        .map(|test| {
            let path = test.path();
            let thunk = Term::written(*test);

            Term::tuple([str_literal(&syntax.string, path.as_bytes()), thunk])
        })
        .collect::<Vec<_>>();

    syn_call(
        syntax.test.main,
        [Term::intrinsic(Intrinsic::List {
            element: Term::hole(element_hole),
            items,
        })],
    )
}
