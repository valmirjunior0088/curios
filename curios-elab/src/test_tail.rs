//! The synthesized tail a unit compiled as its own test program ends in.

use {
    crate::Context,
    curios_core::{Global, Intrinsic, Term, syn_call},
};

/// The synthesized tail of a unit compiled as its own test program: `/test/main([thunk, …])` over `tests` in the order they come, which is the order the runner runs them in and indexes them by, each the declaration itself, which is already the `() -> Test({})` thunk its lowering built. The list's element type is one fresh hole minted here, solved bidirectionally from `/test/main`'s parameter exactly as a written literal's would be.
///
/// **There is nothing to decide here, and that is the point.** A test takes no parameters, so it makes no claim about instantiations it has not been given and there is no discharge to choose: the tail lists each declaration and stops. A test's path is the runner's, which reads it off the schedule the compile hands back.
///
/// Built by the elaborator once the unit's items have been defined, in the scope the tail is closed over, `/test` among it ([`Tail::Tests`](crate::Tail)), which is where the elaborated definitions it schedules are in reach.
pub fn test_program_tail(context: &mut Context, tests: &[Global]) -> Term {
    let syntax = context.syntax();
    let element_hole = context.mint_metavar();
    let items = tests
        .iter()
        .map(|test| Term::written(*test))
        .collect::<Vec<_>>();

    syn_call(
        syntax.test.main,
        [Term::intrinsic(Intrinsic::List {
            element: Term::hole(element_hole),
            items,
        })],
    )
}
