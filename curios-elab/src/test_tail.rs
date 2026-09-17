//! The synthesized tail a unit compiled as its own test program ends in, and the record of each test it schedules.

use {
    crate::Context,
    curios_core::{Free, Global, Intrinsic, Term, Var, str_literal, syn_call},
    curios_utilities::Span,
};

/// One registered test as the synthesized tail schedules it: its name, and the span of its authored body. Read off the lowered definition by `curios-pipeline`, since `Module::tests` records names alone.
#[derive(Debug, Clone)]
pub struct ScheduledTest {
    pub name: Global,
    pub span: Option<Span>,
}

/// The synthesized tail of a unit compiled as its own test program: `Test/main([("path", thunk), …])` over `tests` in declaration order, each pair the test's path as its `Global` renders it and the declaration itself, which is already the `() -> Test` thunk its lowering built. The list's element type is one fresh hole minted here, solved bidirectionally from `Test/main`'s parameter exactly as a written literal's would be.
///
/// **There is nothing to decide here, and that is the point.** A test takes no parameters, so it makes no claim about instantiations it has not been given and there is no discharge to choose: the tail pairs each declaration with its path and stops. What used to live here — an oracle asking whether a body was a theorem under its telescope, and two closers to pick between — went with the parameters.
///
/// Built by the elaborator after the unit's items have been defined and before the entry is checked ([`Tail::Tests`](crate::Tail)), which is where the elaborated definitions it schedules are in reach.
pub(crate) fn test_program_tail(context: &mut Context, tests: &[ScheduledTest]) -> Term {
    let syntax = context.syntax();
    let element_hole = context.mint_metavar();
    let items = tests
        .iter()
        .map(|test| {
            let path = test.name.path();
            let thunk = Term::var(Var::free(Free::from(&test.name)));

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
