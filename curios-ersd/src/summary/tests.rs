//! What a recursive function's summary says about divergence, and what decides it.

use {
    crate::*,
    curios_abi::{DeclaredForeign, ForeignFunction, WireResults, WireSignature, WireType},
    std::sync::Arc,
};

/// `fn loop(n) = loop(n)` — a self-call, so its component is recursive whatever it does. With `effectful`, a host call sits in the body beside it.
fn a_self_recursive_function(total: bool) -> Module {
    a_self_recursive_function_that(total, false)
}

fn a_self_recursive_function_that(total: bool, effectful: bool) -> Module {
    let mut builder = ErsdBuilder::new();
    let id = builder.reserve_function();
    let param = builder.value(Some("n".into()));
    builder.open_block();
    if effectful {
        let row = Arc::new(ForeignFunction::Declared(DeclaredForeign {
            name: "/beep".into(),
            label: "beep".into(),
            signature: WireSignature {
                params: vec![],
                results: WireResults::single("r".into(), WireType::Nat),
            },
        }));
        let foreign = builder.foreign(row);
        builder.let_value(
            None,
            Rhs::Foreign {
                foreign,
                operands: vec![],
            },
        );
    }
    let call = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(id),
            arguments: vec![Atom::Value(param)],
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(call)));
    builder.define_function(id, Some("loop".into()), vec![param], body);
    if total {
        builder.mark_total(id);
    }

    builder.open_block();
    builder.let_functions(vec![id]);
    let entry = builder.seal_block(Terminator::Return(Atom::Function(id)));
    builder.set_entry(entry);
    builder.finalize().expect("the fixture verifies")
}

fn may_diverge(module: &Module) -> bool {
    let summary = Summary::analyze(module, &Analysis::analyze(module));
    let id = module
        .function_ids()
        .next()
        .expect("the fixture defines one function");
    summary
        .rhs_behavior(
            module,
            &Rhs::Apply {
                callee: Atom::Function(id),
                arguments: vec![],
            },
        )
        .observable
        .may_diverge
}

/// Recursion is what makes this stage unable to tell termination, and the carried verdict is what spares it from having to: the same function, seeded divergent without the verdict and pure with it.
///
/// The pair is the point. Reading the verdict is only sound because nothing here derives it — it is the size-change engine's, decided above Core, and already load-bearing for what erasure may delete.
#[test]
fn a_recursive_function_diverges_unless_its_definition_was_proved_total() {
    assert!(
        may_diverge(&a_self_recursive_function(false)),
        "a recursive function with no verdict must be assumed divergent",
    );
    assert!(
        !may_diverge(&a_self_recursive_function(true)),
        "a recursive function whose definition was proved total is not divergent",
    );
}

/// The conclusion the erased stage hands Cont is the *conjunction*, and it is the only form of either fact that crosses.
///
/// Cont has no lattice and no notion of purity: termination arrived from above Core, effects are this stage's summary, and both are in hand exactly once — at the lowering. A function proved total but performing an effect must not cross as droppable, which is the half a test of the verdict alone would miss.
#[test]
fn only_a_total_and_effect_free_function_crosses_as_droppable() {
    for (label, total, effectful, expected) in [
        ("total and pure", true, false, true),
        ("pure but unproved", false, false, false),
        ("total but performing", true, true, false),
    ] {
        let source = a_self_recursive_function_that(total, effectful);
        let lowered = crate::lower_to_cont(&source);
        let droppable = lowered
            .functions()
            .iter()
            .flatten()
            .any(|function| function.droppable);
        assert_eq!(droppable, expected, "{label}");
    }
}
