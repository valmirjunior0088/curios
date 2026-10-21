//! The pieces the representation suites build their modules from by hand.

use {
    crate::{
        Atom, Continuation, ContinuationId, Edge, Function, FunctionId, Literal, Module, Node,
        NodeId, ValueId,
    },
    curios_num::Natural,
};

/// Make `body` the entry function's body, with `params` as its parameters. The return continuation is reserved and never defined, which is exactly what a function's return sentinel is.
pub(super) fn entry(module: &mut Module, params: Vec<ValueId>, body: NodeId) {
    let function = module.reserve_function();
    let return_cont = module.reserve_continuation();

    module.define_function(
        function,
        Function {
            debug_name: Some("main".into()),
            params,
            return_cont,
            body,
            droppable: false,
        },
    );
    module.set_entry(function);
}

/// Route `body` through a continuation taking `params`, and answer the node that enters it. A filler stands in for the incoming values: it inhabits whatever carrier the parameter settles at, so entering the continuation demands nothing and withholds nothing of its own.
pub(super) fn through_continuation(
    module: &mut Module,
    params: Vec<ValueId>,
    body: NodeId,
) -> NodeId {
    let head = module.reserve_continuation();
    let args = params.iter().map(|_| Atom::Filler).collect();

    module.define_continuation(
        head,
        Continuation {
            debug_name: Some("head".into()),
            params,
            body,
        },
    );

    module.add_node(Node::ApplyCont(Edge { target: head, args }))
}

/// Define the reserved `function` as `name(params)`, its body built by `body` against the function's own return sentinel, which it is handed.
pub(super) fn define(
    module: &mut Module,
    function: FunctionId,
    name: &str,
    params: Vec<ValueId>,
    body: impl FnOnce(&mut Module, ContinuationId) -> NodeId,
) {
    let return_cont = module.reserve_continuation();
    let body = body(module, return_cont);
    module.define_function(
        function,
        Function {
            debug_name: Some(name.into()),
            params,
            return_cont,
            body,
            droppable: false,
        },
    );
}

/// Define the reserved `continuation` taking `params`, its body `body`.
pub(super) fn resume(
    module: &mut Module,
    continuation: ContinuationId,
    params: Vec<ValueId>,
    body: NodeId,
) {
    module.define_continuation(
        continuation,
        Continuation {
            debug_name: Some("resume".into()),
            params,
            body,
        },
    );
}

/// The `Nat` literal `value`.
pub(super) fn nat(value: u32) -> Atom {
    Atom::Literal(Literal::Nat(Natural::from(value)))
}

/// Jump to a reserved-but-undefined continuation — the return sentinel, which demands nothing of what it carries.
pub(super) fn finish(module: &mut Module, args: Vec<Atom>) -> NodeId {
    let sentinel = module.reserve_continuation();
    module.add_node(Node::ApplyCont(Edge {
        target: sentinel,
        args,
    }))
}
