//! Frames on the heap: a program whose recursions run on their twins answers what the same program answers with every call native, whether the budget admits each call or refuses every one.

use {
    curios_common::fill,
    curios_cps::{Atom, Callee, Edge, IntrinsicCall, Literal, Module, Node, NodeId},
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_native::{Compiled, ForeignBindings, test_support::*},
    curios_num::Natural,
    curios_pipeline::{Stage, compile_with_prelude},
    curios_surface::Entrypoint,
    curios_wasm::to_bytes,
};

/// The module `source` is emitted from, its recursions on the heap, and the one it would be emitted from with every call native: its lowering optimized and rebuilt as the pipeline does, but kept off the heap.
fn modules(source: &str) -> (Module, Module) {
    let entrypoint = source.parse::<Entrypoint>().expect("fixture parses");
    let (mut lowered, mut emitted) = (None, None);
    compile_with_prelude(
        DEFAULT_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| match stage {
            Stage::Cps(module) => lowered = Some(module.clone()),
            Stage::CpsOptm(module) => emitted = Some(module.clone()),
            _ => {}
        },
    )
    .expect("fixture compiles");
    let mut native = lowered.expect("the pipeline observes cps");
    curios_cps::optimize(&mut native);
    curios_cps::rebuild_in_place(&mut native);
    (emitted.expect("the pipeline observes cps-optm"), native)
}

/// `module` with every call the budget guards refused it, so each goes to the heap and everything below it runs there.
fn refused(module: &Module) -> Module {
    let mut refused = module.clone();
    let enters: Vec<_> = refused
        .live_nodes()
        .filter_map(|(id, node)| match node {
            Node::Intrinsic {
                op: IntrinsicCall::FrameEnter { .. },
                return_to,
                ..
            } => Some((id, *return_to)),
            _ => None,
        })
        .collect();
    assert!(!enters.is_empty(), "the program guards no call:\n{module}");
    for (id, chosen) in enters {
        refused.remove_node(id);
        refused.define_node(
            id,
            Node::ApplyCont(Edge {
                target: chosen,
                args: vec![Atom::Literal(Literal::Nat(Natural::zero()))],
            }),
        );
    }
    refused
}

/// What `module` exits with and prints, given `n` as its first argument.
fn run(module: &Module, n: &str) -> (u8, Vec<u8>) {
    let (wasm, foreigns) = curios_emit::into_wasm(module);
    let code = curios_cranelift::compile(&to_bytes(&wasm)).expect("the module validates");
    let (host, io) = MockHost::builder().args(["program", n]).build();
    let status = Compiled::of(code, foreigns)
        .run(host, ForeignBindings::empty())
        .expect("the program runs");
    (status, io.output())
}

/// A program reading `n` from its first argument and printing `result`, under `definitions`.
fn program(definitions: &str, result: &str) -> String {
    fill!(
        r#"
        use /std/{Str, Nat, List, Option, Result, Try, Io, Proc, Fmt};
        ${definitions}
        let execute() -> Try(Io, Str, {}) =
            let arguments = Proc/args()!;
            let bytes = Option/to_result(List/try_get(arguments, 1), "no input")!;
            let s = Result/map_failure(Str/of_bytes(bytes), (_) => "invalid utf-8")!;
            let n = Option/to_result(Nat/of_str(Str/trim(s)), "bad input")!;
            let _ = Fmt/print("%\n")(${result})!;
            Try/pure(());
        execute()
        "#,
        definitions,
        result,
    )
}

/// Each recursion a twin is written for, by the shape that reaches its own part of the rewrite: a frame kept under a constructor, an answer the call's own result feeds, two members calling each other, a first call of two whose answer waits on the second, an answer handed back as a constructor's fields, a list map whose mapper is the recursion, and a closure no analysis resolves.
const RECURSIONS: [(&str, &str, &str); 8] = [
    (
        "a chain built under a constructor",
        r#"
        induct Chain: Type
        | stop()
        | link(Nat, Chain)
        end
        let build(n: Nat) -> Chain =
            match n | 0 => Chain/stop() | m + 1 => Chain/link(n, build(m)) end;
        let total(c: Chain) -> Nat =
            match c | stop() => 0 | link(x, rest) => x + total(rest) end;
        "#,
        "total(build(n))",
    ),
    (
        "an affine step on the call's own answer",
        r#"
        let step(n: Nat) -> Nat =
            match n | 0 => 0 | m + 1 => (step(m) * 3 + 1) % 1000003 end;
        "#,
        "step(n)",
    ),
    (
        "two members calling each other",
        r#"
        let ping(n: Nat) -> Nat =
            match n | 0 => 0 | m + 1 => (pong(m) * 3 + 1) % 1000003 end
        and pong(n: Nat) -> Nat =
            match n | 0 => 1 | m + 1 => (ping(m) * 5 + 2) % 1000003 end;
        "#,
        "ping(n)",
    ),
    (
        "a degenerate tree read by two calls a node",
        r#"
        induct Tree: Type
        | leaf()
        | node(Tree, Tree)
        end
        let grow(n: Nat) -> Tree =
            match n | 0 => Tree/leaf() | m + 1 => Tree/node(grow(m), Tree/leaf()) end;
        let depth(t: Tree) -> Nat =
            match t | leaf() => 0 | node(l, r) => Nat/max(depth(l), depth(r)) + 1 end;
        "#,
        "depth(grow(n))",
    ),
    (
        "a pair read apart at every level",
        r#"
        induct Pair: Type
        | pair(Nat, Nat)
        end
        let walk(n: Nat) -> Pair =
            match n
            | 0 => Pair/pair(0, 1)
            | m + 1 => match walk(m) | pair(a, b) => Pair/pair(b, (a + b) % 1000003) end
            end;
        let first(p: Pair) -> Nat = match p | pair(a, _) => a end;
        "#,
        "first(walk(n))",
    ),
    (
        "a tree read through a map of its children",
        r#"
        induct Rose: Type
        | rose(Nat, List(Rose))
        end
        let grow(n: Nat) -> Rose =
            match n | 0 => Rose/rose(0, []) | m + 1 => Rose/rose(n, [grow(m)]) end;
        let sum(l: List(Nat)) -> Nat =
            match l | [] => 0 | [x, ..rest] => x + sum(rest) end;
        let total(r: Rose) -> Nat =
            match r | rose(x, children) => (x + sum(List/map(children, total))) % 1000003 end;
        "#,
        "total(grow(n))",
    ),
    (
        "a call through a closure picked from a list",
        r#"
        let call(gs: List((Nat) -> Nat), i: Nat, n: Nat) -> Nat =
            match List/try_get(gs, i) | some(g) => g(n) | none() => n end;
        let down(n: Nat) -> Nat =
            match n
            | 0 => 0
            | m + 1 => (call([(x: Nat) => x, down], 1 - n % 2 + n % 2, m) * 3 + 1) % 1000003
            end;
        "#,
        "down(n)",
    ),
    (
        "a recursion defined within a recursion",
        r#"
        let outer(n: Nat) -> Nat =
            let inner(m: Nat) -> Nat =
                match m | 0 => n | j + 1 => (inner(j) * 3 + 1) % 1000003 end;
            match n | 0 => 0 | k + 1 => (outer(k) + inner(n % 7)) % 1000003 end;
        "#,
        "outer(n)",
    ),
];

/// The nodes a function's body holds from `body`, its continuations' included and the functions defined within it left out.
fn own_nodes(module: &Module, body: NodeId) -> Vec<NodeId> {
    let mut nodes = Vec::new();
    let mut work = vec![body];
    while let Some(node) = work.pop() {
        nodes.push(node);
        match module.node(node) {
            Some(Node::LetValue { next, .. } | Node::LetIntrinsic { next, .. }) => work.push(*next),
            Some(Node::LetFun { body, .. }) => work.push(*body),
            Some(Node::LetCont {
                continuations,
                body,
            }) => {
                work.push(*body);
                work.extend(
                    continuations
                        .iter()
                        .filter_map(|continuation| module.continuation(*continuation))
                        .map(|continuation| continuation.body),
                );
            }
            _ => {}
        }
    }
    nodes
}

/// Every recursion a million calls deep answers what it answers with every call native: past the budget's few thousand frames the rest runs on the heap, and a guest stack a gibibyte deep is what lets the native module answer too.
#[test]
fn every_recursion_a_million_deep_answers_past_the_budget_what_it_answers_natively() {
    for (shape, definitions, result) in RECURSIONS {
        let (heap, native) = modules(&program(definitions, result));
        assert_eq!(
            run(&heap, "1000000"),
            run(&native, "1000000"),
            "{shape} must answer alike a million deep"
        );
    }
}

/// Every call a function makes of itself that keeps a frame, outside the twins that run past the budget, asks the budget: it resumes where the weight is handed back. A recursion defined within another is guarded where it is copied into the outer one's twin, as where it stands.
#[test]
fn every_native_call_a_recursion_makes_of_itself_asks_the_budget() {
    let (definitions, result) = RECURSIONS
        .iter()
        .find(|(shape, _, _)| *shape == "a recursion defined within a recursion")
        .map(|(_, definitions, result)| (*definitions, *result))
        .expect("the fixture is among the recursions");
    let (heap, _) = modules(&program(definitions, result));
    for (function, definition) in heap.live_functions() {
        if definition
            .debug_name
            .as_deref()
            .is_some_and(|name| name.starts_with("heap/"))
        {
            continue;
        }
        for node in own_nodes(&heap, definition.body) {
            let Some(Node::ApplyFun {
                callee: Callee::Known(callee),
                return_to,
                ..
            }) = heap.node(node)
            else {
                continue;
            };
            if *callee != function || *return_to == definition.return_cont {
                continue;
            }
            let resumed = heap
                .continuation(*return_to)
                .map(|resume| heap.node(resume.body));
            assert!(
                matches!(
                    resumed,
                    Some(Some(Node::LetCont { body, .. }))
                        if matches!(heap.node(*body), Some(Node::Intrinsic { op: IntrinsicCall::FrameLeave { .. }, .. }))
                ),
                "{function} calls itself natively past the budget:\n{heap}"
            );
        }
    }
}

#[test]
fn every_recursion_answers_under_the_budget_and_past_it_what_it_answers_natively() {
    for (shape, definitions, result) in RECURSIONS {
        let (heap, native) = modules(&program(definitions, result));
        assert!(
            heap.live_functions()
                .any(|(function, _)| heap.twin(function).is_some()),
            "{shape} must run on a twin:\n{heap}"
        );
        // A function a guard stands in is on a cycle, so a closure of it has a twin to enter from the heap; one without would run natively there, frame upon frame.
        for (function, definition) in heap.live_functions() {
            let guarded = own_nodes(&heap, definition.body).into_iter().any(|node| {
                matches!(
                    heap.node(node),
                    Some(Node::Intrinsic {
                        op: IntrinsicCall::FrameEnter { .. },
                        ..
                    })
                )
            });
            assert!(
                !guarded
                    || heap
                        .twin(function)
                        .is_some_and(|twin| heap.function(twin).is_some()),
                "{shape}: {function} is guarded and has no twin:\n{heap}"
            );
        }
        let answer = run(&native, "2000");
        assert_eq!(
            run(&heap, "2000"),
            answer,
            "{shape} must answer alike under the budget"
        );
        assert_eq!(
            run(&refused(&heap), "2000"),
            answer,
            "{shape} must answer alike on the heap"
        );
    }
}
