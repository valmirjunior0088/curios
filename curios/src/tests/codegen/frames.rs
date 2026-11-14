//! Frames on the heap: a program whose recursions run on their twins answers what the same program answers with every call native.

use {
    curios_common::fill,
    curios_cps::Module,
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_native::{Compiled, ForeignBindings, test_support::*},
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
const RECURSIONS: [(&str, &str, &str); 7] = [
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
];

#[test]
fn every_recursion_answers_on_the_heap_what_it_answers_natively() {
    for (shape, definitions, result) in RECURSIONS {
        let (heap, native) = modules(&program(definitions, result));
        assert!(
            heap.live_functions()
                .any(|(function, _)| heap.twin(function).is_some()),
            "{shape} must run on a twin:\n{heap}"
        );
        assert_eq!(
            run(&heap, "2000"),
            run(&native, "2000"),
            "{shape} must answer alike on the heap"
        );
    }
}
