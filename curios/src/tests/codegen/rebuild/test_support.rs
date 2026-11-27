//! What the suites of `rebuild` share: the programs they compile and the rebuilds read off them.

use {
    curios_common::fill,
    curios_cps::{Module, Node, ValueExpr},
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_native::{Compiled, ForeignBindings, test_support::*},
    curios_pipeline::{Stage, compile_with_prelude},
    curios_surface::Entrypoint,
    curios_wasm::{Names, to_bytes},
};

/// A program reading `n` from its first argument and printing `result`, under `definitions`.
pub fn program(definitions: &str, result: &str) -> String {
    fill!(
        r#"
        use /std/{Str, Nat, Bool, Map, List, Option, Result, Try, Io, Proc, Fmt};
        ${definitions}
        let execute() -> Try(Io, Str, {}) =>
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

/// The module `source` lowers to before the CPS optimizer, and the one it is emitted from.
pub fn modules(source: &str) -> (Module, Module) {
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
    (
        lowered.expect("the pipeline observes cps"),
        emitted.expect("the pipeline observes cps-optm"),
    )
}

/// The rows `module` rebuilds, by name, with each rebuild's slots.
pub fn rebuilt(module: &Module) -> Vec<(String, Vec<usize>)> {
    module
        .live_nodes()
        .filter_map(|(_, node)| match node {
            Node::LetValue {
                value: ValueExpr::Rebuilt { row, slots, .. },
                ..
            } => Some((
                module.row(*row).debug_name.clone().unwrap_or_default(),
                slots.iter().map(|(slot, _)| *slot).collect(),
            )),
            _ => None,
        })
        .collect()
}

/// How many rebuilds of a row named `row` `module` holds.
pub fn rebuilds_of(module: &Module, row: &str) -> usize {
    rebuilt(module)
        .iter()
        .filter(|(name, _)| name.ends_with(row))
        .count()
}

/// The exit code and output of `module` run with `n` as its argument.
pub fn run(module: &Module, n: &str) -> (u8, Vec<u8>) {
    let (wasm, foreigns) = curios_emit::into_wasm(module, module.entry_suspension());
    start_test_engine();
    let code = curios_cranelift::compile(&to_bytes(&wasm, &Names::default()))
        .expect("the module validates");
    let (host, io) = MockHost::builder().args(["program", n]).build();
    let status = Compiled::of(code, foreigns)
        .run(host, ForeignBindings::empty())
        .expect("the program runs");
    (status, io.output())
}

/// A map threaded once through a loop of inserts.
pub const SPINES: &str = r#"
    let walk(n: Nat, x: Nat, m: Map(Nat, Nat)) -> Map(Nat, Nat) =>
        match n
        | 0 => m
        | k + 1 =>
            let y = 75 * x % 65537;
            walk(k, y, Map/insert(m, y, k))
        end;
"#;

/// A map threaded once through a loop of inserts that counts its rounds, which two calls each start with a map of its own.
pub const TWO_WALKS: &str = r#"
    let walk(n: Nat, i: Nat, x: Nat, m: Map(Nat, Nat)) -> Map(Nat, Nat) =>
        match n
        | 0 => m
        | k + 1 =>
            let y = 75 * x % 65537;
            walk(k, i + 1, y, Map/insert(m, y, i % 1000003))
        end;
    let sum(m: Map(Nat, Nat)) -> Nat => Map/fold(m, 0, (_, v, acc) => (acc + v) % 1000003);
"#;

/// The two walks of [`TWO_WALKS`] unioned, the union summed.
pub const TWO_WALKS_UNION: &str = "sum(Map/union(walk(n, 0, (n + 1) % 65537, Map/empty()), walk(n, 0, (n + 7) % 65537, Map/empty())))";

/// One walk of [`TWO_WALKS`] filtered, what is left summed.
pub const WALK_FILTERED: &str =
    "sum(Map/filter(walk(n, 0, (n + 1) % 65537, Map/empty()), (k, v) => (k + v) % 3 == 0))";

/// A map built from a list of pairs, summed.
pub const PAIRS_AS_A_MAP: &str = "sum(Map/of_list([(n, 1), (n + 1, 2), (n + 2, 3)]))";

/// What the two walks of [`TWO_WALKS`] hold, summed.
pub const TWO_WALKS_SUM: &str =
    "sum(walk(n, 0, (n + 1) % 65537, Map/empty())) + sum(walk(n, 0, (n + 7) % 65537, Map/empty()))";

/// A search tree whose insert rebuilds the node it takes apart on every path, threaded once through a loop.
pub const SEARCH_TREE: &str = r#"
    induct Tree: Type
    | leaf()
    | node(left: Tree, key: Nat, right: Tree)
    end
    let insert(t: Tree, k: Nat) -> Tree =>
        match t
        | leaf() => Tree/node(Tree/leaf(), k, Tree/leaf())
        | node(l, x, r) =>
            match
            | k < x => Tree/node(insert(l, k), x, r)
            | x < k => Tree/node(l, x, insert(r, k))
            | _ => Tree/node(l, x, r)
            end
        end;
    let size(t: Tree) -> Nat => match t | leaf() => 0 | node(l, _, r) => size(l) + 1 + size(r) end;
    let walk(n: Nat, x: Nat, t: Tree) -> Tree =>
        match n | 0 => t | k + 1 => let y = 75 * x % 65537; walk(k, y, insert(t, y)) end;
    let shared(n: Nat) -> Tree => Tree/node(Tree/leaf(), n + 1, Tree/leaf());
"#;

/// A map filled with `n` keys, then drained of every other one and of keys it never held.
pub const REMOVALS: &str = r#"
    let fill(n: Nat, m: Map(Nat, Nat)) -> Map(Nat, Nat) =>
        match n | 0 => m | k + 1 => fill(k, Map/insert(m, k * 7 % 1009, k)) end;
    let drain(n: Nat, m: Map(Nat, Nat)) -> Map(Nat, Nat) =>
        match n | 0 => m | k + 1 => drain(k, Map/remove(m, k * 14 % 1009)) end;
"#;

/// A trie of eight twigs a structure holds, probed through `get`, an accessor the inliner leaves a call, before each replace takes the twig at its key out and puts back what replaces it.
pub const ACCESSED_TRIE: &str = r#"
    struct T(X: Type): Type { c0: X, c1: X, c2: X, c3: X, c4: X, c5: X, c6: X, c7: X }
    induct N: Type
    | empty()
    | leaf(key: Nat, value: Nat)
    | branch(twigs: T(N))
    end
    let get(@X: Type, t: T(X), d: Nat) -> X => match d | 0 => t.c0 | 1 => t.c1 | 2 => t.c2 | 3 => t.c3 | 4 => t.c4 | 5 => t.c5 | 6 => t.c6 | _ => t.c7 end;
    let set(@X: Type, t: T(X), d: Nat, x: X) -> T(X) => match d | 0 => T { ..t, c0 = x } | 1 => T { ..t, c1 = x } | 2 => T { ..t, c2 = x } | 3 => T { ..t, c3 = x } | 4 => T { ..t, c4 = x } | 5 => T { ..t, c5 = x } | 6 => T { ..t, c6 = x } | _ => T { ..t, c7 = x } end;
    let take(t: T(N), d: Nat) -> {N, T(N)} => match d | 0 => (t.c0, T { ..t, c0 = N/empty() }) | 1 => (t.c1, T { ..t, c1 = N/empty() }) | 2 => (t.c2, T { ..t, c2 = N/empty() }) | 3 => (t.c3, T { ..t, c3 = N/empty() }) | 4 => (t.c4, T { ..t, c4 = N/empty() }) | 5 => (t.c5, T { ..t, c5 = N/empty() }) | 6 => (t.c6, T { ..t, c6 = N/empty() }) | _ => (t.c7, T { ..t, c7 = N/empty() }) end;
    let build(depth: Nat, k: Nat) -> N =>
        match depth | 0 => N/leaf(k, 0) | d + 1 => N/branch(T { c0 = build(d, 8 * k + 0), c1 = build(d, 8 * k + 1), c2 = build(d, 8 * k + 2), c3 = build(d, 8 * k + 3), c4 = build(d, 8 * k + 4), c5 = build(d, 8 * k + 5), c6 = build(d, 8 * k + 6), c7 = build(d, 8 * k + 7) }) end;
    let near(t: N, k: Nat) -> Nat =>
        match t | empty() => 0 | leaf(k0, _) => k0 | branch(tw) => near(get(tw, k % 8), k / 8) end;
    let replace(t: N, k: Nat, v: Nat) -> N =>
        match t
        | empty() => t
        | leaf(k0, _) => N/leaf(k0, v)
        | branch(tw) =>
            let d = k % 8;
            let (s, rest) = take(tw, d);
            N/branch(set(rest, d, replace(s, k / 8, v)))
        end;
    let put(t: N, k: Nat, v: Nat) -> N => match near(t, k) != 1000000007 | true => replace(t, k, v) | false => t end;
    let fold_twigs(tw: T(N), i: Nat, acc: Nat) -> Nat =>
        match i < 8 | false => acc | true => fold_twigs(tw, i + 1, fold(get(tw, i), acc)) end
    and fold(t: N, acc: Nat) -> Nat =>
        match t | empty() => acc | leaf(_, v) => acc + v | branch(tw) => fold_twigs(tw, 0, acc) end;
    let walk(n: Nat, t: N) -> N => match n | 0 => t | j + 1 => walk(j, put(t, j % 64, j)) end;
"#;

/// What the trie of [`ACCESSED_TRIE`] holds after a walk of `n` puts, summed.
pub const ACCESSED_TRIE_SUM: &str = "fold(walk(n, build(2, n)), 0)";
