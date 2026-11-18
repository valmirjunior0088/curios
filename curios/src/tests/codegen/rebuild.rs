//! Rebuilding in place: `/std/Map/insert` rebuilding its node at every level, what looks fresh and is shared rebuilding nothing, and every program printing one answer whether it rebuilds or not.

use {
    curios_common::fill,
    curios_cps::{Atom, Intrinsic, Module, Node, ValueExpr, ValueId},
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_native::{Compiled, ForeignBindings, test_support::*},
    curios_pipeline::{Stage, compile_with_prelude},
    curios_surface::Entrypoint,
    curios_wasm::to_bytes,
    std::collections::BTreeSet,
};

/// A program reading `n` from its first argument and printing `result`, under `definitions`.
fn program(definitions: &str, result: &str) -> String {
    fill!(
        r#"
        use /std/{Str, Nat, Bool, Map, List, Option, Result, Try, Io, Proc, Fmt};
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

/// The module `source` lowers to before the CPS optimizer, and the one it is emitted from.
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
    (
        lowered.expect("the pipeline observes cps"),
        emitted.expect("the pipeline observes cps-optm"),
    )
}

/// The rows `module` rebuilds, by name, with each rebuild's slots.
fn rebuilt(module: &Module) -> Vec<(String, Vec<usize>)> {
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
fn rebuilds_of(module: &Module, row: &str) -> usize {
    rebuilt(module)
        .iter()
        .filter(|(name, _)| name.ends_with(row))
        .count()
}

/// The exit code and output of `module` run with `n` as its argument.
fn run(module: &Module, n: &str) -> (u8, Vec<u8>) {
    let (wasm, foreigns) = curios_emit::into_wasm(module, module.entry_suspension());
    let code = curios_cranelift::compile(&to_bytes(&wasm)).expect("the module validates");
    let (host, io) = MockHost::builder().args(["program", n]).build();
    let status = Compiled::of(code, foreigns)
        .run(host, ForeignBindings::empty())
        .expect("the program runs");
    (status, io.output())
}

const NODE: &str = "/std/Map/internal/Node/Node";

/// A map threaded once through a loop of inserts.
const SPINES: &str = r#"
    let walk(n: Nat, x: Nat, m: Map(Nat, Nat)) -> Map(Nat, Nat) =
        match n
        | 0 => m
        | k + 1 =>
            let y = 75 * x % 65537;
            walk(k, y, Map/insert(m, y, k))
        end;
"#;

/// A map threaded once through a loop of inserts that counts its rounds, which two calls each start with a map of its own.
const TWO_WALKS: &str = r#"
    let walk(n: Nat, i: Nat, x: Nat, m: Map(Nat, Nat)) -> Map(Nat, Nat) =
        match n
        | 0 => m
        | k + 1 =>
            let y = 75 * x % 65537;
            walk(k, i + 1, y, Map/insert(m, y, i % 1000003))
        end;
    let sum(m: Map(Nat, Nat)) -> Nat = Map/fold(m, 0, (_, v, acc) => (acc + v) % 1000003);
"#;

/// The two walks of [`TWO_WALKS`] unioned, the union summed.
const TWO_WALKS_UNION: &str = "sum(Map/union(walk(n, 0, (n + 1) % 65537, Map/empty()), walk(n, 0, (n + 7) % 65537, Map/empty())))";

/// One walk of [`TWO_WALKS`] filtered, what is left summed.
const WALK_FILTERED: &str =
    "sum(Map/filter(walk(n, 0, (n + 1) % 65537, Map/empty()), (k, v) => (k + v) % 3 == 0))";

/// A map built from a list of pairs, summed.
const PAIRS_AS_A_MAP: &str = "sum(Map/of_list([(n, 1), (n + 1, 2), (n + 2, 3)]))";

/// What the two walks of [`TWO_WALKS`] hold, summed.
const TWO_WALKS_SUM: &str =
    "sum(walk(n, 0, (n + 1) % 65537, Map/empty())) + sum(walk(n, 0, (n + 7) % 65537, Map/empty()))";

/// A search tree whose insert rebuilds the node it takes apart on every path, threaded once through a loop.
const SEARCH_TREE: &str = r#"
    induct Tree: Type
    | leaf()
    | node(left: Tree, key: Nat, right: Tree)
    end
    let insert(t: Tree, k: Nat) -> Tree =
        match t
        | leaf() => Tree/node(Tree/leaf(), k, Tree/leaf())
        | node(l, x, r) =>
            match
            | k < x => Tree/node(insert(l, k), x, r)
            | x < k => Tree/node(l, x, insert(r, k))
            | _ => Tree/node(l, x, r)
            end
        end;
    let size(t: Tree) -> Nat = match t | leaf() => 0 | node(l, _, r) => size(l) + 1 + size(r) end;
    let walk(n: Nat, x: Nat, t: Tree) -> Tree =
        match n | 0 => t | k + 1 => let y = 75 * x % 65537; walk(k, y, insert(t, y)) end;
    let shared(n: Nat) -> Tree = Tree/node(Tree/leaf(), n + 1, Tree/leaf());
"#;

/// A rope whose `tip` rides as its bare number, reversed onto an accumulator by a loop that recolors each link it takes apart.
const ROPE: &str = r#"
    induct Rope: Type
    | tip(value: Nat)
    | red(value: Nat, rest: Rope)
    | blue(value: Nat, rest: Rope)
    end
    let make(n: Nat, acc: Rope) -> Rope =
        match n | 0 => acc | k + 1 => make(k, Rope/red(k, acc)) end;
    let flip(r: Rope, acc: Rope) -> Rope =
        match r
        | tip(_) => acc
        | red(v, rest) => flip(rest, Rope/blue(v, acc))
        | blue(v, rest) => flip(rest, Rope/red(v + 1, acc))
        end;
    let total(r: Rope, sum: Nat) -> Nat =
        match r
        | tip(v) => (sum + v) % 1000003
        | red(v, rest) => total(rest, (sum + v) % 1000003)
        | blue(v, rest) => total(rest, (sum + 2 * v) % 1000003)
        end;
"#;

/// A rope of [`ROPE`] reversed twice, each reversal started on a tip that is a literal.
const ROPE_FLIPPED_TWICE: &str =
    "total(flip(flip(make(n, Rope/tip(n)), Rope/tip(1)), Rope/tip(2)), 0)";

/// A rope of [`ROPE`]'s links in alternating colors, built over a tip that is a literal.
const MIXED_ROPE: &str = r#"
    let mixed(n: Nat, acc: Rope) -> Rope =
        match n
        | 0 => acc
        | k + 1 =>
            match k % 2
            | 0 => mixed(k, Rope/red(k, acc))
            | _ => mixed(k, Rope/blue(k, acc))
            end
        end;
"#;

/// A map filled with `n` keys, then drained of every other one and of keys it never held.
const REMOVALS: &str = r#"
    let fill(n: Nat, m: Map(Nat, Nat)) -> Map(Nat, Nat) =
        match n | 0 => m | k + 1 => fill(k, Map/insert(m, k * 7 % 1009, k)) end;
    let drain(n: Nat, m: Map(Nat, Nat)) -> Map(Nat, Nat) =
        match n | 0 => m | k + 1 => drain(k, Map/remove(m, k * 14 % 1009)) end;
"#;

/// A trie of eight twigs a structure holds, probed through `get`, an accessor the inliner leaves a call, before each replace takes the twig at its key out and puts back what replaces it.
const ACCESSED_TRIE: &str = r#"
    struct T(X: Type): Type { c0: X, c1: X, c2: X, c3: X, c4: X, c5: X, c6: X, c7: X }
    induct N: Type
    | empty()
    | leaf(key: Nat, value: Nat)
    | branch(twigs: T(N))
    end
    let get(@X: Type, t: T(X), d: Nat) -> X = match d | 0 => t.c0 | 1 => t.c1 | 2 => t.c2 | 3 => t.c3 | 4 => t.c4 | 5 => t.c5 | 6 => t.c6 | _ => t.c7 end;
    let set(@X: Type, t: T(X), d: Nat, x: X) -> T(X) = match d | 0 => T { ..t, c0 = x } | 1 => T { ..t, c1 = x } | 2 => T { ..t, c2 = x } | 3 => T { ..t, c3 = x } | 4 => T { ..t, c4 = x } | 5 => T { ..t, c5 = x } | 6 => T { ..t, c6 = x } | _ => T { ..t, c7 = x } end;
    let take(t: T(N), d: Nat) -> {N, T(N)} = match d | 0 => (t.c0, T { ..t, c0 = N/empty() }) | 1 => (t.c1, T { ..t, c1 = N/empty() }) | 2 => (t.c2, T { ..t, c2 = N/empty() }) | 3 => (t.c3, T { ..t, c3 = N/empty() }) | 4 => (t.c4, T { ..t, c4 = N/empty() }) | 5 => (t.c5, T { ..t, c5 = N/empty() }) | 6 => (t.c6, T { ..t, c6 = N/empty() }) | _ => (t.c7, T { ..t, c7 = N/empty() }) end;
    let build(depth: Nat, k: Nat) -> N =
        match depth | 0 => N/leaf(k, 0) | d + 1 => N/branch(T { c0 = build(d, 8 * k + 0), c1 = build(d, 8 * k + 1), c2 = build(d, 8 * k + 2), c3 = build(d, 8 * k + 3), c4 = build(d, 8 * k + 4), c5 = build(d, 8 * k + 5), c6 = build(d, 8 * k + 6), c7 = build(d, 8 * k + 7) }) end;
    let near(t: N, k: Nat) -> Nat =
        match t | empty() => 0 | leaf(k0, _) => k0 | branch(tw) => near(get(tw, k % 8), k / 8) end;
    let replace(t: N, k: Nat, v: Nat) -> N =
        match t
        | empty() => t
        | leaf(k0, _) => N/leaf(k0, v)
        | branch(tw) =>
            let d = k % 8;
            let (s, rest) = take(tw, d);
            N/branch(set(rest, d, replace(s, k / 8, v)))
        end;
    let put(t: N, k: Nat, v: Nat) -> N = match near(t, k) != 1000000007 | true => replace(t, k, v) | false => t end;
    let fold_twigs(tw: T(N), i: Nat, acc: Nat) -> Nat =
        match i < 8 | false => acc | true => fold_twigs(tw, i + 1, fold(get(tw, i), acc)) end
    and fold(t: N, acc: Nat) -> Nat =
        match t | empty() => acc | leaf(_, v) => acc + v | branch(tw) => fold_twigs(tw, 0, acc) end;
    let walk(n: Nat, t: N) -> N = match n | 0 => t | j + 1 => walk(j, put(t, j % 64, j)) end;
"#;

/// What the trie of [`ACCESSED_TRIE`] holds after a walk of `n` puts, summed.
const ACCESSED_TRIE_SUM: &str = "fold(walk(n, build(2, n)), 0)";

/// A list of four counters, each round of a loop adding to the one its round names: read at an index, then updated there.
const BUMPS: &str = r#"
let bump(k: Nat, l: List(Nat)) -> List(Nat) =
    match k
    | 0 => l
    | j + 1 =>
        let i = j % 4;
        bump(j, match i < List/len(l) | true => List/set(l, i, List/get(l, i) + j) | false => l end)
    end;
"#;

/// What the counters of [`BUMPS`] hold after `n` rounds, summed.
const BUMPS_SUM: &str = "List/fold(bump(n, [n, n, n, n]), 0, (x, acc) => (acc + x) % 1000003)";

/// A list updated while a window of it, an append to it and the list itself are still read, and a constant list updated while it is read.
const KEPT_LISTS: &str = r#"
let kept(n: Nat) -> Nat =
    let l = [n, n + 1, n + 2, n + 3];
    let w = List/slice(l, 1, 2);
    let c = [..l, n];
    let u = List/set(l, 1, 0);
    let k = [1, 2, 3];
    let v = List/set(k, 0, n);
    List/get(l, 1) + List/get(w, 0) + List/get(c, 1) + List/get(u, 1) + List/get(k, 0) + List/get(v, 0);
"#;

/// A trie whose nodes hold their twigs in a list, an insert reading the twig at its key and updating the list there with what replaces it.
const LIST_TRIE: &str = r#"
induct N: Type
| empty()
| leaf(key: Nat, value: Nat)
| branch(twigs: List(N))
end
let fresh(k: Nat, v: Nat, d: Nat) -> N =
    match d
    | 0 => N/leaf(k, v)
    | e + 1 => N/branch([fresh(k, v, e), N/empty(), N/empty(), N/empty()])
    end;
let put(t: N, k: Nat, v: Nat) -> N =
    match t
    | empty() => t
    | leaf(k0, _) => N/leaf(k0, v)
    | branch(tw) =>
        let d = k % 4;
        match d < List/len(tw)
        | true => N/branch(List/set(tw, d, put(List/get(tw, d), k / 4, v)))
        | false => N/branch(tw)
        end
    end;
let sum(t: N) -> Nat =
    match t
    | empty() => 0
    | leaf(_, v) => v
    | branch(tw) => List/fold(tw, 0, (c, acc) => (acc + sum(c)) % 1000003)
    end;
let walk(n: Nat, t: N) -> N =
    match n
    | 0 => t
    | m + 1 => walk(m, put(t, m, m + 1))
    end;
"#;

/// What the trie of [`LIST_TRIE`] holds after a walk of `n` puts, summed.
const LIST_TRIE_SUM: &str = "sum(walk(n, fresh(0, n, 6)))";

/// The updates of a list `module` writes into the list it is handed.
fn overwrites(module: &Module) -> usize {
    module
        .live_nodes()
        .filter(|(_, node)| {
            matches!(
                node,
                Node::LetIntrinsic {
                    op: Intrinsic::ListOverwrite,
                    ..
                }
            )
        })
        .count()
}

/// The constructions keeping a slot of the node the function named `name` takes, which a rebuild in place leaves none of: each reads a field of it.
fn copies_keeping_a_slot(module: &Module, name: &str) -> usize {
    let taken = module
        .live_functions()
        .find(|(_, function)| {
            function
                .debug_name
                .as_deref()
                .is_some_and(|debug| debug.ends_with(name))
        })
        .map(|(_, function)| function.params[0])
        .unwrap_or_else(|| panic!("`{name}` is a function of its own"));
    let fields = module
        .live_nodes()
        .filter_map(|(_, node)| match node {
            Node::LetIntrinsic {
                result,
                op: Intrinsic::RowGet(..),
                args,
                ..
            } if args.as_slice() == [Atom::Value(taken)] => Some(*result),
            _ => None,
        })
        .collect::<BTreeSet<ValueId>>();
    module
        .live_nodes()
        .filter(|(_, node)| {
            matches!(node, Node::LetValue { value: ValueExpr::Row(_, atoms), .. }
                if atoms.iter().any(|atom| matches!(atom, Atom::Value(value) if fields.contains(value))))
        })
        .count()
}

/// The updates the function named `name` makes of a list it reads out of the node it takes, as the copies and the writes in place among them.
fn twig_updates(module: &Module, name: &str) -> (usize, usize) {
    let taken = module
        .live_functions()
        .find(|(_, function)| {
            function
                .debug_name
                .as_deref()
                .is_some_and(|debug| debug.ends_with(name))
        })
        .map(|(_, function)| function.params[0])
        .unwrap_or_else(|| panic!("`{name}` is a function of its own"));
    let lists = module
        .live_nodes()
        .filter_map(|(_, node)| match node {
            Node::LetIntrinsic {
                result,
                op: Intrinsic::RowGet(..),
                args,
                ..
            } if args.as_slice() == [Atom::Value(taken)] => Some(*result),
            _ => None,
        })
        .collect::<BTreeSet<ValueId>>();
    let updates = |wanted: Intrinsic| {
        module
            .live_nodes()
            .filter(|(_, node)| {
                matches!(node, Node::LetIntrinsic { op, args, .. }
                    if *op == wanted && matches!(args.first(), Some(Atom::Value(list)) if lists.contains(list)))
            })
            .count()
    };
    (
        updates(Intrinsic::ListSet),
        updates(Intrinsic::ListOverwrite),
    )
}

/// Every construction `/std/Map/internal/Node/insert` makes keeping a slot of the node it took apart rebuilds that node, at every level of the descent.
#[test]
fn map_insert_rebuilds_its_node_at_every_level_in_a_loop_using_its_map_once() {
    let (_, module) = modules(&program(
        SPINES,
        "Map/len(walk(n, (n + 1) % 65537, Map/empty()))",
    ));
    assert_eq!(
        copies_keeping_a_slot(&module, "/std/Map/internal/Node/insert"),
        0,
        "{module}"
    );
    assert!(rebuilds_of(&module, NODE) >= 8, "{module}");
}

/// A map a read-only walk takes apart before an insert: `Map/update` reads the value at its key through `try_get`'s descent, which the optimizer makes a loop, and `Map/has` asks the same of a key before an insert at it, each arm inserting another value. The loop only reads the map it is handed, so its parameter borrows it, the insert after it holds the one name the map goes by, and it rebuilds its node at every level as an insert alone does. Mutation-checked: with a jump into a continuation counted as consuming what it hands, both copy every level.
#[test]
fn a_map_a_loop_reads_before_an_insert_rebuilds_its_node_at_every_level() {
    for insert in [
        "Map/update(m, y, (_) => Option/some(k))",
        "match Map/has(m, y) | true => Map/insert(m, y, k + 1) | false => Map/insert(m, y, k) end",
    ] {
        let walk = fill!(
            r#"
            let walk(n: Nat, x: Nat, m: Map(Nat, Nat)) -> Map(Nat, Nat) =
                match n
                | 0 => m
                | k + 1 =>
                    let y = 75 * x % 65537;
                    walk(k, y, ${insert})
                end;
            "#,
            insert
        );
        let (_, module) = modules(&program(
            &walk,
            "Map/len(walk(n, (n + 1) % 65537, Map/empty()))",
        ));
        assert_eq!(
            copies_keeping_a_slot(&module, "/std/Map/internal/Node/insert"),
            0,
            "{insert}:\n{module}"
        );
        assert!(rebuilds_of(&module, NODE) >= 8, "{insert}:\n{module}");
    }
}

/// One map handed at once to a parameter that only reads it and to one that inserts into it, by a jump into a loop and by a call: the reader names the object the insert would write, so it does not borrow it, and each program answers as it does when the insert copies. And a loop reading a map the function around it inserts into while the loop's parameter is live: the insert would write what the parameter still reads, and the program answers as if it copied. The optimizer reorders, splits and routes these programs so that no rule of the borrow alone decides them; each rule is held to its own shape in `curios-cps`'s `rebuild::tests`, and these hold whole programs to their answers.
#[test]
fn a_map_a_reader_holds_beside_its_insert_answers_as_if_copied() {
    let definitions = r#"
        let fill(n: Nat, m: Map(Nat, Nat)) -> Map(Nat, Nat) =
            match n | 0 => m | k + 1 => fill(k, Map/insert(m, n, n + 6)) end;
        let looped(a: Map(Nat, Nat), b: Map(Nat, Nat), k: Nat) -> Nat =
            match k
            | 0 =>
                let b2 = Map/insert(b, 1, 99);
                Option/unwrap_or(Map/try_get(a, 1), 0) * 1000 + Map/len(b2)
            | j + 1 => looped(a, b, j)
            end;
        let called(a: Map(Nat, Nat), b: Map(Nat, Nat), k: Nat) -> Nat =
            match k
            | 0 =>
                let b2 = Map/insert(b, 1, 99);
                Option/unwrap_or(Map/try_get(a, 1), 0) * 1000 + Map/len(b2)
            | j + 1 => match called(a, b, j) | 0 => 1 | i + 1 => i + 1 end
            end;
        let outer(x: Map(Nat, Nat), n: Nat) -> Nat =
            let probe(t: Map(Nat, Nat), k: Nat) -> Nat =
                match k
                | 0 =>
                    let x2 = Map/insert(x, 1, 99);
                    Map/len(x2) * 1000 + Option/unwrap_or(Map/try_get(t, 1), 0)
                | j + 1 => probe(t, j)
                end;
            probe(x, n);
    "#;
    for start in [
        "(let m = fill(n % 50 + 3, Map/empty()); looped(m, m, n % 7))",
        "(let m = fill(n % 50 + 3, Map/empty()); let p = fill(n % 50 + 4, Map/empty()); called(m, m, n % 7) + called(p, p, n % 7) * 100000)",
        "outer(fill(n % 50 + 3, Map/empty()), n % 7)",
    ] {
        let (lowered, module) = modules(&program(definitions, start));
        let mut kept = lowered;
        curios_cps::optimize(&mut kept);
        assert_eq!(run(&module, "300"), run(&kept, "300"), "{start}");
    }
}

/// Two loops each threading a map of its own through its inserts: `walk` stays a function both call, whose loop hands its map back where it ends, and what a later round does to the map it is handed is done to another map than the one a round took apart, so each insert still reaches the version that rebuilds its node at every level.
#[test]
fn map_insert_rebuilds_its_node_in_each_of_two_loops_threading_a_map_of_its_own() {
    let (_, module) = modules(&program(TWO_WALKS, TWO_WALKS_SUM));
    assert!(rebuilds_of(&module, NODE) >= 8, "{module}");
}

/// A payload read back out of a map or a list shares the slot it is stored in and nothing above it: a map nothing else holds is rebuilt at every level where its program unions it with another, filters it, or builds it from a list of pairs, and the walks beside those keep their rebuilds.
#[test]
fn map_insert_rebuilds_its_node_over_a_payload_read_back_out_of_a_map_or_a_list() {
    for result in [TWO_WALKS_UNION, WALK_FILTERED, PAIRS_AS_A_MAP] {
        let (_, module) = modules(&program(TWO_WALKS, result));
        assert!(rebuilds_of(&module, NODE) >= 8, "{result}: {module}");
    }
}

/// A program that inserts into a map it reads again, beside two walks it unions: the insert into what something else holds shares the children it copies around, each with the fork that holds them, so it says nothing of the forks the walks and the union build, which keep their rebuilds.
#[test]
fn an_insert_into_a_map_read_again_leaves_the_maps_held_once_their_rebuilds() {
    let source = program(
        &[
            TWO_WALKS,
            r#"
            let held(n: Nat) -> Nat =
                let m = walk(3, 0, n % 65537, Map/empty());
                Map/len(Map/insert(m, 70000, 1)) + Map/len(m);
            "#,
        ]
        .concat(),
        &[TWO_WALKS_UNION, " + held(n)"].concat(),
    );
    let (lowered, module) = modules(&source);
    assert!(rebuilds_of(&module, NODE) >= 8, "{module}");
    let mut kept = lowered;
    curios_cps::optimize(&mut kept);
    assert_eq!(run(&module, "300"), run(&kept, "300"));
}

/// A map whose keys are removed one by one, each removal's map used once: every fork `/std/Map/internal/Node/remove` makes keeping a slot of the node it took apart rebuilds that node, the key found or absent.
#[test]
fn map_remove_rebuilds_its_node_at_every_level_in_a_loop_using_its_map_once() {
    let (_, module) = modules(&program(
        REMOVALS,
        "Map/len(drain(n, fill(n, Map/empty())))",
    ));
    assert_eq!(
        copies_keeping_a_slot(&module, "/std/Map/internal/Node/remove"),
        0,
        "{module}"
    );
    assert!(rebuilds_of(&module, NODE) >= 4, "{module}");
}

/// A map nothing else holds has each level's twigs written in place by an insert and by a remove: the descent reads the twig at its digit, hands it down and sets what comes back at the same index, so the list is the node's alone and nothing else reads it, and no update of it copies. Mutation-checked: with no list written in place, both copy at every level.
#[test]
fn a_maps_descent_writes_each_levels_twigs_in_place() {
    for (definitions, result, function) in [
        (
            SPINES,
            "Map/len(walk(n, (n + 1) % 65537, Map/empty()))",
            "/std/Map/internal/Node/insert",
        ),
        (
            REMOVALS,
            "Map/len(drain(n, fill(n, Map/empty())))",
            "/std/Map/internal/Node/remove",
        ),
    ] {
        let (_, module) = modules(&program(definitions, result));
        let (copies, writes) = twig_updates(&module, function);
        assert_eq!(copies, 0, "{function}:\n{module}");
        assert!(writes >= 2, "{function}:\n{module}");
    }
}

/// What looks fresh and is shared: a map read after its insert, a map whose successor's predecessor is read again, a map a list holds, a map a closure captures, and a map that is constant data. Each program answers as it does when every insert copies, a rebuild of what another map still holds showing in what it prints.
#[test]
fn a_map_something_else_still_holds_answers_as_if_copied() {
    let walks = [
        // Read after its insert.
        r#"
        let walk(n: Nat, m: Map(Nat, Nat), acc: Nat) -> Nat =
            match n
            | 0 => acc + Map/len(m)
            | k + 1 => let m2 = Map/insert(m, k % 97, k); walk(k, m2, acc + Map/len(m))
            end;
        "#,
        // Two maps sharing a subtree, the older read again.
        r#"
        let walk(n: Nat, m: Map(Nat, Nat), acc: Nat) -> Nat =
            match n
            | 0 => acc + Map/len(m)
            | k + 1 =>
                let m1 = Map/insert(m, k % 89, k);
                let m2 = Map/insert(m1, k % 97, k);
                walk(k, m2, acc + Map/len(m1))
            end;
        "#,
        // Held by a list.
        r#"
        let walk(n: Nat, ms: List(Map(Nat, Nat)), acc: Nat) -> Nat =
            match List/try_get(ms, 0)
            | none() => acc
            | some(m) =>
                match n
                | 0 => acc + Map/len(m)
                | k + 1 => walk(k, [Map/insert(m, k % 97, k), m], acc + Map/len(m))
                end
            end;
        "#,
        // Captured by a closure called after the insert.
        r#"
        let walk(n: Nat, m: Map(Nat, Nat), acc: Nat) -> Nat =
            match n
            | 0 => acc + Map/len(m)
            | k + 1 =>
                let size = () => Map/len(m);
                let m2 = Map/insert(m, k % 97, k);
                walk(k, m2, acc + size())
            end;
        "#,
        // Constant data inserted into afresh on every round.
        r#"
        let base = Map/insert(Map/insert(Map/empty(), 1, 1), 2, 2);
        let walk(n: Nat, m: Map(Nat, Nat), acc: Nat) -> Nat =
            match n
            | 0 => acc + Map/len(m)
            | k + 1 => walk(k, m, acc + Map/len(Map/insert(base, k % 97, k)))
            end;
        "#,
    ];
    for (walk, start) in walks.iter().zip([
        "walk(n, Map/empty(), 0)",
        "walk(n, Map/empty(), 0)",
        "walk(n, [Map/empty()], 0)",
        "walk(n, Map/empty(), 0)",
        "walk(n, Map/empty(), 0)",
    ]) {
        let (lowered, module) = modules(&program(walk, start));
        let mut kept = lowered;
        curios_cps::optimize(&mut kept);
        assert_eq!(run(&module, "300"), run(&kept, "300"), "{walk}");
    }
}

/// A map a cell holds: taken out of it, inserted into and taken out again, the cell still hands back the map it was filled with, as it does when the insert copies.
#[test]
fn a_map_a_cell_holds_answers_as_if_copied() {
    let source = r#"
        use /std/{Str, Nat, Map, List, Option, Result, Try, Io, Proc, Fmt, Cell};
        let execute() -> Try(Io, Str, {}) =
            let arguments = Proc/args()!;
            let bytes = Option/to_result(List/try_get(arguments, 1), "no input")!;
            let s = Result/map_failure(Str/of_bytes(bytes), (_) => "invalid utf-8")!;
            let n = Option/to_result(Nat/of_str(Str/trim(s)), "bad input")!;
            let cell = Cell/new()!;
            let _ = Cell/fill(cell, Map/insert(Map/insert(Map/empty(), n, 1), n + 1, 2))!;
            let first = Cell/poll(cell)!;
            let grown = match first | some(m) => Map/len(Map/insert(m, n + 2, 3)) | none() => 0 end;
            let again = Cell/poll(cell)!;
            let kept = match again | some(m) => Map/len(m) | none() => 0 end;
            let _ = Fmt/print("%\n")(grown * 10 + kept)!;
            Try/pure(());
        execute()
    "#;
    let (lowered, module) = modules(source);
    let mut kept = lowered;
    curios_cps::optimize(&mut kept);
    assert_eq!(run(&module, "5"), run(&kept, "5"));
}

/// A search tree whose leaves are one constant: an insert rebuilds the fork it took apart, and never the leaf, which is the global every leaf of every tree shares.
#[test]
fn a_constant_leaf_a_fork_holds_is_never_rebuilt() {
    let tree = r#"
        induct Tree: Type
        | leaf()
        | node(left: Tree, key: Nat, right: Tree)
        end
        let insert(t: Tree, k: Nat) -> Tree =
            match t
            | leaf() => Tree/node(Tree/leaf(), k, Tree/leaf())
            | node(l, x, r) =>
                match
                | k < x => Tree/node(insert(l, k), x, r)
                | x < k => Tree/node(l, x, insert(r, k))
                | _ => Tree/node(l, x, r)
                end
            end;
        let size(t: Tree) -> Nat = match t | leaf() => 0 | node(l, _, r) => size(l) + 1 + size(r) end;
        let walk(n: Nat, x: Nat, t: Tree) -> Tree =
            match n | 0 => t | k + 1 => let y = 75 * x % 65537; walk(k, y, insert(t, y)) end;
    "#;
    let (_, module) = modules(&program(
        tree,
        "size(walk(n, (n + 1) % 65537, Tree/node(Tree/leaf(), n, Tree/leaf())))",
    ));
    let rebuilt = rebuilt(&module);
    let trees = rebuilt
        .iter()
        .filter(|(name, _)| name.ends_with("/Tree"))
        .collect::<Vec<_>>();
    assert!(!trees.is_empty(), "{module}");
    // A leaf rebuilt as a fork writes the tag.
    assert!(
        trees.iter().all(|(_, slots)| !slots.contains(&0)),
        "{module}"
    );
}

/// A search tree inserted into where it is read again, beside one threaded once through a loop: the first insert reaches the function as it is, and the loop, handed what each insert hands back, reaches a copy that rebuilds the forks it takes apart while the shared tree answers as it did.
#[test]
fn a_search_tree_shared_in_one_place_and_threaded_in_another_rebuilds_in_a_copy() {
    let source = program(
        SEARCH_TREE,
        "size(insert(shared(n), 3)) + size(shared(n)) + size(walk(n, (n + 1) % 65537, Tree/node(Tree/leaf(), n, Tree/leaf())))",
    );
    let (lowered, module) = modules(&source);
    let mut kept = lowered;
    curios_cps::optimize(&mut kept);
    assert_eq!(run(&module, "300"), run(&kept, "300"));
    let rebuilt = rebuilt(&module);
    let trees = rebuilt
        .iter()
        .filter(|(name, _)| name.ends_with("/Tree"))
        .collect::<Vec<_>>();
    assert!(trees.len() >= 2, "{module}");
    assert!(
        trees.iter().all(|(_, slots)| !slots.contains(&0)),
        "{module}"
    );
}

/// A tree a path copy was made of and is read again: the copy holds the branches the insert left untouched, which the original still holds, so a later insert into the copy, reaching the version that rebuilds, must not rebuild them. The loop that grows the tree rebuilds as it may, and every size the program prints is the one it prints when every insert copies.
#[test]
fn a_path_copy_keeps_the_branches_it_shares_out_of_a_rebuild() {
    let source = program(
        &[
            SEARCH_TREE,
            r#"
            let copied(n: Nat) -> Nat =
                let t0 = walk(n, (n + 1) % 65537, Tree/node(Tree/leaf(), n + 30000, Tree/leaf()));
                let t1 = insert(t0, 1);
                let t2 = insert(insert(t1, 2), 65000);
                size(t0) * 100000 + size(t1) * 100 + size(t2);
            "#,
        ]
        .concat(),
        "copied(n)",
    );
    let (lowered, module) = modules(&source);
    assert!(rebuilds_of(&module, "/Tree") > 0, "{module}");
    let mut kept = lowered;
    curios_cps::optimize(&mut kept);
    assert_eq!(run(&module, "50"), run(&kept, "50"));
}

/// A tree a loop binds once and reads a branch of on every round: each round's read names the one branch, so an insert into what a round read copies, and every round's size is the one it is when every insert copies.
#[test]
fn a_branch_read_on_every_round_of_a_loop_is_inserted_into_by_a_copy() {
    let source = program(
        &[
            SEARCH_TREE,
            r#"
            let drip(n: Nat, t: Tree) -> Nat =
                let go(k: Nat, acc: Nat) -> Nat =
                    match k
                    | 0 => acc
                    | j + 1 =>
                        match t
                        | leaf() => acc
                        | node(l, _, _) => go(j, acc + size(insert(l, 70000 + j)))
                        end
                    end;
                go(n, 0);
            "#,
        ]
        .concat(),
        "drip(n, walk(n, (n + 1) % 65537, Tree/node(Tree/leaf(), n + 100000, Tree/leaf())))",
    );
    let (lowered, module) = modules(&source);
    let mut kept = lowered;
    curios_cps::optimize(&mut kept);
    assert_eq!(run(&module, "5"), run(&kept, "5"));
}

/// A tree a loop hands on as it took it, a branch of it read and inserted into on every round: the tree the next round is handed is the one this round read, so the insert copies, and every round's size is the one it is when every insert copies.
#[test]
fn a_branch_of_a_tree_a_loop_hands_on_as_it_took_it_is_inserted_into_by_a_copy() {
    let source = program(
        &[
            SEARCH_TREE,
            r#"
            let drip(n: Nat, t: Tree, acc: Nat) -> Nat =
                match n
                | 0 => acc + size(t)
                | k + 1 =>
                    match t
                    | leaf() => acc
                    | node(l, _, _) => drip(k, t, acc + size(insert(l, 70000 + k)))
                    end
                end;
            "#,
        ]
        .concat(),
        "drip(n, walk(n, (n + 1) % 65537, Tree/node(Tree/leaf(), n + 100000, Tree/leaf())), 0)",
    );
    let (lowered, module) = modules(&source);
    let mut kept = lowered;
    curios_cps::optimize(&mut kept);
    assert_eq!(run(&module, "5"), run(&kept, "5"));
}

/// A loop whose accumulator starts as a literal, the bare number a tip is, and whose result a second loop takes apart, over a rope built on a tip that is the number the building loop's counter starts at too: neither a literal nor a number handed at two parameters is a row, so the accumulator a link is built on is no less unique for having been one, and each loop rebuilds the links it takes apart — the program collects as often as the one that only builds the rope.
#[test]
fn a_loop_started_on_a_literal_rebuilds_the_links_it_takes_apart() {
    let (lowered, module) = modules(&program(ROPE, ROPE_FLIPPED_TWICE));
    let (_, built) = modules(&program(ROPE, "total(make(n, Rope/tip(n)), 0)"));
    let collections = |module: &Module| collected(|| run(module, "600000")).1.collections;
    assert_eq!(collections(&module), collections(&built), "{module}");
    let mut kept = lowered;
    curios_cps::optimize(&mut kept);
    for n in ["0", "1", "300"] {
        assert_eq!(run(&module, n), run(&kept, n), "{n}");
    }
}

/// A loop that takes apart a rope nothing else holds, building a link in each of its arms: each round takes apart another link than the round before, so either arm rebuilds the link it takes apart, and the program collects as often as the one that only builds the rope. Collections are counted from the engine's own records, at a size whose links outgrow the heap's first half.
#[test]
fn a_loop_over_a_rope_nothing_else_holds_rebuilds_its_links_in_either_arm() {
    let rope = [ROPE, MIXED_ROPE].concat();
    let (lowered, flipped) = modules(&program(
        &rope,
        "total(flip(mixed(n, Rope/tip(0)), Rope/tip(1)), 0)",
    ));
    let (_, built) = modules(&program(&rope, "total(mixed(n, Rope/tip(0)), 0)"));
    let collections = |module: &Module| collected(|| run(module, "600000")).1.collections;
    assert_eq!(collections(&flipped), collections(&built), "{flipped}");
    let mut kept = lowered;
    curios_cps::optimize(&mut kept);
    for n in ["0", "1", "300"] {
        assert_eq!(run(&flipped, n), run(&kept, n), "{n}");
    }
}

/// A loop of inserts started on the constant empty tree: the tree it threads is that constant on its first round and a tree nothing else holds on every other, so each insert rebuilds the fork it takes apart, and never the leaf, which only its tag tells from a fork.
#[test]
fn a_loop_started_on_a_constant_rebuilds_the_forks_it_takes_apart() {
    let (lowered, module) = modules(&program(
        SEARCH_TREE,
        "size(walk(n, (n + 1) % 65537, Tree/leaf()))",
    ));
    let rebuilt = rebuilt(&module);
    let trees = rebuilt
        .iter()
        .filter(|(name, _)| name.ends_with("/Tree"))
        .collect::<Vec<_>>();
    assert!(!trees.is_empty(), "{module}");
    assert!(
        trees.iter().all(|(_, slots)| !slots.contains(&0)),
        "{module}"
    );
    let mut kept = lowered;
    curios_cps::optimize(&mut kept);
    for n in ["0", "1", "300"] {
        assert_eq!(run(&module, n), run(&kept, n), "{n}");
    }
}

/// One constant fork two loops of inserts each start on: the fork is one global both name, which a test of its tag does not tell from a fork a loop built, so no insert writes into what it is handed and the second loop starts on the fork the first did, each tree the size it is when every insert copies.
#[test]
fn a_constant_fork_two_loops_start_on_is_never_rebuilt() {
    let source = program(
        SEARCH_TREE,
        "size(walk(n, (n + 1) % 65537, Tree/node(Tree/leaf(), 500, Tree/leaf()))) * 100000 + size(walk(3, (n + 7) % 65537, Tree/node(Tree/leaf(), 500, Tree/leaf())))",
    );
    let (lowered, module) = modules(&source);
    let mut kept = lowered;
    curios_cps::optimize(&mut kept);
    for n in ["0", "5", "300"] {
        assert_eq!(run(&module, n), run(&kept, n), "{n}");
    }
}

/// A map of maps holding one inner map under two keys: an insert into the inner map read out under one key copies, and the map under the other key is the size it was, as when every insert copies.
#[test]
fn a_map_two_keys_of_a_map_hold_is_inserted_into_by_a_copy() {
    let source = program(
        &[
            TWO_WALKS,
            r#"
            let nest(n: Nat) -> Nat =
                let inner = walk(n, 0, (n + 1) % 65537, Map/empty());
                let outer: Map(Nat, Map(Nat, Nat)) =
                    Map/insert(Map/insert(Map/empty(), 1, inner), 2, inner);
                let grown =
                    match Map/try_get(outer, 1)
                    | some(m) => Map/len(Map/insert(m, 70000, 1))
                    | none() => 0
                    end;
                let kept = match Map/try_get(outer, 2) | some(m) => Map/len(m) | none() => 0 end;
                grown * 100000 + kept;
            "#,
        ]
        .concat(),
        "nest(n)",
    );
    let (lowered, module) = modules(&source);
    let mut kept = lowered;
    curios_cps::optimize(&mut kept);
    for n in ["0", "5", "300"] {
        assert_eq!(run(&module, n), run(&kept, n), "{n}");
    }
}

/// A map whose payloads are constant trees, each read out and inserted into on every round of a loop: a payload is a tree a reader takes apart, so its slot is no payload's and the tree read out is what the map still holds, a constant besides, and every round's size is the one it is when every insert copies.
#[test]
fn a_constant_tree_a_map_holds_is_inserted_into_by_a_copy() {
    let source = program(
        &[
            SEARCH_TREE,
            r#"
            let visit(n: Nat, m: Map(Nat, Tree), acc: Nat) -> Nat =
                match n
                | 0 => acc
                | k + 1 =>
                    match Map/try_get(m, k % 3)
                    | none() => acc
                    | some(t) => visit(k, m, acc + size(insert(t, 70000 + k)))
                    end
                end;
            "#,
        ]
        .concat(),
        "visit(n, Map/of_list([(0, Tree/node(Tree/leaf(), 5, Tree/leaf())), (1, Tree/node(Tree/leaf(), 7, Tree/leaf())), (2, Tree/node(Tree/leaf(), 9, Tree/leaf()))]), 0)",
    );
    let (lowered, module) = modules(&source);
    let mut kept = lowered;
    curios_cps::optimize(&mut kept);
    for n in ["0", "7", "300"] {
        assert_eq!(run(&module, n), run(&kept, n), "{n}");
    }
}

/// A search tree's insert that hands back the node it was handed where the key is found: the node it may return holds the child it hands on, but on another path, so where the insert of a child is handed that child, the node is rebuilt around what comes back and holds the child no more, and the child is the insert's alone. Each level that descends rebuilds its node, and two walks over the same keys, the second finding each, answer as the same program with nothing rebuilt does.
#[test]
fn an_insert_returning_the_node_it_was_handed_rebuilds_where_it_descends() {
    let tree = r#"
        induct Tree: Type
        | leaf()
        | node(left: Tree, key: Nat, right: Tree)
        end
        let insert(t: Tree, k: Nat) -> Tree =
            match t
            | leaf() => Tree/node(Tree/leaf(), k, Tree/leaf())
            | node(l, x, r) =>
                match
                | k < x => Tree/node(insert(l, k), x, r)
                | x < k => Tree/node(l, x, insert(r, k))
                | _ => t
                end
            end;
        let size(t: Tree) -> Nat = match t | leaf() => 0 | node(l, _, r) => size(l) + 1 + size(r) end;
        let walk(n: Nat, x: Nat, t: Tree) -> Tree =
            match n | 0 => t | k + 1 => let y = 75 * x % 65537; walk(k, y, insert(t, y)) end;
    "#;
    let (lowered, module) = modules(&program(
        tree,
        "size(walk(n, (n + 1) % 65537, walk(n, (n + 1) % 65537, Tree/leaf())))",
    ));
    assert!(rebuilds_of(&module, "/Tree") > 0, "{module}");
    let mut kept = lowered;
    curios_cps::optimize(&mut kept);
    for n in ["0", "1", "7", "300"] {
        assert_eq!(run(&module, n), run(&kept, n), "{n}");
    }
}

/// A probe through an accessor before each replace: `get` hands back a field of the twigs it is handed, so a call of it reads them as a row read in the probe's own body would, its parameter borrowing them, and every level the replace takes apart is rebuilt as where the probe reads the twigs itself. Mutation-checked: with an accessor's returned field making its parameter own, the probe consumes the trie and nothing is rebuilt.
#[test]
fn a_probe_through_an_accessor_leaves_what_it_reads_to_be_rebuilt() {
    let (_, module) = modules(&program(ACCESSED_TRIE, ACCESSED_TRIE_SUM));
    let inline = ACCESSED_TRIE.replace(
        "near(get(tw, k % 8), k / 8)",
        "near(match k % 8 | 0 => tw.c0 | 1 => tw.c1 | 2 => tw.c2 | 3 => tw.c3 | 4 => tw.c4 | 5 => tw.c5 | 6 => tw.c6 | _ => tw.c7 end, k / 8)",
    );
    let (_, read_inline) = modules(&program(&inline, ACCESSED_TRIE_SUM));
    assert!(rebuilds_of(&module, "/N") > 0, "{module}");
    for row in ["/N", "/T"] {
        assert_eq!(
            rebuilds_of(&module, row),
            rebuilds_of(&read_inline, row),
            "{row}"
        );
    }
}

/// A loop that reads a list nothing else holds at an index and updates it there writes every update into the list it is handed, and answers as copying each does. Mutation-checked: with a list's read a consumption, the read and the update consume it twice and nothing is written in place.
#[test]
fn a_loop_updating_a_list_nothing_else_holds_writes_into_it() {
    let (lowered, module) = modules(&program(BUMPS, BUMPS_SUM));
    assert!(overwrites(&module) > 0, "{module}");
    let mut kept = lowered;
    curios_cps::optimize(&mut kept);
    assert_eq!(overwrites(&kept), 0);
    assert_eq!(run(&module, "300"), run(&kept, "300"));
}

/// An insert into a trie holding its twigs in a list rebuilds every level it takes apart and writes the list at each into itself: the twig read at the key is held by nothing else once the list is updated there, and what a constant list stores elsewhere in the program says nothing of what this list holds. Mutation-checked: with every list's elements followed as one, the constants the library's lists hold reach the twig read and nothing is rebuilt.
#[test]
fn an_insert_into_a_trie_of_listed_twigs_writes_every_level_in_place() {
    let (lowered, module) = modules(&program(LIST_TRIE, LIST_TRIE_SUM));
    assert!(rebuilds_of(&module, "/N") > 0, "{module}");
    assert!(overwrites(&module) > 0, "{module}");
    let mut kept = lowered;
    curios_cps::optimize(&mut kept);
    assert_eq!(run(&module, "300"), run(&kept, "300"));
}

/// A node holding its twigs in a structure the program takes apart and puts back but never builds: every construction of the structure is rewritten into a rebuild, which leaves the flow none to start from, and the program compiles and answers as the same program building one twig structure does.
#[test]
fn a_structure_the_program_only_rebuilds_compiles_and_answers() {
    let twigs = r#"
        struct T(X: Type): Type { c0: X, c1: X }
        induct N: Type
        | empty()
        | leaf(key: Nat, value: Nat)
        | branch(twigs: T(N))
        end
        let get2(@X: Type, t: T(X), d: Nat) -> X = match d | 0 => t.c0 | _ => t.c1 end;
        let set2(@X: Type, t: T(X), d: Nat, x: X) -> T(X) =
            match d | 0 => T { ..t, c0 = x } | _ => T { ..t, c1 = x } end;
        let take2(t: T(N), d: Nat) -> {N, T(N)} =
            match d | 0 => (t.c0, T { ..t, c0 = N/empty() }) | _ => (t.c1, T { ..t, c1 = N/empty() }) end;
        let replace(t: N, k: Nat, v: Nat) -> N =
            match t
            | empty() => t
            | leaf(k0, _) => N/leaf(k0, v)
            | branch(tw) =>
                let d = k % 2;
                let (s, rest) = take2(tw, d);
                N/branch(set2(rest, d, replace(s, k / 2, v)))
            end;
        let first(k: Nat, v: Nat) -> N = N/leaf(k, v);
        let insert(m: Option(N), k: Nat, v: Nat) -> Option(N) =
            match m | none() => Option/some(first(k, v)) | some(t) => Option/some(replace(t, k, v)) end;
        let fold_twigs(tw: T(N), i: Nat, acc: Nat) -> Nat =
            match i < 2 | false => acc | true => fold_twigs(tw, i + 1, fold(get2(tw, i), acc)) end
        and fold(t: N, acc: Nat) -> Nat =
            match t | empty() => acc | leaf(_, v) => acc + v | branch(tw) => fold_twigs(tw, 0, acc) end;
        let walk(n: Nat, m: Option(N)) -> Option(N) = match n | 0 => m | j + 1 => walk(j, insert(m, j, j + 1)) end;
    "#;
    let result = "match walk(n, Option/none()) | none() => 0 | some(t) => fold(t, 0) end";
    let (_, module) = modules(&program(twigs, result));
    let built = twigs.replace(
        "N/leaf(k, v);",
        "N/branch(T { c0 = N/leaf(k, v), c1 = N/empty() });",
    );
    let (_, building) = modules(&program(&built, result));
    for n in ["0", "1", "7"] {
        assert_eq!(run(&module, n), run(&building, n), "{n}");
    }
}

/// Every program of the corpus, and the shapes this suite rebuilds, prints one answer and exits alike whether its module rebuilds in place or not.
#[test]
fn a_program_answers_alike_rebuilding_in_place_or_not() {
    // Each with an argument it runs in a moment at: `trees` takes a depth.
    let corpus = [
        (
            include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../programs/chain/chain.crs"
            ))
            .to_owned(),
            "300",
        ),
        (
            include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../programs/churn/churn.crs"
            ))
            .to_owned(),
            "300",
        ),
        (
            include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../programs/lcg/lcg.crs"
            ))
            .to_owned(),
            "300",
        ),
        (
            include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../programs/spines/spines.crs"
            ))
            .to_owned(),
            "300",
        ),
        (
            include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../programs/trees/trees.crs"
            ))
            .to_owned(),
            "10",
        ),
        (
            include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../programs/walk/walk.crs"
            ))
            .to_owned(),
            "300",
        ),
        (
            include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../programs/dependent_vectors.crs"
            ))
            .to_owned(),
            "300",
        ),
        (
            include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../programs/monad_io.crs"
            ))
            .to_owned(),
            "300",
        ),
        (
            include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../programs/monad_result.crs"
            ))
            .to_owned(),
            "300",
        ),
        (
            include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../programs/monad_suspend.crs"
            ))
            .to_owned(),
            "300",
        ),
        (
            include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../programs/parse_bindless.crs"
            ))
            .to_owned(),
            "300",
        ),
        (
            include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../programs/parse_digits.crs"
            ))
            .to_owned(),
            "300",
        ),
        (
            include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../programs/rng_state.crs"
            ))
            .to_owned(),
            "300",
        ),
        (
            include_str!(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/../programs/state_monad.crs"
            ))
            .to_owned(),
            "300",
        ),
    ];
    let heap = program(
        r#"
        induct Heap: Type
        | empty()
        | node(rank: Nat, key: Nat, left: Heap, right: Heap)
        end
        let rank(h: Heap) -> Nat = match h | empty() => 0 | node(r, _, _, _) => r end;
        let make(k: Nat, a: Heap, b: Heap) -> Heap =
            match
            | rank(b) <= rank(a) => Heap/node(rank(b) + 1, k, a, b)
            | _ => Heap/node(rank(a) + 1, k, b, a)
            end;
        let merge(a: Heap, b: Heap) -> Heap =
            match a
            | empty() => b
            | node(_, ka, la, ra) =>
                match b
                | empty() => Heap/node(rank(a), ka, la, ra)
                | node(_, kb, lb, rb) =>
                    match
                    | ka <= kb => make(ka, la, merge(ra, Heap/node(rank(b), kb, lb, rb)))
                    | _ => make(kb, lb, merge(Heap/node(rank(a), ka, la, ra), rb))
                    end
                end
            end;
        let least(h: Heap) -> Nat = match h | empty() => 0 | node(_, k, _, _) => k end;
        let walk(n: Nat, x: Nat, h: Heap) -> Heap =
            match n
            | 0 => h
            | k + 1 =>
                let y = 75 * x % 65537;
                walk(k, y, merge(h, Heap/node(1, y, Heap/empty(), Heap/empty())))
            end;
        "#,
        "least(walk(n, (n + 1) % 65537, Heap/empty()))",
    );
    let spines = program(
        SPINES,
        "Map/fold(walk(n, (n + 1) % 65537, Map/empty()), 0, (_, v, acc) => (acc + v) % 1000003)",
    );
    let tree = program(SEARCH_TREE, "size(walk(n, (n + 1) % 65537, Tree/leaf()))");
    let removals = program(
        REMOVALS,
        "Map/fold(drain(n, fill(n, Map/empty())), 0, (_, v, acc) => (acc + v) % 1000003)",
    );
    let two_walks = program(TWO_WALKS, TWO_WALKS_SUM);
    let rope = program(ROPE, ROPE_FLIPPED_TWICE);
    let union = program(TWO_WALKS, TWO_WALKS_UNION);
    let filtered = program(TWO_WALKS, WALK_FILTERED);
    let pairs = program(TWO_WALKS, PAIRS_AS_A_MAP);
    let accessed = program(ACCESSED_TRIE, ACCESSED_TRIE_SUM);
    let bumps = program(BUMPS, BUMPS_SUM);
    let kept_lists = program(KEPT_LISTS, "kept(n)");
    let list_trie = program(LIST_TRIE, LIST_TRIE_SUM);
    for (source, n) in corpus.into_iter().chain([
        (accessed, "300"),
        (bumps, "300"),
        (kept_lists, "300"),
        (list_trie, "300"),
        (heap, "300"),
        (spines, "300"),
        (removals, "300"),
        (tree, "300"),
        (two_walks, "300"),
        (rope, "300"),
        (union, "300"),
        (filtered, "300"),
        (pairs, "300"),
    ]) {
        let (lowered, rebuilt) = modules(&source);
        let mut kept = lowered;
        curios_cps::optimize(&mut kept);
        assert_eq!(run(&rebuilt, n), run(&kept, n), "{source}");
    }
}
