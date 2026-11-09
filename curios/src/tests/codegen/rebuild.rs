//! Rebuilding in place: `/std/Map/insert` rebuilding its node at every level, what looks fresh and is shared rebuilding nothing, and every program printing one answer whether it rebuilds or not.

use {
    curios_common::fill,
    curios_cps::{Atom, Intrinsic, Module, Node, ValueExpr, ValueId},
    curios_module::RootSource,
    curios_native::{Compiled, ForeignBindings, MockHost},
    curios_pipeline::{DEFAULT_BUDGET, Stage, compile_with_prelude},
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
    let (wasm, foreigns) = curios_emit::into_wasm(module);
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
        | k + 1; _ =>
            let y = 75 * x % 65537;
            walk(k, y, Map/insert(m, y, k))
        end;
"#;

/// A map filled with `n` keys, then drained of every other one and of keys it never held.
const REMOVALS: &str = r#"
    let fill(n: Nat, m: Map(Nat, Nat)) -> Map(Nat, Nat) =
        match n | 0 => m | k + 1; _ => fill(k, Map/insert(m, k * 7 % 1009, k)) end;
    let drain(n: Nat, m: Map(Nat, Nat)) -> Map(Nat, Nat) =
        match n | 0 => m | k + 1; _ => drain(k, Map/remove(m, k * 14 % 1009)) end;
"#;

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

/// What looks fresh and is shared: a map read after its insert, a map whose successor's predecessor is read again, a map a list holds, a map a closure captures, a map an effect description holds, and a map that is constant data. None of them is rebuilt.
#[test]
fn a_map_something_else_still_holds_is_never_rebuilt() {
    let walks = [
        // Read after its insert.
        r#"
        let walk(n: Nat, m: Map(Nat, Nat), acc: Nat) -> Nat =
            match n
            | 0 => acc + Map/len(m)
            | k + 1; _ => let m2 = Map/insert(m, k % 97, k); walk(k, m2, acc + Map/len(m))
            end;
        "#,
        // Two maps sharing a subtree, the older read again.
        r#"
        let walk(n: Nat, m: Map(Nat, Nat), acc: Nat) -> Nat =
            match n
            | 0 => acc + Map/len(m)
            | k + 1; _ =>
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
                | k + 1; _ => walk(k, [Map/insert(m, k % 97, k), m], acc + Map/len(m))
                end
            end;
        "#,
        // Captured by a closure called after the insert.
        r#"
        let walk(n: Nat, m: Map(Nat, Nat), acc: Nat) -> Nat =
            match n
            | 0 => acc + Map/len(m)
            | k + 1; _ =>
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
            | k + 1; _ => walk(k, m, acc + Map/len(Map/insert(base, k % 97, k)))
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
        let (_, module) = modules(&program(walk, start));
        assert_eq!(rebuilds_of(&module, NODE), 0, "{walk}\n{module}");
    }
}

/// A map a cell holds: taken out of it, inserted into and taken out again, the cell still hands back the map it was filled with, so the insert copies.
#[test]
fn a_map_a_cell_holds_is_never_rebuilt() {
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
    assert_eq!(rebuilds_of(&module, NODE), 0, "{module}");
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
                choose
                | k < x => Tree/node(insert(l, k), x, r)
                | x < k => Tree/node(l, x, insert(r, k))
                | _ => Tree/node(l, x, r)
                end
            end;
        let size(t: Tree) -> Nat = match t | leaf() => 0 | node(l, _, r) => size(l) + 1 + size(r) end;
        let walk(n: Nat, x: Nat, t: Tree) -> Tree =
            match n | 0 => t | k + 1; _ => let y = 75 * x % 65537; walk(k, y, insert(t, y)) end;
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

/// A search tree's insert that hands back the node it was handed where the key is found: the child it hands on is held by a node it may return, so no unique value enters the loop and nothing is rebuilt.
#[test]
fn an_insert_returning_the_node_it_was_handed_rebuilds_nothing() {
    let tree = r#"
        induct Tree: Type
        | leaf()
        | node(left: Tree, key: Nat, right: Tree)
        end
        let insert(t: Tree, k: Nat) -> Tree =
            match t
            | leaf() => Tree/node(Tree/leaf(), k, Tree/leaf())
            | node(l, x, r) =>
                choose
                | k < x => Tree/node(insert(l, k), x, r)
                | x < k => Tree/node(l, x, insert(r, k))
                | _ => t
                end
            end;
        let size(t: Tree) -> Nat = match t | leaf() => 0 | node(l, _, r) => size(l) + 1 + size(r) end;
        let walk(n: Nat, x: Nat, t: Tree) -> Tree =
            match n | 0 => t | k + 1; _ => let y = 75 * x % 65537; walk(k, y, insert(t, y)) end;
    "#;
    let (_, module) = modules(&program(
        tree,
        "size(walk(n, (n + 1) % 65537, Tree/node(Tree/leaf(), n, Tree/leaf())))",
    ));
    assert_eq!(rebuilds_of(&module, "/Tree"), 0, "{module}");
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
            choose
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
                    choose
                    | ka <= kb => make(ka, la, merge(ra, Heap/node(rank(b), kb, lb, rb)))
                    | _ => make(kb, lb, merge(Heap/node(rank(a), ka, la, ra), rb))
                    end
                end
            end;
        let least(h: Heap) -> Nat = match h | empty() => 0 | node(_, k, _, _) => k end;
        let walk(n: Nat, x: Nat, h: Heap) -> Heap =
            match n
            | 0 => h
            | k + 1; _ =>
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
    let removals = program(
        REMOVALS,
        "Map/fold(drain(n, fill(n, Map/empty())), 0, (_, v, acc) => (acc + v) % 1000003)",
    );
    for (source, n) in corpus
        .into_iter()
        .chain([(heap, "300"), (spines, "300"), (removals, "300")])
    {
        let (lowered, rebuilt) = modules(&source);
        let mut kept = lowered;
        curios_cps::optimize(&mut kept);
        assert_eq!(run(&rebuilt, n), run(&kept, n), "{source}");
    }
}
