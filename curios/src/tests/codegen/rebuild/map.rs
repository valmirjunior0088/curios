//! `/std/Map/insert` and `remove` rebuilding their node at every level, and a map something else holds — a reader, a cell, a loop, two keys — answering as if copied.

use {
    super::test_support::*,
    curios_common::fill,
    curios_cps::{Atom, Intrinsic, Module, Node, ValueExpr, ValueId},
    std::collections::BTreeSet,
};

const NODE: &str = "/std/Map/internal/Node/Node";

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
        "Map/length(walk(n, (n + 1) % 65537, Map/empty()))",
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
            "Map/length(walk(n, (n + 1) % 65537, Map/empty()))",
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
                Option/unwrap_or(Map/try_get(a, 1), 0) * 1000 + Map/length(b2)
            | j + 1 => looped(a, b, j)
            end;
        let called(a: Map(Nat, Nat), b: Map(Nat, Nat), k: Nat) -> Nat =
            match k
            | 0 =>
                let b2 = Map/insert(b, 1, 99);
                Option/unwrap_or(Map/try_get(a, 1), 0) * 1000 + Map/length(b2)
            | j + 1 => match called(a, b, j) | 0 => 1 | i + 1 => i + 1 end
            end;
        let outer(x: Map(Nat, Nat), n: Nat) -> Nat =
            let probe(t: Map(Nat, Nat), k: Nat) -> Nat =
                match k
                | 0 =>
                    let x2 = Map/insert(x, 1, 99);
                    Map/length(x2) * 1000 + Option/unwrap_or(Map/try_get(t, 1), 0)
                | j + 1 => probe(t, j)
                end;
            probe(x, n);
    "#;
    for start in [
        "(() => let m = fill(n % 50 + 3, Map/empty()); looped(m, m, n % 7))()",
        "(() => let m = fill(n % 50 + 3, Map/empty()); let p = fill(n % 50 + 4, Map/empty()); called(m, m, n % 7) + called(p, p, n % 7) * 100000)()",
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
                Map/length(Map/insert(m, 70000, 1)) + Map/length(m);
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
        "Map/length(drain(n, fill(n, Map/empty())))",
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
            "Map/length(walk(n, (n + 1) % 65537, Map/empty()))",
            "/std/Map/internal/Node/insert",
        ),
        (
            REMOVALS,
            "Map/length(drain(n, fill(n, Map/empty())))",
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
            | 0 => acc + Map/length(m)
            | k + 1 => let m2 = Map/insert(m, k % 97, k); walk(k, m2, acc + Map/length(m))
            end;
        "#,
        // Two maps sharing a subtree, the older read again.
        r#"
        let walk(n: Nat, m: Map(Nat, Nat), acc: Nat) -> Nat =
            match n
            | 0 => acc + Map/length(m)
            | k + 1 =>
                let m1 = Map/insert(m, k % 89, k);
                let m2 = Map/insert(m1, k % 97, k);
                walk(k, m2, acc + Map/length(m1))
            end;
        "#,
        // Held by a list.
        r#"
        let walk(n: Nat, ms: List(Map(Nat, Nat)), acc: Nat) -> Nat =
            match List/try_get(ms, 0)
            | none() => acc
            | some(m) =>
                match n
                | 0 => acc + Map/length(m)
                | k + 1 => walk(k, [Map/insert(m, k % 97, k), m], acc + Map/length(m))
                end
            end;
        "#,
        // Captured by a closure called after the insert.
        r#"
        let walk(n: Nat, m: Map(Nat, Nat), acc: Nat) -> Nat =
            match n
            | 0 => acc + Map/length(m)
            | k + 1 =>
                let size = () => Map/length(m);
                let m2 = Map/insert(m, k % 97, k);
                walk(k, m2, acc + size())
            end;
        "#,
        // Constant data inserted into afresh on every round.
        r#"
        let base = Map/insert(Map/insert(Map/empty(), 1, 1), 2, 2);
        let walk(n: Nat, m: Map(Nat, Nat), acc: Nat) -> Nat =
            match n
            | 0 => acc + Map/length(m)
            | k + 1 => walk(k, m, acc + Map/length(Map/insert(base, k % 97, k)))
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

/// A map a box holds: taken out of it, inserted into and taken out again, the box still hands back the map it was made with, as it does when the insert copies.
#[test]
fn a_map_a_box_holds_answers_as_if_copied() {
    let source = r#"
        use /std/{Str, Nat, Map, List, Option, Result, Try, Io, Proc, Fmt, Box};
        let execute() -> Try(Io, Str, {}) =
            let arguments = Proc/args()!;
            let bytes = Option/to_result(List/try_get(arguments, 1), "no input")!;
            let s = Result/map_failure(Str/of_bytes(bytes), (_) => "invalid utf-8")!;
            let n = Option/to_result(Nat/of_str(Str/trim(s)), "bad input")!;
            let held = Box/new(Map/insert(Map/insert(Map/empty(), n, 1), n + 1, 2))!;
            let first = Box/held(held)!;
            let grown = Map/length(Map/insert(first, n + 2, 3));
            let again = Box/held(held)!;
            let kept = Map/length(again);
            let _ = Fmt/print("%\n")(grown * 10 + kept)!;
            Try/pure(());
        execute()
    "#;
    let (lowered, module) = modules(source);
    let mut kept = lowered;
    curios_cps::optimize(&mut kept);
    assert_eq!(run(&module, "5"), run(&kept, "5"));
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
                    | some(m) => Map/length(Map/insert(m, 70000, 1))
                    | none() => 0
                    end;
                let kept = match Map/try_get(outer, 2) | some(m) => Map/length(m) | none() => 0 end;
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
