//! What looks fresh and is shared rebuilding nothing — a constant, a tree shared in one place, a branch a loop reads or hands on — the links a loop takes apart rebuilt, and every program answering alike rebuilding or not.

use {
    super::test_support::*,
    curios_cps::{Intrinsic, Module, Node},
    curios_native::test_support::*,
};

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

/// A list of four counters, each round of a loop adding to the one its round names: read at an index, then updated there.
const BUMPS: &str = r#"
let bump(k: Nat, l: List(Nat)) -> List(Nat) =
    match k
    | 0 => l
    | j + 1 =>
        let i = j % 4;
        bump(j, match i < List/length(l) | true => List/set(l, i, List/get(l, i) + j) | false => l end)
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
        match d < List/length(tw)
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
