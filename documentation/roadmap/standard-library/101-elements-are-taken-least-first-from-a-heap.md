# Elements are taken least first from a heap

**Adopted by the user on 2026-11-22; its next step is stage 1, a ready delivery.** A scheduler, a shortest-path search or a merge of sorted streams takes the least element next, and `/std` answers that only through `Map/least`, which holds one entry a key, so equal priorities need a key made distinct by hand. `/std/Heap` is a persistent min-heap ordered by its elements' `Ord`, as Rust's `BinaryHeap`, Python's `heapq`, Java's `PriorityQueue` and Go's `container/heap` each ship one.

## Goal

- `Heap(A: Type, use Ord(A))`, Okasaki's leftist heap: a node holds an element, its rank and two children, the left child's rank at least the right's, so the right spine of `n` elements is at most `log₂(n + 1)` long.
- `merge`, which walks the two right spines, keeps the smaller root and merges into its right child, a node's children swapped where their ranks ask; `insert`, `least`, `without_least` and `of_list` built on it, the last merging in pairwise rounds; and a sorted drain.
- Its theorems: its size, what it holds as a permutation of what was put in, its least element at most every other, and its drain sorted, each taking no hypothesis.

## Settled

- **A heap of its own rather than a multiset over `Map`** (decided by the user on 2026-11-22): it needs `Ord` alone where `Map` needs a `Key`, and equal priorities stand side by side.
- **The order is in the type**, as a container shaped by a dictionary names it ([Concepts resolve with global coherence](../../design/surface/concepts-resolve-with-global-coherence.md)), so a heap built under one order is read under it, as `Map(K, use Key(K), V)` is under its `Key`.
- **Leftist, persistent and worst-case**: each operation is O(log n) in the worst case, so persistence breaks no amortized bound, as it does a pairing heap's.
- **Equal priorities come out in no promised order**; a program wanting stability pairs each priority with a counter.
- **The leftist invariant is held by construction**: a node carries a hidden proof that its ranks are in order, discharged where `make` builds it from the comparison it made, as `Json/Number` carries its `canonical`.

**Rejected.** A multiset over `Map`, `Map(P, List(V))`: a priority needs a `Key`, and ties keep insertion order where nothing asks for it. A pairing heap: amortized bounds persistence breaks. A concept of the heap's own carrying its order's laws: [Equality and order carry their laws](100-equality-and-order-carry-their-laws.md) puts them in `Ord`.

## Stages

**1. The heap.** `/std/Heap`, its operations and its invariant held by construction, with its tests, a shortest-path search among them. Check: the tests pass, a node whose ranks are out of order is refused where it is built, and the drain sorts the corpus's inputs.

**2. Its theorems**, once [Equality and order carry their laws](100-equality-and-order-carry-their-laws.md)'s stage 3 gives `Ord` its laws: size, permutation, least and sorted drain. Check: each stated over every witness of `Ord`, with no hypothesis.

## Verification and retirement

Each stage's check, then `cargo xtask workspace lint`, `cargo xtask jig lint`, the suite and `cargo xtask docs build`. Once stage 2 lands, this file is deleted.
