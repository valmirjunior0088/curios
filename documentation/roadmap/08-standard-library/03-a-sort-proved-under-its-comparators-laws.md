# A sort proved under its comparator's laws

Working specification for `/std/List`'s sort as a merge sort with its theorem, and for what the theorem is stated in: a permutation, an order decided over a list, and a comparator's laws named once. It replaces the insertion sort, which stops at five thousand elements. [A map in its keys' order](04-a-map-in-its-keys-order.md) is the other half of what this file once reserved.

It waits on no other spec.

## What this builds on

- **A `List` is a rope** (`curios-emit/src/types.rs`): an append is one node, a peel of the head and the tail is constant once the list is filled, and the first read of a list an append extended fills it whole. A list built whole and then read costs its length; an append and a read in alternation cost its square.
- **A call that is no tail call takes a frame**, so a recursion under a cons is as deep as its list. [A module's stack](../../../curios-runtime/README.md#a-module-runs-on-a-stack-of-its-own) holds that depth, and a frame an element is still what such a recursion costs.
- **Totality grades a recursion over two lists** where each call peels one and keeps the other ([Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md)), so a merge is total as written and stands in a type.
- **Conversion decides the free monoid on a list's elements** ([The carriers' algebra stays in conversion](../../design/arithmetic/the-carriers-algebra-stays-in-conversion.md)): a concatenation's associativity and its two identities, a cons through a spread, and the length of a concatenation each hold by `Eq/refl()`.
- **A match on an expression refines it in the arm**, in the goal and in every hypothesis that mentions it ([A motive is a term](../../design/theory/a-motive-is-a-term-not-a-grammar.md)), which is how a `Holds` over a decision is taken apart and put together.
- **A decided proposition is discharged by reduction** ([A bound is stated in a decided proposition and discharged by reduction](../../design/arithmetic/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md)), so a fact about a closed list is `True/qed()`.

## The gap

- **No comparator in `/std` has a stated law, so the sort's theorem is read at no type's own order.** That `Nat/ord` answers `gt` one way exactly where it answers `lt` the other is provable and unstated, as it is of `Bytes/ord` and `Str/ord`, and [the numeric laws](../04-arithmetic/03-numeric-laws.md) state it at `Nat` and `Int`.

## Prior art

- **A merge sort is written structurally, as a stack of runs.** Rocq's `Mergesort` pushes each element onto a stack of optional runs, merging two runs of one size as a binary counter carries, and every function in it is a structural `Fixpoint` ([`Sorting/Mergesort.v`](https://github.com/rocq-prover/stdlib/blob/master/theories/Sorting/Mergesort.v)). Lean 4 halves the list instead and recurses on its length, a well-founded recursion its runtime replaces with a tail-recursive implementation proved equal ([`Init/Data/List/Sort/Basic.lean`](https://github.com/leanprover/lean4/blob/master/src/Init/Data/List/Sort/Basic.lean)). Cohen and Sakaguchi give the reasons for the first shape: a structurally recursive merge sort runs in the kernel, and a merge that is a tail call "does not use up stack space" under call-by-value ([A bargain for mergesorts](https://arxiv.org/abs/2403.08173), ICFP 2025).
- **The permutation needs no law, and order over adjacent pairs needs a total comparator alone.** Rocq's functor takes one law, `leb_total`, proves the output locally sorted from it, and asks transitivity only for the reading over every pair ([`Sorting/Sorted.v`](https://github.com/rocq-prover/stdlib/blob/master/theories/Sorting/Sorted.v)). Lean proves `mergeSort_perm` with no hypothesis, and its order over every pair under transitivity and totality ([`Init/Data/List/Sort/Lemmas.lean`](https://github.com/leanprover/lean4/blob/master/src/Init/Data/List/Sort/Lemmas.lean)). Both state a permutation as the inductive relation of a skip, a swap and a chain ([`Sorting/Permutation.v`](https://github.com/rocq-prover/stdlib/blob/master/theories/Sorting/Permutation.v)).
- **Order and stability are one theorem.** Cohen and Sakaguchi's Theorem 3.11: for a total `≤₁` and any `≤₂`, sorting by `≤₁` a list in order by `≤₂` at every pair answers a list in order, at adjacent pairs, by `≤₁` and then `≤₂`. Order alone is its instance at the relation that always holds, and the usual statement, that equivalent elements keep their order, is a corollary they derive from it. Lean states stability as a sorted sublist of the input staying a sublist of the output, and Isabelle's `sort_key` as a filter by one key that the sort leaves as it was; Sternagel's mechanized proof of GHC's merge sort proves it stable as well ([Efficient Mergesort](https://www.isa-afp.org/entries/Efficient-Mergesort.html)).
- **A comparator's laws stand beside it.** Lean's `OrientedCmp`, that swapping the arguments swaps the answer, and `TransCmp` are classes over a bare comparator, apart from `Ord` ([`Init/Data/Order/Ord.lean`](https://github.com/leanprover/lean4/blob/master/src/Init/Data/Order/Ord.lean)). Haskell documents `Ord`'s laws and checks none ([`Data.Ord`](https://hackage.haskell.org/package/base/docs/Data-Ord.html)), and Rust's `sort_by` answers an unspecified order, or panics, under a comparator that is no total order, keeping every element ([`slice::sort_by`](https://doc.rust-lang.org/std/primitive.slice.html#method.sort_by)).

## Decisions

- **There is one sort.** The merge sort is `sort_by`, and `sort` is `sort_by` at `Ord/ord`; the insertion sort is deleted, and no proved sort stands beside a fast one.
- **It is a bottom-up merge sort written structurally.** A stack of optional runs, each element pushed as a run of one and equal runs merged as they meet; the older run merges on the left and a tie takes from the left, which is what keeps tied elements in their order. Every function descends on a list.
- **The merge is a tail call into an accumulator.** It appends to what it has built and never reads it, so a run is built whole before the next merge reads it, each merge costs its length, and its depth is constant. The proofs read it through one equation, that merging into an accumulator is the accumulator followed by the merge into none.
- **The theorem is two statements.** The answer is a permutation of the input, under no law. And over an input in order by a relation `second` at every pair, the answer is in order by the comparator and then by `second`, at adjacent pairs, under the comparator's flip. Order alone is the second at the relation that always holds, by conversion, and two sorts in a row order by both keys.
- **A list's order is a decision over a `Bool` relation**, `Holds(ordered(l, le))`: a closed list discharges it by reduction, and the same function decides it of a list met at run time. A comparator becomes a relation through `Ord/le` and `Ord/then`.
- **A permutation is an inductive relation.** It has no decided form: `Eql` owes no proof that its answer is equality, so counting occurrences states nothing.
- **A law is a hypothesis, named once.** `Ord/Flips(ord)` and `Ord/Transitive(le)` are propositions a theorem takes, and an order's own proof of one stands in a module beside it, `Nat/ord/flips`. `Ord` stays a concept of one function, as [the numeric laws](../04-arithmetic/03-numeric-laws.md) decide.
- **Membership is an inductive proposition, and a case split on it takes two continuations**, `/std` having no disjunction.

## Stages

Each lands alone and is one commit. What the theorem is stated in has landed: `/std/List/Permutation` and `/std/List/Member`, `/std/List/ordered` and `/std/List/Pairwise`, and `/std/Ord`'s `Flips`, `Transitive`, `le` and `then`. The sort has landed with its theorem: `/std/List/sort_by` is the merge sort, with `permuted`, `ordered` and `sorted`, and `/std/List/sort` reads the three at `Ord/ord`.

**Stage 4 — the laws the sort is used at.** `Nat/ord/flips`, taken from [the numeric laws](../04-arithmetic/03-numeric-laws.md) where they have landed and written here otherwise; `Bytes/ord/flips` and `Str/ord/flips`; and `sort`'s three theorems.

Check: the `aggregates` unit states a list of `Nat` and a list of `Str` sorted, and two sorts in a row in order by both keys through `Pairwise/of_ordered`.

## Verification

- Every declaration elaborates over symbolic operands and is certified, which `cargo xtask clippy` holds for all of `/std`.
- The corpus rows above are counted: an answer, or a refusal.
- **Timed, and not yet taken.** The machine the campaign ran on was shared, so no figure is stated. The protocol: a program builds `n` naturals into a list by appending, in one of three orders — ascending from zero, descending to zero, and the sequence `s ↦ (1103515245 · s + 12345) mod 2³¹` from 42 with each term reduced modulo 1,000,003 — sorts it and prints its length and first element, beside a control that prints the same of the unsorted list; each is run through `cargo run --release --package curios -- run -` at `n` of 100,000 and 1,000,000, the sort and its control interleaved over three readings, and reported as a median with its span.

## Rejected

- **Proving the insertion sort as it is written**, the fold a proof follows most easily. Both statements check over it in about a hundred lines, and it remains quadratic on a rope and as deep as its input.
- **A proved sort beside a fast one, held equal**, as Lean's runtime replacement holds its two. Nothing here swaps a definition for another at run time, so the proved function is the one that runs, and it is written to run well.
- **Halving the list**, over `List/balanced` or by well-founded recursion on the length. The first descends on a fuel whose sufficiency the proof would owe, and reads its halves as windows whose union is a law of positions; the second does not compute in a type as readily as a structural descent, where a closed sort holds by `Eq/refl()`.
- **A merge that conses onto its recursive call.** It is the shape a proof follows with no accumulator equation, and it is as deep as its runs.
- **Detecting runs already in order**, as GHC's sort does. It changes the cost on nearly sorted input and nothing else, a measurement before it is a design.
- **Antisymmetry as the hypothesis.** It is false of a comparator that reads one key of a pair, the use stability exists for, and no statement here needs it.
- **Laws as fields of `Ord`.** A derived witness would owe proofs the compiler does not write, and `Flt`'s order, which leads with a comparison the logic says nothing of, could not be a witness.
- **An inductive proposition for a list in order.** It decides nothing of a list met at run time and would need its decision procedure beside it.
- **Stability as a sublist kept, or as a filter commuting with the sort.** The first needs a sublist relation and the second an equality decided on keys; the statement over a second relation needs neither and is what a second sort reads.

## Left to other specifications

- **`ord`'s other laws at `Nat` and `Int`**, and transitivity among them: [the numeric laws](../04-arithmetic/03-numeric-laws.md).
- **A map that enumerates in its keys' order, and an order's laws read off a key's encoding**: [a map in its keys' order](04-a-map-in-its-keys-order.md).

## Completion and retirement

- `sort_by` and `sort` are the merge sort, and their theorems, `Permutation`, `Member`, `ordered`, `Pairwise` and the four names in `/std/Ord` exist as specified.
- Before this specification is deleted, each contract is its declaration's documentation and signature, the decisions and what they reject are in [`curios-std`'s README](../../../curios-std/README.md), what stayed unmeasured is a finding with the protocol above, the roadmap entry is a checked summary, and no reference to this filename remains.
