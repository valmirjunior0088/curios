# curios-anf

The Curios erased IR, in A-normal form: the flat, explicit, first-order stage between `curios-elab`'s type-directed erasure and the continuation IR of `curios-cps`, with the one-way door `lower_to_cps` between them. Types, proofs and erasable binders are gone by construction. How the module prints and how its identities are spelled is [A printer states each fact once, where it is bound](../documentation/design/tools/a-printer-states-each-fact-once-where-it-is-bound.md); the representation and its derived analyses belong to the crate rustdoc.

## Design

### The ANF optimizer is thin

**Decision.** The ANF optimizer runs exactly the transformations whose leverage is semantic — pruning, compile-time partial evaluation of closed terms and literal spines, reconstruction recovery, and the monoid worker/wrapper rebase — and nothing else. Folding, dead code, inlining, contification and specialization belong to `curios-cps`, after the lowering.

**Rationale.** ANF's leverage is what it still knows: hand CPS no work it can delete, run what compile time has decided, and re-base a recursion around a monoid, which would take a frame an element, onto a loop. A second local-rewrite engine would restate CPS's reductions over a second representation, and the two would drift.

### Shapes stay distinct

**Decision.** The erased alphabet keeps erased Core's semantic identities — distinct scalar shapes, schema-carrying products and variants, dedicated `Bool` and `Nat` switches, first-class folds. One shape's operations are never reused for another, and conversions between shapes are explicit operations.

**Rationale.** Every encoding decision — carriers, tag layouts, dispatch, loop synthesis — belongs to the lowering into CPS. Collapsing shapes early discards information the backend cannot recover, and an operation reused across shapes acquires a per-context meaning no node-local classification can read.

### An item imports what it reads of another's row

**Decision.** A row another declaration declares is one of a fragment's imports, as a value or a function it uses is: a structure's schema at its width, and a family at the count of constructors it declares, with each constructor the item names at its position, which is its discriminant. Erasure reads a mask where the declaring item holds it, and only the declaring item lists a row whole, as does every program, which has no import; linking maps an imported constructor by its position and asserts its row resolved. A module that reads no row, as an item's evaluation does, holds each row as its fragments do, an import at what they state of it ([`Resolution::imports_rows`](src/link.rs)), so evaluating a row's readers lists none of it.

**Rationale.** A unit of erasure is a declaration, not a module, so a row copied into each fragment that names it is a copy per declaration: a family's constructors, each an item reading its family, cost the square of the family. [Lean's](https://raw.githubusercontent.com/leanprover/lean4/master/src/Lean/Declaration.lean) constructor carries its index (`cidx`) and [OCaml's](https://raw.githubusercontent.com/ocaml/ocaml/trunk/typing/data_types.mli) its tag and its type's constructor counts (`cstr_tag`, `cstr_consts`, `cstr_nonconsts`): an importer reads the declaration and keeps the tag.

**Rejected.** A position on every constructor's record, which every reader of a record would carry for an importer's sake; a row held by reference, where a fragment would no longer verify alone.

### Numeric carriers are exact

**Decision.** The erased carriers are Core's, unbounded — `Nat` and `Int` as `curios-num`'s `Natural` and `Integer`, `Flt` as binary64 — with the carrier methods every stage's constant folder shares. The runtime's split between an i31 and a boxed magnitude appears nowhere in the IR; it is `curios-cps`'s and `curios-emit`'s ([Nat and Int are an i31 until they outgrow it](../documentation/design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md)).

**Rationale.** One shared semantics keeps the folders from drifting from each other or from emitted code, and keeping the runtime's representation out of the IR keeps a representation choice from becoming a semantic one.

### Reconstruction recovers the existing value

**Decision.** The erased optimizer removes complete, ordered product reconstruction and exhaustive constructor identity before layout selection, and forwards retained unit fields only where immutable value flow proves the particular source field is unit. Construction sites and fields form a shared graph; a deterministic return-and-field fixed point widens beyond 32 origins to unknown. Parameters, opaque calls, state reads and computed recursive members establish no contents. Callable proof stubs remain functions. Constructor wrappers are recognized by their bodies, including acyclic forwarding, and removing a match proves every arm contains only safe administrative work. Substitution and removal of the bindings it strands are one transformation, so an immediately returned call reaches the existing tail-call lowering.

**Rationale.** Erasure can leave runtime reconstruction around an answer whose source only restates evidence. Schema and constructor identities make equality explicit here, before layout encoding obscures it. Computed recursive members are forcing boundaries; effects, traps and unknown calls retain their order.

**Rejected.** A second general simplifier in ANF, constructor recognition by prelude names, and treating an arm's return value alone as proof that its evaluation can be deleted.

### A recursion closed by a monoid is a loop

**Decision.** The erased optimizer makes a loop over accumulators of a recursion whose leaves close their own call with combines of one monoid: tail recursion modulo a monoid context, as Leijen and Lorenzen derive it ([*Tail Recursion Modulo Context*](https://www.microsoft.com/en-us/research/uploads/prod/2023/07/trmc-popl23.pdf), §4.4–4.5). A leaf ends in a self-call and a chain of combines, each reading the answer before it once, in a place the monoid's product takes, every other operand a value from before the call, so the chain is one word `l ⊕ □ ⊕ r`. A monoid that commutes folds both sides into one accumulator; a sequence's concatenation, which does not, threads the left side's accumulator, the right side's, or both, as the leaves use them. Which scalar operation is a monoid and its identity, and which sequence operation is its grain's free monoid's product or append, are the oracle's to state (`semantics.rs`), by matches over the closed alphabets, and a law test holds every one against the oracle's own folds.

**Rationale.** A recursion left as written takes a frame and a return an element, which a module's stack holds ([`curios-native`'s README](../curios-native/README.md#a-module-runs-on-a-stack-of-its-own)) and a loop does not pay. Only the combines move across the call — Koka's liberal context, whose operands after the hole are values — so nothing observable is reordered, and the licence is associativity and an identity, since no carrier here is partial. The laws are the language's own: `Nat`, `Bytes` and `List` are Core's free monoids, and conversion decides their concatenation's associativity and identities ([The carriers' algebra stays in conversion](../documentation/design/arithmetic/the-carriers-algebra-stays-in-conversion.md)). A rope's concatenation is one node, so the accumulator builds what the written recursion's cons built at the same allocations and needs no reverse, the case Erlang finds faster than a body recursion ([The Seven Myths of Erlang Performance](https://www.erlang.org/docs/22/efficiency_guide/myths)); it leans the rope left, as `List/fold`'s builders already do.

**Rejected.** A destination with a hole, OCaml's [tail modulo constructor](https://ocaml.org/manual/5.3/tail_mod_cons.html) and Koka's constructor contexts: a rope node states its length where it is built, and that length is the part not yet built. A twin proved equal, Lean's `@[csimp]`: a second definition and a proof for every builder, a door that swaps a definition where it is erased, and `/std` alone reached. A move across the call licensed by the effect summary: the summary decides whether an unused binding may be deleted, a division takes its proof by position, and that proof can be the recursive call's answer, erased. Commutativity as the licence, as LLVM's accumulator rewrite takes it: it leaves every sequence out. A continuation as the accumulator: it moves the frames to the heap, where the stack already holds them. Mutually recursive groups and a recursion of two calls: `/std`'s descend a command tree and a key's bits, never a list's length.
