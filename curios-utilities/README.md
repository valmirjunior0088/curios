# curios-utilities

Foundational utilities shared across every pipeline stage: source spans and reports, the `Entropy`/`Mint` fresh-name supply, the `name!` and `id!` newtype macros, the typed identity-addressed `Arena`, the persistent maps a state extended a step at a time is kept in, the resolved-module-path `Qualifier`, the mount table, the SHA-256 content digest and the fingerprint that finishes into one, the shape of the compiler's emitted vocabulary, and the native-stack bracket every recursive walk over user data runs inside. The numeric vocabulary is `curios-num`'s, and the two combinator DSLs are `curios-parse` and `curios-print`; each module's contract belongs to the crate rustdoc.

## Design

### Names are never ordered

**Decision.** No `name!` form derives `PartialOrd` or `Ord`.

**Rationale.** Ordering a name orders its spelling, and a spelling is identity and rendering only, never a source of behaviour: a `BTreeMap` keyed on a name makes constructor collation order the emitted runtime tag order, so renaming a case renumbers the tags of every case it sorts past. Without the derive, an ordered collection keyed on a name is a compile error; hash collections serve where a name is a key, and an explicit sequence where order is load-bearing.

### One source per identity space

**Decision.** `ArenaId::from_index` is the one narrowing intrinsic — loud on exhaustion, never wrapping — and both ways of minting an identity go through it: an arena's `mint` and `reserve`, and the `Entropy` gensym the `id!(Foo, "f", mint)` form implements. An arena-backed identity is minted only by its arena, a gensym identity has no arena, and no identity type is both. Identities are never reused, removal tombstones rather than moves, and `compact` is the one pass that moves slots. `Entropy` counts in `u64` on every target: an identity that indexes nothing, a binder's, keeps the whole count, and one that indexes a store narrows where it is minted.

**Rationale.** Two sources over one space would hand out one index twice. Tombstoning keeps iteration order equal to identity order, so a deterministic construction yields deterministic identities, a contract stated here once for `curios-anf` and `curios-cps`. A `usize` counter is 32 bits on wasm32, where the playground runs, so it would wrap where the native build does not, and a width read off the host is a second limit no refusal names; a count advanced one tick at a time does not reach the top of a `u64` in any computation that ends, as `std::thread::ThreadId`'s does not, and a store indexed by a narrower one cannot hold more entries than its address space.

### Qualifiers and symbols are interned once per process

**Decision.** A `Qualifier` is a `&'static` reference to the one allocation of its path, and a binder's display hint a `Symbol`, a `&'static` reference to the one allocation of its spelling, both interned once per process and never freed, so both are `Copy` and cross threads. Equality compares addresses, which interning makes exact; ordering and hashing read the content, so both come out the same in every process. The archive reads a path or spelling back through the same tables, found by its archived content where it lies (`Interned`, `InternedText`), so its archived form is the bare content and an occurrence of a name the process holds already allocates nothing.

**Rationale.** An owned `Vec<String>` per qualifier makes whole-program compilation markedly slower than strings, and sharing with a pointer-identity fast path brings it below them; sharing pays where names are created sharing, which for the prelude is the archive's hundred thousand qualifier occurrences over a couple of thousand distinct paths, and where they are read: an occurrence read back as segments of its own and then looked up allocates once for the path and once for each segment, every time a record names it. One process-wide table lets a term cross threads and a name restored on one thread share with the same name on another.

**Rejected.** A memoized structural hash, measured indistinguishable from the uncached one while its `OnceCell` trips `mutable_key_type` wherever a qualifier keys a map — do not add one back without a measurement. A dense id into a process-wide vector, whose content ordering would take the table's lock per comparison. Freeing an entry once nothing names it, which a `Copy` identity cannot know; the tables are bounded by the distinct names a process meets. Sources are the opposite case — a language server loads a new text per edit — so a span holds its source by reference count.

### What a segment may spell is decided once, beside the identity

**Decision.** `is_identifier` and `is_keyword` — the identifier characters and the reserved words — live beside `Qualifier`, not in the parser.

**Rationale.** A segment's legality is a property of the identity: `curios-text` refuses a keyword in a path and `curios-package` refuses one as a package's name, which becomes a mount prefix nothing could write. One list below both keeps the two refusals one refusal.

### A mount is a prefix, not an identity beside it

**Decision.** Which mount owns a declaration is `Mount::owning` over the name — the most specific mounted prefix it lies within — and the only thing carried is the mount list, one per module. No answer is derivable from a name alone, since a leading segment identifies a mount only against the table of what is mounted: a package's prefix and a module the entry declares are the same shape.

**Rejected.** A root stamp on each declaration, cached beside the name whose leading segment already determines it, which archived means something only in the compilation that wrote it — the shape rustc pays a `cnum_map` to translate.

### The syntax registry states slots, never spellings

**Decision.** `SyntaxRegistry` names every compiler-known slot as a typed field; `curios-text` fills it (`registry.rs`), `curios-prelude`'s build holds every slot to a declaration of the prelude, and `curios-text`'s lowering and `curios-elab`'s type-directed features read the filled registry. Every enumeration over it opens by destructuring the struct it enumerates.

**Rationale.** The consumers sit below the crate holding the authored declarations, so the shape lives below both. A pattern naming fewer fields than its struct does not compile, so a slot added to a group is a compile error until it is enumerated.

**Rejected.** Enumerations written out by hand, which leave a slot unenumerated, and so unchecked, from the commit that adds it.

### Stack depth is bought here

**Decision.** Recursive walks over data-shaped depth run inside `recurse`, which grows the native stack when the reserve runs low, and a stage's entry point inside `grown`, which takes a segment unconditionally; the two figures are written here and nowhere else ([Depth is bought with stack, not with hand-rolled frames](../documentation/design/compilation/depth-is-bought-with-stack-not-with-hand-rolled-frames.md)).

**Rationale.** Figures kept once cannot drift, where three call sites carrying their own constants could.

### A persistent map's shape is a function of the keys it holds

**Decision.** A unit's tables are held in this crate's persistent maps (`Persistent`, `Ordered`, `Sorted`): a treap whose shape is a function of the keys it holds, so two versions holding one set of keys are one tree. A table a unit stores is held in a map read back in a stated order — `Ordered`, by a declaration's place, or `Sorted`, by name, archived as its entries in that order (`KeyOrder`) — and never in `Persistent`, whose order follows the process; an index a walk only looks up in may be. Two versions are compared by the nodes they do not share (`Ordered::diff`). Two `Sorted` maps are joined smaller into larger (`Sorted::union_with`): the larger is taken as it stands, the smaller's entries put into it, so a union copies the paths to the keys it adds and is the larger itself where it adds none. An `Ordered` map may keep a summary of each subtree (`Summary`), made again on every path a write copies, which a rewrite reads to pass over a subtree whole (`Ordered::rewritten_where`).

**Rationale.** A recompile makes a version of its baseline ([A compile costs what it reads](../documentation/design/compilation/a-compile-costs-what-it-reads.md)): replacing an entry costs a path, and two versions are compared by what they do not share, which a shape that is a function of the keys lets a walk find node beside node. A table stored in an order the process chose makes a unit's bytes follow the process ([`curios-unit`'s README](../curios-unit/README.md#a-unit-is-a-function-of-what-it-was-compiled-from)). A union that shares the larger operand is what lets a term's node carry its free variables in what it adds ([`curios-core`'s README](../curios-core/README.md#a-nodes-free-variables-are-its-childrens-shared)), O(m log n) for maps of m ≤ n entries, and a subtree's summary what lets a metavariable's spine, as wide as the scope it was born in, be rewritten in the entries a capture reaches — the augmented maps of [Sun, Ferizovic and Blelloch](https://arxiv.org/abs/1612.05665).

**Rejected.** A stored table held in `Persistent`, whose order is a hash's the process draws.
