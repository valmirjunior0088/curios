# curios-utilities

Foundational utilities shared across every pipeline stage: source spans and reports, the `Entropy`/`Mint` fresh-name supply, the `name!` and `id!` newtype macros, the typed identity-addressed `Arena`, the resolved-module-path `Qualifier`, the mount table, the SHA-256 content digest and the fingerprint that finishes into one, the shape of the compiler's emitted vocabulary, and the native-stack bracket every recursive walk over user data runs inside. Each module's contract belongs to the crate rustdoc. The numeric half of what this crate once held is `curios-num`; the two combinator DSLs are `curios-parse` and `curios-print`.

## Design

### Names are never ordered

**Decision.** No `name!` form derives `PartialOrd`/`Ord`.

**Rationale.** Ordering a name is ordering its spelling, and a spelling is identity and rendering only, never a source of behaviour. Deriving it once let a `BTreeMap<Atom, _>` make constructor collation order the emitted runtime tag order, so renaming a case silently renumbered the tags of every case it sorted past. Without the derive, an ordered collection keyed on a name is a compile error rather than a convention someone has to remember; the hash collections serve where a name is genuinely a key, and an explicit sequence where the order is load-bearing.

### One source per identity space

**Decision.** `ArenaId::from_index` is the one narrowing intrinsic — loud on exhaustion, never wrapping — and both ways of minting an identity go through it: an arena's `mint`/`reserve`, and the `Entropy` gensym the `id!(Foo, "f", mint)` form implements. An arena-backed identity is minted only by its arena, a gensym identity has no arena, and no identity type is both. Identities are never reused, removal tombstones rather than moves, and `compact` is the one pass that moves slots.

**Rationale.** Two sources over one space would hand out one index twice, which is the failure this rules out. Tombstoning keeps iteration order equal to identity order, so a deterministic construction yields deterministic identities; the contract is written here once rather than per module in `curios-ersd` and `curios-cont`.

### Qualifiers and symbols are interned once per process

**Decision.** A `Qualifier` is a `&'static` reference to the one allocation of its path, and a binder's display hint is a `Symbol`, a `&'static` reference to the one allocation of its spelling. Both are interned once per process and never freed, so both are `Copy` and both can cross a thread. Equality compares addresses, which interning makes exact; ordering and hashing read the content, so both come out the same in every process. The archive reads a path or a spelling back through the same tables (`Interned`, `InternedText`), so the archived form is the bare content it always was.

**Rationale.** Measured: an owned `Vec<String>` per qualifier made whole-program compilation markedly slower than the strings it replaced, and sharing with a pointer-identity fast path brought it below them. A memoized structural hash on top was then written, measured, and removed — indistinguishable from the uncached version, while its `OnceCell` tripped `mutable_key_type` at 79 sites across two crates; do not add one back without a measurement that says otherwise. Sharing only pays where names are created sharing, which for the fixed prelude is the archive: a hundred thousand qualifier occurrences over a couple of thousand distinct paths. Sharing behind an `Rc` kept every term on the thread that built it, and interning per thread kept a name restored on one thread from sharing anything with the same name on another; one table per process answers both.

**Rejected — a dense id into a process-wide vector.** It is smaller than a pointer, but ordering by content — which every ordered map over names needs for its output to be the same in every process — would then take the table's lock per comparison. **Rejected — freeing an entry once nothing names it.** An identity that is `Copy` cannot know when its last copy is gone; the tables are bounded by the distinct names a process ever meets instead. Sources are the opposite case — a language server loads a new text on every edit — which is why a span holds its source by reference count rather than by an interned id.

### What a segment may spell is decided once, beside the identity

**Decision.** `is_identifier` and `is_keyword` — the identifier characters and the reserved words — live beside `Qualifier`, not in the lexer.

**Rationale.** A segment's legality is a property of the identity, not of one stage's reading of it: `curios-text` refuses a keyword when it parses a path, and `curios-package` refuses one when it parses the name a package declares for itself — a name that becomes a mount prefix, and a prefix nothing can write is unreachable. One list below both keeps those two refusals the same refusal.

### A mount is a prefix, not an identity beside it

**Decision.** Which mount owns a declaration is `Mount::owning` over the name — the most specific mounted prefix it lies within — and the only thing carried is the mount list, one per module.

**Rationale.** A declaration used to carry a `RootId` stamp naming its root, cached beside the name whose leading segment already determined it; archived, the stamp meant something only in the compilation that wrote it — the shape rustc pays a `cnum_map` to translate. There is deliberately no answer derivable from a name alone: a leading segment identifies a mount only against the table of what is mounted, because a package's prefix and a module the entry declares are the same shape.

### The syntax registry states slots, never spellings

**Decision.** `SyntaxRegistry` names every compiler-known slot as a typed field; `curios-prelude-archive` fills it, and the two stages that emit those names — `curios-text`'s lowering and `curios-elab`'s type-directed features — read the filled registry. Every enumeration over it opens by destructuring the struct it enumerates.

**Rationale.** The consumers sit below the crate that holds the authored declarations, so the shape must live below both — a consumer must see the type, and the prelude sits above every consumer in the crate graph. The destructuring is not stylistic: a pattern naming fewer fields than the struct has does not compile, so a slot added to a group is a compile error until it is enumerated. The lists were written out by hand before, and one slot sat unenumerated, and so unchecked, from the commit that added it.

### Depth is bought with stack, not with hand-rolled frames

**Decision.** Recursive walks over data-shaped depth run inside `recurse`, which grows the native stack when the reserve runs low, and a stage's entry point inside `grown`, which takes a segment unconditionally; the two figures are written here and nowhere else. The decision is [Depth is bought with stack, not with hand-rolled frames](../documentation/design/toolchain/depth-is-bought-with-stack-not-with-hand-rolled-frames.md).

**Rationale.** The design entry's: reduction and conversion are implemented twice so that a bug in one is caught by disagreement with the other, and two recursive strategies can be read side by side where two hand-rolled state machines cannot. The figures live once because three call sites once carried their own pair of constants — three chances to drift, and no way to tell which was right.
