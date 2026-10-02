# Privacy is scoped to a subtree

**Decision.** A declaration without `pub` in module `M` is visible exactly within `M`'s subtree, `M` and its descendants at any depth; a `pub` declaration is also visible wherever `M` is. The same rule governs both namespaces and the representation marker a struct, an inductive or a concept carries — `: pub Type` is transparent, `: Type` sealed — so a sealed representation is transparent throughout its declaring subtree and opaque outside it. For a concept, sealing confines witness declarations, dictionary literals, structure updates and raw projections to the subtree, while resolution, `use` parameters and the method wrappers work everywhere; a sealed `pub` concept's fields are not interface, so a private superclass is an obligation resolution discharges without the consumer naming it. Reachability along a path is the conjunction of the rule at each hop. `use M/*` imports the exported surface only, and the interface audit compares audiences rather than paths, so a public module can re-export selected names out of a private child.

**Rationale.**

- **The unit of trust is not the unit of file organization.** Were privacy per file, an abstraction that outgrew one would choose between a monolith and a public representation, and splitting it would add a public helper at every boundary the split crossed. The trusted set is a directory the author owns, which is the boundary a facade hides behind.
- **The asymmetry is deliberate**: descendants gain, ancestors and siblings gain nothing, so a sibling subtree cannot open a representation and `pub` keeps one meaning.
- **Sealing a concept fixes its instance set**, which nothing else can: the public wrappers and explicit implicit arguments leak any field, defeating a private-token workaround. A concept is a record, so its visibility is the record's and adds no check.
- **Privacy is a rule about what surface elaboration may write, never what conversion sees.** Enforcement is the island in `curios-elab`'s `Context`; erasure and the metavariable oracle, which re-derive types from elaborated terms, suppress it through the one bracket that clears the island, since compiler-built projections are never subject to it. It cannot reach `False`, which is why [the soundness board](../soundness/the-soundness-board.md) holds no entry for it.

**Rejected.**

- **Ancestor privilege**, which answers "who can break this invariant" only by reading a whole subtree the declaration does not own.
- **`pub(<path>)` targeted export**, a widening still available later, which makes a declaration name its consumers.
- **ML-style signature ascription**: modules are namespaces, and visibility belongs on the declaration.
- **Coherence-only opacity for concepts**, blocking dictionary literals while exempting `satisfy`: no `Key` dictionary can corrupt `/std/Map`'s trie, whose invariant is over bytes, and what a second one can do — miss an entry, put one element in a `Set` twice — takes a `use value` the caller wrote ([Concepts resolve with global coherence](concepts-resolve-with-global-coherence.md)).
- **A tighter orphan rule** in place of sealing: representation privacy already gates the construction site.
