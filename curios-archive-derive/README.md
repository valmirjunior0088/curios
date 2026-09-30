# curios-archive-derive

The attribute macro behind `curios-archive`: `#[archived]`, which gates rkyv's three derives on the consuming crate's own `archive` feature and redirects their paths through `curios_archive::rkyv`. Depend on `curios-archive`, never on this crate. What the macro accepts, how its keywords compose and how a field marker is rewritten belong to the crate rustdoc.

## Design

### A crate of its own, because a proc-macro crate can export nothing else

**Decision.** The macro is a separate crate, the serde and serde_derive arrangement for serde's reason: a `proc-macro = true` crate exports nothing but macros, so `curios-archive`'s types, traits and functions cannot share a crate with the attribute that annotates them.

### The expansion is gated, never the attribute

**Decision.** `#[curios_archive::archived]` is written unconditionally, and the consuming crate's `archive` feature gates the expansion, through `cfg_attr`; `curios-archive` depends on the macro unconditionally.

**Rationale.** A macro that vanished with the feature would make every annotated type a compile error with archiving off. `cfg_attr` is evaluated where the macro expands, so `feature = "archive"` names the consuming crate's own feature, and this crate need not know which crates have it on.

### No dependencies, by token concatenation

**Decision.** The crate depends on nothing, neither `syn` nor `quote`: it prepends attributes and walks the item's token trees only far enough to recognise the two field markers — `#`, a bracket group, and one of two idents.

**Rationale.** Recognising a marker needs no grammar, so the field adapters are read without a parser dependency.

### The field markers are inert

**Decision.** `#[archived_with(Adapter)]` and `#[archived_omit_bounds]` are declared by nothing; the macro consumes them and rewrites them into the gated `rkyv(…)` helper.

**Rationale.** A marker written outside an `#[archived]` item is an unresolved-attribute error rather than a line that silently does nothing, and consuming them here lets rkyv be spelled nowhere but its two owning crates.
