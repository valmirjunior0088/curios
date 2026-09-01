# curios-archive-derive

The attribute macro behind `curios-archive`: `#[archived]`, which gates rkyv's three derives on the consuming crate's own `archive` feature and redirects their paths through `curios_archive::rkyv`. Depend on `curios-archive`, never on this crate. What the macro accepts, how its keywords compose, and how a field marker is rewritten belong to the crate rustdoc.

## Design

### A crate of its own, because a proc-macro crate can export nothing else

**Decision.** The macro is a separate crate — the serde/serde_derive arrangement, for the same reason serde has it.

**Rationale.** A `proc-macro = true` crate can export nothing but macros, so the types, traits and functions of `curios-archive` cannot share a crate with the attribute that annotates them.

### The expansion is gated, never the attribute

**Decision.** `#[curios_archive::archived]` is written unconditionally, and what the consuming crate's `archive` feature gates is the *expansion*, through `cfg_attr`. The macro is not itself feature-gated, and `curios-archive` depends on it unconditionally.

**Rationale.** A macro that vanished with the feature would make every annotated type a compile error in a build with archiving off. `cfg_attr` is evaluated where the macro expands, so `feature = "archive"` names the consuming crate's own feature — each crate keeps its gate, and this one neither knows nor needs to know which crates have it on.

### No dependencies, by token concatenation

**Decision.** The crate depends on nothing: neither `syn` nor `quote`. It prepends attributes and walks the item's token trees only far enough to recognise the two field markers — `#`, a bracket group, and one of two idents.

**Rationale.** Reading the body at all was declined while "field adapters are rare" stood in for a number, and the number turned out to be fifteen. What made reading it cheap in the end is that recognising a marker needs no grammar, so the walk is over token trees and the crate still depends on nothing. `syn` was never the price; it was only assumed to be.

### The field markers are inert

**Decision.** `#[archived_with(Adapter)]` and `#[archived_omit_bounds]` are declared by nothing; the macro consumes them and rewrites them into the gated `rkyv(…)` helper.

**Rationale.** A marker written outside an `#[archived]` item is then an unresolved-attribute error rather than a line that silently does nothing — and consuming them here is what lets rkyv be spelled nowhere but its two owning crates.
