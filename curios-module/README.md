# curios-module

The Curios module system, over the tree [`curios-surface`](../curios-surface/README.md) reads a file into: loading a file or a supplied text, discovery and module resolution, the lints, the `into_core` lowering that hands the rest of the pipeline a flat `curios_core::Definitions`, and the registry of the names the compiler emits in `/sys` and in `/std`, the two roots [`curios-prelude`](../curios-prelude/README.md) builds. That a surface feature is a tree node desugared during lowering, never in the parser, is [Syntax forms are closed, semantics extend by witness](../documentation/design/surface/syntax-forms-are-closed-semantics-extend-by-witness.md). Local architecture — the visibility algebra, the lowering's shape — belongs to the crate rustdoc.

## Design

### The logical-to-physical mapping is two halves, and they stay apart

**Decision.** A `Mount` binds a logical prefix to a unit and records whether its root is one the compiler supplies, which no manifest can name — which root may reference which is the units' declared dependencies; lookup is longest-match, since the entry mounts the empty prefix. A `RootSource` binds each prefix to a base on disk. Neither derives the other.

**Rationale.** They answer different questions — what is this name, where do its bytes live — and only the first exists in every product: the browser has mounts and no directories.

### A root's modules are beside it, a module's in its stem directory

**Decision.** `mod x` declared in a namespace's header resolves to `x.crs` in that namespace's directory. A unit's root — a library's `lib.crs`, a program's entry, a file checked as a module of its own — has the directory it sits in as its namespace's, and every module below it its stem directory: `mod util` in `<dir>/main.crs` reads `<dir>/util.crs`, and `util`'s own children read from `<dir>/util/` (`RootSource::namespace_of`). A stem is never part of a name: `main` is spelling, and the qualifier is `/util`. Roots that sit in one directory share it, and a file two units' `mod` chains reach is refused before elaboration, naming both (`curios-package`'s `layout` module).

**Rationale.** A root is where a unit begins, so its namespace begins where it stands, and below it a qualified name's tail and its path are one string: the library's `/json/parse/lexer` is `parse/lexer.crs` beside the manifest and a program's `/util` is `util.crs` beside its entry, by one rule that no manifest has to state an exception to. It is Rust's: a crate root reads its modules beside itself and any other module file from a directory named for it ([The Rust Reference](https://doc.rust-lang.org/reference/items/modules.html)). Its known cost is two roots in one directory declaring one file, which Cargo compiles twice as two unrelated modules, a trap Rust's maintainers have proposed deprecating `main.rs` beside `lib.rs` to close ([Rust internals](https://internals.rust-lang.org/t/deprecating-having-both-src-lib-rs-and-src-main-rs-in-the-same-package/23033)); here it is a refusal naming both, as dune refuses a module two stanzas claim. The price is that a file reads its `mod`s beside itself as a root and from its stem directory as a module, so declaring a loose file as a module moves where its children are read; a file is placed in one unit, and that decides which it is.

**Rejected.** One rule for every header, its stem directory, with the library's exception stated by its manifest: a package's library read its modules beside itself and its own program from `exe/`, two rules for the two files a package is. A rule by file name, `lib.crs` and `exe.crs` beside themselves wherever they sit: a meaning for two spellings in the language, and every other entry still read from its stem directory. A flat rule, every file's modules beside it: a qualified name's tail stops being its path, and modules at two depths collide.

### The compiler emits names, not a root of its own

**Decision.** Every name `curios-module`'s lowering or `curios-elab` emits is an ordinary `/sys` or `/std` declaration reached through the filled `SyntaxRegistry` (`src/registry.rs`), never through a root reserved for it: the concepts syntax dispatches through, the certified literal structures `/std/Char` and `/std/Str`, the test vocabulary and the spell renderers sit beside the types and witnesses that consume them. `/sys` holds only what `/sys` itself needs — the propositions its intrinsics state as preconditions, and the guest outcomes those operations return. The registry contract belongs to `curios-common`.

**Rationale.** The emission contract needs a closed, auditable surface, and the registry makes it mechanical: the build asserts every registered name is declared and every registered concept declares the method the elaborator projects. A root of the compiler's own would create a cycle — it imports `/std` for the types its concepts mention while `/std` imports it for the concepts its witnesses satisfy — so the prelude could not be split into units nor `/std` be a package. The compiler is not a unit, so an emitted name lives wherever it belongs without an edge in the fold.

**Rejected.** Moving the emitted names down into `/sys`: `Test/main` needs `List`, `Str`, `Io` and `Bool`, and `Test/equal` needs `Equal`, `Spell` and `Show`, so `/sys` would absorb most of `/std` and re-create the cycle a level lower. `/sys` membership is decided by what `/sys` itself must name.

### Optional and fallible values belong to the guest foundation

**Decision.** `/sys/Option` and `/sys/Result` are ordinary universe-polymorphic inductives, `some` before `none` and `success` before `failure`, the failure's type first; `/std/Option` and `/std/Result` re-export each family and its constructors, and their functions, witnesses and derivations stay in `/std`. The registry names each declaration and its constructors directly. The generated declaration keeps source identities for its `Type` occurrences, so its type former and constructor signatures share their universe seeds.

**Rationale.** `/sys` constructs them without reaching into `/std`, as a cell's poll answers an `Option`. Anonymous generated occurrences would assign unrelated levels and specialize consumers such as `Cli` wrongly.

### A unit read from Core text is a root source over its file

**Decision.** `RootSource::core_text`, behind `test-support`, is the source of a unit written as Core text, mounted at its prefix: it reads its one `.crsc` file through the loader, so the file is logged as read, and lowers nothing, since the module is read from the text and not lowered from a program.

**Rationale.** A unit a history steps is filed, verified and offered as a baseline by the store as any unit is, and the store keys a unit by what its source read, so a Core-text unit's source is one that reads its file.

**Rejected.** A Core-text unit kept out of the store, handed to the next step directly: it would never be mounted as a build mounts one.
