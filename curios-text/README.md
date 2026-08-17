# curios-text

The Curios surface language: lexer, parser, surface AST, printer, module resolution, generated `/sys`, and the `into_core` lowering that hands the rest of the pipeline a flat `curios_core::Module`. What the surface language *is* — every form, and what it means — belongs to [syntax.md](../documentation/syntax.md), which is normative and which `src/parse.rs` implements. That a surface feature is always an AST node desugared during lowering, never in the parser, is the cross-cutting rule in [Syntax forms are closed: semantics extend by witness](../documentation/design/language/syntax-forms-are-closed-semantics-extend-by-witness.md). Local architecture — the combinator grammar, the visibility algebra, the lowering's shape — belongs to the crate rustdoc.

## Design

### The logical-to-physical mapping is two halves, and they stay apart

**Decision.** A `Mount` binds a logical prefix to a unit and carries the privilege tier that decides what may reference an internal root; lookup is longest-match, because the entry mounts the empty prefix and every qualifier lies within it. A `RootSource` binds each of those prefixes to a base on disk. Neither derives the other.

**Rationale.** The two answer different questions — *what is this name* and *where do its bytes live* — and only one of them exists in every product. The browser has mounts and no directories at all, so a design that reached the physical half through the logical one would have nothing to hand it.

### A stem is never part of a name

**Decision.** `mod x` declared in a namespace's header resolves to `x.crs` in that namespace's directory, and a header's namespace directory is its stem directory: `mod util` in `<dir>/main.crs` reads `<dir>/main/util.crs`, and `util`'s own children read from `<dir>/main/util/`. The stem `main` is spelling; the qualifier is `/util`.

**Rationale.** One rule governs every file in the language, so the file handed to `run` is a header like any other and declares its children the same way — there is no special case for the entry, which is what lets a `.crs` file be standalone wherever it sits.

The rule is also what shapes the API: `RootSource::mounted` takes the header and the directory as two arguments rather than deriving one from the other. A package's library header sits beside its manifest while its namespace *is* the manifest's directory, and that exception is the manifest's to state — so this crate must be able to be told, rather than inferring a layout it does not own.

**Rejected.** Deriving the directory from the header path. It reads correctly for every standalone file and silently wrongly for every package, and the failure is a name resolving to the wrong file rather than an error.
