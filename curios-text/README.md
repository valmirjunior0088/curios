# curios-text

The Curios surface language: the scannerless parser, surface AST, printer and formatter, module resolution, generated `/sys`, and the `into_core` lowering that hands the rest of the pipeline a flat `curios_core::Module`. What the surface language *is* belongs to [syntax.md](../documentation/syntax.md), which is normative and which `src/parse.rs` implements; that a surface feature is an AST node desugared during lowering, never in the parser, is [Syntax forms are closed, semantics extend by witness](../documentation/design/surface/syntax-forms-are-closed-semantics-extend-by-witness.md). Local architecture — the combinator grammar, the visibility algebra, the lowering's shape — belongs to the crate rustdoc.

## Design

### The logical-to-physical mapping is two halves, and they stay apart

**Decision.** A `Mount` binds a logical prefix to a unit and records whether its root is one the compiler supplies, which no manifest can name — which root may reference which is the units' declared dependencies; lookup is longest-match, since the entry mounts the empty prefix. A `RootSource` binds each prefix to a base on disk. Neither derives the other.

**Rationale.** They answer different questions — what is this name, where do its bytes live — and only the first exists in every product: the browser has mounts and no directories.

### A stem is never part of a name

**Decision.** `mod x` declared in a namespace's header resolves to `x.crs` in that namespace's directory, and a header's namespace directory is its stem directory: `mod util` in `<dir>/main.crs` reads `<dir>/main/util.crs`. The stem `main` is spelling; the qualifier is `/util`. `RootSource::mounted` takes the header and the directory as two arguments.

**Rationale.** One rule governs every file, so the file handed to `run` is a header like any other and a `.crs` file is standalone wherever it sits. A package's library header sits beside its manifest while its namespace is the manifest's directory, and that exception is the manifest's to state, so this crate is told rather than inferring a layout it does not own.

**Rejected.** Deriving the directory from the header path, which reads correctly for every standalone file and resolves names to the wrong file in every package.

### A top-level item is dispatched on its head, and a reserved head commits

**Decision.** `parse_top_item` reads the optional `pub` and the item's leading word once and switches on it. A head that is reserved and cannot begin a term — `mod`, `use`, `induct`, `struct`, `foreign` — commits, so its arm owns the diagnosis. The other four fall through, and the language decides which: `concept`, `satisfy` and `test` are contextual words [syntax.md](../documentation/syntax.md) keeps as identifiers outside a declaration position, so one may begin a program's tail, and `let` is shared with the term grammar, since `let x = 1; tail` must fall through to a local binding. A `pub` or a `---` documentation comment in front makes every arm commit, since after either there is no tail for a failed item to become. A head that names no item is recoverable under the same exception — it is how the item loop ends before a tail — and reports the heads that would have named one. A module, having no tail, re-runs the item parser when input remains, since the repetition keeps only committed failures. The head is read raw, without its trailing whitespace, so an unrecognized one is reported against the word.

**Rationale.** Every top-level form is led by one word from a disjoint set, so ordered choice encodes a dispatch as backtracking: all nine alternatives fail at one offset, the furthest-failure tie-break keeps the earliest and blames `mod` for every unrecognized head, and the recoverable failures leave a library reporting `Expected 'end-of-file'` at its first column. Reading the head once makes the arm that owns the error the arm that produced it. Commitment is stated at the dispatch, as `curios-parse`'s choice asks ([its decision](../curios-parse/README.md#choice-backtracks-until-an-alternative-commits)), because depth is the wrong selector.

**Rejected.** Reserving `concept`, `satisfy` and `test`, which syntax.md keeps contextual and which `is_keyword`, owned by `curios-utilities`, would then refuse as a module or package name. An offset threshold, trusting the item error only past the first token, which leaves `satisfy` blaming `mod`. Committing on all nine in modules only, a flag threaded into nested bodies for a message the module's leftover-input path already gives.

### A broken item stays in the tree, and `str::parse` is whole or nothing

**Decision.** A committed parse failure inside a top-level item does not end the module. The item loop keeps it as `TopItem::Broken` — the report, the text skipped, and the name the head declared where it parsed that far — and resumes at the next line beginning at column 0 with `pub`, a `---` comment, or an item's head word followed by whitespace (`curios_parse::recover`, with this crate's anchor rule). A candidate is parsed, never trusted, and only committed failures recover; a `let` commits once its signature and `=` are read. A failure inside an inline module body is the `mod` item's, and its name is nobody's. Lowering registers a broken item's name as a public binding and lowers nothing for it; the compile boundary refuses a unit holding one, listing every broken item ahead of elaboration's reports, and the elaborator withholds every dependent of a broken name. `Module::parse` and `Entrypoint::parse` recover — the compilation's, the editor's and standard input's reading — while `str::parse` is whole or nothing, refusing at the first broken item, for a fixture, the header `curios-package` reads, and the formatter.

**Rationale.** Without recovery one parse failure costs the whole file's diagnostics, the one stage stopping at its first mistake where elaboration recovers per item. Recovery is commitment read one level up, so the item-or-tail ambiguity gains no heuristic. Column 0 is the formatter's convention, so code ignoring it only loses an anchor, and a wrong anchor produces neither a wrong tree nor a spurious diagnostic. A fixture asserting a diagnosis must not pass on a module parsed with a hole in it, and a formatter must not rewrite a file it could not read whole.

**Rejected.** Resynchronizing at an item's terminator, which needs a lexer for strings, comments and `end` nesting the scannerless grammar lacks, and meets `;` inside a body; reusing the editor grammar's recovery, a second grammar the editors own; a side list of parse errors beside a module that pretends to be whole, which a consumer could forget ([A module is a compilation unit](../documentation/design/architecture/a-module-is-a-compilation-unit-and-the-prelude-is-an-environment.md)); registering a broken `mod`'s scope, when a broken module declares nothing.
