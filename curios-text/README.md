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

### A top-level item is dispatched on its head, and a reserved head commits

**Decision.** `parse_top_item` reads the optional `pub` and the item's leading word once and switches on it, rather than trying nine ordered alternatives that each re-read the same two tokens. A head that is reserved *and* cannot begin a term — `mod`, `use`, `induct`, `struct`, `foreign` — commits through `curios_parse::commit`, so its arm owns the diagnosis and no enclosing choice may backtrack into a vaguer one. The other four are caught back to recoverable, and the language decides which: `concept`, `satisfy` and `test` are contextual words that [syntax.md](../documentation/syntax.md) keeps as ordinary identifiers outside a declaration position, so one of them may really begin a program's tail; `let` is reserved but shared with the term grammar, since a top-level `let` requires an annotation and `let x = 1; tail` has to fall through to a local binding. A head that names no item is recoverable for the same reason — it is how the item loop terminates before a tail — and reports the heads that would have named one. A module, having no tail, additionally re-runs the item parser when input remains, because the repetition combinator keeps only uncaught failures and would otherwise leave the end-of-input expectation to invent a message.

**Rationale.** Every top-level form is led by one word from a disjoint set, so ordered choice was encoding a dispatch as backtracking, and both of its failure modes followed from that. `parse_keyword` rejects only *after* consuming the identifier, so all nine alternatives failed at one offset and `Parser::or`'s furthest-failure tie-break kept the earliest — every unrecognized head blamed `mod`, confidently and wrongly. And because the arms failed recoverably, the item loop dropped the diagnosis entirely and a library reported `Expected 'end-of-file'` against its first column, for a malformed `use`, `satisfy` or `let` alike. Reading the head once makes the arm that owns the error the arm that produced it, which is a property of the structure rather than of which alternative happened to read furthest.

Commitment is stated at the dispatch rather than derived from depth because depth is what was already wrong: the furthest failure is not the same as the right one, and preferring it is what selected `mod`. `commit` is the dual of `catch` and exists for this shape. Both are kept, against the instinct that a library should plant one foot, because the position `catch` alone occupies is a known-bad one for error quality — Parsec ships `try` alone and [elm/parser](https://github.com/elm/parser/blob/master/comparison.md) names its consequence exactly: *"`try` often leads to 'bad commits' where your parser fails in a very specific way, but you then backtrack to a less specific error message."* That is the defect this decision records. Elm ships `backtrackable` and `commit` as the primitive pair and derives `try` from them; `nom` plants the opposite foot, backtracking by default with `cut` to commit and no way to undo it. Curios's default is commit-on-progress, so `catch` is Parsec's `try`, and `commit` is what bounds it.

The head is read *raw*, without the trailing whitespace an identifier normally consumes, so an unrecognized one is reported against the word rather than against wherever the whitespace after it ended — for a one-word line, the next line or end of input. Each arm consumes that whitespace itself.

**Rejected.** *Reserving `concept`, `satisfy` and `test`*, which would make all nine commit and the rule uniform. `syntax.md` states their contextuality three times as normative language reference, and `is_keyword` is not the parser's alone: `curios-utilities` owns it so `curios-text` refuses a keyword in a path and `curios-package` refuses one as a package name, so reserving `test` would forbid a `test` module and a `test` package. The corpus would migrate for free today, which is what makes it tempting and why the documented decision outranks the current corpus.

*Furthest-failure tracking in `curios-parse`*, threading the deepest error through the combinators and reporting it at the top. It treats depth as the selector, which is the mechanism that produced the `mod` blame, and it would spread that failure mode to the WAT parser, which shares the crate.

*An offset threshold* — trusting the item error only when it lands past the first token. It fixes a bare identifier and leaves `satisfy` blaming `mod`, and it is a rule with no justification beyond the case that prompted it.

*Committing on all nine in modules and five in entrypoints*, which a module's lack of a tail would permit. It needs a flag threaded through `parse_top_item` and into nested `mod` bodies, and it delivers the message the module's leftover-input path already delivers.
