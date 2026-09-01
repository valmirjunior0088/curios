# curios-parse

The parser combinator DSL behind both the `.crs` surface grammar (`curios-text`) and the WAT parser (`curios-wasm`): single-use `Parser` actions, ordered choice under progress-based commitment, packrat memoization, and byte-offset errors rendered as caret snippets. Each combinator's contract belongs to the crate rustdoc. Why this and `curios-print` are two crates rather than two modules of one is `curios-print/README.md`'s decision.

## Design

### A parser is a single-use `FnOnce`

**Decision.** `Parser<'a, A>` is a boxed `FnOnce` from an input position to a value and the rest of the input, or an error. The repetition combinators — `many0`, `sep_by0` and their siblings — therefore take parser-*building* closures rather than parsers.

**Rationale.** Being `FnOnce` lets combinators move captured values into results without cloning. The cost is that a parser cannot be run twice, which is why every iteration builds a fresh instance.

### Choice commits on progress

**Decision.** `or` tries its second alternative only when the first failed *without consuming input*; a failure past the choice point is fatal and owns the diagnosis. `catch` downgrades a failure to recoverable, for alternatives that share a prefix; `commit` upgrades one, for a keyword-dispatched body whose head has already been eaten. When both alternatives fail recoverably, the error that got further into the input is reported.

**Rationale.** A failure after progress means that alternative was the right branch, so its error — not a generic complaint at the choice point — is what the reader needs, and the error that got further is almost always the more informative one. The escape hatches exist because a grammar with shared prefixes, such as WAT's `(keyword …` forms, would otherwise die on the first probe that consumed the `(`.

### Memoization is packrat, keyed by nonterminal and offset

**Decision.** `memoize(key, parser)` caches one nonterminal's result per start offset in a thread-local table that `run_parser` clears on entry and again on exit.

**Rationale.** The term grammar probes one position through several overlapping alternatives — a `(` is tried as a dependent function type, a non-dependent one, a lambda, then parentheses — so without memoization each retry re-parses the whole nested subterm and the grammar is exponential. Straight packrat is sound because the memoized parsers are pure functions of the offset: parsing carries no symbol table that could make the same input parse differently. The table is cleared on the way out as well as in because its `Rc`-backed entries would otherwise drop a deep tree at thread teardown, where the guard page is all the stack that is left.
