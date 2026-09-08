# curios-parse

The parser combinator DSL behind both the `.crs` surface grammar (`curios-text`) and the WAT parser (`curios-wasm`): single-use `Parser` actions, freely backtracking ordered choice that an alternative stops with `commit`, packrat memoization, and byte-offset errors rendered as caret snippets. Each combinator's contract belongs to the crate rustdoc. Why this and `curios-print` are two crates rather than two modules of one is `curios-print/README.md`'s decision.

## Design

### A parser is a single-use `FnOnce`

**Decision.** `Parser<'a, A>` is a boxed `FnOnce` from an input position to a value and the rest of the input, or an error. The repetition combinators — `many0`, `sep_by0` and their siblings — therefore take parser-*building* closures rather than parsers.

**Rationale.** Being `FnOnce` lets combinators move captured values into results without cloning. The cost is that a parser cannot be run twice, which is why every iteration builds a fresh instance.

### Choice backtracks until an alternative commits

**Decision.** `or` tries its second alternative whenever the first failed, however much input it read. `commit` marks a failure as the diagnosis, and an alternative that commits stops the choice on either side; `uncommit` takes a commitment back, for a caller that may legitimately re-read the same text. When neither alternative committed, the error that got further into the input is reported.

**Rationale.** An alternative knows when it has read the prefix that discriminates it, and nothing else does: `parse_struct_pattern` reads `Name {` and owes a missing `}`, while a grammar with shared prefixes — WAT's `(keyword …` forms, or a Curios tuple against a parenthesized term — must probe past the `(` and still yield. Commitment is asked for rather than inferred from consumption, so the two cannot disagree, and the offset heuristic decides only between two failures that are both guesses.

`uncommit` is the rarer half and each use marks a real boundary: a speculative alternative that invokes the term grammar contains what that grammar commits to, since the same text is about to be read another way. Three sites carry it, and a fourth would be a reason to ask whether the grammar is sharing too much rather than to write it.

### Memoization is packrat, keyed by nonterminal and offset

**Decision.** `memoize(key, parser)` caches one nonterminal's result per start offset in a thread-local table that `run_parser` clears on entry and again on exit.

**Rationale.** The term grammar probes one position through several overlapping alternatives — a `(` is tried as a dependent function type, a non-dependent one, a lambda, then parentheses — so without memoization each retry re-parses the whole nested subterm and the grammar is exponential. Straight packrat is sound because the memoized parsers are pure functions of the offset: parsing carries no symbol table that could make the same input parse differently. The table is cleared on the way out as well as in because its `Rc`-backed entries would otherwise drop a deep tree at thread teardown, where the guard page is all the stack that is left.
