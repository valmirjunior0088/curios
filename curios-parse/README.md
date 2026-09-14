# curios-parse

The parser combinator DSL behind both the `.crs` surface grammar (`curios-text`) and the WAT parser (`curios-wasm`): single-use `Parser` actions, freely backtracking ordered choice that an alternative stops with `commit`, packrat memoization, and byte-offset errors rendered as caret snippets. Each combinator's contract belongs to the crate rustdoc. Why this and `curios-print` are two crates rather than two modules of one is `curios-print/README.md`'s decision.

## Design

### A parser is a single-use `FnOnce`

**Decision.** `Parser<'a, A>` is a boxed `FnOnce` from an input position to a value and the rest of the input, or an error. The repetition combinators — `many0`, `sep_by0` and their siblings — therefore take parser-*building* closures rather than parsers.

**Rationale.** Being `FnOnce` lets combinators move captured values into results without cloning. The cost is that a parser cannot be run twice, which is why every iteration builds a fresh instance.

### Reading control flow is temporary instrumentation

**Decision.** What this crate carries permanently is one span per parse. A question about *which* alternative ran, or where a commitment went, is answered by adding `curios_profile::note!` to `or`, `commit` and `uncommit` — the site from `Location::caller()` under `#[cfg_attr(feature = "profile", track_caller)]`, and the error's offset and message — capturing with `curios_profile::trace`, and taking the notes out again.

**Rationale.** Those notes answer the question completely: a commitment that escapes reads as a `commit` row with no `uncommit` after it, and an alternative that never ran reads as the short-circuit that skipped it. They also cost roughly five rows per byte of input, against the eleven rows a whole 52 KB module emits through the permanent spans, so leaving them in would mean every profile is mostly parser chatter. The span stays because it is bounded by parses rather than by nodes; the notes go because they are bounded by neither.

### Choice backtracks until an alternative commits

**Decision.** `or` tries its second alternative whenever the first failed, however much input it read. `commit` marks a failure as the diagnosis, and an alternative that commits stops the choice on either side; `uncommit` takes a commitment back, for a caller that may legitimately re-read the same text. When neither alternative committed, the error that got further into the input is reported.

**Rationale.** An alternative knows when it has read the prefix that discriminates it, and nothing else does: `parse_struct_pattern` reads `Name {` and owes a missing `}`, while a grammar with shared prefixes — WAT's `(keyword …` forms, or a Curios tuple against a parenthesized term — must probe past the `(` and still yield. Commitment is asked for rather than inferred from consumption, so the two cannot disagree, and the offset heuristic decides only between two failures that are both guesses.

`uncommit` is the rarer half and each use marks a real boundary: a speculative alternative that invokes the term grammar contains what that grammar commits to, since the same text is about to be read another way. One site carries it, and a second would be a reason to ask whether the grammar is sharing too much rather than to write it.

### Memoization is packrat, keyed by nonterminal and offset

**Decision.** `memoize(key, parser)` caches one nonterminal's result per start offset in a thread-local table that `run_parser` clears on entry and again on exit.

**Rationale.** The term grammar probes one position through several overlapping alternatives — a `(` is tried as a dependent function type, a non-dependent one, a lambda, then parentheses — so without memoization each retry re-parses the whole nested subterm and the grammar is exponential. Straight packrat is sound because the memoized parsers are pure functions of the offset: parsing carries no symbol table that could make the same input parse differently. The table is cleared on the way out as well as in because its `Rc`-backed entries would otherwise drop a deep tree at thread teardown, where the guard page is all the stack that is left.

### A repetition can recover past a committed failure

**Decision.** `recover` is `many0` with one more outcome: a committed failure does not abort the parse but is kept in the list as `Recovered::Broken`, with the span the loop skipped, and parsing resumes where the caller's `anchor` says an item could next begin. An uncommitted failure ends the loop as before, so a tail grammar still gets its turn. `tagging` lets an alternative name what its failure was about — the declaration whose head it had read — and the outermost name wins, so a failure inside an inline body is the enclosing item's. `raise` fails with an error already in hand, for a caller that reads one out of a recovered list and decides the whole parse must not survive it.

**Rationale.** Recovery is commitment read one level up: a committed failure is an item that was there and broke, an uncommitted one is the loop ending, and the same flag decides both, so no second notion of "was this an item" is introduced. The anchor is the caller's because only the grammar knows what a line that begins an item looks like; the crate owns the loop and the progress guarantee, nothing about any language. A terminator scan was rejected in the surface grammar's own record: it needs a lexer for strings, comments and nesting that a scannerless grammar does not have.

**Rejected.** Furthest-failure recovery, resuming wherever the deepest alternative stopped: depth is what the dispatch decision above already found to be the wrong selector. Recovering inside `many0` itself: every existing repetition — arguments, fields, cases — would then swallow a committed failure its enclosing grammar owes a diagnosis for.
