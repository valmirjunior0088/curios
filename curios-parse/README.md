# curios-parse

The parser combinator DSL behind both the `.crs` surface grammar (`curios-text`) and the WAT parser (`curios-wasm`): single-use `Parser` actions, freely backtracking ordered choice that an alternative stops with `commit`, packrat memoization, recovery past a committed failure, and byte-offset errors rendered as caret snippets. Each combinator's contract belongs to the crate rustdoc; why this and `curios-print` are two crates is `curios-print/README.md`'s decision.

## Design

### A parser is a single-use `FnOnce`

**Decision.** `Parser<'a, A>` is a boxed `FnOnce` from an input position to a value and the rest of the input, or an error, so the repetition combinators — `many0`, `sep_by0` and their siblings — take parser-building closures rather than parsers.

**Rationale.** `FnOnce` lets combinators move captured values into results without cloning; a parser cannot run twice, so every iteration builds a fresh one.

### Choice backtracks until an alternative commits

**Decision.** `or` tries its second alternative whenever the first failed, however much input it read. `commit` marks a failure as the diagnosis, and an alternative that commits stops the choice on either side; `uncommit` takes a commitment back, for a caller that may legitimately re-read the same text. When neither alternative committed, the error that got further into the input is reported.

**Rationale.** An alternative knows when it has read the prefix that discriminates it, and nothing else does: `parse_func` has read `(params) =>` and owes the body that follows, while a grammar with shared prefixes — WAT's `(keyword …` forms, a Curios tuple against a parenthesized term — must probe past the `(` and still yield. Commitment is asked for rather than inferred from consumption, so the two cannot disagree, and the offset heuristic decides only between two failures that are both guesses. A default that commits on progress is known-bad for error quality: Parsec ships `try` alone, and [elm/parser](https://github.com/elm/parser/blob/master/comparison.md) names the consequence — *"`try` often leads to 'bad commits' where your parser fails in a very specific way, but you then backtrack to a less specific error message."* Elm ships `backtrackable` and `commit` as the primitive pair and derives `try` from them; `nom` backtracks by default with `cut` to commit and no way to undo it, which is this crate's default with `uncommit` beside it. Each `uncommit` marks a real boundary — a speculative alternative invoking the term grammar contains what that grammar commits to — and one site carries it; a second would be a reason to ask whether the grammar shares too much.

**Rejected.** Furthest-failure tracking threaded through the combinators and reported at the top, which treats depth as the selector: the furthest failure is not the right one, and it blames `mod` for every unrecognized top-level head.

### Memoization is packrat, keyed by nonterminal and offset

**Decision.** `memoize(key, parser)` caches one nonterminal's result per start offset in a thread-local table `run_parser` clears on entry and on exit.

**Rationale.** The term grammar probes one position through several overlapping alternatives — a `(` is tried as a dependent function type, a non-dependent one, a lambda, then parentheses — so without memoization the grammar is exponential. Straight packrat is sound because the memoized parsers are pure functions of the offset, with no symbol table that could make one input parse differently. Clearing on exit keeps the table's `Rc`-backed entries from dropping a deep tree at thread teardown, where the guard page is all the stack left.

### A repetition can recover past a committed failure

**Decision.** `recover` is `many0` with one more outcome: a committed failure is kept in the list as `Recovered::Broken`, with the span the loop skipped, and parsing resumes where the caller's `anchor` says an item could next begin; an uncommitted failure ends the loop, so a tail grammar still gets its turn. `tagging` lets an alternative name what its failure was about, the outermost name winning, and `raise` fails with an error already in hand.

**Rationale.** Recovery is commitment read one level up: a committed failure is an item that was there and broke, an uncommitted one the loop ending. The anchor is the caller's, since only the grammar knows what a line beginning an item looks like; the crate owns the loop and its progress guarantee.

**Rejected.** Resuming wherever the deepest alternative stopped, depth again as the selector; recovering inside `many0` itself, which would make every argument, field and case list swallow a failure its enclosing grammar owes a diagnosis for.

### Reading control flow is temporary instrumentation

**Decision.** What this crate carries permanently is one span per parse. A question about which alternative ran, or where a commitment went, is answered by adding `curios_profile::note!` to `or`, `commit` and `uncommit` — the site from `Location::caller()` under `#[cfg_attr(feature = "profile", track_caller)]`, and the error's offset and message — capturing with `curios_profile::trace`, and removing the notes again.

**Rationale.** A commitment that escapes reads as a `commit` row with no `uncommit` after it, an alternative that never ran as the short-circuit that skipped it. The notes cost about five rows per byte of input, against eleven rows for a whole 52 KB module through the permanent spans, so leaving them in would make every profile mostly parser chatter.
