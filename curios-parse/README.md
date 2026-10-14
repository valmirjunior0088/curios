# curios-parse

The parser combinator DSL behind both the `.crs` surface grammar (`curios-text`) and the WAT parser (`curios-wasm`): single-use parsers that are functions of the input, freely backtracking ordered choice that an alternative stops with `commit`, packrat memoization, recovery past a committed failure, and byte-offset errors rendered as caret snippets. Each combinator's contract belongs to the crate rustdoc; why this and `curios-print` are two crates is `curios-print/README.md`'s decision.

## Design

### A parser is a function of the input

**Decision.** `Parser<'a, A>` is a trait every `FnOnce(ParserState<'a>) -> Parsed<'a, A>` implements: a nonterminal is a plain function of the input, a combinator returns the closure it is, and a parser runs once, so the repetition combinators — `many0`, `sep_by0` and their siblings — take what hands them a parser for each iteration, a function that builds one or a parser that is `Copy`, as a nonterminal is. Where a grammar chooses at run time among parsers of different shapes, each is `boxed`.

**Rationale.** Nothing is allocated to run a grammar but at a box a choice asks for: a combinator a grammar function builds on the way is a value on that function's stack, and a nonterminal that names itself, or another that names it back, is a function calling a function, so recursion needs no box and no return type states a shape. A parser that runs once moves what it captured into its result without cloning.

**Rejected.** A boxed `FnOnce` for every application of a combinator that captures anything, which a grammar function built again at every position it was tried at, most of them for an ordered choice to drop unrun. Parsers built once and shared behind `Rc<dyn Fn>`, which keeps a box at every nonterminal and clones whatever a parser hands on of what it captured. Nonterminals written by hand as functions of the input, which restate commitment and the offset tie-break at every site.

### A combinator takes a parser or what builds one

**Decision.** Every combinator that holds a parser it may not run takes an `IntoParser`: a `Parser` in hand, a nonterminal among them, or a function that builds one, which the combinator calls where the parser would run and nowhere else. A grammar hands a combinator its nonterminal, `term.and_drop(parse_whitespace)`, and an operand it would build for nothing behind a closure, `open.or(|| parse_literal("["))`.

**Rationale.** An ordered choice reaches a fraction of what it is handed: the alternatives after the one that matched, and whatever follows a token that was not there. A function handed over is built only once its combinator gets that far.

**Rejected.** An operand that is always a function: a parser made of values in hand, a literal or a prefix already parsed, would be wrapped in a closure that defers nothing.

### A grammar function that takes arguments returns one closure

**Decision.** A grammar function that takes arguments answers `impl Parser`, and its body is one closure of the input that builds its chain where it runs: `move |state| take_exact(expected).and_drop(parse_whitespace).parse(state)`.

**Rationale.** A combinator's type is the chain it was built from, and a closure's is what it captures. A function returning its chain returns the chain's whole type, and a function over such a function nests it into the type of every caller, which the compiler's checks of each caller pay for, the borrow checker's most: returned as their chains, the struct and concept members over `declared` over `parse_literal` make the borrow checking of `curios-text` most of its build.

**Rejected.** Boxing what such a function returns, an allocation at every application again. A check of the source refusing a function written as its chain, which reads Rust text for what a type says.

### A failure is rendered where it is reported

**Decision.** A `ParserError` holds what its message is made of, and `report` renders it: a message the grammar wrote, borrowed where it is a constant, or the literal a primitive did not find, with what stood in its place read off the source when a report asks.

**Rationale.** A grammar tries a literal wherever an alternative could begin, so nearly every failure is backtracked and dropped unread. Formatted where it is raised, each of them allocates the text it obtained and the sentence around it.

**Rejected.** A message built by a closure the error carries, which boxes what the enumeration states in a word.

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
