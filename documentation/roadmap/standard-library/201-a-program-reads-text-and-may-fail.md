# A program reads text and may fail

Working specification for the three steps a first program takes past printing: reading a line as text, reading a file as text, and ending in a failure. `/std` hands each over as bytes or as a `Try`, the term that closes a program is judged at `Io({})`, and no concept joins two `Str`, so a program that reads a number and prints it is three nested matches, and one that reads a file cannot be written at the top level at all.

It reads a line through [a stream's reader](../../../curios-std/README.md#a-piece-of-a-stream-is-cut-from-the-bytes-its-reader-holds). It is independent of every other spec.

## What this builds on

- **Standard input is read as bytes, in a `Try`.** A line is `Io/Reader/line` of a reader the program makes over `Io/stdin`, a `Try(M, Io/Error, Option(Bytes))` (`curios-std/src/Io/Reader.crs`). `Str/of_bytes` answers `Result(Nat, Str)`, the text or the length of the longest prefix that is text, and `Nat/of_str` an `Option(Nat)`.
- **A file is read as bytes, in a `Try`.** `File/read_all` is a `Try(Io, Io/Error, Bytes)` (`curios-std/src/File.crs`), as every host operation that can fail is ([A fallible operation returns `Try`, and `!` lifts along declared edges](../../design/standard-library/a-fallible-operation-returns-try-and-bang-lifts-along-declared-edges.md)).
- **A program is judged at one closed type.** The entry states `Io({})` before elaboration, the body is checked against it, the kernel rechecks it and erasure seals at it (`curios-pipeline/src/compile.rs`).
- **A `!` reads its region from the type it is checked against**, never from the action it sequences, and a region whose type is an unsolved metavariable waits for it (`elaborate_bang`, `curios-elab/src/elaborate/binding.rs`). The same decision rejects "letting the action elect the monad where the region is flexible".
- **An action is lifted into its region along a declared edge.** An `Io` action sequences in a `Try` region over `Io`, and a region's tail is lifted by the same read where its head is a declared name ([`documentation/syntax.md`](../../syntax.md)). There is no edge from `Try` into `Io`: a failure has nowhere to go there.
- **A form opts into syntax through a concept.** An infix operator is resolved through its `/std/ops` concept, a `!` through `/std/Monad`, and a test program's tail is `Test/main` over the unit's tests, written by the compiler through a `SyntaxRegistry` slot (`curios-elab/src/test_tail.rs`).
- **`Io/Error` is shown.** `satisfy Show(Error)` (`curios-std/src/Io/Error.crs`) spells each case, and `/std/proc/exit` ends a program with a status.
- **`+` dispatches through `Add`, which states no law.** `pub concept Add(A: Type): pub Type { add(A, A) -> A }` (`curios-std/src/ops.crs`). `Str/concat` joins two, `Str/flatten` and `Str/join` a list, and `Fmt/render` builds one from a format.

## The gap

Probed at `d5046c5cc`, the line of text at `5fbe62825`.

- **A line of text.** `let line = Io/Reader/line(input)!;` at a program's top level is refused as `no witness of /std/Monad/Lift((x2) => /std/Try(?, Io/Error, x2), Io) found`, the read a `Try` and the program an `Io`; what compiles is `Try/run`, a match on its `Result` and on the `Option` in it, a match on `Str/of_bytes`'s `Result`, and a match on `Nat/of_str`'s `Option`.
- **A file's text.** `let text = File/read_all(Path/of_str("notes.txt"))!; print(text)` as a program is refused the same way, and once the bytes are decoded it is refused again: the `!` sequences a `Try` in the program's `Io` region.
- **A failure at the end.** A function answering `Try(Io, Io/Error, {})`, called as the term that closes the program, is refused as `no witness of /std/Monad/Lift((x) => Try(Io, Io/Error, x), Io) found`, `needed to sequence a (x) => Try(Io, Io/Error, x) action in this Io region`. What compiles is `Try/run` and a match on its `Result`, in every program that opens a file.
- **Two strings.** `"Hello, " + name + "!\n"` is refused as `no witness of /std/ops/Add(Str) found`, and names no function that joins them.
- **A region nothing states.** `let main(@T: Type, program: () -> T) -> Io({}) = print("x"); main(() => let _ = print("a")!; print("b"))` is given no answer: under a 4 GB cap the compiler aborts in under three seconds, allocating in `drain_parked`, `retry_checking`, `elaborate_bang`, `park_checking`. A parked check is retried under `blocked_on_metavar` (`curios-elab/src/typing.rs`), which knows a lambda, a tuple and a numeral; a `!` parks on `region_flex`, so a retried one is elaborated, parks itself again under a fresh placeholder and fills the old placeholder with the new, which the drain reads as a solution landed. Probed at `d4246fb98`.

## Prior art

- **Rust's second chapter is this program, and it is three steps there too**: `read_line` into a `String`, `trim`, and `parse` under a `match` on its `Result` ([The Rust Programming Language, chapter 2](https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html)). What Rust's has that the gap lacks is text at the first step.
- **Rust's `main` answers any type a trait admits.** [`Termination`](https://doc.rust-lang.org/std/process/trait.Termination.html), "A trait for implementing arbitrary return types in the `main` function", has one method, `fn report(self) -> ExitCode`, and is implemented for `()`, `!`, `ExitCode`, `Infallible` and `Result<T, E>` where `T: Termination` and `E: Debug`; "the executable will exit with a value of `0` if `main` returns `Ok(())` and will exit with a nonzero value if `main` returns an `Err` value" ([chapter 9](https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html)).
- **Lean admits three types by name**: "`main : IO Unit` is used for simple programs that cannot read their command-line arguments and always return exit code 0", `IO UInt32` "for programs without arguments that may signal success or failure", and `List String → IO UInt32` ([Functional Programming in Lean](https://lean-lang.org/functional_programming_in_lean/Hello___-World___/Summary/)).
- **Haskell admits any payload and drops it**: `main` "must be a computation of type `IO τ` for some type `τ`", and "its result (of type `τ`) is discarded" ([Haskell 2010, chapter 5](https://www.haskell.org/onlinereport/haskell2010/haskellch5.html)).
- **Zig's `main` may answer an error**, `pub fn main() !void`, and "Return an error from main" is one of the ways an error return trace is printed, opening with the error's name ([Zig 0.15.1](https://ziglang.org/documentation/0.15.1/#Error-Return-Traces)).
- **A read that meets the end of its input is among what novices hit most**: `EOFError: EOF when reading a line` is the third most frequent report in a large body of novice Python (Pritchard, [Frequency distribution of error messages](https://doi.org/10.1145/2846680.2846681), PLATEAU 2015). `Io/Reader/line`'s `none` is that case, already in the type.

## Decisions

Settled 2026-10-13.

- **What closes a program is performed through a concept.** `/std/Program` declares `pub concept Program(T: Type): pub Type { perform(T) -> Io({}) }`, and the compiler writes `Program/perform(t)` around each tail of an authored entry's final term, as it writes `Test/main` around a test program's tests. The tail's type is the call's implicit, read by ordinary inference, and the witness is resolved as an operator's is.
- **The top level stays an `Io({})` region.** The entry's stated type does not move, so a `!` at the top level sequences in `Io` exactly as it does today. The dispatch is at the tails because a `!` reads its region from the type it is checked against: a final term dispatched whole would be checked against the call's unsolved implicit, and every program that sequences at its top level would be a region nothing states.
- **The tails of a term** are reached through a local `let` to its body, through a `!` to its continuation, and through a `match` that writes no motive to each of its arms. Anything else is a tail. A `match` with a written motive is one term of the type it states, and a written goal is left as it stands, so `?` at a program's end still reports `Io({})`.
- **Three witnesses.** `Io({})` is the program itself. `Try(Io, E, {})`, under `use Show(E)`, is run: a failure is written to standard error with a newline and the program exits with status 1. `Test` is reported as `Test/main` reports one, and the program exits with status 1 where it did not hold. A witness is keyed by its type's head, one per head program-wide, so `Io` at another payload and a `Try` over another base are refused, and a function type, having no head, is no program.
- **The runtime is untouched.** `perform` answers `Io({})` and a failing witness ends through `/std/proc/exit`, so `curios test`'s tail stays `Test/main` and no exit is read as a verdict.
- **A program that may fail is declared and named**: `let main: Try(Io, Io/Error, {}) = …;`, closed with `main`.
- **A text read answers a `Try` whose failure is a case of `Io/Error`**: `not_text(Nat)`, carrying the length of the longest prefix that is text, which is what `Str/of_bytes` answers. The reads are `Io/Reader/line_text` and `File/read_all_text`, each beside the bytes read it is named after.
- **`Str` takes no `Add` witness.** `+` at every other carrier commutes, and the carriers' algebra decides sums in conversion, where a `Str`'s would be an opaque call. The refusal of `+` at `Str` names `Str/concat` and `Fmt/render`, as [a form the grammar does not have is refused by name](../../design/tools/a-diagnostic-spells-what-its-reader-can-write.md).
- **A region nothing states is refused, in bounded time.** A retried `!` is asked the question that parked it and waits under the placeholder it has; the drain then reports the region as undetermined, at the `!`, which is what [`documentation/syntax.md`](../../syntax.md) states.

What the tree's rules already fix:

- **No value is invented where a proof belongs** ([`curios-std/README.md`](../../../curios-std/README.md#std-invents-no-value-where-a-proof-belongs-and-text-is-addressed-by-position)): bytes that are not text are answered as a failure, never as text with a stand-in character.
- **A name that exists is kept**: an operation `/std` already computes takes no second spelling.
- **No stage spells a prelude name**: what the compiler writes or names in a report is a `SyntaxRegistry` slot.

## The dispatch, probed

Counted. At `d4246fb98`, through the release compiler on standard input: the concept and the `Io({})` and `Try(Io, E, {})` witnesses declared in the probe itself, `let work: Try(Io, Io/Error, {})` a region that reads `absent.txt`, `let n: Nat = 3`, and `Program/perform` written by hand where the compiler is to write it.

| The program closes with | Answer |
| --- | --- |
| `let _ = print("a")!; perform(print("b"))` | prints `ab`, status 0 |
| `perform(Io/pure(()))` | status 0 |
| `let _ = print("a")!; perform(proc/exit(2))` | prints `a`, status 2 |
| `let _ = print("a")!; perform(work)` | prints `a`, `not_found` on standard error, status 1 |
| `perform(match n: (_) => Io({}) \| 0 => (let _ = print("a")!; print("b")) \| _ => (let _ = print("c")!; print("d")) end)` | prints `cd`, status 0 |
| `match n \| 0 => (let _ = print("a")!; perform(print("b"))) \| _ => (let _ = print("c")!; perform(print("d"))) end` | prints `cd`, status 0 |
| `match n \| 0 => perform(print("x")) \| _ => perform(work) end` | `not_found`, status 1 |
| `perform(match n \| 0 => (let _ = print("a")!; print("b")) \| _ => (let _ = print("c")!; print("d")) end)` | no answer: the region nothing states |
| `perform(Io/pure(3))` | refused at the `3`, `inferred: Nat`, `expected: {}` |
| `perform(3)` | `no witness of Program(Nat) found`, `needed by 'Program/perform' for its 1st 'use' premise` |
| `perform(?)` | `no witness of Program(?) found`, where the goal alone reports `? : Io({})` |
| `perform(Monad/pure(()))` | `no witness of Monad(?) found`: its monad came from the expectation |

Judged at `Try(Io, Io/Error, {})` instead, 24 of the 26 programs of `programs/` are refused as written, each at a final `match` whose motive states `Io({})`.

## Stages

1. **A region nothing states is refused.** `retry_checking` re-parks a `!` whose region is still headed by an unsolved metavariable, under the placeholder it has. Check: the gap's program is refused with the region's report, at the `!`; a `match` with one arm a region and one a call compiles.
2. **The concept.** `/std/Program` and its three witnesses. Check: tests in `curios-std/src/tests/`, each witness called by hand.
3. **What closes a program is performed.** One registry slot; the pipeline wraps the tails of an authored entry where it states the entry's type; a missing witness at a program's tail is reported as the term that closes the program and its type. Check: a program closing in a `Try` over an absent file ends with the failure on standard error and status 1; every program of `programs/` and every entry the tests compile is judged as it was, or is listed with what it now writes.
4. **Text is read as text.** `Io/Error/not_text`, `Io/Reader/line_text`, `File/read_all_text`. Check: tests over the mock host — a line, the end of input, bytes that are not text — and a program that reads a number and prints its double, with one match.
5. **The reports name the way out.** A `Try` sequenced in an `Io` region and a `+` at `Str` each say what is written instead. Check: the two fixtures of the gap, rewritten.

## Verification

`cargo xtask workspace lint` and `cargo xtask workspace format` after each stage, with the stage's tests by name; the third stage changes what the pipeline writes into every program, so it takes the whole workspace's tests once, and names the figures it may have moved without retaking them: what a contestant's build weighs in the bench's next reading, and the counts the census tests report.

## Rejected

- **A read that answers text and replaces what is not.** It invents a value.
- **An edge from `Try` into `Io`.** It would let a failure be sequenced where nothing can receive it, which is what the `Try` exists to refuse.
- **The entry's one type becoming `Try(Io, Io/Error, {})`.** Every program that states `Io({})` on its final `match` is refused, and a program's own error type has no place in it.
- **A `/std` function the tail hands a `Try` to.** Every fallible program then closes with the same call, which is what a concept writes once.
- **The final term dispatched whole.** Its own `!` would be a region nothing states.
- **A region that takes its monad from its first action or its tail** where nothing states it: two readings of one form, and the first would follow the order of a region's parts.

## Completion and retirement

The library's doc comments state each read's grain and what it answers, and `/std/Program`'s what a witness is asked for; `documentation/syntax.md`'s "Declarations and modules" and "Host effects and `Io`" state what a program is judged at, `documentation/usage.md` the status a failed program exits with, and `curios-std/README.md` the decision with what it rejects; the tests hold the behaviour. The line in `documentation/roadmap.md` becomes a checked summary, and this file is deleted.
