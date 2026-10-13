# A program reads text and may fail

Working specification for the three steps a first program takes past printing: reading a line as text, reading a file as text, and ending in a failure. `/std` hands each over as bytes or as a `Try`, the program's final term is judged at `Io({})`, and no concept joins two `Str`, so a program that reads a number and prints it is three nested matches, and one that reads a file cannot be written at the top level at all.

It is independent of every other spec.

## What this builds on

- **Standard input is read as bytes.** `/std/read()` is `Io/read_line(Io/stdin)`, an `Io(Option(Bytes))` (`curios-std/src/lib.crs`, `Io.crs`). `Str/of_bytes` answers `Result(Nat, Str)`, the text or the length of the longest prefix that is text, and `Nat/of_str` an `Option(Nat)`.
- **A file is read as bytes, in a `Try`.** `File/read_all` is a `Try(Io, Io/Error, Bytes)` (`curios-std/src/File.crs`), as every host operation that can fail is ([A fallible operation returns `Try`, and `!` lifts along declared edges](../../design/standard-library/a-fallible-operation-returns-try-and-bang-lifts-along-declared-edges.md)).
- **A program is judged at one closed type.** The entry states `Io({})` before elaboration, the body is checked against it, the kernel rechecks it and erasure seals at it (`curios-pipeline/src/compile.rs`). The comment there gives the reason it is closed: `Io(?T)` "would need a metavariable minted before the elaboration context exists, which would leave the contract a post-hoc head test on the inferred type".
- **An action is lifted into its region along a declared edge.** An `Io` action sequences in a `Try` region over `Io`, and a region's tail is lifted by the same read where its head is a declared name ([`documentation/syntax.md`](../../syntax.md)). There is no edge from `Try` into `Io`: a failure has nowhere to go there.
- **`Io/Error` is shown.** `satisfy Show(Error)` (`curios-std/src/Io/Error.crs`) spells each case, and `/std/proc/exit` ends a program with a status.
- **`+` dispatches through `Add`, which states no law.** `pub concept Add(A: Type): pub Type { add(A, A) -> A }` (`curios-std/src/ops.crs`). `Str/concat` joins two, `Str/flatten` and `Str/join` a list, and `Fmt/render` builds one from a format.

## The gap

Probed at `d5046c5cc`.

- **A line of text.** `let line = read()!; match line | some(bytes) => match Nat/of_str(Str/trim(bytes)) …` is refused as a mismatch of `/std/Bytes` against `Str`; what compiles is a match on the `Option`, a match on `Str/of_bytes`'s `Result`, and a match on `Nat/of_str`'s `Option`.
- **A file's text.** `let text = File/read_all(Path/of_str("notes.txt"))!; print(text)` as a program is refused the same way, and once the bytes are decoded it is refused again: the `!` sequences a `Try` in the program's `Io` region.
- **A failure at the end.** A function answering `Try(Io, Io/Error, {})`, called as the program's final term, is refused as `no witness of /std/Monad/Lift((x) => Try(Io, Io/Error, x), Io) found`, `needed to sequence a (x) => Try(Io, Io/Error, x) action in this Io region`. What compiles is `Try/run` and a match on its `Result`, in every program that opens a file.
- **Two strings.** `"Hello, " + name + "!\n"` is refused as `no witness of /std/ops/Add(Str) found`, and names no function that joins them.

## Prior art

- **Rust's second chapter is this program, and it is three steps there too**: `read_line` into a `String`, `trim`, and `parse` under a `match` on its `Result` ([The Rust Programming Language, chapter 2](https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html)). What Rust's has that the gap lacks is text at the first step.
- **Rust's `main` may answer a `Result`**, and "the executable will exit with a value of `0` if `main` returns `Ok(())` and will exit with a nonzero value if `main` returns an `Err` value"; it is admitted through a trait, `std::process::Termination`, which any type `main` answers implements ([chapter 9](https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html)). That is the third shape of the first open question.
- **A read that meets the end of its input is among what novices hit most**: `EOFError: EOF when reading a line` is the third most frequent report in a large body of novice Python (Pritchard, [Frequency distribution of error messages](https://doi.org/10.1145/2846680.2846681), PLATEAU 2015). `read()`'s `none` is that case, already in the type.

## Decisions

None is settled. What the tree's rules already fix:

- **No value is invented where a proof belongs** ([`curios-std/README.md`](../../../curios-std/README.md#std-invents-no-value-where-a-proof-belongs-and-text-is-addressed-by-position)): bytes that are not text are answered as a failure, never as text with a stand-in character.
- **A name that exists is kept**: an operation `/std` already computes takes no second spelling.

## Open questions

- **What a program is judged at.** Three shapes. A `/std` function from `Try(Io, Io/Error, {})` to `Io({})` that writes the failure to standard error and exits non-zero, with the entry as it is: the narrowest, and every fallible step of a program then lives inside a function the tail hands to it. The entry's one type becoming `Try(Io, Io/Error, {})`: still one closed type stated before elaboration, an `Io` tail headed by a declared name is lifted into it by the rule that lifts any tail, a fallible step is sequenced with `!` at the top level, and a failure that reaches the entry is reported and the program exits non-zero. Or an entry dispatched through a `/std` concept, as `test` goes through `/std/Test` and as Rust's `main` goes through `Termination`: the tail's type would have to be inferred before its contract is stated, which is the head test the entry's comment rules out. Recommended, and not settled: the second, probed before it is taken. Not read: what a tail that is no declared name's call costs — it is not lifted on its own, so a program that closes with one is refused where it compiled — how many programs of `programs/` and of the tests close so; and what a program that never fails pays at run time for a `Try` at its root, which [What a compiled program still pays at run time](../compilation/206-what-a-compiled-program-still-pays-at-run-time.md) would hold.
- **What a text read answers where the bytes are not text.** A `Try(Io, Io/Error, Option(Str))`, the failure a case of `Io/Error`, composes with `!` beside every other read; `Io(Option(Result(Nat, Str)))` keeps `Str/of_bytes`'s answer and nests as the gap shows. Recommended: the first, under the name the bytes read has with its grain stated, so `/std` holds one read of each grain. `Io/Error`'s cases are the host's statuses and `other(Nat)`, so none says the bytes were not text: the read takes a case, or answers under an error of its own.
- **Whether `Str` joins by `+`.** `Add` states no law, so a witness is one declaration. Against it: `+` at every other carrier commutes, and the carriers' algebra decides sums in conversion, where a `Str`'s would be an opaque call. Recommended: no witness, and the refusal of `+` at `Str` names `Str/concat` and `Fmt/render`, which is [A refusal names the form it met](../surface/200-a-refusal-names-the-form-it-met.md)'s to word.

## Stages

1. **Text is read as text.** A line of standard input and a whole file, each at the type the second open question settles. Check: tests in `curios-std/src/tests/` over the mock host — a line, the end of input, bytes that are not text — and a program that reads a number and prints its double, with one match.
2. **A program may end in a failure**, as the first open question settles. Check: a program whose final term opens a file that is absent ends with the failure spelled on standard error and a non-zero status; every program of `programs/` and every entry the tests compile is judged as it was, or is listed with what it now writes.
3. **The reports name the way out.** A `Try` sequenced in an `Io` region and a `+` at `Str` each say what is written instead. Check: the two fixtures of the gap, rewritten.

## Verification

`cargo xtask workspace lint` and `cargo xtask workspace format` after each stage, with `curios-std`'s tests by name; the second stage changes what the pipeline judges an entry at, so it takes the whole workspace's tests once, and names the run-time figure it may have moved without retaking it.

## Rejected

- **A read that answers text and replaces what is not.** It invents a value.
- **An edge from `Try` into `Io`.** It would let a failure be sequenced where nothing can receive it, which is what the `Try` exists to refuse.

## Completion and retirement

The library's doc comments state each read's grain and what it answers; `documentation/syntax.md`'s "Declarations and modules" and "Host effects and `Io`" state what a program is judged at, if it moves; the tests hold the behaviour. The line in `documentation/roadmap.md` becomes a checked summary, and this file is deleted.
