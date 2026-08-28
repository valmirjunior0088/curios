# Test harness: a test is a definition, and its unit is the program that runs it

## Status

Not started, and sequenced behind [auto-derive](auto-derive-spec.md): the harness reports every value through `Spell`, and the property rung needs the derivation seam that specification leaves open for a generator and a shrinker. The design was reached by a survey of test harnesses — Rust (`rustc --test`, libtest, nextest), Lean 4 (`#guard`, `#guard_msgs`, `decide`, Plausible, LSpec, `lake test`), Rocq (QuickChick, `Fail`), Isabelle (`quickcheck`, `nitpick`), ACL2s (`cgen`), Agda and Idris 2 (`refl` tests, `Test.Golden`, `idris2-hedgehog`), Dafny (`assert`/`expect`/`assume`, `{:test}`), Unison (`test>`, cached by dependency hash, `io.test`), Haskell (QuickCheck, Hedgehog, falsify, io-sim, inspection-testing), Hypothesis, proptest, Jane Street's expect tests, Zig, Roc, Go, Elixir, Julia, Swift Testing, pytest and Jest — and by reading the tree against it. **No program has yet been put to the compiler for this design.** Every claim below that rests on the compiler's answer names the file it was read from, and step 1 is the probe ledger that turns those readings into answers.

A landed step is replaced here by one checked line; its contracts move to their owners (`usage.md`, the owning module's rustdoc, `/std/Test.crs`, a decision file under `documentation/design/toolchain/`), and the design section is rewritten at the end to state what is left against what landed.

## Mission

`curios test` runs every test the governing package declares, and a test is an ordinary top-level definition beside the code it tests: a function of no parameters returning `/syn/Test`. The compiler registers such definitions as it registers witnesses — by their elaborated type, ignoring `pub` — and runs them by compiling **the unit that declares them** as a program whose final term the compiler wrote. A test that performs no host effect is a function of its unit, so its verdict is filed beside that unit's record and believed until the record no longer holds; a test that performs effects is run every time. A failure is reported as source text: the counterexample is spelled, and the report is something a reader can paste back into the program.

What is borrowed, and from where:

- From `rustc --test`: **the compiler collects the tests and synthesizes the program's entry.** A crate under test is compiled as itself with a generated `main` calling into libtest over the descriptors a compiler pass gathered (`rustc_builtin_macros/src/test_harness.rs`). Here the same two acts land at two stages — collection at elaboration, the tail in Core — and the tail is built below name resolution, so the `__test_reexports` chain and the hygiene mark that rustc needs to reach a private test have no counterpart.
- From Dafny's `assert`/`expect`/`assume` and Lean's `decide`/`#guard`/`native_decide`: **a ladder of trust, with the rungs visibly distinct in the source.** A proposition discharged by reduction is kernel-checked and budget-bounded; a `Bool` evaluated by the compiled program is neither; the two are different spellings, so no rung can be mistaken for the one above it.
- From Unison: **a pure test's verdict is cached by what it depends on**, and an effectful test is a separate, uncached tier. Here purity is read off the type — a `() -> Test` payload that is not an `Io` cannot perform an effect (`documentation/design/language/effects-are-descriptions-and-the-carrier-has-no-eliminator.md`) — and the address and record are the store's own (`documentation/soundness/admission-without-judgment/cached-verdicts.md`).
- From Roc and Zig: **tests live beside the code**, and cost nothing in `run`. Here the cost is removed by the erased optimizer's reachability prune rather than by a build mode: a definition nothing references is dropped (`curios-ersd/src/optimize/prune.rs`).
- From Hypothesis's example database and proptest's `proptest-regressions`: **a shrunk counterexample is persisted** — as source text through `Spell`, not as a seed or a serialized value, so it is reviewed in a diff and typechecked on the next build rather than replayed blindly.
- From Jane Street's expect tests: **an expected output is accepted from a diff**, for the `example` rung, behind an explicit `--accept`.
- From QuickChick and Hedgehog: **derived generators and shrinkers**, `Failed after N cases and M shrinks (D discards)`, and a report that shows values rather than a bare `false`. Shrinking is structural and derived, because the measured evidence (Evaluating Shrinking, 2026) says structural shrinking is usually faster and competitive, and integrated shrinking's bind cut-point is a known cost.
- From Lean's `#guard_msgs`, Rocq's `Fail` and Rust's `trybuild`: **a program that must not compile is a test**, and `wonder diagnostics` is already its oracle.
- From Julia's `@test_broken`: **a known failure that starts passing is a failure**, so a `broken` marker cannot rot.
- From nextest: **every test runs isolated** — here one Wasmtime instantiation each, which the runtime already makes cheap (`curios/src/tests.rs`, `Compiled`).
- Rejected, from Go, Rust, Zig and Unison: name-prefix discovery, attribute discovery, a `test` keyword, and a watch-expression form. Discovery is by type because names are identity only (`documentation/design/toolchain/one-naming-scheme-for-compiler-identities.md`) and syntax forms are closed (`documentation/design/language/syntax-forms-are-closed-semantics-extend-by-witness.md`).

## Steps

- [ ] 1. Probe the ground
- [ ] 2. `/syn/Test` and `/std/Test`, the library half
- [ ] 3. Registration by type, and the recorded sort
- [ ] 4. The synthesized tail, and a unit compiled as its own test program
- [ ] 5. `curios test`, isolation, and the interruptible engine
- [ ] 6. `wonder tests`
- [ ] 7. Verdicts filed beside the record, and `--failed`
- [ ] 8. The property rung: `Gen`, `Shrink`, seeds, shrinking, discards
- [ ] 9. `--accept` for `example` and `refuse`
- [ ] 10. Rewrite the design into what remains

Each step is one authorization and one commit, lands its tests before its mechanism, and runs the full gate at its end. 2 needs 1; 3 needs 2; 4 needs 3; 5 needs 4; 6 and 7 need 5 and are independent of each other; 8 needs 5 and the auto-derive landing; 9 needs 8; 10 needs everything. A defect found in 1 is a new step inserted before 2.

### 1. Probe the ground

**Inputs.** The ledger below, each row a program on standard input, put to the tree's compiler.

**Outputs.** The diff between what was read and what the compiler answers, row by row; and each design question below re-decided with the probe that decided it.

**Rule.** No implementation in this step. A defect is a new numbered step before 2.

### 2. `/syn/Test` and `/std/Test`, the library half

**Lands.** `/syn/Test.crs` declaring `Verdict` and `Test`, registered in `curios-prelude-archive/syn.crs`; `/std/Test.crs` as the facade plus the combinators under *The library*, registered in `std.crs`; a `Spell` witness for `Verdict`. No compiler change: the type is declared in `/syn` because step 3 emits its name, and landing it first lets its combinators be exercised as ordinary programs through `run`.

**Verification.** A `curios/src/tests/harness.rs` in the `run(source)` style: each combinator's verdict on a passing and a failing case, the report text of `equal` on two spelled values, `broken` inverting a verdict, `effect` sequencing a host read, and `Test/main`'s dispatch on a scripted argument through `MockHost::builder().args(…)`.

### 3. Registration by type, and the recorded sort

**Lands.** A `SyntaxRegistry` slot for `/syn/Test` (`curios-utilities/src/syntax.rs`, filled in `curios-prelude-archive/src/syntax.rs`, checked by the prelude build like every slot). `Module::tests: BTreeSet<Global>` beside `Module::witnesses` (`curios-core/src/module.rs`), filled by the elaborator when a definition's elaborated type is a function of no parameters whose result reduces to the `/syn/Test` head — the arm sits where `register_witness` is called from `elaborate_module_let` (`curios-elab/src/elaborate/module.rs`) and, like registration there, ignores `pub`: *"visibility governs the name, never table membership"* (`curios-elab/src/resolve.rs`, `register_witness`). A recorded sort on `Definition` — whether its declared type settled at `Prop` — written back the way `totality` is (`curios-core/src/module.rs`, `Definition::totality`), because `Context::checked` (`curios-elab/src/context.rs`) is dropped after elaboration and nothing on the finished module says which definitions are theorems. The archived prelude carries both fields; `/std` declares no tests, so its set is empty and the archive's replay test pins that.

**Verification.** Registration of a test in the entry module, in a private child module, and in a library unit reached through the store; a definition of type `Test` that is not a function is not registered and is reported at its declaration with the function form quoted; a `(x: Nat) -> Test` is not registered; the sort field on a `Prop`-typed and a `Type`-typed definition; the prelude's archive round-trips both fields.

### 4. The synthesized tail, and a unit compiled as its own test program

**Lands.** The tail synthesis under *The synthesized tail*, and the pipeline entry point that compiles one unit as a program: the unit lowered as a unit (`curios-text`'s `into_core_unit`), its items elaborated, the tail built in Core from `Module::tests` and checked against `Io({})` as every entry's tail is (`curios-pipeline/src/compile.rs`, `elaborate_and_zonk`), and the result carried through the same back half `compile_entrypoint` uses. An executable compiled this way has its authored tail replaced. Nothing about the surface language changes, and no file is written.

**Verification.** A library unit with three tests compiles to a program whose `wonder stage core-elab` shows the tail naming all three, private ones included; the same library compiled ordinarily has no body; an executable's authored tail is absent from its test program; a unit with no tests compiles to a program that runs nothing and exits 0; the kernel judges every test program, and the prelude compiled as a test program is a no-op (it declares none).

### 5. `curios test`, isolation, and the interruptible engine

**Lands.** The `test` subcommand (`curios/src/cli.rs`, `main.rs`): the governing package's library and each of its executables compiled as test programs, one instantiation per test through an `instantiate` that takes a deserialized module (`curios-runtime/src/engine.rs`, today private to `run_bytes`), the report under *Reporting*, the exit codes, filters, and epoch interruption on the shared engine with a per-test deadline (`--timeout`, its default stated in `curios --help`). Enabling epochs moves `engine_compatibility` (`curios-runtime/src/cranelift.rs`), so every stored payload misses once, and the launcher shares the engine and pays the epoch check too; that cost is measured before this lands and recorded beside `programs/README.md`'s instruments under its rule.

**Verification.** A package with a passing, a failing, a trapping, an exiting, a diverging and an effectful test: the six outcomes, the exit code, and a failing report carrying the spelled values; a private test in `mod tests` runs; `curios run` of the same package's executable neither runs nor links a test (`wonder stage ersd-optm` shows none, in the pattern of `threaded_record_allocates_nothing` in `curios/src/tests/codegen/churn.rs`); a filter naming no test exits nonzero naming the filter.

### 6. `wonder tests`

**Lands.** The `tests` query: every test and every theorem the governing package declares — its library, then each executable, as `wonder diagnostics` walks them (`curios/src/wonder/ask.rs`) — as records `{ path, kind }` read off `Module::tests` and the recorded sort, executing nothing and typing nothing (`curios/src/wonder.rs`'s contract). The record is the seam an editor's run-test lens reads; the server transport gains nothing here.

**Verification.** A listing per target form; a package with no tests lists nothing and exits 0; `usage.md`'s query table gains the row.

### 7. Verdicts filed beside the record, and `--failed`

**Lands.** A verdict of a `pure` or `property` test is filed beside its test program's payload slot (`curios/src/cache/payload.rs`), whose record already names the files the unit read and what each predecessor contained. A hit is exactly that record verifying — the same verification, not a second one — and an `effect` verdict is never filed, because the record covers files and predecessors and nothing else (`cached-verdicts.md`, *"nothing outside the binary and the recorded files"*). `--failed` reruns what was last filed as failed. A finer key — one per test over its erased reference closure, which `prune.rs`'s subtree walk already computes — is a seam, not this step.

**Verification.** A second run reports every pure verdict as `cached`; an edit to a file the library read reruns the library's tests and not an executable's; an effectful test reruns every time; a damaged verdict file is a miss; `--failed` after a mixed run runs the failures alone.

### 8. The property rung: `Gen`, `Shrink`, seeds, shrinking, discards

**Lands.** `/syn/Gen` and `/syn/Shrink` with their `/std` facades and the derivation arms the auto-derive seam reserves (`auto-derive-spec.md`, *Extension seam*); written witnesses for the carriers; `Test/forall` and `Test/forall_where` under *The library*; the pure `Rng`; the seed protocol; shrinking; and the `Failed after N cases and M shrinks (D discards), seed S` report with the counterexample spelled and an `example` line to paste.

**Verification.** A property that holds; one that fails on a derived type, with the shrunk counterexample asserted; one whose precondition discards, reporting the count; the same failure reproduced from its printed seed; a derived `Gen` for a recursive family bounded by size; and the round-trip of every spelled counterexample through `run`.

### 9. `--accept` for `example` and `refuse`

**Lands.** When an `example`'s spelled value or a `refuse`'s expected text differs, `--accept` rewrites the string literal in place. The literal's span comes from the Text stage — the declaring file is re-parsed and the registered definition's `example` or `refuse` call located — and the rewrite is verified by reparse as `curios format` verifies its own (`usage.md`, *Formatting*). Without `--accept` the report prints the replacement line to paste.

**Verification.** An accepted example round-trips; an accepted refusal round-trips; a file whose rewrite would not reparse is refused untouched; `--accept` with nothing to accept writes nothing.

### 10. Rewrite the design into what remains

The design below rewritten against what landed, restating nothing its owners now hold, and this file deleted under *Completion criteria*.

## Probe ledger

Every probe is a program on standard input; the answer column is filled by step 1. The *read* column is what the tree's source says today, and the file it was read from.

| # | Probe | Read | Bears on |
| --- | --- | --- | --- |
| P1 | `let t() -> Nat = 1;` then `t()` — a nullary function definition and its call | `let next() -> State(Nat, Nat)` in `programs/rng_state.crs` is called as `next()`; the elaborated type is a `Func` of no parameters | the discovered shape |
| P2 | `let t: Nat = expensive();` unreferenced, beside `let u() -> Nat = expensive();` unreferenced; `wonder stage ersd-optm` | items are evaluated eagerly at module init (`curios-ersd/src/module.rs`); an item whose summary is observable — a call into a recursive component seeds `may_diverge` (`curios-ersd/src/summary.rs`) — is kept unused; constructing a function contributes nothing | why a test is a function |
| P3 | `let t: Io(Nat) = Io/pure(expensive());` unreferenced | `IoPure` erases its operand eagerly (`curios-elab/src/into_ersd/intrinsic.rs`) | why `Io/pure` is not a delay |
| P4 | a `--unit` library whose module has no final term, compiled as a unit and then as an entry | `Module::body` being `Some` is the only thing that makes a unit the entry (`curios-core/src/module.rs`); a module file has no final term (`syntax.md`, *Declarations and modules*) | the synthesized tail |
| P5 | a private `let t() -> Nat` in `mod tests`, referenced from its own unit's final term | a declaration without `pub` is visible within its module's subtree, and a unit's tail is inside it (`syntax.md`, *Visibility*) | why nothing is suppressed |
| P6 | `proc/exit(3)` in a program run through `run_wasm` | an `ExitTrap` caught by `instantiate` and returned as the code (`curios-runtime/src/engine.rs`) | an exiting test |
| P7 | a program indexing past a list's end, run through `run_wasm` | `ListGet` is trap-classified (`curios-ersd/src/semantics.rs`); a trap returns as `Err` with its root cause (`engine.rs`) | a trapping test |
| P8 | a program that loops forever | no fuel and no epoch interruption in `shared_engine` (`engine.rs`) | the deadline |
| P9 | `/std/proc/args` under `MockHost::builder().args(…)` | `args` is a host op served from a field on both hosts (`os_host.rs`, `mock_host.rs`) | the selection protocol |
| P10 | `wonder diagnostics` with no target in a package with a library and two executables | library first, then each executable, each its own subject (`curios/src/wonder/ask.rs`) | the walk `curios test` takes |
| P11 | a unit stored, then one of its files edited, then `run` again | the record's read set misses (`curios/src/cache.rs`) | verdict invalidation |
| P12 | `Fmt/print("# and %")(x)(x)` for a derived `Spell` | the `#` slot per `auto-derive-spec.md` | the report |
| P13 | `let p: Eq(2 + 2, 4) = Eq/refl();` and `let q(x: Nat) -> Eq(x + 0, x) = Eq/refl();` | held rows of `curios/src/tests/laws.rs` | the `prove` rung |

## Design (provisional — owned by step 10)

### Permanent design decisions

1. **A test is a definition.** It has a path, a declared type, a body the kernel checks, and a span; it is discovered by its type and named by its path, and no test has a description string, a `describe` block or a registration call. The sentence is the identifier: `let an_absent_key_is_inserted() -> Test = …;`, as the Rust corpus already names its tests (`CLAUDE.md`, *Writing Rust*).
2. **A test is a function, never a value.** Top-level items are evaluated eagerly at initialization and an observable one is kept even unused (`curios-ersd/src/module.rs`, `prune.rs`), so a test holding a computed value would run at the start of every program linking its unit. A function is dormant by construction, so a test costs nothing anywhere but under the harness; and the report prints the body's source on failure, which a function has and a value does not.
3. **Discovery is registration at elaboration, by type.** The elaborator fills `Module::tests` when a definition's type is `() -> /syn/Test`, keyed on a `SyntaxRegistry` slot, exactly as it fills `Module::witnesses` from a signature — and, like that registration, it ignores `pub`. Nothing branches on a name, and no stage above the elaborator walks declared types looking for a head.
4. **A unit's tests are run by that unit, under a tail the compiler wrote.** Asked for a test compilation, the elaborator finishes the unit's items and then builds its final term from the set it just filled: `Test/main` applied to a list pairing each test's path with a reference to it. `Module::body` being `Some` is what makes a unit the entry, so a library under test *is* the program, and an executable under test is itself with its authored tail replaced. The tail is built in Core, below name resolution, so a private test is named by an item of its own unit and no visibility rule is bent, no name crosses a unit boundary, and no runner source exists to be written, formatted or read.
5. **Purity is read off the type, and it decides caching.** A `Test/pure` payload is a `Verdict` computed by a function that cannot perform an effect; a `Test/effect` payload is an `Io(Verdict)`. The first is a function of its unit and is filed beside that unit's test-program record; the second is never filed. No analysis decides this — the typing rule that makes every non-`Io` term pure does.
6. **The ladder's rungs are different spellings.** `prove` is a `Prop`-typed definition discharged by the kernel, listed from the recorded sort and never run. `pure`, `property`, `example` and `effect` are `Test` constructors run by the compiled program, untrusted in the sense `#guard` is. `refuse` is a program the harness compiles and does not run. A `prove` that exhausts the budget is reported as such, naming `pure` as the rung to move to.
7. **Every test runs in an instantiation of its own.** One `Store`, one call of `func/main`, one deadline. A trap, an exit and a timeout are each a named failure of that test alone (`curios-runtime/src/engine.rs`, `instantiate`); a unit's test program is deserialized once and instantiated per test, which `curios/src/tests.rs`'s `Compiled` already prices at milliseconds a run.
8. **A failure is source.** Values are spelled through `Spell`; a counterexample is printed as an `example` line; an accepted expectation is a string literal rewritten in place and verified by reparse. Seeds are printed and accepted on the command line, never persisted — the persisted artifact is text a reviewer reads.
9. **Rust runs, Curios asserts.** Discovery, the tail, instantiation, deadlines, caching, reporting and the exit code are the compiler's and `curios`'s (`documentation/design/toolchain/curios-owns-the-language-rust-owns-the-host.md`); `Verdict`, `Test`, the combinators, generators, shrinkers and the PRNG are `/std/Test`'s. The two meet at `/syn/Test`, as `Fmt` and `Spell` meet the compiler at their `/syn` names.

### The types

```crs
pub induct Verdict: pub Type
| passed()
| failed(Str)
end

pub induct Test: pub Type
| pure(Verdict)
| property((seed: Nat) -> Verdict)
| effect(Io(Verdict))
| refuse(program: Str, expected: Str)
end
```

Both live in `/syn/Test.crs` because the elaborator emits `Test`'s name and `Test/main`'s, and every `.crs` consumer imports them from `/std/Test`, in the facade form every emitted name has. `pure` carries a `Verdict` and not a thunk: the test *is* the function, and its body is evaluated only when the tail's `Test/main` calls it (decision 2). `property` takes the seed the runner supplies. `refuse` carries data the runner reads through the same run as every other rung, so nothing is read out of a term.

### The library

`/std/Test` supplies what a body is written with. Every combinator takes values, because a body is only evaluated when called:

- `Test/check(condition: Bool) -> Test` — `pure(passed())` or `pure(failed("the condition was false"))`.
- `Test/equal(@A: Type, use Eql(A), use Spell(A), actual: A, expected: A) -> Test` — the report is `expected # but got #` through `Fmt`, both spelled.
- `Test/example(@A: Type, use Spell(A), actual: A, expected: Str) -> Test` — `Spell/spell(actual) == expected`; the report is the `example` line with the actual spelling, which `--accept` writes back.
- `Test/effect(action: Io(Verdict)) -> Test`, with `Test/verdict(condition: Bool) -> Verdict` and `Test/verdict_equal(…)` for use inside an `Io` region.
- `Test/refuse(program: Str, expected: Str) -> Test` — the runner compiles `program` in the unit's scope as `wonder diagnostics` would, and passes when the rendered diagnostics contain `expected`; whitespace normalization and ordering modes follow `#guard_msgs` when a consumer asks.
- `Test/broken(reason: Str, test: Test) -> Test` — a `pure` or `effect` test whose failure is expected: it passes when the inner test fails, and fails naming `reason` when the inner test passes.
- `Test/forall(@A: Type, use Gen(A), use Shrink(A), use Spell(A), predicate: (A) -> Bool) -> Test` and `Test/forall_where(…, precondition: (A) -> Bool, predicate: (A) -> Bool) -> Test` — step 8.
- `Test/main(tests: List({Str, () -> Test})) -> Io({})` — what the synthesized tail applies: it reads `/std/proc/args`, selects a test by index, runs it with the seed given, prints the report and exits with the outcome's code. It is a library function so the protocol is spelled once, in Curios, and the tail is one application.

A pure PRNG for the property rung — `Rng`, a `State`-threaded generator in the shape of `programs/rng_state.crs` — and `Gen(A) { gen(size: Nat, rng: Rng) -> {A, Rng} }` and `Shrink(A) { shrink(A) -> List(A) }` are step 8's; their exact shapes are decided there against the derivation seam.

Writing a test:

```crs
use /std/{Test, Map, List, Option, Nat, Str, Eq, Io};

mod tests
    let an_absent_key_is_inserted() -> Test =
        Test/equal(Option/unwrap_or(Map/get(Map/insert(Map/empty(), "k", 3), "k"), 0), 3);

    let flattening_a_pair_doubles_the_length() -> Test =
        Test/forall((xs: List(Nat)) => List/len(List/flatten([xs, xs])) == 2 * List/len(xs));

    let a_message_is_written() -> Test =
        Test/effect(
            let _ = /std/print("hello\n")!;
            Io/pure(Test/verdict(true)));

    let a_string_is_not_a_nat() -> Test =
        Test/refuse("let x: Nat = \"one\";\n/std/print(\"\")", "type mismatch");
end
```

The module is private, the tests are private, and the enclosing module's private declarations are in scope for them — the subtree rule (`syntax.md`, *Visibility*). A theorem beside them is written as a proof and needs nothing from this file:

```crs
let two_and_two_are_four: Eq(2 + 2, 4) = Eq/refl();
```

### The synthesized tail

Asked to compile a unit as a test program, the driver lowers it as a unit and elaborates its items; the elaborator then builds the final term from `Module::tests`, in registration order, and checks it against `Io({})` exactly as an authored tail is checked (`curios-pipeline/src/compile.rs`, `elaborate_and_zonk`). The term is what this program would say if a program could name what it names:

```crs
Test/main([
    ("/app/Map/tests/an_absent_key_is_inserted", /app/Map/tests/an_absent_key_is_inserted),
    ("/app/Map/tests/flattening_a_pair_doubles_the_length", /app/Map/tests/flattening_a_pair_doubles_the_length),
])
```

It is built with the Core builders (`curios-elab/src/builders.rs`): a list of tuples pairing a `Str` literal — the path as `Item::describe` renders it, which is what filters and verdicts are keyed by — with a `Var` at the definition's own `Global`. `Test/main` is reached through its registry slot. A unit with no registered test gets `Test/main([])`, which runs nothing and exits 0.

Because every registered test is referenced by the tail, none is pruned from a test program; because none is referenced from any other program, every one is pruned from every other (`curios-ersd/src/optimize/prune.rs`). One compile serves a unit's whole run: the payload is deserialized once and instantiated once per test, with `[index, seed]` as the program's arguments.

`curios test` therefore compiles the library once and each executable once, taking the store's units and payloads as `run` takes them (`curios/src/pipeline.rs`, `payload_of`). A test program's payload is filed under the package's name and a reserved executable name no identifier can spell, so it cannot collide with a declared executable's slot (`curios-package/src/manifest.rs` requires an identifier of every declared executable).

### The `prove` rung

Nothing is written for it. A `Prop`-typed definition is a theorem the kernel checked; `wonder tests` lists it as `theorem` from the recorded sort, `curios test` reports it as passed because the unit compiled, and a definition that exhausts the budget is reported by the compile path as it is today, with the runner adding the sentence naming `Test/check` as the rung to move to. Nothing distinguishes a theorem written as a test from any other proof in the package, and that is intended: the harness does not own proofs.

### Reporting

One line per test, path then outcome — `passed`, `failed`, `cached`, `trapped`, `exited N`, `timed out`, `broken` — and, for a failure, the report indented beneath it followed by the test's body as written, located by the declaration's span. A property failure reads `Failed after N cases and M shrinks (D discards), seed S`, the counterexample spelled, and one `example` line to paste beside the property. A `refuse` failure prints the diagnostics that were produced against the text that was expected. The last line counts outcomes.

Exit codes: `0` when every test passed or was cached passing, `1` when any failed, trapped, exited, timed out or could not be built, `2` when a unit under test holds a written goal — the tri-state `usage.md` states, unchanged, because a failed test is a hard fact and a goal batch is what it already is.

Filters name a path prefix — `curios test /app/Map` — never a pattern, and a filter matching nothing is exit `1` naming the filter (the libtest defect that reports `ok` for a test that does not exist). `--seed S` sets the property seed for the run; `--timeout` the per-test deadline; `--failed` selects what was last filed as failed; `--accept` is step 9's.

### Caching

A verdict is a pair of the test's path and its outcome, filed beside its test program's payload record — which names the files that unit read, what each predecessor contained, the compiler and the engine. A hit is that record verifying, the verification the store already performs to believe a payload, shared rather than restated; nothing is added to the address, because a verdict is a fact about the same inputs the payload is. A `property` verdict is filed under its seed. An `effect` verdict is never filed. `curios test` reports a hit as `cached` on the test's line, as the fold reports a `reused` unit.

Two consequences the spec states rather than implies. A test whose closure is unchanged by an edit to another file of its unit reruns anyway, because the unit is the record's granularity; a per-test key over the erased reference closure is the seam that narrows it, and `prune.rs`'s subtree walk already computes that closure. And a verdict is believed on the strength of the compiler that reached it, which the address already names, so a compiler rebuild reruns everything — over-invalidation, and the safe direction.

### Timeouts and isolation

The shared engine gains epoch interruption (`curios-runtime/src/engine.rs`, `shared_engine`); the runner bumps the epoch from a timer thread, and a test whose deadline passes is `timed out`, its instantiation dropped. There is one engine, so the launcher and every `run` pay the epoch check too; the cost is measured before step 5 lands, and if it registers, the runner takes an engine of its own and its payloads an address of their own. A trap is reported with its root cause as `instantiate` renders it, an exit with its code; neither reaches another test, because none shares a store.

### The `wonder tests` record

`{ path, kind }` with `kind` one of `test`, `theorem`, read off `Module::tests` and the recorded sort of each unit the package declares, in the order `wonder diagnostics` walks them. No rung below `test` can be listed, because a rung is a constructor the body builds at run time and the query executes nothing. The server transport does not adapt the record in this design; a run-test lens is a consumer that arrives with its own step.

### Soundness discipline

The harness adds nothing to the perimeter. A test is a definition the kernel checks like any other; the synthesized tail is an ordinary closed term checked against `Io({})` and rechecked by the kernel with the module it belongs to, so a test program is judged exactly as an authored program is. A verdict is what the compiled program computed, trusted exactly as any `run` is; `Test/check(a == b)` is decided by an `Eql` witness with no law tying it to `Eq`, and that is the untrusted rung by design rather than an oversight. Caching a verdict admits nothing the payload's own admission did not: it is filed beside the same record and believed on the same terms.

### Extension seam

- **A finer verdict key** over each test's erased reference closure.
- **A scripted host** for `effect` tests — `MockHost` already scripts stdin, files, endpoints, both clocks, a seeded RNG, args and env (`curios-runtime/src/mock_host.rs`) — which would make a scripted `effect` test a function of its script and so cacheable under the same rule; exposing the script from Curios needs an ABI decision and is its own specification.
- **`Enum(A)`** for finite types, exhausted rather than sampled.
- **Generators from inductive relations** (QuickChick's `ArbitrarySuchThat`, Chamelean), for `Prop`-indexed inputs.
- **Doctests**: an example in a doc comment is `Test/example` with the comment as its source, once doc comments exist; shape `example` so a documentation generator can lower one without a second mechanism.
- **A run-test lens** in the server transport, reading `wonder tests`.
- **Stateful model testing** over `Cell`/`Async`, in the Erlang QuickCheck shape, once the scripted host exists.

### Non-goals

- Fixtures, setup and teardown, `describe`, tags, and test descriptions apart from the path.
- Name-based discovery, a `test` keyword, attributes, conditional compilation, or a fourth presence file in a package.
- Snapshot tests of arbitrary output; `example` accepts a spelled value and nothing else.
- Process-level isolation, retries of flaky tests, parallel execution across hosts.
- Coverage, mutation testing.
- Tests for `/std` itself through this harness; the fixed prelude is tested by the Rust corpus, and a `curios test` over the prelude archive is a different product question.
- Persisting seeds.

### Verification

Each step's row above, and across them: a package exercised end to end under `curios test` in `curios/tests/` beside the bundle test, asserting the report text, the exit code, the cached second run and the miss after an edit; a test program's `wonder stage core-elab` pinned against its tail; the epoch cost figure as an ignored measurement in the pattern of `stored_prelude_measurements`.

### Completion criteria

- `/syn/Test`, `/std/Test`, the registry slot, `Module::tests` and the recorded sort exist, and the archive carries them.
- A unit compiles as its own test program under a synthesized tail, and the kernel judges it.
- `curios test`, `wonder tests`, `--failed`, `--seed`, `--timeout` and `--accept` exist and are documented in `usage.md`; `syntax.md` gains nothing, because no syntax changed.
- `Gen` and `Shrink` derive through the auto-derive seam, and that seam's note about a generator and a shrinker is discharged.
- Every row under Verification is a test, and the gate passes.
- The roadmap's *Test runner* item is checked with a summary, and `documentation/design/toolchain/` gains one decision file — a test is a definition discovered by type, and a unit's tests run under a tail the compiler synthesizes — with a separate runner program, name-based discovery, a `test` keyword, seed persistence and integrated shrinking recorded as rejected for the stated reasons.
- This file is deleted, its contracts moved to `usage.md`, `/std/Test.crs`, the runner module's rustdoc, and the decision file.

### Decisions taken here, still reversible

- A test is `() -> Test` rather than a value of type `Test` (decided by P2/P3; reversible only if items stop being eager). An `expect name = term;` declaration form desugaring to this shape stays available, at the cost of the surface grammar, its printer, the formatter, `syntax.md`, the tree-sitter grammar and both editor extensions.
- The tail synthesized in Core rather than lowered from generated text.
- Verdicts at unit granularity before per-test granularity.
- One shared engine with epochs rather than a runner-only engine, pending the measurement.
- The selection protocol as program arguments and exit codes, with the report on standard output.
- `Test/refuse` carrying its program as a string literal rather than as a file beside the test.
- Exit `1` for a failed test.
