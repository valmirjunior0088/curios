# Test harness: a test is a declared description, and its unit is the program that runs it

## Status

Nothing is built. Every design claim below that rests on compiler behavior was put to the tree's compiler on standard input on 2026-08-31 and answered; the step that lands a claim pins it with a test. This specification depends on no other; the [auto-derive specification](auto-derive-spec.md) builds on what lands here and changes no signature stated below.

A landed step is replaced by one checked line; its contracts move to their owners (`usage.md`, `syntax.md`, `/std/Test.crs`, the owning module's rustdoc, the decision file under step 7).

## Mission

`curios test` runs every test the governing package declares. A test is a `test` declaration beside the code it tests, whose body is a `/syn/Test`: a description, built by library combinators and scheduled by the harness, the way an `Io` is a description performed by the host. The description holds one of three things, each naming the authority that settles it: `theorem()` — a proposition the kernel settled during elaboration, nothing runs; `verdict(Verdict)` — a verdict the compiled program computes; `action(Io(Test))` — an action the host performs, yielding another description.

A test is compiled into **the unit that declares it**: the compiler collects the declarations and writes the unit's final term itself, so a private test in a private module is reached by its own unit and no visibility rule bends. A unit's tests cost nothing in every other program: the declaration lowers to a nullary function, and an unreferenced function is pruned (probed: absent at `ersd-optm`).

## Design

### Permanent decisions

1. **A test is declared, never discovered.** Every registered item kind has its declaration keyword (`satisfy`, `foreign`, `concept`, `induct`, `struct`); `let` registers nothing, and discovery by type would make it register for the first time and would run every nullary `Test`-valued helper as a test. `test` is a contextual keyword exactly as `satisfy` is — `curios-utilities/src/qualifier.rs` reserves neither — so `Pred/test` and every other `test`-named member keep compiling.
2. **The name is the sentence, and the path is the identity.** `test name = body;` declares an item named at its path, reported and filtered by it. `pub` is refused: a test's name is its report line, not an export. The subtree rule is untouched: a test in `mod tests` sees the enclosing module's private declarations.
3. **`Test` is a description because only the harness can eliminate it.** Its representation is private to `/syn/Test.crs`, where the combinators and `Test/main` live; `/std/Test` is the facade. A consumer builds a `Test` through the combinators and can do nothing else with one; only `Test/main` matches it.
4. **The tail is synthesized in Core.** Asked for a test compilation, the elaborator finishes the unit's items and builds the final term itself — `Test/main([("path", thunk), …])` in declaration order, each pair a `Str` literal (the path as `Item::describe` renders it) and a `Var` at the test's own `Global` — checked against `Io({})` as every authored tail is and judged by the kernel like any program. An executable under test has its authored tail replaced; a unit with no tests gets `Test/main([])`, which runs nothing and exits 0.
5. **`Test/equal` reports through `Spell`, not `Show`.** Derivation will derive `Spell` and `Eql` and never `Show`, so `Show` premises would leave fixtures hand-written forever; and `Show(Str)` is the identity, which makes a `Show` report of two strings ambiguous (probed). `Spell` lands here as an ordinary library concept; derivation arrives with the auto-derive specification and changes no signature.
6. **`Test/perform` takes a thunk to open a region.** A `!` reads its monad from its region; regions open only at value bodies, lambda bodies and match arms, and a call argument is none of these — an inline action under an `Io(Test)`-typed parameter is refused (`Monad(?)`, probed), and honoring it would hoist the effect into the surrounding pure region. A lambda body is a fresh region with its type pushed from `() -> Io(Test)`, so the inline form sequences inside the lambda (probed), and a named nullary action passes as itself: `perform(an_action)`.
7. **The kernel's rung costs no witness.** `Test/refl(f(x), y, Eq/refl())` is settled by conversion during elaboration, on any user type, with no `Eql` and no `Spell`; a failure spells both sides in the report (probed). A wrong proof is a build failure — the unit's tests do not run, as with any compile-time assertion — and a budget exhaustion is reported by the compile path, naming `Test/check` as the rung to move to.
8. **Exit codes are the tri-state `usage.md` states**: 0 when every test passed or proved, 1 when any failed, trapped, exited or could not be built, 2 when a unit under test holds a written goal.

### The types

```crs
pub induct Verdict: pub Type
| passed()
| failed(Str)
end

pub induct Test: pub Type
| theorem()
| verdict(Verdict)
| action(Io(Test))
end
```

Both in `/syn/Test.crs` with `Test`'s representation private, because the elaborator emits `Test`'s name at registration and `Test/main`'s in the tail. `theorem` is nullary: its evidence is consumed by the checker inside `prove`/`refl` and would erase in any case, so the description carries only the fact. `action` holds a `Test`, not a `Verdict`, so every pure combinator works at the end of an effectful body; positivity accepts the recursion through `Io` (probed).

### The library

- `Test/prove(P: Prop, proof: P) -> Test` — states the proposition and discards the checked evidence. Blocked on step 1: its fully erased signature trips the defect that step fixes.
- `Test/refl(@A: Type, a: A, b: A, proof: Eq(a, b)) -> Test` — the proposition inferred from the operands; the proof is written, because the metavariable fill covers decided propositions and not `Eq` (probed: it is not inferred).
- `Test/check(cond: Bool) -> Test`.
- `Test/equal(@A: Type, use Eql(A), use Spell(A), actual: A, expected: A) -> Test` — `failed("expected … but got …")`, both spelled.
- `Test/perform(thunk: () -> Io(Test)) -> Test`.
- `Test/main(tests: List({Str, () -> Test})) -> Io({})` — reads `/std/proc/args`, selects one test by index (`List/try_get`), runs its description, prints its outcome line and exits with the outcome's code, so the protocol is spelled once, in Curios.

`/std/Spell.crs` declares `pub concept Spell(A: Type): pub Type { spell(A) -> Str }`: `spell(v)` is Curios source text denoting `v` at its type. Written witnesses beside each carrier's `Show` witness: `Nat`, `Int`, `Byte`, `Bool`, `Flt` (non-finite values by their `/std/Flt` names), `Char` and `Str` (quoted, escaped), `Bits`, `Bytes`, `List`. Beside them, the `/std` gap this harness needs closed: written `Spell` and `Eql` for `Option`, `Result` and `Order`, and written `Eql` for `List` — the first three replaced by derived witnesses when auto-derive lands, `List`'s written by design.

### Reporting

One line per test, path then outcome — `proved`, `passed`, `failed`, `trapped`, `exited N` — and, for a failure, the report indented beneath it followed by the test's body as written, located by the declaration's span. The last line counts outcomes. Filters name a path prefix — `curios test /app/Map` — and a filter matching nothing exits 1 naming the filter.

## Steps

- [x] 1. Erasure agrees with itself about a dependent proof payload — landed: one threaded classification for every telescope walk (`curios-elab/src/into_ersd/classify.rs`), the three repros as tests in `curios/src/tests/erasure.rs`, and the verifier's display frame quoting its detail neutrally
- [x] 2. `/syn/Test`, `/std/Test`, `/std/Spell`, and the `/std` witness gap — landed: the description and its combinators in `/syn/Test.crs` with `/std/Test` as facade, `/std/Spell.crs` with every carrier witness, written `Spell`+`Eql` for `Option`/`Result`/`Order` and `Eql` for `List`, pinned by `curios/src/tests/harness.rs`
- [ ] 3. The `test` declaration form and registration
- [ ] 4. The synthesized tail, and a unit compiled as its own test program
- [ ] 5. `curios test`
- [ ] 6. `wonder tests`
- [ ] 7. Documentation, the decision file, and this file's deletion

Each step is one authorization and one commit, lands its tests before its mechanism, and runs the full gate at its end. 2 needs 1; 3 needs 2; 4 needs 3; 5 needs 4; 6 needs 3; 7 needs everything.

### 3. The `test` declaration form and registration

**Lands.** The surface form `test name = body;`: parser (contextual keyword, `curios-text/src/parse/top_level.rs`), printer, `curios format` round trip, the tree-sitter grammar with its regenerated committed `src/`, both editor extensions, and the `syntax.md` section. Lowering as `satisfy` lowers (`curios-text/src/into_core.rs`): a `FlatLet` of a new `DefinitionKind::Test`, named at the declaration's path, declared type `() -> /syn/Test` through a new `SyntaxRegistry` slot (`curios-utilities/src/syntax.rs`, spelled in `curios-prelude-archive/src/syntax.rs`, covered by the prelude presence check), body the authored body under a nullary lambda. `Module::tests: Vec<Global>` in declaration order, filled at elaboration by kind; `pub test` refused at parse. The archived prelude carries the field, empty.

**Verification.** Registration in the entry module, a private `mod tests`, and a library unit through the store; formatter round trip; `pub test` refused; `Pred/test` and a value named `test` keep compiling; the archive replay pins the empty set; a bare `!` in a test body is refused by the region rules as today.

### 4. The synthesized tail, and a unit compiled as its own test program

**Lands.** The pipeline entry that compiles one unit as a test program: items elaborated as a unit, the final term built from `Module::tests` with the Core builders (`curios-elab/src/builders.rs`) as `Test/main` applied through its registry slot, checked against `Io({})` (`curios-pipeline/src/compile.rs`, `elaborate_and_zonk`), and carried through the back half `compile_entrypoint` uses. An executable's authored tail is replaced; no file is written; nothing about the surface changes.

**Verification.** A library with three tests, private ones included, shows all three in `wonder stage core-elab`'s tail; an executable's authored tail is absent from its test program; a unit with no tests runs nothing and exits 0; the kernel judges every test program; every registered test survives the prune in its test program and none appears in the same unit's ordinary program (`wonder stage ersd-optm`).

### 5. `curios test`

**Lands.** The subcommand (`curios/src/cli.rs`, `main.rs`): the governing package's library and each executable compiled as test programs, taking the store's units and payloads as `run` takes them, the payload filed under a reserved executable name no identifier can spell; one instantiation per test through the runtime with `[index]` as the program's arguments; the report and exit codes under *Reporting*, `trapped` and `exited N` classified from the runtime's error as `instantiate` renders it; filters; `usage.md`'s subcommand section.

**Verification.** A package with a passing, failing, proving, trapping, exiting and effectful test: the six lines, exact report text, the exit code; a filter naming no test exits 1 naming it; `curios run` of the same package neither runs nor links a test.

### 6. `wonder tests`

**Lands.** The query: every test the governing package declares, as `{ path }` records read off `Module::tests`, walking library then executables as `wonder diagnostics` walks them, executing nothing. A rung is a constructor the body builds at run time, so the record does not name one. `usage.md`'s query table gains the row.

**Verification.** A listing per target form; a package with no tests lists nothing and exits 0.

### 7. Documentation, the decision file, and this file's deletion

**Lands.** `documentation/design/toolchain/a-test-is-a-declared-description-run-by-a-synthesized-tail.md`: the declaration form, the description, the thunked `perform`, the synthesized tail — with discovery by type, name or attribute, `Bool`-only bodies, and a region change for `perform` recorded as rejected. The roadmap's *Test runner* item checked with a summary; every contract moved to its owner; this file deleted.

## Extension seam

Each arrives with its own step or specification: the property rung (`Gen`, seeds, shrinking, derived generators through the auto-derive seam); `Test/refuse` with `wonder diagnostics` as oracle; `Test/broken`; `example` with `--accept`; verdict caching (purity is a runtime constructor here, so caching needs a statically recorded head, and until then nothing is cached); per-test deadlines through epoch interruption (until then a diverging test hangs the run, and the epoch cost is priced before landing); `Fmt`'s `#` spell-slot; the two-argument `Test/refl` (extending the decided-proposition fill to `Eq` on convertible operands); a scripted host for cacheable effect tests; doctests.

## Non-goals

Discovery by name, attribute or file convention; descriptions apart from the path; fixtures, tags, `describe`; snapshot tests of arbitrary output; process-level isolation, retries, parallel hosts; coverage and mutation testing; tests for `/std` through this harness; property-based testing in this version.

## Completion criteria

- The three erasure repros compile and run, pinned by tests.
- `/syn/Test`, `/std/Test`, `/std/Spell`, the registry slots, `DefinitionKind::Test` and `Module::tests` exist, and the archive carries them.
- `test` parses, prints, formats, registers, and is documented in `syntax.md`; the tree-sitter grammar agrees.
- A unit compiles as its own test program under a synthesized tail, and the kernel judges it.
- `curios test` and `wonder tests` exist and are documented in `usage.md`.
- Every step's Verification row is a test, and the gate passes.
- The roadmap item is checked, the decision file exists, and this file is deleted with its contracts moved to their owners.
