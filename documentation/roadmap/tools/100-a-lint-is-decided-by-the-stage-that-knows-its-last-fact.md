# A lint is decided by the stage that knows its last fact

**The benefit.** Each lint is decided by the stage that establishes its last fact and kept in that stage's record: the lowering words `unused-import` and `unused-declaration`, elaboration files each declaration's binder lints with its record, and a unit gathers what the records hold. What is left is the owner that decides one lint: `curios-lint` holds `unused-dependency` alone, which the package pass deciding it can hold. GHC, OCaml and rustc's resolver and liveness pass decide a warning in the phase that knows it, and rustc keeps a query's diagnostics with its result. The project has decided to adopt it.

The next step is stage 1, a delivery. It waits on nothing.

## What is wrong today

- **The decision still rejects the shape the code has taken.** [A lint is an exact finding read off the compilation](../../design/tools/a-lint-is-an-exact-finding-read-off-the-compilation.md) rejects "a stage deciding a lint, which spreads lint logic across every stage that knows a fact, as GHC's warnings spread across its renamer, typechecker and desugarer",. Two stages know a lint's facts, and the later one can be handed the earlier's half, as the lowering already hands elaboration `curios_core::Minted` beside the module and an `Unresolved` under a binder for elaboration to report. GHC's failure is a phase that runs twice reporting twice ([#23569](https://gitlab.haskell.org/ghc/ghc/-/issues/23569)), where a stage here decides a declaration once and keeps the decision in the record a recompile reuses. One owner did not remove the spread: both stages recorded lint-shaped facts for it, and it added a join each reader restated.

## What this builds on

- **`Minted`** (`curios-core/src/names.rs`) is what the lowering hands elaboration beside the module: per declaration, persistent over a baseline, read once where elaboration seeds its solver, and never on the `Definitions` every later stage and the certifier read.
- **`Unresolved`** (`curios-core/src/unresolved.rs`) is a report one stage makes and another reports, in `curios-core`, which both reach.
- **A declaration's trace record is filed whenever an attempt stands** (`demand.rs`), its refused items included, with its credits (`credited_in`) and its sites' classes (`classes_in`) decided there; a recompile keeps it with the declaration, and a store's candidate is adopted with it.
- **A lint placed when a record is made stays placed wherever the record is reused.** A term archives its extent (`curios-core/src/term.rs`), so a lowered item's digest, which a store's candidate is found by (`lowered_digest`, `curios-elab/src/candidate.rs`), holds every extent; and a declaration lowered again is elaborated again (`curios-pipeline/src/recompile.rs`).
- **Each declaration's mints hold its unread sites** (`curios_core::Mints::unread`), which a recompile keeps, renames and forgets with what it minted, and elaboration decides the binder lints from as it files the declaration's record (`curios_elab::binder_lints`, `Context::lints_in`); `Observed` holds every record's lints and the final term's, whatever the walk concluded.

## What the peers do

GHC, OCaml, Agda and Gleam warn as they check; rustc declares every lint once and decides each where its facts are — the resolver buffers `unused_imports` ([`check_unused.rs`](https://doc.rust-lang.org/stable/nightly-rustc/src/rustc_resolve/check_unused.rs.html)), liveness decides `unused_variables` ([`liveness.rs`](https://doc.rust-lang.org/nightly/nightly-rustc/src/rustc_mir_transform/liveness.rs.html)) — and keeps the diagnostics a query emitted beside its cached result, emitted again when the result is reused ([`QuerySideEffects`](https://doc.rust-lang.org/stable/nightly-rustc/rustc_query_system/query/struct.QuerySideEffects.html)). Kotlin K2 reports only in a checkers phase over resolved FIR ([`fir-basics.md`](https://pigweed.googlesource.com/third_party/github/JetBrains/kotlin/+/49b2ac1b100c5042a0e98582713938062efa6a31/docs/fir/fir-basics.md)), and Lean's linters read the info trees elaboration left ([unused variables](https://lean-lang.org/doc/api/Lean/Linter/UnusedVariables.html)): a phase of their own pays where linters are many and open, which Curios's six exact lints are not. **What is taken:** the decision where the fact is (GHC, rustc), the kept result (rustc). **What is set aside:** a phase that decides twice (GHC's desugarer), and an owner that joins records kept for it.

## Settled

- **Each lint is decided and worded by the stage that establishes its last fact.** The lowering decides `unused-import` and `unused-declaration`, a dead private module folding its declarations as now; elaboration decides `unused-binder`, `unused-let` and `unit-wildcard`; `curios lint`'s package pass decides `unused-dependency` (`curios-wonder/src/lint.rs`). The kinds, their wording, where each stands and `usage.md`'s table do not change.
- **The vocabulary is `curios-core`'s**, as `Unresolved` is: `Lint`, `LintKind` and its names, the one phrasing every `unused-*` lint shares, `Lint::report` and `Lint::ordered`, in `curios-core/src/lint.rs`. `curios-lint` is deleted.
- **The lowering hands elaboration what no written name reads in each declaration's `Mints`**, as `Mints::unbound` carries its unresolved names (`curios_core::Unread`), never on `Definitions`, which the certifier reads and Core text spells; a declaration holding a written goal holds none. Elaboration decides the binder lints as it files the declaration's record, keeps them there (`Concluded::lints`), and hands out every record's whatever it concluded (`Observed`). Credits and classes never leave elaboration.
- **Every reader gathers.** `Unit::new` stores the lowering's lints and `Observed`'s in reading order (`Lint::ordered`), and the pipeline's `Findings` gathers the same for a refused program and a refused library, handed beside the refusal; `every_authored_source_is_lint_clean` reads the lints the archive's units store. No reader decides.
- **What the store holds changes, and nothing a program is judged or runs as.** The stored bytes that change with the remaining stages are none but a refused unit's, which no store files; the store's `SCHEMA` (`curios-package/src/store.rs`) is raised with any that moves. Every verdict, certification, erased and emitted program stays what it is.

## Stages

1. **`curios-lint` goes.** `unused_dependencies` and its suite move to `curios-wonder`, and the crate, its README and its rows in the workspace manifest and `.claude/rules/pipeline.md` are deleted. Check: `cargo xtask workspace lint`; `curios lint`'s suite in `curios-wonder`.

Each stage's contract lands in its owner as it lands: the decision restates who decides a lint — the stage that establishes its last fact, in its own record — with its **Rejected** paragraph naming an owner that joins records kept for it, and dropping "a stage deciding a lint"; `.claude/rules/pipeline.md`'s lint line names the two stages and the vocabulary.

## Retirement

Once the stage has landed, the decision, the crates' READMEs and the rules state who decides each lint, and nothing references this specification, it is deleted. A later lint is decided by the stage that establishes its last fact, and a fact one stage knows and a later one needs travels beside the module, as each declaration's unread sites do in its mints.
