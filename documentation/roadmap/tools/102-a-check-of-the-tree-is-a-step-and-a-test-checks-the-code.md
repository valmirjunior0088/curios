# A check of the tree is a step, and a test checks the code

**Adopted by the user on 2026-11-22 and restated on 2026-11-23; its next step is stage 1, a ready delivery; it extends the fetching [`xtask`'s README](../../../xtask/README.md#a-tool-a-step-runs-is-fetched-at-its-digest-and-named-by-the-step) states.** A test holds the code to what it promises, and a step holds the tree to what it states: a standard's corpus, a revision published elsewhere, a description agreeing with its owner, a program linting clean. The task files are descriptions the runner reads, and nothing outside the runner's crate reads them.

## What is wrong today

- **Two corpora are run by nothing.** At `3ea89cfbf`, `Json/decode` answers all 318 of JSONTestSuite's cases as the suite asks — the 95 to read read, the 188 to refuse refused, the 35 left to a parser 11 read and 24 refused — and at `2e44199cb` `Toml/decode` refuses all 509 of `toml-test`'s invalid TOML 1.0 documents and reads all 208 of its valid ones; only `Json/tests`'s number cases run, by hand-copied names.
- **Checks of the tree run among the tests of the code.** `editors/zed`'s whole suite is three: its pins publishing these grammars, Core text's query opening with the program's, and its lint table being the workspace's (`every_pinned_rev_publishes_this_grammar`, `the_core_text_query_opens_with_the_programs`, `the_lint_table_is_the_workspace_s`); and `curios/src/tests/programs.rs` holds `programs/` linting clean, the twin of the formatting `workspace lint curios-format` holds.
- **Tests of the runner read the repository's task files.** `xtask/src/cli/tests.rs`'s four and `xtask/src/schedule/tests.rs`'s seven build the real tree (`Tree::read`) and assert the steps it lists, so a step added fails a test of the scheduler, as `curios-format` failed `a_lint_of_the_workspace_builds_the_launcher_first`, and a fault of the scheduler the files of the day do not exercise passes.
- **Two values are stated twice.** The edition handed to `rustfmt` is `Cargo.toml`'s and Biome's paths are `biome.json`'s, each restated in `xtask/tasks/workspace.toml` and held equal by a test (`the_edition_handed_to_rustfmt_is_the_workspace_s`, `the_workspace_s_biome_steps_reach_every_file_biome_reads_of_it`).
- **Biome runs as eight steps over four slices of one list.** `workspace`, `tree-sitter`, `textmate` and `vscode` each format and lint their part of what `biome.json` lists, and `format web` writes all of it at once.
- **The subjects are named for a layout, a moment and a nesting.** `workspace` holds the shipped crates' lint, suite and caches, named for the Cargo workspace whose default members they are; `compiler` the build as it is released and its pages, which leaves no name for releasing it; `launcher` one step only the compiler consumes; and `workspace test` a group of one step, `suite`.
- **Two topics of the documentation are named for what they are not.** `standard-library` holds `/std`'s capabilities and laws, which the documents call the prelude's, and `tools` every decision a programmer meets of the toolchain, which `.claude/rules/tooling.md` names tooling.
- **Unicode's conformance files are fetched on every build.** `curios-unicode`'s build script fetches `GraphemeBreakTest.txt` and `NormalizationTest.txt` beside the files its tables are built from, though only tests read them.

## Goal

- `conformance`, a group of two steps, `json` and `toml`: Curios programs, `curios run conformance/<suite>.crs -- {corpus}`, which list the corpus, decode each case and exit nonzero naming every case answered otherwise, a valid TOML document's value held to the tagged JSON beside it. A corpus is a `tools` entry with one row for every host and the digest of its unpacked files, named by the step that reads it as `{.tools.<name>}`, a directory where a tool names an executable.
- `zed check`, the runner's own check that each pin of `extension.toml` publishes its grammar, that Core text's query opens with the program's and that the extension's lint table is the workspace's; `editors/zed`'s tests go, and `zed test` with them.
- `compiler`, today's `workspace` with the launcher folded in: `compiler launcher`, `compiler lint` (`rustfmt`, `curios-format`, `curios-lint`, `clippy`), `compiler test`, a step, and `compiler caches`. `curios-lint` runs `curios lint` over the tree's programs, and `programs.rs` goes.
- `distribution`, today's `compiler`: `distribution build`, the compiler as it is released, and `distribution test`, its pages; `release` stays free for the act of releasing it, as rust-analyzer's xtask keeps `dist` apart from `release`.
- `biome`, a subject whose `lint` formats and lints what `biome.json` lists, and `format biome` in place of `format web`; the Biome steps of `compiler`, `tree-sitter`, `textmate` and `vscode` go, and `variables.web` with them.
- The runner's tests of its own code over fixtures: the eleven build their trees with `Tree::parse`.
- The edition read from `Cargo.toml` and Biome's paths from `biome.json`, by the runner, and the two tests holding them deleted.
- `documentation/roadmap/` and `documentation/design/`'s `standard-library` named `prelude` and `tools` named `tooling`, every link to them restated.
- Unicode's two conformance files fetched by `curios-unicode`'s test support through `curios-download`, a dev-dependency, at their pinned digests, and the build script fetching only what the tables are built from, the database's version one constant both read.

## Settled

- **A test checks the code and a step the tree** (decided by the user on 2026-11-23). A crate that reads a file as its given keeps its checks as tests: `curios-unicode`'s and `/std`'s conformance to the cases the crate pins, the formatter's laws over the tree's sources, the parser's words where the editors' grammars state them, and the documentation's examples, the compiler holding its documented behaviour (`curios/src/tests/readme.rs`, `reference.rs`).
- **The runner's crate alone reads the task files** (decided by the user on 2026-11-23). Its tests of its own code build their trees from fixtures; its tests of the descriptions against another owner — the workflow covering the handoff, a workspace step naming no feature, the lint going on past a crate that fails, a tool fetched at the lock's or the schema's version — read the real files, a claim about them being one no fixture carries.
- **The Zed checks are the runner's own** (decided by the user on 2026-11-23), a step under `zed`, as the runner already fetches and unpacks what a step names.
- **Biome is a subject**, its list `biome.json`'s, as a tree's lint holds its formatting.
- **A subject is named for what it holds** (decided by the user on 2026-11-23): `compiler` for the product its crates are, holding the launcher only it consumes; `distribution` for the compiler as it ships, `release` left to releasing it; a group only where its steps are run apart, so the suite is a step. The editors' four subjects, `bundle`, `native`, `conformance`, `docs`, `jig` and `profile` stay subjects of their own, each a job on its own machine or a part a reader runs alone.
- **A topic is named for what it holds** (decided by the user on 2026-11-23): `prelude`, `/sys` and `/std` as every program gets them, and `tooling`, as `.claude/rules/tooling.md` names the area.
- **A corpus is downloaded, not vendored.** It is pinned at a commit and checked by the digest of its unpacked files, since the archive a forge generates at a commit is not byte-stable where its contents are: Go's module hash (`h1:`, a digest of the file tree) and Nix's `fetchFromGitHub` check what was unpacked for the same reason.
- **A corpus's check is written in Curios**, a program over `File`, `fs` and the codec it holds, exercising `/std` as a program does.

**Rejected.** The corpora vendored into the tree, a second copy of what a commit already pins. A crate of their own whose build script fetches them, where `xtask` fetches what a step names. The archive's bytes digested, which a forge's recompression breaks. The checks as Rust tests the cases are handed to. Unicode's conformance as a step: a second pin of the database, or a path into `curios-unicode`'s kept files. The descriptions' checks as a step of the runner: one pair that disagrees would stop every command, `format` among them. The Zed checks as a Curios program. Biome's steps per subject, four slices of one list. `rust`, `cargo`, `crates` or `curios` for the compiler's subject: the first two name what every other subject shares, the third nothing of what the crates are, and the last the language. One subject for the editors, and `bundle`, `native` and `conformance` folded into `compiler`: a nesting no reader runs by. `release` for the build as it ships, the name of the act of releasing it.

## Stages

**1. The runner's tests over fixtures, and the two values stated once.** Check: the eleven pass over fixtures, and a step added to a repository's task file fails none of them; no task file states the edition or Biome's paths.

**2. The subjects reshaped**: `compiler`, `distribution`, `biome`, `zed check` and `curios-lint`, with `CLAUDE.md`, `.claude/`, the READMEs and both workflows restated. Check: each step passes, and fails where its check is made to — a pin moved off its grammar, a program that lints, a file Biome would reformat — and `every_step_the_handoff_runs_has_a_job` holds the workflow's new jobs.

**3. Unicode's conformance files fetched for tests.** Check: a build fetches only the tables' files, the tests the two beside them, and a conformance file altered is refused at its digest.

**4. The topics renamed**: `prelude` and `tooling`, every link restated. Check: `cargo xtask docs build` and the agent files' citations hold every link.

**5. Corpus entries in the `tools` table.** A host-independent entry and a digest of an unpacked tree. Check: a corpus fetched, kept and refused where a file of it is changed.

**6. `json`.** Check: the step passes, and fails naming a case `Json/decode` is made to answer otherwise.

**7. `toml`.** Check: the same, a valid document's value among what it holds.

## Verification and retirement

Each stage's check, then `cargo xtask compiler lint`, the jig's lint, the suite, `cargo xtask docs build` and `cargo xtask handoff`'s new requirement. Once stage 7 lands, this file is deleted.
