# A check of the tree is a step, and a test checks the code

**Adopted by the user on 2026-11-22 and restated on 2026-11-23; its next step is stage 1, a ready delivery; it extends the fetching [`xtask`'s README](../../../xtask/README.md#a-tool-a-step-runs-is-fetched-at-its-digest-and-named-by-the-step) states.** A test holds the code to what it promises, and a step holds the tree to what it states: a standard's corpus, a revision published elsewhere, a copy agreeing with its original, a citation reaching its file, a program linting clean. The task files are descriptions the runner reads, and nothing outside the runner's crate reads them.

## What is wrong today

- **Two corpora are run by nothing.** At `3a407eed0`, counted: `Json/decode` answers all 318 of JSONTestSuite's cases at `1ef36fa` as the suite asks — the 95 to read read, the 188 to refuse refused, and of the 35 left to a parser 11 read — and `Toml/decode` refuses all 509 invalid TOML 1.0 documents of `toml-test` at `96fd088` and reads all 208 valid ones, each to the value its tagged JSON states, a NaN of either sign being a NaN and floats compared as IEEE compares them. Only `Json/tests`'s number cases run, copied by name.
- **Checks of the tree run among the tests of the code.** `xtask/src/tests/agents.rs` holds the agent files' globs matching tracked files and their citations reaching the tree (`every_glob_of_a_rule_matches_a_tracked_file`, `every_citation_of_an_agent_file_is_in_the_tree`); and `curios/src/tests/programs.rs` holds `programs/` linting clean, the twin of the formatting `workspace lint curios-format` holds, under a doc comment saying it holds the formatting too.
- **Biome's list is stated three times.** What `biome.json` lists is restated by `variables.web` and by the paths the editors' Biome steps hand it, held by `the_workspace_s_biome_steps_reach_every_file_biome_reads_of_it`.
- **Biome runs as eight steps over four slices of one list.** `workspace`, `tree-sitter`, `textmate` and `vscode` each format and lint their part, 10, 4, 4 and 3 of the 21 files `biome format .` reads at `3a407eed0`, and `format web` writes all of it at once.
- **The subjects are named for a layout, a moment and a nesting.** `workspace` holds the shipped crates' lint, suite and caches, named for the Cargo workspace whose default members they are; `compiler` the build as it is released and its pages, which leaves no name for releasing it; `launcher` one step only the compiler consumes; and `workspace test` a group of one step, `suite`. `xtask`'s README names a `grammar` subject the files do not hold and leaves `textmate` out.
- **Two topics of the documentation are named for what they are not.** `standard-library` holds `/std`'s capabilities and laws, which the documents call the prelude's, and `tools` every decision a programmer meets of the toolchain, which `.claude/rules/tooling.md` names tooling.
- **Unicode's conformance files are built into every build.** `curios-unicode`'s build script fetches `GraphemeBreakTest.txt` and `NormalizationTest.txt`, 3.0 MB only tests read, beside the files its tables are built from, and writes their cases into `tables.rs`: 2.0 MB of its 2.5 MB at `3a407eed0`, counted, which rustc reads in every build of the crate, for every profile and target, though only a test or `test-support` compiles it.

## Goal

- `conformance`, a subject of two steps, `json` and `toml`: Curios programs, `curios run conformance/<suite>.crs -- {.corpora.<suite>}`, which list the corpus, decode each case and exit nonzero naming every case answered otherwise. A valid TOML document's value is held to the tagged JSON beside it as `toml-test` compares them, its scalars read by `/std`'s own readers, `Int/of_str`, `Flt/of_str` and `/std/time/rfc3339`.
- `corpora`, a table of the root file beside `tools`: each a commit of a repository, fetched once a checkout as its forge's archive at that commit, of which one directory is kept as `xtask/.artifacts/<name>-<commit>`, checked by the digest of the files it holds, and trusted by its presence once kept, as a tool's copy is. A step names one as `{.corpora.<name>}`, the kept directory, which orders its fetch as a tool's name orders the tool's. The digest is the SHA-256 of the lines `<sha256>  <path>` of every file under the directory, sorted by path, as Go's `h1:` is defined, so a reader retakes it with `sha256sum`.
- `jig agents`, a check the runner holds, as `zed check` is: every glob of a rule matches a tracked file, and every citation of an agent file is in the tree; `agents.rs` goes.
- `compiler`, today's `workspace` with the launcher folded in: `compiler launcher`, `compiler lint` (`rustfmt`, `curios-format`, `curios-lint`, `clippy`), `compiler test`, a step, and `compiler caches`. `curios-lint` runs `curios lint` over the tree's programs, `programs/` and `conformance/`, and `programs.rs` goes.
- `distribution`, today's `compiler`: `distribution build`, the compiler as it is released, and `distribution test`, its pages; `release` stays free for the act of releasing it, as rust-analyzer's xtask keeps `dist` apart from `release`.
- `biome`, a subject whose `lint` formats and lints what `biome.json` lists, handed the root, and `format biome` in place of `format web`; the Biome steps of `compiler`, `tree-sitter`, `textmate` and `vscode` go, `variables.web` with them, and the test holding it.
- `documentation/roadmap/` and `documentation/design/`'s `standard-library` named `prelude` and `tools` named `tooling`, every link to them restated.
- Unicode's two conformance files fetched by `curios-unicode`'s test support through `curios-download` at their pinned digests — an optional dependency the `test-support` feature turns on, since `curios-prelude`'s tests read the cases through it, and a dev-dependency for the crate's own — and the build script fetching only what the tables are built from, the database's version one constant in a file both include.

## Settled

- **A test checks the code and a step the tree** (decided by the user on 2026-11-23). A crate that reads a file as its given keeps its checks as tests: `curios-unicode`'s and `/std`'s conformance to the cases the crate pins, the formatter's laws over the tree's sources, the parser's words where the editors' grammars state them, and the documentation's examples, the compiler holding its documented behaviour (`curios/src/tests/readme.rs`, `reference.rs`).
- **The agent files' checks are the runner's**, a check of the tree as the Zed extension's are (decided by the user on 2026-11-23), under `jig`, whose subject the handoff already requires.
- **Biome is a subject, its list `biome.json`'s alone**, which Biome reads from the root, as a tree's lint holds its formatting.
- **A subject is named for what it holds** (decided by the user on 2026-11-23): `compiler` for the product its crates are, holding the launcher only it consumes; `distribution` for the compiler as it ships, `release` left to releasing it; a group only where its steps are run apart, so the suite is a step. The editors' four subjects, `bundle`, `native`, `conformance`, `docs`, `jig` and `profile` stay subjects of their own, each a job on its own machine or a part a reader runs alone.
- **A topic is named for what it holds** (decided by the user on 2026-11-23): `prelude`, `/sys` and `/std` as every program gets them, and `tooling`, as `.claude/rules/tooling.md` names the area.
- **A corpus is downloaded, not vendored.** It is pinned at a commit and checked by the digest of the files kept, since the archive a forge generates at a commit is not byte-stable where its contents are: GitHub holds no archive's bytes stable, Go's module hash (`h1:`, a digest of the file tree) and Nix's `fetchFromGitHub` check what was unpacked for the same reason. One directory of the archive is kept, since JSONTestSuite's at `1ef36fa` is 63.2 MB for the 1.6 MB of `test_parsing/` (counted).
- **A corpus is no tool**: no executable, and nothing told apart by host, so it is a table of its own sharing the fetch.
- **A corpus's check is written in Curios**, a program over `File`, `fs` and the codec it holds, exercising `/std` as a program does.

**Rejected.** The corpora vendored into the tree, a second copy of what a commit already pins. A crate of their own whose build script fetches them, where `xtask` fetches what a step names. The archive's bytes digested, which a forge's recompression breaks. A corpus as a `tools` entry, one name for an executable and a directory of data, with a row for each host where a corpus has one. The checks as Rust tests the cases are handed to. Unicode's conformance as a step: a second pin of the database, or a path into `curios-unicode`'s kept files. Biome's paths read off `biome.json` by the runner, which Biome reads itself. Biome's steps per subject, four slices of one list. `curios lint` run once a program, a list the files would restate; `programs/` made a package so a bare `curios lint` reaches it, where the corpus is loose by design. `rust`, `cargo`, `crates` or `curios` for the compiler's subject: the first two name what every other subject shares, the third nothing of what the crates are, and the last the language. One subject for the editors, and `bundle`, `native` and `conformance` folded into `compiler`: a nesting no reader runs by. `release` for the build as it ships, the name of the act of releasing it.

## Stages

**1. `jig agents`**, `agents.rs` gone. Check: the step passes, and fails where it is made to — a glob matching nothing, a citation of a moved file.

**2. The subjects reshaped**: `compiler`, `distribution`, `biome` and `curios-lint`, with `CLAUDE.md`, `.claude/`, the READMEs, the handoff's decision and both workflows restated. Check: each step passes, and fails where its check is made to — a program that lints, a file Biome would reformat — and `every_step_the_handoff_runs_has_a_job` holds the workflow's new jobs.

**3. Unicode's conformance files fetched for tests.** Check: a build fetches and writes only the tables' files, the tests read the two beside them, and a conformance file altered is refused at its digest.

**4. The `corpora` table.** Check: a corpus fetched and kept as its one directory, and refused where what it unpacks differs from its digest; its digest retaken with `sha256sum` equal to the pinned one.

**5. `json`.** Check: the step passes, and fails naming a case `Json/decode` is made to answer otherwise.

**6. `toml`.** Check: the same, and fails naming a valid document whose value is made to differ.

**7. The topics renamed**: `prelude` and `tooling`, every link restated, taken last, after any branch editing a file under either has landed. Check: `cargo xtask docs build` and `jig agents` hold every link.

## Verification and retirement

Each stage's check, then `cargo xtask compiler lint`, the jig's lint, the suite, `cargo xtask docs build` and `cargo xtask handoff`'s new requirements. Once stage 7 lands, this file is deleted.
