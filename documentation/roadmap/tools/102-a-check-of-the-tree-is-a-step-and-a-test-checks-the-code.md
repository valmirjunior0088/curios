# A check of the tree is a step, and a test checks the code

**Adopted by the user on 2026-11-22 and restated on 2026-11-23; its next step is stage 1, a ready delivery; it extends the fetching [`xtask`'s README](../../../xtask/README.md#a-tool-a-step-runs-is-fetched-at-its-digest-and-named-by-the-step) states.** A test holds the code to what it promises, and a step holds the tree to what it states: a standard's corpus, a revision published elsewhere, a copy agreeing with its original, a citation reaching its file, a program linting clean. The task files are descriptions the runner reads, and nothing outside the runner's crate reads them.

## What is wrong today

- **A corpus is run by nothing.** At `3a407eed0`, counted: `Toml/decode` refuses all 509 invalid TOML 1.0 documents of `toml-test` at `96fd088` and reads all 208 valid ones, each to the value its tagged JSON states, a NaN of either sign being a NaN and floats compared as IEEE compares them; `conformance json` holds `Json/decode` to JSONTestSuite, and nothing holds `Toml/decode` so.
- **Two topics of the documentation are named for what they are not.** `standard-library` holds `/std`'s capabilities and laws, which the documents call the prelude's, and `tools` every decision a programmer meets of the toolchain, which `.claude/rules/tooling.md` names tooling.

## Goal

- `conformance toml`, beside `conformance json`: a Curios program, `conformance/toml.crs`, over toml-test's `tests/` at `96fd088`, a corpus of the `corpora` table, which reads the TOML 1.0 list, decodes each case and exits nonzero naming every case answered otherwise: an invalid document read, a valid one refused, or a valid one whose value is not the tagged JSON beside it as `toml-test` compares them, its scalars read by `/std`'s own readers, `Int/of_str`, `Flt/of_str` and `/std/time/rfc3339`.
- `documentation/roadmap/` and `documentation/design/`'s `standard-library` named `prelude` and `tools` named `tooling`, every link to them restated.

## Settled

- **A test checks the code and a step the tree** (decided by the user on 2026-11-23). A crate that reads a file as its given keeps its checks as tests: `curios-unicode`'s and `/std`'s conformance to the cases the crate pins, the formatter's laws over the tree's sources, the parser's words where the editors' grammars state them, and the documentation's examples, the compiler holding its documented behaviour (`curios/src/tests/readme.rs`, `reference.rs`).
- **A topic is named for what it holds** (decided by the user on 2026-11-23): `prelude`, `/sys` and `/std` as every program gets them, and `tooling`, as `.claude/rules/tooling.md` names the area.
- **A corpus's check is written in Curios**, a program over `File`, `fs` and the codec it holds, exercising `/std` as a program does.

**Rejected.** The corpora's checks as Rust tests the cases are handed to.

## Stages

**1. `toml`.** Check: the step passes, and fails naming a case `Toml/decode` is made to answer otherwise, a valid document whose value is made to differ among them.

**2. The topics renamed**: `prelude` and `tooling`, every link restated, taken last, after any branch editing a file under either has landed. Check: `cargo xtask docs build` and `jig agents` hold every link.

## Verification and retirement

Each stage's check, then `cargo xtask compiler lint`, the jig's lint, the suite, `cargo xtask docs build` and `cargo xtask handoff`'s new requirements. Once stage 2 lands, this file is deleted.
