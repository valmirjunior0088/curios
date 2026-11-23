# A check of the tree is a step, and a test checks the code

**Adopted by the user on 2026-11-22 and restated on 2026-11-23; its next step is stage 1, a ready delivery; it extends the fetching [`xtask`'s README](../../../xtask/README.md#a-tool-a-step-runs-is-fetched-at-its-digest-and-named-by-the-step) states.** A test holds the code to what it promises, and a step holds the tree to what it states: a standard's corpus, a revision published elsewhere, a copy agreeing with its original, a citation reaching its file, a program linting clean. The task files are descriptions the runner reads, and nothing outside the runner's crate reads them.

## What is wrong today

- **Two topics of the documentation are named for what they are not.** `standard-library` holds `/std`'s capabilities and laws, which the documents call the prelude's, and `tools` every decision a programmer meets of the toolchain, which `.claude/rules/tooling.md` names tooling.

## Goal

- `documentation/roadmap/` and `documentation/design/`'s `standard-library` named `prelude` and `tools` named `tooling`, every link to them restated.

## Settled

- **A test checks the code and a step the tree** (decided by the user on 2026-11-23). A crate that reads a file as its given keeps its checks as tests: `curios-unicode`'s and `/std`'s conformance to the cases the crate pins, the formatter's laws over the tree's sources, the parser's words where the editors' grammars state them, and the documentation's examples, the compiler holding its documented behaviour (`curios/src/tests/readme.rs`, `reference.rs`).
- **A topic is named for what it holds** (decided by the user on 2026-11-23): `prelude`, `/sys` and `/std` as every program gets them, and `tooling`, as `.claude/rules/tooling.md` names the area.

## Stages

**1. The topics renamed**: `prelude` and `tooling`, every link restated, taken last, after any branch editing a file under either has landed. Check: `cargo xtask docs build` and `jig agents` hold every link.

## Verification and retirement

Each stage's check, then `cargo xtask compiler lint`, the jig's lint, the suite, `cargo xtask docs build` and `cargo xtask handoff`'s new requirements. Once stage 1 lands, this file is deleted.
