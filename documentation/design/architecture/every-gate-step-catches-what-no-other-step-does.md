# Every gate step catches what no other step does

**Decision.** The hand-off gate, `/full-gate` (`.claude/commands/full-gate.md`), is a minimal set: a step earns its place by being the only thing that fails when something specific is wrong, and a step whose findings another reports is removed rather than kept for reassurance. Each step's sole catch is stated here, and a step added later states its own first. That each step is one `cargo x` recipe is [`xtask`'s own decision](../../../xtask/README.md).

- **`runtime`** is a prerequisite rather than a check: it builds the slim launcher in its own Cargo invocation, which `curios` embeds.
- **`fmt-check`** alone fails on formatting drift.
- **`clippy`** compiles the workspace over every target and feature with warnings denied, the same compilation as `cargo check` with more lints. It stops at erasure and certification, over the whole standard library.
- **`test`** alone reaches `curios-cont`, `curios-emit` and `curios-wasm`, through the cross-stage corpus, and renders `/std`'s pages end to end through `curios document --std`.
- **`doctest`** alone compiles and runs what a `///` block asserts, since cargo's every target excludes doctests. It passes over an empty set, so the first example written is run the day it is written.
- **`docs`** alone runs rustdoc, under `[workspace.lints.rustdoc] all = "deny"` inherited through `[lints] workspace = true`, over private items because the crates state their invariants on `pub(crate)` ones; a broken intra-doc link is checked nowhere else.
- **`js-test`** alone runs a compiled program under a JavaScript engine against the browser harness's own host, building the bundle for `wasm32-unknown-unknown` first.
- **The grammar and VS Code steps** install their npm trees from the lock file and test them; `vscode-package` alone runs `vsce` over the extension's manifest and bundles it as it ships, which the grammar snapshots never load.
- **The Zed steps** format, lint, build for `wasm32-wasip2` and test the extension, and `zed-test` checks that `editors/zed/extension.toml`'s grammar rev is the grammar the checkout holds, since Zed installs the grammar from a pushed commit. It fails exactly on a regenerated grammar not yet pushed, which is the two-commit sequence those moves follow.

**Rationale.** A gate is read under time pressure, and a reader who cannot tell which step would have caught a mistake starts skipping the slow ones. Naming each unique catch makes that judgement unnecessary: nothing in the list is redundant, so nothing is optional.

**Rejected.**

- **`cargo check` beside `clippy`**, the same compilation reporting a subset; **`cargo x js` beside `js-test`**, the same bundle.
- **A step rendering `/std`'s pages**, which the test suite already renders through the same command.
- **Denying rustdoc's lints through an environment variable**, a second place to set it and so one to forget.
- **Leaving the grammar rev to CI alone**: the check is cheap and local, and only its failure mode is remote.
