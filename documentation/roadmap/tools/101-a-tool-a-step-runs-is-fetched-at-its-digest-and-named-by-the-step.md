# A tool a step runs is fetched at its digest and named by the step

**Adopted by the user on 2026-11-22; its next step is stage 1, a ready delivery.** [`xtask`'s README](../../../xtask/README.md#a-tool-a-step-runs-is-fetched-at-its-digest-and-named-by-the-step) states the rule: each tool a step runs is declared in the task files with its download and SHA-256 for each host, named by the step that runs it, and fetched before a run's first step. One tool the steps reach is not held to it yet, and the workflows' own run of the rule waits on a push.

## What is wrong today

- **One tool is fetched by an npm tree, unchecked.** The tree-sitter steps run the CLI through `editors/tree-sitter`'s npm scripts, whose one dependency, `tree-sitter-cli`, downloads the binary from its install script, outside the lock file's integrity and any digest of ours; the tree, its lock file and its `install` step exist for that binary alone.

## Goal

- The tree-sitter steps name the CLI, a tool of `xtask/tasks.toml` fetched as tree-sitter's gzipped binary for each host, `tree-sitter-{linux,macos}-{x64,arm64}.gz`, at GitHub's digest of the release asset; and `editors/tree-sitter` holds the grammars and no npm tree.

## Settled

- **Generation runs under the fetched Node.** `tree-sitter generate` evaluates `grammar.js`, and the Node every other step's JavaScript runs under is the one engine that wrote the committed parsers; its built-in QuickJS would be a second engine to keep in agreement with the first.
- **A parser is compiled into the checkout.** tree-sitter keeps a compiled parser by the grammar's name in a cache of the user's, which every checkout of the machine shares, so two checkouts load each other's parser or wait on each other's lock; each grammar is built with `--output` into `editors/tree-sitter/.artifacts`, a produced file the steps that parse load with `--lib-path`.
- **The sources are read from git.** The test's `find` over the library, the corpus and the board becomes a variable whose command lists the tracked sources, so the steps run no shell.

## Stages

**1. tree-sitter fetched.** The tree-sitter steps name the CLI and read the sources they parse from a variable; `editors/tree-sitter`'s `package.json`, lock file and `install` step retired, the Zed and tree-sitter jobs' npm cache dropped, and the documents naming its `npm test` restated. Check: `cargo xtask tree-sitter test` and `cargo xtask zed test` pass, the committed parsers unchanged.

**2. The workflows run it.** Check: both workflows pass on the push that carries the effort, the macOS job's nextest fetched for `aarch64-apple-darwin`. A push is the user's to give.

## Verification and retirement

Stage 1's check, then `cargo xtask jig lint`, `cargo xtask jig test` and `cargo xtask docs build`. Once stage 1 lands, this file is deleted, and stage 2's check is reported as pending on the push.
