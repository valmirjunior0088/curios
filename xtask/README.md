# xtask

The workspace's build recipes as a cargo subcommand: `cargo x runtime` files the slim launcher and `build` builds the compiler that embeds it; `fmt`, `fmt-check`, `clippy`, `test` and `doctest` are the workspace's own checks; `js` builds the browser bundle, and `rust-docs` and `std-docs` the two halves of the documentation; `grammar-install`, `grammar-test`, `vscode-install`, `vscode-test`, `vscode-package`, `zed-fmt`, `zed-fmt-check`, `zed-clippy`, `zed-build` and `zed-test` are the editor trees' own; `installer`, `profile`, `benchmarks` and `clean` are the occasional ones. The five npm recipes need `npm` on `PATH`, `js` needs the `wasm32-unknown-unknown` target and the Zed recipes `wasm32-wasip2`; every other recipe needs cargo alone, and `installer` renders the release's install script from the template under `templates/` without even that. The alias lives in `.cargo/config.toml`; the recipes and their reasons are the crate's own documentation.

## Decisions

### Recipes are a cargo subcommand, not a Makefile

**Decision.** Every build recipe is a subcommand of this crate, reached through the `x` alias, and the workspace has no Makefile.

**Rationale.** A fresh clone then needs exactly one tool, cargo, on Linux and macOS alike. Each recipe is cargo with flags and whatever step cargo does not do — copy the launcher, generate the browser bindings, run a container — and Rust states those steps with the same error handling on Linux and macOS, without a shell between the recipe and cargo.

**Rejected.** A build script for the browser bundle: it would run before `curios-js` compiles, so there is nothing to generate bindings from, and a nested cargo inside a build script contends for the target-directory lock. Keeping the Makefile beside this crate: two entry points is the cost this crate removes.

### Every gate step is a recipe

**Decision.** Each step of `CLAUDE.md`'s hand-off gate is one recipe whose arguments are written in this crate, and no recipe forwards arguments to the tool it runs. A recipe may take a parameter it places itself — a release's version, a program to profile, a package to narrow a check to — but never a tail it hands on unread. Every `run:` line in the check workflow is one of those recipes. A recipe names a tool and a tree: cargo at the workspace root, npm in the two editor packages under `editors/`, cargo in the Zed extension's own workspace.

**Rationale.** A step spelled in prose is copied into the check workflow and into a contributor's terminal, and the copies drift silently, because the flags are what decide how much a step checks: `--all-targets --all-features -- -Dwarnings` is a lint gate in one spelling and something weaker in another. As recipe names, the gate and the workflow's `run:` lines are the same names, so a step changes in one place and `cargo x --help` is the roster.

**Rejected.** A passthrough beside the fixed recipes, which is two spellings for one action and reopens the drift the fixed ones close; an ad-hoc run belongs in the tree, where that tree's README already sends the reader. A `gate` recipe running every step: the steps are long, they are run and measured one at a time, and the check workflow runs them as nine parallel jobs. A test asserting that the gate's prose lists exactly these recipes, which is a test of a Markdown fence.

### The bindings generator is a library dependency

**Decision.** `js` calls `wasm-bindgen-cli-support`, the crate the `wasm-bindgen` command line wraps, pinned in the workspace manifest at the version of `wasm-bindgen` itself.

**Rationale.** The generator must match the `wasm-bindgen` crate version exactly. As a dependency that match is the lockfile's — the two rows sit together in the workspace manifest — and the generator refuses a module built against another version, naming both, so a drift fails loudly.
