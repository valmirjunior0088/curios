# xtask

The workspace's build recipes as a cargo subcommand: `cargo x runtime` files the slim launcher and `build` builds the compiler that embeds it; `fmt`, `fmt-check`, `clippy`, `test` and `doctest` are the workspace's own checks; `js` builds the browser bundle and `js-test` runs its Node suite against it, and `docs` builds the workspace's Rust documentation; `grammar-install`, `grammar-test`, `vscode-install`, `vscode-test`, `vscode-package`, `zed-fmt`, `zed-fmt-check`, `zed-clippy`, `zed-build` and `zed-test` are the editor trees' own; `release`, `installer`, `profile`, `benchmarks` and `clean` are the occasional ones. The five npm recipes need `npm` on `PATH`, `js` needs the `wasm32-unknown-unknown` target, `js-test` that target and Node 22 or later, and the Zed recipes `wasm32-wasip2`; every other recipe needs cargo alone, `release` needs git beside it and reaches for `gh` only to report, and `installer` renders the release's install script from `templates/`. The alias lives in `.cargo/config.toml`; which steps the gate holds is [Every gate step catches what no other step does](../documentation/design/architecture/every-gate-step-catches-what-no-other-step-does.md), and the recipes and their reasons are the crate's own documentation.

## Decisions

### Recipes are a cargo subcommand, not a Makefile

**Decision.** Every build recipe is a subcommand of this crate, reached through the `x` alias, and the workspace has no Makefile.

**Rationale.** A fresh clone needs exactly one tool, cargo, on Linux and macOS alike, and each recipe's steps beyond cargo — copying the launcher, generating the browser bindings, running a container — are stated in Rust with one error handling, without a shell between the recipe and cargo.

**Rejected.** A build script for the browser bundle, which runs before `curios-js` compiles and contends for the target-directory lock with a nested cargo; a Makefile beside this crate, a second entry point.

### Every gate step is a recipe

**Decision.** Each step of the hand-off gate, `/full-gate`, is one recipe whose arguments are written in this crate, and no recipe forwards arguments to the tool it runs: a recipe may take a parameter it places itself — a release's version, a program to profile, a package to narrow a check to — but never a tail it hands on unread. Every `run:` line in the check workflow is one of those recipes. A recipe names a tool and a tree: cargo at the workspace root, npm in the two editor packages under `editors/`, cargo in the Zed extension's own workspace.

**Rationale.** A step spelled in prose is copied into the workflow and a contributor's terminal, and the copies drift silently, because the flags decide how much a step checks: `--all-targets --all-features -- -Dwarnings` is a lint gate in one spelling and something weaker in another. As recipe names, the gate and the workflow are the same names, and `cargo x --help` is the roster.

**Rejected.** A passthrough beside the fixed recipes, two spellings for one action; a `gate` recipe running every step, when the steps are long, measured one at a time, and run by the check workflow as eight parallel jobs; a test asserting that the gate's prose lists these recipes, a test of a Markdown fence.

### The bindings generator is a library dependency

**Decision.** `js` calls `wasm-bindgen-cli-support`, the crate the `wasm-bindgen` command line wraps, pinned in the workspace manifest beside `wasm-bindgen` itself.

**Rationale.** The generator must match the `wasm-bindgen` crate's version exactly; as a dependency the match is the lockfile's, and the generator refuses a module built against another version, naming both.

### The release dance is a recipe

**Decision.** `release` cuts a release end to end. It reads the workspace version from `Cargo.toml`, resolves the one its argument names, refuses unless the tree is clean, the branch is `main`, `main` agrees with `origin/main`, the tag is free and the target is above the current version, then writes the manifest, updates the lock, checks the diff it produced, commits, tags, and pushes `main` and the tag. Invoking it is the intent to publish.

**Rationale.** The tag push fires `release.yml`, so it is the step whose spelling this crate exists to fix. The version is computed from the manifest rather than recalled, and "the diff is that one line plus the lock's member versions" is an assertion rather than a reading. The monotonicity refusal closes what the tag check cannot see: the history skips ranges — `0.1.0` is followed by `0.2.1` — so a mistyped version landing in a gap has no tag to collide with.

**Rejected.** Stopping before the pushes, which leaves the irreversible step in prose; consulting the check workflow, which has already answered for the commits on `main`; undoing a failed dance, which is destructive and the user's to ask for — the recipe prints what exists and what would undo it; testing the dance itself, which would cut a release, where every judgment it makes before and after writing is tested.

### A command is run or asked, never both

**Decision.** `run` and `run_in` spawn a tool to do something: they echo the command line and inherit the terminal's streams. `ask` spawns one to ask something: it echoes nothing, captures stdout and hands it back, and leaves stderr inherited. Both go through one private spawn holding the echo, the status check and the error text.

**Rationale.** A step's output is the user's — it streams live and in colour, and the echoed command line makes the terminal a transcript of the work — while a question's output is the program's, and streaming it prints work that is not being done. An inherited stderr lets a failing question explain itself in the tool's own words.

**Rejected.** One verb that always captures, which costs `test` and `fmt-check` their colour for a buffer nobody reads; a `git` verb beside `cargo`, `grammar`, `vscode` and `zed`, which name a tree where git runs at the root — `release` wraps its own calls privately, as `installer` owns `validated`.
