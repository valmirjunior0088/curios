# xtask

The workspace's build recipes as a cargo subcommand: `cargo x runtime` files the slim launcher and `build` builds the compiler that embeds it; `fmt`, `fmt-check`, `clippy`, `test` and `doctest` are the workspace's own checks; `js` builds the browser bundle and `js-test` runs its Node suite against it, and `rust-docs` and `std-docs` build the two halves of the documentation; `grammar-install`, `grammar-test`, `vscode-install`, `vscode-test`, `vscode-package`, `zed-fmt`, `zed-fmt-check`, `zed-clippy`, `zed-build` and `zed-test` are the editor trees' own; `release`, `installer`, `profile`, `benchmarks` and `clean` are the occasional ones. The five npm recipes need `npm` on `PATH`, `js` needs the `wasm32-unknown-unknown` target, `js-test` that target and Node 22 or later on `PATH`, and the Zed recipes `wasm32-wasip2`; every other recipe needs cargo alone, `release` needs git beside it and reaches for `gh` only to report, and `installer` renders the release's install script from the template under `templates/` without even that. The alias lives in `.cargo/config.toml`; the recipes and their reasons are the crate's own documentation.

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

### The release dance is a recipe

**Decision.** `release` cuts a release end to end. It reads the workspace version from `Cargo.toml`, resolves the one its argument names, refuses unless the tree is clean, the branch is `main`, `main` agrees with `origin/main`, the tag is free and the target is above the current version, and then writes the manifest, updates the lock, checks the diff it produced, commits, tags, and pushes `main` and the tag. Invoking it is the intent to publish.

**Rationale.** The tag push is what fires `release.yml`, so it is precisely the step whose exact spelling this crate exists to fix — and a dance spelled in prose drifts the way every step spelled in prose drifts, besides needing someone to read it before a release can be cut at all. Two of its steps are stronger here than in prose: the version is read from the manifest and computed rather than recalled, and "the diff is that one line plus the lock's member versions" is an assertion rather than a reading. The monotonicity refusal closes what the tag check cannot see: this history skips ranges — `0.1.0` is followed by `0.2.1`, and every minor bump abandons the rest of its patch range — so a mistyped version landing in a gap has no tag to collide with.

**Rejected.** Stopping before the pushes, which leaves the irreversible step in prose, which is where the drift would come back. Consulting the check workflow, which runs on every push to `main` and has already answered for the commits being released, while the tag's own workflow builds without it. Undoing a failed dance: removing a local commit or tag is destructive and so the user's to ask for, and the recipe prints what exists and what would undo it instead. Testing the dance itself, which would have to cut a release — what is testable without one is every judgment it makes before it writes and the judgment it makes about what it wrote, and those are the tests it has.

### A command is run or asked, never both

**Decision.** `run` and `run_in` spawn a tool to *do* something: they echo the command line and inherit the terminal's streams. `ask` spawns one to *ask* something: it echoes nothing, captures stdout and hands it back, and leaves stderr inherited. Both go through one private spawn, which is where the echo, the status check and the error text live.

**Rationale.** These are different acts, not one act with two return types. A step's output is the user's — it has to stream live and in colour, and the echoed command line is what makes the terminal a transcript of the work. A question's output is the program's, and streaming it prints work that is not being done: a preflight that echoed `git rev-parse HEAD` five times and dumped the whole text of `git diff` would bury the report it exists to produce. Leaving stderr inherited on a question is what lets a failing `git rev-parse origin/main` explain itself in git's own words, with no relaying here.

**Rejected.** One verb that always captures and returns, with callers ignoring the value they do not want. It reads as the smaller design and is not: piping every step's stdout costs `test` and `fmt-check` their colour, since only stderr stays a terminal, and it buys a buffer nobody reads. A `git` verb beside `cargo`, `grammar`, `vscode` and `zed` — those name a *tree*, and git runs at the root, which both spawn verbs already default to; `release` wraps its own dozen calls privately, as `installer` owns `validated`.
