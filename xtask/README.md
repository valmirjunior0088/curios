# xtask

The repository's steps as a cargo subcommand, `cargo xtask <subject> [action]`: one authored table says what each step is about, what it does, what it is run after, what it needs and the one thing only it catches, and the command line, a bare subject's sequence and what the check workflow must run are read off it. The subjects are the four trees — `workspace`, `grammar`, `vscode`, `zed` — and what is built from the workspace's: `compiler`, `launcher`, `bundle`, `docs` and `instrumented`. No command runs every subject; `release`, `installer`, `profile`, `profile-prelude` and `clean` are commands of their own, outside the table. A test step needs `cargo-nextest`, the bundle's tests Node, the npm trees and every JavaScript check `npm`, and the bundle and the Zed extension the two Wasm targets `rust-toolchain.toml` installs; every other step needs cargo alone, and `release` needs git beside it and reaches for `gh` only to report. The alias lives in `.cargo/config.toml`; why the gate holds the steps it does is [Every gate step catches what no other step does](../documentation/design/every-gate-step-catches-what-no-other-step-does.md), and the steps and their reasons are the crate's own documentation.

## Decisions

### Steps are a cargo subcommand, not a Makefile

**Decision.** Every step is reached through this crate, by the `xtask` alias, and the workspace has no Makefile.

**Rationale.** A fresh clone reaches every step through one tool, cargo, on Linux and macOS alike, and what a step does beyond cargo — copying the launcher, generating the browser bindings — is stated in Rust with one error handling, without a shell between the step and cargo.

**Rejected.** A build script for the browser bundle, which runs before `curios-js` compiles and contends for the target-directory lock with a nested cargo; a Makefile beside this crate, a second entry point.

### A step is an action on a subject, stated once

**Decision.** `steps.rs` is one table, a row a step: its subject, its action, the steps run first whenever it is asked for, the tools it needs beyond cargo, the one path condition a step may carry, and the one thing only it catches. The order of the rows is the order steps run in. Everything else is read off the rows. The command line is `cargo xtask <subject> [action]`, each subject a subcommand whose actions are an enum of its own, so an action a subject lacks is the parser's error. A bare subject is every step about it. An action's help is what its steps catch. No step forwards arguments to the tool it runs: an action may take a parameter it places itself — a package to narrow a check to, a name to narrow a test run by — but never a tail it hands on unread.

A subject is a tree or something built from one. A tree is a manifest root — the Rust workspace, the two npm packages under `editors/`, the Zed extension's own workspace — and a tracked file belongs to the tree of its nearest one. Format and lint are a tree's, over every file it holds, whatever its language; build, test and install belong to what is built: `compiler`, `launcher`, `bundle` and `docs` from the workspace, and `instrumented`, the workspace built with every feature.

**Rationale.** A fact about a step stated in several places drifts: what runs before what was written in the steps' own code, in the gate's prose and in the check workflow, and which flags a step passes decide how much it checks. One table makes agreement a property of the code, as the compiler's own contract table does for its commands ([An argument names one subject, and each command states what it accepts](../documentation/design/tools/an-argument-names-one-subject-and-each-command-states-what-it-accepts.md)). The subject comes first because a person works on one thing: `cargo xtask vscode` is everything about the extension, and the same word means the same thing under every action. Ownership by the nearest manifest root is computed rather than listed, so a file added anywhere is already some tree's and no list has to be kept complete.

**Rejected.**

- **A name per step that fuses a tool and a tree** (`zed-clippy`, `grammar-install`), which encodes one of three things about a step and leaves the rest to prose.
- **The action first with the subject its argument**, which makes a pair no subject has writable, to be refused in a sentence rather than by the parser.
- **A subject per language** (`js`, `rust`), which gives one word a second meaning beside what is built, and cannot lint Rust at all, since what a Rust lint sees depends on the features and the target of what is built.
- **Lanes**, a second grouping of steps beside subjects, and a machine in a row: a step runs wherever the checkout is, and which machine runs which subject is the workflow's to say.
- **A passthrough beside the fixed steps**, two spellings for one action.

### A step runs in the configuration that ships

**Decision.** `workspace`'s lint and tests build with default features, and every product builds as it is released. What only a feature compiles is `instrumented`'s to lint, with every feature on, and `docs` documents every feature. The compiler's suite is compiled once, by `workspace test`: no step runs it in a second configuration. The recorder's tests are the one second run, `curios-profile` under its `enabled` feature: that crate depends on nothing of the compiler, so the run compiles it alone. The workspace's lint table denies warnings, so no step passes a flag to deny them and a build no step lints fails on one all the same.

**Rationale.** A check that runs in a configuration nothing ships finds what that configuration has and misses what the shipped one has: an import only a backend reads is used with every feature on and unused in the launcher. One configuration for the day-to-day lint and tests is also one build of the prelude's images for both. Denying warnings where the lints are stated holds every build anyone runs, in whatever configuration, without a list of configurations to keep.

**Rejected.**

- **Every feature for the day-to-day lint**, which sees all the code at once and never the compiler a user runs, and pays a second build of the prelude's images beside the tests'.
- **A lint pass per product**, a list of configurations to maintain where the lint table needs none.
- **A crate of its own for the recorder**, so that its tests need no feature: a crate, a second manifest naming `tracing` and every taker repointed, for tests one narrow step reaches.
- **A test run with every feature on**, for the tests the profiling feature adds: a second compilation of the whole suite for a handful of tests, which are taken by hand as the measurements beside them are.
- **Denying warnings through an environment variable or a flag**, a second place to set it and so one to forget; Cargo's own `build.warnings`, which the pinned toolchain does not yet read.

### What is not Rust is formatted and linted by Biome

**Decision.** The JavaScript, JSON and CSS of every tree are formatted and linted by Biome, run through `npx` at the one exact version this crate states, under the one `biome.json` at the repository's root. A tree's `format` and `lint` hand Biome the paths the tree holds, so the workspace's cover the browser harness and the documentation pages' script and stylesheet, and each npm tree's cover its own. A warning fails as an error does, as the workspace's Rust is held by its lint table. The workspace's Rust that belongs to no package, the corpus's Rust twins under `programs/`, is formatted by `workspace format` beside the packages, under the workspace's edition.

**Rationale.** Nothing held these files: the browser harness the compiler's output runs under and the script every documentation page loads were neither formatted nor linted. One tool reads the three languages under one configuration, and `npx` fetches it on first use into npm's own cache, so the repository's root gains no `package.json`, no lock file and no `node_modules` beside the Rust workspace. The version is exact because a formatter's output moves between releases, and it is stated here because this crate is what runs it.

**Rejected.** A `package.json` at the root to pin it, which makes the repository an npm package for one tool's sake; a development dependency in each npm tree, two pins of one tool and none for the workspace's own JavaScript; a linter and a formatter as two tools with a plugin tree between them, for what one binary does.

### A bare subject is the largest run

**Decision.** `cargo xtask <subject>` with no action runs every step about the subject, one after another in the table's order, formatting as a check; it goes on past a step that fails and names the ones that failed where it ends. No command runs every subject. The hand-off gate is `.claude/commands/run-full-gate.md`, which states each subject's command and what orders two of them, and whoever runs it decides what runs together.

**Rationale.** A subject's steps share a tree and a build, so their order is a fact the table states and one process carries out. Across subjects, what runs together is a judgement about a machine and a moment: which builds may share the cores, what a change touched, what is already built. A command that fixes one answer to that runs the longest steps there are whether or not they are wanted, as one process to wait on.

**Rejected.** A `gate` command running every step in an order derived from the rows, each step a process of its own with a log of its own: it needed a second relation in the table beside the order of its rows, a runner to schedule by it and a step reachable by name for the runner to start, all for a run the reader could not take a subject at a time. Running only what a change touched, which needs every step's inputs declared and is left until they are: one step carries a path condition, because it deletes a build to prove it can be made again.

### The check workflow is held to the table

**Decision.** `.github/workflows/check.yml` is written by hand and thin: a job sets up a machine and runs `cargo xtask` lines, and `every_step_has_a_job` reads the file and holds that every line is a command this crate parses and that the lines together run every step of the table. Each tool's version is stated once, by whoever installs it: Rust's in `rust-toolchain.toml`, which rustup reads; nextest's and Node's in the workflow, for its runners; Biome's in this crate, which fetches it. This crate asks of nextest and Node only that they are there.

**Rationale.** A workflow that restates the steps drifts from them silently, and one generated from the table would put runner images, caches and action versions — GitHub's vocabulary — in this crate. Held by a test, the file stays where GitHub reads it and cannot leave a step out.

**Rejected.** Generating the workflow; a workflow that names a toolchain of its own, which named `stable` while the repository pinned a version.

### The bindings generator is a library dependency

**Decision.** `bundle build` calls `wasm-bindgen-cli-support`, the crate the `wasm-bindgen` command line wraps, pinned in the workspace manifest beside `wasm-bindgen` itself.

**Rationale.** The generator must match the `wasm-bindgen` crate's version exactly; as a dependency the match is the lockfile's, and the generator refuses a module built against another version, naming both.

### The suite runs under nextest, a process to a test, as one run

**Decision.** A test step runs under `cargo-nextest` rather than `cargo test`, as one run: it takes a package and a name to narrow the run by and no shard. Nextest is a tool a contributor installs: the step looks for it before building anything and refuses when it is absent, printing the command that installs it.

**Rationale.** Nextest runs each test as a process of its own, and the compiler's last stage wants one. Binaryen's settings are its process's and its optimizer is not safe to call from two threads, so `curios-binaryen` serializes it behind a lock: tests that share a process, as `cargo test`'s do, queue for it however many cores there are, and a test that aborts — Binaryen answering a module it cannot parse with a C++ `assert` — takes every other result of its binary with it. A process to a test runs the suite on every core and fails an abort by name. The suite is compiled optimized ([A test is compiled optimized, with its checks on](../documentation/design/a-test-is-compiled-optimized-with-its-checks-on.md)), so one job runs it and nothing cuts it into slices. Nextest's command line is the interface it versions, so the flags this crate writes are ones a later release still takes.

**Rejected.** `cargo test`, the runner a clone already has: its tests are threads of one process, where the suite waits its turn at Binaryen's lock. Slices of the suite run as parallel jobs, which its time no longer calls for, and with them the oldest nextest that cuts one. Nextest as a library, as the bindings generator is: `nextest-runner` is internal to nextest and breaks with every release, and nothing in the workspace has to match its version, which is the reason `wasm-bindgen-cli-support` is a dependency. A manifest row naming it, since cargo has no stable way to depend on a tool and a `metadata` table says only what something reads out of it. Installing it from the step, which makes a first test run build a tool nobody asked for. Nextest in the workflow alone beside `cargo test` everywhere else, two spellings of one step that also differ in whether tests share a process.

### The release dance is a command

**Decision.** `release` cuts a release end to end. It reads the workspace version from `Cargo.toml`, resolves the one its argument names, refuses unless the tree is clean, the branch is `main`, `origin/main` holds no commit `main` does not, the tag is free and the target is above the current version, then writes the manifest, updates the lock, checks the diff it produced, commits, tags, and pushes `main` and the tag. Invoking it is the intent to publish.

**Rationale.** The tag push fires `release.yml`, so it is the step whose spelling this crate exists to fix. The version is computed from the manifest rather than recalled, and "the diff is that one line plus the lock's member versions" is an assertion rather than a reading. The monotonicity refusal closes what the tag check cannot see: the history skips ranges — `0.1.0` is followed by `0.2.1` — so a mistyped version landing in a gap has no tag to collide with.

**Rejected.** Stopping before the pushes, which leaves the irreversible step in prose; consulting the check workflow, which has already answered for the commits on `main`; undoing a failed dance, which is destructive and the user's to ask for — the command prints what exists and what would undo it; testing the dance itself, which would cut a release, where every judgment it makes before and after writing is tested.

### A command is run or asked, never both

**Decision.** `run` and `run_in` spawn a tool to do something: they echo the command line and inherit the terminal's streams. `ask` spawns one to ask something: it echoes nothing, captures stdout and hands it back, and leaves stderr inherited. Both go through one private spawn holding the echo, the status check and the error text.

**Rationale.** A step's output is the user's — it streams live and in colour, and the echoed command line makes the terminal a transcript of the work — while a question's output is the program's, and streaming it prints work that is not being done. An inherited stderr lets a failing question explain itself in the tool's own words.

**Rejected.** One verb that always captures, which costs a test and a format check their colour for a buffer nobody reads; a `git` verb beside `cargo`, `grammar`, `vscode` and `zed`, which name a tree where git runs at the root — `release` wraps its own calls privately, as `installer` owns `validated`.
