# xtask

The repository's steps as a cargo subcommand, `cargo xtask <subject> [action]`: one authored table says what each step is about, what it does, what it is run after, what it needs and the one thing only it catches, and the command line, a bare subject's sequence and what the check workflow must run are read off it. The subjects are the five trees — `workspace`, `grammar`, `vscode`, `zed` and `xtask`, these recipes' own — and what is built from the workspace's: `compiler`, `launcher`, `bundle`, `docs` and `instrumented`. `gate` runs every step of the table, one after another; `release`, `installer`, `profile`, `profile-prelude` and `clean` are commands of their own, outside the table. A test step needs `cargo-nextest`, the bundle's tests Node, the npm trees and every JavaScript check `npm`, and the bundle and the Zed extension the two Wasm targets `rust-toolchain.toml` installs; every other step needs cargo alone, and `release` needs git beside it and reaches for `gh` only to report. The alias lives in `.cargo/config.toml`; why the gate holds the steps it does is [Every gate step catches what no other step does](../documentation/design/every-gate-step-catches-what-no-other-step-does.md), and the steps and their reasons are the crate's own documentation.

## Decisions

### Steps are a cargo subcommand, not a Makefile

**Decision.** Every step is reached through this crate, by the `xtask` alias, and the workspace has no Makefile.

**Rationale.** A fresh clone reaches every step through one tool, cargo, on Linux and macOS alike, and what a step does beyond cargo — copying the launcher, generating the browser bindings — is stated in Rust with one error handling, without a shell between the step and cargo.

**Rejected.** A build script for the browser bundle, which runs before `curios-js` compiles and contends for the target-directory lock with a nested cargo; a Makefile beside this crate, a second entry point.

### The recipes are a workspace of their own

**Decision.** This crate is its own root, excluded from the compiler's workspace as the Zed extension is, with its own lock file and target directory, reached by `run --manifest-path xtask/Cargo.toml` from the repository's root. It restates the workspace's lint table, and pins `wasm-bindgen-cli-support` beside `curios-js`'s `wasm-bindgen`; `the_lint_table_is_the_workspace_s` and `the_bindings_generator_is_the_bindings_version` hold the two copies to their originals. Its own tree is formatted, linted and tested by the `xtask` subject.

**Rationale.** A member's dependencies unify their features into every `--workspace` build: as a member this crate gave the compiler's `anyhow` its `default` and `hashbrown` its `default-hasher` and `serde` in `workspace lint` and `workspace test`, which no shipped compiler has (counted at `19a7b4a97`, against `-p curios`), so the configuration the day-to-day checks claim to share with the release was not quite it. A member is also built in the compiler's target directory, so every `cargo xtask`, whatever subject it ran, first waited on that directory's lock behind any build of the compiler in progress.

**Rejected.** Membership, the cargo-xtask convention's usual choice, which shares one lock file and lets the wasm-bindgen pair be one row, at the price above; the alias run from any directory, which only membership gives.

### A step is an action on a subject, stated once

**Decision.** `steps.rs` is one table, a row a step: its subject, its action, the steps run first whenever it is asked for, the tools it needs beyond cargo, and the one thing only it catches. The order of the rows is the order steps run in. Everything else is read off the rows. The command line is `cargo xtask <subject> [action]`, each subject a subcommand whose actions are an enum of its own, so an action a subject lacks is the parser's error. A bare subject is every step about it. An action's help is what its steps catch. No step forwards arguments to the tool it runs: an action may take a parameter it places itself — a package to build alone, names to pick tests by — but never a tail it hands on unread.

A subject is a tree or something built from one. A tree is a manifest root — the Rust workspace, the two npm packages under `editors/`, the Zed extension's own workspace — and a tracked file belongs to the tree of its nearest one. Format and lint are a tree's, over every file it holds, whatever its language; build, test and install belong to what is built: `compiler`, `launcher`, `bundle` and `docs` from the workspace, and `instrumented`, the workspace built with every feature.

**Rationale.** A fact about a step stated in several places drifts: what runs before what was written in the steps' own code, in the gate's prose and in the check workflow, and which flags a step passes decide how much it checks. One table makes agreement a property of the code, as the compiler's own contract table does for its commands ([An argument names one subject, and each command states what it accepts](../documentation/design/tools/an-argument-names-one-subject-and-each-command-states-what-it-accepts.md)). The subject comes first because a person works on one thing: `cargo xtask vscode` is everything about the extension, and the same word means the same thing under every action. Ownership by the nearest manifest root is computed rather than listed, so a file added anywhere is already some tree's and no list has to be kept complete.

**Rejected.**

- **A name per step that fuses a tool and a tree** (`zed-clippy`, `grammar-install`), which encodes one of three things about a step and leaves the rest to prose.
- **The action first with the subject its argument**, which makes a pair no subject has writable, to be refused in a sentence rather than by the parser.
- **A subject per language** (`js`, `rust`), which gives one word a second meaning beside what is built, and cannot lint Rust at all, since what a Rust lint sees depends on the features and the target of what is built.
- **Lanes**, a second grouping of steps beside subjects, and a machine in a row: a step runs wherever the checkout is, and which machine runs which subject is the workflow's to say.
- **A passthrough beside the fixed steps**, two spellings for one action.

### A step runs in the configuration that ships

**Decision.** `workspace`'s lint and tests build with default features, and every product builds as it is released. What only a feature compiles is `instrumented`'s: its lint runs with every feature on, and `docs` documents every feature. The compiler's suite is compiled once, by `workspace test`: no step runs it in a second configuration. A filter picks tests out of that one build by their path and `--binary` by their test binary's name, all of them one union, and neither changes what is built; a package given to `workspace lint` or `workspace test` is a build of its own, which a job on one machine asks for, as the macOS job builds the native host alone, and a question about a few tests does not. The recorder's tests are the one second run, `instrumented test`: `curios-profile` under its `enabled` feature, a crate that depends on nothing of the compiler, so the run compiles it alone. The workspace's lint table denies warnings, so no step passes a flag to deny them and a build no step lints fails on one all the same.

**Rationale.** A check that runs in a configuration nothing ships finds what that configuration has and misses what the shipped one has: an import only a backend reads is used with every feature on and unused in the launcher. One configuration for the day-to-day lint and tests is also one build of the prelude's images for both. Denying warnings where the lints are stated holds every build anyone runs, in whatever configuration, without a list of configurations to keep.

**Rejected.**

- **Every feature for the day-to-day lint**, which sees all the code at once and never the compiler a user runs, and pays a second build of the prelude's images beside the tests'.
- **A lint pass per product**, a list of configurations to maintain where the lint table needs none.
- **A package as the way to pick tests**, which compiles the suite again under that package's features for the few tests asked about, a filter that matches test names alone, which cannot reach a suite whose tests carry no word of their binary's name, and one filter matching paths and binary names at once, which nextest refuses wherever no binary has the name.
- **A crate of its own for the recorder**, so that its tests need no feature: a crate, a second manifest naming `tracing` and every taker repointed, for tests one narrow step reaches.
- **A test run with every feature on**, for the tests the profiling feature adds: a second compilation of the whole suite for a handful of tests, which are taken by hand as the measurements beside them are.
- **Denying warnings through an environment variable or a flag**, a second place to set it and so one to forget; Cargo's own `build.warnings`, which the pinned toolchain does not yet read.

### What is not Rust is formatted and linted by Biome

**Decision.** The JavaScript, JSON and CSS of every tree are formatted and linted by Biome, run through `npx` at the one exact version this crate states, under the one `biome.json` at the repository's root, which lists the directories Biome reads, each with the languages it holds. A tree's `format` and `lint` hand Biome the paths the tree holds, so the workspace's cover the browser harness and the documentation pages' script and stylesheet, and each npm tree's cover its own. A warning fails as an error does, as the workspace's Rust is held by its lint table. The workspace's Rust that belongs to no package, the corpus's Rust twins under `programs/`, is formatted by `workspace format` beside the packages, under the workspace's edition.

**Rationale.** Nothing held these files: the browser harness the compiler's output runs under and the script every documentation page loads were neither formatted nor linted. One tool reads the three languages under one configuration, and `npx` fetches it on first use into npm's own cache, so the repository's root gains no `package.json`, no lock file and no `node_modules` beside the Rust workspace. The version is exact because a formatter's output moves between releases, and it is stated here because this crate is what runs it. A listed directory is one whose files are written in the language their names give, so a template that renders JavaScript, which is no JavaScript until it is rendered, stays out by not being listed rather than by an exception.

**Rejected.** Every tracked file of the three languages wherever it stands, with the files Biome cannot read excluded one by one, which a new template has to know to add; a `package.json` at the root to pin it, which makes the repository an npm package for one tool's sake; a development dependency in each npm tree, two pins of one tool and none for the workspace's own JavaScript; a linter and a formatter as two tools with a plugin tree between them, for what one binary does.

### The gate is every step, one after another

**Decision.** `cargo xtask gate` runs every step of the table, `cargo xtask <subject>` every step about one subject, and `cargo xtask <subject> <action>` one action's, each with the steps it ensures, in the table's order, one after another, formatting as a check. A step runs to its end whatever the one before it did, and says how it ended and in how long; a step whose prerequisite did not pass is not run, since what it reads is not there, while the steps that read nothing of the failure go on, and the end names what failed and what was not run. A step that reads another subject's build ensures it, as `zed test` ensures `grammar build`, and a prerequisite two steps share runs once. The hand-off gate, `.claude/commands/run-full-gate.md`, is `cargo xtask gate`, and a step that failed in it is run again by its subject and action.

**Rationale.** The table already states what each step reads, and its order is one every prerequisite stands ahead of, so one process walking it is the whole schedule: no order between two subjects is left to prose, and no two steps write one place at once, as two subjects each generating the grammar once did. A step skipped behind a failed prerequisite reports its cause rather than an error of its own that only looks unrelated.

**Rejected.** The gate as each subject's command, run together as the machine allows, which left what orders two subjects, and which of them write one place, to prose beside the table; a runner starting steps side by side, each a process with a log of its own, which needs the table to say which steps conflict, kept until a gate's wait calls for it; running only what a change touched, which needs every step's inputs declared and is left until they are.

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

**Decision.** `release` cuts a release end to end. It reads the workspace version from `Cargo.toml`, resolves the one its argument names, refuses unless the tree is clean, the branch is `main`, `origin/main` holds no commit `main` does not, the tag is free, the target is above the current version and `curios-std/src/curios.toml`'s `compiler` minimum matches the current version, then writes both manifests, updates the lock, checks the diff it produced, commits, tags, and pushes `main` and the tag. The bundled standard library's minimum follows each release's version; ordinary packages choose their own minimum under [the package contract](../curios-package/README.md#a-compiler-minimum-belongs-to-the-package-or-its-enumerating-umbrella). Invoking the command is the intent to publish.

**Rationale.** The tag push fires `release.yml`, so it is the step whose spelling this crate exists to fix. The version is computed from the manifest rather than recalled, and "the diff is the workspace version, the lock's member versions and the standard library's compiler minimum" is an assertion rather than a reading. The bundled library ships with the compiler that validates it, so recording that pairing avoids a separate claim about older compilers that the release does not test. The monotonicity refusal closes what the tag check cannot see: the history skips ranges — `0.1.0` is followed by `0.2.1` — so a mistyped version landing in a gap has no tag to collide with.

**Rejected.** Stopping before the pushes, which leaves the irreversible step in prose; consulting the check workflow, which has already answered for the commits on `main`; undoing a failed dance, which is destructive and the user's to ask for — the command prints what exists and what would undo it; testing the dance itself, which would cut a release, where every judgment it makes before and after writing is tested; updating the bundled library's minimum by hand, which can drift from the compiler released with it; retaining an older minimum, which promises compatibility outside that validated pairing.

### A command is run or asked, never both

**Decision.** `run` and `run_in` spawn a tool to do something: they echo the command line and inherit the terminal's streams. `ask` spawns one to ask something: it echoes nothing, captures stdout and hands it back, and leaves stderr inherited. Both go through one private spawn holding the echo, the status check and the error text.

**Rationale.** A step's output is the user's — it streams live and in colour, and the echoed command line makes the terminal a transcript of the work — while a question's output is the program's, and streaming it prints work that is not being done. An inherited stderr lets a failing question explain itself in the tool's own words.

**Rejected.** One verb that always captures, which costs a test and a format check their colour for a buffer nobody reads; a `git` verb beside `cargo`, `grammar`, `vscode` and `zed`, which name a tree where git runs at the root — `release` wraps its own calls privately, as `installer` owns `validated`.
