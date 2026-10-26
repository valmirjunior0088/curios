# xtask

The repository's steps as a cargo subcommand, `cargo xtask <subject> [action]`: one authored table says what each step is about, what it does and the one thing only it catches, each step's plan says what it reads, and the command line, a bare subject's sequence, what each step is run after and what the check workflow must run are read off the two. The subjects are the four trees — `workspace`, `grammar`, `vscode` and `zed` — and what is built from the workspace's: `compiler`, `launcher`, `bundle`, `docs`, `instrumented` and `tools`, the workspace's own tools. `gate` runs every step of the table, one after another; `release`, `installer`, `profile`, `profile-prelude` and `clean` are commands of their own, outside the table. A test step needs `cargo-nextest`, the bundle's tests Node, the npm trees `npm` and every JavaScript check Biome, and the bundle and the Zed extension the two Wasm targets `rust-toolchain.toml` installs; every other step needs cargo alone, and `release` needs git beside it and reaches for `gh` only to report. The alias lives in `.cargo/config.toml`; why the gate holds the steps it does is [Every gate step catches what no other step does](../documentation/design/every-gate-step-catches-what-no-other-step-does.md), and the steps and their reasons are the crate's own documentation.

## Decisions

### Steps are a cargo subcommand, not a Makefile

**Decision.** Every step is reached through this crate, by the `xtask` alias, and the workspace has no Makefile.

**Rationale.** A fresh clone reaches every step through one tool, cargo, on Linux and macOS alike, and what a step does beyond cargo — copying the launcher, generating the browser bindings — is stated in Rust with one error handling, without a shell between the step and cargo.

**Rejected.** A build script for the browser bundle, which runs before `curios-js` compiles and contends for the target-directory lock with a nested cargo; a Makefile beside this crate, a second entry point.

### The recipes are a member the compiler's checks leave out

**Decision.** This crate is a member of the compiler's workspace, beside the bench and the board, reached by `run --package xtask`. It inherits the workspace's lint table, and `wasm-bindgen-cli-support` is a row of the workspace's table beside the `wasm-bindgen` the browser's crates build against. The compiler's lint, tests and instrumented lint select the workspace with its three tools left out — `xtask`, `xbench` and `xboard` — and the `tools` subject lints and tests those three on their own. `workspace format` formats every member, the tools among them, and `docs` documents them all.

**Rationale.** One lock holds the bindings pair to one version, which no test has to: the generator and the bindings each pin `wasm-bindgen-shared` exactly, and its `links` key lets a lock hold only one version of it, so cargo refuses a pair that disagrees. A member inherits the lint table, which a root of its own restated. Cargo unifies a build's features across the packages it selects, so a tool in the compiler's selection gives the compiler's dependencies features no shipped compiler has: selected beside the compiler, the tools give `anyhow` its `default` and `hashbrown` its `default-hasher` and `serde`, and with them left out the compiler's selection adds no feature over `-p curios` to any package `-p curios` builds (counted on top of `130821c4d`, by `cargo tree -e normal,build` over each selection). The bench and the board stand with the recipes because neither ships either.

**Rejected.** A root of its own, with its own lock file and target directory: the lint table restated and the generator pinned apart, each held to its original by a test, and a second lock for dependencies the workspace shares, to spare a `cargo xtask` the wait behind a compiler build in progress, which every step that builds in the workspace's target directory waits behind anyway. A member with a target directory of its own, for the same wait.

### A step is an action on a subject, stated once

**Decision.** `steps.rs` is one table, a row a step: its subject, its action, and the one thing only it catches. The order of the rows is the order steps run in, and what a step is run after is read off its plan. A step asks for a tool beyond cargo by launching it, so what it needs is what it runs as it was asked. Everything else is read off the rows. The command line is `cargo xtask <subject> [action]`, each subject a subcommand whose actions are an enum of its own, so an action a subject lacks is the parser's error. A bare subject is every step about it. An action's help is what its steps catch. No step forwards arguments to the tool it runs: an action may take a parameter it places itself — a package to build alone, names to pick tests by — but never a tail it hands on unread.

A subject is a tree or something built from one. A tree is a manifest root — the Rust workspace, the two npm packages under `editors/`, the Zed extension's own workspace — and a tracked file belongs to the tree of its nearest one. Format and lint are a tree's, over every file it holds, whatever its language; build, test and install belong to what is built: `compiler`, `launcher`, `bundle` and `docs` from the workspace, `instrumented`, the workspace built with every feature, and `tools`, the recipes, the bench and the board built apart from the compiler.

**Rationale.** A fact about a step stated in several places drifts: what runs before what was written in the steps' own code, in the gate's prose and in the check workflow, and which flags a step passes decide how much it checks. One table makes agreement a property of the code, as the compiler's own contract table does for its commands ([An argument names one subject, and each command states what it accepts](../documentation/design/tools/an-argument-names-one-subject-and-each-command-states-what-it-accepts.md)). The subject comes first because a person works on one thing: `cargo xtask vscode` is everything about the extension, and the same word means the same thing under every action. Ownership by the nearest manifest root is computed rather than listed, so a file added anywhere is already some tree's and no list has to be kept complete.

**Rejected.**

- **A name per step that fuses a tool and a tree** (`zed-clippy`, `grammar-install`), which encodes one of three things about a step and leaves the rest to prose.
- **The action first with the subject its argument**, which makes a pair no subject has writable, to be refused in a sentence rather than by the parser.
- **A subject per language** (`js`, `rust`), which gives one word a second meaning beside what is built, and cannot lint Rust at all, since what a Rust lint sees depends on the features and the target of what is built.
- **Lanes**, a second grouping of steps beside subjects, and a machine in a row: a step runs wherever the checkout is, and which machine runs which subject is the workflow's to say.
- **A passthrough beside the fixed steps**, two spellings for one action.
- **The tools a step needs as a column of its row**, a second statement of what its recipe launches, which a narrowed request contradicts: a lint narrowed to one package demanded Biome and ran none.
- **The steps a step is run after as a column of its row**, a second statement of what its plan reads, which disagreed with the plans both ways: it ran the launcher's build before a lint of the native host, which builds no `curios`, and named no install for the npm scripts that read one.

### A step is a plan the runner performs

**Decision.** A step's recipe, `Step::plan`, returns the tasks it needs done — a tool run in a tree, a binary filed where a build script reads it, the browser's bindings generated, a file written whole, a file a task must leave as it found it — computed from the request and the checkout as it stands, and performs none of them. The runner alone performs a plan, task by task, echoing each command line as it runs. The kinds of task are a closed set, each carrying what it reads and writes in its fields, and planning reads the checkout through one handle that asks git once a run and writes nothing.

**Rationale.** A Curios program's `Io` is a description that only its entry point performs ([Effects are descriptions, and the carrier has no eliminator](../documentation/design/theory/effects-are-descriptions-and-the-carrier-has-no-eliminator.md)), and a step is held the same way: what it runs is data the runner reads before running it, so what a step reads and writes is a fact of its plan rather than of code that has to run to say. A closed set of kinds keeps that fact whole, since a task whose effect were a function would hide what it reads from everything that reads plans, and the set stays small for the reason the host's table does ([A host operation is a foreign declaration whose types are its whole contract](../documentation/design/runtime/a-host-operation-is-a-foreign-declaration-whose-types-are-its-whole-contract.md)): each kind is a row whose fields are its contract.

**Rejected.** Recipes that run their own commands, where what a step does is known only by running it; a kind of task holding a function, for the two steps whose work is no command line — the Binaryen cache check is a task that keeps a file, and the documentation's landing page a file written.

### What a step is run after is read off its plan

**Decision.** A product is what one step leaves and another reads: the launcher, the compiler as it is released, the browser bundle, the grammar's generated parser, an npm tree's installed tools. A step is run after the step that leaves each product its plan reads, and what a plan reads is read off its tasks: a cargo or nextest run whose selection builds `curios`, as `cargo metadata` answers, reads the launcher `curios/build.rs` embeds; an npm script reads its tree's install; a run handed a product as an argument or in its environment reads it; and a run that finds a product by a path it is not handed, as `zed test` finds the grammar where `extension.toml` says Zed installs it, declares it. Each product is left by one step, the table stands every such step before each step that reads it, and the run's whole schedule exists before its first task. Nothing is reused across runs by this crate: a step another reads from runs once a run, and whether its work is already done is its tool's to say — cargo's fingerprints, the filing's byte comparison, npm's clean install from the lock file.

**Rationale.** Two graphs joined by hand build too much and declare too little, which is Miller's account of recursive make ([Recursive Make Considered Harmful](https://accu.org/journals/overload/14/71/miller_2004/)), and the hand-written column did both: 104 s of every macOS run went to a launcher nothing in that run read, and `cargo xtask vscode test` on a fresh clone ran npm with nothing installed. The cure is one graph, and the boundary with cargo's is crossed by asking cargo, which owns it, rather than by copying it beside the manifests ([One crate is the authority for one external concern](../documentation/design/one-crate-is-the-authority-for-one-external-concern.md)). Every edge is known before anything runs, since no plan reads what another step writes, so the schedule is computed rather than discovered — a build plan as Nix makes one with import-from-derivation off and Bazel's analysis phase does, where Shake's `need` and redo's `redo-ifchange` learn an edge only by running ([Build systems à la carte](https://www.microsoft.com/en-us/research/uploads/prod/2018/03/build-systems-a-la-carte.pdf)). A product's freshness belongs to the tool that read its inputs, because a second record of them here would be Make's modification times or Ninja's log over what cargo already fingerprints.

**Rejected.**

- **Dependencies discovered as a step runs**, a `need` called from a recipe: a graph known only by running can be neither printed nor held by the workflow's test, and nothing here needs one.
- **Tracing the files each command touches**, as tup and rattle do: a tracer per platform, for products coarse enough to name.
- **A build system beside cargo**, Bazel's, Buck's or Nix's: a second statement of cargo's graph, in a tool every clone installs.
- **Reuse across runs kept by this crate**, by modification times or a log of commands: a second record of what cargo and npm know, for installs that take a second or two.
- **`cargo tree`'s text** for which packages a selection builds: `cargo metadata` is the output cargo versions.

### A step runs in the configuration that ships

**Decision.** `workspace`'s lint and tests build with default features and leave the workspace's own tools out, which `tools` lints and tests apart, and every product builds as it is released. What only a feature compiles is `instrumented`'s: its lint runs with every feature on, and `docs` documents every feature. The compiler's suite is compiled once, by `workspace test`: no step runs it in a second configuration. A filter picks tests out of that one build by their path and `--binary` by their test binary's name, all of them one union, and neither changes what is built; a package given to `workspace lint` or `workspace test` is a build of its own, which a job on one machine asks for, as the macOS job builds the native host alone, and a question about a few tests does not. The recorder's tests are the one second run, `instrumented test`: `curios-profile` under its `enabled` feature, a crate that depends on nothing of the compiler, so the run compiles it alone. The workspace's lint table denies warnings, so no step passes a flag to deny them and a build no step lints fails on one all the same.

**Rationale.** A check that runs in a configuration nothing ships finds what that configuration has and misses what the shipped one has: an import only a backend reads is used with every feature on and unused in the launcher. One configuration for the day-to-day lint and tests is also one build of the prelude's images for both. Denying warnings where the lints are stated holds every build anyone runs, in whatever configuration, without a list of configurations to keep.

**Rejected.**

- **Every feature for the day-to-day lint**, which sees all the code at once and never the compiler a user runs, and pays a second build of the prelude's images beside the tests'.
- **A lint pass per product**, a list of configurations to maintain where the lint table needs none.
- **A package as the way to pick tests**, which compiles the suite again under that package's features for the few tests asked about, a filter that matches test names alone, which cannot reach a suite whose tests carry no word of their binary's name, and one filter matching paths and binary names at once, which nextest refuses wherever no binary has the name.
- **A crate of its own for the recorder**, so that its tests need no feature: a crate, a second manifest naming `tracing` and every taker repointed, for tests one narrow step reaches.
- **A test run with every feature on**, for the tests the profiling feature adds: a second compilation of the whole suite for a handful of tests, which are taken by hand as the measurements beside them are.
- **Denying warnings through an environment variable or a flag**, a second place to set it and so one to forget; Cargo's own `build.warnings`, which the pinned toolchain does not yet read.

### What is not Rust is formatted and linted by Biome

**Decision.** The JavaScript, JSON and CSS of every tree are formatted and linted by Biome, the `biome` a machine has on its path, which the check workflow installs at the one exact version it states, under the one `biome.json` at the repository's root, which lists the directories Biome reads, each with the languages it holds. A tree's `format` and `lint` hand Biome the paths the tree holds, so the workspace's cover the browser harness and the documentation pages' script and stylesheet, and each npm tree's cover its own. A warning fails as an error does, as the workspace's Rust is held by its lint table. The workspace's Rust that belongs to no package, the corpus's Rust twins under `programs/`, is formatted by `workspace format` beside the packages, under the workspace's edition.

**Rationale.** Nothing held these files: the browser harness the compiler's output runs under and the script every documentation page loads were neither formatted nor linted. One tool reads the three languages under one configuration, and a binary on the path leaves the repository's root with no `package.json`, no lock file and no `node_modules` beside the Rust workspace. A step launches the `biome` on the path, as it launches nextest, refused with what installs it where there is none, and the version is the check workflow's, which installs it on its runners; it is exact because a formatter's output moves between releases. A listed directory is one whose files are written in the language their names give, so a template that renders JavaScript, which is no JavaScript until it is rendered, stays out by not being listed rather than by an exception.

**Rejected.** Every tracked file of the three languages wherever it stands, with the files Biome cannot read excluded one by one, which a new template has to know to add; a `package.json` at the root to pin it, which makes the repository an npm package for one tool's sake; a development dependency in each npm tree, two pins of one tool and none for the workspace's own JavaScript; `npx` at a version this crate states, a fetch on each machine's first use and a pin in the recipes where the workflow states nextest's and Node's; a linter and a formatter as two tools with a plugin tree between them, for what one binary does.

### The gate is every step, one after another

**Decision.** `cargo xtask gate` runs every step of the table, `cargo xtask <subject>` every step about one subject, and `cargo xtask <subject> <action>` one action's, each after the steps that leave what it reads, in the table's order, one after another, formatting as a check. A step runs to its end whatever the one before it did, and says how it ended and in how long; a step whose prerequisite did not pass is not run, since what it reads is not there, while the steps that read nothing of the failure go on, and the end names what failed and what was not run. A step that reads another subject's product runs after the step that leaves it, as `zed test` runs after `grammar build`, and a step two others read from runs once. The hand-off gate, `.claude/commands/run-full-gate.md`, is `cargo xtask gate`, and a step that failed in it is run again by its subject and action.

**Rationale.** Each plan states what its step reads, and the table's order is one every producer stands ahead of, so one process walking it is the whole schedule: no order between two subjects is left to prose, and no two steps write one place at once, as two subjects each generating the grammar once did. A step skipped behind a failed prerequisite reports its cause rather than an error of its own that only looks unrelated.

**Rejected.** The gate as each subject's command, run together as the machine allows, which left what orders two subjects, and which of them write one place, to prose beside the table; a runner starting steps side by side, each a process with a log of its own, which needs the table to say which steps conflict, kept until a gate's wait calls for it; running only what a change touched, which needs every input of every step declared to the file, where a plan names the products it reads and leaves their files to the tools that read them.

### The check workflow is held to the table

**Decision.** `.github/workflows/check.yml` is written by hand and thin: a job sets up a machine and runs `cargo xtask` lines, and `every_step_has_a_job` reads the file and holds that every line is a command this crate parses and that the lines together run every step of the table. Each tool's version is stated once, by whoever installs it: Rust's in `rust-toolchain.toml`, which rustup reads; nextest's, Biome's and Node's in the workflow, for its runners. This crate launches each from the path and refuses with what installs it where one is absent.

**Rationale.** A workflow that restates the steps drifts from them silently, and one generated from the table would put runner images, caches and action versions — GitHub's vocabulary — in this crate. Held by a test, the file stays where GitHub reads it and cannot leave a step out.

**Rejected.** Generating the workflow; a workflow that names a toolchain of its own, which named `stable` while the repository pinned a version.

### The bindings generator is a library dependency

**Decision.** `bundle build` calls `wasm-bindgen-cli-support`, the crate the `wasm-bindgen` command line wraps, pinned in the workspace manifest beside `wasm-bindgen` itself.

**Rationale.** The generator must match the `wasm-bindgen` crate's version exactly; as a dependency the match is the lockfile's, and the generator refuses a module built against another version, naming both.

### The suite runs under nextest, a process to a test, as one run

**Decision.** A test step runs under `cargo-nextest` rather than `cargo test`, as one run: it takes a package and a name to narrow the run by and no shard. Nextest is a tool a contributor installs: the step launches it before building anything, as its own program, `cargo-nextest`, and a launch that finds none is refused with the command that installs it.

**Rationale.** Nextest runs each test as a process of its own, and the compiler's last stage wants one. Binaryen's settings are its process's and its optimizer is not safe to call from two threads, so `curios-binaryen` serializes it behind a lock: tests that share a process, as `cargo test`'s do, queue for it however many cores there are, and a test that aborts — Binaryen answering a module it cannot parse with a C++ `assert` — takes every other result of its binary with it. A process to a test runs the suite on every core and fails an abort by name. The suite is compiled optimized ([A test is compiled optimized, with its checks on](../documentation/design/a-test-is-compiled-optimized-with-its-checks-on.md)), so one job runs it and nothing cuts it into slices. Nextest's command line is the interface it versions, so the flags this crate writes are ones a later release still takes.

**Rejected.** `cargo test`, the runner a clone already has: its tests are threads of one process, where the suite waits its turn at Binaryen's lock. Slices of the suite run as parallel jobs, which its time no longer calls for, and with them the oldest nextest that cuts one. Nextest as a library, as the bindings generator is: `nextest-runner` is internal to nextest and breaks with every release, and nothing in the workspace has to match its version, which is the reason `wasm-bindgen-cli-support` is a dependency. A manifest row naming it, since cargo has no stable way to depend on a tool and a `metadata` table says only what something reads out of it. Installing it from the step, which makes a first test run build a tool nobody asked for. Nextest in the workflow alone beside `cargo test` everywhere else, two spellings of one step that also differ in whether tests share a process.

### The release dance is a command

**Decision.** `release` cuts a release end to end. It reads the workspace version from `Cargo.toml`, resolves the one its argument names, refuses unless the tree is clean, the branch is `main`, `origin/main` holds no commit `main` does not, the tag is free, the target is above the current version and `curios-prelude/std/curios.toml`'s `compiler` minimum matches the current version, then writes both manifests, updates the lock, checks the diff it produced, commits, tags, and pushes `main` and the tag. The bundled standard library's minimum follows each release's version; ordinary packages choose their own minimum under [the package contract](../curios-package/README.md#a-compiler-minimum-belongs-to-the-package-or-its-enumerating-umbrella). Invoking the command is the intent to publish.

**Rationale.** The tag push fires `release.yml`, so it is the step whose spelling this crate exists to fix. The version is computed from the manifest rather than recalled, and "the diff is the workspace version, the lock's member versions and the standard library's compiler minimum" is an assertion rather than a reading. The bundled library ships with the compiler that validates it, so recording that pairing avoids a separate claim about older compilers that the release does not test. The monotonicity refusal closes what the tag check cannot see: the history skips ranges — `0.1.0` is followed by `0.2.1` — so a mistyped version landing in a gap has no tag to collide with.

**Rejected.** Stopping before the pushes, which leaves the irreversible step in prose; consulting the check workflow, which has already answered for the commits on `main`; undoing a failed dance, which is destructive and the user's to ask for — the command prints what exists and what would undo it; testing the dance itself, which would cut a release, where every judgment it makes before and after writing is tested; updating the bundled library's minimum by hand, which can drift from the compiler released with it; retaining an older minimum, which promises compatibility outside that validated pairing.

### A command is run or asked, never both

**Decision.** `launch_in`, `run` and `run_in` spawn a program to do something: they echo the command line and inherit the terminal's streams, and `launch_in`, which the runner performs a plan's runs with, refuses a tool whose program is not there with what installs it. `ask` spawns one to ask something: it echoes nothing, captures stdout and hands it back, and leaves stderr inherited. Both go through one private spawn holding the echo, the status check and the error text.

**Rationale.** A step's output is the user's — it streams live and in colour, and the echoed command line makes the terminal a transcript of the work — while a question's output is the program's, and streaming it prints work that is not being done. An inherited stderr lets a failing question explain itself in the tool's own words.

**Rejected.** One verb that always captures, which costs a test and a format check their colour for a buffer nobody reads; a `git` verb beside `cargo`, for the commands outside the table, where only `release` asks git anything — it wraps its own calls privately, as `installer` owns `validated`.

### The launcher is built by a cargo of its own

**Decision.** `launcher build` builds `curios-native` in a `cargo` invocation of its own, separate from every build of the workspace, and files what it built where `curios/build.rs` embeds it.

**Rationale.** Cargo unifies a dependency's features across everything one invocation builds, so a launcher built beside the compiler would link what the compiler turns on; spawned apart, workspace feature unification cannot reach it, and the launcher is built in the one configuration it ships in.
