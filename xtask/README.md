# xtask

The runner of the repository's task files, as a cargo subcommand, `cargo xtask <task>`. The files say everything the repository runs: `xtask/tasks.toml` and a file under `xtask/tasks/` for each subject — the four trees, `workspace`, `grammar`, `vscode` and `zed`, and what is built from the workspace's: `compiler`, `launcher`, `bundle`, `docs`, `instrumented`, `native` and `tools`, the jig's own crates — each holding its steps, what each requires and consumes and the files it produces. `format` writes every tree's formatting, `profile` runs a program under a recorder, and `gate` and `clean` are the top's own steps; `release` and `installer` are the two commands outside the files. A test step needs nextest, the bundle's build the `wasm-bindgen` program and its tests Node, the npm trees `npm` and every JavaScript check Biome, and the bundle and the Zed extension the two Wasm targets `rust-toolchain.toml` installs; every other step needs cargo alone, and `release` needs git beside it and reaches for `gh` only to report. What version of each program a run needs is the `programs` of `xtask/tasks.toml`. The alias lives in `.cargo/config.toml`; why the gate holds the steps it does is [Every gate step catches what no other step does](../documentation/design/every-gate-step-catches-what-no-other-step-does.md), and each step's reason is its `description`.

## Decisions

### Steps are a cargo subcommand, not a Makefile

**Decision.** Every step is reached through this crate, by the `xtask` alias, and the workspace has no Makefile.

**Rationale.** A fresh clone reaches every step through one tool, cargo, on Linux and macOS alike, the two platforms Curios runs on, and what the task files state is read and run by one program with one error handling, without a shell between a step and the programs it launches.

**Rejected.** A build script for the browser bundle, which runs before `curios-js` compiles and contends for the target-directory lock with a nested cargo; a Makefile beside this crate, a second entry point; `just` or `cargo-make` in this crate's place, each a second entry point every machine installs, its prerequisites written by hand.

### The jig's crates are members the compiler's checks leave out

**Decision.** A crate of the workspace whose name starts with `x` — this crate, the bench and the board — is part of the jig the repository is for the compiler, and ships nothing. Each is a member of the compiler's workspace, reached by an alias of `.cargo/config.toml`, and inherits the workspace's lint table. The compiler's lint, tests and instrumented lint select the workspace with the jig's crates left out, `--workspace --exclude x*`, and the `tools` steps lint and test them on their own, `--package x*`; `format` formats every member, and `docs` documents them all.

**Rationale.** A member inherits the lint table, which a root of its own restated. Cargo unifies a build's features across the packages it selects, so a crate of the jig in the compiler's selection gives the compiler's dependencies features no shipped compiler has: selected beside the compiler, the tools give `anyhow` its `default` and `hashbrown` its `default-hasher` and `serde`, and with them left out the compiler's selection adds no feature over `-p curios` to any package `-p curios` builds (counted on top of `130821c4d`, by `cargo tree -e normal,build` over each selection). The prefix is the rule cargo's own patterns read, so a crate added to the jig is left out of the compiler's checks and taken by the tools' with no list to edit.

**Rejected.** A root of its own, with its own lock file and target directory: the lint table restated and held to its original by a test, and a second lock for dependencies the workspace shares, to spare a `cargo xtask` the wait behind a compiler build in progress, which every step that builds in the workspace's target directory waits behind anyway. A member with a target directory of its own, for the same wait. The jig's crates listed by name, once for the selection that leaves them out and again for the one that takes them.

### The steps are task files, and this crate holds only rules

**Decision.** Every fact about what the repository runs is data in the task files, and every rule that would hold for any repository is this crate's. `xtask/tasks.toml` is the group the files in `xtask/tasks/` belong to, each a group named after its file, and a key's meaning is where it sits: a file's top holds `description`, `variables`, `programs` and its steps and groups; a group holds only steps and groups; a step holds `description`, `requires`, `consumes`, `produces`, `run` and `dir`. A task's path is its command line, `cargo xtask workspace lint clippy`, and naming a group runs every step it holds. A command is the words a person would type, leading `NAME=value` words setting its environment, and `{name}` in a word is a variable's value or a produced file's path: a variable is a constant of the run — words as written, a command's output, or a flag or argument given on the command line — and `host` and `target` are the two the runner fills. A file's names share one namespace; without a dot a name is the file's own, and with dots a path from the top. The files are read when `xtask` runs, so editing them needs no build.

**Rationale.** The steps were five modules of Rust — the table, each step's plan, the command line, the products and the rules that derived what each step read, the checkout's graph — and a reader could see the system only by reading them all. As data, what a step runs, what it waits for and why sit together where a person reads them, and what is left in Rust is the reading, the ordering and the running, which know nothing of cargo, npm or Biome. A Curios program's `Io` is a description only its entry point performs ([Effects are descriptions, and the carrier has no eliminator](../documentation/design/theory/effects-are-descriptions-and-the-carrier-has-no-eliminator.md)), and the files are held the same way: what a step does is read before it runs. A key's meaning by its position is what tells a command from a container without a name to learn, and a name resolved as the files are read refuses a typo before anything runs, with its file, line and column.

**Rejected.**

- **The steps as Rust**, a table, a plan per step and an enum per subject's actions: the compiler checked them, and no reader could take them in as one system.
- **Command lines as strings with an `xtask` syntax inside**, conditions and macros dressed as commands: a third language TOML cannot see, which a reader of its own would have to parse.
- **A make-like notation read by `curios-parse`**: 51 more packages in this crate's build, and the Unicode database downloaded in the jobs that build this crate alone.
- **A root file of another kind**, holding variables, install lines and the tasks the gate leaves out: a file that is not a group like the others.
- **A mark on a task that keeps it out of the gate**: which tasks a group runs is the group's to state.
- **Built-in operations in `run`**, a launcher filed and bindings generated by this crate: every entry of `run` is a command, the launcher filed with `cp -p` and the bindings generated by `wasm-bindgen`.
- **A name per step that fuses a tool and a tree** (`zed-clippy`), a subject per language, lanes of steps beside subjects, and a passthrough beside the fixed steps, as the table once weighed them.

### What a step runs after is stated beside it

**Decision.** A step runs after the steps and groups it `requires`, and after the producer of each file it `consumes`; a file is produced by the one step whose `produces` names it, and must exist once that step has run. `requires` names only steps and groups and `consumes` only produced files, and a command that writes a produced file's path in a step that neither consumes nor produces it is refused, so no command reads a file without the edge that orders it. A run takes what was asked and everything it runs after, the whole schedule known before its first task, and orders it the way the files write it: an asked group's steps in its file's order, a step's requirements in its `requires`' order, each step after what it runs after. A step runs once a run, and whether its work is already done is its tool's to say — cargo's fingerprints, which the launcher's filing keeps in the times it copies, npm's clean install from the lock file.

**Rationale.** Two graphs joined by hand build too much and declare too little, which is Miller's account of recursive make ([Recursive Make Considered Harmful](https://accu.org/journals/overload/14/71/miller_2004/)), so every task file is a part of one graph, crossed only by produced files and requirements. An edge written beside the command that needs it is read where the command is, and one that names a file is held to it: the producer must leave it, and its readers must say they read it. Every edge is known before anything runs, since no step's edges depend on what another does, so the schedule is computed rather than discovered — a build plan as Nix makes one with import-from-derivation off and Bazel's analysis phase does, where Shake's `need` and redo's `redo-ifchange` learn an edge only by running ([Build systems à la carte](https://www.microsoft.com/en-us/research/uploads/prod/2018/03/build-systems-a-la-carte.pdf)).

**Rejected.**

- **Edges derived by rules the runner holds**: a cargo run reading the launcher where `cargo metadata` said its selection built `curios`, an npm run reading its tree's install. The rules were facts about this repository in code, and the narrowing they served, a lint of one package building no launcher, is the `native` subject's own steps.
- **Dependencies discovered as a step runs**, a `need` called from a recipe: a graph known only by running can be neither printed nor held by the workflow's test.
- **Tracing the files each command touches**, as tup and rattle do: a tracer per platform, for products coarse enough to name.
- **A build system beside cargo**, Bazel's, Buck's or Nix's: a second statement of cargo's graph, in a tool every clone installs.
- **Reuse across runs kept by this crate**, by modification times or a log of commands: a second record of what cargo and npm know.
- **Depending on a step where a file passes between two**: the reader names the thing it reads, so its producer can be renamed or split without touching it.

### A step runs in the configuration that ships

**Decision.** `workspace`'s lint and tests build with default features and leave the jig's crates out, which `tools` lints and tests apart, and every product builds as it is released. What only a feature compiles is `instrumented`'s: its lint runs with every feature on, and `docs` documents every feature. The compiler's suite is compiled once, by `workspace test suite`: no step runs it in a second configuration. Its `-E` takes nextest's own filterset — `test(~name)` for a test's path, `binary(=name)` for a test binary — and picks tests out of that one build without changing it. The native host's lint and tests alone, which the macOS job runs, are `native`'s steps, a build of that one crate. The recorder's tests are the one second run, `instrumented test`: `curios-profile` under its `enabled` feature, a crate that depends on nothing of the compiler, so the run compiles it alone. The workspace's lint table denies warnings, so no step passes a flag to deny them and a build no step lints fails on one all the same.

**Rationale.** A check that runs in a configuration nothing ships finds what that configuration has and misses what the shipped one has: an import only a backend reads is used with every feature on and unused in the launcher. One configuration for the day-to-day lint and tests is also one build of the prelude's images for both. Denying warnings where the lints are stated holds every build anyone runs, in whatever configuration, without a list of configurations to keep. Nextest's filterset is the language it versions, so a selection is written once, in it, rather than in a grammar of this crate's.

**Rejected.**

- **Every feature for the day-to-day lint**, which sees all the code at once and never the compiler a user runs, and pays a second build of the prelude's images beside the tests'.
- **A lint pass per product**, a list of configurations to maintain where the lint table needs none.
- **A package as the way to pick tests**, which compiles the suite again under that package's features for the few tests asked about; a package parameter beside the fixed steps, which the one machine that asked for it has as steps of its own.
- **`--filter` and `--binary` spelled by this crate**, each name made into a filterset by a grammar the task files would have to state.
- **A crate of its own for the recorder**, so that its tests need no feature: a crate, a second manifest naming `tracing` and every taker repointed, for tests one narrow step reaches.
- **A test run with every feature on**, for the tests the profiling feature adds: a second compilation of the whole suite for a handful of tests, which are taken by hand as the measurements beside them are.
- **Denying warnings through an environment variable or a flag**, a second place to set it and so one to forget; Cargo's own `build.warnings`, which the pinned toolchain does not yet read.

### Formatting is written by one task and held by every tree's lint

**Decision.** `cargo xtask format` writes every tree's formatting: the workspace's Rust, the corpus's Rust twins under `programs/` beside it under the workspace's edition, the JavaScript, JSON and CSS `biome.json` lists, and the Zed extension's Rust. Each tree's `lint` holds its own formatting, as a check that writes nothing.

**Rationale.** A subject and the gate are checks, so none of their steps writes; a step that writes is one a person asks for, and formatting is fast enough to write everywhere at once. With the check under `lint`, no step takes a flag that turns a writer into a checker.

**Rejected.** A tree's `format` that writes or checks by a flag, which made a bare subject's run a check by a rule of the runner's.

### What is not Rust is formatted and linted by Biome

**Decision.** The JavaScript, JSON and CSS of every tree are formatted and linted by Biome, the `biome` a machine has on its path, at the one exact version the task files pin, under the one `biome.json` at the repository's root, which lists the directories Biome reads, each with the languages it holds. Each tree's Biome steps hand Biome the paths the tree holds: the workspace's the four `biome.json` lists outside the editors' trees, which a test holds to it, and each npm tree its own directory. A warning fails as an error does, as the workspace's Rust is held by its lint table.

**Rationale.** Nothing held these files: the browser harness the compiler's output runs under and the script every documentation page loads were neither formatted nor linted. One tool reads the three languages under one configuration, and a binary on the path leaves the repository's root with no `package.json`, no lock file and no `node_modules` beside the Rust workspace. A step launches the `biome` on the path, refused with what installs it where there is none or it is another version, and the version is exact because a formatter's output moves between releases. A listed directory is one whose files are written in the language their names give, so a template that renders JavaScript, which is no JavaScript until it is rendered, stays out by not being listed rather than by an exception.

**Rejected.** Every tracked file of the three languages wherever it stands, with the files Biome cannot read excluded one by one, which a new template has to know to add; the workspace's paths computed from what git tracks, a rule of the runner's for what four words state; a `package.json` at the root to pin it, which makes the repository an npm package for one tool's sake; a development dependency in each npm tree, two pins of one tool and none for the workspace's own JavaScript; a linter and a formatter as two tools with a plugin tree between them, for what one binary does.

### The gate is a step that requires every subject

**Decision.** `cargo xtask gate` is the top's step that requires every subject the hand-off checks, in the order it names them, and nothing else: `format`, `native`, `profile` and `clean` are tasks no requirement reaches. Each step runs to its end whatever the one before it did, and says how it ended and in how long; a step behind one that did not pass is not run, since what it reads is not there, while the steps that read nothing of the failure go on, and the end names what failed and what was not run. The hand-off gate, `.claude/commands/run-full-gate.md`, is `cargo xtask gate`, and a step that failed in it is run again by its path.

**Rationale.** Which tasks the gate runs is the gate's to state, so no task carries a mark about a group it is not in, and a step added to a subject joins the gate with no edit to it. Requirements and produced files order every step, so one process walking the schedule is the whole of it, and no two steps write one place at once. A step skipped behind a failed prerequisite reports its cause rather than an error of its own that only looks unrelated.

**Rejected.** The gate as every task of the tree, which would write, clean and profile; a mark on the tasks the gate leaves out; a runner starting steps side by side, each a process with a log of its own, kept until a gate's wait calls for it; running only what a change touched, which needs every input of every step declared to the file.

### A program a step launches is pinned in the task files

**Decision.** Each program a step launches that is no part of the Rust toolchain, whose version `rust-toolchain.toml` pins for rustup, is pinned once, in the `programs` of `xtask/tasks.toml`, by the name a step launches it as: at an exact `version` where its output moves between releases — Biome's formatting, and the bindings generator, which must match the crate `Cargo.lock` gives the bundle — and at a `minimum` otherwise, nextest's and Node's, beside the command that installs it, `{version}` in it the pin. The first time a step launches a pinned program in a run, the run holds the first version its `--version` prints to the pin, and a program that falls short fails that step with what installs one that meets it; a program no step launches is never asked. The workflows read the pins into each job's environment through `.github/actions/pins` and install each program at its pin, so a runner runs the version a contributor's machine is held to, a minimum's lowest.

**Rationale.** What version a step needs of a program is a fact about the step as much as its command line is, so it sits in the files a run reads rather than in a workflow no local run reads, where it held the runners and left a contributor's machine at whatever it had, a formatter there formatting otherwise than the one the gate ran. Asking on first launch holds what a run was asked and nothing else, at one process a program. A runner installing a minimum runs the oldest version the pin claims, so the claim is the one the check workflow tests.

**Rejected.** The pins in the workflows, which no local run reads; asking every pinned program before a run starts, which refuses a step for a program it does not launch; ranges or a requirement syntax, for what an exact version and a minimum say; installing a missing or stale program from the step, which makes a run build a tool nobody asked for.

### The check workflow is held to the task files

**Decision.** `.github/workflows/check.yml` is written by hand and thin: a job sets up a machine and runs `cargo xtask` lines, and `every_step_the_gate_runs_has_a_job` reads the file and holds that every line is a task the command line parses and that the lines together run every step the gate runs. A job installs each program at the pin the task files state, which `.github/actions/pins` reads into its environment, and `every_pin_a_job_installs_at_is_read_from_the_task_files_first` holds that every pin a job installs at is one the files hold, read before the step that installs at it.

**Rationale.** A workflow that restates the steps drifts from them silently, and one generated from the files would put runner images, caches and action versions — GitHub's vocabulary — in this crate. Held by a test, the file stays where GitHub reads it and cannot leave a step out.

**Rejected.** Generating the workflow; a workflow that names a toolchain of its own, which named `stable` while the repository pinned a version.

### The bindings generator is the program `wasm-bindgen` ships

**Decision.** `bundle build` runs the `wasm-bindgen` program, `wasm-bindgen --target web --out-dir`, at the version `Cargo.lock` gives the `wasm-bindgen` crate. The task files pin it at that version exactly, which `the_bindings_generator_is_pinned_at_the_lock_s_version` holds equal to the lock's, and a machine without it, or with another, is refused with what installs it.

**Rationale.** The program is the interface the generator is published as; the library it wraps, `wasm-bindgen-cli-support`, calls itself an internal dependency, with no promise about its API between releases, and it was most of what this crate built. The generator must match the crate exactly, and it refuses a module built against another version, naming both, so a stale install fails loudly whichever way it is reached.

**Rejected.** The library as a dependency, which held the pair to one version through the lock at the cost of an internal crate and its dependencies in every build of this crate; a packager, `wasm-pack`, which adds a second build system for no capability.

### The suite runs under nextest, a process to a test, as one run

**Decision.** A test step runs under `cargo-nextest` rather than `cargo test`, as one run: it takes no shard. Nextest is a tool a contributor installs: the step launches it before building anything, as the program `nextest` through its own executable, `cargo-nextest`, and a launch that finds none, or one older than the task files' minimum, is refused with the command that installs it.

**Rationale.** Nextest runs each test as a process of its own, and the compiler's last stage wants one. Binaryen's settings are its process's and its optimizer is not safe to call from two threads, so `curios-binaryen` serializes it behind a lock: tests that share a process, as `cargo test`'s do, queue for it however many cores there are, and a test that aborts — Binaryen answering a module it cannot parse with a C++ `assert` — takes every other result of its binary with it. A process to a test runs the suite on every core and fails an abort by name. The suite is compiled optimized ([A test is compiled optimized, with its checks on](../documentation/design/a-test-is-compiled-optimized-with-its-checks-on.md)), so one job runs it and nothing cuts it into slices. Nextest's command line is the interface it versions, so the flags the task files write are ones a later release still takes.

**Rejected.** `cargo test`, the runner a clone already has: its tests are threads of one process, where the suite waits its turn at Binaryen's lock. Slices of the suite run as parallel jobs, which its time no longer calls for, and with them the oldest nextest that cuts one. Nextest as a library: `nextest-runner` is internal to nextest and breaks with every release. A manifest row naming it, since cargo has no stable way to depend on a tool and a `metadata` table says only what something reads out of it. Installing it from the step, which makes a first test run build a tool nobody asked for. Nextest in the workflow alone beside `cargo test` everywhere else, two spellings of one step that also differ in whether tests share a process.

### The release dance is a command

**Decision.** `release` cuts a release end to end. It reads the workspace version from `Cargo.toml`, resolves the one its argument names, refuses unless the tree is clean, the branch is `main`, `origin/main` holds no commit `main` does not, the tag is free, the target is above the current version and `curios-prelude/std/curios.toml`'s `compiler` minimum matches the current version, then writes both manifests, updates the lock, checks the diff it produced, commits, tags, and pushes `main` and the tag. The bundled standard library's minimum follows each release's version; ordinary packages choose their own minimum under [the package contract](../curios-package/README.md#a-compiler-minimum-belongs-to-the-package-or-its-enumerating-umbrella). Invoking the command is the intent to publish.

**Rationale.** The tag push fires `release.yml`, so it is the step whose spelling this crate exists to fix. The version is computed from the manifest rather than recalled, and "the diff is the workspace version, the lock's member versions and the standard library's compiler minimum" is an assertion rather than a reading. The bundled library ships with the compiler that validates it, so recording that pairing avoids a separate claim about older compilers that the release does not test. The monotonicity refusal closes what the tag check cannot see: the history skips ranges — `0.1.0` is followed by `0.2.1` — so a mistyped version landing in a gap has no tag to collide with. It is Rust and no task, because each of its refusals is a judgment rather than a command to run.

**Rejected.** Stopping before the pushes, which leaves the irreversible step in prose; consulting the check workflow, which has already answered for the commits on `main`; undoing a failed dance, which is destructive and the user's to ask for — the command prints what exists and what would undo it; testing the dance itself, which would cut a release, where every judgment it makes before and after writing is tested; updating the bundled library's minimum by hand, which can drift from the compiler released with it; retaining an older minimum, which promises compatibility outside that validated pairing.

### A command is run or asked, never both

**Decision.** `launch_in`, `run` and `run_in` spawn a program to do something: they echo the command line and inherit the terminal's streams, and `launch_in`, which the runner performs a step's commands with, refuses a command whose program is not there with what installs it. `ask` spawns one to ask something: it echoes nothing, captures stdout and hands it back, and leaves stderr inherited. Both go through one private spawn holding the echo, the status check and the error text.

**Rationale.** A step's output is the user's — it streams live and in colour, and the echoed command line makes the terminal a transcript of the work — while a question's output is the program's, and streaming it prints work that is not being done. An inherited stderr lets a failing question explain itself in the tool's own words.

**Rejected.** One verb that always captures, which costs a test and a format check their colour for a buffer nobody reads; a `git` verb beside `cargo`, for the commands outside the task files, where only `release` asks git anything — it wraps its own calls privately, as `installer` owns `validated`.

### The launcher is built by a cargo of its own

**Decision.** `launcher build` builds `curios-native` in a `cargo` invocation of its own, separate from every build of the workspace, and files what it built where `curios/build.rs` embeds it: the binary and the dep-info cargo wrote beside it, each copied with `cp -p`, keeping the time cargo gave it. `curios/build.rs` reads the dep-info to warn when the launcher is older than a source it was built from.

**Rationale.** Cargo unifies a dependency's features across everything one invocation builds, so a launcher built beside the compiler would link what the compiler turns on; spawned apart, workspace feature unification cannot reach it, and the launcher is built in the one configuration it ships in. The binary's time moves only when cargo built it again, so a run that rebuilt nothing leaves the file the compiler's build script watches as it was, and the compiler is not built again for nothing; a launcher rebuilt after an edit is newer than what it read, so the staleness warning is one `launcher build` always clears. The dep-info is cargo's documented record of what it read, so the build script reads it as cargo wrote it, and does not watch it, since cargo writes it again on every build of the launcher.

**Rejected.** A list of the launcher's inputs written by this crate, with the lock file beside the dep-info's sources, and the filed copy skipped where its bytes did not change and its time refreshed where an input was newer: a second record of what cargo knows, and a refresh that cannot be stated as copying. The dep-info names no registry crate, so a dependency bump is not watched: it reaches the launcher through the task files, where every step that builds `curios` consumes the launcher.
