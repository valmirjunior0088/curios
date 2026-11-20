# The prelude is elaborated once, whatever configurations embed it

Every step that compiles the compiler elaborates, certifies and erases `/std` again, and each makes the same bytes. One elaboration for a state of the sources leaves every step after the first what its own tool takes, and the check a focused change is held to then needs no selection: it runs every step that reads Rust.

The prelude's build script does the work (`curios-prelude/build.rs`, `compile.rs`) and files five images under its `OUT_DIR`, which the library includes (`src/restore.rs`). Cargo gives each build-script fingerprint an `OUT_DIR` of its own, and each configuration links a build script of its own: Clippy against a build, the shipped selection against the jig's, every feature, each profile. So `workspace lint`, `workspace test`, `tools lint`, `tools test`, `docs build`, `instrumented lint`, `compiler build` and a plain `cargo run` each run the work once after a change to a crate the script links.

Read on 2026-11-20 over the tree of `ce7306e3c`, in `target/debug/build/curios-prelude-*/`:

- Counted: four units, made by four configurations of one chain of `workspace lint`, `workspace test`, `tools lint`, `tools test` and `docs build`, hold `sys.rkyv`, `sys.pack`, `std.rkyv`, `std.pack` and `documentation.rkyv` the same byte for byte, by `sha256sum`. `compile.rs` reads no environment, clock or thread count.
- Timed, one reading each, with other sessions building: a run of the script took 154 to 243 s, from a unit's `invoked.timestamp` to the last write under its `out/`; in the check that landed `53df1066c` it ran 388 of `tools lint`'s 546 s and 193 of `tools test`'s 316 s.

`curios-binaryen/build.rs` meets the same fact of Cargo, "distinct build-script fingerprints their own `OUT_DIR`", with one locked entry under `.artifacts/` and a marker written last, and `workspace test binaryen-cache` holds that a second cargo mode reuses it.

The next step is stage 1.

## Decided

Settled with the user on 2026-11-20.

- **One entry a checkout, in `curios-prelude/.artifacts/`.** It holds the five images, the files the lowering read, and a marker written last, behind a lock. A miss replaces it, so it never grows. Cargo still decides when the script runs, so a plain `cargo run` after an edit is right with no step before it.
- **The key is what the script links, read off the manifests.** A digest of the root manifest, the lock, the toolchain file, what `rustc -vV` says, and every file under each workspace crate the script links, its `.artifacts/` apart: the crates `curios-prelude`'s build-dependencies reach through the manifests' dependency tables, 22 of the workspace's 37 today, counted whether a feature turns one on or not, so the set errs toward more. No configuration is in it: no profile, feature, target or `OUT_DIR`. A list written by hand would lag an edge added to a manifest.
- **A hit is verified.** The marker holds each image's digest, and a hit hashes the entry's files against it; an entry that differs is said in a warning and filled again. A compilation still checks no digest of the images it embeds, which are constants of the executable.
- **The released compiler elaborates for itself.** `compiler build` sets a switch under which the script elaborates whatever the entry holds and refuses a difference, so what ships embeds bytes its own configuration made ([A step runs in the configuration that ships](../../xtask/README.md#a-step-runs-in-the-configuration-that-ships)), and a key that left an input out is caught where the handoff runs.
- **Reuse is held as Binaryen's is.** A step builds the prelude in one mode and lints it in a second, under a switch that refuses to elaborate.
- **The decision it restates.** [The archive is build-scoped, not an interchange format](../../curios-prelude/README.md#the-archive-is-build-scoped-not-an-interchange-format) keeps that a compilation replays the images with no source fallback and that the format moves with the compiler; its scope becomes one state of the sources the build links, where it is one unit of Cargo's.
- **Rejected.** A step of its own, as the launcher's is, which a `cargo run` after an edit embeds stale unless every edit runs it first; features and units trimmed until Cargo shares the run itself, which reaches two runs at best and rests on how Cargo fingerprints a Clippy build; a key over every workspace crate, which an edit to the command line or a test of `curios-wonder` between the lint and the suite misses; and a step that reads what a change touched and runs the steps it reaches, which no runner decides exactly and which this cost alone asked for.

## Open

- **Test files in the key.** A crate's test modules are files the script does not compile, so a test fixed between the lint and the suite misses once, at today's cost. Leaving them out by the layout `.claude/rules/rust.md` states needs that layout held by a test; weighed once stage 1's reuse is read.

## Stages

1. **The entry.** `build.rs` computes the key, takes a hit or fills a miss under the lock, verifies a hit, and answers the two switches, each declared to Cargo as an environment it reads; `restore.rs` reads `OUT_DIR` as it does. Two build-dependency rows at most, `toml` and `sha2`, each a crate the script links already. The decision and `compile.rs`'s note on the image are restated.
2. **The holders.** `compiler build` sets the switch that elaborates and compares; `workspace test prelude-cache` builds the prelude and lints it under the switch that refuses to elaborate; the check workflow follows the task files.
3. **The check a focused change runs.** CLAUDE.md's Build and check names the steps that read Rust in the place of its list keyed by what a change touched, and loses its sentence on feature sets; `/tackle-spec` copies the entry into a new worktree with the artifacts it copies already; `xtask/README.md` is restated where it argues from a second build of the images.

## Verification

- Stage 1: over one state of the sources, `workspace lint`, `workspace test`, `tools lint`, `tools test` and `docs build` in turn run the script's work once. A test of the key: an edit to a crate the script links moves it, and an edit to one it does not, or to a file under an `.artifacts/`, leaves it. An image altered in the entry is filled again, with the warning.
- Stage 2: each switch mutation-checked: a key that takes in `OUT_DIR` fails the reuse step, and an entry whose image and digest are altered together fails `compiler build`.
- Stage 3: the readings above retaken on a quiet machine, a median of three with their span, after an edit to `curios-elab/src/lib.rs`, before the stage and after it.

## Retirement

When stage 3 lands, the entry's contract lives in `curios-prelude/README.md` and the steps' in `xtask/README.md`, and this file is deleted.
