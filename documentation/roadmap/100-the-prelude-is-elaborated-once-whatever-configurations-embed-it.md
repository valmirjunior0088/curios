# The prelude is elaborated once, whatever configurations embed it

The prelude's build script makes its images once a state of the sources it links and files them in the checkout's one entry, from which a run under any other configuration copies them ([The archive is build-scoped, not an interchange format](../../curios-prelude/README.md#the-archive-is-build-scoped-not-an-interchange-format), `curios-prelude/entry.rs`). What rests on it is not yet held or written down:

- No step asks the build script what it answers to, `CURIOS_PRELUDE_ENTRY=reuse` and `compare`, so nothing the handoff or the check workflow runs holds that a second configuration reuses the entry, or that the images made again are those the entry holds for their key.
- CLAUDE.md's Build and check keys the check a focused change runs by what the change touched, and asks for one feature set a session, for an elaboration of the prelude each configuration no longer pays; `/tackle-spec` copies no entry into a new worktree, whose first step then makes again what the main checkout made; and `xtask/README.md`'s "A step runs in the configuration that ships" argues from a second build of the images.

The next step is stage 1.

## Decided

Settled with the user on 2026-11-20.

- **One step holds the entry, asking two cargo modes.** `workspace test prelude-cache` builds the prelude alone asked to compare, so it makes the images again and holds them to the entry the lint filled, then lints the prelude alone asked to reuse. A selection of the prelude alone puts together host dependencies of its own, so its units are ones no other step builds, and the variable is declared to Cargo, so the step runs the script whatever ran before it. On the check workflow the workspace job runs the lint and then this step on a fresh machine, so every push compares two configurations' images byte for byte.
- **No profile is told apart.** A release build takes the entry as any other configuration does, and what is released is made on machines whose caches keep no entry.
- **Test files stay in the key.** A test-only edit makes Cargo run the script nowhere, since a crate's library reads none of its tests, and with every lint before the tests in the check a focused change runs, a test fixed after them runs no script; the case left, a configuration first run after a test fix, costs one elaboration.
- **The check a focused change runs** is `cargo xtask format`, then every step that reads Rust — `workspace lint`, `instrumented lint`, `tools lint`, and `docs build` last — plus the change's own tests by name, or `tools test` where they are the jig's, and the corpus tests where `curios-cps`, `curios-emit` or `curios-wasm` changed. Test code alone and Markdown alone keep their lines.

## Stages

1. **The holder.** `xtask/tasks/workspace.toml` gains `test.prelude-cache`, which the check workflow's `workspace test` runs with no edit, and `.claude/rules/prelude.md` names it.
2. **The check a focused change runs.** CLAUDE.md's Build and check as decided, losing its sentence on feature sets and the prelude's clause of its sentence on by-package runs; `/tackle-spec` copies `curios-prelude/.artifacts` beside the artifacts it copies already; `xtask/README.md` loses its arguments from a second build of the images.

## Verification

- Stage 1: run right after `workspace lint`, the step passes; a key that takes in `OUT_DIR` fails its second command, and an entry whose image and digest are altered together fails its first.
- Stage 2: the script's own run retaken on a quiet machine, a median of three with their span, the figure the entry's rationale quotes: with the entry removed and `curios-elab/src/lib.rs` touched, the span of the unit `cargo build --package curios-prelude` runs, from its `invoked.timestamp` to its last write.

## Retirement

When stage 2 lands, the steps' contract lives in `xtask/README.md` and CLAUDE.md and the entry's in `curios-prelude/README.md`, with its timed figure, and this file is deleted.
