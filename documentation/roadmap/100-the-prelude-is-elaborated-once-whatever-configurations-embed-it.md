# The prelude is elaborated once, whatever configurations embed it

The prelude's build script makes its images once a state of the sources it links and files them in the checkout's one entry, from which a run under any other configuration copies them ([The archive is build-scoped, not an interchange format](../../curios-prelude/README.md#the-archive-is-build-scoped-not-an-interchange-format), `curios-prelude/entry.rs`), and `workspace test prelude-cache` holds the entry. What rests on it is not yet written down: CLAUDE.md's Build and check keys the check a focused change runs by what the change touched, and asks for one feature set a session, for an elaboration of the prelude each configuration no longer pays; `/tackle-spec` copies no entry into a new worktree, whose first step then makes again what the main checkout made; and `xtask/README.md`'s "A step runs in the configuration that ships" argues from a second build of the images.

The next step is stage 1.

## Decided

Settled with the user on 2026-11-20.

- **The check a focused change runs** is `cargo xtask format`, then every step that reads Rust — `workspace lint`, `instrumented lint`, `tools lint`, and `docs build` last — plus the change's own tests by name, or `tools test` where they are the jig's, and the corpus tests where `curios-cps`, `curios-emit` or `curios-wasm` changed. Test code alone and Markdown alone keep their lines.

## Stages

1. **The check a focused change runs.** CLAUDE.md's Build and check as decided, losing its sentence on feature sets and the prelude's clause of its sentence on by-package runs; `/tackle-spec` copies `curios-prelude/.artifacts` beside the artifacts it copies already; `xtask/README.md` loses its arguments from a second build of the images.

## Verification

- Stage 1: the script's own run retaken on a quiet machine, a median of three with their span, the figure the entry's rationale quotes: with the entry removed and `curios-elab/src/lib.rs` touched, the span of the unit `cargo build --package curios-prelude` runs, from its `invoked.timestamp` to its last write.

## Retirement

When stage 1 lands, the steps' contract lives in `xtask/README.md` and CLAUDE.md and the entry's in `curios-prelude/README.md`, with its timed figure, and this file is deleted.
