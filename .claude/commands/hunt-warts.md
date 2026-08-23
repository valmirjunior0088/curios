---
description: Iterative bug, wart, and misfit hunt — find one, present it, fix on approval, commit, repeat
argument-hint: "[crate or path to hunt in — curios-cont, curios-elab/src/convert.rs, …]"
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(rg:*), Bash(cargo:*), Bash(git:*), Bash(make:*)
---

Hunt the scope `$ARGUMENTS` names for one thing at a time worth fixing. With no scope given, check the hunt-progress memories for what is finished or deferred, pick a crate, and say why.

## What to look for

Judge the code at every altitude, not line by line alone:

- **Wrong answers.** The toolchain misbehaving — a fold that wraps where it must refuse, a cache consulted after its inputs changed, a diagnostic naming the wrong fault. Demonstrate with a scratch program or test before presenting; a plausible story is not a find.
- **Hard to trust.** Locally sound but fragile — one fact spelled twice so the spellings can drift, prose the code has left behind, a name that lies about its roster, an invariant resting on a reader where a type could hold it, a test that passes with the defect present, mechanism where data would do, dead weight, waste with a measurement beside it.
- **Wrong shape.** The structure itself — a responsibility in a crate that shouldn't own it, a seam two components fit badly across, an abstraction at the wrong altitude, parallel machinery where the ownership map says one boundary. Judge against CLAUDE.md's ownership map and the crates' own READMEs.

This repo argues for its odd shapes: read the scope's `//!` docs and README before hunting, and treat a defended shape as a finding only when the defense no longer holds. Anything that makes the code worse and fits no bullet still counts — present it as a judgment call.

## The loop

1. Find one thing.
2. Present it: what and where (`file:line`), why it's wrong, the fix, and a real alternative only if one exists. Then stop and wait.
3. On approval: make exactly that change, nothing beside it. Verify per CLAUDE.md's "While iterating" — workspace clippy and fmt between fixes; below Ersd add `cargo test -p curios --lib`, since no workspace check reaches curios-cont or curios-wasm. Commit the named files only: one-line imperative subject, no body, no trailers.
4. On skip: record it for the final report and move on.
5. Repeat. Anything noticed mid-fix queues for a later round, never bundles in.

A structural finding too large for one sitting is presented, not started: name the end state and the first commit, and the user decides whether it becomes its own task.

Files already modified at session start belong to the user — do not edit or stage them; a finding that lands in one is reported, not fixed.

## Stopping

Stop when the remaining candidates are taste with no consequence, and say so rather than trickling them out. If any Rust changed, run CLAUDE.md's full hand-off gate once. Report fixed (subjects), skipped, and where the hunt should resume, then update the hunt-progress memory to match.
