---
description: Iterative bug, wart, and misfit hunt — find one, present it, fix on approval, commit, repeat
argument-hint: "[crate or path to hunt in — curios-cont, curios-elab/src/convert.rs, …]"
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(rg:*), Bash(cargo:*), Bash(git:*)
---

Hunt the scope `$ARGUMENTS` names for one thing at a time worth fixing. With no scope given, pick a crate, and say why.

## What to look for

Judge the code at every altitude, from the message a user reads to the crate graph, not line by line alone:

- **Wrong answers.** The toolchain misbehaving — a fold that wraps where it must refuse, a cache consulted after its inputs changed, a verdict the theory does not license. Demonstrate with a scratch program or test before presenting; a plausible story is not a find.
- **Wrong refusals.** A program `documentation/syntax.md` and the theory license that the compiler refuses, or accepts only after a workaround the language should not need. Reading Rust does not reach this altitude: probe the scope's boundary with scratch programs on standard input, through `wonder diagnostics -` and `?`, and treat each refusal as evidence about the compiler until shown otherwise.
- **Wrong words.** The verdict is right and the message is not — it names a fault other than the one made, points at a span other than where it was made, prints an internal spelling where the surface one exists, or stops short of what the reader needs to make the next move. Read every message a probe produces as the person who wrote the program, not the one who wrote the rule; the rendered message beside the program is the demonstration.
- **Hard to trust.** Locally sound but fragile — one fact spelled twice so the spellings can drift, prose the code has left behind, a name that lies about its roster, an invariant resting on a reader where a type could hold it, a test that passes with the defect present, mechanism where data would do, dead weight, waste with a measurement beside it.
- **Wrong kind.** An item cast as a kind of thing it is not — a closure bound to a local where a named function would carry a name and a doc, a free function whose first argument is the receiver every caller already holds, a method on a receiver it barely reads while another type holds what it needs, an alias over a primitive standing where a nominal type would keep its invariant and give its operations a home, a struct grown by accretion whose fields split into rosters that never change together. The consequence is the reader's: what a value may be, and where its operations are, cannot be read off the item.
- **Wrong shape.** The structure itself — a responsibility in a crate that shouldn't own it, a crate naming a dependency another crate seals for it (`documentation/design/toolchain/one-crate-is-the-authority-for-one-external-concern.md`), a seam two components fit badly across, an abstraction at the wrong altitude, parallel machinery where the ownership map says one boundary. Judge against CLAUDE.md's ownership map and the crates' own READMEs.

This repo argues for its odd shapes: read the scope's `//!` docs and README before hunting, and treat a defended shape as a finding only when the defense no longer holds.

A finding is a defect, the consequence it has for someone — the user, the next reader, the next editor — and the cost of the fix; rank by consequence over cost, and anything with a consequence counts whether or not it fits a bullet. A change whose only case is taste — the code would read better and nothing else changes — is held, not presented: when the consequential candidates run out, list the held taste in one message, one line each with its cost, for the user to pick from. A pick goes through the loop like any other finding.

## A refusal met on the way

When a probe you wrote is refused, do not rewrite it until it compiles. First decide which of three things the refusal is, and say which:

- the theory forbids the program — the refusal is right and the probe was wrong;
- the theory allows it and the rule over-approximates — lifting the rule is a finding, presented like any other;
- the refusal is right but the diagnostic misnames the fault, its span, or what the reader needs — the diagnostic is the finding.

"The elaborator was just being conservative" is the comfortable reading, not the demonstrated one: read the refusing rule and the syntax reference against each other before choosing. A rule inside the soundness perimeter (`documentation/soundness.md`) is lifted only with its row — present the change with the row named, never fix it inline.

The opposite discovery — a program accepted that should be refused — is not a wart. Hand it to `/hunt-unsoundness` with the row named, and do not fix it here.

## The loop

1. Find one thing. When more than one is pending, take them by consequence, not by the order found.
2. Present it: what and where (`file:line`), why it's wrong, the fix, and a real alternative only if one exists. Then stop and wait.
3. On approval: make exactly that change, nothing beside it. Between fixes the check is `cargo fmt --all` and `cargo clippy --workspace --all-targets --all-features -- -Dwarnings`, plus one fast crate-local test run only to prove a test you added passes. A change touching only prose (`//!`, `///`, comments, Markdown) commits as soon as it is written and waits on no check. Below Ersd, clippy proves nothing about behavior, so a fix there is unverified until the gate runs — say so in the report. Commit the named files only: one-line imperative subject, no body, no trailers.
4. On skip: record it for the final report and move on.
5. Repeat. Anything noticed mid-fix queues for a later round, never bundles in.

A structural finding too large for one sitting is presented, not started: name the end state and the first commit, and the user decides whether it becomes its own task.

Files already modified at session start belong to the user — do not edit or stage them; a finding that lands in one is reported, not fixed.

## Stopping

Stop when the consequential candidates are exhausted and the taste list has been offered. The full hand-off gate is the user's to schedule — about every ten fixes, when they name it — so never run it unprompted; the report lists which fixes have had clippy alone over them. Report fixed (subjects) and skipped.
