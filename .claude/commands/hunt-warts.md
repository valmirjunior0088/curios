---
description: Hunt for what is wrong and not yet written down, and file it where it belongs, up to ten entries a batch, each batch once it is approved
argument-hint: "[crate, path or area to hunt in — curios-cont, curios-elab/src/convert.rs, compilation, …]"
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(rg:*), Bash(cargo:*), Bash(git:*)
---

Hunt the scope `$ARGUMENTS` names for what is wrong and not yet written down. With no scope given, choose one and say why.

## What a find is

A defect with a consequence for someone — the user, the next reader, the next editor — shown rather than told: a wrong behaviour by a probe or a test, or a departure from a rule the tree states, by the rule and the code that departs from it. Each kind names the document that states its standard, where the tree has one:

- **Wrong answers.** The toolchain computing what `documentation/syntax.md` or a design decision says it does not — a fold that wraps where it must refuse, a cache consulted after its inputs changed, a verdict the theory does not license.
- **Wrong refusals.** A program the syntax reference and the theory license that the compiler refuses, or accepts only after a workaround the language should not need. Reading Rust does not reach this: probe the scope's boundary on standard input, as CLAUDE.md says, and treat each refusal as evidence about the compiler until shown otherwise.
- **Wrong words.** The verdict is right and the message is not, measured against `documentation/design/tools/a-diagnostic-spells-what-its-reader-can-write.md`. Read every message a probe produces as the person who wrote the program, not the one who wrote the rule.
- **Hard to trust.** Locally sound but fragile, measured against `.claude/rules/rust.md` and, for prose, `.claude/rules/documentation.md`: one fact spelled twice, prose the code has left behind, an invariant a reader holds where a type could, a test that passes with the defect present, dead weight.
- **Wrong kind.** An item cast as a kind of thing it is not — a closure where a named function would carry a name and a doc, a free function whose first argument is the receiver every caller holds, an alias over a primitive where a nominal type would keep its invariant — so what a value may be, and where its operations live, cannot be read off the item.
- **Wrong shape.** A responsibility in a crate that should not own it, a dependency named where another crate seals it, a seam two components fit badly across, parallel machinery where one boundary is stated, measured against the crates' READMEs, the area rules in `.claude/rules/` and `documentation/design/one-crate-is-the-authority-for-one-external-concern.md`.

A change whose only case is taste — no rule the tree states, no consequence beyond reading better — is held, not found.

**Weigh stated reasoning three times.** Where the tree argues for what you are about to call a wart — a decision's rationale, a **Rejected** paragraph, a comment saying why — consider it three times over before judging against it: what it claims, whether the code is still what it argues about, and whether its premise still holds. A defended shape is a find only where its defence fails, and the find names the sentence that fails.

What is written down already is known: an entry in a findings file, an open line of `documentation/roadmap.md`, a ticket on the board, a decision's **Rejected** paragraph.

## Where a find goes

A bug or a quick win is an entry in the findings file of its area, or in the workspace's for what belongs to no area. A departure from a rule that is no quick win is an open line of `documentation/roadmap.md` with a spec not yet refined, placed where the roadmap's opening paragraphs order a broken rule — after the findings, before every capability and cost — among its area's specs, or among the workspace's, loose in `roadmap/`, for what belongs to no area.

A closed term the checkers admit at `/std/Bool/False` is never set aside: it is an open ticket on the soundness board, filed as `/hunt-unsoundness`'s **The open ticket** files one, and it joins the batch like any entry. A term admitted that should be refused, from which no `False` is built, is a finding, in soundness's findings file.

## A refusal met on the way

When a probe you wrote is refused, do not rewrite it until it compiles. First decide which of three things the refusal is, and say which:

- the theory forbids the program — the refusal is right and the probe was wrong;
- the theory allows it and the rule over-approximates — lifting the rule is a find;
- the refusal is right but the diagnostic misnames the fault, its span, or what the reader needs — the diagnostic is the find.

"The elaborator was just being conservative" is the comfortable reading, not the demonstrated one: read the refusing rule and the syntax reference against each other before choosing. Lifting a rule argued under `documentation/design/soundness/` is a find only with that rule named.

## Read first

The opening paragraphs of `documentation/roadmap.md` state what a findings entry, an open line and a spec hold, and where each goes. The entries already filed in the findings files the scope touches show the size, the evidence and the form a find carries. The scope's `README.md` and `//!` documentation argue for its shape. CLAUDE.md says how to probe and how to commit; this file restates none of it.

## The loop

1. Hunt until a batch is ready: anything from none to ten finds.
2. Present the batch, by consequence, and wait. For each entry, where it goes — a findings file, the roadmap with its spec, or a part of the board — and its text exactly as it will be written, in the form `documentation/roadmap.md` states for an entry, an open line and a spec, or `xboard/README.md` for a ticket; then why it belongs there — the evidence, the rule or the consequence it rests on, why nothing written down already holds it, and, where the tree argues otherwise, the sentence whose defence fails.
3. On approval, write what was approved and commit it as prose, by itself; an approved ticket is its own commit, as `/hunt-unsoundness` records it. An entry the user declines is not proposed again.
4. Hunt on to the next batch.

## Stopping

Stop when the scope yields nothing more with a consequence, after offering what was held as taste in one message, one line each with its cost, for the user to pick from. Report the entries filed, by file, and those declined.
