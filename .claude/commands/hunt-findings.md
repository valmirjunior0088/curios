---
description: Hunt for what is wrong and not yet written down, one charter an invocation, and file it where it belongs, one entry a cause — what the tree can show as its cause closes, what would set the roadmap once it is approved
argument-hint: "[scope — a crate, a path or an area — and optionally a seed: a commit, a decision, a section of the reference]"
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(rg:*), Bash(cargo:*), Bash(git:*)
disable-model-invocation: true
---

Hunt for what is wrong and not yet written down. One invocation is one charter: a scope, a seed and a way of looking. `$ARGUMENTS` names the scope — a crate, a path or an area — and may name the seed; what it leaves out is the hunt's to choose. Say all three, and why, before the first probe.

## Read first

`.claude/rules/findings.md` states what a find is, where it goes and how an entry is kept. It loads with the first findings file read, so read the findings files the scope touches before anything else: for the rule, and for the size, the evidence and the form a find carries. The opening paragraphs of `documentation/roadmap.md` state what a findings entry, an open line and a spec hold. The scope's `README.md` and `//!` documentation argue for its shape. CLAUDE.md says how to probe and how to commit; this file restates none of it.

## Where a suspicion comes from

A seed states a theory a probe can ask, where a scope alone states none. Take one of four:

- **A defect already fixed.** A commit that deleted a findings entry, or turned a ticket to fixed, states a cause and shows what settled it: `git log` over the findings files and `xboard/src/board/`, narrowed to the scope. The theory is that the cause stands where the fix did not reach — another crate, another form of the same mistake, the same rule over another carrier.
- **What landed last.** The scope's newest commits, each read whole, its message, its diff and the tests it added. The theory is that a change made for one reason broke what another reader of the same code relies on, and that no hunt has been over it.
- **A claim.** A sentence of `documentation/syntax.md` or `documentation/usage.md`, a decision's rule, a checked line of the roadmap, a doc comment's contract. Each says what a program does, and a probe at its edge asks whether it does.
- **Two answers that must agree.** Where the tree answers one question twice, ask both and compare: the elaborator and the kernel over one term, conversion and the compiled program over one closed computation, a unit compiled over a baseline and the same unit compiled whole, `wonder diagnostics` and `run` over one refusal. A disagreement is shown by itself, and which side is wrong is the find.

The scope bounds where a hunt starts, never where a cause is followed.

## What a hunt can judge

A write **follows** where everything it claims can be shown again by what the tree runs or states: an entry added, restated, merged or dropped on a probe, a test or a rule and its code, and a ticket `cargo xboard report` shows open. `/fix-findings` shows an entry's defect again before any fix and drops one the code does not bear out, so a wrong entry costs one investigation and nothing is held to it. Such a write is made as its cause closes, and invoking this command is standing authorization for it and for its commit.

A write **sets** where it states what work is held to, and it waits for the user:

- **An open line and its spec.** It takes a number, a place in the order work is taken.
- **A find against stated reasoning.** Its case is that a sentence the tree argues by fails.

## A refusal met on the way

When a probe you wrote is refused, do not rewrite it until it compiles. First decide which of three things the refusal is, and say which:

- the theory forbids the program — the refusal is right and the probe was wrong;
- the theory allows it and the rule over-approximates — lifting the rule is a find;
- the refusal is right but the diagnostic misnames the fault, its span, or what the reader needs — the diagnostic is the find.

"The elaborator was just being conservative" is the comfortable reading, not the demonstrated one: read the refusing rule and the syntax reference against each other before choosing. Lifting a rule argued under `documentation/design/soundness/` is a find only with that rule named.

## The loop

1. Take a suspicion the seed raises and hunt it until it is shown or let go. Shown, close its cause as `.claude/rules/findings.md` closes one. Let go, keep one line of it: what was asked, by which probe, and what answered.
2. Where its write follows, make it, in the form `documentation/roadmap.md` states for an entry or `xboard/README.md` for a ticket, and commit it as prose, by itself. Where it sets, hold it with its text exactly as it would be written.
3. Hunt on.

## Stopping

Stop when the charter is spent: every suspicion its seed raised is shown or let go. A scope that would take another seed is another invocation's.

Close with the charter and three lists. What was written: every write made, by file and by kind, with its commit. What held: each suspicion let go, by its line, and what of the scope the seed did not reach, so a quiet scope is told from a shallow hunt. What is held: each write that sets, put to the user in one round of structured questions with where it goes, its text exactly as it will be written, in the form `documentation/roadmap.md` states for an entry, an open line and a spec; the evidence and the rule or the consequence it rests on; why nothing written down already holds it; and, where the tree argues otherwise, the sentence whose defence fails.

Write what is approved and commit it as prose, by itself. A write declined is not proposed again in this session. For a later hunt to know, the round offers a third answer beside writing and declining: declined, with its reason recorded. That reason is written where the tree argues — one comment saying why at the site, or a sentence of the **Rejected** paragraph of the decision that owns the rule — so the next hunt meets it as stated reasoning. The close names each write declined with no reason recorded, which a later hunt may raise again.

Last, offer in one message, one line each with its cost, what was held as taste, and each probe that held whose claim no test holds, with where a test of it would stand: an attack that found nothing is a test in the crate it attacked, and the user picks which are written.
