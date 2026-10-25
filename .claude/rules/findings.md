---
paths:
  - "documentation/roadmap/000-findings.md"
  - "documentation/roadmap/**/000-findings.md"
---

# Findings: what a finding is, where it goes, how an entry is kept

Whoever writes a findings file is held to this: a hunt, a fix run, a campaign filing what it noticed on the way. The form of an entry and a spec is [Pending work is owned by its specification](../../documentation/design/pending-work-is-owned-by-its-specification.md)'s; this file restates none of it.

## What a finding is

A defect with a consequence for someone — the user, the next reader, the next editor — shown rather than told: a wrong behaviour by a probe or a test, or a departure from a rule the tree states, by the rule and the code that departs from it. Each kind names the document that states its standard, where the tree has one:

- **Wrong answers.** The toolchain computing what `documentation/syntax.md` or a design decision says it does not — a fold that wraps where it must refuse, a cache consulted after its inputs changed, a verdict the theory does not license.
- **Wrong refusals.** A program the syntax reference and the theory license that the compiler refuses, or accepts only after a workaround the language should not need. Reading Rust does not reach this: probe the scope's boundary on standard input, as CLAUDE.md says, and treat each refusal as evidence about the compiler until shown otherwise.
- **Wrong words.** The verdict is right and the message is not, measured against `documentation/design/tools/a-diagnostic-spells-what-its-reader-can-write.md`. Read every message a probe produces as the person who wrote the program, not the one who wrote the rule.
- **Hard to trust.** Locally sound but fragile, measured against `.claude/rules/rust.md` and, for prose, `.claude/rules/documentation.md`: one fact spelled twice, prose the code has left behind, an invariant a reader holds where a type could, a test that passes with the defect present, dead weight.
- **Wrong kind.** An item cast as a kind of thing it is not — a closure where a named function would carry a name and a doc, a free function whose first argument is the receiver every caller holds, an alias over a primitive where a nominal type would keep its invariant — so what a value may be, and where its operations live, cannot be read off the item.
- **Wrong shape.** A responsibility in a crate that should not own it, a dependency named where another crate seals it, a seam two components fit badly across, parallel machinery where one boundary is stated, measured against the crates' READMEs, the area rules in `.claude/rules/` and `documentation/design/one-crate-is-the-authority-for-one-external-concern.md`.
- **Wrong cost.** Work a decision says is not done, or a figure a document rests on that a reader can no longer retake — a walk per path where a decision states one per node, an instrument that hears nothing of what it counts, a reading that names no commit — measured against the decision that states the cost and against what `.claude/rules/documentation.md` asks of a measured figure.

A change whose only case is taste — no rule the tree states, no consequence beyond reading better — is held, not found.

**Weigh stated reasoning three times.** Where the tree argues for what you are about to call a finding — a decision's rationale, a **Rejected** paragraph, a comment saying why — consider it three times over before judging against it: what it claims, whether the code is still what it argues about, and whether its premise still holds. A defended shape is a finding only where its defence fails, and the finding names the sentence that fails.

What is written down already is known: an entry in a findings file, a specification under `documentation/roadmap/`, a ticket on the board, a decision's **Rejected** paragraph.

## Where a finding goes

A finding goes into its area's findings file as an entry or among its area's specifications, as [Pending work is owned by its specification](../../documentation/design/pending-work-is-owned-by-its-specification.md) draws the line between the two and numbers a specification; what belongs to no area goes into the workspace's findings file, or loose in `documentation/roadmap/`. A point seen and not yet shown goes in as an entry marked uncertain, as the same decision states one.

A proof of `False`, a closed term the checkers admit at `/std/Bool/False`, is never set aside, whoever meets it: it is filed and fixed as [Pending work is owned by its specification](../../documentation/design/pending-work-is-owned-by-its-specification.md) states, its ticket written as **The open ticket** of `.claude/commands/hunt-soundness.md` writes one, and it leads the close of whatever run met it. A term admitted that should be refused, from which no `False` is built, is a finding like any other ([`xboard`'s README](../../xboard/README.md#a-ticket-is-a-proof-of-false-and-no-witness-states-the-type)); one seen that might reach `False` and not driven there is an entry marked uncertain, naming the `False` it might reach.

## One entry a cause

A cause is what one change settles, and a findings file holds one entry for each, standing while the code bears it out. Every write follows from that:

- **Add** an entry where a cause is shown and nothing written down holds it, marked uncertain where it is seen and not yet shown.
- **Restate** the entry that holds a cause where the hunt has shown what its text does not say, or no longer says truly: another site, code that moved, a figure taken at an older commit, a narrower unknown.
- **Merge** two entries that one change settles, and split one that two changes settle.
- **Fold** a finding into the specification whose stages settle it, as a site in what the specification states is wrong today and where; no entry is added, since a specification owns its pending work and has no second inventory. Where the finding shows the specification's goal or stages wrong, that restatement sets.
- **Drop** an entry the code no longer bears out, or one that a spec or a ticket now holds.

A cause is closed once three things are done: it is shown, by a probe or a test or by the rule and the code that departs from it; its extent is swept, every site that shares it found, in the workspace and not only the scope it was met in, so the entry is whole; and it is read against every entry, specification, ticket and **Rejected** paragraph that names the same code or the same rule. Whoever closes a cause reconciles the entries and specifications it meets and no others. Whoever fixes an entry shows its defect again first, as the entry states it, and drops the entry instead where the code no longer bears it out.
