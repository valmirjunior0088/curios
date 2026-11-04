---
description: Hunt for what is wrong and not yet written down, gathering what a count of hunters finds, one charter each, and file it where it belongs, one entry a cause: what the tree can show as its cause closes, what would set the roadmap once it is approved
argument-hint: "<count> [directions in prose — a scope: a crate, a path, an area or a theme; a seed: a commit, a decision, a section of the reference; what to look at or leave alone]"
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(rg:*), Bash(cargo:*), Bash(git:*), Bash(df:*), Agent, SendMessage
disable-model-invocation: true
---

Hunt for what is wrong and not yet written down. A charter is a scope, a seed and a way of looking. `$ARGUMENTS` is a count of hunters, 1 or more, then directions in prose: a scope — a crate, a path, an area or a theme — a seed, what to look at or leave alone. Without that count, ask for it and start nothing. You are the gatherer: you cut the scope into one charter a hunter, each hunter hunts its own, and you show again and file what they send, as **Gathering** says. The charters follow the directions, and what they leave out, the seed or the scope itself, is the hunt's to choose. Every charter — all three, and why — is said before its first probe. This command is the user's to invoke and not the model's, so it does not run under `/loop`.

## Read first

`.claude/rules/findings.md` states what a finding is, where it goes and how an entry is kept. It loads with the first findings file read, so read the findings files the scope touches before anything else: for the rule, and for the size, the evidence and the form a finding carries. [Pending work is owned by its specification](../../documentation/design/pending-work-is-owned-by-its-specification.md) states what a findings entry and a spec hold, and where the line between them falls. The scope's `README.md` and `//!` documentation argue for its shape. CLAUDE.md says how to probe and how to commit; this file restates none of it.

## Where a suspicion comes from

A seed states a theory a probe can ask, where a scope alone states none. Take one of six:

- **A defect already fixed.** A commit that deleted a findings entry, or turned a ticket to fixed, states a cause and shows what settled it: `git log` over the findings files and the board's parts, `xboard/src/<part>.rs`, narrowed to the scope. The theory is that the cause stands where the fix did not reach — another crate, another form of the same mistake, the same rule over another carrier.
- **What landed last.** The scope's newest commits, each read whole, its message, its diff and the tests it added. The theory is that a change made for one reason broke what another reader of the same code relies on, and that no hunt has been over it.
- **A claim.** A sentence of `documentation/syntax.md` or `documentation/usage.md`, a decision's rule, a doc comment's contract. Each says what a program does, and a probe at its edge asks whether it does.
- **Two answers that must agree.** Where the tree answers one question twice, ask both and compare: the elaborator and the kernel over one term, conversion and the compiled program over one closed computation, a unit compiled over a baseline and the same unit compiled whole, `wonder` and `run` over one refusal. A disagreement is shown by itself, and which side is wrong is the finding.
- **A form that must not matter.** Two spellings of one program the theory judges alike, put to the same question: a call direct and through an instance, a term under an arm and outside it, eta-expanded and not, bound by a `let` and inlined, a case split one way and the other. A verdict that moves with the spelling is the finding, and the spelling that moved it names the rule.
- **A check waived.** A lint exemption, `#[allow]` or `#[expect]`, states a reason the code is held to by hand where the compiler would have held it. The theory is that the reason has lapsed, or never held, at this site or at a sibling that waived the same check.

The scope bounds where a hunt starts, never where a cause is followed.

## What a hunt can judge

A write **follows** where everything it claims can be shown again by what the tree runs or states: an entry added, restated, merged or dropped on a probe, a test or a rule and its code, and a ticket `cargo xboard report` shows open. Whoever fixes an entry shows its defect again first and drops one the code does not bear out (`.claude/rules/findings.md`), so a wrong entry costs one investigation and nothing is held to it. Such a write is made as its cause closes, and invoking this command is standing authorization for it and for its commit.

A write **sets** where it states what work is held to, and it waits for the user:

- **A new specification.** It takes a number, a place in the order work is taken.
- **A finding against stated reasoning.** Its case is that a sentence the tree argues by fails.

## A refusal met on the way

When a probe you wrote is refused, do not rewrite it until it compiles. First decide which of three things the refusal is, and say which:

- the theory forbids the program — the refusal is right and the probe was wrong;
- the theory allows it and the rule over-approximates — lifting the rule is a finding;
- the refusal is right but the diagnostic misnames the fault, its span, or what the reader needs — the diagnostic is the finding.

"The elaborator was just being conservative" is the comfortable reading, not the demonstrated one: read the refusing rule and the syntax reference against each other before choosing. Lifting a rule argued under `documentation/design/soundness/` is a finding only with that rule named.

## Hunting

Take a suspicion the seed raises and hunt it until it is shown, let go, or seen with what would show it lying past the charter. Shown, close its cause as `.claude/rules/findings.md` closes one. Let go, keep one line of it: what was asked, by which probe, and what answered. Seen and no further, it is an entry marked uncertain, with what would settle it. A proof of `False` is a red alert: it is filed as an open ticket the moment it is shown, as `.claude/rules/findings.md` says, before hunting on, and its fix is not this command's. Then hunt on. The charter is spent when every suspicion its seed raised is shown, let go or filed uncertain; a scope that would take another seed is another charter's.

## Hunting for a gatherer

Up to here this file is the hunter's as much as the gatherer's; from **Filing** on it is the gatherer's, and the writes it makes are never yours. How a unit is sent, and what every one carries, is `.claude/agents/hunter.md`'s. Under this command you send three kinds:

- **A finding**, `finding: <the finding>`, carries its entry as [Pending work is owned by its specification](../../documentation/design/pending-work-is-owned-by-its-specification.md) forms one — the finding in bold, where, what is wrong, the fix, its check and its size in commits — with its kind under `.claude/rules/findings.md`, the findings file it belongs to, and whether its write follows or sets as **What a hunt can judge** draws the line, and, where it meets an entry or a specification, which one and what it adds. One seen and not yet shown says so, with what would settle it, and is filed uncertain.
- **A candidate for `False`**, as **Hunting for a gatherer** of `.claude/commands/hunt-soundness.md` sends one: a proof of `False` is a red alert wherever it is met.
- **A held bundle**, as **Hunting for a gatherer** of `.claude/commands/hunt-soundness.md` sends one, over the probes of your charter that held and whose claim no test holds: an attack that held is a test where [the soundness board](../../documentation/design/soundness/the-soundness-board.md) places one.

## Filing

Where a write follows, make it as its cause closes, in the form [Pending work is owned by its specification](../../documentation/design/pending-work-is-owned-by-its-specification.md) states for an entry or `xboard/README.md` for a ticket, and commit it as prose, by itself. A finding whose fix changes documentation alone — a comment, a doc comment, a `.md` file — is fixed on the spot instead, in a commit of its own, and never filed. Where it sets, hold it with its text exactly as it would be written. A probe that held whose claim no test holds leaves a test, as **Attacking a charter** of `.claude/commands/hunt-soundness.md` leaves one, and a charter's tests land together in one commit, its subject led by `Findings hunt: `.

## Gathering

1. **Before spawning.** Cut the scope into one charter a hunter — the scope, a seed of its own and a way of looking — disjoint from its siblings', so a cause two hunters meet is a sibling's by charter and not by chance; say them, and why. Make the snapshot, as **The snapshot** says, and give each hunter its directory there: every hunter reads the tree the snapshot froze and probes its binaries, so nothing landed in the main checkout moves under one, and none builds. Then spawn the count of `hunter` agents, `.claude/agents/hunter.md`, in one message, naming `model` on each call, since a definition's own field is not always applied mid-session; each prompt carries this file's path, its charter, the snapshot's path and its directory there, its siblings' charters and the commit of the tree, `git rev-parse --short HEAD`, and no suspicion of yours: a hunter handed a hypothesis returns it confirmed, so one you hold goes as a question to refute. From then on hunt nothing yourself; your context is for what comes back.
2. **On a unit.** It arrives as `.claude/agents/hunter.md` sends one, carrying what **Hunting for a gatherer** says of its kind, and nothing in it is filed on the hunter's word. Show it again by the command it carries, in the main checkout — a candidate by probing the files it carries, a test by landing it, its run left to the sweep of step 4 — and read it against every entry, specification, ticket and **Rejected** paragraph that names the same code or rule: the hunter read them, and you are the one closing the cause. Then file it as **Filing** files one, hold it where it sets, send it back with what it lacks, or answer it as already held, naming what holds it, and write nothing. A candidate for `False` whose probe exits 0 is a ticket filed open, alone, ahead of everything else waiting. An entry, a ticket, a fix and a test each land in the main checkout as its cause closes, since no hunter builds there, and what each commit's check runs waits for that sweep. Where a cause needs its extent swept or its probe narrowed, send that to the hunter that found it, by `SendMessage`: it has the context, and it resumes. Otherwise answer a hunter only where a unit of its meets something filed or sent back, or where a ticket filed names its charter: what one hunter hands another is the tree as committed, and nothing else.
3. **On a report.** A hunter's final report comes whole when it finishes; keep its let-go lines and what it did not reach for the close. The report is a pause: a message resumes the hunter with its context, so a hunt is ended by asking for its report, and `TaskStop`, which no message can undo, is for a hunter that does not answer. A report marked partial, the turn budget spent, is read like any other, and the hunter is resumed where its charter is not spent.
4. **After the last report.** Run once, over every commit the run made, what their checks name — `cargo xtask format`, `cargo xtask workspace lint`, each test by name, and `cargo xtask workspace test` where a fix landed — and fix what fails in a commit of its own. Then close as **Close** says.

This is one gatherer, and no workflow or team: the compiler answers every unit with one command, so a second judge adds a mechanism where the oracle stands, and a single writer is what keeps one entry a cause.

## The snapshot

A hunt reads a snapshot, `.claude/snapshots/<commit>/`, which the checkout ignores: `tree/` holds the tree at the hunt's commit, `bin/` the `curios` and `xboard` binaries built once from it in the main checkout, and `hunters/hunter-<n>/` each hunter's directory, the one place it writes, where it prototypes its candidates and the tests it sends. Every hunter of a run shares the tree and the binaries and builds nothing, so the main checkout moves on under its own commits while a hunter's answers stay those of the commit it hunts. A worktree under `.claude/worktrees` is one the user asks for, and no hunt's.

- **Before spawning.** Read `df`. With the tree clean, `git status --short` printing nothing, the commit is `git rev-parse --short HEAD`: build the binaries at it, `cargo xtask compiler build` and `cargo build --release --package xboard`, then make the snapshot, `mkdir -p .claude/snapshots/<commit>/tree .claude/snapshots/<commit>/bin`, `git archive <commit> | tar -x -C .claude/snapshots/<commit>/tree`, `cp target/release/curios target/release/xboard .claude/snapshots/<commit>/bin/`, and `mkdir -p .claude/snapshots/<commit>/hunters/hunter-<n>` for each hunter.
- **At the close.** Once every unit a hunter sent has been shown again in the main checkout, remove the snapshot, `rm -rf .claude/snapshots/<commit>`: a hunter's directory is scratch the gatherer owns, so what no unit carried goes with it.

## Close

Close with the charters, each with its hunter, and three lists. What was written: any ticket filed, first, then every write made, by file and by kind, with its commit. What held: each suspicion let go, by its line, and what of the scope the seed did not reach, so a quiet scope is told from a shallow hunt. What is held: each write that sets, put to the user in one round of structured questions. Each question carries where the write goes and its text exactly as it will be written, in the form [Pending work is owned by its specification](../../documentation/design/pending-work-is-owned-by-its-specification.md) states for an entry and a spec; the evidence and the rule or the consequence it rests on; why nothing written down already holds it; and, where the tree argues otherwise, the sentence whose defence fails.

Write what is approved and commit it as prose, by itself. A write declined is not proposed again in this session. For a later hunt to know, the round offers a third answer beside writing and declining: declined, with its reason recorded. That reason is written where the tree argues — one comment saying why at the site, or a sentence of the **Rejected** paragraph of the decision that owns the rule — so the next hunt meets it as stated reasoning. The close names each write declined with no reason recorded, which a later hunt may raise again.

Remove the snapshot as **The snapshot** says. Last, offer in one message, one line each with its cost, what was held as taste.
