---
description: Fix findings entries — every one whose fix follows what the tree states, then those whose fix would set it once the user answers for them, each fixed on the spot or lifted into a spec
argument-hint: "[autonomous | supervised]"
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(rg:*), Bash(cargo:*), Bash(git:*)
disable-model-invocation: true
---

Fix entries of the findings files, each area's `documentation/roadmap/<area>/000-findings.md` and the workspace's `documentation/roadmap/000-findings.md`. A run is one procedure, and `$ARGUMENTS` says how far it goes unattended:

- `autonomous` makes every fix that follows without asking, and asks once, at its close, for those that set.
- `supervised` waits for the user once the cases are sorted and before each commit of a case that sets.

With none given, work supervised.

## Read first

`.claude/rules/findings.md` states what a find is, how stated reasoning is weighed and how an entry is kept; it loads with the first findings file read. **When a step fails** of `.claude/commands/run-full-gate.md` says how a failing check is read and its cause fixed. The opening paragraphs of `documentation/roadmap.md` state what an entry and a spec hold, and when one becomes the other. CLAUDE.md says how a focused change is checked and committed. This file restates none of them.

## What every fix is

An entry is a claim someone filed, and the code may have moved since. Before changing anything, show the defect as the entry states it, by the probe it gives or by the code it names read against the rule it cites, and weigh what the tree argues for that code as `.claude/rules/findings.md` weighs stated reasoning: the decision the entry cites, the crate's `README.md`, the `//!` and the comments saying why. Then sweep the cause's sites as the workspace stands now: the entry was swept at an older commit, and a site it does not name is the same case where the same change settles it, and a find where it does not. The sweep also names the cause: where the sites each state for themselves a rule their siblings state too, the cause is the second statement, and what settles it is one statement every site reads, never a patch at the entry's site, which writes a third. Before adding a refusal there, see whether the form already fails and is only reported in the wrong place. An entry marked uncertain is investigated until what it says is not known is known.

- **A drop**, where the code no longer bears the entry out, is one commit that deletes it and nothing else.
- **A fix** is one commit that makes the fix, lands the test that holds it, and deletes the entry, as the opening paragraphs of `documentation/roadmap.md` require. A fix that changes what a program does, is told or is refused lands with a test that fails without it: the one its entry's check names, or one written for it where the entry names none. A fix that changes none of that has no such test to add, and lands on the third rule below. Its check is the entry's own and CLAUDE.md's for a focused change, and where it changes code, the whole test suite, `cargo xtask workspace test`; a comment or a doc comment is not code. It is committed as CLAUDE.md says. Anything noticed on the way is a find, never part of it.

A fix is held to three things beside its entry's check:

- **Its test fails first.** A test the fix adds is run with the defect present, before the fix or with the fix reversed under it, and fails for the entry's reason.
- **It loosens nothing.** A failing check is read as **When a step fails** of `.claude/commands/run-full-gate.md` reads one and its cause fixed as that section fixes one; the one expectation a fix rewrites is the one its entry names as wrong.
- **It moves what its entry says it moves.** Where the entry holds a form to the bytes it was, or names the declarations and reports that change, a difference past that is a failed check.

## What a run can judge

A finding is a departure, and its entry names what from: a rule a document states, a paragraph of the reference, a signature, a test, the bytes a form must keep. A fix **follows** where it brings the code to that and leaves standing everything else the tree states: a message brought to the wording the reference promises, a walk replaced by the one a decision names, a fact stated twice folded into its first statement. Its check holds it to what is written, and a fix that fails is undone, so the tree judges it: it is autonomous, whatever its size. What a fix answers to is the rule its entry cites, never the entry's own fix, which proposes a way of following it.

A fix **sets** where making it would write what the tree judges by. Nothing in the tree can then say it is wrong, so it is supervised. A fix sets where it would:

- **Pick a shape.** Later code, or a reader, would be built on which fix was made — a type's variants, where a responsibility lives, what a stored form holds — and more than one follows with nothing the tree states ranking them: no decision, no crate's `README.md`, no rule under `.claude/rules/`. A second design can always be found; a case sets only where something would rely on the one picked.
- **Move a rule.** It contradicts a sentence of `documentation/design/`, of a crate's `README.md` or of a **Rejected** paragraph that still stands once weighed, or it needs a rule no document states. Prose a fix leaves false is corrected with it; a rule is not rewritten to fit one.
- **Move the reference.** `documentation/syntax.md` or `documentation/usage.md` would state after it what neither states now. Where one states it already and the code falls short, the fix follows.
- **Widen the trusted base.** The kernel, or an analysis it shares with the elaborator, admits a term it refused or checks less of one, and nothing checks the kernel. Refusing what the entry's rule refuses follows.
- **Move a public contract.** What a consumer of one CLAUDE.md names relies on differs after it: a stage's form, a host row, what the launcher or the harness answers, a public name of `/std`. Code behind a contract it leaves standing follows.

What following leaves open is the run's to choose where a rule the tree states holds the choice and nothing relies on which was made: a private name, a message's wording under `documentation/design/tools/a-diagnostic-spells-what-its-reader-can-write.md`, where a helper stands in its crate, one mechanism over another where no caller and no check tells them apart. Each such choice is listed in the close.

A fix that is not one pass is no finding's: the opening paragraphs of `documentation/roadmap.md` make it a spec, and a lift is put to the user as a case that sets is.

## Sorting the cases

1. Read every entry of every file and gather them into cases. A case is an entry and every other the same change settles, with every open spec that names the same code or follows from the same cause.
2. Show each case's defect again and sweep its sites, as **What every fix is** says. Investigate each entry marked uncertain.
3. Sort each case into one of five: a **drop**, the code no longer bearing it out; one that **follows**, with the rule its fix answers to named; one that **sets**, with which of the five it would do; a **lift**, its fix not one pass; one that **waits**, on a reading no quiet machine allows or on another case, with what for. Where nothing can be named for a fix to answer to, or the fix would rewrite what is named, it sets.
4. Order them: a case another waits on before it, and otherwise the one of most consequence first.

Under `supervised`, present the table and wait: each case, its entries, how it was sorted and why, and what the run would commit next.

## The cases that follow

Invoking `autonomous` is standing authorization for five kinds of commit: the fix of a case that follows, the drop of an entry, the restatement of an entry with what the run learned of it, the finds the run files at its end, and the fix of a case the user answered for. Under `supervised`, the user's word on the table gives the same for the cases it names.

1. Drop each entry the code no longer bears out, and restate each the sorting learned something of that its text does not say: an uncertain entry over a narrower unknown, a figure taken again, a site the sweep found.
2. Make each fix that follows, one commit a case. Cases that share no file, none naming what another adds, moves or removes, may share one run of `cargo xtask workspace lint` and `cargo xtask workspace test`, each having passed its own check by name before it and each committed by itself after it. A shared run that fails is no case's verdict yet: name the case by what the failing test exercises, confirm it by running that test with that case's patch reversed, and where no one case answers for it, run each alone.
3. Undo a fix where the making shows it sets after all, or where its check fails for a cause that is the stated fix's and not a slip in the making. Reverse exactly its own edits, by `git apply -R` over a patch of them, never by a reset, a checkout or a stash, which take with them what another session wrote. Then restate its entry with what was tried and what the check showed. Never try a second design unasked, and never guess.
4. Take the measurement an entry asks for where a figure can be recorded as `.claude/rules/documentation.md` states one, a timed one on a quiet machine alone.
5. File what the run noticed on the way, each a find as `.claude/rules/findings.md` states one, in the findings file of its area or the workspace's and in the form `documentation/roadmap.md` states, all in one commit of prose.

## The cases that set

Put every case whose fix sets, and every lift, to the user in one round of structured questions: what the code shows and whether it still bears each entry out, the alternatives with their trade-offs, the one recommended first, the steps each ending on its check, and for a lift the spec's open line and what the spec will state. Each answer is the go-ahead for that case.

- **A fix** is made as a case that follows is, a step at a time where it takes several. Where a step meets a decision the case did not settle, stop and ask. Under `supervised`, each step ends on a checkpoint — what changed, what its check said, what comes next — and the commit waits for the user's word on the result.
- **A lift** writes the spec and its open line as the opening paragraphs of `documentation/roadmap.md` state, or rewrites the open spec that already covers the case, deletes the case's entries, and presents the draft.
- **An answer the session does not carry out** is written into its entry, which states one fix from then on.

## Close

The commits made, by subject; each choice the run made, by case, a wording in full; each entry dropped or restated, with why; each fix undone, with what its check showed; each case that waits, with what on; the specs lifted; each entry filed; and the entries left, with where each stands.
