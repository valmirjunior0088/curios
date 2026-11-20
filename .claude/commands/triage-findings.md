---
description: Triage the findings with the user, in three layers — tackle every finding first, asking the user about each fix, with its reasoning, before it lands; refine the rest of the roadmap whenever the findings leave a wait, every finding it turns up joining the queue; and, beneath both and throughout, brainstorm with the user, take their steering and collect the notes, findings and directions the session turns up — acting on no specification's plan
argument-hint: "[directions in prose — areas, entries, specifications, what to take first or leave alone]"
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(rg:*), Bash(cargo:*), Bash(git:*), Bash(df:*), Bash(mkdir:*), Bash(python3:*), Bash(systemd-run:*), WebSearch, WebFetch, AskUserQuestion
disable-model-invocation: true
---

Triage the findings with the user, who is present in the thread throughout. `$ARGUMENTS` is directions in prose — the areas or entries to take first, the specifications to look at, what to leave alone — and without any, every area is in scope. It is one session, with no hunters, no snapshot and no workflow: every answer here is the user's or the compiler's. To ask the user, anywhere below, is to call `AskUserQuestion`, the question interface Claude Code shows: recommendation first, any draft in an option's preview, and never a question left in prose, which a reader mid-run may not see; a probe asks the compiler. It runs until the user stops it.

## The three layers

1. **The findings, first and always.** Every findings entry is tackled, one at a time, the user asked about each fix before it lands.
2. **The rest of the roadmap, second.** Whenever the findings leave the session waiting — a build, a suite — the rest of the roadmap is refined: the findings the queue has not reached as much as the specifications, and every finding the refining turns up joins the queue.
3. **The conversation, beneath both and constant.** The user brainstorms and steers in the thread. Each direction is taken as it comes and folded into the work, without dropping either layer above it.

Nothing else is done. The roadmap is its documents and never the plan they encode: a specification is refined and never acted upon — no stage of it delivered, no step of it taken, its next-step measurement included — so once every finding has been tackled the session goes on refining the roadmap, and whatever finding that turns up is triaged in its turn.

## Read first

`.claude/rules/findings.md` states what a finding is, where it goes and how one closes; it loads with the first findings file read. [Pending work is owned by its specification](../../documentation/design/pending-work-is-owned-by-its-specification.md) states what an entry and a specification hold, the numbering that orders them — findings, then rules, then costs, then capabilities — and the door a `1nn` specification passes. Each entry names the decision its rule is argued in: read the decision, its rationale and its **Rejected** paragraph before judging the entry. CLAUDE.md says how to probe, build, check and commit. Read those there. This file restates none of them.

## Layer one: the findings

**The inventory.** Before the first fix, read every findings file the directions reach, and say once, in a few lines, what each entry is: settled, its fix stated and its decisions made; waiting on an investigation, with what would settle it; waiting on a decision of the user's; or uncertain. Give each the outcome it is headed for, one of five, since every entry the session takes ends in one: **fixed**, its fix landed; **lifted** into a specification; **merged** into a neighbour; **dropped**, the tree no longer bearing it out or it being worth too little; or **held** uncertain, with what would settle it.

**The queue.** Then say the order you take them in, and why: by the harm each does first — a proof of `False`, then a program compiled to do something else than it says, then a wrong refusal or a wrong report, then a cost, then wording — then a settled entry before one that needs a decision, and the smaller before the larger where nothing else orders them. That order is the queue, and the user may reorder it. A finding filed while the session runs, by it or by a commit another session makes, is classified as it arrives and joins the back of the queue, unless its harm or the user's word places it further up; after each landing, `git log` over the findings files shows what arrived. Take every finding on with confidence, whatever its size.

**Each entry, in the queue's order:**

- **Show it again first.** Reproduce it with this tree's compiler, as CLAUDE.md probes, before trusting a word of it; one the code no longer bears out is dropped, as `.claude/rules/findings.md` says.
- **Question what it touches.** Read the rule it crosses and question the decision three times over: whether its reasoning still holds of the tree as it stands, and whether a revised rule would serve the language's philosophy better and let the code be written more plainly.
- **Survey the peers** where a design is open, and cite them: what each does, why, and what it would mean here.
- **Ask the user, before anything lands.** Every decision is the user's, however small — a name, a range, a flag, a refusal's wording, where a piece lives — the user asked about it on its own. And no fix lands without the user's permission, however settled its entry reads: ask for it with a detailed explanation of the approach and why it works best for Curios, how peers approach the same situation, and where Curios's own design — its checkers, its rules, what it already holds — opens an opportunity no peer has. An answer is followed as it is given, recorded in memory, and not reopened.
- **Land what is approved.** The fix with its tests, each test's claim mutation-checked, the documents its rule is stated in restated, and the entry dropped in the same commit: one commit a fix, its checks those CLAUDE.md names for what it touched, run over a batch as the next bullet says.
- **Land in batches.** Fixes that are ready and settled are asked together, one question naming each, its approach in the preview, and land together: each its own commit, its own test written and mutation-checked as the fix is, then one sweep of the checks CLAUDE.md names over every commit of the batch once the last has landed, under every feature a touched file builds with. What the sweep refuses is fixed up into the commit that owns it, never a commit of its own. A mutation is applied at an anchor that occurs once, the check refused where it does not apply rather than passed, and undone by paths that hold wherever the check runs, so the tree is left as it was found.

**What may be lifted.** A finding, or a group of related ones, that is a well-bounded task of its own, encapsulated and defined enough to be planned as one, is better as a specification: ask the user, with its text exactly as it would be written.

## Layer two: the rest of the roadmap

Whenever the findings leave a wait, and never by waiting: refine the rest of the roadmap — the findings entries the queue has not reached, in every area unless the directions leave one alone, and the specifications. A finding the refining turns up — an entry shown again as a defect it never named, a specification's claim that is a defect today, what a seed shows — is filed on the user's word and joins the queue.

- **Citations and figures.** Each path, name, commit and figure a document states, looked up against the tree, and what is stale restated; a figure whose retaking is a specification's own next step is left to that step.
- **The shape of the roadmap.** Each specification weighed against the numbering and its neighbours, and the user asked about any of these: **reprioritised**, renumbered within its hundred or moved to another — lifted to `1nn` where a rule is broken, or a benefit is beyond doubt with one way to take it and little left to land; **merged** with one related closely enough to be one piece of work; **dropped** where it is worth too little at its priority to keep; **updated** where what it states has moved — its facts, its next step, its stages, its decisions. A findings entry is weighed the same way: merged with its neighbour, or lifted, alone or with others, into a specification. And what is missing is named.
- **Decisions questioned**, from more than one side, with the peers surveyed.
- **What is not written down yet**, hunted from a seed that states a theory a probe can ask:
  - **A defect already fixed.** A commit that deleted a findings entry, or turned a ticket to fixed, states a cause and shows what settled it: `git log` over the findings files and the board's parts, `xboard/tickets/<part>/`. The theory is that the cause stands where the fix did not reach — another crate, another form of the same mistake, the same rule over another carrier.
  - **What landed last.** An area's newest commits, each read whole, its message, its diff and the tests it added. The theory is that a change made for one reason broke what another reader of the same code relies on.
  - **A claim.** A sentence of `documentation/language.md` or `documentation/usage.md`, a decision's rule, a doc comment's contract, and a probe at its edge.
  - **Two answers that must agree.** The elaborator and the certifier over one term, conversion and the compiled program over one closed computation, a unit compiled over a baseline and the same unit compiled whole, `wonder` and `run` over one refusal: which side is wrong is the finding.
  - **A form that must not matter.** Two spellings the theory judges alike — a call direct and through an instance, a term under an arm and outside it, eta-expanded and not, bound by a `let` and inlined — and a verdict that moves with the spelling names the rule.
  - **A check waived.** An `#[allow]` or an `#[expect]` whose reason has lapsed, or never held, at its site or a sibling's.

  The user is asked about what a seed shows, and on their word it is filed as `.claude/rules/findings.md` files a finding, in a commit of its own, and joins the queue; a proof of `False` is filed as an open ticket the moment it is shown, as `.claude/commands/hunt-false.md` files one, before anything else.

Documentation lands in runs, one commit a run: the refinements, additions, removals and updates made between two fixes — an entry filed, restated, merged or dropped, a citation fixed, a decision the user made recorded, a specification renumbered — each amend the run's commit while it is the last one. A fix is a commit of its own, the change with its one finding deleted in the same commit, and the documentation after it begins the next run.

## Layer three: the conversation

The user is in the thread for the whole session, brainstorming and steering. A message from the user is answered when it arrives, whatever is in flight, and its direction is folded into the layer it belongs to; a question of the user's about the work is answered in full, with the evidence. Brainstorm with the user as an equal: offer options, argue for a recommendation, say where you were wrong. Collect what surfaces — a note, a new finding, a new direction, a lesson — as it surfaces: the user is asked about a finding, its text exactly as it would be filed, a direction is taken into the work or the user asked about it, and a note goes into the scratchpad or memory. A question for the user is asked the moment it is ready, and a line of status is given while a long run goes.

## What follows and what sets

A write **follows** where what it claims is shown again by what the tree runs or states, and it is made as it closes: a fix the user approved, committed by itself, and a stale citation restated, joining the run of documentation it belongs to.

A write **sets** where it decides what work is held to, and the user is asked before it is made: a finding the session comes across, whatever surfaced it — a probe, a run, a file read on the way, the conversation — since the defect it claims may not hold or the direction it proposes may be misguided; a new specification, a lift, a merge or a drop, a rule revised, a rejection reopened, a design an entry leaves open. The user is asked about each, its text exactly as it would be written.

## Pacing and records

A long command runs in the background, as CLAUDE.md runs one, and the next piece of work starts at once: a wait is never spent waiting, and one watcher serves a run. A scratch note of what is in flight is kept in the scratchpad, so a long session survives its context being summarized. The user's decisions and the lessons a session teaches go into memory as they are made. Every commit is one imperative line on `main`, as CLAUDE.md commits one, and nothing is pushed.

## A pause

There is no close: the session runs until the user stops it. Where it pauses, say what was committed, by its subject, what is in flight, and which questions are open, and give the queue as it stands: the ready entries in order, and what each held entry waits on.
