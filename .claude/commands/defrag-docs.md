---
description: Defragment the documentation — draw the map of where it has drifted from the tree, propose a charter, and restate it shard by shard, `documentation/` first, then every README, then the code's comments — keeping no record between runs but the tree itself
argument-hint: "[optional — directions in prose: areas to take first or leave alone]"
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(rg:*), Bash(git:*), Bash(cargo:*), Bash(python3:*), AskUserQuestion
disable-model-invocation: true
---

Defragment the documentation with the user, who is present in the thread. `$ARGUMENTS` is directions in prose — areas to take first, what to leave alone — and without any, everything below is in scope. The run goes on, shard by shard, until the user stops it or nothing is left to restate.

**The tree is the only record.** A run keeps nothing for the next: no memory file of where it reached, no scratch ledger, no list handed on. Every run starts cold and reads where the documentation stands off the tree itself, as a defragmenter analyzes the disk before it moves a block: a shard one run restated is a shard the next run's map finds clean, and a run stopped anywhere loses at most the shard in hand. A fact the tree cannot give back is one the run does not depend on.

Keep these steps in view, and say at each which one the run is at:

1. The cold start: the method chosen, the signals measured, the map drawn.
2. The charter, proposed and answered.
3. Each shard in turn: surveyed, changed, measured again, checked, committed.
4. The map drawn again where the run pauses.

## What it covers

Three layers, in this priority: `documentation/`; every `README.md`; the comments of the code — Rust's `//!`, `///` and `//`, and Curios's own in `curios-prelude/std/` — tests included. A test is documentation that runs: its name and its doc comment are the claim, and its body is what is shown of it.

Left alone: `documentation/roadmap/`, which `/triage-findings` refines; `CLAUDE.md` and `.claude/`, whose text is rules, so a change there is asked as any rule is; and every file with uncommitted changes, which are someone else's, as CLAUDE.md says.

## Read first

`.claude/rules/documentation.md` states how a document is written and `.claude/rules/rust.md` how a comment is; [Pending work is owned by its specification](../../documentation/design/pending-work-is-owned-by-its-specification.md) states that pending work belongs to the roadmap and never to a reference. Read those there. This file restates none of them.

## 1. The cold start: how to survey

Read the three layers in their priority to choose a method, and change nothing yet: `documentation/` first, since its conventions are what the READMEs and the comments are held to, then the READMEs, then the comments.

Then measure, for every shard, the signals every run measures — the same ones each time, so two runs over one tree draw one map:

- **Dead references**: a backticked name, path or command a document or a comment states that the tree no longer holds.
- **Drift**: the commits that touched what a document cites — the paths, crates and names it states — since the document itself was last committed.
- **Comments older than their code**: a comment block `git blame` dates before the code lines it documents.
- **Comments the code gives the lie to**: what a comment claims — a name, a behaviour, an invariant, a cost, an order, a case it says cannot happen — held against the code it stands on, its tests and the decision it cites, whatever its age.
- **Tests that do not test what they say**: a test's name and doc comment held against what its body exercises — the input it builds, the assertion it makes, the mutation it says it was checked against.
- **Guarantees overstated or understated**: a comment promising more than the code keeps — always, never, cannot happen, a bound or an order that does not hold — or less than it keeps, so readers guard against what cannot occur.
- **Comments that do not pull their weight**: a restatement of the line beneath it, a duplicate of another comment or of a document, one a reader would not miss.
- **Invariants a comment holds**: a comment asking callers or later code to keep something — an order, a shape, a pairing, a case that must not arise — that a type, a constructor or the grammar could hold instead.
- **Links and anchors**: a relative link or a `#fragment` that resolves to nothing.
- **Retired vocabulary**: a word `.claude/rules/documentation.md`'s retired words name still standing.
- **The house rules**: a hard-wrapped paragraph, a decision missing its rationale or its rejections, a comment in the past tense or stating what the code already says, a paragraph copied rather than linked to its owner.
- **Orphans**: a document nothing links to.

A **shard** is a stable unit one commit closes: a directory of `documentation/`, a crate's README, a crate's comments and its tests', a subject of `/std` with its tests. Its debt is what the signals count in it, and the **map** is the shards worst first, ties in the layers' priority. Where a signal needs judgment past its count — a comment older than its code may still be right — the method says where that judgment goes, worst shards first.

## 2. The charter

Propose the run in one `AskUserQuestion`, the charter in its preview, in prose and short: the shards the run takes and in what order, with their debt, the head of the map beyond them, the checks each shard gets, and the method the cold start chose. The user approves, narrows, widens or reorders it, and nothing changes before.

## 3. The survey and the changes

Take the charter's shards in order. In each, read every document, README or comment, check what it states against the tree, and restate what the tree contradicts.

- **What follows lands on the agent's own word**: a dead path or name restated to where the tree has it, a broken link or anchor fixed, a retired word replaced, a hard wrap joined, a past tense made present, a comment that does not pull its weight removed — a restatement, one a reader would not miss, a duplicate kept once where its owner is — and a stale or wrong comment restated to what the code does, where its tests and the decision it serves show the code is the one right. Each claim written is read off the tree, never off another document.
- **What sets is asked**, its text exactly as it would be written, in at most one question a shard: deleting or merging documents, rewriting a decision's argument, a rename past the retired vocabulary, a change to a rule.
- **A guarantee overstated or understated is restated or filed.** Restated, on the agent's own word, where the code, its tests and the decision it serves show the guarantee plainly; a finding where the gap may be the code's — the comment states what the decision promises and the code does not keep it — or where telling the true guarantee takes more than reading: asked in the shard's one question and filed for a session of its own, the comment left as it stands and nothing investigated in the run.
- **A test whose sentence claims more than its body runs is a finding.** The part nothing runs may hide an edge case, so it is asked in the shard's one question and filed for a session of its own, its sentence left as it stands: the run neither investigates it nor writes a test. A sentence that names its body's claim wrongly, or states less than the body plainly shows, is restated on the agent's own word.
- **An invariant a comment holds that the structure could is a finding**, never restructured here: asked in the shard's one question, its text exactly as it would be filed, and on the user's word filed as `.claude/rules/findings.md` files one, for a session of its own.
- **A disagreement documentation cannot settle is a finding.** Where a document and the code disagree and the code may be the one wrong, change neither: ask the user, and on their word file it as `.claude/rules/findings.md` files a finding.

Close each shard as soon as it is done. Measure its signals again first: what follows lands on the agent's word, so its signals read nothing left, and a shard where they still count something is not done. Then `cargo xtask format`, the checks CLAUDE.md names for what the shard touched, and a commit — one imperative line naming the shard, as `Defrag the soundness decisions`, with its explicit paths staged and nothing else. A run that touched `///` or `//!` runs `cargo xtask docs build` once, at its end, rather than per shard, since that step builds with every feature. Then the next shard.

## A pause

Where the run pauses, say what was committed, by its subject, and draw the map again as the tree now stands. Nothing else is kept.
