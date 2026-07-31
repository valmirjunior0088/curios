---
description: Hunt for an unsoundness in the trusted base, record it as a regression test, and land the fix only when it is unambiguous
argument-hint: "[auto] [perimeter row — check_positivity, (V), Coverage, Large-elimination guard, Index inversion and K, …]"
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(rg:*), Bash(cargo:*), Bash(git:*), Bash(make:*)
---

Hunt for an unsoundness in the trusted base.

## Authority

`$ARGUMENTS` selects the mode. The word `auto` anywhere in it means unattended; anything else is an interactive session. Whatever remains names a row of the soundness perimeter table to attack, spelled as that table spells it. With no row given, choose one by "Where to hunt" and say why.

**Interactive is read-only.** Investigate, report, propose the fix and its control test, and wait — whatever the gate would say. Discussing why a rule is wrong is worth more here than a fast patch.

**Auto is the user's standing authorization, and it covers exactly two kinds of commit:** one carrying a regression test together with the fix it guards, and one carrying a probe with its Status update to DESIGN.md's perimeter table. For those it overrides CLAUDE.md's prohibition on unattended edits and commits, and for nothing else. Commit on whatever branch is already checked out; never create, switch, or rebase one. Prefer stopping to guessing — idleness is the correct failure mode here, overreach is not.

**Pre-flight, before any investigation.** Run `git status`. Every file already modified belongs to the user: do not edit it, stage it, or read a finding into its contents. If the row you mean to attack is implemented in one, choose another row or stop. A regression test left uncommitted by an earlier run is covered by the same rule — it is a finding waiting for a human, not work to resume, and the row it names is spent for this run.

Running a witness needs the embedded launcher built first — `make curios/runtime`, then `cargo run --package curios -- run <file.crs>`. Scratch programs stay in the scratchpad; they are how you find the hole, never how you record it.

## Read before forming a hypothesis

Everything factual about the perimeter lives elsewhere and is read there. This file restates none of it, deliberately: a second copy drifts, and the last one did.

- `documentation/DESIGN.md`, "The soundness perimeter" — every rule that can admit a term, each graded *probed*, *argued*, or *auditable only*, plus which entries are load-bearing and unargued.
- `documentation/DESIGN.md`, "An independent kernel re-checks what the elaborator accepts" — what the second checker buys, and in numbers what it does not.
- `curios-cert/README.md`, "Incompleteness is the safe direction".
- `curios/src/tests/perimeter.rs`, `soundness.rs`, and `kernel.rs` — what is already guarded, by which diagnostic, and what each checker is recorded as saying about it.

## What counts as a find

Three things qualify, in descending order of severity:

- a **witness program** — a `.crs` file the compiler accepts and should not, one inhabiting `/syn/False`, or one whose erased output diverges where totality was required
- a **term the kernel certifies and the elaborator refuses**, which is either unsoundness in the certifier or unnecessary strictness in the elaborator, as set out below
- a **certifier relying on a conclusion it did not establish** — the kernel believing elaborator output rather than certifying it

What unites them is demonstration. A plausible story about a rule being wrong is not a find: this project's wrong answers have repeatedly come from a diagnosis derived by reading a refusal and reasoning about which pass let it through, naming a mechanism that was working correctly. Run the program, or construct the term and hand it to the kernel. Do not reason your way to a verdict.

## Where to hunt

The kernel runs on the compile path, so a *disagreement* between the two checkers already fails the build. Do not hunt disagreement. Hunt **agreement on a wrong rule**, which lives in three places:

- the shared analyses behind the `Env`/`Judge` seam — inversion, positivity, size-change totality, level entailment — where one implementation serves both checkers and no disagreement is structurally possible, and where the known `Prop`-valued index defect actually lived
- the elaborator-only whole-module passes the kernel never re-runs
- rules both sides implement identically wrong

Rank candidates by that asymmetry and by the weakness of their Status, not by how easy they are to test.

The third class of find has one operative sentence: **the kernel relies on something it did not itself decide.** Reading elaborator output is not the violation — it is the kernel's entire input. Believing it is. Two things are routinely misreported as violations and are not: an evaluation memo replays the kernel's own pure function of the terms, and `recheck_module_suffix`'s archived prefix rests on the kernel's own full walk at archive-build time. Cached kernel judgment is not inherited elaborator judgment. Report a genuine dependency even when it is currently benign; it is the shape every future unsoundness will arrive through.

## The reverse direction

The instrumented direction is the kernel refusing what the elaborator accepted. The opposite — the elaborator refusing what the kernel would certify — means exactly one of two things, and both matter: the **certifier is too permissive**, which is unsoundness in the trusted base, or the **elaborator is unnecessarily strict**, which is a usability defect and evidence the two decide by different rules where they should agree. Decide which and say so plainly. "The elaborator was just being conservative" is the comfortable reading, not the demonstrated one.

`both_checkers` in `curios/src/tests/perimeter.rs` reaches that direction, but only as far as it can: `typecheck_reporting` defers the erasure obligations so a program only the elaborator refuses still yields a module for the kernel to judge, and every other refusal happens while the module is still being built, so the kernel never sees it — which is what `Expect::NotAsked` records, and why it is not a pass. Past the erasure obligations, reaching this direction means constructing the finished term by hand and handing it to the kernel. **No test of that shape exists yet**, so a finding there invents its own home as well as its fixture; say so in the report rather than forcing it into a file whose fixtures are all surface programs.

## Technique follows Status

- **auditable only** — no surface program reaches the rule, so probing is off the table. Read the implementation, or count whether the rule ever fires.
- **argued, unprobed** — an argument exists but no adversarial program. Attack the argument's premise and check its reasoning still holds against the current code.
- **probed** — the rule has programs already. Check whether they exercise the *side condition* or merely the easy rung. The large-elimination guard was graded "probed, both directions" while its probes hit the rungs rather than the singleton condition, and a closed inhabitant of `False` followed.

Counting deserves particular weight. Every defect this design has produced was found by counting or by probing, and none by the corpus failing — the corpus passed throughout the period (V)'s argument rule was inert at 6010 of 6041 sites. A rule that never fires is invisible to any program you can write.

## On a null result

Attacking a row and finding nothing is a result, not a wasted iteration: it is the difference between *unprobed* and *probed*. Commit the probe and the Status update — the Status column is this hunt's only memory across runs, so an unrecorded null result will be re-attacked. In interactive mode, propose it and wait.

## Recording a find

The deliverable is a Rust regression test, and it is **never committed ignored and never committed alone**. A test that asserts a rejection no rule performs is a red build with an excuse attached; an `#[ignore]` on it is that excuse. Either the fix is unambiguous and the two land in one commit, or nothing is committed at all — see "The fix gate".

Write the test first, before attempting the fix. It fails while the hole is open, and that failure is the demonstration: it is what turns a story about a rule into a find.

Fixtures go to `perimeter.rs` when they guard a perimeter row, `soundness.rs` when they guard a totality obligation. Follow the conventions of the file you are writing into — each documents its own, and a fixture that reads unlike its neighbours is the one that rots. Three of those conventions are not local style and hold everywhere:

- **Assert the diagnostic, never bare failure.** A soundness test that accepts any error is worthless: a typo in the fixture passes it while the hole stays open. The substring must name the rule doing the rejecting.
- **Pair it with a control.** The witness proves the hole is shut; only the control proves you shut it with something other than a brick.
- **The comment carries the reasoning** — the mechanism, why the gap is a closed inhabitant of `False`, and what the paired control covers. These comments are the real documentation of the perimeter; write one at that standard.

In the comment, record how the hole was verified while it was open — that the program compiled, that the compile-path recheck certified it, and what it did at runtime (printed `FORGED`, trapped at an `unreachable`). That verification is what separates a witness from a suspicion, and it is what a reader needs to re-confirm months later that the test guards something real. Write it in the past tense: the commit that adds the test is the commit that closes the hole, so a present-tense note is false the moment it lands.

A certifier dependency on elaborator output is the one finding with no natural fixture. Report it, and say what re-deriving it would take.

## The fix gate

Run all of it, in order, and read the output:

```sh
make curios/runtime
cargo fmt --all -- --check
cargo check --workspace --all-targets --all-features
RUSTFLAGS="-Dwarnings" cargo clippy --workspace --all-targets --all-features
cargo test --workspace --all-targets --all-features > /tmp/curios-hunt.txt 2>&1
cargo test --workspace -- --ignored kernel_disagreements
```

Every command must pass, the witness must now be rejected by the diagnostic it names, and `kernel_disagreements` must report zero items refused for every fixture. If its output cannot be read as a count, that is a stop, not a pass.

Two conditions the script cannot check, which are judgments and are named as such:

- **The control test passes.** A witness alone proves the hole is shut, not that you shut it with something other than a brick; the guard fix shipped with `an_unmentioned_payload_binder_is_not_forced` precisely to prove it had not closed the hole by rejecting every indexed proposition. If the fix has no control, it is not ready.
- **The fix diff is additive or corrective only.** No deleted assertion, no loosened bound, no `#[ignore]` anywhere, no edited expectation on an existing test. Making a rule stricter always looks safe given that incompleteness is the safe direction, and it is not: a stricter rule may reject valid programs, which is what the prelude compiling and the disagreement count exist to catch.

Clear all of it and the test and the fix go in one commit, and the hunt continues.

**If the fix is not unambiguous, correct, and targeted, yield immediately — in auto mode as much as in interactive.** Do not move to another candidate, do not try a second approach, do not commit a partial fix, and do not weaken the test to fit what you have. Leave the regression test on the tree, uncommitted and unstaged, exactly as it stands: a failing test nobody has explained away is the honest handoff, and it is the run's deliverable. Say in the report which file it is in and what it asserts. Stopping in the middle of the night is fine.

That the tree is left dirty is deliberate and is not a half-edit. A half-edit is a partial fix; this is a complete demonstration with the fix withheld.

## Commits

Imperative, capitalized, one line. No body, no bullet points, no trailing explanation, no `Co-Authored-By` or any other attribution — ever, in any mode. `Refuse a universe context that names what it does not declare` and `Answer an out-of-set polarity by coverage instead of by the carried vector` are the house style: the subject names the rule now enforced, not the test that guards it.

Commit only what the current step authorizes, and never sweep in a file the pre-flight found already modified.

## Stopping and reporting

Auto mode stops on any of: a fix that does not clear the gate, a gate check that cannot be run, an uncommitted regression test on the tree, an exhausted candidate list, or anything unexpected in the build or test infrastructure. Never retry an approach that already failed, and never attack the same row twice in one run.

Close with the same five fields every time, so consecutive runs are diffable: **row attacked**, **technique**, **verdict**, **what was committed**, and **what is on the tree uncommitted** — naming the file and what the test asserts, or `nothing` when the tree is clean.
