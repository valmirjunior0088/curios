---
description: Hunt for an unsoundness in the trusted base, record it as a regression test, and land the fix only when it is unambiguous
argument-hint: "[perimeter row — check_positivity, (V), Coverage, Large-elimination guard, Index inversion and K, …]"
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(rg:*), Bash(cargo:*), Bash(git:*)
---

Hunt for an unsoundness in the trusted base.

## Authority

`$ARGUMENTS` names an entry of the soundness perimeter to attack, spelled as `documentation/soundness.md` spells it. With no row given, choose one by "Where to hunt" and say why.

**One invocation is one row.** The run ends when the row's outcome is committed or a stopping condition hands control back. Iteration lives outside — run this under `/loop` to hunt while away from the keyboard — and the committed Status rows are what one run hands the next.

**Invoking this command is the user's standing authorization, and it covers exactly two kinds of commit:** one carrying a regression test, the fix it guards, and the updated `documentation/soundness.md` entry recording the find, and one carrying a probe with its evidence recorded against the entry it attacked in `documentation/soundness.md`. For those it overrides CLAUDE.md's prohibition on unattended edits and commits, and for nothing else. Commit on whatever branch is already checked out; never create, switch, or rebase one. Prefer stopping to guessing — idleness is the correct failure mode here, overreach is not.

**Pre-flight, before any investigation.** Run `git status`. Every file already modified belongs to the user: do not edit it, stage it, or read a finding into its contents. If the row you mean to attack is implemented in one, choose another row or stop. A regression test left uncommitted by an earlier run is covered by the same rule — it is a finding waiting for a human, not work to resume, and the row it names is spent for this run.

Running a witness needs the embedded launcher built first — `cargo x runtime`, then `cargo run --package curios -- run <file.crs>`. Scratch programs stay in the scratchpad; they are how you find the hole, never how you record it.

## Read before forming a hypothesis

Everything factual about the perimeter lives elsewhere and is read there. This file restates none of it, deliberately: a second copy drifts, and the last revision of this file did.

- `documentation/soundness.md` — every rule that can admit a term, each graded *probed*, *argued*, or *auditable only*, with one file per entry carrying its evidence, and `documentation/soundness/across-the-perimeter.md` carrying the routes named and not yet attacked. `documentation/design/language/the-soundness-perimeter.md` holds the decision behind it and nothing else; the grades and the evidence are not there.
- `documentation/design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md` — what the second checker buys and what it does not — and `documentation/design/language/the-perimeters-boundaries.md`, the edges of the region the rules are looked for in.
- `curios-cert/README.md`, "Incompleteness is the safe direction".
- `curios/src/tests/perimeter.rs` and `soundness.rs` — what is already guarded from the surface, by which diagnostic, and what each checker is recorded as saying about it; each file's own documentation names the entries homed elsewhere. The constructed-term regressions live beside the rules they guard, in `curios-cert`'s and `curios-analysis`'s own test modules, and the rows name them.

## What counts as a find

Three things qualify, in descending order of severity:

- a **witness program** — a `.crs` file the compiler accepts and should not, one inhabiting `/syn/False`, or one whose erased output diverges where totality was required
- a **term the kernel certifies and the elaborator refuses**, which is either unsoundness in the certifier or unnecessary strictness in the elaborator, as set out below
- a **certifier relying on a conclusion it did not establish** — the kernel believing elaborator output rather than certifying it

What unites them is demonstration. A plausible story about a rule being wrong is not a find: this project's wrong answers have repeatedly come from a diagnosis derived by reading a refusal and reasoning about which pass let it through, naming a mechanism that was working correctly. Run the program, or construct the term and hand it to the kernel. Do not reason your way to a verdict.

## Where to hunt

The kernel runs on the compile path, so a *disagreement* between the two checkers already fails the build. Do not hunt disagreement. Hunt **agreement on a wrong rule**, which lives in four places:

- the shared analyses behind the `Env`/`Judge` seam, where one implementation serves both checkers and the known `Prop`-valued index defect actually lived. `curios-analysis`'s crate documentation is the roster; do not recite one from memory — an earlier revision of this file counted level entailment among them after design.md's amendment had already corrected exactly that, and the amendment records a hunt the wrong roster misdirected
- the rules only one checker ever reaches — the rows record which ones, and whose copy is inert
- rules both sides implement identically wrong
- the components a rule reads an answer out of rather than deriving — the *Admission without judgment* and *What the kernel consults* parts of the table, where every grade is **argued** and admitting is done by believing

Rank candidates by that asymmetry and by the weakness of their Status, not by how easy they are to test. The index in `documentation/soundness.md` carries the grades; each entry's own file carries what has already been tried; `documentation/soundness/across-the-perimeter.md` carries the missing tests already named, which is the candidate list before it is anything else.

The third class of find has one operative sentence: **the kernel relies on something it did not itself decide.** Reading elaborator output is not the violation — it is the kernel's entire input. Believing it is. The perimeter now rows what the kernel consults and what is admitted without judgment, so check the table before reporting: a memo replay and the archived prefix's recorded walk are cached kernel judgment, not inherited elaborator judgment, and their rows say so — while the same part of the table names the verdicts that *are* believed rather than re-derived, each a standing target its own row describes. Report a genuine dependency the table does not already carry, even when it is currently benign; it is the shape every future unsoundness will arrive through.

## The reverse direction

The instrumented direction is the kernel refusing what the elaborator accepted. The opposite — the elaborator refusing what the kernel would certify — means exactly one of two things, and both matter: the **certifier is too permissive**, which is unsoundness in the trusted base, or the **elaborator is unnecessarily strict**, which is a usability defect and evidence the two decide by different rules where they should agree. Decide which and say so plainly. "The elaborator was just being conservative" is the comfortable reading, not the demonstrated one.

`both_checkers` in `curios/src/tests/perimeter.rs` reaches that direction, but only as far as it can: `typecheck_reporting` defers the erasure obligations so a program only the elaborator refuses still yields a module for the kernel to judge, and every other refusal happens while the module is still being built, so the kernel never sees it — which is what `Expect::NotAsked` records, and why it is not a pass. Past the erasure obligations, reaching this direction means constructing the finished module by hand and asking `recheck_module_verdicts` directly — and that home exists: `curios-cert/src/recheck/` names itself the place for the hand-built adversarial modules — one file per rule the walk decides, over the shared forgeries in its `test_support.rs` — and the rows name the fixtures already living there and beside the rules in the kernel's own test modules. A finding of this shape goes with those neighbours, never into a file whose fixtures are all surface programs.

## Technique follows Status

- **auditable only** — no surface program reaches the rule, so probing is off the table. Read the implementation, or count whether the rule ever fires.
- **argued, unprobed** — an argument exists but no adversarial program. Attack the argument's premise and check its reasoning still holds against the current code.
- **probed** — the rule has programs already. Check whether they exercise the *side condition* or merely the easy rung. The large-elimination guard was graded "probed, both directions" while its probes hit the rungs rather than the singleton condition, and a closed inhabitant of `False` followed.

Counting deserves particular weight. Every defect this design has produced was found by counting or by probing, and none by the corpus failing — the corpus passed throughout the period (V)'s argument rule sat inert at nearly every site it was written for. The figures live beside the probes that retake them, not here. A rule that never fires is invisible to any program you can write.

## On a null result

Attacking a row and finding nothing is a result, not a wasted iteration: it is the difference between *unprobed* and *probed*. Commit the probe and the Status it updates — that entry's Status in `documentation/soundness.md` is this hunt's only memory across runs, so an unrecorded null result will be re-attacked.

**A null's gate is everything below except the suite** — `cargo x runtime` (the workspace does not build without the embedded launcher), `cargo fmt --all -- --check`, clippy as the fix gate spells it, and one targeted run of the probe in the form being committed: `cargo test --package <crate> <probe_name>`. That last is not the suite creeping back in — fmt and clippy compile a test without running it, and evidence gathered from an earlier spelling of the probe is evidence about that spelling. Clear those and commit. The reason the suite is missing is the reason it is present for a fix: it is there to catch a rule that now over-refuses, and a probe leaves every rule exactly as it found it — that is what makes it a null. The compiler decides the same thing after the commit as before, so there is nothing for the suite to catch, and running it costs minutes per iteration to confirm an answer it cannot change.

## Recording a find

The deliverable is a Rust regression test, and it is **never committed ignored and never committed alone**. A test that asserts a rejection no rule performs is a red build with an excuse attached; an `#[ignore]` on it is that excuse. Either the fix is unambiguous and the test, the fix, and the row's updated account land in one commit, or nothing is committed at all — see "The fix gate".

Write the test first, before attempting the fix. It fails while the hole is open, and that failure is the demonstration: it is what turns a story about a rule into a find.

A fixture goes where its row's neighbours already live. From the surface, `curios/src/tests/perimeter.rs` and `soundness.rs` are the defaults, and each file's own documentation names the entries homed elsewhere; a constructed-term regression lives beside the rule it guards, in the owning crate's test module, the way the fixtures the rows name already do. Follow the conventions of the file you are writing into — each documents its own, and a fixture that reads unlike its neighbours is the one that rots. Three of those conventions are not local style and hold everywhere:

- **Assert the diagnostic, never bare failure.** A soundness test that accepts any error is worthless: a typo in the fixture passes it while the hole stays open. The substring must name the rule doing the rejecting.
- **Pair it with a control.** The witness proves the hole is shut; only the control proves you shut it with something other than a brick.
- **The comment carries the reasoning** — the mechanism, why the gap is a closed inhabitant of `False`, and what the paired control covers. These comments are the real documentation of the perimeter; write one at that standard.

In the comment, record how the hole was verified while it was open — that the program compiled, that the compile-path recheck certified it, and what it did at runtime (printed `FORGED`, trapped at an `unreachable`). That verification is what separates a witness from a suspicion, and it is what a reader needs to re-confirm months later that the test guards something real. Write it in the past tense: the commit that adds the test is the commit that closes the hole, so a present-tense note is false the moment it lands.

The find is recorded twice, and the second record rides the same commit: update the entry's section in `documentation/soundness.md` — and its grade, where the find moves it — the way every row's account of a demonstrated defect got there. A fix that lands without its row leaves the next run reading a grade the hole already falsified.

A certifier dependency on elaborator output is the one finding with no natural fixture. Report it, and say what re-deriving it would take.

## The fix gate

A fix changes what the compiler decides, so it clears all of this, in order, and you read the output:

```sh
cargo x runtime
cargo fmt --all -- --check
cargo clippy --workspace --all-targets --all-features -- -Dwarnings
cargo test --workspace --all-targets --all-features > /tmp/curios-hunt.txt 2>&1
```

This is CLAUDE.md's hand-off gate, the suite's output redirected as its iteration advice asks; the reasons for its exact shape — no `check` step, the Clippy denial after the separator — are argued there and not restated here. Every command must pass, and the witness must now be rejected by the diagnostic it names. The suite is in this list for one reason, and it is the same reason a null skips it: making a rule stricter always looks safe given that incompleteness is the safe direction, and it is not — a stricter rule may reject valid programs, and the standard library plus the whole corpus compiling is the only thing that catches one which now over-refuses.

**Do not run the ignored `kernel_disagreements`.** It is a measurement, not an assertion — it prints a per-class tally and never fails, so a green run of it establishes nothing. What the two checkers are held to each other on is enumerated in `documentation/soundness/across-the-perimeter.md`, and the strongest of those runs inside the gate above without being a test: a kernel edit re-runs `curios-prelude`'s build script, which walks the whole prelude with the kernel from an empty environment and panics on the first refusal, so the suite *compiling* is already the whole-prelude verdict — and every `.crs` fixture then compiles through `compile_entrypoint`, which runs `recheck_module_suffix` on the compile path. Note which named comparison carries your rule rather than re-running the tally.

Two conditions the script cannot check, which are judgments and are named as such:

- **The control test passes.** A witness alone proves the hole is shut, not that you shut it with something other than a brick; the guard fix shipped with `an_unmentioned_payload_binder_is_not_forced` precisely to prove it had not closed the hole by rejecting every indexed proposition. If the fix has no control, it is not ready.
- **The fix diff is additive or corrective only.** No deleted assertion, no loosened bound, no `#[ignore]` anywhere, no edited expectation on an existing test. Each of those is a way to make the suite green without making the rule right, which is exactly what the over-refusal check above cannot see.

Clear all of it and the test, the fix, and the row's updated account go in one commit, and the run is complete — the next invocation continues the hunt.

**If the fix is not unambiguous, correct, and targeted, yield immediately.** Do not move to another candidate, do not try a second approach, do not commit a partial fix, and do not weaken the test to fit what you have. Leave the regression test on the tree, uncommitted and unstaged, exactly as it stands: a failing test nobody has explained away is the honest handoff, and it is the run's deliverable. Say in the report which file it is in and what it asserts. Stopping in the middle of the night is fine.

That the tree is left dirty is deliberate and is not a half-edit. A half-edit is a partial fix; this is a complete demonstration with the fix withheld.

## Commits

Imperative, capitalized, one line. No body, no bullet points, no trailing explanation, no `Co-Authored-By` or any other attribution — ever. `Refuse a universe context that names what it does not declare` and `Answer an out-of-set polarity by coverage instead of by the carried vector` are the house style: the subject names the rule now enforced, not the test that guards it.

Commit only what the current step authorizes, and never sweep in a file the pre-flight found already modified.

## Stopping and reporting

A run stops early on any of: a fix that does not clear the gate, a gate check that cannot be run, an uncommitted regression test on the tree, an exhausted candidate list, or anything unexpected in the build or test infrastructure. Never retry an approach that already failed, and never attack the same row twice in one run.

Close with the same five fields every time, so consecutive runs are diffable: **row attacked**, **technique**, **verdict**, **what was committed**, and **what is on the tree uncommitted** — naming the file and what the test asserts, or `nothing` when the tree is clean.
