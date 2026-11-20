---
description: Hunt for a proof of False the checkers admit, gathering what a count of hunters finds, one charter each, a theme with the rules it crosses — file it on the soundness board as an open ticket, and land its fix next where the fix follows; an attack that holds is left as a test, and every other wrong a hunt meets as a finding
argument-hint: "<count> [directions in prose — a theme, a rule by its decision, a part (formation, introduction, elimination, conversion, totality, admission), a seed, what to look at or leave alone]"
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(rg:*), Bash(cargo:*), Bash(git:*), Bash(df:*), Bash(mkdir:*), Bash(tar:*), Bash(cp:*), Bash(systemd-run:*), Bash(rm -rf .claude/snapshots/:*), Agent, SendMessage
disable-model-invocation: true
---

Find a proof of `False`: a closed term the checkers admit at `/std/Bool/False`, and file every other wrong met on the way as a finding.

A hunt has two roles. **The gatherer** is the conversation this command was invoked in: it cuts the charters, spawns one hunter a charter, shows again what each sends, and alone files and commits. **A hunter** is a `hunter` agent, `.claude/agents/hunter.md`, which attacks one charter over a snapshot of the tree and sends what it finds. Part I is both roles', Part II the hunter's and Part III the gatherer's; a hunter reads Parts I and II, and the gatherer reads all three.

`$ARGUMENTS` is a count of hunters, 1 or more, then directions in prose — a theme, a rule by the decision that designs it, a part of the board by its directory `xboard/tickets/<part>/`, a seed, what to look at or leave alone. Without that count, ask for it and start nothing. Prefer stopping to guessing.

# Part I — What is sought

## A proof of `False`

A closed term the checkers admit at `/std/Bool/False`, and nothing else. It is shown by a witness, never by a story about a rule being wrong, and `xboard/README.md` states the forms a witness takes. The witness supplies the term and the board states the type, so a false equation or a declaration wrongly admitted is driven all the way to `False` before it is a ticket.

A program the elaborator admits and the certifier refuses is refused: a compilation fails closed. The certifier is what is trusted, so a module it admits at `False` is a proof of `False` whatever the elaborator makes of its source.

A run that finds none has not shown the rule sound: soundness is a claim about every closed term, and a hunt tries some. It has shown that its attacks failed, and it leaves them where every later change is held to them, as tests.

A ticket already open on the board is spent: it is a proof of `False` waiting for its fix, not a target.

## A finding

Whatever is wrong in the tree and not yet written down, wherever a hunt meets it: shown by a probe, a test or a rule set against its code, never by a story about one. A proof of `False` is the gravest, and a ticket; everything else a hunt meets is a finding, whatever its charter:

- a term a checker admits that its rule should refuse — the elaborator admitting a program the certifier refuses among them — and a sound program a checker refuses;
- a rule that refuses what its theory allows, whose lifting is the finding;
- a program compiled to do what its source does not say, and an effect outside `Io`;
- a diagnostic that misnames its fault, its place or what its reader can write;
- a claim the tree does not keep: a sentence of `documentation/language.md` or `documentation/usage.md`, a decision's rule, a doc comment's contract;
- two answers that must agree and do not: the elaborator and the certifier over one term, conversion and the compiled program over one closed computation, a unit compiled over a baseline and the same unit compiled whole, `wonder` and `run` over one refusal;
- a verdict that moves with a spelling the theory judges alike: a call direct and through an instance, a term under an arm and outside it, bound by a `let` and inlined;
- a check waived, an `#[allow]` or an `#[expect]`, whose reason has lapsed;
- and what falls short of what the tree states it does.

Shown against its rule, it is a finding; seen and not shown, it is an entry marked uncertain, saying what would settle it, and one met on the way to `False` names the `False` it might reach. It is filed where `.claude/rules/findings.md` says a finding goes.

## Read first

`xboard/README.md` states what a part, a ticket and a witness are, and `documentation/design/soundness/the-soundness-board.md` the claim, the trusted base, and the boundaries no rule covers. Each part's directory holds every proof of `False` already found in it, a file a ticket, each with its witnesses, and a rule's decision — under `documentation/design/soundness/` or `documentation/design/theory/`, or in its crate's README — states what the rule assumes. What was found short of `False`, and the specifications a fix waits on, are under `documentation/roadmap/`, each area's findings file loading `.claude/rules/findings.md` as it is read. `curios-cert`'s README and `curios-analysis`'s crate documentation are the roster of what the certifier decides and what both checkers share. Read those there. This file restates none of them.

# Part II — Hunting a charter

This part is the hunter's: "you" is the hunter. How a unit is sent, and what a hunter may touch, is `.claude/agents/hunter.md`'s.

## The charter

A charter is a theme and its specificities. The theme is one sentence naming a way the checkers could go wrong — a fact settled in one scope answering in another, a call or an occurrence reached through an indirection, a proof handled as data — and the specificities are the rules it crosses, each by the decision that designs it, and within each the premises the theme bears on: what that decision and the code's own documentation state the rule relies on. A route to `False` rarely stays inside one rule, and the board's own tickets share causes across parts, so a theme is what a charter is cut by and a part is only where a ticket is filed. A rule named alone is a charter whose theme is that rule and whose specificities are all its premises; a part named alone is where its themes are looked for.

Attack each premise named through the theme, with a probe. The charter is spent once every premise named has given a proof of `False` or held. A route to `False` met on the way is chased whatever rule it reaches, to `False` or to the refusal that stops it: any route to `False` is one, so a charter bounds where you start and never which route you follow, and a route that held leaves its test as a premise does. A finding met on the way is sent as one, whatever its charter.

## Where a route comes from

An attack on a premise starts from a seed, a theory a probe can ask where a premise alone states none:

- **A flaw already fixed.** Every ticket of every part, not only the part attacked, read with the commit that turned it fixed. The theory is that the cause stands where the fix did not reach: the same mistake over another carrier, the same rule reached by another route, through an instance where a direct call was fixed.
- **A paradox already known.** The routes the field has, each by the features it needs, under the part it attacks. Formation's universes: type-in-type, two impredicative layers or an impredicative pair, which is Hurkens' route. Formation's positivity: an inductive not strictly positive, or a negative one, under impredicativity. Elimination: large elimination out of an impredicative inductive, or Berardi's route through excluded middle. Totality: impredicative ordinals, relaxed guardedness of cofixpoints, and the sized types Agda's `false` label fills up with. Conversion: the machines, where Rocq's register of critical bugs is thickest. Admission: constraints lost through a module or a substitution, and input trusted as it is deserialized. Each is a theory of where this tree's rule would admit the same construction, read against what its decision says refuses it ([the catalogue](https://ionathan.ch/2021/11/24/inconsistencies.html), [coq-paradoxes](https://arxiv.org/abs/2605.27633), [Rocq's register](https://raw.githubusercontent.com/rocq-prover/rocq/master/dev/doc/critical-bugs.md)).
- **What landed last** in the checker crates — `curios-elab`, `curios-cert` and the layer both judge through — each change read whole.
- **Two checkers that must agree.** The board's probe prints what each made of a program, and a program one admits and the other refuses is a route into whichever is wrong.
- **A check waived.** Every `#[allow]` and `#[expect]` in the checker crates and the layer both judge through is a place where a compiler's own check was argued away by hand, and the reason it states is a premise.
- **A rule no program reaches.** Its witness is a module built by hand, written as Core text, as a ticket's `.crsc` witness is. Write it as a `.crsc` file in your directory of the snapshot and probe it there. A route through an object one walk filed and a later walk read — a unit offered as a baseline, a unit mounted from the cabinet — is a history, a `metadata.toml` beside the texts of its steps, as `xboard/README.md` states one. An input no program and no history writes — an archive, a stored verdict forged — is attacked by the test that builds it and hands it to the path that reads it, written whole and sent for the gatherer to run; that the board's own witness paths never reach it is no reason to stop.

## Probing

The snapshot's board, `<snapshot>/binaries/xboard probe <file>`, asks the board's question of a candidate on no ticket, as `xboard/README.md` states it, and a route to `False` is tried there before anything is sent. A candidate is a file written in your directory of the snapshot: a program, `.crs`; Core text, `.crsc`; or a ticket, a `metadata.toml` beside the texts its witnesses name, a witness among them a history. A program written there as `f.crs`,

```crs
use /std/{Nat};
use /std/Bool/{True, False, Holds};

let f(n: Nat, h: Holds(n == 2)) -> False =
    match n | 0 => h | k + 1 => f(k + 1, h) end;

f(2, True/qed())
```

is put by `<snapshot>/binaries/xboard probe <snapshot>/hunters/hunter-<n>/f.crs`. What it shows admitted is what `cargo xboard report` then shows `OPEN`, and the checker and error it names for a refusal are what a fixed ticket expects. The snapshot's compiler, `<snapshot>/binaries/curios`, answers `run -` and `wonder -` on standard input as CLAUDE.md probes.

## A refusal met on the way

When a probe you wrote is refused, do not rewrite it until it compiles. First read which checker made the refusal, by which error, and whether it is the refusal of the premise attacked: a probe another rule refuses has not attacked this one yet. Then decide which of three things the refusal is, and say which:

- the theory forbids the program — the refusal is right and the probe was wrong;
- the theory allows it and the rule over-approximates — lifting the rule is a finding;
- the refusal is right but the diagnostic misnames the fault, its span, or what the reader needs — the diagnostic is the finding.

"The elaborator was just being conservative" is the comfortable reading, not the demonstrated one: read the refusing rule and the syntax reference against each other before choosing. Lifting a rule argued under `documentation/design/soundness/` is a finding only with that rule named.

## An attack that held

Each premise that held leaves a test: the attack that came nearest, written whole in your directory as the test it would land as, in the crate [the soundness board](../../documentation/design/soundness/the-soundness-board.md) places a held attack in and as `.claude/rules/rust.md` places one, expecting the refusal it met as a pattern over that checker's error, so a change that reopens the route fails the suite. The attack is the run's own, so a test already in the tree stands for it only where it states that attack's shape, and the bundle names it; a rule's existing tests are no reason to land none. A route argued and not run has not been attacked: a construction the elaborator would refuse first is a module built by hand and put to the certifier.

## What you send

Three kinds of unit, each the moment it is shown:

- **A candidate for `False`**, `false: <its title>`, carries what the board puts: the files it was probed as, each whole — a program, Core text, or a ticket's `metadata.toml` and the texts beside it — with the probe's command and its output showing it admitted and exiting 0; with the part it is filed under, the rules and the premise it went through, which checker admitted it, and a title in the sentence style of the board's tickets. A program the probe refused is no candidate, whatever stands behind it.
- **A finding**, `finding: <the finding>`, carries its entry as [Pending work is owned by its specification](../../documentation/design/pending-work-is-owned-by-its-specification.md) forms one — the finding in bold, where, what is wrong, the fix, its check and its size in commits — with its kind under `.claude/rules/findings.md`, the findings file it belongs to, and, where it meets an entry or a specification, which one and what it adds. One seen and not yet shown says so, with what would settle it, and is filed uncertain.
- **A held bundle**, `held: Soundness hunt: <the theme, in a sentence>`, sent once every premise the charter names has given a proof of `False` or held, carries each premise that held, by the rule it is a premise of, and each route chased past the charter that held, with the attack that came nearest, the checker that refused it and the error the probe named; the test of each, with a sentence for its name, written whole as **An attack that held** writes one; any mutation check a test's comment claims, named for the gatherer to make; and the checks by name.

# Part III — Gathering

This part is the gatherer's: "you" is the gatherer, and every commit of the run is yours.

## Authority

Invoking this command is standing authorization for five kinds of commit, each made in the main checkout on the branch checked out — never create, switch or rebase one:

- an open ticket with its witnesses, alone;
- the fix of an open ticket where the fix follows, with the ticket turned to fixed;
- the tests of the attacks that held, one commit a charter, under `cargo xtask format`, `cargo xtask workspace lint` and the tests by name;
- a finding filed as prose where its write follows — shown again by what the tree runs or states;
- a fix met on the way that changes documentation alone — a comment, a doc comment, a `.md` file — by itself, made on the spot and never filed.

A write **sets** where it decides what work is held to — a new specification, a finding against stated reasoning, a fix that sets as **Recording a proof of `False`** says — and it waits for the user, held with its text exactly as it would be written.

## 1. The charters

Cut the directions into one charter a hunter, each a theme with its specificities as **The charter** of Part II states one: the charter the directions name goes to the first hunter, and the rest take premises this invocation has not given out, near what the directions name or, with none, where you choose. No premise is named by two charters, so a route two hunters meet is a sibling's by charter and not by chance. Where the directions leave a charter open, choose. Say the charters, and why.

## 2. The snapshot

A hunt reads a snapshot, `.claude/snapshots/<commit>/`, which the checkout ignores: `tree/` holds the tree at the hunt's commit, `binaries/` the `curios` and `xboard` binaries built once from it in the main checkout, and `hunters/hunter-<n>/` each hunter's directory, the one place it writes. Every hunter shares the tree and the binaries and builds nothing, so the main checkout moves on under your commits while a hunter's answers stay those of the commit it hunts. A worktree under `.claude/worktrees` is one the user asks for, and no hunt's.

Read `df`. With the tree clean, `git status --short` printing nothing, the commit is `git rev-parse --short HEAD`: build the binaries at it, `cargo xtask compiler build` and `cargo build --release --package xboard`, then make the snapshot, `mkdir -p .claude/snapshots/<commit>/tree .claude/snapshots/<commit>/binaries`, `git archive <commit> | tar -x -C .claude/snapshots/<commit>/tree`, `cp target/release/curios target/release/xboard .claude/snapshots/<commit>/binaries/`, and `mkdir -p .claude/snapshots/<commit>/hunters/hunter-<n>` for each hunter.

## 3. Spawning

Spawn the count of `hunter` agents in one message, naming `model` on each call, since a definition's own field is not always applied mid-session. Each prompt carries this file's path, its charter, the snapshot's path and its directory there, its siblings' charters and the commit of the tree, and no suspicion of yours: a hunter handed a hypothesis returns it confirmed, so one you hold goes as a question to refute. From then on hunt nothing yourself; your context is for what comes back. This is one gatherer, and no workflow or team: the compiler answers every unit with one command, so a second judge adds a mechanism where the oracle stands, and a single writer is what keeps one entry a cause.

## 4. On a unit

Nothing a unit carries is filed on the hunter's word. Show it again by the command it carries, in the main checkout, and read it against every entry, specification, ticket and **Rejected** paragraph that names the same code or rule: the hunter read them, and you are the one closing the cause. Then, by its kind:

- **A candidate for `False`** is probed with the main checkout's board, `cargo xboard probe <file>`. One whose probe exits 0 is a ticket filed open, alone, ahead of everything else waiting, as **Recording a proof of `False`** records one, and every hunter whose charter the ticket names is told: its snapshot still holds the commit the fix changes.
- **A finding** is filed where it follows, or held where it sets, as **Authority** draws the line.
- **A held bundle** lands its tests in one commit, once they run in the main checkout, with each mutation check its tests' comments claim made first.

Or send it back with what it lacks, or answer it as already held, naming what holds it, and write nothing. Each write lands as its cause closes, since no hunter builds; its checks wait for the sweep of step 6. Where a cause needs its extent swept or its probe narrowed, send that to the hunter that found it, by `SendMessage`: it has the context, and it resumes. Otherwise answer a hunter only where a unit of its meets something filed or sent back, or where a ticket filed names its charter: what one hunter hands another is the tree as committed, and nothing else.

## Recording a proof of `False`

A proof of `False` is two commits, and the first is the ticket alone.

**The open ticket.** File the ticket under the part whose question its flaw answered wrongly, as `xboard/README.md` files one, whatever charter found it: a directory of that part's named for the day it was found and then its title, `xboard/tickets/<part>/<YYYY_MM_DD>_<ticket>/`, holding its `metadata.toml` and each witness's text; give it `status = "open"` and its witnesses: each expects a refusal the checkers do not yet make, by the checker that should make it, with the error left unnamed where it does not exist yet. `cargo xboard report` must then show the ticket `OPEN` and its witness admitted, which is the demonstration. Its check is `cargo xtask workspace lint`, which holds the formatting too, and `cargo xboard report`. Commit it by itself, changing no checker.

**The fix.** The next commit changes the checker and turns the ticket to `status = "fixed"`, naming in each witness the error that now refuses it, made as soon as it follows. It is this run's where it follows — it brings the checker to the rule its entry states and leaves standing everything else the tree states — and the user's where it sets: where it would pick a shape later code relies on, move a rule a document states, or widen what the certifier admits. Before changing the checker, weigh the rule's premises three times over: a fix that closes this hole by breaking what the rule's argument rests on opens another. The board forces the two together: with the proof refused and the ticket still open, the run fails. So the commit that refuses it is the one git blames for its status line, and nothing else records it. Its check is the open ticket's, plus `cargo xtask workspace test`, the whole suite, since what the checkers decide reaches every stage after them. One judgment no check makes: the diff is additive or corrective only — no deleted witness, loosened expectation or weakened assertion.

Where the fix sets, or is not correct and targeted, yield: the open ticket is already committed and is the record of what stands. The fix it waits on is a specification, as [Pending work is owned by its specification](../../documentation/design/pending-work-is-owned-by-its-specification.md) states one for a proof of `False`; propose it in the close with its text exactly as it would be written, and do not try a second approach or weaken the witness.

## 5. On a report

A hunter's final report comes whole when it finishes; keep its let-go lines and what it did not reach for the close. The report is a pause: a message resumes the hunter with its context, so a hunt is ended by asking for its report, and `TaskStop`, which no message can undo, is for a hunter that does not answer. A report marked partial, the turn budget spent, is read like any other, and the hunter is resumed where its charter is not spent.

## 6. After the last report

Once every unit a hunter sent has been shown again, run once, over every commit the run made, what their checks name — `cargo xtask format`, `cargo xtask workspace lint`, each test by name, and `cargo xtask workspace test` where a fix landed — and fix what fails in a commit of its own. Then remove the snapshot, `rm -rf .claude/snapshots/<commit>`: a hunter's directory is scratch the gatherer owns, so what no unit carried goes with it.

## 7. Commits and the report

Commit as CLAUDE.md says: the open ticket's message names the proof of `False` found, the fix's the rule now enforced. A commit that lands the tests of a run which filed no ticket — a hunt that reached no `False` — leads its subject with `Soundness hunt: `, so such a run is read off the log; the open ticket and its fix carry no such prefix, told apart from each other by the board file's own history instead.

Close with any ticket filed, first; then each charter attacked, its theme and the rules and premises it named, with its hunter; each premise attacked, by its probe and what refused it; each suspicion let go, by its line, and what of each charter was not reached, so a quiet charter is told from a shallow hunt; what was committed; what waits on the user — each fix that sets with its alternatives, each finding that sets and each specification, with its text exactly as it would be written — put in one round of structured questions; and what is on the tree uncommitted.

## Under `/loop`

On its own, an invocation hunts its count of charters and ends with its report. Under `/loop`, each next invocation takes as many charters, naming premises this loop has not attacked yet, and what one invocation hands the next is the tree as committed: the board, the findings files and the tests. The loop ends when a proof of `False` stays open because its fix sets; when a run cannot go on, on a check it cannot make pass; and once every rule's premises have been attacked in it.
