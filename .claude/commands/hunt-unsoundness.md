---
description: Hunt for a proof of False the checkers admit, file it on the soundness board as an open ticket, and land its fix next where the fix follows; an attack that holds is left as a test, and what stops short of False as a finding
argument-hint: "[part — formation, introduction, elimination, conversion, totality, admission — and optionally a rule in it]"
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(rg:*), Bash(cargo:*), Bash(git:*)
---

Find a proof of `False`: a closed term the checkers admit at `/std/Bool/False`.

`$ARGUMENTS` names a part of the board, its index `xboard/src/<part>.rs`, and may name a rule in it by the decision that designs it. With none given, choose a rule and say why. One invocation is one rule; iteration lives outside, under `/loop`, and what one run hands the next is the tree as committed: the board, the findings files and the tests.

## Authority

Invoking this command is standing authorization for four kinds of commit: an open ticket with its witnesses, alone; the fix of an open ticket where the fix follows, with the ticket turned to fixed; the tests of the attacks that held; and what the run files short of `False` in the findings files, as prose. Commit on the branch checked out; never create, switch or rebase one. A ticket already open on the board is spent: it is a proof of `False` waiting for its fix, not a target. Prefer stopping to guessing.

## What a proof of `False` is

A closed term the checkers admit at `/std/Bool/False`, and nothing else. It is shown by a witness, never by a story about a rule being wrong: a source program whose tail is the proof, or, where no program reaches the rule, a module built by hand with the term it closes with, put to the kernel. The witness supplies the term and the board states the type, so a false equation or a declaration wrongly admitted is driven all the way to `False` before it is a ticket. Both are written in the part's board file and put by `cargo xboard report`.

A run that finds none has not shown the rule sound: soundness is a claim about every closed term, and a hunt tries some. It has shown that its attacks failed, and it leaves them where every later change is held to them.

What stops short of `False` is not a ticket: a term a checker admits that its rule should refuse, a program compiled to do what its source does not say, a refusal of something sound, an effect outside `Io` that proves nothing. Each goes where `.claude/rules/findings.md` says a finding goes: shown against its rule, it is a finding; seen and not driven to `False`, it is an entry marked uncertain, naming the `False` it might reach.

A program the elaborator admits and the kernel refuses is refused: a compilation fails closed. The kernel is what is trusted, so a module it admits at `False` is a proof of `False` whatever the elaborator makes of its source; the elaborator admitting a program the kernel refuses is a term a checker admits that its rule should refuse, and a sound program the elaborator refuses is a wrong refusal, which is `/hunt-findings`'s.

## Read first

`xboard/README.md` states what a part, a ticket and a witness are, and `documentation/design/soundness/the-soundness-board.md` the claim, the trusted base, and the boundaries no rule covers. The part's directory holds every proof of `False` already found in it, a file a ticket, each with its witnesses, and a rule's decision — under `documentation/design/soundness/` or `documentation/design/theory/`, or in its crate's README — states what the rule assumes. `curios-cert`'s README and `curios-analysis`'s crate documentation are the roster of what the kernel decides and what both checkers share. Read those there. This file restates none of them.

## Probing

Before there is a ticket to write, `cargo xboard probe <PROGRAM>` asks the board's question of a candidate route to `False`: one program over the standard library whose final term is the proof, put to the checkers by the function a ticket filed from it is put by. The program is read from standard input for `-`, or from a file named in its place:

```sh
cargo xboard probe - <<'CRS'
use /std/{Nat};
use /std/Bool/{True, False, Holds};

let f(n: Nat, h: Holds(n == 2)) -> False =
    match n: (m) => False
    | 0 => h
    | k + 1; ih => ih
    end;

f(2, True/qed())
CRS
```

It prints what each checker made of the program, and exits 0 where it was admitted, 1 where it was refused and 2 where it could not be put. What it shows admitted is what `cargo xboard report` then shows `OPEN`, and the checker and error it names for a refusal are what a fixed ticket expects. A rule no program reaches has no probe: its witness is a module built by hand.

## Attacking a rule

What a rule's decision and the code's own documentation state it relies on are its premises, and a run attacks each with a probe. Its charter is spent once every premise has given a proof of `False` or held. Read each refusal as **A refusal met on the way** of `.claude/commands/hunt-findings.md` reads one: which checker made it, by which error, and whether it is the refusal of the rule attacked; a probe another rule refuses has not attacked this one yet.

Each premise that held leaves a test: the attack that came nearest, in the crate it attacked and placed as `.claude/rules/rust.md` places one, expecting the refusal it met by that checker's error, so a change that reopens the route fails the suite. A run's tests land together, in one commit, under `cargo xtask workspace format`, `cargo xtask workspace lint` and the tests by name.

## Recording a proof of `False`

A proof of `False` is two commits, and the first is the ticket alone.

**The open ticket.** File the ticket as a file of its part's directory named for its title, `xboard/src/<part>/<ticket>.rs` exporting `TICKET`, with its `mod` line and its entry in the part's index; give it `Status::Open`, the day, and its witnesses: each expects a refusal the checkers do not yet make, by the checker that should make it, with the error left unnamed where it does not exist yet. `cargo xboard report` must then show the ticket `OPEN` and its witness admitted, which is the demonstration. Its check is `cargo xtask workspace format --check`, `cargo xtask workspace lint` and `cargo xboard report`. Commit it by itself, changing no checker.

**The fix.** The next commit changes the checker and turns the ticket to `Status::Fixed`, naming in each witness the error that now refuses it. It is this run's where it follows — it brings the checker to the rule its entry states and leaves standing everything else the tree states — and the user's where it sets: where it would pick a shape later code relies on, move a rule a document states, or widen what the kernel admits. Before changing the checker, weigh what the rule's entry and the code's own documentation state it relies on three times over: a fix that closes this hole by breaking what the rule's argument rests on opens another. The board forces the two together: with the proof refused and the ticket still open, the run fails. So the commit that refuses it is the one git blames for its status line, and nothing else records it. Its check is the open ticket's — `cargo xtask workspace format --check`, `cargo xtask workspace lint` and `cargo xboard report` — plus `cargo xtask workspace test`, the whole suite, since what the checkers decide reaches every stage after them, and one judgment none of them can make: the diff is additive or corrective only — no deleted witness, loosened expectation or weakened assertion.

Where the fix sets, or is not correct and targeted, yield: the open ticket is already committed and is the record of what stands. The fix it waits on is a specification, as [Pending work is owned by its specification](../../documentation/design/pending-work-is-owned-by-its-specification.md) states one for a proof of `False`; propose it in the close with its text exactly as it would be written, and do not try a second approach or weaken the witness.

## Under `/loop`

On its own, a run hunts one rule and ends with its report. Under `/loop`, each next run takes a rule this loop has not attacked yet, and the loop ends when a proof of `False` stays open because its fix sets; when a run cannot go on, on a check it cannot make pass; and once every rule has been attacked in it.

## Commits and report

Commit as CLAUDE.md says: the open ticket's message names the proof of `False` found, the fix's the rule now enforced. A commit that lands the tests of a run which filed no ticket — a hunt that reached no `False` — leads its subject with `Soundness hunt: `, so such a run is read off the log; the open ticket and its fix carry no such prefix, told apart from each other by the board file's own history instead. Close with any ticket filed, first; then the part and rule attacked; each premise attacked, by its probe and what refused it; what was committed; what waits on the user, each fix that sets with its alternatives and each specification with its text; and what is on the tree uncommitted.
