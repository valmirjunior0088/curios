---
description: Hunt for a proof of False the checkers admit — alone, or gathering what a count of hunters finds, one charter each, a theme with the rules it crosses — file it on the soundness board as an open ticket, and land its fix next where the fix follows; an attack that holds is left as a test, and what stops short of False as a finding
argument-hint: "[count] [directions in prose — a theme, a rule by its decision, a part (formation, introduction, elimination, conversion, totality, admission), a seed, what to look at or leave alone]"
allowed-tools: Read, Edit, Write, Grep, Glob, Bash(rg:*), Bash(cargo:*), Bash(git:*), Agent, SendMessage
---

Find a proof of `False`: a closed term the checkers admit at `/std/Bool/False`.

After an optional count, `$ARGUMENTS` is directions in prose — a theme, a rule by the decision that designs it, a part of the board by its index `xboard/src/<part>.rs`, a seed, what to look at or leave alone — and the charters follow them. A first word that is a number is a count of hunters and no part of the directions: with none, or `0`, you attack one charter yourself; with one or more you gather as **Gathering** says, and each hunter attacks one charter, cut as **Attacking a charter** says — the charter the directions name goes to the first, and the rest take premises this invocation has not given out, near what they name or, with no directions, where you choose — so the count is a count of charters. Where the directions leave a charter open, choose and say why. A ticket already open on the board is spent: it is a proof of `False` waiting for its fix, not a target. One run is one charter, whoever runs it; iteration lives outside, under `/loop`, and what one run hands the next is the tree as committed: the board, the findings files and the tests. Prefer stopping to guessing.

## What a proof of `False` is

A closed term the checkers admit at `/std/Bool/False`, and nothing else. It is shown by a witness, never by a story about a rule being wrong, and `xboard/README.md` states the two forms a witness takes. The witness supplies the term and the board states the type, so a false equation or a declaration wrongly admitted is driven all the way to `False` before it is a ticket.

A run that finds none has not shown the rule sound: soundness is a claim about every closed term, and a hunt tries some. It has shown that its attacks failed, and it leaves them where every later change is held to them.

What stops short of `False` is not a ticket: a term a checker admits that its rule should refuse, a program compiled to do what its source does not say, a refusal of something sound, an effect outside `Io` that proves nothing. Each goes where `.claude/rules/findings.md` says a finding goes: shown against its rule, it is a finding; seen and not driven to `False`, it is an entry marked uncertain, naming the `False` it might reach.

A program the elaborator admits and the kernel refuses is refused: a compilation fails closed. The kernel is what is trusted, so a module it admits at `False` is a proof of `False` whatever the elaborator makes of its source; the elaborator admitting a program the kernel refuses is a term a checker admits that its rule should refuse, and a sound program the elaborator refuses is a wrong refusal, which is `/hunt-findings`'s.

## Read first

`xboard/README.md` states what a part, a ticket and a witness are, and `documentation/design/soundness/the-soundness-board.md` the claim, the trusted base, and the boundaries no rule covers. Each part's directory holds every proof of `False` already found in it, a file a ticket, each with its witnesses, and a rule's decision — under `documentation/design/soundness/` or `documentation/design/theory/`, or in its crate's README — states what the rule assumes. What was found short of `False`, and the specifications a fix waits on, are under `documentation/roadmap/soundness/`, whose findings file loads `.claude/rules/findings.md` as it is read. `curios-cert`'s README and `curios-analysis`'s crate documentation are the roster of what the kernel decides and what both checkers share. Read those there. This file restates none of them.

## Where a route comes from

An attack on a premise starts from a seed, as **Where a suspicion comes from** of `.claude/commands/hunt-findings.md` takes one, read for the board:

- **A flaw already fixed.** Every ticket of every part, not only the part attacked, read with the commit that turned it fixed. The theory is that the cause stands where the fix did not reach: the same mistake over another carrier, the same rule reached by another route, through an instance where a direct call was fixed.
- **A paradox already known.** The routes the field has, each by the features it needs, under the part it attacks. Formation's universes: type-in-type, two impredicative layers or an impredicative pair, which is Hurkens' route. Formation's positivity: an inductive not strictly positive, or a negative one, under impredicativity. Elimination: large elimination out of an impredicative inductive, or Berardi's route through excluded middle. Totality: impredicative ordinals, relaxed guardedness of cofixpoints, and the sized types Agda's `false` label fills up with. Conversion: the machines, where Rocq's register of critical bugs is thickest. Admission: constraints lost through a module or a substitution, and input trusted as it is deserialized. Each is a theory of where this tree's rule would admit the same construction, read against what its decision says refuses it ([the catalogue](https://ionathan.ch/2021/11/24/inconsistencies.html), [coq-paradoxes](https://arxiv.org/abs/2605.27633), [Rocq's register](https://raw.githubusercontent.com/rocq-prover/rocq/master/dev/doc/critical-bugs.md)).
- **What landed last** in the checker crates — `curios-elab`, `curios-cert` and the layer both judge through — each change read whole.
- **Two checkers that must agree.** `cargo xboard probe` prints what each made of a program, and a program one admits and the other refuses is a route into whichever is wrong.
- **A check waived.** Every `#[allow]` and `#[expect]` in the checker crates and the layer both judge through is a place where a compiler's own check was argued away by hand, and the reason it states is a premise.
- **A rule no program reaches.** It has no probe; its witness is a module built by hand, the `fn() -> (Module, Term)` a ticket's `Proof::Module` names, written as the tickets of its part write theirs. A hunter writes it as a ticket file in its worktree and puts it there, `(cd <worktree> && cargo xboard report)`. An input no program writes — an archive, a stored verdict, a store's entry — is forged the same way, by a test in the worktree that builds it and hands it to the path that reads it; that the board's own witness paths never reach it is no reason to stop.

## Probing

`cargo xboard probe -` asks the board's question of a program on no ticket, as `xboard/README.md` states it, and a route to `False` is tried there before anything is written:

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

What it shows admitted is what `cargo xboard report` then shows `OPEN`, and the checker and error it names for a refusal are what a fixed ticket expects. A hunter asks it of its worktree's board, the program inside the subshell that enters it: `(cd <worktree> && cargo xboard probe - <<'CRS'`, the program, `CRS`, then `)`.

## Attacking a charter

A charter is a theme and its specificities. The theme is one sentence naming a way the checkers could go wrong — a fact settled in one scope answering in another, a call or an occurrence reached through an indirection, a proof handled as data — and the specificities are the rules it crosses, each by the decision that designs it, and within each the premises the theme bears on: what that decision and the code's own documentation state the rule relies on. A route to `False` rarely stays inside one rule, and the board's own tickets share causes across parts, so a theme is what a charter is cut by and a part is only where a ticket is filed. A rule named alone is a charter whose theme is that rule and whose specificities are all its premises; a part named alone is where its themes are looked for. A run attacks each premise named through the theme, with a probe, and its charter is spent once every premise named has given a proof of `False` or held. A route to `False` met on the way is chased whatever rule it reaches, to `False` or to the refusal that stops it: any route to `False` is one, so a charter bounds where a run starts and never which route it follows, and a route that held leaves its test as a premise does. Read each refusal as **A refusal met on the way** of `.claude/commands/hunt-findings.md` reads one: which checker made it, by which error, and whether it is the refusal of the premise attacked; a probe another rule refuses has not attacked this one yet.

Each premise that held leaves a test: the attack that came nearest, in the crate it attacked and placed as `.claude/rules/rust.md` places one, expecting the refusal it met by that checker's error, so a change that reopens the route fails the suite. A run's tests land together, in one commit, under `cargo xtask workspace format`, `cargo xtask workspace lint` and the tests by name.

## Hunting for a gatherer

Up to here this file is the hunter's as much as the gatherer's; from **Authority** on it is the gatherer's, and no commit is yours. How a unit is sent, and what every one carries, is `.claude/agents/hunter.md`'s. Under this command you send three kinds, and the first two are owned here:

- **A candidate for `False`**, `false: <its title>`, carries what the board puts: a program as `cargo xboard probe -` takes it, with the probe's output showing it admitted and exiting 0, or the module built by hand **Where a route comes from** names, as the ticket file it was put as in your worktree, with that report's output showing it `OPEN`; with the part it is filed under, the rules and the premise it went through, which checker admitted it, and a title in the sentence style of the board's tickets. A program the probe refused is no candidate, whatever stands behind it.
- **A held bundle**, `held: Soundness hunt: <the theme, in a sentence>`, is sent once every premise the charter names has given a proof of `False` or held, and carries each premise that held, by the rule it is a premise of, and each route chased past the charter that held, with the attack that came nearest, the checker that refused it and the error the probe named; the test of each, with a sentence for its name, written and placed as `.claude/rules/rust.md` says and run in your worktree, with what the run printed, and any mutation check a test's comment claims run there too; and the checks by name.
- **A finding**, what stops short of `False`, as **Hunting for a gatherer** of `.claude/commands/hunt-findings.md` sends one.

## Authority

Invoking this command is standing authorization for five kinds of commit: an open ticket with its witnesses, alone; the fix of an open ticket where the fix follows, with the ticket turned to fixed; the tests of the attacks that held; what the run files short of `False` in the findings files, as prose; and a fix met on the way that changes documentation alone — a comment, a doc comment, a `.md` file — by itself, made on the spot and never filed. Commit on the branch checked out; never create, switch or rebase one.

## Recording a proof of `False`

A proof of `False` is two commits, and the first is the ticket alone.

**The open ticket.** File the ticket under the part whose question its flaw answered wrongly, as `xboard/README.md` files one, whatever charter found it: a file of that part's directory named for its title, `xboard/src/<part>/<ticket>.rs` exporting `TICKET`, with its `mod` line and its entry in the part's index; give it `Status::Open`, the day, and its witnesses: each expects a refusal the checkers do not yet make, by the checker that should make it, with the error left unnamed where it does not exist yet. `cargo xboard report` must then show the ticket `OPEN` and its witness admitted, which is the demonstration. Its check is `cargo xtask workspace format --check`, `cargo xtask workspace lint` and `cargo xboard report`. Commit it by itself, changing no checker.

**The fix.** The next commit changes the checker and turns the ticket to `Status::Fixed`, naming in each witness the error that now refuses it. It is this run's where it follows — it brings the checker to the rule its entry states and leaves standing everything else the tree states — and the user's where it sets: where it would pick a shape later code relies on, move a rule a document states, or widen what the kernel admits. Before changing the checker, weigh the rule's premises three times over: a fix that closes this hole by breaking what the rule's argument rests on opens another. The board forces the two together: with the proof refused and the ticket still open, the run fails. So the commit that refuses it is the one git blames for its status line, and nothing else records it. Its check is the open ticket's, plus `cargo xtask workspace test`, the whole suite, since what the checkers decide reaches every stage after them. One judgment no check makes: the diff is additive or corrective only — no deleted witness, loosened expectation or weakened assertion.

Where the fix sets, or is not correct and targeted, yield: the open ticket is already committed and is the record of what stands. The fix it waits on is a specification, as [Pending work is owned by its specification](../../documentation/design/pending-work-is-owned-by-its-specification.md) states one for a proof of `False`; propose it in the close with its text exactly as it would be written, and do not try a second approach or weaken the witness.

## Gathering

Gather as **Gathering** of `.claude/commands/hunt-findings.md` gathers, each hunter's charter a theme with its specificities, as **Attacking a charter** cuts one, and no premise named by two charters, so a route two hunters meet is a sibling's by charter and not by chance; each prompt names this file. Each hunter hunts in a worktree of its own, kept as **Worktrees** of `.claude/commands/hunt-findings.md` says, so nothing the gatherer lands in the main checkout moves under it. Three things are this command's. A candidate for `False` is shown again by the probe it carries, and one built by hand by filing its ticket file in the main checkout, as **Recording a proof of `False`** files one. A held bundle is one charter's attacks that held, and lands as **Attacking a charter** lands a run's tests, one commit a charter, once it is shown again. The fix of a ticket, where it follows, is made as **Recording a proof of `False`** makes one as soon as it follows, its check run with every other commit's in the sweep **Gathering** of `.claude/commands/hunt-findings.md` makes after the last report, and every hunter whose charter the ticket names is told: its worktree still holds the commit the fix changed.

## Under `/loop`

On its own, a run hunts one charter and ends with its report. Under `/loop`, each next run takes a charter naming premises this loop has not attacked yet — the count of them at once, where there is a count — and the loop ends when a proof of `False` stays open because its fix sets; when a run cannot go on, on a check it cannot make pass; and once every rule's premises have been attacked in it.

## Commits and report

Commit as CLAUDE.md says: the open ticket's message names the proof of `False` found, the fix's the rule now enforced. A commit that lands the tests of a run which filed no ticket — a hunt that reached no `False` — leads its subject with `Soundness hunt: `, so such a run is read off the log; the open ticket and its fix carry no such prefix, told apart from each other by the board file's own history instead. Close with any ticket filed, first; then each charter attacked, its theme and the rules and premises it named, with its hunter where there were hunters; each premise attacked, by its probe and what refused it; what was committed; what waits on the user, each fix that sets with its alternatives and each specification with its text; what is on the tree uncommitted; and, where there were hunters, their worktrees, put to the user as **Worktrees** of `.claude/commands/hunt-findings.md` says.
