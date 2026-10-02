# xboard

The soundness board: every flaw found in the Curios checkers, kept as something that is run. `cargo xboard` puts the board to the checkers and prints the report. The checkers themselves are `curios-elab`'s and `curios-cert`'s; this crate judges through `curios-pipeline` and the kernel, and nothing depends on it.

## Design

### A flaw is a ticket that is run

**Decision.** The board is the binary's, `src/board/`, one module per part of the judgment, and the library is what it is written in and run by: a part owns the tickets for the flaws found in it. A ticket states the day it was found and whether it is open or fixed. It carries witnesses, and `cargo xboard` puts every one to the checkers: a fixed flaw whose witness is no longer refused as it expects has regressed, an open flaw whose witnesses are all refused was shut without the ticket saying so, and either fails the run. The report then says of each part what was found in it.

**Rationale.** A flaw in the kernel of a proof assistant is not fixed and forgotten: every later change is held against every flaw already found, and the list is one a person can read and a machine runs. The fields are Rust values, so a date that does not exist does not compile, and where a ticket sits says which part it was found in, so that relation is stated once.

**Rejected.** Keeping on the board the attacks that found nothing: each is a test, it runs with the suite of the crate it tests, and beside the flaws it would outnumber them. A prose record of each flaw, which nothing runs: its witness is the text it had the day it was written, and the surface language has moved since. A status written by hand, which says what someone believed. The commit that closed a flaw, named on its ticket: that a commit exists says nothing of what it closed, a fix cannot name itself, and git already answers the question, since an open ticket whose witnesses are refused fails the run, so the commit that closes a flaw is the one that turns its ticket to fixed. A document per rule grading its evidence in words, which is what the report computes.

### The board is filed by part of the judgment

**Decision.** The board has six parts, the ones `documentation/design/soundness/` files a rule's argument under: formation, introduction, elimination, conversion, totality and admission. The first three are the kinds of rule every type former has — forming the type, building a value of it, taking one apart — so Π, Σ, a declared family and a record are each covered by all three. A part is a question, stated at the head of its file, and its tickets: a ticket is filed under the part whose question its flaw answered wrongly. A part claims nothing beyond its question, and it is split when it holds enough tickets about one rule to name the rule by what they show.

**Rationale.** A ticket needs a place and a hunt needs a target, and a part gives both without asserting anything no ticket has tested. The rules themselves are argued one entry each under those directories, so the board repeats none of them.

**Rejected.** One module per rule, each named by the sentence it must satisfy, written before any ticket: a sentence nothing has tested says only that its rule is sound, which is true of every rule and tells none apart. Parts named for subsystems, typing, universes and inductive types among them: "typing" names every rule at once, a universe is what a formation rule assigns, and a part for inductive types leaves Π and Σ without one. Substitution or binding as the name of the part a value is built in: each is the means, and the part is what they are the means of. No part at all, a rule being created by the ticket that shows it: the first ticket still needs a place, and whoever files it, an unattended hunt among them, would invent the sentence.

### A ticket is a proof of `False`, and no witness states the type

**Decision.** A flaw is on the board only as a closed term the checkers admitted at `/std/Bool/False`. A witness supplies the term — a program's tail, or the term a module built by hand closes with — and the type it is put at is fixed beneath it: the pipeline judges a program put as a proof at `False`, the one type besides `Io({})` an entry can be judged at, and the board states the same name for a module. Nothing a witness says of itself is read. A flaw found as a false equation or as a declaration wrongly admitted is driven to `False` before it is filed.

**Rationale.** A logic is unsound exactly where it proves a falsehood, and every such proof is one step from `False`, so `False` is the one standard that needs no judgment of what a rule ought to refuse. The question a run asks is the same while a ticket is open and once it is fixed — is this term admitted at `False` — so the witness seen admitted and the witness held refused are one. It is the line Lean's `soundness` tag and Agda's `false` label draw.

**Rejected.** A second kind of ticket, for a checker admitting what its rule should refuse with no `False` built from it: "should" is measured against prose of our own, so it is a finding, in `documentation/roadmap/<area>/00-findings.md`, until someone builds the `False`. A witness naming the definition that proves `False`, for the board to look up: the board would read what the witness chose to show it, and once the flaw is fixed no judged program is left to read. A control beside a witness, a sound program expected admitted: it guards against refusing too much, which is incompleteness, and is a test of the crate that refuses. Reading erasure's verdict: a term the kernel admitted at `False` is unsound whatever is built from it afterwards, and a program compiled to do what its source does not say is no proof of anything.

### A ticket enters the board open

**Decision.** A flaw is filed while it stands: the ticket is committed open, by itself, with its witness admitted, and the run shows it `OPEN`. The commit that closes the flaw turns the ticket to fixed, and the board forces the two together, since an open ticket whose witnesses are all refused fails the run. The board began with no ticket.

**Rationale.** A witness seen admitted and then refused is the only one known to be able to fail. A ticket filed after its fix says that a program is refused today and nothing of whether the program ever showed the flaw, or whether what refuses it is what closed it.

**Rejected.** Filing the flaws fixed before the board existed, each with the test that guarded it: none was ever seen admitted by a run of the board, several are refused today by a rule that replaced the one the flaw was in, and each test still runs in the crate it tests.

### A witness is put here, and answers whether it was admitted

**Decision.** A witness is put one of two ways. A source program goes to the compiler through both checkers as a compilation takes it: the elaborator, then the kernel over the module it built. A module built by hand goes to the kernel alone, with the prelude in scope, as a program closing with its term, through the walk a compilation puts a program to. Each answers one thing, admitted or refused, and a refusal is the checker's own error, kept as it was raised; a witness states what refuses it once its flaw is closed, by the error each checker it lists raises, written as a pattern over that error.

**Rationale.** Every witness runs in the one process that prints the report, so a ticket never reads as holding on the strength of a test another suite may or may not have run. Both go through what the checkers export, so a witness exercises a checker as a whole and needs nothing private to it. The named error is what keeps a witness that has rotted — one that no longer parses, or fails on an unbound name — from passing by being refused for another reason, and a pattern over the error is compiled, so an error that does not exist is not expected. A program both checkers refuse lists both, so the kernel's refusal is not taken on the elaborator's.

**Rejected.** Naming another crate's test by its spelling and running it through that crate's test binary: the name is a string nothing compiles. A scenario staged inside the crate that owns a rule and exported for the board: it calls one function of the checker rather than the checker, and the one tried called the half of a flaw that had been right all along. A grade for a checker that was not asked: the question is whether the witness was admitted, and which checker refused is part of the answer, not a third verdict. Matching a fragment of the printed diagnostic: it is a string nothing compiles, any refusal that happens to contain it passes, and it reads a message where the error is at hand. Expecting a refusal by the program's text, before either checker: a fix that lands in the parser leaves the kernel admitting the term.

### The report says which tickets never reach the kernel

**Decision.** A program witness holds the elaborator and a module witness holds the kernel; a flaw both had carries one of each. The report lists each ticket the kernel judged no witness of, and counts them. It does not fail on one.

**Rationale.** The elaborator refuses most forged programs while the module is still being built, so the kernel never sees them, and a ticket holding only such programs says nothing of the trusted base. Whether the kernel judged a witness is something the run observes, so the report states it rather than taking a ticket's word for which checker was at fault. The kernel is what is trusted, so every known forgery belongs before it in the end, whichever checker let it through.

**Rejected.** A field saying which checker certified the flaw, and a failure where it named the kernel and no witness reached it: the field restates what the witnesses show, and nothing held it to them. Failing every ticket that does not reach the kernel, while most of the board is still programs.
