# The Curios Book

## Status

Not refined yet. This file records a name, a destination, and the one rule that keeps the book from becoming a second reference. No chapter is written and no directory exists; whoever picks this up creates `documentation/book/` and refines this file first.

It is one file today because there is one thing to say. A campaign that grows a sequence of its own takes a directory then, under the rule the roadmap already states for every other campaign.

## Why it exists

A reader arriving at Curios finds a complete surface reference, a command-line manual, and the design and soundness trees. Nothing teaches. The gap is sharpest at the half that distinguishes the language: [syntax.md](../syntax.md)'s "Equality and proofs" is thirteen lines, and no document in the repository walks anyone through stating and proving a proposition.

## The rule that comes before any prose

**The book owns sequence and motivation; it owns no facts.** Every rule it touches links to [syntax.md](../syntax.md), every command to [usage.md](../usage.md), and every "why is it this way" to the design decision that settled it. Where the book and its source disagree, the source is right and the book is broken.

Without that rule the book becomes exactly the parallel explanation the documentation ownership table exists to prevent, and it drifts silently, because nothing checks prose against a compiler.

## What is certain

- **The destination is a reader who states and proves a proposition of their own.** That is the capability nothing teaches today, and it is what gives every earlier chapter a reason to exist.
- **Teaching programs already have a category and a home.** `programs/README.md` names `hello_world.crs` and `dependent_vectors.crs` as samples that "show the language rather than measure it", sitting beside the measurement corpus rather than inside it.
- **Nothing in the tree checks a document's examples.** The workspace has no doctests at all, and the gate's separate doctest step exists precisely to catch the first one written.
- **The sources the book would link are written and current** — the reference, the manual, the design decisions and the soundness perimeter. The book links to them; it does not restate them.

## What has to be decided

- **Where the programs live, and how they are checked.** An example that is not compiled and run by a test drifts. Whether they extend `programs/`'s sample category or take a directory of their own, and whether the test asserts each program's output or only that it compiles.
- **The chapter sequence**, and how early the proof half arrives — late enough that the reader can already write a program, early enough that it does not read as an appendix.
- **What the book teaches that the reference only states.** The i31 envelope, so that overflow is a boundary a reader knows about rather than a surprise; and `/std/Map`'s deliberate lack of a `Key(Nat)`, which today is explained only in a benchmark's methodology notes.
- **Where it sits.** `documentation/book/` with its own index, a row in the documentation ownership table saying what it owns and what it must not, a row in `README.md`'s "Go deeper", and whether it becomes the landing page for the site that today opens the playground.
- **Whether chapters ship independently**, which is what would make this the one 1.0 item that can land in pieces rather than whole.

## Deliberately not specified

The chapter list, the prose voice, and the rendering. Whether the book is published anywhere but the repository.
