# Core text is printed by the program's printer

Held to [One grammar reads a program and Core text, marked by the dialect](../../curios-surface/README.md#one-grammar-reads-a-program-and-core-text-marked-by-the-dialect): the tree and the parser are written once over a dialect, since a grammar of Core text's own "would copy the program's term grammar, and the round-trip over `/std` could not hold the copy in step". Core text is read by that grammar and printed by a printer of its own, which copies the program's layout and has drifted from it, and a `.crsc` cannot be formatted.

## What is wrong today

- **Two printers lay out one grammar.** The Core text writer (`curios-surface/src/core_text/`, twelve modules, about 5,800 lines, `intrinsic.rs` alone 1,543) lays a Core term out directly, in two notations: Core text proper, which a module's and a program's dump, `curios-pipeline`'s stages, `wonder --stage`, a kernel's refusal, the board and `curios-prelude`'s `every_unit_of_the_prelude_reads_back_from_its_core_text` print, and a reader's notation for a term [`from_core`](../../curios-surface/src/from_core.rs) declines. The program's printer (`curios-surface/src/print.rs`) is a `TreePrinter` over any dialect, with a hook of its own for each production a dialect's grammar has (`Layout`), but only a program's tree has a layout: Core text has none.
- **The two layouts differ for forms both write.** The program's printer keeps `match g(x): (m) => False | 0 => h | k + 1; ih => ih end` on one line where the writer gives every arm a line of its own, and the writer spaces a declaration's colon, `let /p : Prop =`, where a program writes `let p: Prop =`. Core text reads either: `let /p: Prop` and a match whose arms share its line both read.
- **`curios format` reads `.crs` alone.** A `.crsc` is laid out by hand or by printing its module back, which drops its comments, and the board's seventeen mix the two layouts.

## Goal

The writer builds the tree, `Term<CoreText>` and `CoreFile`, the reader's inverse, and the program's printer lays it out: Core text's layout is stated once, `curios format` formats a `.crsc` with its comments, and formatting a printed module is its round trip.

## Settled

- **A decorator stays beside what it decorates where the two fit their line**, and takes a line of its own above it where they do not (`own_line_if_broken`, `curios-print`); the writer lays its decorators out so today.
- **Core text needs no new grammar for the program's layout**, as probed above.
- **A declined term's reader notation is a dialect of its own.** The forms only a report shows — `…`, a name's instance `.{…}`, a level metavariable `?u1`, a metavariable `?7`, a witness by its identity — are a third dialect, which no grammar reads and the one printer lays out: a parsed tree cannot hold one, and `to_core` is never handed one. Rejected: those nodes in Core text's dialect, which every reader would refuse by hand; a declined term shown as Core text proper, decorators and all; a printer of the notation's own, which keeps a second layout.
- **Core text takes the program's conventions wholesale**: the layout branches on the dialect only at its hooks.
- **The printer is a value carrying what a render of its dialect is told**: Core text's whether a decorator stands above its operand where the two do not fit their line, as a text is written, or beside it, as a refusal quotes one. A program's is told nothing.
- **The writer builds trees in one commit**: every node kind nests every other, so no stage can build half a tree, and `every_unit_of_the_prelude_reads_back_from_its_core_text` holds the whole.

## Stages

1. **Core text in the printer, and the formatter over it.** A decorated operand, a witness by its identity, an intrinsic by its variant, a decorator's values, a declaration's head under its decorator and absolute name, a group of any kinds joined by `and`, the entry and the type it states; the formatter over a `CoreFile`, with its comments, its items' spans and the reparse check; `curios format` taking a `.crsc`, in its help and [`usage.md`](../usage.md). Check: format tests over each Core text form, a comment at each place one stands, and a `.crsc` the reader refuses formatted all the same.
2. **The writer builds trees.** Each writer module builds the tree its form is, under the same spelling, minting and elision (`Minting`, [A printer states each fact once, where it is bound](../design/tools/a-printer-states-each-fact-once-where-it-is-bound.md)'s bound on a print), and the program's printer lays it out; Core text takes the program's conventions. Check: `every_unit_of_the_prelude_reads_back_from_its_core_text`, each pinned Core printout restated where its layout moved, and a module's dump formatted to itself.
3. **The reader's notation**, as settled above. Check: the diagnostics that pin a declined term.
4. **The committed Core text formatted.** Every `.crsc` the tree holds through `curios format`, and a test holding them formatted, as the board's programs are. Check: `cargo xboard report` unchanged.

## Retirement

Once the stages land, `curios-surface`'s README states the printer beside the tree and the parser in the decision this specification is held to, and its Core text decision the layout Core text takes; nothing references this specification, and it is deleted.
