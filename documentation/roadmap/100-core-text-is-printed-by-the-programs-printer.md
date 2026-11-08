# Core text is printed by the program's printer

Held to [One grammar reads a program and Core text, marked by the dialect](../../curios-surface/README.md#one-grammar-reads-a-program-and-core-text-marked-by-the-dialect): the tree and the parser are written once over a dialect, since a grammar of Core text's own "would copy the program's term grammar, and the round-trip over `/std` could not hold the copy in step". Core text is read by that grammar, and a `.crsc` is laid out by the program's printer, but a term Core holds is written by a printer of its own, which copies the program's layout and has drifted from it.

## What is wrong today

- **Two printers lay out one grammar.** The Core text writer (`curios-surface/src/core_text/`, twelve modules, about 5,800 lines, `intrinsic.rs` alone 1,543) lays a Core term out directly, in two notations: Core text proper, which a module's and a program's dump, `curios-pipeline`'s stages, `wonder --stage`, a kernel's refusal, the board and `curios-prelude`'s `every_unit_of_the_prelude_reads_back_from_its_core_text` print, and a reader's notation for a term [`from_core`](../../curios-surface/src/from_core.rs) declines. The program's printer (`curios-surface/src/print.rs`) is a `TreePrinter` over any dialect, with a hook of its own for each production a dialect's grammar has (`Layout`), and lays out a program and the Core text `curios format` reads from a `.crsc`; the writer builds no tree for it to lay out.
- **The two layouts differ for forms both write.** The program's printer keeps `match g(x): (m) => False | 0 => h | k + 1; ih => ih end` on one line where the writer gives every arm a line of its own, and the writer spaces a declaration's colon, `let /p : Prop =`, where a program writes `let p: Prop =`; a dump is in the writer's layout and a formatted `.crsc` in the program's. Core text reads either: `let /p: Prop` and a match whose arms share its line both read.

## Goal

The writer builds the tree, `Term<CoreText>` and `CoreFile`, the reader's inverse, and the program's printer lays it out: Core text's layout is stated once, `curios format` formats a `.crsc` with its comments, and formatting a printed module is its round trip.

## Settled

- **A decorator stays beside what it decorates where the two fit their line**, and takes a line of its own above it where they do not (`own_line_if_broken`, `curios-print`); the writer lays its decorators out so today.
- **Core text needs no new grammar for the program's layout**, as probed above.
- **What a print shows and no grammar reads back is a dialect of its own, the shown dialect.** It writes Core text's forms over its own terms and the forms only a bounded print or a reader's spelling writes: `…`, a name's instance `.{…}`, a metavariable `?7`. A bounded dump, a kernel's refusal and a report's declined term are shown, since each may elide; text written whole, which a reader reads back, is Core text. Rejected: those nodes in Core text's dialect, which every reader would refuse by hand; a dialect for the reader's notation alone, which leaves `…` to both; a declined term shown as Core text proper, decorators and all; a printer of the notation's own, which keeps a second layout.
- **Core text takes the program's conventions wholesale**: the layout branches on the dialect only at its hooks.
- **The printer is a value carrying what a render of its dialect is told**: Core text's whether a decorator stands above its operand where the two do not fit their line, as a text is written, or beside it, as a refusal quotes one. A program's is told nothing.
- **The writer builds shown trees in one commit**: every node kind nests every other, so no stage can build half a tree, the two notations it writes are one walk, and `every_unit_of_the_prelude_reads_back_from_its_core_text` holds the whole.

## Stages

1. **The writer builds shown trees.** Each writer module builds the tree its form is, as a shown term, under the same spelling, minting and elision (`Minting`, [A printer states each fact once, where it is bound](../design/tools/a-printer-states-each-fact-once-where-it-is-bound.md)'s bound on a print), the program's printer lays it out, and the writer's printer goes; Core text takes the program's conventions. Check: `every_unit_of_the_prelude_reads_back_from_its_core_text`, and each pinned printout restated where its layout moved.
2. **Text written whole is Core text.** The writer, generic over the dialect it builds, builds Core text's tree where it writes whole, so what a reader reads back is a tree the grammar reads. Check: the round trip, and a module's dump written whole formatted to itself.

## Retirement

Once the stages land, `curios-surface`'s README states the printer beside the tree and the parser in the decision this specification is held to, and its Core text decision the layout Core text takes; nothing references this specification, and it is deleted.
