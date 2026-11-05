# Core text is printed by the program's printer

Held to [One grammar reads a program and Core text, marked by the dialect](../../curios-surface/README.md#one-grammar-reads-a-program-and-core-text-marked-by-the-dialect): the tree and the parser are written once over a dialect, since a grammar of Core text's own "would copy the program's term grammar, and the round-trip over `/std` could not hold the copy in step". Core text is read by that grammar and printed by a printer of its own, which copies the program's layout and has drifted from it, and a `.crsc` cannot be formatted.

## What is wrong today

- **Two printers lay out one grammar.** The Core text writer (`curios-surface/src/core_text/`, twelve modules, about 5,800 lines, `intrinsic.rs` alone 1,543) lays a Core term out directly, in two notations: Core text proper, which a module's and a program's dump, `curios-pipeline`'s stages, `wonder --stage`, a kernel's refusal, the board and `curios-prelude`'s `every_unit_of_the_prelude_reads_back_from_its_core_text` print, and a reader's notation for a term [`from_core`](../../curios-surface/src/from_core.rs) declines. The program's printer (`curios-surface/src/print.rs`) lays out the tree a formatter reads and a report's term `from_core` translates, and prints no Core text form: `Subterm::Core` is unreachable there, though the tree it walks is generic over the dialect.
- **The two layouts differ for forms both write.** The program's printer keeps `match g(x): (m) => False | 0 => h | k + 1; ih => ih end` on one line where the writer gives every arm a line of its own, and the writer spaces a declaration's colon, `let /p : Prop =`, where a program writes `let p: Prop =`. Core text reads either: `let /p: Prop` and a match whose arms share its line both read.
- **`curios format` reads `.crs` alone.** A `.crsc` is laid out by hand or by printing its module back, which drops its comments, and the board's seventeen mix the two layouts.

## Goal

The writer builds the tree, `Term<CoreText>` and `CoreFile`, the reader's inverse, and the program's printer lays it out: Core text's layout is stated once, `curios format` formats a `.crsc` with its comments, and formatting a printed module is its round trip.

## Settled

- **A decorator stays beside what it decorates where the two fit their line**, and takes a line of its own above it where they do not (`own_line_if_broken`, `curios-print`); the writer lays its decorators out so today.
- **Core text needs no new grammar for the program's layout**, as probed above.

## Open

- **A declined term's reader notation.** `from_core` declines a term no program spells where its reader stands, and the writer prints it with a name's instance as `.{…}` and a level metavariable as `?u1`, which Core text's grammar does not write. (a) Nodes the printer writes and no parse makes, as the program's tree holds nodes only lowering mints (recommended: one tree, one printer); (b) a declined term shown as Core text proper, its decorators written; (c) a printer of the reader's notation's own, which keeps a second layout.
- **Which conventions Core text takes.** The program's wholesale (recommended: no branch on the dialect in the layout), or branches keeping the writer's where they differ.

## Stages

1. **The printer over a dialect.** `print.rs` generic over the dialect, with what a dialect prints beyond the forms both write — a form, a declaration's head, a case's fields — a hook of its own; a program's prints nothing. Check: every program prints as it did, `cargo xtask workspace test` with `curios-surface`'s format suites.
2. **Core text in the printer, and the formatter over it.** A decorated operand, a witness by its identity, an intrinsic by its variant, a decorator's values, a declaration's head under its decorator and absolute name, a group of any kinds joined by `and`, the entry and the type it states; the formatter over a `CoreFile`, with its comments, its items' spans and the reparse check; `curios format` taking a `.crsc`, in its help and [`usage.md`](../usage.md). Check: format tests over each Core text form, a comment at each place one stands, and a `.crsc` the reader refuses formatted all the same.
3. **The writer builds trees.** Each writer module builds the tree its form is, under the same spelling, minting and elision (`Minting`, [A printer states each fact once, where it is bound](../design/tools/a-printer-states-each-fact-once-where-it-is-bound.md)'s bound on a print), and the program's printer lays it out; Core text takes the conventions the open question settles. Check: `every_unit_of_the_prelude_reads_back_from_its_core_text`, each pinned Core printout restated where its layout moved, and a module's dump formatted to itself.
4. **The reader's notation**, as the open question settles it. Check: the diagnostics that pin a declined term.
5. **The committed Core text formatted.** Every `.crsc` the tree holds through `curios format`, and a test holding them formatted, as the board's programs are. Check: `cargo xboard report` unchanged.

## Retirement

Once the stages land, `curios-surface`'s README states the printer beside the tree and the parser in the decision this specification is held to, and its Core text decision the layout Core text takes; nothing references this specification, and it is deleted.
