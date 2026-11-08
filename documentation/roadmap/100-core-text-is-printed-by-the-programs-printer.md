# Core text is printed by the program's printer

Held to [One grammar reads a program and Core text, marked by the dialect](../../curios-surface/README.md#one-grammar-reads-a-program-and-core-text-marked-by-the-dialect): the tree, the parser and the printer are written once over a dialect. The Core text writer (`curios-surface/src/core_text/`) builds a shown tree for every print, which the program's printer lays out, but text written whole, which a reader reads back, is a shown tree too.

## What is wrong today

- **What a reader reads back is a tree no grammar reads.** Under `Spelling::written_whole` the writer elides nothing, so the tree it builds holds no shown form, but its type does not say so: `every_unit_of_the_prelude_reads_back_from_its_core_text` reads a print back through the parser, where a shown form written whole would surface only as a parse refusal. The writer builds `Term<Shown>` (`Tree`, `curios-surface/src/core_text/frame.rs`) whatever it writes.

## Goal

The writer builds Core text's own tree, `Term<CoreText>` and `CoreFile`, the reader's inverse, where it writes whole, so formatting a printed module is its round trip and the shown dialect stays out of what a reader reads.

## Settled

- **A shown print and a whole one are one walk.** The writer is generic over the dialect it builds; the shown dialect's forms are built only where its dialect holds them, an elision among them.

## Stages

1. **Text written whole is Core text.** The writer, generic over the dialect it builds, builds Core text's tree where it writes whole, so what a reader reads back is a tree the grammar reads. Check: the round trip, and a module's dump written whole formatted to itself.

## Retirement

Once the stage lands, `curios-surface`'s Core text decision states that text written whole is Core text's tree; nothing references this specification, and it is deleted.
