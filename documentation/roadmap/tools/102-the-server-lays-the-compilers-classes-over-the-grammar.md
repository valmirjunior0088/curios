# The server lays the compiler's classes over the grammar

An editor has no semantic tokens from the server, so its colours are a grammar's guess alone. The guess is wrong in places: a capitalised name is taken for a type, though `List` is both a module and a type, and a constructor, a function and a local are all written in lower case. [Curios is coloured from one class set](../../design/tools/curios-is-coloured-from-one-class-set.md) has the server's semantic tokens lay the compiler's answer over that guess. Today the server advertises none (`ServerCapabilities`, `curios-wonder/src/server.rs`), and `wonder` answers no class.

The next step is stage 1, a delivery, which settles the open question under *Open*.

## What is held, and what is missing

- **A name's class is held.** Each name a check leaves for a document (`Named`, `curios-wonder/src/named.rs`) carries what its declaration is declared as. `Declared::class` (`curios-core/src/definitions.rs`) classes it, as the documentation's builder does. Whether the name is the declaration's own (`Named::declares`) and the global it resolved to (`Named::named`) are held beside it.
- **Where a contextual word stood is not held.** `test`, `concept` and `satisfy` at an item's head, and `and` between a group's members, are read through `parse_keyword` (`curios-surface/src/parse.rs`), which hands back `()`. Neither the tree (`TopTest`, `TopConcept`, `TopWitness`, `TopLet`, `TopInduct`, `TopStruct`, and a local `LetBinding`) nor the lowering records where one stood. The class set paints such a word as a keyword only where it stands as one, and a grammar cannot tell `test` at an item's head from `test` bound by a `let`.
- **No walk reaches every term.** `curios-surface` has no walk over the term forms, and a local `let … and …` group stands at any depth of a term.

## Decided

- **The command line answers by a query of its own**, settled 2026-11-17: `curios wonder --classes [SUBJECT]` lists every classed span of the file the subject is asked through, one row per line: its place as a report prints one, `<file>:<line>:<column>`, its length, its class and its modifiers. They are the same records the server encodes as tokens, documented in `documentation/usage.md` beside `--at`.
- **The legend is the class set's, mapped once, exhaustively**: a module `namespace`, a type `type`, a constructor `enumMember`, a concept `interface`, a method `method`, a function `function`, a value `variable` with `readonly`, a field `property`, a parameter `parameter`, a type parameter `typeParameter`, a local `variable`, and the keyword `keyword`. The modifiers are `declaration`, where the name is its declaration's own, and `defaultLibrary`, where it resolved into `/std` or `/sys`.
- **The whole document at once**, with positions in UTF-16 at the protocol's boundary, as every other answer of the server's converts.
- **What no record answers keeps the grammar's colour.** A check's record of a file (`Placed`, `curios-wonder/src/named.rs`) answers a name, a local where it is bound and read, a field's label and a path's segment; what it answers nothing of keeps the grammar's.

## Open

- **Where the engine reads a local `and`**: from a walk of the terms it owns, or from a list of a module's contextual words that the parse keeps beside its items. The first adds a walk over every term form, and the second states the words a second time in the tree. Settled in stage 1 by what each costs in files.

## Stages

1. **The tree records where each contextual word was read.** Each member of a top-level group, a test, and each member of a local group records the span of the contextual word that opened it, where one did, excluded from `PartialEq` as `TopWitness::head` is. `parse_keyword` gains a form that hands the span back, and every construction site, `curios-surface/src/from_core.rs` among them, states none. Check: parser tests of each word's span, `test` bound by a `let` among them, and printing, lowering and `/std`'s image unchanged. One commit.
2. **The engine classes every name it answers and every contextual word, and `wonder --classes` answers them.** Each `Named` is classed by `Declared::class` with its modifiers, and each contextual word where it stands as one is classed as the keyword. Check: a type, a constructor, a concept, a method, a function and a value each classed at its place; `test` classed a keyword at an item's head and not where a `let` binds it; nothing classed where the text does not lower. One commit.
3. **The server advertises semantic tokens** for the whole document over the legend above, its positions in UTF-16. Check: the stage 2 cases through the server, positions over a line holding a character past the Basic Multilingual Plane, and nothing classed where the text does not lower. One commit.

## Verification

`cargo xtask workspace lint` and the stage's tests by name at each stage; `curios-surface`'s and `curios-module`'s suites at stage 1; `curios-wonder`'s suite at stages 2 and 3.

## Retirement

Once stage 3 lands, the decision's sentence on semantic tokens holds as stated, and this file is deleted.
