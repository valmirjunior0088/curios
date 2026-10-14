# The editor is answered at a position

Working specification for what an editor may still ask of a place in a file. A written name answers where it is declared, what its declaration states of it and what its file declares, at the command line and in an editor ([A place in a file is answered from the declaration that writes it](../../design/tools/a-place-in-a-file-is-answered-from-the-declaration-that-writes-it.md)). What is left is what no lowered term holds at its own place: a segment of a path, a name on a `use` line, a name a node holds rather than a term, a binder, and the type of a term.

It is independent of every other spec, and waits on nothing.

## What this builds on

- **A name is read off the lowered declaration.** `Text::mentions` (`curios-text/src/into_core/mention.rs`) reads the names a declaration's text writes off its lowered items, and `mention/tests.rs` holds it over a program that writes a name in every position and marks each one that has no answer, so a position that gains one fails there until its mark is taken out.
- **One engine and one record serve both transports.** `written` (`curios-wonder/src/named.rs`) lowers a subject over the fold and no further, and hands back each text of a file with its names; `Named::at` is the one rule for the name at an offset. `wonder at` and `wonder declarations` print the records, and the server answers a definition, a hover and a document's symbols from the ones its last check left (`Answers`, `Standing`, `curios-wonder/src/server.rs`).
- **A declaration's head and prose are read off its own text** (`heading`, `curios-text/src/into_core/document.rs`), through the one reading of what an item states of its definitions (`stated`), which the documentation pages are built by too.
- **A unit keeps what each module declares and exposes, and every binding a `use` brought into scope** (`PreparedText`'s `table`, `public` and `imports`, `curios-text/src/into_core.rs`). An import holds the spelling it came in under and no place (`curios_core::Import`), and a `use` item is the text of no declaration: `anchored` gives it no anchor, so no extent is measured in it.
- **The elaborator settles a type for every term it finishes.** Each rebuilt term is recorded with its type as it settles, the list of a finished item is zonked once, the erasure obligations read it, and it is dropped (`Context::record_checked`, `settle_checked`, `split_checked`, `curios-elab/src/context.rs`). A written goal reports its scope, its expected type and what fits.
- **A parse is not the protocol thread's to pay for a document.** Timed at `37974e00f`, one reading on a machine in use: `curios format --check` on `curios-std/src/Flt.crs`, 46,282 bytes, 543 ms; on `Str.crs`, 33,622 bytes, 319 ms; on `Bool.crs`, 3,102 bytes, 11 ms. The three readings predate `ed3f643e4`, which stopped a parse reading the rest of its file at each keyword, and [A parser is a function of the input](../../../curios-parse/README.md#a-parser-is-a-function-of-the-input), either of which may have moved them. A hover parses the one declaration it is asked of.
- **What a check leaves for a document is small.** Counted at `ae734e225`: `curios-std/src/Flt.crs` leaves 112 texts and 1,084 names, about 109 KB of records beside the checked text they keep.
- **An item the parser cannot read costs that item** (`Module::parse`, `curios-text/src/module.rs`), so a file being typed keeps the declarations around the one being written.

## Prior art

- **Three compilers record at elaboration what an editor asks, by place.** Lean: "The InfoTree is a structure that is generated during elaboration and used by the language server to look up information about objects at particular points", a term's entry holding its local context, its expected type and its expression ([`InfoTree/Types.lean`](https://github.com/leanprover/lean4/blob/master/src/Lean/Elab/InfoTree/Types.lean)). Idris 2: a "Mapping from annotation to the name defined with that annotation, and its type" ([`Core/Metadata.idr`](https://github.com/idris-lang/Idris2/blob/main/src/Core/Metadata.idr)). GHC: "HIE files are a new type of file that can be emitted by GHC via the `-fwrite-ide-info` flag", since otherwise "all this information is not easily accessible to you after GHC is finished" ([HIE files](https://www.haskell.org/ghc/blog/20190626-HIEFiles.html)). Lean's terms carry no place, so its tree is where a place meets a term and nothing is held twice; GHC's file is derived from the tree its checker built.
- **Idris edits through the same questions**: the type of a hole, a case split on a pattern variable, the clauses a definition lacks ([Idris 2, Interactive Editing](https://idris2.readthedocs.io/en/latest/tutorial/interactive.html)). The written goal is Curios's form of the first.
- **Proof writers work by reacting to what the assistant shows.** Across thirty users of Rocq and Lean observed at their own work, proof writers iterate on a proof by reacting to the feedback the assistant gives (Shi, Torczon, Goldstein, Pierce and Head, [QED in Context](https://2025.splashcon.org/details/OOPSLA/15/QED-in-Context-An-Observation-Study-of-Proof-Assistant-Users), OOPSLA 2025).

## Decisions

Settled 2026-10-13.

- **What the elaborator knows is the list it already settles, kept.** A type at a place is read from the settled terms of a declaration, kept with it where they are dropped today: the record the three compilers above write, made once, by the elaboration that already makes it. Not verified: whether every written node has an entry; what the list weighs, in a session and stored with a declaration a recompile keeps; what a refused item's entries hold, which are truncated today; and where an expected type would be recorded, the list holding the settled one alone.
- **A name no term holds is recorded where it is resolved.** The lowering resolves a bare literal's head and a structure pattern's into the node they open, and the elaborator resolves a pattern's constructor, a field's label, a projection's and a method reached through a witness: none is a term at its own place, so each is recorded by its extent where it is resolved, in one record beside the settled list. Not a second copy, since no term holds them. Chosen over a record in the lowering alone, whose home and shape would have been set before the elaborator's were designed.
- **A stage's answers reach both transports as the names' do**: a record of the engine, a `wonder` query, and the server's request from what the last check left.

## What has no answer yet

- **A segment of a path.** A path is one name with one place, so a cursor anywhere in `Io/Reader/line` is answered with `line`'s declaration, and no segment with the module it names.
- **A name on a `use` line.** The line is no declaration's text and its bindings are held under no place.
- **A name no term holds at its own place**: the bare head of a structure literal and of a structure pattern, a pattern's constructor, a field's label where a literal writes it, a projection's label, and a method reached through a witness.
- **A binder.** A local variable answers nothing, at its use or where it is bound.
- **The type of a term.**
- **The place of a declaration no file holds**: the library a compiler was built with, under an installed release. Its head and prose are answered.

## What an answer costs, not measured

- **A check that refused its subject is followed by a second lowering of it.** What a document names is left once its diagnostics are published (`Analyst::leave`): a subject the check accepted is read off the unit the session kept, and one it refused was kept as no unit, so it is lowered again over the session's last. A check that hands on the lowering it made would lower once.
- **A hover parses one declaration on the protocol thread**, which the declaration's size bounds and nothing else does.

## Stages

1. **The members of the module a path names, and a `use` line's names.** A segment of a path answered as its module; a `use` line given a text of its own, so its names are placed as a declaration's are; and the names a module exposes offered where a path is being typed, read from what the unit's text stage holds of each module. Not refined: what a path being typed parses to, an item the parser cannot read costing that item, and what a module is answered with where no file is its own.
2. **The type of the term at a position, a binder, and a name no term holds**, from the settled list kept and the record beside it. Not refined: the four points the first decision lists, and what each of the six names in the second is recorded as.

## Verification

`cargo xtask workspace lint` and `cargo xtask workspace format` after each stage, with the stage's tests by name, and the whole workspace's tests once where both have landed. `mention/tests.rs`'s program loses the mark of each name a stage answers. Through a release build: `wonder at` over a file of the library and a program of `programs/`, and a scripted session with the server — a document opened, a path's segment asked of, a pattern's constructor, a term's type, one declaration edited and another asked of — each answer shown.

## Rejected

- **A type asked of the kernel again.** The elaborator settled it, and a second derivation is a second answer that may be spelled another way.
- **A record of names by place for every name.** A written name's term holds its place and what it resolved to; the record is for the names no term holds.
- **A record in the lowering alone, made before the elaborator's is designed**: two records of one kind, each with a home of its own.

## Completion and retirement

Each landed stage is restated here as what is left; `documentation/usage.md` lists the queries, `curios-wonder`'s rustdoc their contracts, and [the decision](../../design/tools/a-place-in-a-file-is-answered-from-the-declaration-that-writes-it.md) takes each stage's design with what it rejects. Once both have landed the line in `documentation/roadmap.md` becomes a checked summary, and this file is deleted.
