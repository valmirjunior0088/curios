# The editor is answered at a position

Working specification for what an editor may ask of a place in a file. The language server publishes a document's diagnostics and formats it, and answers no question about a position: where a name is declared, what it is declared as, what a module holds, what type a term has. A reader finds each by leaving the editor for the library's pages or its source.

It is independent of every other spec, and waits on nothing.

## What this builds on

- **The server states two capabilities.** `serve` (`curios-wonder/src/server.rs`) declares full-text synchronization and document formatting; the request loop answers `Formatting` and hands every other method to `reply_unhandled`. The VS Code extension launches the server through a generic client (`editors/vscode/extension.js`) and the Zed extension through the editor's own (`editors/zed/src/lib.rs`), so a capability the server declares is used with no change to either.
- **A question the server answers is a `wonder` query.** [`documentation/usage.md`](../../usage.md) states `server` as "the same questions over the language server protocol", and both transports stand on one engine (`curios-wonder/src/ask.rs`, `server.rs`). Today the sentence holds of `diagnostics` alone, which is an entry of [tools' findings](000-findings.md). An answer about a position is then a query first, asked of a place on the command line and of a cursor by an editor.
- **The compiler is on one thread, and its terms do not leave it.** The analyst owns the compiler, its `Rc`-shared terms and the units a session keeps (`curios-verdicts/src/verdicts.rs`); the protocol thread reads messages, keeps the overlay and answers what needs no compilation, so a request is never behind a compile (`server.rs`'s `//!`).
- **A written name carries its place.** The lowering places every term by the span it was written at (`Lowerer::term`, `curios-text/src/into_core/lowerer.rs`), and a written name lowers to the declaration it resolved to, so a use of a name is a node that holds both.
- **A place is its declaration's own.** An extent is two offsets into the text of the declaration that wrote it, with a stamp of that text, and a unit's table says where each text lies and where each definition's name was written (`Extent`, `Anchors::resolve`, `Anchors::label`, `curios-core/src/extent.rs`). An edit elsewhere in a file moves none of a declaration's places.
- **A unit keeps each declaration lowered, read one at a time.** `PreparedText` holds where each name's lowered item is and what it is declared as, with nothing read, and reads the one item asked for (`curios-text/src/into_core.rs`); it holds what each module declares and exposes, and every binding a `use` brought into scope.
- **A declaration's head and prose have one maker.** The documentation builder prints each declaration's head as its author wrote it, with the prose above it, from the surface tree the lowering parsed (`curios-text/src/into_core/document.rs`), and the record of the library a compiler was built with is archived in it ([A library is documented for its consumers, from the compilation that builds it](../../design/tools/a-library-is-documented-for-its-consumers-from-the-compilation-that-builds-it.md)).
- **The elaborator settles a type for every term it finishes.** Each rebuilt term is recorded with its type as it settles, the list of a finished item is zonked once, the erasure obligations read it, and it is dropped (`Context::record_checked`, `settle_checked`, `split_checked`, `curios-elab/src/context.rs`). A written goal reports its scope, its expected type and what fits.
- **A parse is not the protocol thread's to pay.** Timed at `37974e00f`, one reading on a machine in use: `curios format --check` on `curios-std/src/Flt.crs`, 46,282 bytes, 543 ms; on `Str.crs`, 33,622 bytes, 319 ms; on `Bool.crs`, 3,102 bytes, 11 ms. [A parse allocates for what it builds](../surface/201-a-parse-allocates-for-what-it-builds.md) holds the cause.
- **The library a compiler was built with names its sources by the paths it was built from**, and carries their text: the paths of the checkout where a compiler is built there, and of another machine under an installed release.
- **An item the parser cannot read costs that item** (`Module::parse`, `curios-text/src/module.rs`), so a file being typed keeps the declarations around the one being written.

## Prior art

- **Proof writers work by reacting to what the assistant shows.** Across thirty users of Rocq and Lean observed at their own work, proof writers iterate on a proof by reacting to the feedback the assistant gives, and build proofs with many resources outside it (Shi, Torczon, Goldstein, Pierce and Head, [QED in Context](https://2025.splashcon.org/details/OOPSLA/15/QED-in-Context-An-Observation-Study-of-Proof-Assistant-Users), OOPSLA 2025).
- **A newcomer's account of Lean and Coq names the editor first among what worked**: "Lean has a nice LSP implementation, making it easy to integrate with your text editor of choice" ([ntietz, My first impressions from a few weeks with Lean and Coq](https://ntietz.com/blog/first-impressions-of-lean-and-coq/)).
- **Most of a typed functional language's users run its server.** 68% of the 2022 State of Haskell survey's respondents use the Haskell Language Server ([results](https://taylor.fausak.me/2022/11/18/haskell-survey-results/)).
- **Idris edits through the same questions**: the type of a hole, a case split on a pattern variable, the clauses a definition lacks ([Idris 2, Interactive Editing](https://idris2.readthedocs.io/en/latest/tutorial/interactive.html)). The written goal is Curios's form of the first.
- **Three compilers record at elaboration what an editor asks, by place.** Lean: "The InfoTree is a structure that is generated during elaboration and used by the language server to look up information about objects at particular points", a term's entry holding its local context, its expected type and its expression ([`InfoTree/Types.lean`](https://github.com/leanprover/lean4/blob/master/src/Lean/Elab/InfoTree/Types.lean)). Idris 2: a "Mapping from annotation to the name defined with that annotation, and its type" ([`Core/Metadata.idr`](https://github.com/idris-lang/Idris2/blob/main/src/Core/Metadata.idr)). GHC: "HIE files are a new type of file that can be emitted by GHC via the `-fwrite-ide-info` flag", since otherwise "all this information is not easily accessible to you after GHC is finished" ([HIE files](https://www.haskell.org/ghc/blog/20190626-HIEFiles.html)). Lean's terms carry no place, so its tree is where a place meets a term and nothing is held twice; GHC's file is derived from the tree its checker built.
- **A server answers from a snapshot, in the editor's terms.** rust-analyzer: "`Analysis` is an immutable snapshot of the state", its interface "talks about offsets and string labels rather than in terms of definitions or types", and one crate "is the only crate that knows about LSP and JSON serialization" ([Architecture](https://rust-analyzer.github.io/book/contributing/architecture.html)).
- **The protocol lets an answer be of an older text**: "The result even computed on an older state might still be useful for the client" ([Language Server Protocol 3.17](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)).

## Decisions

Settled 2026-10-13.

- **A place's answer is read off the declaration the unit keeps.** A file and an offset give the declaration whose text holds it, by the unit's table; its lowered item is read, the one item; the innermost node at the offset is a name, and the name gives where it was written and what declares it. No table of names by place is written beside the terms that carry them.
- **A question is a query first.** `wonder at <LINE>:<COLUMN> <FILE>` answers what is written at a place: the name, where it is declared, and its head and prose. `wonder declarations <FILE>` answers what a file declares: each name, what it is declared as, and where it stands. The protocol's requests are the same questions asked by a cursor.
- **The server answers from what the last check left, with no parse and no wait.** After a check the analyst hands the protocol thread plain data for each open document: for each declaration, its text, and its answers by offsets within that text. A request finds the declaration whose text stands unchanged around the cursor in the buffer as it is. The declaration being edited is not found, and is answered after the next check.
- **A head and its prose are the documentation builder's**, asked for one declaration, public or not, and the archived record's for the library a compiler was built with. Nothing prints a head a second way.
- **The protocol's units are `server.rs`'s**, as UTF-16 is: the engine's coordinates are bytes.
- **What the elaborator knows is the list it already settles, kept.** A type at a place, and a name only elaboration resolves, are read from the settled terms of a declaration, kept with it where they are dropped today: the record the three compilers above write, made once, by the elaboration that already makes it. Not verified: whether every written node has an entry; what the list weighs, in a session and stored with a declaration a recompile keeps; what a refused item's entries hold, which are truncated today; where an expected type would be recorded, the list holding the settled one alone; and a pattern's constructor, which is compiled into a match and has no entry at the tag as written.

## What has no answer yet

- **A name the elaborator resolves**: a pattern's constructor, a field's label, a method reached through a witness. Each waits for the settled list.
- **A segment of a path.** A path is one name with one place, so a cursor anywhere in `Io/Reader/line` is answered with `line`'s declaration; a segment answered as the module it names is the third stage's.
- **A declaration whose source is not on disk**: the library a compiler was built with, under an installed release. Its head and prose are answered, and its place is not.

## Stages

1. **Where a name is declared, and what a file declares.** The read off the lowered item, held by a test that every written name's place answers the declaration it names; the two queries; the server's answers to a definition request and to a document's symbols. Check: both queries over a file of the library and a program of `programs/`; a request sent while a check runs is answered at once, and one in a declaration edited since the last check is answered with nothing.
2. **A name's head and prose at a use.** `wonder at` prints them, and the server answers a hover. Check: a public name of the library, a private name of the program's own, and a name whose unit was restored from the store.
3. **The members of the module a path names.** A segment of a path answered as its module, and the names a module exposes offered where a path is being typed, read from what the unit's text facet holds of each module. Not refined: what a path being typed parses to, an item the parser cannot read costing that item.
4. **The type of the term at a position**, and a name the elaborator resolves, from the settled list kept. Not refined: the five points above.

## Verification

`cargo xtask workspace lint` and `cargo xtask workspace format` after each stage, with the stage's tests by name, and the whole workspace's tests once where the first two have landed. Through a release build: both queries, and a scripted session with the server — a document opened, a name asked of, the document's declarations, a hover, one declaration edited and another asked of — each answer shown.

## Rejected

- **A table of names by place, written beside the terms.** A written name's node holds its place and what it resolved to, so the table is a second copy of both.
- **A type asked of the kernel again.** The elaborator settled it, and a second derivation is a second answer that may be spelled another way.
- **A parse on the protocol thread**, to find the declaration under a cursor: a large file's costs what a request must not wait, and the declaration's text as last checked finds it with none.
- **A request handed to the analyst.** It waits behind the check in hand, which is what the two threads exist to prevent.
- **An answer by the buffer's own coordinates as last checked.** An edit above a declaration moves every place below it, where a declaration's own text does not move.

## Completion and retirement

Each landed stage is restated here as what is left; `documentation/usage.md` lists the queries, `curios-wonder`'s rustdoc their contracts, and a decision under `documentation/design/tools/` takes the design with what it rejects. Once the fourth has landed the line in `documentation/roadmap.md` becomes a checked summary, and this file is deleted.
