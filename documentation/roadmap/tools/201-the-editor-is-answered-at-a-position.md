# The editor is answered at a position

Not refined yet. Working specification for what an editor may ask of a place in a file. The language server publishes a document's diagnostics and formats it, and answers no question about a position: where a name is declared, what it is declared as, what a module holds, what type a term has. A reader finds each by leaving the editor for the library's pages or its source.

It is independent of every other spec, and waits on nothing.

## What is known

- **The server states two capabilities.** `serve` (`curios-wonder/src/server.rs`) declares full-text synchronization and document formatting; the request loop answers `Formatting` and hands every other method to `reply_unhandled`. The VS Code extension contributes a language, a grammar, one setting and one command (`editors/vscode/package.json`), and the Zed extension a configuration and a highlight query (`editors/zed/languages/curios/`), so neither answers a position either.
- **A question the server answers is a `wonder` query.** [`documentation/usage.md`](../../usage.md) states `server` as "the same questions over the language server protocol", and both transports stand on one engine (`curios-wonder/src/ask.rs`, `server.rs`). Today the sentence holds of `diagnostics` alone, which is an entry of [tools' findings](000-findings.md). An answer about a position is then a query first, asked of a place on the command line and of a cursor by an editor.
- **A request is never behind a compile.** The compiler is on one thread, the analyst, and the protocol thread answers what needs no compilation at once (`server.rs`'s `//!`). The analyst's `Session` (`curios-verdicts/src/verdicts.rs`) keeps the last unit it compiled for each unit it reached.
- **What an answer could be read from is already made.** A declaration's place is the label the lowering keeps for every declaration a name was written for (`curios_core::Anchors`, `curios-core/src/extent.rs`). The paths a unit gives a name are listed for it (`PreparedText::spellings`, `curios-text/src/into_core.rs`), with its lexicon of marks and labels (`curios_core::Lexicon`). A public declaration's signature and documentation are a record the compilation builds ([A library is documented for its consumers, from the compilation that builds it](../../design/tools/a-library-is-documented-for-its-consumers-from-the-compilation-that-builds-it.md)), and the prelude's is archived. A written goal reports its scope, its expected type and what fits, and `let y: ? = e` reports `e`'s type.
- **An item the parser cannot read costs that item** (`Module::parse`, `curios-text/src/module.rs`), so a file being typed keeps the declarations around the one being written.

## Prior art

- **Proof writers work by reacting to what the assistant shows.** Across thirty users of Rocq and Lean observed at their own work, proof writers iterate on a proof by reacting to the feedback the assistant gives, and build proofs with many resources outside it (Shi, Torczon, Goldstein, Pierce and Head, [QED in Context](https://2025.splashcon.org/details/OOPSLA/15/QED-in-Context-An-Observation-Study-of-Proof-Assistant-Users), OOPSLA 2025).
- **A newcomer's account of Lean and Coq names the editor first among what worked**: "Lean has a nice LSP implementation, making it easy to integrate with your text editor of choice" ([ntietz, My first impressions from a few weeks with Lean and Coq](https://ntietz.com/blog/first-impressions-of-lean-and-coq/)).
- **Most of a typed functional language's users run its server.** 68% of the 2022 State of Haskell survey's respondents use the Haskell Language Server ([results](https://taylor.fausak.me/2022/11/18/haskell-survey-results/)).
- **Idris edits through the same questions**: the type of a hole, a case split on a pattern variable, the clauses a definition lacks ([Idris 2, Interactive Editing](https://idris2.readthedocs.io/en/latest/tutorial/interactive.html)). The written goal is Curios's form of the first.

## What is not known

- **Which answers, and in what order.** Recommended: where a name is declared and what a file declares, which read names alone; then a declared name's signature and documentation at a use of it; then the members of the module a path being typed names; then the type of the term at a position, which is the one answer no stage keeps today.
- **What a use of a name is read from.** A report spells a `Global` at a place it already holds. Not read: whether the lowering keeps, for each written path, the extent it was written at beside what it resolved to, or keeps the resolution alone.
- **Whether an answer is the last unit's or the buffer's.** The session's unit is one edit behind while a check runs. An answer read from it is immediate and may be stale; one that waits is behind the compile the design keeps requests out of.
- **What the type at a position costs.** The elaborator keeps a type for a goal it reports and for nothing else it has finished. Keeping one for every extent is a table as large as the declaration; asking again with a goal in the term's place is a check a question.
- **Whether a goal becomes something an editor acts on**: a fit applied as an edit, a split written out. Each is a code action over what a goal's report already holds.

## Completion and retirement

Refined, this file states each answer's query, what it reads and its check; landed, `documentation/usage.md` lists the queries, `curios-wonder`'s rustdoc their contracts, and the line in `documentation/roadmap.md` becomes a checked summary.
