# Curios is coloured from one class set

**The benefit.** Every place Curios is coloured — the editors' grammars, the documentation, the website, the playground — colours it from one statement of the language's words and one set of classes. A word the language adds reaches every one of them at once, a page colours code with the grammar and the classes of the release it pins rather than a table it keeps by hand, and the documentation colours a signature's names by what each is. The project has decided to adopt it, and its first stage brings the tree back under a rule it breaks: a value is "computed from the source constant, imported by name, never restated" (`.claude/rules/rust.md`), and the language's words are restated six times.

The next step is stage 1, a delivery; stage 2 is independent of it and as ready, and stages 3 and 4 follow stage 2.

## What is missing

**The vocabulary is restated six times, and nothing holds the statements together.** The surface parser writes fifteen words, the nine `parse_keyword` matches and the item heads `top_level.rs` dispatches on (`curios-surface/src/parse/`); `KEYWORDS` in `curios-common/src/qualifier.rs` reserves eleven of them, the four left being contextual — `concept`, `satisfy` and `test` at an item's head, `and` between a group's members. The two TextMate grammars (`editors/vscode/syntaxes/`) state theirs in patterns of their own, Zed's two query files (`editors/zed/languages/*/highlights.scm`) capture thirteen as keywords, and `KEYWORDS` in `curios-document/src/pages/context.rs` sets nine apart in signatures. `editors/grammar`'s `npm test` holds the queries to the tree-sitter grammars, and the TextMate snapshots hold three sample files; nothing holds any statement to the parser's, so a word the language adds reaches whichever list its author remembers.

**A release ships no grammar.** The release workflow (`.github/workflows/release.yml`) attaches the compilers, `curios-js.tar.gz`, `curios-docs.tar.gz`, `checksums.txt` and `install.sh`. The website colours Curios with this tree's TextMate grammar through Shiki, read from the pinned tag's source, and maps the grammar's scopes to its classes by a table of its own (`CLASSES` in `src/lib/highlight.ts` in the site's repository): the grammar a page renders with is no published contract of the release it pins, and a scope a later grammar renames falls out of the page's palette unseen.

**The documentation colours a signature by a list.** It sets apart the words of its own list of nine and links every name alike, whatever the name is (`segments`, `curios-document/src/pages/context.rs`).

What a name is — a type, a constructor, a concept, a module — no grammar can tell, and an editor learns it from the compiler: [The editor is answered at a position](300-the-editor-is-answered-at-a-position.md) answers it as semantic tokens, in the classes this specification states.

## What this builds on

- **A tree-sitter grammar that parses everything the tree compiles.** `editors/grammar`'s `npm test` parses every `.crs` of `/std` and `programs/` and every `.crsc` of the board, and compiles each query file over them, in the gate (`grammar`), so a host running the tree-sitter grammars colours the syntax of a compiling file exactly; the TextMate grammars, regular expressions over lines, approximate it.
- **A signature's names carry what they resolved to.** `segments` cuts a printed signature at the marks the printer leaves, each holding the declaration its name resolved to (`mark.referent`), so the documentation knows what every name of a signature is without asking anything new of the compiler.
- **Report roles.** `curios-common/src/style.rs` names the roles a report is painted in, each mapped once to a terminal's attribute and read back into runs for a page (`Role`, `runs`): the shape a class takes here, one statement mapped once to each consumer's encoding.

## What the peers do

A grammar is the layer every text gets: Shiki colours code on a page with TextMate grammars, as this tree's website already does, and an editor runs its grammar on every keystroke. What a name is comes from the compiler in an editor — VS Code applies LSP semantic tokens "on top of the highlighting from grammars" ([semantic highlight guide](https://code.visualstudio.com/api/language-extensions/semantic-highlight-guide)), Zed lays them over tree-sitter in its `combined` mode, off by default ([semantic tokens](https://zed.dev/docs/semantic-tokens)), Lean's server makes them from syntax and info trees ([`SemanticHighlighting`](https://lean-lang.org/doc/api/Lean/Server/FileWorker/SemanticHighlighting.html)) — which is [The editor is answered at a position](300-the-editor-is-answered-at-a-position.md)'s. A static page that colours names by the compiler takes what the compiler exported for it: Agda's HTML backend reads a module's interface ([`Highlighting.HTML`](https://hackage.haskell.org/package/Agda-2.6.3/docs/src/Agda.Interaction.Highlighting.HTML.Base.html)), Haddock's hyperlinker GHC's `.hie` files ([`Hyperlinker`](https://hackage.haskell.org/package/haddock-api-2.24.2/docs/src/Haddock.Backends.Hyperlinker.html)), and Twoslash compiles each code block at a page's build ([Shiki](https://shiki.style/packages/vitepress)).

What is taken: the grammar as every page's layer, read from the release, and one class set every consumer maps. What is set aside: a page asking the compiler of each snippet, Twoslash's way, which needs a whole program and a compile at every build for names coloured exactly, a nicety no page asks for, and which a fragment — most of a cheatsheet — cannot have at all.

## Settled

- **One vocabulary, stated in `curios-common`.** The reserved words and each contextual word with the position it is a word in, beside `KEYWORDS`, which it replaces; the parser's keyword matches, `is_keyword` and the documentation read it, and a test holds both TextMate grammars and both query files to it.
- **One class set, stated beside it.** The lexical classes a grammar gives, each mapped to a TextMate scope and a tree-sitter capture: keyword, comment, documentation comment, string, character, number, escape, sort — `Type` and the other sorts, which are words — operator, punctuation and goal. The name classes, which the documentation colours a signature's names in and an editor's semantic tokens are answered in, each mapped as well to an LSP standard token type:

  | Class | What it names | LSP type |
  | --- | --- | --- |
  | module | a module, in a path or a `use` line | `namespace` |
  | type | an inductive or a structure | `type` |
  | constructor | an inductive's constructor | `enumMember` |
  | concept | a concept | `interface` |
  | function | a declaration whose type is a function's, a test among them | `function` |
  | value | any other declaration | `variable`, with `readonly` |
  | field | a structure's field, written or projected | `property` |
  | parameter | a function's or a lambda's binder | `parameter` |
  | type parameter | a binder whose type is a sort | `typeParameter` |
  | local | a `let`'s binder | `variable` |
  | intrinsic | an operation of `/sys` the compiler states | `function`, with `defaultLibrary` |

  Two modifiers stand beside a name class: `declaration` where the name is declared, and `defaultLibrary` where it resolved into `/std` or `/sys`. A witness has no class: a `satisfy` writes no name, its name being the compiler's ([A witness is named by what it witnesses](../../design/compilation/a-witness-is-named-by-what-it-witnesses.md)). A consumer maps a class to its palette, as a report's role is mapped. A grammar is held to the vocabulary and to the lexical mappings; what it makes of a name — a capitalised one taken for a type — is a guess an editor's semantic tokens replace, and no test holds it to the compiler's classes.
- **A page colours with what its release ships.** A release attaches one archive beside `curios-js.tar.gz` holding both TextMate grammars, each tree-sitter grammar's sources with its queries, and the class set's mapping as data — each lexical class with the TextMate scopes and tree-sitter captures it covers, each name class with its LSP type — so a page reads the grammar and the table it maps the grammar's scopes by from the tag it pins. Rejected: classified spans a page asks the compiler for, from the bundle or from a `wonder` query, which no page reads; and, until a consumer asks, the tree-sitter grammars built to WebAssembly, which would put that toolchain in the release workflow for no reader — the site renders with Shiki and the TextMate grammar, and an editor builds tree-sitter from its sources.
- **The documentation colours a signature from the set.** A word of the vocabulary as a keyword, and each name by the declaration its mark resolved to — a type, a constructor, a concept, a function, a value — its list of nine gone.

## Stages

1. **One vocabulary.** The statement in `curios-common`, the parser, `is_keyword` and the documentation reading it, and a test reading the two TextMate grammars and the two query files against it, each difference the test states being a context it names. Check: the test, a word taken out of one statement failing it, and the parser's and the lowering's suites unchanged.
2. **One class set.** The statement and its mappings, and tests holding each TextMate scope and each query capture the grammars write to a lexical class. Check: the tests, and a scope or capture no class maps failing them.
3. **The grammars and the classes ship.** The release workflow builds the archive, the mapping written from the class set's statement, and attaches it. Check: the mapping's form held by a test against the statement, the workflow on a tag in a fork, and the archive's listing.
4. **The documentation's signatures.** Coloured from the class set, each name by its mark's referent. Check: `curios-document`'s suites, a signature naming a type, a constructor, a concept and a function rendering each in its class.

Each stage's contract lands in its owner: `curios-common`'s documentation states the vocabulary, the class set and the mapping's form, the README's installation section names the archive, and a decision under `documentation/design/tools/` states the one class set and where each consumer reads it, with what it rejected.

## Retirement

Once the four stages have landed, `curios-common`, the README and the decision state the vocabulary, the class set and the archive, and nothing references this specification, it is deleted.
