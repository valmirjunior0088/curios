# A library is documented for its consumers, from the compilation that builds it

**Decision.** `curios document` writes a package's library interface as static pages, and everything on a page is read off the compilation that builds the library. Which modules and declarations appear is the export view the text lowering resolves to a fixed point: a private declaration is absent, a re-export links to the declaration it names or, when that declaration's module has no page, documents it where the re-export puts it, a constructor or field appears exactly when the representation is public, and a test never appears. Signatures are the surface tree's, printed by `curios format`'s printers, with every name resolved through the lowering's visibility functions over its recorded import scopes — a referent inside the unit is a link, one outside its qualified name. Prose is syntax: `--- ` opens a documentation comment the parser attaches to the declaration, constructor, field, concept method or `mod` immediately below it, and a block before anything else is refused; a module's prose is its `mod`'s block, and a package's is its manifest's `description`. The text lowering builds the record, `curios-document` owns its types and renderer, and it is carried on the unit, so a stored unit renders as a fresh compilation does. The pages land under the store's `documentation/` family. The subject is a package's library, since a library is what has an interface; `--std` documents the standard library the compiler embeds, off the prelude every compilation starts from, into a directory it is given.

**Rationale.**

- **The compiler already computes every fact a page needs.** Visibility is a subtree-scoped judgment the elaborator makes, so a heuristic would be wrong rather than imprecise; every modern generator reads the compiler's result — doc-gen4 the environment, odoc the typed interface, Swift a symbol graph, Elm the compiler's JSON.
- **The surface signature**, because the elaborated print spells `Nat` as `/sys/Nat/Nat` under universe variables.
- **A distinguished comment token the parser attaches**, as Rust, Lean, Elm, Idris, Swift, Haskell and OCaml do, so a reformat cannot break the association. `--- ` triples the line-comment opener as `///` does in Rust, C#, Swift and Zig and `---` in Lua, and spends no `|`, which opens the constructor a block so often sits above.
- **Pre-rendered pages readable from `file://`**, where a renderer over data pays with a serving requirement; **a record before a renderer**, as Elm, Gleam, Swift and EEP-48 serve a site, an editor and a registry from one artifact.
- **Only a package**, because a product on disk needs an owner to be filed under and a name to be filed as.

**Rejected.**

- **Association by a plain comment's adjacency**, which a blank line or a reformat silently changes, and which publishes every implementation note.
- **Documentation as typed terms**, Unison's design, a language campaign; **rendering the elaborated module**, which spells nothing as the author wrote it.
- **A `wonder document` query that writes files**: a query answers on stdout and leaves no product on disk.
- **A second marker for module prose**, Rust's `//!`; **positional rules for a block at the top of a file, or `---` after code on a line**, each readable differently after a reformat; **Haskell's `-- |` or `--|`**, whose bar stacks on the constructors' bars.
- **Loose files and standard input as subjects**, which have no consumer and nowhere to be filed; **an archived unit named by path**, when a store slot's library is documented by `document` itself and the prelude image is the one the compiler carries.
- **The dependency closure documented as one bundle**, deferred with an audience parameter, since the record already carries each referent's identity.
