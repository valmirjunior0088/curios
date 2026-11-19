# The places a declaration is written are answered, and renamed

**Not refined yet.** Every place a declaration is written across a package and every unit that imports it is answered, and a rename edits each, the edit handed back as text the formatter lays out. Today a place answers the declaration written there ([The editor is answered at a position](101-the-editor-is-answered-at-a-position.md)), and no query answers the places a declaration is written at, within a package or across its importers.

## What is known

- **A declaration's name is told from another of the same spelling by what it resolves to**, as `wonder --at` tells `/std/Bool/not` from `/std/Bytes/not` a place at a time, off the names each check reads (`Named`, `curios-wonder/src/named.rs`).
- **A place answers an occurrence**, recorded where the compilation resolved it, by tools/101: names a term holds now, a binder at its stage 3, a name the elaborator resolves at its stage 5, a path's segments and a `use` line's names at its stage 6. A rename reads every one of them: a name on a `use` line renamed, or a local's every use.

## What it waits on

- **Whether a unit's occurrences are stored with it**, a facet read without the unit, so a query across the units that import a declaration reads no unit whole. Lean writes `.ilean` files, "All references of this module" and "All declarations of this module", and "The current references in a file worker take precedence over those in .ilean files" ([`References.lean`](https://github.com/leanprover/lean4/blob/master/src/Lean/Server/References.lean)); hiedb indexes HIE files and answers `point-refs` of a place ([HieDb](https://github.com/wz1000/HieDb)).
- **What `curios-wonder`'s README states the engine is** once it writes a file as well as a store.
- **tools/101's stages 3, 5 and 6**, for the occurrences a rename edits beyond the names a term holds.

## Prior art

- **A rename is resolution read backwards.** rust-analyzer renames an item with every use from its own resolution, and its structural search and replace matches a pattern's paths "if they resolve to the same item, even if they're written differently" ([`ide_ssr`](https://rust-lang.github.io/rust-analyzer/ide_ssr/index.html)); [Comby](https://comby.dev/docs/basic-usage) matches text within balanced delimiters and [Semgrep](https://semgrep.dev/docs/writing-rules/glossary) binds metavariables over a parsed pattern, neither by what a name resolves to. A structural search and replace is left out, no consumer asking for one.

## Verification and retirement

Check, once refined: a `/std` declaration renamed with its importers, a local renamed at its binding and its uses, and one of two declarations sharing a name renamed alone, through both transports. Once it lands, `documentation/usage.md` lists the query and the server's request, and this file is deleted.
