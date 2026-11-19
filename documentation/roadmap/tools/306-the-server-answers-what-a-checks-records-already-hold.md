# The server answers what a check's records already hold

**Not refined yet.** A check's record of a file holds more than the server answers, which is formatting, definition, hover and a document's symbols beside its diagnostics (`curios-wonder/src/server.rs`; [`wonder`](../../usage.md#wonder)'s `--server` row). Each answer below is a delivery of its own: a handler and a test over the records as they stand, nothing compiled at the request, released by a consumer that asks for it and waiting on no other.

## What is known

- **Every term's range, nested** (`Written::terms`, `curios-wonder/src/named.rs`, a term before the ones it holds), which `textDocument/selectionRange` expands a selection along.
- **The type each written binder was opened at** (`Written::binders`), which `textDocument/inlayHint` writes beside a binder written without one. Open: whether the hint is the type spelled where the binder is written, as `Written::binders` spells it for a reader standing in the text.
- **The witnesses in scope a concept or a type takes part in** (`Described::witnesses`), which `textDocument/implementation` answers.
- **Where a `mod` line declares each module a text names** (`Placed::modules`), which `textDocument/documentLink` links a `mod` or a `use` line by.
- **Each text's range** (`Written::text`), which `textDocument/foldingRange` folds, and **the fold's progress**, which `wonder` narrates and `$/progress` would report while a check runs.
- **The `use` line a bare name came in through.** The lowering knows it where it resolves the name (`Context::note_binding_use`, `curios-module/src/into_core/context.rs`) and keeps none of it with the declaration, so one record beside what the lowering mints and one fact in a place's answer would say it, as Agda's "why in scope" does; a name's exposed path already says which module it came from.

## What it waits on

- **A consumer for each**: a reader in an editor asking for it.
- **Beyond these**: a quick fix (`textDocument/codeAction`) needs a lint's way out as an edit, where it is prose; `textDocument/signatureHelp` and the workspace's symbols need records no check keeps. Semantic tokens, completion and the places a declaration is written are [303](303-the-server-lays-the-compilers-classes-over-the-grammar.md)'s, [304](304-a-path-being-typed-is-completed-from-what-its-module-exposes.md)'s and [305](305-the-places-a-declaration-is-written-are-answered-and-renamed.md)'s.

## Prior art

- rust-analyzer answers a selection's expansion, folding ranges and inlay hints off its syntax tree and its inferred types, each a request of its own ([`ide` crate](https://github.com/rust-lang/rust-analyzer/tree/master/crates/ide/src)); Lean's server reports a file's progress through `$/lean/fileProgress` as its elaboration runs; Agda's interaction answers "why in scope" for a name with the `open` or `import` that brought it in.

## Verification and retirement

Each delivery: a scripted server session asking it, its answer read off the record and equal to what `--at` answers at the same place where `--at` answers one. Once each has landed or been dropped, the `--server` row of `documentation/usage.md` states what is answered, and this file is deleted.
