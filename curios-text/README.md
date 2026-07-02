# curios-text

The curios surface syntax: lexer/parser, lowering to core (`to_core/`), plus the embedded standard library (`std/`, `syn/`) baked into the compiler at build time.

The entry point of the compile pipeline — the first stage to see `.crs` source. Depends on [`curios-core`](../curios-core) (its `to_core` lowering constructs core terms); code dependencies run opposite to data flow.

See [SYNTAX.md](../SYNTAX.md) for the language reference and [AGENTS.md](../AGENTS.md#where-things-live) for how this crate fits into the pipeline. API docs: `cargo doc --package curios-text --open`.
