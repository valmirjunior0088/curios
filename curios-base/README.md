# curios-base

Foundational utilities shared across every curios pipeline stage: source spans, the fresh-name `Entropy`/`Mint` supply, the `name!` newtype macro, the parser/printer monad combinators, and the slice `suffix_view` re-base laws.

The bottom of the dependency graph — every other crate in the workspace depends on it (directly or transitively), and it depends on nothing in-repo.

See [AGENTS.md](../AGENTS.md#where-things-live) for how this crate fits into the pipeline. API docs: `cargo doc --package curios-base --open`.
