# curios-core

The Curios core language: elaboration, typing, reduction, conversion, inductives, and erasure. This is the heart of the type checker — where dependent types, propositions, and indexed inductives are given their meaning.

Sits between [`curios-text`](../curios-text) (which lowers surface syntax into core via `text/to_core`) and [`curios-ersd`](../curios-ersd) (which it erases into via `core/erase`). Code dependencies run opposite to data flow: this crate depends on `curios-ersd`, not the other way around.

See [AGENTS.md](../AGENTS.md#where-things-live) for how this crate fits into the pipeline. API docs: `cargo doc --package curios-core --open`.
