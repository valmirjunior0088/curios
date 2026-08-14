# Design

This document owns only the cross-cutting Curios design decisions — those that span the language or several crates: what was decided, why, and what was rejected. An entry is amended only when its decision is superseded. A decision scoped to one crate is documented in that crate's `README.md`, not here. What the language *is* belongs to [syntax.md](syntax.md), what exists or is pending belongs to [roadmap.md](roadmap.md), and local architecture belongs to crate and module rustdoc — link there, do not restate here.

## Objectives

Curios is a small, fully dependent language: types depend on values, proofs live beside ordinary code, and one pure pipeline compiles everything to WebAssembly-GC, serving a native product and a browser product from the same backend. The long-term objective is a self-hosting compiler — every language-specific stage from source text to raw Wasm bytes written in Curios — running on the retained Rust host.

Curios aims to be an ergonomic proof assistant and an ergonomic functional programming language at once, and the totality obligations are what let one language be both: general recursion stays unrestricted wherever a program uses it, and is removed from exactly the positions where erasure would turn it into a logical hole. The consistency claim rests on that line, and [Totality of the erased program](design/language/totality-of-the-erased-program.md) states both the claim and what enforces it. Reduction is not strongly normalizing and values are not canonical — a program may diverge, while a proof may not.

An independent kernel re-checks every compilation from the finished terms alone, on the compile path in production; see [An independent kernel re-checks what the elaborator accepts](design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md). The term language has native inductive families, structures, an implicit universe hierarchy, and an intrinsic roster with folds, so that kernel is thousands of lines rather than hundreds, and what keeps it reviewable is that its trusted base is a crate boundary: `cargo tree -p curios-cert -e normal` enumerates it, and the compiler enforces it.

## Where the decisions are

One decision per file, under [design/](design): [`language/`](design/language) for what Curios is as a language, [`toolchain/`](design/toolchain) for how it is built and run. Listing the directory is how you find a decision, which is why each filename spells its heading out rather than abbreviating it — an index maintained by hand goes stale silently, and a directory cannot.

A decision states what was **decided**, the **rationale** behind it, and what was **rejected** — the alternatives that were considered and lost, so a later reader can tell a settled question from an unasked one. Rejections earn their place: several here record something that was tried, shipped, and found wrong, and the account of how it was found is the part worth keeping.

Cite a decision by its path, so a moved or renamed one fails loudly instead of leaving a quoted title that no longer exists.
