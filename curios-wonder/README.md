# curios-wonder

What the compiler knows about a program, handed out as records: the `wonder` engine, which runs the compiler as far as a question needs and reads the answer off what it already decided, and the two transports that ask it — the command line (`ask`) and the language server (`server`) — with the `lint` gate that turns one of those answers into an exit code. What each query means at the command line belongs to [usage.md](../documentation/usage.md)'s Asking about a program and Linting; what a record carries, and how a transport converts it, belongs to the crate rustdoc.

## Design

### Under the native compiler, and free of what it links

**Decision.** This crate depends on the pipeline, the package crate and `curios-verdicts`, and on nothing that links a back end: `cargo tree -p curios-wonder --edges normal` contains neither `curios-binaryen` nor `curios-runtime`. The one rung the driver cannot render, `wasm-optm`, is handed back as the emitted module for the transport that owns Binaryen to finish, through the `finish` argument of `wonder_stage`.

**Rationale.** A question costs the compiler and nothing after it, so a crate answering questions has no use for an optimizer, a code generator or a launcher — and paying for all three on every build and test of the engine is what the module inside `curios` cost. Handing the last rung back rather than rendering it is what the engine already did, since `curios-js` can reach the emitted module and not Binaryen; making the transport take the renderer as an argument states the same fact at the crate boundary.

**Rejected.** Keeping the engine in `curios` until a browser editor needed it: the split was owed to the native build's test cycle before any second consumer existed.

### The engine names no transport's types, and no product's

**Decision.** Nothing in the engine reads a file by a name it was not handed, encodes JSON, or spells an LSP type; a record is plain data over the compiler's coordinates — a `Span` of source identity and UTF-8 byte range — and a transport converts at its own edge. UTF-16 exists only in the server's adapter.

**Rationale.** Every rendering is computed from the record rather than from the compiler beside it, which is what keeps the two transports honest with each other: the command line reads `wonder diagnostics` exactly as `curios run` would have reported the same program, because it is the same `Report` rendered.

### A query never writes the store

**Decision.** Dependencies come from the store already built, and one that is not is compiled in memory and forgotten. The engine wraps the store it is handed in one that files nothing — while still placing every unit the fold compiles, through `Verdicts::place`, because a slot is addressed after the units before it and a chain with a gap in it misses for the whole tail.

**Rationale.** The store addresses a unit by content, and a server that filed what it checked would file a unit per keystroke. Placing without filing is why the engine holds `curios-verdicts`'s store rather than a `dyn Cache`: the trait has no way to say the first without the second.
