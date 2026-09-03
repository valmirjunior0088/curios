# curios-abi

The host/guest wire contract shared by the compiler and both runtimes: the numeric wire codes for `/sys/Handle`'s status, poll-event, open-mode, file-kind, stdio-wiring and stdio-handle tags, the `ForeignStore` of self-describing `ForeignFunction` rows every host operation is, the `HostOps` trait a host implements, and the two import namespaces both ends link on. Every consumer — the compiler minting the `/sys` prelude and emitting the wasm imports, the native runtime typing and binding the `sys.*` imports, the browser harness answering with the codes — reads these definitions rather than restating them. What each type means, how a row is read, and where the crate sits in the layering belong to the crate rustdoc; that a host operation is complete only when its row, its compiler use and both runtime implementations agree is CLAUDE.md's invariant.

## Design

### The builtin operations are authored once and projected

**Decision.** `host_ops!` in `host/ops.rs` is the one place a builtin host operation is written — its wire name, its `/sys` placement, its operands and results as slot kinds — and it is an X-macro: invoked with a callback, it applies that callback to the whole table. Two projections come off it, the `host_ops()` wire store and the typed `HostOps` trait. The native adapter's codec bindings are hand-written against that pair and cross-checked three ways: each `define` name must be a real store row (asserted), every method call must match the trait (compiler-checked), and no row may go unbound (asserted when the bindings are built).

**Rationale.** A store and a trait written separately are two spellings of one contract, and the wasm import a row names is the identity both ends link on — a mismatch strands an import silently until a program reaches it. One authored list with derived projections cannot drift between them, and the hand-written third leg cannot drift from either without an assertion or the compiler saying so.

**Rejected.** Generating the bindings too. They marshal wasmtime values, which cannot live in this leaf, so they stay hand-written and the cross-checks stand in for generation.

### `exit` is not a row

**Decision.** `exit` traps rather than returns, so no result row describes it; it stays a hardcoded intrinsic outside both the store and the trait, and only its import name lives here, as `EXIT`. Its guest declaration and type are `curios-text`'s prelude's.

**Rationale.** A row exists to describe what comes back, and a call that never returns has nothing for a `WireSignature` to say — a row would let the store promise a result the runtime cannot deliver. The name still lives here because it is wire: stamped by the emitter and matched by the runtime linker, and a wire string spelled once at each end is exactly the drift this crate exists to remove.

### The wire vocabulary is a closed subset of guest types, and lists do not nest

**Decision.** `WireType` is `Nat`, `Int`, `Bool`, `Bytes`, `Handle` and `List` of a `WireLeaf` — the same vocabulary minus `List` itself — so `List(List(_))` is unrepresentable rather than merely unchecked. Nothing below the type distinguishes `Bytes` from `Handle`: they share a wasm `ValType`, a wasmtime `FuncType` slot and a load/force/embed path, and only the guest type built from them differs.

**Rationale.** Codegen's host-boundary force and embed steps handle exactly one level of nesting, and the runtime's uniform `List` load cannot distinguish layers, so a second level would silently hand the host rope structs where flat arrays belong. Making the shape unwritable is cheaper than checking for it in each of three consumers.

### A reference result is the last, and the type holds it

**Decision.** `WireResults` is a list of scalar results and at most one reference result, which crosses last; `WireSignature` carries that rather than a plain list. The table's projection spells the same rule: a row with a reference anywhere but its final result slot does not expand.

**Rationale.** Codegen embeds only the final result back into a rope, because an earlier reference would sit under later stack values and need juggling through locals, and the runtime lowers references on the same assumption. That rested on a debug assertion at the one call site, which a new row would meet only when a program first called it — in a release build, as a module that fails wasm validation, naming the emitter. The rule belongs to the table, and a type that cannot hold the wrong shape is the cheapest place to keep it.

**Rejected.** A test over `host_ops()` beside the table. It pins the builtin rows and nothing else, and a user `foreign` declaration's single result was already well-formed by construction, so the test would have guarded exactly the rows a type guards better.

### A row's identity is its import pair, and the namespace is an enum

**Decision.** A `ForeignFunction` compares and hashes by `(namespace, name)` alone, and `Namespace` is a two-variant enum rather than a `&'static str`.

**Rationale.** A store never holds two functions with one name, so the pair determines the whole row; comparing by it keeps term-level equality and hashing O(1), and lets a cached prelude row match a freshly minted one with the same content. The enum makes the namespaces that exist exactly the namespaces that can be written, and it archives as its own discriminant — the byte a hand-rolled code table used to assign, beside a panic asserting a validity the string type could not give it.
