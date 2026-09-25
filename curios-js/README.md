# curios-js

The Curios ↔ JavaScript boundary: wasm-bindgen exports of the pure compile pipeline plus the browser run harness. Build steps belong to `xtask`.

## Design

### Plain cargo plus the bindings generator as a library

**Decision.** The browser build is `cargo x js`: `cargo build` for wasm32, then `--target web` bindings generation. No `wasm-pack`, and no `wasm-opt`: Binaryen optimization belongs only to the native `curios` product.

**Rationale.** The two tools the build actually needs are the compiler and the bindings generator; a packager on top adds a second build system to version, cache, and debug without adding a capability. Keeping Binaryen out preserves the crate-boundary ownership of optimization, so the browser artifact is the pure pipeline's output, reproducible from the workspace toolchain alone.

### A compiled program carries its foreign rows, and a hook is held to them

**Decision.** `compile` returns `{ program, foreigns }`: the module bytes, and the program's own `foreign` rows as plain data, each type spelled as its declaration spells it. `run` takes that object whole and implements each `ffi` import the module declares through a checked adapter over the hook `hooks.foreign` names for it. The operands are decoded into copies the hook owns — a `BigInt` for a `Nat` or `Int`, a boolean for a `Bool`, a number for a `Byte` or `Flt`, a `Uint8Array` for a `Bytes`, `Bits` or `Handle`, an array of those for a `List` — and the answer is held strictly to the row's results and copied in: nothing, the one value, or an object keyed by exactly the results' labels. An import the rows do not describe, a called row with no hook and a hook naming no row each stop the run by name; a declaration the program never calls is never imported and needs no hook, as on the native runtime.

**Rationale.** The native bundle carries its `ForeignStore` beside its payload, and a binding there is typed by its row; the browser's hooks answer to the same rows only if the rows travel with the program. A raw Wasm-reference hook can read none of the guest's arrays without the bridge and can hand back any value at all, and a checked path beside an unchecked one makes the unchecked one the easy one. Checking strictly keeps a disagreement between a program and its hook visible: a `Number` accepted for a `Nat` passes until the first value past 2⁵³, and a `1` accepted for `true` hides a hook written against another row. Labels are part of a tuple type's identity, so an object keyed by them is the result as the program reads it, and field order stops being something a hook can get wrong. Copying both ways leaves no hook holding a live alias to a guest value.

**Rejected.** Raw hooks beside checked ones. Carrying the rows in a custom section of the module, a second encoding of the same rows for one consumer. Converting close relatives — a safe-integer `Number` for a `Nat`, `0` or `1` for a `Bool`. An array in row order for several results, which is the wasm multi-value shape rather than the row's. Requiring every declared row to be imported, which refuses every program with a declaration it does not call.

### A handle is keyed by its exact token

**Decision.** The harness keys a handle on the hex of its token's exact bytes, the standard streams' tokens arriving from `curios-abi` in the same spelling, and never decodes a token into a number.

**Rationale.** A number decoded from a token's little-endian bytes reads the empty token as stdin's `[0]` and a padded `[1, 0]` as stdout's `[1]`, and loses a token longer than a `Number` holds, so a handle no host minted could reach a standard stream. The native hosts compare the bytes, and so does this one.

### The harness is tested under Node

**Decision.** `cargo x js-test` builds the bundle and runs `tests/*.test.mjs` with Node's built-in test runner against what was filed: fixture programs compiled by the bundle's own `compile` and run by its `run` against scripted hooks. The suite imports nothing but Node's own modules and the bundle.

**Rationale.** The harness is JavaScript, so no Rust test executes it: `tests.rs` pins the wire names it spells, not what it answers. Node runs the bundle on V8, one of the engines the playground runs under, and its runner needs no package, so the step has no lock file and no install beyond Node itself.

**Rejected.** A headless browser, a download and a driver for what the same engine already runs. A test framework from npm, an install step for what `node:test` does.
