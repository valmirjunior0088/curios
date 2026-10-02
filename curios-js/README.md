# curios-js

The Curios ↔ JavaScript boundary: wasm-bindgen exports of the pure compile pipeline plus the browser run harness. Build steps belong to `xtask`.

## Design

### Plain cargo plus the bindings generator as a library

**Decision.** The browser build is `cargo xtask js`: `cargo build` for wasm32, then `--target web` bindings generation. No `wasm-pack`, and no `wasm-opt`: Binaryen optimization belongs only to the native `curios` product.

**Rationale.** The build needs the compiler and the bindings generator, and a packager adds a second build system to version, cache and debug for no capability. Keeping Binaryen out keeps the browser artifact the pure pipeline's output, reproducible from the workspace toolchain alone.

### A compiled program carries its foreign rows, and a hook is held to them

**Decision.** `compile` returns `{ program, foreigns }`: the module bytes and the program's own `foreign` rows as plain data, each type spelled as its declaration spells it. `run` takes that object whole and implements each `ffi` import the module declares through a checked adapter over the hook `hooks.foreign` names for it. Operands are decoded into copies the hook owns — a `BigInt` for a `Nat` or `Int`, a boolean for a `Bool`, a number for a `Byte` or `Flt`, a `Uint8Array` for a `Bytes`, `Bits` or `Handle`, an array of those for a `List` — and the answer is held strictly to the row's results and copied in: nothing, the one value, or an object keyed by exactly the results' labels. An import the rows do not describe, a called row with no hook and a hook naming no row each stop the run by name; a declaration the program never calls is never imported.

**Rationale.** A native binding is typed by its row because the bundle carries its `ForeignStore`, and the browser's hooks answer to the same rows only if the rows travel with the program. A raw Wasm-reference hook can read none of the guest's arrays and hand back any value, and a checked path beside an unchecked one makes the unchecked one the easy one. Strictness keeps a disagreement visible: a `Number` accepted for a `Nat` passes until the first value past 2⁵³. Labels are part of a tuple type's identity, so an object keyed by them is the result as the program reads it, and copying both ways leaves no hook holding a live alias to a guest value.

**Rejected.** Raw hooks beside checked ones; the rows in a custom section of the module, a second encoding for one consumer; converting close relatives, a safe-integer `Number` for a `Nat` or `0` and `1` for a `Bool`; an array in row order for several results, the multi-value shape rather than the row's; requiring every declared row to be imported.

### A handle is keyed by its exact token

**Decision.** The harness keys a handle on the hex of its token's exact bytes, the standard streams' tokens arriving from `curios-abi` in the same spelling, and never decodes a token into a number.

**Rationale.** A number decoded from a token's little-endian bytes reads the empty token as stdin's `[0]` and a padded `[1, 0]` as stdout's `[1]`, and loses a token longer than a `Number` holds, so a handle no host minted could reach a standard stream. The native hosts compare the bytes too.

### The harness is tested under Node

**Decision.** `cargo xtask js-test` builds the bundle and runs `tests/*.test.mjs` with Node's built-in test runner against what was filed: fixture programs compiled by the bundle's own `compile` and run by its `run` against scripted hooks. The suite imports only Node's own modules and the bundle.

**Rationale.** The harness is JavaScript that no Rust test executes — `tests.rs` pins the wire names it spells, not what it answers. Node runs the bundle on V8, one of the playground's engines, and its runner needs no package, so the step has no lock file.

**Rejected.** A headless browser, a download and a driver for what the same engine already runs; a test framework from npm, an install for what `node:test` does.
