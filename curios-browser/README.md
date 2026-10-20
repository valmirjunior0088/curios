# curios-browser

The Curios browser host: `harness.mjs`, which runs a compiled program under a JavaScript engine against the browser's answers to the host rows and an embedder's hooks; the bridge module it reads the program's values through; what it is configured with, rendered from `curios-abi`; and the page a browser build of a program is, which `curios compile --target browser` writes. Its wasm-bindgen `run` reaches the playground through the bundle `curios-js` is built into. How a page is laid out is [A browser page is closed and laid out as its packages](../documentation/design/tools/a-browser-page-is-closed-and-laid-out-as-its-packages.md); build steps belong to `xtask`.

## Design

### A compiled program carries its foreign rows, and a hook is held to them

**Decision.** `curios-js`'s `compile` returns `{ program, foreigns }`: the module bytes and every `foreign` row the program reaches as plain data, the standard library's among them, each type spelled as its declaration spells it. `run` takes that object whole and answers each `ffi` import the module declares: through a checked adapter over the hook `hooks.foreign` names for it where there is one, and by the harness's own host otherwise, which answers the rows the standard library declares under the names `curios-abi` gives them. A hook therefore answers a row ahead of the host, as a module does natively. Operands are decoded into copies the hook owns — a `BigInt` for a `Nat` or `Int`, a boolean for a `Bool`, a number for a `Byte` or `Flt`, a `Uint8Array` for a `Bytes` or `Handle`, an array of those for a `List` — and the answer is held strictly to the row's results and copied in: nothing, the one value, or an object keyed by exactly the results' labels. A row that does not return is described as one, and its hook ends the program by throwing: one that comes back is stopped by the row's name, since the guest holds no code for a return from it. An import the rows do not describe, a called row neither a hook nor the host answers, and a hook naming no row each stop the run by name; a declaration the program never calls is never imported.

**Rationale.** A native binding is typed by its row because the bundle carries its `ForeignStore`, and the browser's hooks answer to the same rows only if the rows travel with the program. A raw Wasm-reference hook can read none of the guest's arrays and hand back any value, and a checked path beside an unchecked one makes the unchecked one the easy one. Strictness keeps a disagreement visible: a `Number` accepted for a `Nat` passes until the first value past 2⁵³. Labels are part of a tuple type's identity, so an object keyed by them is the result as the program reads it, and copying both ways leaves no hook holding a live alias to a guest value. One list of rows, the library's with the program's own, is what lets an embedder replace a host operation by the door it already uses for its own.

**Rejected.** Raw hooks beside checked ones; the rows in a custom section of the module, a second encoding for one consumer; converting close relatives, a safe-integer `Number` for a `Nat` or `0` and `1` for a `Bool`; an array in row order for several results, the multi-value shape rather than the row's; requiring every declared row to be imported; leaving the standard library's rows out of what `compile` returns, which would give the host's operations a door of their own.

### A handle is keyed by its exact token

**Decision.** The harness keys a handle on the hex of its token's exact bytes, the standard streams' tokens arriving from `curios-abi` in the same spelling, and never decodes a token into a number.

**Rationale.** A number decoded from a token's little-endian bytes reads the empty token as stdin's `[0]` and a padded `[1, 0]` as stdout's `[1]`, and loses a token longer than a `Number` holds, so a handle no host minted could reach a standard stream. The native hosts compare the bytes too.

### The harness is configured once, as JSON

**Decision.** What the harness is configured with that no program changes — the status codes, the poll masks, the standard streams' tokens, the first token it may mint, the deadlock report, the name each host row is declared under — is `configuration`, and a program's rows are `rows`, each rendered once, as JSON, from `curios-abi` and the program's `ForeignStore`. `run` parses the same text, and a status code is a decimal string the harness reads as a `BigInt`.

**Rationale.** One rendering is one statement of each table: a live JavaScript object built in Rust beside a text a page carries would be two, free to drift. JSON carries no integer past 2⁵³ exactly, and a code is a `Nat`, so the codes cross as strings; the masks and the seed are small numbers and cross as themselves.

**Rejected.** The configuration built with `js-sys` for the playground and written out as text for a page, two renderings of one table; a constants file copied by hand, which is how a harness drifts from the compiler; the code sets the harness answers no row with, which it never reads.

### A page's loader is rendered, and carries the harness whole

**Decision.** `page` answers a program's page as data: `index.html` and `index.js` rendered by askama from `templates/`, and `index.wasm` the module it is handed. The loader carries `harness.mjs` whole, the configuration and the rows as `configuration` and `rows` render them, the bridge in base64, and an import of every gateway, and calls its own `start` where it stands in a document; a test imports it and calls `start` with the program's bytes. The templates render JavaScript and are none, so `biome.json` lists no directory holding them, and `src/page.test.mjs` runs what they render: `cargo xtask compiler test` has the compiler as it is released write each fixture's page and starts its loader under Node.

**Rationale.** The page is three files the toolchain owns, so the harness travels inside the loader rather than beside it, and each table is the one rendering the harness is configured from. A page is what `compile` writes, not what this crate's function returns, so its suite asks the compiler that writes it.

**Rejected.** The harness as a fourth file of the page; the loader's logic moved into `harness.mjs`, which puts a page's concerns in the playground's harness for a linter's sake; a page suite through this crate's own function, which a change to `compile`'s side of the page would pass.

### The harness is tested under Node

**Decision.** `cargo xtask bundle test` builds the bundle `curios-js` is built into and runs `src/harness.test.mjs`, beside the harness, with Node's built-in test runner against what was filed: fixture programs compiled by the bundle's own `compile` and run by its `run` against scripted hooks. The suite imports only Node's own modules and the bundle.

**Rationale.** The harness is JavaScript that no Rust test executes — `src/tests.rs` pins the wire names it spells and what it is configured with, not what it answers. Node runs the bundle on V8, one of the playground's engines, and its runner needs no package, so the step has no lock file.

**Rejected.** A headless browser, a download and a driver for what the same engine already runs; a test framework from npm, an install for what `node:test` does.
