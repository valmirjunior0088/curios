# A program runs in a browser

A program reaches a browser only through the playground, which compiles typed-in source with no package around it. A package cannot be compiled for a browser from the command line, and a `foreign` declaration cannot be answered by a JavaScript file, although [A host operation is a foreign declaration whose types are its whole contract](../design/runtime/a-host-operation-is-a-foreign-declaration-whose-types-are-its-whole-contract.md) already decides who answers a row where the program is linked, and the browser harness already holds a JavaScript function to a row.

## What is missing

- **A browser product.** `curios compile` writes a native executable alone (`curios/src/main.rs`, `Mode::Compile`): the module goes through Binaryen and then Cranelift (`curios::to_cwasm`), and the launcher deserializes what Cranelift wrote. Nothing writes the module Binaryen produced, or a page that runs it: `curios-browser` holds the harness, the bridge and what they are configured with, and has no page format yet.
- **A way to look at the page.** No command serves what a browser build would write.

## What is known

- **The harness is already the JavaScript link.** `harness.mjs`'s `checked` adapter decodes a row's operands into copies a hook owns, holds what the hook answers strictly to the row's results, and files a promise under a token the program suspends on (`laned`), aborting the hook's `AbortSignal` where the call is dropped. A `.js` delivery's exports, keyed by `[foreign.answers]`, are `hooks.foreign` filled from the manifest instead of by hand.
- **The browser's module is a rung that exists.** `curios::optimize` validates the module with the engine's validator, which compiles nothing, and runs Binaryen; what it returns is what `wonder stage wasm-optm` prints. Binaryen's feature set (`curios-binaryen/src/lib.rs`) lists nothing a module that declares no memory can carry past what Chrome 119, Firefox 121 and Safari 18.2 ship (GC and tail calls).
- **The payload store takes any engine key.** `Verdicts::payload_get` and `payload_put` (`curios-verdicts/src/payload.rs`) address a slot by executable, compiler and an engine string the caller hands in, so a browser payload filed under a key of its own evicts no native one.
- **The host's rows are one table.** `host_answers` (`curios-native/src/engine.rs`) asks `curios-abi`'s `HostOp::ALL` by declared name, and the harness answers every row of the same table, denying what a page cannot do.
- **The playground is a consumer.** The site repository's `public/curios/worker.js` imports `curios-js`'s `compile` and `run` from the release tarball, which packs `curios-js/.artifacts/wasm32-unknown-unknown/` whole.
- **A page waits without JSPI.** A hook that answers a promise already suspends the program on a token, so nothing here suspends the guest's stack.

## Settled decisions

- **Vocabulary.** `compile` and `run` take `--target native|browser`, native when omitted, beside the SUBJECT a command takes. `curios-native` holds the native host, and a new `curios-browser` holds the browser's: the harness, the bridge and the page's format.
- **The page is closed.** A browser build writes `index.html`, holding one `<pre>` a gateway may remove, `index.js`, the loader holding the harness, the code tables, the bridge, the rows and the gateways' imports, and `index.wasm`, the module Binaryen wrote. No package supplies any of the three, and none can be claimed: `curios-package`'s `PAGE_FILES` refuses them where the manifest is read, and the names the page writer gives its files are held to it by a test in `curios`, which links both.
- **The page's root is the package's root.** A gateway the package carries stands at its path from `curios.toml`, a fetched one at `<row>.js`, and a dependency's files under `<dep>/`. `[foreign.browser.static]` maps a page path, its key, to a package path, its value, both plain relative: a directory recursively, files starting with `.` inside it skipped, symlinks refused, nothing under `.curios/`, and every page path filled once, a second claim refused naming both. A row's gateway and static files are carried only where the program reaches one of the row's declarations. Nothing generated is root-absolute, so a page works under any path.
- **What the page checks.** The loader fetches `index.wasm` with its integrity and the import map states every `.js` file's, each the `sha256-` form of the file's SHA-256. The program runs on the main thread. Its arguments come from the URL's query, each `arg=` one argument's bytes percent-encoded, after the page's name.
- **The store.** A browser build is written under `.curios/browser/<package>/<executable>/`, beside `.curios/native/`; its `-o` names a directory.
- **Serving.** `run --target browser` compiles, reads every file once, and serves the page from memory on `127.0.0.1` with a server written over `std::net`: under `/<name>/` — the executable's name, a loose file's stem, `stdin` — with `/` and `/<name>` redirected there, GET alone, a `Host` other than the server's own refused, every reply `Cache-Control: no-store`; port 8000 unless `--port` says otherwise, refused where it is taken. It prints `Serving <url>` and serves until stopped.
- **Builds are the author's.** Curios runs no tool for a page: what a gateway imports is built before `curios compile --target browser` runs.

## Stages

1. **`compile --target browser`.** The module goes through Binaryen alone and is filed under a payload key of its own; a reached row neither the host nor a `[foreign.browser]` answers is refused, `host_answers` asked of `curios-abi`; `curios-browser` assembles the page as data, and `curios` writes it under `.curios/browser/` or where `-o` names; the harness answers `proc_args` with what the loader passes. Check: `curios-browser`'s page suite, and `compiler test`, which ensures `compiler build` and runs the released compiler over fixture pages under Node, with its job in `check.yml`.
2. **`run --target browser`.** `curios/src/serve.rs`, a `Serving` heading, and `--port`. Check: the server's suite over a loopback socket and the contract tests.

## Verification and retirement

`cargo xtask workspace lint` and `cargo xtask workspace format` after each stage, with its check, and the full gate after the last. By hand, a scratch package with a gateway that sets the page's text and answers a click by a promise, one `static` directory and one loose file is compiled with `--target browser`, served with `run --target browser` and seen in a browser, and the written directory served under a subpath by any static server behaves the same; a program reaching a row with only `[foreign.browser]` is refused natively, naming the table, and the reverse in the browser. The contracts move as each stage lands — to `usage.md`, the crates' READMEs, the design decisions on per-target deliveries and on the closed page, and the amended decision on host operations — and the last stage deletes this file.
