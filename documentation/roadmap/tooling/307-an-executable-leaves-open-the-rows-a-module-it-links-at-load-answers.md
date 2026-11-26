# An executable leaves open the rows a module it links at load answers

**Not refined yet.** No product leaves a `foreign` row for a later link, and the consumer is dynamic linking (the user, 2026-11-23): an executable that links a module when it starts and leaves that module's rows open for it. It waits on the first stage's decision.

## What is missing today

- **Every product closes its rows where it is made.** A `foreign` declaration compiles to a typed import nothing in compilation answers — `wonder --stage wasm` holds `(import "ffi" "/frob" …)` for a row nothing answers — but `run` and a built executable link what a manifest's `[foreign]` rows deliver and refuse any other row ("no implementation", `curios/src/link.rs`), a built executable carries every module it links ([A foreign row states a delivery for each target](../../design/tooling/a-foreign-row-states-a-delivery-for-each-target.md)), and a page's loader takes no embedder's hooks ([A browser page is closed and laid out as its packages](../../design/tooling/a-browser-page-is-closed-and-laid-out-as-its-packages.md)).
- **Only the library doors leave a row open**: `curios-native`'s `ForeignBindings` and the browser harness's `run` hooks, for whoever embeds the runtime.

## Goal

An executable built with named rows left open links, when it starts, a module answering them, each answer held to its row's declared types by the check every link makes (`plugin_bindings`, `curios-native/src/plugin.rs`) before the program runs.

## Settled

- A product states the rows it leaves open, typed by the rows it carries, and the later link holds each answer to them, as the Component Model keeps the imports a composition's dependencies do not satisfy as imports of the composed component ([composing](https://component-model.bytecodealliance.org/composing-and-distributing/composing.html)) and wasm-ld lets only the symbols `--allow-undefined-file` names stay undefined ([lld](https://lld.llvm.org/_sources/WebAssembly.rst.txt)).
- Never a blanket: macOS's `-undefined dynamic_lookup` ([CPython #97524](https://github.com/python/cpython/issues/97524)) and Emscripten's `ERROR_ON_UNDEFINED_SYMBOLS=0` ([#21104](https://github.com/emscripten-core/emscripten/issues/21104)) move every refusal to the first call.

## Open

- Where the rows left open are stated: the manifest, the command line, or the product.
- Where the module comes from at load: a path the executable is handed, a manifest beside it, or the cabinet.
- What the two decisions linked above become: whether a row left open is a delivery of its own, and whether a page takes an embedder's hooks by the same statement.

## Stages

1. **The statement and the load.** A design decision under `documentation/design/tooling/` answering the three open questions. Check: the decision filed, and this specification refined to it.
2. **A row left open.** A built executable carries the rows it leaves open and links a module at load against them, refusing before the program runs a module whose answer differs from a row's types, and naming a row it leaves unanswered. Check: an executable built with one row left open, run against a module answering it, and refused against one answering another type; each mutation-checked.

## Verification and retirement

Verified by stage 2's tests and `documentation/usage.md`'s rows for the statement; retired when both stages land, the two decisions restated.
