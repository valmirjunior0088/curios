# A program runs in a browser

`curios compile --target browser` writes a program's page, laid out as [A browser page is closed and laid out as its packages](../design/tools/a-browser-page-is-closed-and-laid-out-as-its-packages.md) states, and nothing serves it: a page loads its loader as a module script and fetches its program, which a browser does not do for a file opened from the disk, so seeing a page takes a server the author brings.

## What is missing

- **A way to look at the page.** `curios run` builds and runs natively alone (`curios/src/main.rs`, `Mode::Run`), and takes no `--target`.

## What is known

- **The page is data before it is files.** `curios_browser::page` answers every file of the page by its path, and `compile` writes them with `emit_page` (`curios/src/browser.rs`); a server can hold the same map in memory and write nothing.
- **The page's name is decided.** `named` (`curios/src/browser.rs`) gives the executable's name, a loose file's stem or `stdin`, which a server's path can carry.
- **A page path is a URL already.** The loader and the import map spell each path as `curios-browser`'s `relative` does, percent-encoding what a segment cannot carry, so a server decodes a request's path once to find the file.

## Settled decisions

- **Serving.** `run --target browser` compiles as `compile --target browser` does, reads every file once, and serves the page from memory on `127.0.0.1` with a server written over `std::net`: under `/<name>/`, with `/` and `/<name>` redirected there, GET alone, a `Host` other than the server's own refused, every reply `Cache-Control: no-store`; port 8000 unless `--port` says otherwise, refused where it is taken. It prints `Serving <url>` and serves until stopped. No browser is opened and nothing reloads.

## Stages

1. **`run --target browser`.** `--target` and `--port` on `Run`, `--port` only beside `--target browser`; `curios/src/serve.rs`, with a thread per connection, a request head read to its blank line under a size cap, `Content-Type` from one table in `curios-browser`, `Content-Length` and `Connection: close`; a `Serving` heading whose URL carries the program's arguments. Docs: usage.md's `run`, its exit codes and flags, and the sentence naming the commands that reach the network. Check: the server's suite over a loopback socket on port 0 — the page, `index.wasm`'s type, a 404, a 405, a refused `Host`, the redirects, `no-store` — and the contract tests.

## Verification and retirement

`cargo xtask workspace lint` and `cargo xtask workspace format` with the stage's check, and the full gate after it. By hand, a scratch package with a gateway that sets the page's text and answers a click by a promise, one `static` directory and one loose file is compiled with `--target browser`, served with `run --target browser` and seen in a browser, and the written directory served under a subpath by any static server behaves the same. The stage's commit deletes this file, and moves what [A browser delivery may be a WebAssembly module](runtime/301-a-browser-delivery-may-be-a-webassembly-module.md) waits on.
