# A change on disk is answered without a gesture

A file a check reads can change on disk while no editor holds it — a `git checkout`, a rebase, another editor, a formatter run from a terminal — and the language server answers the change only once the editor sends something: a keystroke, which asks about the document it edits, or a save, which asks about every open document (`curios-wonder/src/server.rs`). Until then the editor shows what a check the disk has since moved under said, and a refusal published on a file no editor holds stays where it was published. This specification has the server ask the editor to watch the files a check reads, and answers a change as a save is answered.

It has no prerequisite in another specification. The next step is a ready delivery, the one stage below.

## What is missing

- **The server registers for no file event.** `serve` declares full-text sync, formatting, definition, hover and document symbols, and `Server::notified` handles `didOpen`, `didChange`, `didSave` and `didClose`; a `workspace/didChangeWatchedFiles` notification reaches its catch-all arm and is dropped, and the server asks no client to send one.
- **Which open document a changed file reaches is known to no state the analyst keeps.** A unit's read log names the files it read when it compiled, and only then: a refused unit is taken into no fold (`compile_units_reading`, `curios-pipeline/src/compile.rs`), a file looked for and missing is no read (`RootSource::reads`, `curios-text/src/root_source.rs`), and the manifests and `mod` chains a document's placement walks are read by `Selection` (`curios-package/src/selection.rs`) into no log at all.

## What this builds on

- **Every notification is answered by a check, and a save asks about every open document** (`curios-wonder/src/server.rs`'s module documentation). Whether anything moved is the fold's to decide, so a document nothing moved under is answered at what its reuse costs. Timed at `5ba96f872` with the debug build (opt-level 3), wall clock including the analyst's settle wait, median over seven runs with their span: asking again about a document whose text and disk are as the last check left them takes 0.7 ms [0.6–0.8] for a loose program reading one module, 6.7 ms [6.1–7.2] for a package of a manifest and a `lib.crs` declaring `M0` through `M11`, each module sixteen each of a `Nat` sum, a `Nat` recursion by `match` and a proof of `Eq()(n + 0, n)` by `Eq/refl()`, opened on `M0.crs`, and 31.4 ms [31.0–32.8] for a copy of `curios-prelude/std` opened on `Nat.crs`. Protocol: one fresh `curios wonder server` per run and case, the three cases interleaved within each run, initialized with no `workspaceFolders`; `didOpen` with the file's text, a `didChange` appending one declaration, that text written to disk, then `didClose` and `didOpen` with the same text twice, a pause of four times `SETTLE` before each open; each step timed from the notification's write to the `publishDiagnostics` whose `uri` is the document, and the second reopen is the figure.
- **Both shipped clients watch on request.** VS Code's language client, 10.1.2 in `editors/vscode/package-lock.json`, registers `workspace.didChangeWatchedFiles` with `dynamicRegistration` and `relativePatternSupport` (`FileSystemWatcherFeature`, [`fileSystemWatcher.ts`](https://github.com/microsoft/vscode-languageserver-node/blob/main/client/src/common/fileSystemWatcher.ts)), and Zed builds the globs of each `workspace/didChangeWatchedFiles` registration into what it matches a file event against ([#49778](https://github.com/zed-industries/zed/pull/49778)).
- **The protocol's types are at hand.** `lsp-types` 0.97, already a dependency of `curios-wonder`, carries `request::RegisterCapability`, `DidChangeWatchedFilesRegistrationOptions`, `FileSystemWatcher` and `notification::DidChangeWatchedFiles`, and a response to a request the server sent already reaches `Message::Response(_) => Ok(())`.

## Prior art

- **The protocol asks the client to watch.** "It is recommended that servers register for these file system events using the registration mechanism", and a server's own watcher "is not recommended", since "getting file system watching on disk right is challenging", it "is not for free", "a client usually starts more than one server" and "there are more server than client implementations" ([LSP 3.17, DidChangeWatchedFiles](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeWatchedFiles)). The registration is dynamic only: "the current protocol doesn't support static configuration for file changes from the server side".
- **clangd revalidates every open file rather than the readers of the changed one.** Saving a header rebuilds the files that include it, and "we revalidate preambles for all open files (stat all their headers) rather than taking advantage of the fact that we know which file changed. This is much simpler!" ([D77847](https://github.com/llvm/llvm-project/commit/596b63ad4019e61030803789a1844a0f1aeb34db)); its record of a file's dependencies misses one that was missing, which took an option of its own "to handle the odd case of a missing header file being added" ([D73916](https://github.com/llvm/llvm-project/commit/6ff0228c6df37e052fa6e8e3927e83b289402cf6)).
- **ghcide trusts a file unchanged only while the client watches it.** A workspace file the client watches is read again when a watch notification resets it, and any other file is stat'd on every rebuild ([`Development.IDE.Core.FileStore`](https://hackage-content-origin.haskell.org/package/ghcide-2.9.0.0/docs/src/Development.IDE.Core.FileStore.html)); it registers its watchers only where the client supports the dynamic registration ([ghcide #322](https://github.com/haskell/ghcide/pull/322)).
- **rust-analyzer watches through the client by default** (`rust-analyzer.files.watcher`, default `"client"`, [configuration](https://rust-analyzer.github.io/book/configuration.html)), with a watcher of its own behind that setting, and registers its globs through the client ([#5516](https://github.com/rust-lang/rust-analyzer/pull/5516)).
- **A server that watches nothing goes stale under version control.** gopls's [#31553](https://github.com/golang/go/issues/31553): of a file changed outside the editor, "often by doing things with their vcs (update/branch switch is the most common thing mentioned)", "we currently do not attempt to re-build that information unless it is invalidated by an editor change".

## Decisions

Settled 2026-10-26.

- **The client watches, and the server registers.** Where the client states `workspace.didChangeWatchedFiles.dynamicRegistration`, the server sends one `client/registerCapability` once `initialized` has arrived; where it does not, nothing is registered and a save stays the editor's gesture. Rejected: a watcher of the server's own, which the protocol argues against, adds a dependency, and serves no client this tree ships.
- **A change asks about every open document, as a save does.** The paths a notification names are not read for which documents they reach. Rejected: an index from a path to the documents whose last check read it, which needs what no log holds ("What is missing") to be right, where asking again about a document nothing moved under costs what the measurement above states; clangd made the same choice for the same reason.
- **The watched files are `**/*.crs` and `**/curios.toml`.** A unit reads `.crs` files and a placement reads manifests. Nothing a check writes matches either: a question files below the store's `verdicts`, `objects` and `compiler` families (`curios_package::Store`), and the `sources` family, which only `curios pin` and a fetch write, holds dependency texts whose change a check should answer.

## Open questions

- **A dependency outside the workspace folder.** A `source = "path"` dependency reaches out of its project (`../json`, `documentation/usage.md`), and a bare glob is relative to the workspace folders. Whether to register a `RelativePattern` at each such dependency's root where the client states `relativePatternSupport`, and whether a manifest edit that adds one registers again, or to leave such a dependency to the save.
- **A burst of events.** A checkout writes many files, and its events join one batch only where they arrive inside the analyst's settle, a tenth of the last check's cost capped at `SETTLE`. Whether events settle at the cap whatever the last check cost, as warming does, is settled by the verification's timing of a checkout.

## Stage

One commit.

- `serve` reads `InitializeParams::capabilities.workspace.did_change_watched_files.dynamic_registration`, and where it is `true` sends a `client/registerCapability` request registering `workspace/didChangeWatchedFiles` with the two watchers above, after `Connection::initialize` has returned and before the loop.
- `Server::notified` answers `DidChangeWatchedFiles::METHOD` as it answers a save: one job naming every open document, against the overlay as it stands.
- `curios-wonder/src/server.rs`'s module documentation says a watched change is answered as a save, and `documentation/usage.md`'s `server` row that the server re-publishes every open document when a file it watches changes.
- Check: an integration test in `curios/tests/wonder/server_tests.rs` that initializes with the capability, reads the registration request and its two globs, answers it, rewrites a module no editor holds, sends `workspace/didChangeWatchedFiles` naming it, and reads the refusal on the open document's module ahead of an edit behind it (`Editor::ahead_of`); the existing tests, which state no capability, show that nothing is registered without it. `cargo xtask workspace lint`.

## Verification

In VS Code and in Zed, with a package open on a document that reads a module no editor holds, a `git checkout` of a branch that changes that module re-publishes the document's diagnostics with no keystroke and no save, and checking out back restores them. Timed from the checkout's end to the last publish, for a checkout touching one file and one touching a hundred, with one and with five documents open, as the second open question asks.

## Retirement

The specification is deleted when the stage lands, the open questions settled in it or filed as findings.
