# A source is named by its package

**The benefit.** Every source is named by its package and its path under the package's root — `/std/Nat/le.crs`, `/app/src/main.crs` — where it is named today by the absolute path it was read from, so **no absolute path is in anything archived, recorded, reported, or keyed beyond one process**: an image is the same bytes wherever it is built, a released compiler carries no directory of its build machine, a report names a place every reader can find, and a package's stored units hold wherever the package travels. A package is its manifest and the tree under it, and knows of no directory outside it; the only absolute path left is a file's handle, which a process resolves from the package's root where it touches the file and never lets out. The project has decided to adopt it.

The next step is stage 1, a delivery. It waits on nothing.

## What this builds on

- **A source is a text and an optional path.** `Source` (`curios-common/src/span.rs`) is `path: Option<PathBuf>` and `text`, archived with the unit it was read for. `Source::labelled` already names text that arrived with no file, the program read from standard input, by a display name that is never opened.
- **The library's path is the checkout's.** `std_directory` (`curios-prelude/src/test_support.rs`) is `CARGO_MANIFEST_DIR` joined with `std`, `std_source` reads the package there as any package is read (`package_at`, `curios-package/src/layout.rs`), and `curios-prelude`'s build writes each file's `Source` into `std.rkyv`, the directory standing beside each file's text and again beside each file's digest.
- **A dependency is materialized by its revision.** The store holds a fetched package under `sources/` keyed by its revision's digest, in one directory shared across projects where `CURIOS_CACHE` is set (`curios-package/src/store.rs`), and a package's name is its identity in the dependency graph: a name resolves to one package, and a diamond shares it (`curios-package/src/graph.rs`).
- **Four things read a source's path.** The header of a report or a note whose span lies in it (`Span::render_snippet`, `curios-common/src/span.rs`); the file a language server files such a diagnostic or a related place under (`adapt`, `curios-wonder/src/server.rs`, which falls back to the open document where a source has none); the key a thread's parse is remembered by (`tree_of`, `curios-module/src/root_source.rs`); and a profile span's label (`curios-parse/src/memo.rs`).
- **A unit's record names each file it read by canonical path** with the digest of its text (`Record::reads`, `curios-unit/src/stored.rs`). A store believes a unit once every file reads the text recorded (`unchanged`, `curios-verdict/src/verdicts.rs`) and lies under its source's directories (`read_within`, `curios-unit/src/stored.rs`), and `the_std_record_names_every_authored_source_and_no_other` (`curios-prelude/src/restore/image_tests.rs`) holds the library's record to its tree.

## The gap

Each file read is a `Source` whose `path` is its canonical absolute path, and that path is archived with its unit, written into its record and printed in every report. So one tree built at two paths is two images, where `sys.rkyv` and `documentation.rkyv` are the same bytes; a digest of the image compares with nothing built elsewhere; a released compiler carries the directory its build machine checked the tree out at — `strings` over an installed `curios` 0.15.9 reads `/home/runner/work/curios/curios/curios-std/src/Vec.crs` — and a report placed in the library names a file the reader's machine does not hold; and a stored unit names the directory its package sat in when it was compiled.

## Decisions

- **A source's name is its package and its path under the package's root**, for every package: `/std/Nat/le.crs`, `/app/src/main.crs`. It is the one name a source has everywhere — a report's header, a unit's record, an image, a store's key — and the language's own: a module is named by its package and its path already. Text with no package, the program read from standard input, is named `<stdin>`. Restated on 2026-11-16 from the library alone to every package: a package travels — published, fetched, built elsewhere — and knows nothing outside its manifest.
- **A source holds its name and, within one process, its file.** The name is archived; the file, the absolute path where a file backs the text, is resolved from the package's root where the source is read and is never archived, recorded or reported. Its readers are the ones that touch a file: the parse memo, a server opening a document, a store reading a file again.
- **A record names what it read by name.** A store resolves each name through the unit's source, the packages' roots on this machine, to read it again, and `read_within` holds the resolved file under the source's directories.
- **A place no file backs has no location.** A server answers no location for a place in a package whose files this machine does not hold, the library an installed compiler embeds among them: its head and prose still answer from the records, and a related place keeps its text. Nothing writes a file to give a place a location, and no editor's extension learns a scheme of the server's.
- **The parse memo keeps its key**, the file it read: it holds the files a thread read, each with at most two texts compared whole, so two checkouts of one package parse as their own, and a name, which two checkouts share, never keys it.
- **`Source::labelled` takes the new name's word away from it**: the display name of text with no file is the source's name like any other, and "label" names nothing apart.

## Stages

1. **A source states its name apart from its file.** Check: a report placed in the library reads a `/std/…` header and one placed in a program's own file reads `/<package>/…`, as fixtures; no report, archive or record a test reads holds an absolute path.
2. **The record and the store read by name.** Check: the store and recompile suites of `curios-verdict` and `curios-pipeline` pass, `the_std_record_names_every_authored_source_and_no_other` is restated over names, and a package compiled, moved to another directory and compiled again believes its stored units.
3. **The server and the memo read the file a unit's source resolves.** Check: the transport test that asks about one of the library's files (`curios/tests/wonder/server.rs`) answers from the checkout it is run in, a library file edited in a second checkout is parsed as that checkout's, and a place in a library with no file on the machine answers no location.
4. **An image is the bytes it is wherever it is built.** Check: `std.rkyv` built from one tree in two directories is the same bytes, and `strings` over the compiler finds no directory of its build.

## Verification

`cargo xtask workspace lint` and `cargo xtask workspace test` after each stage, with that stage's check by name before them. The last stage's comparison is two builds of one commit in two directories under `target/scratch`. The full gate runs once, after the last stage.

## Rejected

- **The directory taken out where the image is written, and nowhere else.** A checkout's compile of the library and the image would then spell one source two ways, and a store or a server would meet both.
- **The library named apart, every other package by its path.** It reasons from where today's symptoms show, the one package shipped inside the compiler; but every package travels, and a stored unit, a record or a report holding a directory is wrong on the next machine whichever package it is.
- **A second spelling in a report**, the path a terminal opens beside the name where a file exists: one place spelled two ways. An editor loses nothing, since the server resolves the name to place a diagnostic.
- **Copies of an embedded library written to disk, or a document the server serves under a scheme of its own**, to give a place in it a location: the one writes files as a side effect of a query, the other asks code of every editor's extension.

## Completion and retirement

What a source's name is and who may resolve its file goes to `curios-common`'s and `curios-module`'s rustdoc, the rationale to a decision under `documentation/design/compilation/`, and the two-directory comparison stays as a stated protocol there. Once those contracts are recorded and no reference to this specification remains, this file is deleted.
