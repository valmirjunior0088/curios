# A library source is named by its package

Working specification for labelling a source of the package `std` by the package's name and the file's path under its root, wherever that package is compiled, where it is named today by the absolute path it was read from. The image then holds no directory of the machine that built it, a report placed in the library reads the same header on every machine, and whoever needs the file on disk asks the unit's own source where it lies.

## What this builds on

- **A source is a text and an optional path.** `Source` (`curios-utilities/src/span.rs`) is `path: Option<PathBuf>` and `text`, archived with the unit it was read for.
- **The library's path is the checkout's.** `std_directory` (`curios-std/src/lib.rs`) is `CARGO_MANIFEST_DIR` joined with `src`, `std_source` reads the package there as any package is read (`package_at`, `curios-package/src/layout.rs`), and `curios-prelude-archive`'s build writes each file's `Source` into `std.rkyv`, the directory standing beside each file's text and again beside each file's digest.
- **Four things read a source's path.** The header of a report or a note whose span lies in it (`Span::render_snippet`, `curios-utilities/src/span.rs`); the file a language server files such a diagnostic or a related place under (`adapt`, `curios-wonder/src/server.rs`, which falls back to the open document where a source has none); the key a thread's parse is remembered by (`tree_of`, `curios-text/src/root_source.rs`); and a profile span's label (`curios-parse/src/memo.rs`).
- **A unit's record names each file it read by canonical path** with the digest of its text (`Record::reads`, `curios-unit/src/stored.rs`). A store believes a unit once every file reads the text recorded (`unchanged`, `curios-verdicts/src/verdicts.rs`) and lies under its source's directories (`read_within`, `curios-unit/src/stored.rs`), and `the_std_record_names_every_authored_source_and_no_other` (`curios-prelude-archive/src/restore/tests.rs`) holds the library's record to its tree.

## The gap

`std_directory` (`curios-std/src/lib.rs`) is `CARGO_MANIFEST_DIR` joined with `src`, each file read from it is a `Source` whose `path` is archived with its unit (`curios-utilities/src/span.rs`), and `curios-prelude-archive`'s build writes them into `std.rkyv`, where the checkout's directory stands beside each file's text and again beside each file's digest. So one tree built at two paths is two images, where `sys.rkyv`, `documentation.rkyv` and `certification.rkyv` are the same bytes, a digest of the image compares with nothing built elsewhere, and a released compiler carries the directory its build machine checked the tree out at: `strings` over an installed `curios` 0.15.9 reads `/home/runner/work/curios/curios/curios-std/src/Vec.crs`. Four things read a source's path: the header of a report or a note whose span lies in it (`Span::render_snippet`, `curios-utilities/src/span.rs`), the file a language server files such a diagnostic or a related place under (`adapt`, `curios-wonder/src/server.rs`), the key a thread's parse is remembered by (`tree_of`, `curios-text/src/root_source.rs`), and a profile span's label (`curios-parse/src/memo.rs`). In a compiler that runs where it was not built, the first two name a file its machine does not hold.

## Decisions

- **The label is the package's name and the path under its root**, `/std/Nat/le.crs`: what a report's header and a server's location show for a place in the library. Settled on 2026-10-12.
- **The package named `std` is labelled wherever it is compiled.** The image and a checkout's compile spell their sources alike, so a store or a server never meets one source under two names. Settled on 2026-10-12.
- **Whoever opens a file asks the unit's source where it lies**, and never `std_directory`, which names the checkout the compiler was built in: another one wherever a second checkout's library is compiled or edited.

## Open questions

- **What a source holds.** A label beside a path, the path present where a file backs the text and archived or not; or one field read two ways. The first states both facts, and which of the four readers takes which is then written at each.
- **What a record's reads name.** By label, a store resolves each through the unit's source to read it again, and `read_within` holds the resolved file under the source's directories; by path, the record keeps the machine's directory the image is meant to lose.
- **What a server answers for a place in the embedded library** where no file of it is on the machine, an installed compiler's case: no location, or a document the server serves itself.
- **The parse memo's key.** Two checkouts of the library in one process share every label and differ in their texts, so a key by label alone answers one checkout's parse for the other's file.
- **Whether a dependency's sources are labelled the same way.** The decision covers `std`; the label is stated in terms any package has.

## Stages

1. **A source states its label apart from where it was read.** Check: a report placed in the library reads a `/std/…` header, as a fixture; every other pinned report is the text it was.
2. **The record and the store read by label.** Check: the store and recompile suites of `curios-verdicts` and `curios-pipeline` pass, and `the_std_record_names_every_authored_source_and_no_other` is restated over labels.
3. **The server and the memo ask the unit's source.** Check: the transport test that asks about one of the library's files (`curios/tests/wonder.rs`) answers from the checkout it is run in, and a library file edited in a second checkout is parsed as that checkout's.
4. **The image is the bytes it is wherever it is built.** Check: `std.rkyv` built from one tree in two directories is the same bytes, and `strings` over the compiler finds no directory of its build.

## Verification

`cargo xtask workspace lint` and `cargo xtask workspace test` after each stage, with that stage's check by name before them. The last stage's comparison is two builds of one commit in two directories under `target/scratch`. The full gate runs once, after the last stage.

## Rejected

- **The directory taken out where the image is written, and nowhere else.** A checkout's compile of the library and the image would then spell one source two ways, and a store or a server would meet both.

## Completion and retirement

What a source's label is and who may ask for its file goes to `curios-utilities`' and `curios-text`'s rustdoc, the rationale to a decision under `documentation/design/compilation/`, and the two-directory comparison stays as a stated protocol there. The line in `documentation/roadmap.md` becomes a checked summary, and this file is deleted.
