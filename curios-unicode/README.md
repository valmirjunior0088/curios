# curios-unicode

The Unicode Character Database the toolchain pins, read once. The build script downloads the files of one version of the database, checks each against its SHA-256, and writes the runs of scalars each property gives a class; the crate answers a scalar's properties from them, segments text into extended grapheme clusters by them, and hands the same tables on packed for `/sys`. What the compiler reads of a character and what a Curios program reads of it are therefore one table.

## Design

### The database is pinned by version and by hash, and fetched at build time

**Decision.** `build.rs` names one version of the database and the SHA-256 of each file it reads — `EastAsianWidth.txt`, `emoji/emoji-data.txt`, `auxiliary/GraphemeBreakProperty.txt`, `DerivedCoreProperties.txt` and `auxiliary/GraphemeBreakTest.txt` — downloads each from `unicode.org` once, and keeps it in `.artifacts/<version>/` beside the crate. A kept or hand-placed file is checked against its hash as a download is, and a build that can reach neither says which file to fetch and where to put it. A new version is a change of the version and the hashes, and nothing generated is committed.

**Rationale.** The database is an external concern, so one crate is its authority ([One crate is the authority for one external concern](../documentation/design/one-crate-is-the-authority-for-one-external-concern.md)), and `curios-binaryen` already pins its source this way. A hash makes the build's input exact without carrying two megabytes of the database in the tree, and the tables are derived, so a committed copy of them would be a second statement of the files it was read from.

**Rejected.** The files vendored in the tree with a generated module committed beside them and a test refusing drift: the same exactness, at the price of the files in the tree and an artifact to regenerate by hand. A crate of another's tables, such as `unicode-width` or `unicode-segmentation`: their version is theirs, and `/sys` would carry a table read from a database the compiler does not name.

### The library's enums are the classes

**Decision.** Each class set is one enum, declared once with the name `/sys` gives each class's code, and the build script writes every run of the tables as the variant it is, named from the database's own spelling. A class's code is its place in its enum. The class of a scalar no run names is the one the file's `@missing` line gives the whole range, and its runs are left out of the table.

**Rationale.** A class the database names and the enum lacks is refused where the crate compiles, so the build script holds no list of classes and nothing states one twice.

### The pieces are neutral

**Decision.** The crate answers what the database and UAX #29 say: each property of a scalar, and the boundaries of extended grapheme clusters. How many columns a cluster takes on a terminal is a choice over those properties, and each consumer makes its own: `curios-utilities`' caret, and `/std/Tui`.

**Rationale.** Terminals disagree on widths, so a width is a policy, and a shared crate offering one would make every consumer's choice its own.
