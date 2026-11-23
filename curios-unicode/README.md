# curios-unicode

The Unicode Character Database the toolchain pins, read once. The build script downloads the files of one version of the database, checks each against its SHA-256, and writes the runs of scalars each property gives a class; the crate answers a scalar's properties from them, segments text into extended grapheme clusters by them, and hands the same tables on packed for `/sys`. What the compiler reads of a character and what a Curios program reads of it are therefore one table.

## Design

### The database is pinned by version and by hash, and fetched at build time

**Decision.** `build.rs` names one version of the database and the SHA-256 of each file the tables are built from — `EastAsianWidth.txt`, `emoji/emoji-data.txt`, `auxiliary/GraphemeBreakProperty.txt`, `DerivedCoreProperties.txt`, `UnicodeData.txt`, `PropList.txt`, `SpecialCasing.txt`, `CaseFolding.txt` and `DerivedNormalizationProps.txt` — downloads each from `unicode.org` once, and keeps it in `.artifacts/<version>/` beside the crate. A kept or hand-placed file is checked against its hash as a download is, and a build that can reach neither says which file to fetch and where to put it. The conformance files the tables are held to, `auxiliary/GraphemeBreakTest.txt` and `NormalizationTest.txt`, are test support's: `src/test_support.rs` fetches them the same way, at digests of its own and at the version the build script hands it, keeps them beside the others, and reads their cases, for this crate's tests and, under `test-support`, another's. A new version is a change of the version and the hashes, and nothing generated is committed.

**Rationale.** The database is an external concern, so one crate is its authority ([One crate is the authority for one external concern](../documentation/design/one-crate-is-the-authority-for-one-external-concern.md)), and `curios-binaryen` already pins its source this way. A hash makes the build's input exact without carrying two megabytes of the database in the tree, and the tables are derived, so a committed copy of them would be a second statement of the files it was read from. A conformance file is read by tests alone, so a build neither fetches it nor compiles its cases.

**Rejected.** The conformance files fetched by the build script beside the tables' and their cases written into the tables behind a gate: a download every fresh build makes and cases rustc reads in every build, for tests alone. The conformance cases held by a step rather than this crate's tests: a second pin of the database, or a path into this crate's kept files. The files vendored in the tree with a generated module committed beside them and a test refusing drift: the same exactness, at the price of the files in the tree and an artifact to regenerate by hand. A crate of another's tables, such as `unicode-width` or `unicode-segmentation`: their version is theirs, and `/sys` would carry a table read from a database the compiler does not name. Rust's own `char` methods for what the language reads of a character, its letters, digits, white space and controls: their version is the Rust toolchain's, which trails the pinned one, so an identifier spelled with a letter the pinned database adds would be refused.

### The library's enums are the classes

**Decision.** Each class set is one enum, declared once with the name `/sys` gives each class's code, and the build script writes every run of the tables as the variant it is, named from the database's own spelling. A class's code is its place in its enum. The class of a scalar no run names is the one the file's `@missing` line gives the whole range, and its runs are left out of the table.

**Rationale.** A class the database names and the enum lacks is refused where the crate compiles, so the build script holds no list of classes and nothing states one twice.

### A mapping is packed as records whose first target moves with the scalar

**Decision.** A case mapping is read off the database's rows — upper and lower case the simple mapping of `UnicodeData.txt` with the unconditional rows of `SpecialCasing.txt` over it, folding the common and full rows of `CaseFolding.txt` — and written as records, each covering the scalars from its first to its last a step of one or two apart, each scalar becoming one to three: the record's first target moved as far as the scalar is from the record's first, then the others as written. `/sys` carries a record in seventeen bytes, a target the record does not have written as U+0000, so that every target slot holds a scalar value.

**Rationale.** A mapping changes some 1,500 scalars, most a constant distance from what they become, many in runs where upper and lower case alternate; a record that steps by two holds such a run as Go's `CaseRanges` marks one `UpperLower`, so a mapping takes some 200 to 270 records. One shape for a scalar that becomes one and one that becomes several keeps one search and one fact over a table, which `/std/Char` holds by reduction when `/std` is built, so each character a lookup builds carries the proof that it is a scalar value; with every slot a scalar value, the fact states a record's targets with no case for how many it has.

**Rejected.** Records of one target beside a table of the scalars that become several: about a third smaller, at two searches a lookup and two facts a mapping. A pair for each scalar, as Rust's own tables are: some six times the records. A class code a run indexing a list of distances, as the property tables carry codes: a second table to read and to hold a fact of, and an alternating run spelled scalar by scalar.

### A decomposition is packed whole, as entries naming a run of a table of scalars

**Decision.** The canonical and the compatibility decompositions are read off field 5 of `UnicodeData.txt` and written fully decomposed, each scalar as every scalar it ends at once nothing it becomes decomposes again; the compatibility decomposition holds only the scalars whose decomposition under every mapping differs from the canonical one. `/sys` carries each as two tables: entries of seven bytes, a scalar and the place its scalars start in the second table and how many there are, and the scalars, three bytes each. The canonical combining classes are a property's runs, its code the class, and the primary composites, each pair a canonical decomposition of two scalars names but one `DerivedNormalizationProps.txt` marks `Full_Composition_Exclusion`, are records of nine bytes in the order of their pairs. The Hangul syllables decompose and compose by arithmetic, which `/std/Str`'s normalization does.

**Rationale.** A compatibility decomposition runs to eighteen scalars, past the three a mapping's record holds, and a decomposed scalar rarely sits a constant distance from its neighbours', so a record's run buys nothing here; written whole, a lookup is one search and no recursion, and `/std` holds by reduction when it is built that every scalar of the second table is a scalar value and every entry names places the table holds, so each character a lookup builds carries its proof.

**Rejected.** The mapping's record grown to eighteen targets: a record of sixty-two bytes for the two most decompositions make. Decompositions one level deep, decomposed again as they are read: a search per level, and a recursion `/std` would have to bound. The composites as a mapping over pairs: no record's run holds a pair.

### The pieces are neutral

**Decision.** The crate answers what the database and UAX #29 say: each property of a scalar, and the boundaries of extended grapheme clusters. How many columns a cluster takes on a terminal is a choice over those properties, and each consumer makes its own: `curios-common`'s caret, and `/std/Tui`.

**Rationale.** Terminals disagree on widths, so a width is a policy, and a shared crate offering one would make every consumer's choice its own.
