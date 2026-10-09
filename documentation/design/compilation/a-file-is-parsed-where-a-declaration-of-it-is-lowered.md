# A file is parsed where a declaration of it is lowered

**Decision.** A unit keeps, for each file it was lowered from, what the file's items declare, with the text they were read from (`Scan`, `curios-text/src/into_core/head.rs`): each item's head — the names it declares and their audiences, a `mod` with the heads of an inline body, a `use` whole, the report of an item the parser could not read — and where the item's own text lies. A unit lowered over a baseline reads each of its files as text (`RootSource::read`), and where that text is the one the baseline's scan was made from it takes the scan's heads and parses nothing. Everything a lowering decides before it lowers a declaration is read off heads: which modules the unit declares, what each binds and exports, the `pub use` fixed point, the scope each declaration is lowered in, and where each declaration's text lies, which is what finds its lowering in the baseline ([A lowering is kept where its text and the names it resolved stand](a-lowering-is-kept-where-its-text-and-the-names-it-resolved-stand.md)). A file's tree is asked for where one of its declarations is lowered, which is what parses it, once. A scan's positions are placed in the file as it is read, since a unit restored from a store holds each scan in a source of its own, under the path it was built at. Scans are part of a unit's baseline half ([`curios-unit`'s README](../../../curios-unit/README.md#a-unit-is-stored-in-parts-by-who-reads-each)), and a module supplied already parsed has none.

**Rationale.**

- **What a file declares is a function of its text**, and the passes over a whole unit read what every file declares and no file's tree: only a declaration's lowering reads what the declaration says. A lowering kept from the baseline reads no tree at all, so with the heads kept, a file none of whose declarations is lowered again has no reader for its tree.
- **The tree is asked for by the lowering that reads it**, so what a recompile parses follows the declarations it lowers again: an edited file, and a standing one where a name one of its declarations wrote resolves elsewhere.
- **One source for each text of a file.** A file's text is read once into the source every position in it lies in, whether the position is a head's, taken from a scan, or a term's, taken from the tree parsed later, and a text a thread already holds is the source it was (`sourced`, `tree_of`, `curios-text/src/root_source.rs`). So a session's scan is the baseline's own, shared, and a report cannot tell which of the two a position came from.
- **A read evicts no parse.** A thread holds, for each file, its last text that parsed beside the text it was last read as, so a file read and found standing, or read and found not to parse, leaves the last good parse where the next read of that text wants it.
- **The text is compared, never the file's time or a digest of it**: the scan holds the text its positions lie in, and comparing it reads what was just read.
- **In the half only a recompile restores.** A program compiled against a unit reads no file of it.

**Rejected.**

- **Parsing every file and comparing what it declares with the baseline's**: the parse is paid to learn it was not needed.
- **The parse kept from one process to the next as trees**, a stored module beside each file: a tree is everything the file says, restored for the declarations a recompile keeps, which read none of it.
- **A file believed by its modification time**: a file rewritten with the text it had is another file, and a text an editor holds has none.
- **Heads read by a second, shallow parser**: two readings of one grammar, agreeing only where both are maintained.
