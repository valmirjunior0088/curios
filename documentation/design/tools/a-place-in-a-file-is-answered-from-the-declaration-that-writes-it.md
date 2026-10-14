# A place in a file is answered from the declaration that writes it

**Decision.** What stands at a place of a file — the name written there, where it is declared, what its declaration states of it, and what the file declares — is read off what the compiler holds of the declaration that writes it, and nothing is recorded for the asking.

- **A name's answer is the lowered term's.** A written name lowers to a variable holding the declaration it resolved to and where it was written, as an extent of the text of the declaration that wrote it, and a unit's table holds where each such text lies and where each definition's own name is written. A file and an offset give the text, the definitions named in the text give its lowered items, the ones read, and the names it writes are the variables placed in it (`curios-text/src/into_core/mention.rs`). A path is one name with one place.
- **It is the lowering's answer, and no checker's.** The fold compiles what a subject is compiled against and lowers the subject itself, no further (`Fold::lowered`, `Fold::lowered_unit`), so a file that does not elaborate answers as one that does.
- **A question is a `wonder` query first, and a protocol request second**, on one engine and one record (`curios-wonder/src/named.rs`): [`wonder at` and `wonder declarations`](../../usage.md#wonder), and the server's definition, hover and document symbols. One name stands at an offset, the narrowest that holds it, a rule stated over the records, which is all a server holds.
- **The server answers from what a document's last check left, with no parse of the document and no wait.** The records cross from the thread that owns the compiler to the one that reads the protocol. A request holds the buffer against the text that was checked by what the two share at their starts and at their ends: a declaration lying in what they share stands where it stood, or moved by the difference in length, and one a difference reaches is the one being edited, answered after its next check. So is one a difference touches with a character that reads on as part of a path, a name a keystroke extends being another name.
- **A declaration's head and prose are read off its own text**, the one its unit read and its record holds, parsed alone where it is asked for and printed by the printers a documentation page prints by, through one reading of what an item states of its definitions (`stated`, `curios-text/src/into_core/document.rs`). A declaration is stated whether or not a page shows it, and whether or not a file holds it.
- **A declaration is placed where a file holds the text its unit read**: always for a unit compiled from the disk or from an editor's text, and for the units a compiler was built with where their file still holds that text, which under an installed release none does.

What has no answer is [the roadmap's](../../roadmap/tools/201-the-editor-is-answered-at-a-position.md): a name no term holds at its own place, a segment of a path, and the type of a term.

**Rationale.**

- **Nothing is derived twice.** A name's place and what it resolved to are the lowered term's, and a head is its declaration's text through the page's printers, so an answer cannot disagree with the compilation or with a page.
- **A declaration's places are its own text's.** An extent names no file and no line, so an edit elsewhere in a file moves none of a declaration's places, and an answer taken from an earlier text is exact wherever it is given: the protocol's own allowance, that a result computed on an older state may still serve its client, is taken with nothing approximate in it.
- **A file being edited seldom elaborates**, and where a name is declared is what a reader asks of it most: a name the lowering resolves needs no elaboration to be answered.
- **A request is never behind a compile**, which is what the server's two threads exist for: what a check left is plain data, and holding a buffer against a checked text is two scans and no search.
- **A reader hovers over few names**, so a head is one declaration's parse where it is asked for, a cost the declaration bounds, where a document's parse is bounded by nothing a request can wait on.

**Rejected.**

- **A table of names by place, written beside the terms**: a written name's term holds its place and what it resolved to, so the table is a second copy of both.
- **The check's own verdict as the door**: a subject that is refused yields no unit, and a file with one mistake would answer nothing.
- **A parse of the document on the protocol thread**, to find the declaration under a cursor: a large file's costs what a request must not wait.
- **A request handed to the analyst**: it waits behind the check in hand.
- **An answer by the buffer's coordinates as last checked**: an edit above a declaration moves every place below it.
- **A search for each declaration's text in the buffer**: a short text is found inside another, and a text written twice is found twice.
- **What a buffer and its checked text share, taken as one difference between a prefix and the suffix after it**: text typed above a declaration that opens as the declaration does reads as typed inside it. Each is taken whole.
- **A head for every name, left by a check**: a parse for each declaration a document names, where a reader asks for few.
- **The documentation record as the source of a head**: it holds what a consumer sees of a library, so a program's own declarations and a private one are in none.
- **A second dispatch over the kinds of item**, beside the one a page is built by: which printer states which definition is read once.
- **The rule for the name at an offset in the surface crate**: a server asks it of records, and stated twice it is two rules.
