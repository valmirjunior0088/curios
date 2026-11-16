# Curios is coloured from one class set

**The benefit.** Every place Curios is coloured — the editors, the documentation, the website, the playground — colours it from one statement of the language's words and one set of classes, each by the best it knows of the text: the printer where the compiler printed it, a grammar where a person typed it, and in an editor the compiler's answer laid over the grammar. The documentation colours a signature's every word and name by what it is, a name of another unit included. The project has decided to adopt it. The words, the class set and the archive a release ships with the grammars and their table are in place already ([Curios is coloured from one class set](../../design/tools/curios-is-coloured-from-one-class-set.md)); what is left is the documentation's signatures.

The next step is stage 1, a delivery.

## What is missing

**The documentation colours a signature by reading its text again.** `words` sets apart every word of its list wherever it stands in a printed signature, so a parameter named `concept` or `satisfy` is painted a keyword and one named `letà` is painted `let` and `à`, the scan cutting at the first character past ASCII. A name is a link or plain text and never classed, and a name outside the documented unit carries only the declaration it resolved to and whether that lies within (`Mark`, `curios-document/src/record.rs`), so nothing on a page says what it is; a sort is plain text.

## What this builds on

- **One statement of the words and one class set**, `curios-common`'s `Keyword` and `Class` ([Curios is coloured from one class set](../../design/tools/curios-is-coloured-from-one-class-set.md)).
- **The printer marks what it writes.** `Printer::Named` reports each name it emits with the range it landed on (`render_annotated`), and the documentation builder resolves each to the declaration it names from the lowering's tables (`curios-module/src/into_core/document.rs`), the builder that answers `wonder --at`'s heading as well.
- **What each declaration is**, outlined by the lowering for every unit, a stored one included (`PreparedText::declared_as`, `curios_core::Declared`).

## What the peers do

**Printed text is classed by its printer.** Unison's pretty printer writes text annotated span by span with an `Element` — a `ControlKeyword`, a `TypeReference` or `TermReference` with what it names, a `DelimiterChar` — which its terminal and its web views each render ([`SyntaxText.hs`](https://github.com/unisonweb/unison/blob/trunk/lib/unison-pretty-printer/src/Unison/Util/SyntaxText.hs)). rustdoc classes every path of a signature by its item's type, one of another crate's by that crate's metadata ([`item_type.rs`](https://github.com/rust-lang/rust/blob/master/src/librustdoc/formats/item_type.rs)); Rocq's compiler writes the name resolutions `rocq doc` links by ([coqdoc](https://rocq-prover.org/doc/master/refman/using/tools/coqdoc.html)); Agda's HTML colours each name by the type checker's `NameKind` ([`Agda.css`](https://github.com/agda/agda/blob/master/src/data/html/Agda.css)).

What is taken: printed text classed by its printer (Unison), a name of another unit included (rustdoc).

## Settled

- **Printed text is classed by its printer.** Each word as `Keyword` states it, or `curios-abi` for a wire word, a type, and each name by what the declaration it resolved to is, the unit's or a dependency's alike, so the documentation reads no text again. A class is read off what a declaration states, never off where it lives, and whether a declaration is a function or a value off the head of the type it states, where the lowering outlines what each declaration is (`Declared`), so the documentation and the server's outline read one answer.

## Stages

1. **The documentation's signatures.** The lowering's outline tells a function from a value (`Declared`), which the server's outline reads as well; the printer annotates each word it writes, as it annotates a name; the builder classes a word by `Keyword`, or `curios-abi` for a wire word, and each name by what the declaration it resolved to is, of the unit or of a unit it was lowered over alike (`declared_as`), and the record carries the class with each mark; a binder, which resolves to no declaration until [The editor is answered at a position](300-the-editor-is-answered-at-a-position.md)'s first stage records binders, stays plain; a page paints each class by its name, `KEYWORDS` and `words` gone and a module card's tally ordered by `Kind`. The record's layout changes, so the store's verdict schema tag, the prelude's records image and `curios/src/tests/document.rs` move with it. Check: `curios-document`'s suites, and a documented package whose signatures name a `/std` type, constructor, concept and function, a value of its own, and parameters named `concept` and `letà`, rendering each in its class.

The stage's contract lands in [the decision](../../design/tools/curios-is-coloured-from-one-class-set.md): what classes printed text.

## Retirement

Once the stage has landed, the decision states what classes printed text, and nothing references this specification, it is deleted.
