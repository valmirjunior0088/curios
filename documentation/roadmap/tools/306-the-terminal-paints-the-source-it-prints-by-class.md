# The terminal paints the source it prints by class

**Not refined yet.** Every piece of source the command line prints is plain: the terminal paints a report's structure alone — its headline bold, its tree and gutter dim, what went wrong red (`Style`, `curios-common/src/style.rs`) — and has no palette for the classes [Curios is coloured from one class set](../../design/tools/curios-is-coloured-from-one-class-set.md) names, which an editor and a documentation page paint by.

## What is known

- **Printed source is classed already.** The terms behind a report's facts, the head `wonder --at` and `--name` print, `--exports`' heads, `--witnesses`' keys, and `--stage text`, `core` and `core-elab` are written by Curios's printers, whose `render_annotated` marks every word and name, as a page is painted from.
- **Verbatim source is classed in part.** A report's quoted line, and the text `--name` and `--at` print as their author wrote it, are classed by a check's record in the subject's own file — names, binders, fields, segments and contextual words (`Placed::classes`, `curios-wonder/src/classes.rs`) — and nothing the command line holds classes a comment, a string or a number in them.
- **Some output is no Curios source**, and stays plain: the rungs past `core-elab`, `format -`'s output, which is the file's own bytes, and what a test's program writes.
- **Peers split**: rustc and GHC print a snippet's code plain, and miette paints one only behind its opt-in `syntect-highlighter` feature, in truecolor alone ([`highlighters`](https://docs.rs/miette/latest/miette/highlighters/index.html)).

## What it waits on

A decision, its own: the palette — one table in `Style` giving each class a terminal role, the terminal being a consumer as `editors/textmate/classes.json` is the editors' — and which output is painted: printed source first, verbatim source only where every class of it is known, and an answer on standard output only where it is a terminal, as the reader's style already decides.
