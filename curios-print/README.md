# curios-print

The Curios pretty-printing combinator DSL: single-use `Printer` actions composed through `flat`/`sep_flat`/`indent` and run by `run_printer` — the document algebra `curios-text`, `curios-core` and `curios-wasm` write their `Display` impls in. `curios-ersd` and `curios-cont` print their arenas directly and depend on nothing here.

The layout algorithm this crate implements is a cross-cutting decision and stays in [One document algebra decides layout for every printer](../documentation/design/toolchain/one-document-algebra-decides-layout-for-every-printer.md). Why the document is data rather than a tree of closures, why the crate depends on nothing beyond `std::fmt`, and every combinator's contract belong to the crate rustdoc, which states them where a reader meets them.

## Design

### Split from `curios-parse` because both name their unit `pure`

**Decision.** The parser and printer combinator DSLs are two crates rather than two modules of one.

**Rationale.** Both are monads and both name their unit `pure`. While they shared a crate they had to stay unflattened namespaces — the single documented exception to this workspace's rule that a crate is a flat namespace — purely so that `parser::pure` and `printer::pure` stayed distinguishable. Split, the crate name does that work: `curios_parse::pure` and `curios_print::pure` are unambiguous at every use site, and each crate is flat like every other.

A crate that exists to hold a name apart is cheaper than an exception to a layout rule. The workspace makes the same move elsewhere for the same reason: the kernel's judgments live flattened on `curios-cert`'s root rather than under a `kernel::` namespace inside `curios-core`, because there too two vocabularies collided on one name and the crate name was the honest disambiguator. Both exceptions that once existed were this shape, and both were resolved by a crate boundary rather than by a namespace.

**Rejected.** Keeping one crate with `parser::` and `printer::` modules. It bought nothing except the two `pure`s and paid a standing exception to a rule every other crate follows — the more expensive of the two, because an exception has to be remembered and a crate boundary does not.
