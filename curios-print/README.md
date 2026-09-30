# curios-print

The Curios pretty-printing combinator DSL: single-use `Printer` actions composed through `flat`, `sep_flat` and `indent` and run by `run_printer` — the document algebra `curios-text`, `curios-core` and `curios-wasm` write their `Display` impls in. `curios-ersd` and `curios-cont` print their arenas directly and depend on nothing here. The layout algorithm is the cross-cutting [A printer states each fact once, where it is bound](../documentation/design/tools/a-printer-states-each-fact-once-where-it-is-bound.md); why the document is data rather than a tree of closures, why the crate depends on nothing beyond `std::fmt`, and every combinator's contract belong to the crate rustdoc.

## Design

### Split from `curios-parse` because both name their unit `pure`

**Decision.** The parser and printer combinator DSLs are two crates rather than two modules of one.

**Rationale.** Both are monads naming their unit `pure`, and in one crate they would have to stay unflattened namespaces — an exception to the workspace's rule that a crate is a flat namespace — so that `parser::pure` and `printer::pure` stay distinguishable. Split, the crate name disambiguates, `curios_parse::pure` and `curios_print::pure`, as `curios-cert`'s flat judgments are disambiguated from the elaborator's by their crate. A crate that holds a name apart is cheaper than an exception to a layout rule, which has to be remembered where a crate boundary does not.

**Rejected.** One crate with `parser::` and `printer::` modules, a standing exception to buy the two `pure`s.
