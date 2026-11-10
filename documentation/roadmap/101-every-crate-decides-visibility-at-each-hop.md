# Every crate decides visibility at each hop

Held to [Visibility is decided at each hop](../design/visibility-is-decided-at-each-hop.md): a name is private or `pub` and every module private, a crate root names its API and never globs, a record's fields are all `pub` and an owner's all private, and a scoped modifier sits only on a method narrower than its type. Every crate keeps it, and no row of the lint table holds any of it.

## What is wrong today

- **The lint table holds none of it.** Neither field lint nor `redundant_pub_crate` is configured, nor rustc's `unnameable_types`, and the table denies warnings, so a row cannot stand ahead of its sites.

## Goal

Every crate keeps the decision, the jig's included: its root names its API, no item but a method narrower than its type carries a scoped modifier, and every struct is a record or an owner. `field_scoped_visibility_modifiers`, `partial_pub_fields` and `redundant_pub_crate` stand at `warn` in `Cargo.toml`'s `[workspace.lints.clippy]`, and rustc's `unnameable_types` in its `[workspace.lints.rust]`, with `editors/zed/Cargo.toml`'s tables alike, the decision names `redundant_pub_crate` and `unnameable_types` beside the two field lints, and every lint step of the gate passes.

## Settled

- **The rows at `warn`**, as the table's others are; `warnings = "deny"` makes a hit an error.
- **`redundant_pub_crate` is all that holds the items.** `pub(super)` on an item, a glob in a root's `pub use` and a method no narrower than its type are held by review; the workspace writes no check of its own for them.
- **A crate is taken from the top of the dependency graph down**, the jig's first and the leaves last, so a root names its API once every crate that reads it has shed its dead code: rustc's dead-code analysis skips an exported name, so a list named first would keep a name its readers stop reading.
- **A crate's root is named before any of its names is respelled `pub`**, since under a glob a respelling is an export. The root names every name some production reads that another crate reads, a suite included, and every type an item it names carries in its signature, a `pub` field or a trait impl, so no value another crate holds has a type it cannot name: rustc's `unnameable_types` holds the first two, and review the third, an archived type's `Archived…` twin among them. A name only tests read, in its crate or another, is moved under the crate's `test_support`. A `test_support` holds what it defines and facades of its own children, and re-exports nothing sideways. A name nothing reads once the glob is gone is deleted.
- **A re-export is a facade or a stated exception, and so is an `as` rename**: `curios-archive`'s mechanism and `curios-js`'s `use curios_browser as _;` are the exceptions. A name that clashes stays qualified rather than aliased or re-exported under a namespace, as `curios_cert::convert` does beside the elaborator's `convert`.
- **A wrapper only tests call is dropped**, and the function it wraps takes its name and its contract; its callers pass the argument it filled in, or call a helper of the tests' `test_support` where that argument is heavy to build. `curios-wonder`'s `diagnosed` became `diagnostics` so.
- **A member reaches as far as its type**: a method keeps a scoped modifier only where it is narrower than the type's reach, and a field keeps none.
- **A field some readers may touch and others may not is an owner's**, whose fields none touch and whose operations are its methods. What a sibling module does with an owner's fields moves onto the owner as a method, and an accessor stays only where the value itself is what the sibling needs, as a memo's key. `ParserState` and `ParserError` (`curios-parse`) are read by six siblings for the span or the progress between two states, the rest of the input, the character before, a literal prefix and the ranking of two failures, each an operation of theirs; `into_cps/lowerer.rs` calls the builder of `Emitter`'s `module` (`curios-anf/src/into_cps/emitter.rs`) 144 times, and those calls become `Emitter`'s methods. An `impl` block outside its type's module subtree moves under it, as `ForeignBindings`'s in `curios-native/src/plugin.rs` does. A struct none of whose fields holds an invariant is a record instead.
- **The eight mixed structs.** Owners: `Source`, whose line starts cache its `text`; `InductArm`, whose marks parallel its body's binders; `Pinning`, whose `directory` is its manifest's; `Indexed`, whose index names exactly the objects it holds; `Loaded`, whose tree is parsed from its source. Records: `Minted`, whose shared entries only make its archive smaller; `Imported`, whose spelling is read off fields that hold no invariant; `BlockData`, none of whose fields depends on another.
- **The other judgment structs.** Owners: `ParserState` and `ParserError` (`curios-parse`); `Emitter`, `RowLayout`, `ConstructionTemplate` and `Mark` (`curios-anf`); `CopyPlan` (`curios-cps`); `EmissionBody`, whose escapes are derived from its blocks and tail, and `ForceWalk` (`curios-emit`); `Grader` and `SizeContext` (`curios-analysis`); `Unseen`, `Shared`, `Lowerer` and `Aliases` (`curios-module`); `Running`, `ForeignBindings`, `MachineCode` and `Compiled` (`curios-native`).
- **`curios-archive`'s re-exports stand**: its derive, `rkyv` and `rkyv`'s traits and field adapters are the mechanism [One crate is the authority for one external concern](../design/one-crate-is-the-authority-for-one-external-concern.md) has one crate absorb, and the derive's expansion reaches `rkyv` through them. The visibility decision's sentence against re-exports is restated in that crate's turn to name this one exception.
- **`curios-cert`'s roster becomes a list**: a judgment imports each shared function from the crate that defines it, and the crate's suite lists every one (`curios-cert/src/tests/shared.rs`), failing on one imported off the list or listed and imported nowhere. `shared.rs`, whose re-exports were the kernel's one path to the layer, goes.
- **`curios-js` links `curios-browser` by `use curios_browser as _;`**, not by re-exporting `run`, where `cargo xtask bundle build` and the browser test show the bundle still exports it; otherwise the re-export stays as a stated exception.
- **Items of a module-local `pub mod test_support` reach its crate**, where a scoped modifier held them to a narrower module: no spelling keeps that audience short of splitting the module, and the module is test scaffolding.
- **The `test_support` records are records**: the six structs holding the 16 such fields left sit in `#[cfg(test)]` modules of their own crate, and each already gives every field its own reach.

## Stages

1. **The four rows in both lint tables**, with the decision's sentence naming `redundant_pub_crate` and `unnameable_types`. Check: every lint step of the gate — `workspace lint`, `tools lint`, `native lint`, `instrumented lint`, `zed lint` — and `zed test`.

The next step is a ready delivery: the rows.

## Retirement

Once the rows hold, the decision's sentences are the code's state; nothing references this specification, and it is deleted.
