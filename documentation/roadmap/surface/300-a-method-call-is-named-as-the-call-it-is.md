# A method call is named as the call it is

**Not refined yet.** This specification reserves naming the function a reader meant where they wrote a function of the head's own module as a method, `xs.len()` for `List/len(xs)`. It is not an implementation plan: the module's exports are known only where names are resolved, which is not where the projection is refused.

## What is missing

`xs.len()` over `xs: List(Nat)` reports `projected from a non-tuple`, `head has type: List(Nat)`, probed again at `00627ea06`. The projection is refused where the head's type is no record (`elaborate_proj` and `project`, `curios-elab/src/elaborate/aggregate.rs`), and what its author meant is `List/len(xs)`. A label a record does not have, `s.len()` over a `Str`, is refused as `no field named`, with the same call unnamed.

## What is known

- **A carrier's type and functions are `/sys`'s, re-exported.** `/std/List` opens with `pub use /sys/{List}` and `pub use /sys/List/{get, len, slice, map, fold}` and declares the rest of its functions itself, and every carrier's module does the same (`curios-std/src/Nat.crs`, `Int.crs`, …): no `/std` name of a carrier's type is declared, so the registry, whose every target is a declared name (`validate_syntax_targets`, `curios-prelude-archive/elaborate.rs`), holds none, and `/std/List/len` is a re-export no assumption of the elaborator's is filed under.
- **Elaboration holds resolved names only.** Lowering resolves every written path to a global, and elaboration reads no module's exports: whether `/std/List` offers `len`, declared or re-exported, is the resolver's to say (`curios-text`).
- **Tried and taken out, on top of `026914537`:** a registry slot for each carrier naming its `/std` type, `/std/List/List` and its siblings, the module read off it, the label looked up as an assumption of `module/label`, and the call written into the refusal for the report to spell. The prelude's build refuses the slots, the names being no declarations, and a re-exported function would not have been found had they been.

## Refinement

This focused diagnostic can be delivered beside the editor's binder and term-type answers, before architectural expansion, once its resolver boundary is settled. It waits on no acceptance of method-call syntax and on no broader editor completion.

- **Where the call is found.** Either the refusal carries the head, its type and the label, and the report, which spells through the units' lexicons and their spellings (`curios_text::PreparedText::spellings`), asks the resolver whether the type's module offers the label; or lowering records, for each carrier and each nominal type, the module whose functions a method over its values names. Which module that is for a carrier, `/std/List` where the type is `/sys/List/List`, is stated once where either reads it.
- **Check**: `xs.len()` names `List/len(xs)`, `s.len()` names `Str/len(s)`, and `xs.size()` reports as it does.
