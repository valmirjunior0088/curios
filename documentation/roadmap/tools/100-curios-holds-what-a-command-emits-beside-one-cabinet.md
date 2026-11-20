# `.curios/` holds what a command emits beside one cabinet

Adopted by the user on 2026-11-19: `.curios/` holds a command's products by target, `native/`, `browser/` and `documentation/`, beside one directory, the **cabinet**, holding whole the half kept for reuse, which one variable, `CURIOS_CABINET`, moves; and a unit's slot is one directory of the cabinet holding its record, its index and its packs. A reader then tells the two halves apart by the tree rather than by the reference's table, a project shares, moves or clears its cabinet as one directory, and a slot is filed, collected and restored at the one address [A slot's objects are kept in packs beside it, at its address](../../../curios-verdict/README.md#a-slots-objects-are-kept-in-packs-beside-it-at-its-address) already gives it.

## What is there today

The cabinet stands whole ([Where things go](../../usage.md#where-things-go), `curios-package/src/cabinet.rs`, and [`.curios/` goes where a manifest governs, and its cabinet nowhere it was not pointed](../../../curios-package/README.md#curios-goes-where-a-manifest-governs-and-its-cabinet-nowhere-it-was-not-pointed)). Inside it, a unit's record and index stand at `verdicts/<slot>` (`Cabinet::verdict`) and its packs at `objects/<slot>/<pack>` (`Cabinet::objects`), one address in two trees.

**A slot's staging is swept by nothing.** `replace` (`curios-verdict/src/slot.rs`) stages a unit's slot at `<slot>.<pid>.part` beside it, in `verdicts/`, outside the directory its address's collection lists (`Packs::collect`), which sweeps a pack's staging, as a payload's filing sweeps a payload's (`sweep`).

**The word.** "The store" still names `.curios/` as a whole or the cabinet in the reference, the decisions, the crates' `README.md`s and rustdoc, the command line's help (`compile --output`, `document --output`, `pin --url`), the refusal `unwritable store` (`curios-package/src/curate.rs`), `Contract`'s `Access`, the fields and locals that hold a `Verdicts`, `curios-wonder/src/tests/store.rs`, `.claude/rules/pipeline.md`'s heading and `.claude/commands/`: some 950 occurrences of "the store", "a store" and "store's" across the tree, a part of them the disk's and the rest the metavariables', the universe solver's and Wasmtime's own stores. The verb stays the language's: a unit is stored, and a unit's `store` node narrates its filing.

## Settled

- **The name.** The half kept for reuse is the cabinet, a collector's cabinet keeping each piece in a drawer of its own, catalogued by what it is, for whoever comes asking — each unit in a slot of its own, each source and module under its digest, any project reusing what its inputs name — the image the language's own name and `wonder` take theirs from. "The store", which names `.curios/` as a whole, what a command emits there and what is filed for reuse alike, retires: `.curios/` is itself, a command's products are named by what made them, and what is kept for reuse is the cabinet.
- **The code follows the word.** A name that says store for `.curios/` or the cabinet follows it.
- **The slot.** A slot is the directory `cabinet/verdicts/<slot>/`, holding its record and index in one file, `verdict`, beside its packs and its address's collection, and `objects/` goes.
- **No migration.** The verdict schema tag (`VERDICT_SCHEMA`) moves with the slot's layout, and a cabinet filed under the old one is orphaned and rebuilt once.
- **The peers.** Cargo's build-dir layout v2 moves from grouping by content type to "each cacheable unit" in "a self-contained directory", for collection, locking and a cache shared across workspaces ([Call for Testing: Build Dir Layout v2](https://blog.rust-lang.org/2026/03/13/call-for-testing-build-dir-layout-v2)).

## Stages

### 1. The slot

`Cabinet::verdict` and `Cabinet::objects` become one slot directory, `cabinet/verdicts/<slot>/`, where `curios-verdict` names what stands in it — `verdict`, the record and then the index, the `*.pack` files and the `collection` — and files, collects and restores; `objects/` goes, `VERDICT_SCHEMA` moves, and a slot's staging, now in the address's directory, is swept by its collection as a pack's is. Restated with it: [A slot's objects are kept in packs beside it, at its address](../../../curios-verdict/README.md#a-slots-objects-are-kept-in-packs-beside-it-at-its-address), [What no slot names is collected five days after a collection first finds it so](../../../curios-verdict/README.md#what-no-slot-names-is-collected-five-days-after-a-collection-first-finds-it-so), `slot.rs`'s `//!` and `replace`'s `///`, `packs.rs`'s `//!`, `curios-package/src/products.rs`'s map and the reference's table. Check: `curios-verdict`'s filing, collection and restore tests at one directory; a slot's staging aged past the grace swept by a collection; a cabinet filed before the stage compiled again once and reused after.

### 2. The word

Every sentence of the tree that says "the store" for `.curios/` or the cabinet says, by what it means, the cabinet or `.curios/`: the decisions, the crates' `README.md`s, rustdoc and comments, the command line's help, the refusal `unwritable store`, `Contract`'s `Access` and the word its test renders, the fields and locals holding a `Verdicts`, the store's test module of `curios-wonder`, `.claude/`'s rules and commands; and `.claude/rules/documentation.md`'s retired words gain "the store", written as the cabinet or `.curios/`. Check: `cargo xtask workspace lint`, `cargo xtask docs build`, the command line's tests, and no sentence left saying it of either, by a search of the tree.

## Verification

Each stage: `cargo xtask workspace lint`, `cargo xtask workspace test -E 'package(curios-package) | package(curios-verdict) | package(curios-wonder)'` with the command line's tests that read `.curios/` (`curios/tests/payload.rs`, `lint.rs`, `document.rs`, `wonder/`, and `curios/src/tests/packages.rs`); then a program run twice over a fresh cabinet, the second run reusing every unit.

## Next step

A ready delivery: stage 1.

## Retirement

Once both stages land, the reference and the decisions state the cabinet and the slot, nothing cites this file, and it is deleted.
