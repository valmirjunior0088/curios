# `.curios/` holds what a command emits beside one cabinet

Adopted by the user on 2026-11-19: `.curios/` holds a command's products by target, `native/`, `browser/` and `documentation/`, beside one directory, the **cabinet**, holding whole the half kept for reuse, which one variable, `CURIOS_CABINET`, moves; and a unit's slot is one directory of the cabinet holding its record, its index and its packs. A reader then tells the two halves apart by the tree rather than by the reference's table, a project shares, moves or clears its cabinet as one directory, and a slot is filed, collected and restored at the one address [A slot's objects are kept in packs beside it, at its address](../../../curios-verdict/README.md#a-slots-objects-are-kept-in-packs-beside-it-at-its-address) already gives it.

## What is there today

`Store` (`curios-package/src/store.rs`) places nine entries side by side in `.curios/` (`STORE`): the products, at `built` and `documentation`, in the project's own, and the shared half — `sources/`, `foreign/`, `verdicts/`, `objects/`, `payloads/`, `compiler/` and `pin`'s transient `pinning.<pid>` (`staging`) — at `shared`, the directory `CURIOS_CACHE` names or else the project's `.curios/` itself, where the two halves interleave and only the reference's table ([Where things go](../../usage.md#where-things-go)) tells which the variable moves. A unit's record and index stand at `verdicts/<slot>` (`Store::verdict`) and its packs at `objects/<slot>/<pack>` (`Store::objects`), one address in two trees. The module's `//!` map lists eight families and leaves out `compiler/`, which the reference lists. [The store goes where a manifest governs, and nowhere it was not pointed](../../../curios-package/README.md#the-store-goes-where-a-manifest-governs-and-nowhere-it-was-not-pointed) and `shared`'s `///` defend the variable as the one way to share, with no divined default, which the cabinet keeps.

Read against what the decisions rest on, neither move breaks one: a filing writes its pack and renames its slot into place within the slot's directory as it does across two, a collection lists one directory and passes over the slot by its name, and a restore probes one address.

## Settled

- **The name.** The half kept for reuse is the cabinet, a collector's cabinet keeping each piece in a drawer of its own, catalogued by what it is, for whoever comes asking — each unit in a slot of its own, each source and module under its digest, any project reusing what its inputs name — the image the language's own name and `wonder` take theirs from. "The store", which names `.curios/` as a whole, what a command emits there and what is filed for reuse alike, retires: `.curios/` is itself, a command's products are named by what made them, and what is kept for reuse is the cabinet.
- **The variable.** `CURIOS_CABINET` replaces `CURIOS_CACHE` and names a replacement of `.curios/cabinet/`: unset, each governing root has its own, an umbrella's members one; set, the directory it names is the cabinet of every run that sees it, so projects share one as they share a cache today. Nothing moves `.curios/` itself, whose products stay with the project that declared them, and there is still no divined default.
- **The slot.** A slot is the directory `cabinet/verdicts/<slot>/`, holding its record and index beside its packs, and `objects/` goes.
- **No migration.** The verdict schema tag (`VERDICT_SCHEMA`) moves with the slot's layout, and an old store is orphaned and rebuilt once.
- **What a command emits stays as it is**, by target, as Cargo's `target/<triple>/` stands beside `target/doc/`.
- **The peers.** Cargo 1.91 keeps intermediates in a `build-dir` apart from a `target-dir` whose layout is public, as Zig keeps `.zig-cache` apart from its `zig-out` prefix, and Cargo's build-dir layout v2 moves from grouping by content type to "each cacheable unit" in "a self-contained directory", for collection, locking and a cache shared across workspaces ([Call for Testing: Build Dir Layout v2](https://blog.rust-lang.org/2026/03/13/call-for-testing-build-dir-layout-v2)).

## Open

- **A run that sees `CURIOS_CACHE` set**: the variable ignored, as a retired form, or refused, naming `CURIOS_CABINET`. Settled at stage 1.
- **The code's names**: whether `Store`, `STORE` and the names that say store elsewhere follow the word, or stay as names of `.curios/` as a whole. Settled at stage 1.

## Stages

### 1. The cabinet

`Store` holds a cabinet, `.curios/cabinet/` or what `CURIOS_CABINET` names, and every shared family and `staging` under it; `built` and `documentation` stay in `.curios/`. Restated with it: `store.rs`'s `//!` map, `compiler/` included, and `shared`'s `///`; the reference's [Where things go](../../usage.md#where-things-go), its table and its prose; [The store goes where a manifest governs, and nowhere it was not pointed](../../../curios-package/README.md#the-store-goes-where-a-manifest-governs-and-nowhere-it-was-not-pointed), its heading and every link to it; every sentence of the tree that says "the store", by what it means, the cabinet or `.curios/`; and `.claude/rules/documentation.md`'s retired words, "the store" written as the cabinet or `.curios/`. Check: `curios-package`'s store tests (`store/test_support.rs`, `compiler/tests.rs`) and `curios-verdict`'s over the new paths, `CURIOS_CABINET` set where they set `CURIOS_CACHE`; a run with `CURIOS_CABINET` set leaving `.curios/` with the products alone.

### 2. The slot

`Store::verdict` and `Store::objects` become one slot directory, `cabinet/verdicts/<slot>/`, holding the record, the index and the packs, where `curios-verdict` files, collects and restores; `objects/` goes, and `VERDICT_SCHEMA` moves. Restated with it: [A slot's objects are kept in packs beside it, at its address](../../../curios-verdict/README.md#a-slots-objects-are-kept-in-packs-beside-it-at-its-address), [What no slot names is collected five days after a collection first finds it so](../../../curios-verdict/README.md#what-no-slot-names-is-collected-five-days-after-a-collection-first-finds-it-so) and `store.rs`'s map. Check: `curios-verdict`'s filing, collection and restore tests at one directory, and a slot filed under the old tag left unread.

## Verification

Each stage: `cargo xtask workspace lint`, the stores' tests, `cargo xtask workspace test -E 'package(curios-package) | package(curios-verdict)'`, and the command line's tests that read `.curios/` (`curios/tests/payload.rs`, `lint.rs`, `document.rs`, `wonder/`, and `curios/src/tests/packages.rs`); then a program run twice over a fresh cabinet, the second run reusing every unit.

## Next step

A ready delivery: stage 1.

## Retirement

Once both stages land, the reference and the decisions state the cabinet, nothing cites this file, and it is deleted.
