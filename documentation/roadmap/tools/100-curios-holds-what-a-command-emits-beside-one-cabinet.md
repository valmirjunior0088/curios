# `.curios/` holds what a command emits beside one cabinet

Adopted by the user on 2026-11-19: `.curios/` holds a command's products by target, `native/`, `browser/` and `documentation/`, beside one directory, the **cabinet**, holding whole the half kept for reuse, which one variable, `CURIOS_CABINET`, moves; and a unit's slot is one directory of the cabinet holding its record, its index and its packs. A reader then tells the two halves apart by the tree rather than by the reference's table, a project shares, moves or clears its cabinet as one directory, and a slot is filed, collected and restored at the one address [A slot's objects are kept in packs beside it, at its address](../../../curios-verdict/README.md#a-slots-objects-are-kept-in-packs-beside-it-at-its-address) already gives it.

## What is there today

`Store` (`curios-package/src/store.rs`) places nine entries side by side in `.curios/` (`STORE`): the products, at `built` and `documentation`, in the project's own, and the shared half — `sources/`, `foreign/`, `verdicts/`, `objects/`, `payloads/`, `compiler/` and `pin`'s transient `pinning.<pid>` (`staging`) — at `shared`, the directory `CURIOS_CACHE` names or else the project's `.curios/` itself, where the two halves interleave and only the reference's table ([Where things go](../../usage.md#where-things-go)) tells which the variable moves. A unit's record and index stand at `verdicts/<slot>` (`Store::verdict`) and its packs at `objects/<slot>/<pack>` (`Store::objects`), one address in two trees. The module's `//!` map lists eight families and leaves out `compiler/`, which the reference lists.

**No caller wants both halves.** The products are asked for by `Home::built` (`curios-package/src/selection.rs`) and `document`'s default directory (`curios/src/main.rs`), each opening a whole `Store` for one path under the project; every other path by `Governing::store` (`curate`, the graph walk, `curios/src/link.rs`), `Pinning::store` and `Verdicts::at`.

**The library reads the variable.** `shared` reads `CURIOS_CACHE` inside `curios-package`, the one environment variable a library crate of the workspace reads — `NO_COLOR` is read by the command line (`curios/src/main.rs`). Its `///` holds every suite hermetic because "nothing in a suite sets the variable", which the shell running the suite decides: at `ef6a83175`, `cargo xtask workspace test -E 'package(curios-package) | package(curios-verdict)'` under a `CURIOS_CACHE` naming an empty directory fails 35 of its 254 tests, and leaves in that directory the sixteen slots, 39 packs, four payloads, three sources and one foreign module its temporary projects filed.

**The tests restate the layout.** `the_families_do_not_share_a_namespace` (`store/tests.rs`) holds that nothing lands under `.curios/executables`, a family nothing writes, so two of its assertions hold whatever the layout; and 37 lines of tests in nine files spell the shared families as literals (`.curios/verdicts`, `.curios/objects`, `join("payloads")`) where a path of the cabinet is computed from it.

**A slot's staging is swept by nothing.** `replace` (`curios-verdict/src/slot.rs`) stages a slot at `<slot>.<pid>.part` beside it and says one a crash leaves "is overwritten by the next write from the same process", which another process never is; a pack's staging is swept by its address's collection (`Packs::collect`), and a unit slot's, in `verdicts/`, stands outside the directory that collection lists.

**The word.** "The store" names `.curios/` as a whole or its shared half in the reference, the decisions, the crates' `README.md`s and rustdoc, the command line's help (`compile --output`, `document --output`, `pin --url`), the refusal `unwritable store` (`curios-package/src/curate.rs`), `Contract`'s `Access`, `.claude/rules/pipeline.md`'s heading and `CLAUDE.md`'s pointer to `.curios/sources/`: some 950 occurrences of "the store", "a store" and "store's" across the tree, a part of them the disk's and the rest the metavariables', the universe solver's and Wasmtime's own stores. The verb stays the language's: a unit is stored, and a unit's `store` node narrates its filing.

## Settled

- **The name.** The half kept for reuse is the cabinet, a collector's cabinet keeping each piece in a drawer of its own, catalogued by what it is, for whoever comes asking — each unit in a slot of its own, each source and module under its digest, any project reusing what its inputs name — the image the language's own name and `wonder` take theirs from. "The store", which names `.curios/` as a whole, what a command emits there and what is filed for reuse alike, retires: `.curios/` is itself, a command's products are named by what made them, and what is kept for reuse is the cabinet.
- **The variable.** `CURIOS_CABINET` replaces `CURIOS_CACHE` and names a replacement of `.curios/cabinet/`: unset, each governing root has its own, an umbrella's members one; set, the directory it names is the cabinet of every run that sees it, so projects share one as they share a cache today. Nothing moves `.curios/` itself, whose products stay with the project that declared them, and there is still no divined default.
- **The command line reads it.** `curios` reads `CURIOS_CABINET` once, as it reads `NO_COLOR`, and hands what it names to whatever opens a cabinet; a library crate reads no environment, so a suite is hermetic whatever the shell running it holds.
- **A retired `CURIOS_CACHE` is ignored**, as the old layout is orphaned: a run that sees it set uses the cabinet `CURIOS_CABINET` or the project names.
- **The code follows the word.** `Store` splits by consumer into `Cabinet`, the shared half, and the products' two paths, and `STORE` becomes the name of `.curios/` itself; a name that says store for either follows.
- **The slot.** A slot is the directory `cabinet/verdicts/<slot>/`, holding its record and index beside its packs, and `objects/` goes.
- **No migration.** The verdict schema tag (`VERDICT_SCHEMA`) moves with the slot's layout, and an old store is orphaned and rebuilt once.
- **What a command emits stays as it is**, by target, as Cargo's `target/<triple>/` stands beside `target/doc/`.
- **The peers.** Cargo 1.91 keeps intermediates in a `build-dir` apart from a `target-dir` whose layout is public, as Zig keeps `.zig-cache` apart from its `zig-out` prefix, and Cargo's build-dir layout v2 moves from grouping by content type to "each cacheable unit" in "a self-contained directory", for collection, locking and a cache shared across workspaces ([Call for Testing: Build Dir Layout v2](https://blog.rust-lang.org/2026/03/13/call-for-testing-build-dir-layout-v2)).

## Stages

### 1. The cabinet

`Cabinet` holds every shared family and `staging` under `.curios/cabinet/` or the directory the command line hands it from `CURIOS_CABINET`; `built` and `documentation` stay in `.curios/`, asked for by the two callers that want them. Restated with it: the module's `//!` map, `compiler/` included, and the variable's `///` where the command line reads it; every protocol that names `CURIOS_CACHE` (`curios-cps/README.md`, `documentation/roadmap/tools/000-findings.md`); the reference's [Where things go](../../usage.md#where-things-go), its table and its prose; [The store goes where a manifest governs, and nowhere it was not pointed](../../../curios-package/README.md#the-store-goes-where-a-manifest-governs-and-nowhere-it-was-not-pointed), its heading and every link to it; the namespace test's assertions, against the families written; and every test's path of a shared family, computed from `Cabinet`. Check: `curios-package`'s and `curios-verdict`'s store tests over the new paths; a command line test running a program with `CURIOS_CABINET` set, leaving `.curios/` with the products alone and the named directory with every family; the suites passing with `CURIOS_CABINET` and `CURIOS_CACHE` set in the shell that runs them.

### 2. The slot

`Cabinet::verdict` and `Cabinet::objects` become one slot directory, `cabinet/verdicts/<slot>/`, holding the record and index in one file beside the packs and the address's collection, where `curios-verdict` files, collects and restores; `objects/` goes, `VERDICT_SCHEMA` moves, and a slot's staging is swept by its address's collection as a pack's is. Restated with it: [A slot's objects are kept in packs beside it, at its address](../../../curios-verdict/README.md#a-slots-objects-are-kept-in-packs-beside-it-at-its-address), [What no slot names is collected five days after a collection first finds it so](../../../curios-verdict/README.md#what-no-slot-names-is-collected-five-days-after-a-collection-first-finds-it-so), `slot.rs`'s `//!` and `replace`'s `///`, and the module's map. Check: `curios-verdict`'s filing, collection and restore tests at one directory; a slot's staging older than the grace swept by a collection; a slot filed under the old tag left unread.

### 3. The word

Every sentence of the tree that says "the store" for `.curios/` or its half kept for reuse says, by what it means, the cabinet or `.curios/`: the decisions, the crates' `README.md`s, rustdoc and comments, the command line's help, the refusal `unwritable store`, `Contract`'s `Access`, `.claude/`'s rules and commands, and `CLAUDE.md`; and `.claude/rules/documentation.md`'s retired words gain "the store", written as the cabinet or `.curios/`. Check: `cargo xtask workspace lint`, the command line's tests that pin a help text or a refusal, and no sentence left saying it, by a search of the tree.

## Verification

Each stage: `cargo xtask workspace lint`, the stores' tests, `cargo xtask workspace test -E 'package(curios-package) | package(curios-verdict)'`, and the command line's tests that read `.curios/` (`curios/tests/payload.rs`, `lint.rs`, `document.rs`, `wonder/`, and `curios/src/tests/packages.rs`); then a program run twice over a fresh cabinet, the second run reusing every unit.

## Next step

A ready delivery: stage 1.

## Retirement

Once the three stages land, the reference and the decisions state the cabinet, nothing cites this file, and it is deleted.
