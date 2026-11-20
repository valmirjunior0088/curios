# A manifest's named declarations and a dependency's source are told by their keys

Adopted by the user on 2026-11-19: every repeated declaration of a manifest that has a name is a table keyed by it, and a dependency's source is told by the key its row states. TOML's grammar then holds what two refusals hold today, `curios pin` writes a provider at its key rather than searching an array for it, and one rule covers a manifest's tables — a table means the keys it states — as [A foreign row states a delivery for each target](../../design/tools/a-foreign-row-states-a-delivery-for-each-target.md) already states it of a delivery, rejecting "one table whose meaning depends on a field beside it".

## What is there today

`Document` (`curios-package/src/manifest.rs`) reads a dependency and a catalog pin as entries of a table keyed by package name, `[dependencies]` and `[catalog]`, and an executable and a foreign provider as rows of an array of tables, `[[executables]]` and `[[foreign]]`, each carrying its `name` ([The manifest](../../usage.md#the-manifest)). So a row's uniqueness is a refusal of its own, "duplicate executable" (`ExecutableRow::executable`) and "duplicate foreign row" (`ForeignRow::foreign`), where TOML refuses a key written twice; a refusal inside a row is placed by the row's index (`Step::Row`), and a delivery's by the provider's name (`Step::Named`, `Delivery::bytes`); `curios pin` finds a provider by searching the array for its `name` (`curios-package/src/pin.rs`); and a provider's tables are written `[foreign.native]` and `[foreign.answers]`, which TOML reads as the last row's. Every repeated declaration is told apart by a name its writer gives — a dependency by its package, an executable by the word `run` takes, a provider by the handle `pin` takes — and Cargo's reason for its `[[bin]]` rows, a target's name inferred from its path, holds of none.

`DependencyRow` reads `source = "git"`, `"path"`, `"member"` or `"catalog"` and holds the row's other fields to it, `json = { source = "git", url = "…", rev = "…", hash = "c1:…" }` (`Resolver`), while a foreign row's target table is a carried or a fetched module by which of `path` and `url` it states. `DependencyRow`'s doc defends the tag as "what a reader looks for first", and the union for telling a row missing its pin which pin; a row keyed by its source states it first too, and decodes as the same union with the same refusals.

## Settled

- **The convention.** A repeated declaration with a name is a table keyed by it, as `[foreign.answers]` is keyed by declaration so that "a row states one export a declaration by its table's shape" ([A module answers its own package's declarations](../../../curios-package/README.md#a-module-answers-its-own-packages-declarations)), and as Cargo's `[dependencies]`, npm's `bin` and Zig's `.dependencies` are keyed; `members`, a list of paths, stays an array.
- **Executables.** `[executables]` is keyed by name, each a table whose `path` stays optional and defaults to `<name>.crs`: `bench = {}`, `serve = { path = "tools/serve.crs" }`, its refusals placed at the key.
- **Foreign providers.** `[foreign]` is keyed by the provider's handle — `[foreign.sha2.native]`, `[foreign.sha2.browser]`, `[foreign.sha2.answers]` — its `name` field gone, and `pin` writes at the key.
- **Uniqueness.** The two duplicate refusals are deleted: a key written twice is TOML's refusal at the second, which names one party where [the four laws](../../../curios-package/README.md#the-four-laws) ask both, as Cargo's does.
- **The old shape.** A manifest still written `[[executables]]` or `[[foreign]]` meets the decoder's own refusal at the key, that an array was written where a table is read, with no hint or code for the past form.
- **Sources.** A dependency's source is told by the key it states — `json = { git = "…", rev = "…", hash = "c1:…" }`, `shape = { path = "../shape" }`, `base = { member = true }`, `x = { catalog = true }` — `Resolver` and the `source` key gone, two sources stated refused at the second's key, and a marker's value anything but `true` refused at it. The peers tell a dependency's kind by its keys: Cargo, Gleam and uv write `{ git = … }` and `{ path = … }`, Cargo and uv a workspace's own as `{ workspace = true }`, Zig's `build.zig.zon` `url` with `hash` or `path`, "mutually exclusive", and Julia's `[sources]` `url` with `rev` or `path`.

## Stages

Every manifest the tests write is restated with the stage that changes its shape: an array header followed by its `name` line becomes the keyed header, and a provider's tables follow its key, a mechanical rewrite of about 150 rows across `curios-package`, `curios` and `curios-verdict`, then the prose, the refusal texts and the expectations that name the old forms by hand.

### 1. Executables keyed by name

`Document::executables` is a table of executable rows by name, `ExecutableRow`'s `name` gone, every refusal placed at the key, and "duplicate executable" deleted; `Package::discovered`'s "a row wins over the file" restated; `selection.rs`'s texts that send a reader to `[[executables]]`, the convention in `curios-package`'s README, and the reference's manifest table and prose. Check: an executable's path refused at its key, a key written twice refused where it is read, and `selection`'s refusals naming the keyed form.

### 2. Foreign providers keyed by handle

`Document::foreign` is a table of providers by handle, `ForeignRow`'s `name` gone, `Step::Named` deleted for the key with its arms in `refused.rs` and `decode.rs`, and "duplicate foreign row" deleted; `pin`, `curate`, `served`, `layout`, `hash`, `curios/src/link.rs` and `curios/src/cli.rs` read and write at the key; `curios-browser`'s page and its test, the reference and [A foreign row states a delivery for each target](../../design/tools/a-foreign-row-states-a-delivery-for-each-target.md) restated. Check: a provider's missing table refused at its key, and `curios pin foreign` writing a new provider at its key.

### 3. Sources told by their key

`DependencyRow` decodes its source from the key it states, `Resolver` and `source` gone; `curios pin`'s rows, `curate`, the reference and `curios-package`'s README restated. Check: each kind read and each refusal standing at its key, `surplus field` and `missing field` naming the source by its key.

## Verification

Each stage: `cargo xtask workspace lint`, `cargo xtask workspace test -E 'package(curios-package) | package(curios) | package(curios-verdict)'`, and for stage 2 `cargo xtask compiler test`, which runs the page's tests (`curios-browser/src/page.test.mjs`).

## Next step

A ready delivery: stage 1.

## Retirement

Once the three stages land, the reference and the decisions state the keyed forms, nothing cites this file, and it is deleted.
