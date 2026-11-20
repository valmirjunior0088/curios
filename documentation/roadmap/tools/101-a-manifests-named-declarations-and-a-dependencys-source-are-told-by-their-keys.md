# A manifest's named declarations and a dependency's source are told by their keys

Adopted by the user on 2026-11-19: every repeated declaration of a manifest that has a name is a table keyed by it, and a dependency's source is told by the key its row states. TOML's grammar then holds what two refusals hold today, `curios pin` writes a provider at its key rather than searching an array for it, and one rule covers a manifest's tables — a table means the keys it states — as [A foreign row states a delivery for each target](../../design/tools/a-foreign-row-states-a-delivery-for-each-target.md) already states it of a delivery, rejecting "one table whose meaning depends on a field beside it".

## What is there today

`Document` (`curios-package/src/manifest.rs`) reads a dependency and a catalog pin as entries of a table keyed by package name, `[dependencies]` and `[catalog]`, and an executable and a foreign provider as rows of an array of tables, `[[executables]]` and `[[foreign]]`, each carrying its `name` ([The manifest](../../usage.md#the-manifest)). So a row's uniqueness is a refusal of its own, "duplicate executable" (`ExecutableRow::executable`) and "duplicate foreign row" (`ForeignRow::foreign`), where TOML refuses a key written twice; a refusal inside a row is placed by the row's index (`Step::Row`), and a delivery's by the provider's name (`Step::Named`, `Delivery::bytes`); `curios pin` finds a provider by searching the array for its `name` (`curios-package/src/pin.rs`); and a provider's tables are written `[foreign.native]` and `[foreign.answers]`, which TOML reads as the last row's. Every repeated declaration is told apart by a name its writer gives — a dependency by its package, an executable by the word `run` takes, a provider by the handle `pin` takes — and Cargo's reason for its `[[bin]]` rows, a target's name inferred from its path, holds of none.

`Package` holds what those rows declare as lists in the order written, `executables: Vec<Executable>` and `foreign: Vec<Foreign>`, each record carrying the `name` its row stated, so the names are unique only by the two duplicate refusals, where `dependencies` is a map keyed by name.

`DependencyRow` reads `source = "git"`, `"path"`, `"member"` or `"catalog"` and holds the row's other fields to it, `json = { source = "git", url = "…", rev = "…", hash = "c1:…" }` (`Resolver`), while a foreign row's target table is a carried or a fetched module by which of `path` and `url` it states. `DependencyRow`'s doc defends the tag as "what a reader looks for first", and the union for telling a row missing its pin which pin; a row keyed by its source states it first too, and decodes as the same union with the same refusals.

`curios pin` reads only the row it writes, so it can repair a row the manifest refuses, and nothing holds the shape of the tables it writes into: `row_mut` (`curios-package/src/pin.rs`) panics, exiting 101, where `foreign` is no array of tables — a keyed `[foreign.sha2.native]` included, the form this specification adopts — where a row's delivery is no table, `native = "m.wasm"`, where `dependencies` is no table, `dependencies = 3`, and where a dependency row is no table, `shape = "1.0"`.

## Settled

- **The convention.** A repeated declaration with a name is a table keyed by it, as `[foreign.answers]` is keyed by declaration so that "a row states one export a declaration by its table's shape" ([A module answers its own package's declarations](../../../curios-package/README.md#a-module-answers-its-own-packages-declarations)), and as Cargo's `[dependencies]`, npm's `bin` and Zig's `.dependencies` are keyed; `members`, a list of paths, stays an array.
- **Executables.** `[executables]` is keyed by name, each a table whose `path` stays optional and defaults to `<name>.crs`: `bench = {}`, `serve = { path = "tools/serve.crs" }`, its refusals placed at the key.
- **Foreign providers.** `[foreign]` is keyed by the provider's handle — `[foreign.sha2.native]`, `[foreign.sha2.browser]`, `[foreign.sha2.answers]` — its `name` field gone, and `pin` writes at the key.
- **Uniqueness.** The two duplicate refusals are deleted: a key written twice is TOML's refusal at the second, which names one party where [the four laws](../../../curios-package/README.md#the-four-laws) ask both, as Cargo's does.
- **The record.** `Package` keys executables and providers by name, as it keys dependencies: `executables: BTreeMap<String, Executable>` and `foreign: BTreeMap<String, Foreign>`, neither record repeating the name its key states, so uniqueness is the type's. The package's own `exe.crs` program is the entry under the package's name where no row claims it. A keyed table is read in name order, the `toml` crate's without `preserve_order`, so the programs, the files declared and a refusal listing executables or naming two providers follow names, as dependencies already do; whoever reads a provider or an executable reads its name off the key, and `declared_modules`'s tuple, gaining the name, becomes a struct.
- **The old shape.** A manifest still written `[[executables]]` or `[[foreign]]` meets the decoder's own refusal at the key, that an array was written where a table is read, with no hint or code for the past form.
- **Sources.** A dependency's source is told by the key it states — `json = { git = "…", rev = "…", hash = "c1:…" }`, `shape = { path = "../shape" }`, `base = { member = true }`, `x = { catalog = true }` — `Resolver` and the `source` key gone, two sources stated refused at the second's key, and a marker's value anything but `true` refused at it. The peers tell a dependency's kind by its keys: Cargo, Gleam and uv write `{ git = … }` and `{ path = … }`, Cargo and uv a workspace's own as `{ workspace = true }`, Zig's `build.zig.zon` `url` with `hash` or `path`, "mutually exclusive", Julia's `[sources]` `url` with `rev` or `path`, and Lake's `[[require]]` `git` with `rev`; Gleam writes a `source` tag only in the `manifest.toml` it generates, which no one writes by hand.
- **The pin flag.** `curios pin dependency` takes `--git <URL>` for the key it writes, as `--path` writes `path` and `--rev` writes `rev`, and as `cargo add --git` is spelled; `pin foreign` keeps `--url`, which writes `url`.
- **A pin states one source.** A pin repointing a dependency row clears every source key but the one it writes, so a `member = true` row pinned `--path` states `path` alone, where it would be refused for two sources by the read-back that holds a pin to the manifest's rules.

## Stages

Every manifest the tests write is restated with the stage that changes its shape: an array header followed by its `name` line becomes the keyed header, a provider's tables follow its key, and a tagged source becomes its key, a mechanical rewrite of about 150 array rows and 100 tagged sources across `curios-package`, `curios`, `curios-verdict` and `curios-browser`, then the prose, the refusal texts and the expectations that name the old forms by hand.

### 1. A pin refuses a table of another kind

`row_mut` refuses, at its key and in the decoder's words — what was written, and that the key is a table — a `foreign`, a `dependencies`, a dependency row or a delivery that is another kind of value, where it panics today. Check: the four manifests above, each refused at its key with nothing written.

### 2. Executables keyed by name

`Document::executables` is a table of executable rows by name, `ExecutableRow`'s `name` gone, every refusal placed at the key, and "duplicate executable" deleted; `Package::executables` a map by name and `Executable` without its `name`; `Package::discovered`'s "a row wins over the file" restated; `selection.rs`'s and `layout.rs`'s readers and the texts that send a reader to `[[executables]]`, the convention in `curios-package`'s README and `layout.rs`'s `//!`, and the reference's manifest table and prose. Check: an executable's path refused at its key, a key written twice refused where it is read, an `[[executables]]` array refused at its key, and `selection`'s refusals naming the keyed form.

### 3. Foreign providers keyed by handle

`Document::foreign` is a table of providers by handle, `ForeignRow`'s `name` gone, `Step::Named` deleted for the key with its arms in `refused.rs` and `decode.rs`, and "duplicate foreign row" deleted; `Package::foreign` a map by handle and `Foreign` without its `name`; `pin`, `curate`, `served`, `layout`, `hash`, `curios/src/link.rs` and `curios/src/cli.rs` read and write at the key, `pin` creating a provider as `[foreign.<handle>.<target>]`; `curios-browser`'s page and its test, the reference and [A foreign row states a delivery for each target](../../design/tools/a-foreign-row-states-a-delivery-for-each-target.md) restated. Check: a provider's missing table refused at its key, a `[[foreign]]` array refused at its key, and `curios pin foreign` writing a new provider at its key.

### 4. Sources told by their key

`DependencyRow` decodes its source from the key it states, `Resolver` and `source` gone; `curios pin dependency` takes `--git` and clears every other source's key; `curate`, `graph.rs`'s marker refusals, the reference, `curios-package`'s README and `curios-wonder`'s README restated. Check: each kind read and each refusal standing at its key, two sources refused at the second's, a marker's `false` refused at it, `surplus field` and `missing field` naming the source by its key, and a `member = true` row pinned `--path` written with `path` alone.

## Verification

Each stage: `cargo xtask workspace lint`, `cargo xtask workspace test -E 'package(curios-package) | package(curios) | package(curios-verdict)'`, and for stage 3 `cargo xtask compiler test`, which runs the page's tests (`curios-browser/src/page.test.mjs`).

## Next step

A ready delivery: stage 1.

## Retirement

Once the four stages land, the reference and the decisions state the keyed forms, nothing cites this file, and it is deleted.
