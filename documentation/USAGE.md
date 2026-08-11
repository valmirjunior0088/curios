# Usage

The complete command-line and package reference. The [README](../README.md) covers the happy path; this covers the rest.

## Running and compiling

`run` and `compile` take the same three forms, so what a bare invocation means never depends on which one you asked.

| Argument | Means |
| --- | --- |
| *(none)* | the governing package's sole executable, or the one `default` names when it declares several |
| an identifier | the executable declared under that name |
| anything ending in `.crs`, or holding a path separator | that file, standalone |

The dispatch is lexical and never probes the disk: an executable's name is a single identifier, so it can hold neither `.crs` nor a path separator, and the two spaces cannot overlap. `curios run scratch.crs` therefore means the file even when the package declares an executable called `scratch`.

A file argument brings no project with it — no manifest, no dependencies, not even the library of the package you are standing in. That is deliberate: project scope is reachable only through something a manifest declares, so a scratch file cannot quietly acquire one. When a scratch program does want the library, one `[[executables]]` line gives it one.

## What a package is made of

Two files are found by *presence* rather than declared, because they are the two things a package is rather than artifacts it opts into:

| File | Is |
| --- | --- |
| `lib.crs` | the package's library, mounted at its declared `name` |
| `exe.crs` | the package's own executable, run by that same name |

So the smallest complete package is one line of manifest:

```toml
name = "hello"
```

Neither stem enters a qualified name — `lib` and `exe` are spellings nothing can refer to, exactly so a package's own name never means two things at once. `lib.crs`'s modules load from the manifest's directory (`pub mod parse;` reads `parse.crs`), which is the one place a header's namespace is not its stem directory.

Everything else is declared. A second program needs a row, and its file is named after it:

```toml
name = "hello"

[[executables]]
name = "bench"          # compiled from bench.crs

[[executables]]
name = "serve"
path = "tools/serve.crs"  # unless the row says otherwise
```

A row always wins over the file, so `[[executables]] name = "hello"` with a `path` overrides the `exe.crs` convention for the package's own program.

With more than one executable and no `default`, a bare `curios run` refuses rather than guessing:

```toml
name = "hello"
default = "bench"
```

**The package root has one stem space.** `lib.crs`, every module it enumerates, and every executable compiled from a file directly beside the manifest all claim a stem there, and a stem claimed twice is refused naming both claimants:

```
the package root claims the stem `hello` twice: `mod hello` in `lib.crs` and the executable "hello"
```

## Starting one

```sh
curios new <DIR>
```

The directory's name is the package's, and it is checked before anything is written — so a name the manifest could not hold fails with an empty disk rather than half a package.

It writes every part a package has: the manifest, `lib.crs`, and `exe.crs`. There is deliberately no flag asking for only one of them. A package may be a library alone or a program alone — deleting either file says so, and the manifest decides — but *starting* one is not the moment to be asked, and a flag asking would only be answerable by somebody who already knows what the two are.

## Exit codes

Exit status is a tri-state, so tooling can tell "here is your goal batch" from "something is wrong" without parsing stderr.

| Code | Meaning |
| --- | --- |
| `0` | compiled, and for `run`, the program itself exited 0 |
| `1` | a hard error |
| `2` | the program contains written goals (`?`), and their report went to stderr |

A running program's own exit code passes through untouched, so `0` never hides a failure.

## Formatting

```sh
curios format <files…>        # rewrite in place
curios format --check <files…>  # write nothing, exit nonzero if anything would change
```

There is one canonical style and no options to configure it. Formatting is verified before anything is written — the output must reparse to exactly the same program, with every comment preserved — so a formatter defect refuses rather than corrupts.

## Dependencies

A dependency is pinned exactly, and its name is how every consumer refers to it. A package named `json` mounts at `/json` and no consumer may rename it, which is what lets two dependents on one package share it instead of compiling it twice.

```toml
name = "app"

[dependencies]
json = { source = "git", url = "https://github.com/you/json", rev = "…", hash = "c1:…" }
shape = { source = "path", path = "../shape" }
```

A `git` row requires all three of `url`, `rev` and `hash`. A `path` row requires only `path`. Two dependents that pin one name two different ways is a refusal naming both, and it fires before anything is compiled.

## Fetching

```sh
curios curate
```

`curate` materializes what the manifests reference, and it is the only part of the toolchain that reaches the network — the compiler itself never fetches. A delivered tree is accepted against its `hash` whatever transport produced it, so a mirror is no weaker than the origin, and a delivery that fails its hash is refused whoever fetched it.

## Umbrellas

Packages developed together sit under an umbrella, which declares `members` rather than a `name`, and may declare a `catalog` of pins its members draw on.

```toml
members = ["app", "base"]

[catalog]
json = { source = "git", url = "https://github.com/you/json", rev = "…", hash = "c1:…" }
```

A member reaches a sibling with `source = "member"` and a catalog pin with `source = "catalog"`. A catalog row on its own fetches nothing: activation lives in the package that names it.

An umbrella governs a package **only if it enumerates it**, so a directory nothing enumerates is governed by nothing above it, however deep it sits.

## Which manifest governs

The governing package is the one whose `curios.toml` sits in the working directory. There is no search above it, so what a command compiles is whatever an `ls` shows — and a subdirectory that holds modules rather than a manifest is not a package at all.

Only the umbrella is looked for further up, and only one that enumerates you governs you. When it does, the umbrella's directory is the governing root: that is where the store goes, and it is shared with your sibling members.

## Where things go

Everything generated lands under `.curios/`, beside the governing manifest — built executables, materialized sources, and compiled units. It is the only directory the toolchain writes into.

Set `CURIOS_CACHE` to share the content-addressed half across projects; unset, each project keeps its own. There is deliberately no divined default, because a toolchain that writes into a home directory nobody pointed it at is doing something the person who ran it did not ask for.

## Global flags

| Flag | Effect |
| --- | --- |
| `--manifest <PATH>` | use this `curios.toml` as the governing package's, instead of the working directory's |
| `--unit <DIR>` | mount the package in `DIR` ahead of the entry program, with no manifest edge; repeat for more, in dependency order |
| `--budget <STEPS>` | reduction steps each declaration may spend while type checking |
| `--print[=STAGES]` | dump selected intermediate representations to stderr |

`--manifest` overrides exactly which manifest is the package's. Which umbrella governs is still enumeration's answer, because a manifest cannot declare itself governed.
