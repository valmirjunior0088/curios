# A source is named by its package

**The benefit.** Every source a package holds is named by the package and its path under the package's root, and holds the file it was read from only within the process that read it ([A source is named by its package and read from its file](../../design/compilation/a-source-is-named-by-its-package-and-read-from-its-file.md)). What is left is that nothing else that outlives a process or leaves a machine holds a directory of the machine: a unit's record names what it read by name, so a store's units and the prelude's image are the same bytes wherever their package stands and a package moved or fetched elsewhere keeps its stored units, and a released compiler and every executable it writes carry no directory of the machine that built them. The project has decided to adopt it.

Its next step is stage 1, a ready delivery. It waits on nothing.

## What this builds on

- **A source is named by its package and read from its file** ([the decision](../../design/compilation/a-source-is-named-by-its-package-and-read-from-its-file.md)). `Source` (`curios-common/src/span.rs`) archives its name (`SourceName`) and holds its file within the process; a unit's source reads for its package (`PackageRoot`, `RootSource::reading_for`, `curios-module`) and resolves a name of that package back to its file (`PackageRoot::file_of`, `RootSource::packages`); a unit the store restores is read for the asking source's packages (`Unit::read_for`).
- **A store believes a unit by its record** ([Cached verdicts](../../design/soundness/cached-verdicts.md)). `Record::reads` (`curios-unit/src/stored.rs`) names each file a unit read by canonical path with the digest of its text. A hit requires every file to hold that text still (`unchanged`, `curios-verdict/src/verdicts.rs`) and to lie under the asking source's directories (`read_within`), which keeps a store `CURIOS_CACHE` shares from admitting one project's unit for another's files. A slot's address names no path (`unit_slot`, `entry_slot`, `curios-package/src/store.rs`).
- **The release is built by two steps.** `launcher build` (`xtask/tasks/launcher.toml`) builds the launcher every executable embeds, and `compiler build` (`xtask/tasks/compiler.toml`) the compiler that embeds it; `compiler test` runs against what `compiler build` filed. A step's braces are filled with built-ins, `{target}` and `{host}` (`xtask/src/places.rs`), and with variables a file states, a command's output among them (`xtask/src/values.rs`).

## What is wrong today

Figures counted at `b1e9c8f37`, or by `strings` over the released 0.16.24 where it says so.

- **A record names each file by its canonical path, and it travels.**
  - `std.rkyv` holds the checkout's directory once for each of the library's 271 files, in its record; `std.pack`, `sys.rkyv`, `sys.pack` and `documentation.rkyv` hold none. One tree built in two directories is two images.
  - A package compiled, moved to another directory and compiled again checks every unit again: its stored records name the directory it left. [Cached verdicts](../../design/soundness/cached-verdicts.md) names it a weakness.
  - Two projects holding one package's texts under a store `CURIOS_CACHE` shares compile it once each, and one's slot is no baseline for the other's (`another_projects_slot_is_no_baseline`).
- **What Rust records of the build travels with the compiler and with every executable it writes.** A panic location names the file it is written in. The released 0.16.24 holds `/home/runner/work/curios/curios/target/release/build/…/out/…` 245 times, from code its dependencies' build scripts generate, and `/home/runner/.cargo/registry/src/…` 733 times. The launcher it embeds in every executable it writes holds the registry's directory 201 times, so a program compiled by a compiler built on a person's machine carries that person's home directory; where `rust-src` is installed, the toolchain's directory as well (29 times over a launcher built at `b1e9c8f37`).

## What the peers do

- **Go keys its cache by what it reads, not where.** Without `-trimpath` a package's directory is hashed into its cache key, and the standard library is always treated as trimmed, so moving the toolchain keeps the cache ([cmd/go: don't include package dir in cache key when -trimpath is set](https://go.googlesource.com/go/+/aa680c0c49b55722a72ad3772e590cd2f9af541d%5E%21/)). Adopted: a record names what it read by package, so moving a package keeps its store.
- **Cargo trims what a binary embeds.** `trim-paths` writes the current package relative to its root, a dependency as `name-version`, and the toolchain as `/rustc/<hash>`, and where `rust-src` is installed the local path is embedded unless remapped ([RFC 3127](https://rust-lang.github.io/rfcs/3127-trim-paths.html)). It is unstable on the pinned cargo 1.99.1, which refuses a manifest naming it; rustc's `--remap-path-prefix`, what it lowers to, is stable. Adopted: the release remaps what Rust embeds.

## Decisions

- **A record names what its unit read by name, and a hit resolves each name through the asking source.** `Record::reads` is by name; `RootSource::reads` keeps each source it read, by its file within the process. A hit reads each recorded name as the file the asking source's package holds under it, through the overlay where the compilation reads through one. A name resolves only under the asking source's own roots, so a slot another project filed is believed only where this project's files hold the texts it was compiled from, after units holding what it was compiled after: a unit this project would compile. No containment is checked, and `read_within` goes. A slot another project filed for a package of the same name is a baseline as any slot is, and a package moved keeps its stored units. A unit that read a file for no package has no place in a store.
- **What Rust records of the release build names no directory of it.** The steps that build the compiler and its launcher as they are released, `compiler build` and `launcher build`, hand rustc `--remap-path-prefix` for the target directory, Cargo's home and the toolchain's root, each by a placeholder of the task files and each to a word of its own, `target`, `cargo` and `rust`; Cargo already hands rustc the workspace's own crates by relative path. Cargo keeps artifacts built under other flags beside the rest (probed on cargo 1.99.1), so no other step builds again.

## Stages

1. **A record names what its unit read by name.**
   - Lands: `Record::reads` by name, a hit resolving each name through the asking source; the store's read logs and the payload family's record the same way; `SCHEMA` and `PAYLOAD_SCHEMA` (`curios-package/src/store.rs`) move.
   - Goes: `read_within` and every clause asking it.
   - Restated: `a_slot_does_not_answer_for_another_projects_source`, now for its texts; `another_projects_slot_is_no_baseline`, which becomes a slot that is one; `the_std_record_names_every_authored_source_and_no_other`, over names; `RootSource::reads`' rustdoc; [Cached verdicts](../../design/soundness/cached-verdicts.md)' record and the weakness of moving a project; `curios-verdict/README.md`'s `earlier` decision; the record's clause of [the decision](../../design/compilation/a-source-is-named-by-its-package-and-read-from-its-file.md).
   - Check: a package compiled, moved and compiled again believes every unit; two projects holding one package's texts under one store compile it once; no byte of `sys.rkyv`, `sys.pack`, `std.rkyv`, `std.pack` or `documentation.rkyv` holds the directory the prelude was built in, as a test of `curios-prelude` that stays.
2. **What Rust records of the release build names no directory of it.** Independent of stage 1: neither waits on the other.
   - Lands: the remaps in `xtask/tasks/compiler.toml` and `xtask/tasks/launcher.toml`, and the placeholders for Cargo's home and the toolchain's root.
   - Check: `compiler test` finds none of the target directory, Cargo's home, the toolchain's root and the workspace in the compiler `compiler build` filed, nor in an executable that compiler writes.

## Verification

After stage 1, `cargo xtask workspace lint`, which builds and certifies the image, then the stage's checks by name with the suites of `curios-unit`, `curios-verdict`, `curios-module`, `curios-wonder`, `curios-pipeline` and `curios-prelude`; after stage 2, `cargo xtask compiler build` and `cargo xtask compiler test`, with `cargo xtask tools lint` and `cargo xtask tools test` for the task files. Stored bytes change: every record, and `std.rkyv`, which loses the directory; `sys.rkyv`, `sys.pack`, `std.pack` and `documentation.rkyv` stay the same bytes. No verdict moves, and a program's executable is the same bytes through stage 1, since it carries no source; stage 2 moves only the bytes of the compiler and the launcher. The full gate runs once, after the last stage.

## Rejected

- **Keeping `read_within`**: it refuses a record naming files this project could not have read, and a name resolves only to files this project holds.
- **Cargo's `trim-paths`**: unstable on the pinned toolchain. **The remaps stated in `.cargo/config.toml`**: it cannot name a directory of the machine it runs on, and would remap every build, a developer's backtraces among them. **The remaps in the release workflow alone**: a local `compiler build` would be another binary than the released one, and its check could run only in continuous integration.

## Completion and retirement

What a record names goes to `curios-unit`'s rustdoc and the record's clause of [the decision](../../design/compilation/a-source-is-named-by-its-package-and-read-from-its-file.md), with [Cached verdicts](../../design/soundness/cached-verdicts.md) restated; the release's remaps go to the descriptions of the steps that make them and to that decision. Once those contracts are recorded and no reference to this specification remains, this file is deleted.
