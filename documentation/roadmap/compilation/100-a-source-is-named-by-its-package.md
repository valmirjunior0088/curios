# A source is named by its package

**The benefit.** Every source a package holds is named by the package and its path under the package's root, holds the file it was read from only within the process that read it, and is recorded by its name ([A source is named by its package and read from its file](../../design/compilation/a-source-is-named-by-its-package-and-read-from-its-file.md)). What is left is that a released compiler, and every executable it writes, carries no directory of the machine that built them. The project has decided to adopt it.

Its next step is stage 1, a ready delivery. It waits on nothing.

## What this builds on

- **The release is built by two steps.** `launcher build` (`xtask/tasks/launcher.toml`) builds the launcher every executable embeds, and `compiler build` (`xtask/tasks/compiler.toml`) the compiler that embeds it; `compiler test` runs against what `compiler build` filed. A step's braces are filled with built-ins, `{target}` and `{host}` (`xtask/src/places.rs`), and with variables a file states, a command's output among them (`xtask/src/values.rs`).
- **What Curios itself stores names no directory.** The prelude's images hold their sources' names alone (`no_image_holds_the_directory_it_was_built_in`, `curios-prelude`).

## What is wrong today

Figures counted by `strings` over the released 0.16.24, or over a launcher built at `b1e9c8f37` where it says so.

- **What Rust records of the build travels with the compiler and with every executable it writes.** A panic location names the file it is written in. The released 0.16.24 holds `/home/runner/work/curios/curios/target/release/build/…/out/…` 245 times, from code its dependencies' build scripts generate, and `/home/runner/.cargo/registry/src/…` 733 times. The launcher it embeds in every executable it writes holds the registry's directory 201 times, so a program compiled by a compiler built on a person's machine carries that person's home directory; where `rust-src` is installed, the toolchain's directory as well (29 times over a launcher built at `b1e9c8f37`).

## What the peers do

- **Cargo trims what a binary embeds.** `trim-paths` writes the current package relative to its root, a dependency as `name-version`, and the toolchain as `/rustc/<hash>`, and where `rust-src` is installed the local path is embedded unless remapped ([RFC 3127](https://rust-lang.github.io/rfcs/3127-trim-paths.html)). It is unstable on the pinned cargo 1.99.1, which refuses a manifest naming it; rustc's `--remap-path-prefix`, what it lowers to, is stable. Adopted: the release remaps what Rust embeds.

## Decisions

- **What Rust records of the release build names no directory of it.** The steps that build the compiler and its launcher as they are released, `compiler build` and `launcher build`, hand rustc `--remap-path-prefix` for the target directory, Cargo's home and the toolchain's root, each by a placeholder of the task files and each to a word of its own, `target`, `cargo` and `rust`; Cargo already hands rustc the workspace's own crates by relative path. Cargo keeps artifacts built under other flags beside the rest (probed on cargo 1.99.1), so no other step builds again.

## Stages

1. **What Rust records of the release build names no directory of it.**
   - Lands: the remaps in `xtask/tasks/compiler.toml` and `xtask/tasks/launcher.toml`, and the placeholders for Cargo's home and the toolchain's root.
   - Check: `compiler test` finds none of the target directory, Cargo's home, the toolchain's root and the workspace in the compiler `compiler build` filed, nor in an executable that compiler writes.

## Verification

The full gate, which runs `launcher build`, `compiler build`, `compiler test` and the task files' own suites. Only the bytes of the compiler and the launcher move.

## Rejected

- **Cargo's `trim-paths`**: unstable on the pinned toolchain. **The remaps stated in `.cargo/config.toml`**: it cannot name a directory of the machine it runs on, and would remap every build, a developer's backtraces among them. **The remaps in the release workflow alone**: a local `compiler build` would be another binary than the released one, and its check could run only in continuous integration.

## Completion and retirement

The release's remaps go to the descriptions of the steps that make them and to [the decision](../../design/compilation/a-source-is-named-by-its-package-and-read-from-its-file.md). Once that is recorded and no reference to this specification remains, this file is deleted.
