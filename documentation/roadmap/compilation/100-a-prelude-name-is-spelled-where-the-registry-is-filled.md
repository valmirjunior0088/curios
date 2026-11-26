# A prelude name is spelled where the registry is filled

**[Writing Rust in Curios](../../../.claude/rules/rust.md) states that no stage spells a prelude name and that a value relative to another is computed from the source constant, and one reader still spells the roots the registry's `roots` group states (`RootSyntax`, filled in `curios-module/src/registry.rs`): the prelude's restorer; its next step is stage 8, a ready fix.** It takes the roots from the group, as every other reader does, save the two row names the wire states for itself.

## What is wrong today

Each is read at `f76966ced`.

- **The restorer names its images and its roots by literal.** `curios-prelude/src/restore.rs` includes `sys.rkyv`, `std.rkyv` and their packs by name, and `root_prefixes` reads each root's prefix off those names, which `each_root_claims_the_prefix_its_name_spells` holds to the restored units' mounts.

Out of scope: Curios written for a reader — a diagnostic's advice (`curios-surface/src/parse/`), the command line's help (`curios/src/cli.rs`), the package `curios new` writes (`curios-package/src/new.rs`) and the bench's programs — and a path on disk, `curios-prelude-build/std/`, which the manifest beside it names, not the directory.

## Settled

- **The restorer reads its roots off its build.** `include_bytes!` takes a file's name as a literal, so the images' file names reach it through variables the build script sets from what `curios-prelude-build` answers: the prelude's roots in the fold's order, read off the registry, and the files it images each under. Each root's word reaches it through a variable of the same kind, which `root_prefixes` reads, since an archived unit keeps its mounts inside its archived lowering and reading them there would restore what `root_prefixes` exists to leave unrestored.
- **The wire keeps its names.** A host row imports under its declaration's whole path, `/std/internal/Handle/handle_read` (`HostOp::declared`), and the runner's row is `/test/report` (`TEST_REPORT`), each the ABI's own statement of a declaration the library makes, as [`curios-abi`'s README](../../../curios-abi/README.md) already argues for a row's module: no Rust macro writes Curios, so `the_library_declares_every_row_a_host_answers` (`curios-prelude/src/tests.rs`) and `the_runner_declares_the_scheduler_a_test_tail_calls_and_the_row_its_runner_answers` (`curios-pipeline/src/standard/tests.rs`) hold the two to each other in both directions. Handing the root to `HostOp::declared` fails where it is read: `curios-native`, which is the launcher, and `curios-browser` reach no registry. A host row named by its path below the library is the README's rejected namespace for the standard library's rows, and gives a row a second name beside its declaration's.
- **`Namespace::Sys` keeps its own word**, a wire word and not the root.

## Stages

**8. The restorer reads its roots off its build.** `curios-prelude-build` answers the prelude's roots and each one's files, which its own build reads too; the build script sets a variable for each root's word, image and pack; `restore.rs` includes the files and names the roots through them; and `each_root_claims_the_prefix_its_name_spells` is restated to hold the restored units' mounts to the registry's roots as well as to `root_prefixes`. It waits on nothing. Check: `curios-prelude`'s suite and `cargo xtask compiler caches prelude` pass, and the restated test fails where the build script names the roots in the other order.

## Verification and retirement

Each stage's check, then `cargo xtask format`, `cargo xtask compiler lint`, `cargo xtask jig lint`, the suite and `cargo xtask docs build`. No production Rust spells a root outside the fill, save the ABI's two row names and `Namespace::Sys`'s word, which a search of the tree for each root's word as a string shows; the images are byte-identical to `f76966ced`'s, so no figure moves. Once stage 8 has landed, `curios-common`'s registry decision and `curios-module`'s README state the roots group beside the names, and this file is deleted.
