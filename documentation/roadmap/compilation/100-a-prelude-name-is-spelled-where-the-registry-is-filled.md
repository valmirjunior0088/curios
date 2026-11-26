# A prelude name is spelled where the registry is filled

**[Writing Rust in Curios](../../../.claude/rules/rust.md) states that no stage spells a prelude name and that a value relative to another is computed from the source constant, and two readers still spell a root the registry's `roots` group states (`RootSyntax`, filled in `curios-module/src/registry.rs`): `curios-surface`, which writes `/sys` into the names it emits, and the prelude's restorer; its next step is stage 7, a ready fix.** Each takes the root from the group, save the two row names the wire states for itself.

## What is wrong today

Each is read at `f76966ced`.

- **The one reader below writes `/sys` into the names it emits.** `intrinsic_path` (`curios-surface/src/spelling.rs`) prefixes `"sys"` to an intrinsic's path, and every intrinsic a tree names is named through it: the printer's quoted `/sys` trees (`Quoted`, `print.rs`), which the documentation builder resolves back by their text; the reverse translation (`from_core.rs`), which every faithful print runs; `carrier_former`; and the elaborator's reading of a reader's spelling (`curios-elab/src/reduce.rs`). Neither the printer nor a `Spelling` — whose registry-less `Spelling::default` and `core_text` are made at 13 production sites, behind about 35 production faithful prints — holds a registry.
- **The restorer names its images and its roots by literal.** `curios-prelude/src/restore.rs` includes `sys.rkyv`, `std.rkyv` and their packs by name, and `root_prefixes` reads each root's prefix off those names, which `each_root_claims_the_prefix_its_name_spells` holds to the restored units' mounts.

Out of scope: Curios written for a reader — a diagnostic's advice (`curios-surface/src/parse/`), the command line's help (`curios/src/cli.rs`), the package `curios new` writes (`curios-package/src/new.rs`) and the bench's programs — and a path on disk, `curios-prelude-build/std/`, which the manifest beside it names, not the directory.

## Settled

- **A reader below `curios-module` takes the filled registry**, as `curios-core` and a `FormScope` already do: a `Spelling` is made with it and a faithful print takes it, the program printer's entries take it, as a parsed tree's printed view does (`Printed`, which stands where its `Display` stood), and `carrier_former` is handed it, as `intrinsic_former` is handed the root's word. `curios-surface`'s suites reach the filled registry through a dev-dependency on `curios-module`, the cycle Cargo admits as `curios-prelude`'s suite does.
- **The restorer reads each image's prefix off the image**, and the images' file names, which `include_bytes!` takes as literals, reach it through variables the build script sets from the roots.
- **The wire keeps its names.** A host row imports under its declaration's whole path, `/std/internal/Handle/handle_read` (`HostOp::declared`), and the runner's row is `/test/report` (`TEST_REPORT`), each the ABI's own statement of a declaration the library makes, as [`curios-abi`'s README](../../../curios-abi/README.md) already argues for a row's module: no Rust macro writes Curios, so `the_library_declares_every_row_a_host_answers` (`curios-prelude/src/tests.rs`) and `the_runner_declares_the_scheduler_a_test_tail_calls_and_the_row_its_runner_answers` (`curios-pipeline/src/standard/tests.rs`) hold the two to each other in both directions. Handing the root to `HostOp::declared` fails where it is read: `curios-native`, which is the launcher, and `curios-browser` reach no registry. A host row named by its path below the library is the README's rejected namespace for the standard library's rows, and gives a row a second name beside its declaration's.
- **`Namespace::Sys` keeps its own word**, a wire word and not the root.

## Stages

**6. The program printer writes `/sys` from the registry.** The printer holds the root its entries are handed, every entry taking the registry, Core text's and a shown term's among them, since an intrinsic node is printed alike in every dialect, and a parsed tree's `printed` takes it; `Quoted` names an intrinsic through `intrinsic_path`, which takes it. It waits on stage 7, whose spelling hands Core text's and a shown term's entries their registry. Check: the suites of `curios-surface`, `curios-module` and `curios`'s document tests pass, and the images stay byte-identical.

**7. A spelling is made with the registry.** `Spelling` holds the root it is made under, every constructor takes the registry and a faithful print takes it; `intrinsic_global`, `carrier_former`, the reverse translation and the elaborator's reading of a reader's spelling read `/sys` through it; `curios-surface`'s suites take the dev-dependency. It waits on nothing. Check: the suites of every crate touched pass, and the images stay byte-identical.

**8. The restorer reads its roots off its build.** The build script sets the images' file names from the roots, `restore.rs` includes them through those variables, and `root_prefixes` reads each image's prefix off the image; `each_root_claims_the_prefix_its_name_spells`, which a prefix read off the image would make vacuous, is restated to hold the images to the registry's roots. It waits on nothing. Check: `curios-prelude`'s suite and `cargo xtask compiler caches prelude` pass.

## Verification and retirement

Each stage's check, then `cargo xtask format`, `cargo xtask compiler lint`, `cargo xtask jig lint`, the suite and `cargo xtask docs build`. No production Rust spells a root outside the fill, save the ABI's two row names and `Namespace::Sys`'s word, which a search of the tree for each root's word as a string shows; the images are byte-identical to `f76966ced`'s, so no figure moves. Once stage 8 has landed, `curios-common`'s registry decision and `curios-module`'s README state the roots group beside the names, and this file is deleted.
