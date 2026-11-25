# A prelude name is spelled where the registry is filled

**Not refined yet.** [Writing Rust in Curios](../../../.claude/rules/rust.md) states that no stage spells a prelude name and that a value relative to another is computed from the source constant, and the prelude's two roots, `sys` and `std`, are spelled in eight crates, one of which puts `/std` on the wire. Its approach is settled under *Settled*; what is left is under *Open*, and its next step is that bounded decision.

## What is wrong today

- **Each reader spells the roots.** `intrinsic_path` (`curios-surface/src/spelling.rs`), the documentation builder's chip (`curios-module/src/into_core/document.rs`), the package layout's implicit dependency (`curios-package/src/layout.rs`), the standard library's record lookup (`curios-wonder/src/document.rs`), the prelude's build (`curios-prelude-build/src/compile.rs`, `std_module.rs` and `sys_module.rs`'s `SYS_NAME`), its restorer's images and roots (`curios-prelude/src/restore.rs`), every name of the registry's fill (`curios-module/src/registry.rs`), the bench's standard-library measurement (`xbench/src/measurements/standard.rs`), and `HostOp::declared` (`curios-abi/src/host/ops.rs`).
- **The registry already says where a spelling lives.** Its shape is `curios-common`'s and names slots, never spellings, and `curios-module` fills it (`curios-common/src/syntax.rs`), where `TEST_UNIT` already stands as the `/test` prefix; the elaborator, `curios-core` and `curios-surface` read the filled registry they are handed. A root stated in `curios-common` would be the one spelling the shape crate holds.
- **The ABI sits below every reader and holds `/std`.** `curios-abi` depends on `curios-archive` and `curios-num` alone, and a host row's import is named by its `/std` path (`HostOp::declared`, read by `host_answers`, the browser's `rows` table and the native link), so `/std` is part of the wire contract, where `/sys` crosses only as shapes the registry maps (`WireType::Option` to `/sys/Option`, `curios-core/src/intrinsic.rs`). The WebAssembly namespace `sys` (`Namespace::Sys`) is another claimant of the word: the module of the imports a program always makes, unrelated to the root.

## Settled

- **The roots are slots of the registry**, filled beside its names in `curios-module/src/registry.rs`, each name of the fill reading its root from there.
- **A reader below `curios-module` takes the filled registry**, as `curios-surface` and `curios-core` already do; the prelude's restorer reads each image's prefix off the image rather than restating it, and the images' file names, which `include_bytes!` takes as literals, reach it through a variable the build script sets from the root.
- **`Namespace::Sys` keeps its own spelling**, a wire word and not the root.

## Open

- **The wire's `/std`.** The root handed to `HostOp::declared` by its callers, each of which holds the filled registry; a host row imported by its path below `/std`, so the wire states no root and the compiler and both runtimes name a host row relative to it; or the wire's own word for the standard library's rows, kept in the ABI and held to the root by the test that reads every row the library declares (`the_library_declares_every_row_a_host_answers`, `curios-prelude/src/tests.rs`).

## Stages

Stated once *Open* is settled.

## Verification and retirement

No production source spells a root outside the registry's fill, `Namespace::Sys`'s word aside, and the suite and the prelude's build pass. Once it lands, the registry's decision states the roots beside its names, and this file is deleted.
