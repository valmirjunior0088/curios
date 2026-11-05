# A struct is a record or an owner

Held to [Visibility is decided at each hop](../design/visibility-is-decided-at-each-hop.md): a record's fields are all `pub`, an owner's all private, a scoped modifier sits only on a method narrower than its type, and Clippy's `partial_pub_fields` and `field_scoped_visibility_modifiers` hold the two kinds of struct. Neither lint is in the workspace's lint table, and the code departs from the rule at 289 sites.

## What is wrong today

- **281 fields carry a scoped modifier**, `pub(crate)` or `pub(super)`, across 23 crates, counted at `779e5e8ae` by `cargo clippy --workspace --exclude 'x*' --all-targets -- --cap-lints warn -W clippy::field_scoped_visibility_modifiers -W clippy::partial_pub_fields`. The densest files are `curios-emit/src/emission.rs` (22), `curios-anf/src/remap.rs` (13), `curios-cps/src/analysis.rs` (12), `curios/src/tests/laws/structural.rs` (11), `curios-anf/src/id.rs` (10) and `curios-cps/src/optimize/clone.rs` (10); 26 of the fields are in `test_support` modules, whose records another crate's tests read.
- **Eight structs mix `pub` and private fields**: in `curios-common/src/span.rs`, `curios-core/src/names.rs` (two), `curios-core/src/term/shape.rs`, `curios-emit/src/frame.rs`, `curios-module/src/into_core.rs`, `curios-package/src/pin.rs` and `curios-unit/src/indexed.rs`.
- **The lint table does not hold the rule.** The two lints are configured nowhere, so the decision's sentence that they hold the two kinds of struct is the intended rule and not the code's state; and the table denies warnings, so the rows cannot be added ahead of the sites without turning `workspace lint clippy` red.

## Goal

Every struct is a record or an owner by the decision's test, the two rows stand at `warn` in `Cargo.toml`'s `[workspace.lints.clippy]` and in `editors/zed/Cargo.toml`'s table, which `zed test` holds equal, and `workspace lint clippy` and `zed lint` pass.

## Settled

- **The rows at `warn`**, as the table's other rows are; `warnings = "deny"` makes a hit an error, which is what holds the rule.
- **A field some readers may touch and others may not is an owner's**, whose fields none touch and whose operations are its methods, as the decision states; a `pub(crate)` field one sibling module reads is read through a method, or the struct becomes a record.

## Open

- **A `test_support` record another crate's tests read** (26 fields). (a) Every field `pub`, the module's `test-support` gating being what bounds the audience (recommended: it is the decision's own answer, a record's fields are `pub`); (b) an owner with accessors the tests read through, which carries methods only a test reads.
- **The eight mixed structs.** Each is a record with a field that holds an invariant, or an owner with fields that hold none; which is read off whether any value keeps the invariants, struct by struct.

## Stages

1. **The sites, a crate at a time, from the leaves up**: `curios-common`, `curios-core`, `curios-algebra`, `curios-analysis`, `curios-parse`, `curios-wasm`, `curios-cps`, `curios-emit`, `curios-anf`, `curios-cert`, `curios-module`, `curios-surface`, `curios-unit`, `curios-pipeline`, `curios-verdict`, `curios-prelude`, `curios-document`, `curios-binaryen`, `curios-native`, `curios`. Each scoped field becomes `pub` or private behind a method, and each mixed struct a record or an owner. Check: `cargo xtask workspace lint`, the crate's tests, and the count above rerun, which falls to the crates not yet taken.
2. **The two rows in both lint tables.** Check: `cargo xtask workspace lint clippy`, `cargo xtask zed lint` and `cargo xtask zed test`.

## Retirement

Once both rows hold, the decision's sentence is the code's state; nothing references this specification, and it is deleted.
