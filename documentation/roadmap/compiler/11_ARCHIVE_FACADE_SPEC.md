# One crate owns rkyv

`curios-profile` is the workspace's only `tracing` dependency: it names the pins in its own manifest rather than in the workspace table, exports `profile!`/`profile_span!`, and every consumer gates on a per-crate `profile` feature. Nothing else in the workspace may name `tracing`, and that is enforced by the dependency graph rather than by convention.

rkyv has no such owner. Eight crates name it — `curios-base`, `curios-abi`, `curios-core`, `curios-text`, `curios-elab`, `curios-ersd`, `curios-cert`, `curios-prelude` — each with `rkyv = { workspace = true }`, each with its own `archive` feature, and 127 type definitions carry the same annotation by hand:

```rust
#[cfg_attr(feature = "archive", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
```

## The distinction the workspace table cannot make

`Cargo.toml`'s `[workspace.dependencies]` row centralizes the *version and feature set*, and that much is already done. It does not centralize *authority*: any crate may add `rkyv = { workspace = true }`, and nothing says which crate is answerable for the choice. A dependency that exists in exactly one manifest cannot be added elsewhere without someone writing the version down again, which is a question a reviewer will ask.

That is why `curios-profile` keeps its pins locally and says so in a comment. The same reasoning applies here, and the root row is part of what has to go.

## Design

**A new leaf crate, `curios-archive`.** Zero workspace dependencies, exactly like `curios-profile`. It names rkyv with the feature set the root table currently carries and re-exports the derives and the crate itself. Every other crate depends on it and none names rkyv.

It must be a leaf, and that is forced rather than chosen: `curios-base` derives `Archive` on `Qualifier`, `Span`, `NumOp` and others, so the owner sits *below* `curios-base`, which is below everything.

**One crate, and the annotation stays a `cfg_attr`.** rkyv's derives expand to `::rkyv::` paths, so a crate that no longer depends on rkyv directly must redirect them — `rkyv_derive` 0.8.17 supports `#[rkyv(crate = …)]`, via `crate_path` in its attribute parser. With the derives imported in the module's own `use` block, a site reads:

```rust
#[cfg_attr(
    feature = "archive",
    derive(Archive, Serialize, Deserialize),
    rkyv(crate = curios_archive::rkyv)
)]
pub struct Qualifier { … }
```

which is about what it costs today. The concentration of authority is the goal; the line count is not.

**The attribute macro is deferred, not rejected.** `#[curios_archive::archived]` would collapse those four lines to one across 127 sites, but an attribute macro needs `proc-macro = true`, and such a crate can export *nothing else* — so it means a second crate on the serde/serde_derive pattern, and the workspace has none today. It is purely additive later: the annotation above already routes through `curios_archive`, so adding the macro rewrites the sites once and changes nothing else.

**A `macro_rules!` wrapper is rejected outright.** It cannot be an attribute, so it would have to wrap each type definition — and rustfmt does not format inside a braced macro invocation, which would leave 127 type bodies unformatted against a gate that runs stock `cargo fmt --check`. `curios-base`'s existing `name!` escapes this only because it generates a fixed shape with no author-written body.

## Rejected

- **Folding the facade into `curios-base`.** It is the one crate that could host it without a cycle, and it is still wrong: the principle is *one crate is the authority for one external concern*, and `curios-base` would then own shared foundations and serialization. The same principle says `curios-profile` should not move into `curios-base` either.
- **Giving this name to the prelude image producer.** `10_PRELUDE_ENVIRONMENT_SPEC.md`'s M5 creates a crate that elaborates and serializes the fixed prelude — *above* `curios-ersd`, where this one is a leaf below `curios-base`, so they cannot be one crate. The two compose instead of competing: this crate owns *archiving* as a capability, and M5's `curios-prelude-archive` is *an archive, of the prelude*. `PreludeArchive`, `archive.rs` and "archive-build time" keep meaning what they already mean.

## Out of scope

- Which types are archived, and the `archive` feature fan-out per crate. Both stay exactly as they are; only the spelling and the owner change.
- `curios-base`'s `archive.rs` rkyv adapters for arbitrary-precision integers. They are `With` implementations, not annotations, and move only if that turns out to be free.

## Tests

- The workspace builds with `rkyv` absent from every manifest but `curios-archive`'s, which is the whole claim and is checked by compilation.
- An archive round-trip that already exists must pass unchanged: this is a spelling change, and any behavioural difference is a defect.
- `curios-prelude`'s image is byte-identical across the change. The archive is build-scoped, so a differing image is a signal, not an expected consequence.

## Retirement criteria

Before this specification is deleted: `benchmarks/Dockerfile` carries a `COPY` line for each new crate, since it enumerates workspace members with no wildcard; `CLAUDE.md`'s ownership map names `curios-archive` the way it names `curios-profile`; and the root `[workspace.dependencies]` no longer has an `rkyv` row.
