# curios-package

What a Curios package is, and everything that reads one: the `curios.toml` manifest, the walk that decides which manifest governs an invocation, the resolver that turns a declared dependency into a module tree, and the store the results are filed in. It sits beside the compiler boundary rather than under it — `curios-pipeline` folds its stages over whatever scope it is handed, and deciding that scope is a product's job. The four laws the crate enforces are stated below, where its comments cite them by number; the subsystem's invariants and refusal discipline belong to the crate rustdoc, and the command-line and manifest reference to [usage.md](../documentation/usage.md).

## Design

### The four laws

1. **Declaration decides; location does not.** Modules exist because a header declares `mod`, artifacts because the manifest declares them, members because the umbrella enumerates them — a file nothing names is inert, wherever it sits. The two exceptions are a package's own `lib.crs` and `exe.crs`, whose presence beside the manifest is their declaration, for the reason `layout.rs` states.
2. **Identity is declared exactly once, by its owner.** A package names itself, every consumer refers to it by that name, and the filesystem spells structure, never names; no identity meaningful only in the compilation that assigned it is ever stored.
3. **Membership organizes; dependency compiles.** The umbrella's tree decides where the store goes and what a marker may resolve to; only declared dependencies order compilation, and neither implies the other. Declared dependencies also bound it: a unit may name the prefixes its manifest declares and `/std`, and nothing else — a transitive dependency is in the fold, and writing its prefix is refused by name.
4. **A refusal fires early and names both parties.** Conflicts, collisions, cycles and missing obligations are diagnosed before elaboration, against the file somebody wrote, never surfaced as an unbound name or a conversion failure holding no span.

### A stored unit is reused whole, and a question compiles over it

**Decision.** A build files a unit whole and reuses it whole. A question about an edited unit compiles over the unit the store holds, reusing every item the edit did not reach, through the `Cache` seam `curios-verdicts` implements over this crate's store ([A stored unit is a baseline for an item-level recompile](../documentation/design/compilation/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md)); nothing here recognizes a declaration. A dependency's verdict is cached against the compiler that reached it and verified against the files it was compiled from, so it is certified once when stored ([Cached verdicts](../documentation/design/soundness/admission/cached-verdicts.md)).

**Rejected.** Parallel per-item certification — a serial define-all phase, then one `Kernel` per item over a shared read-only environment, verdicts sorted by item index. It spends concurrency inside the trusted base, where parallel verdicts equal serial verdicts becomes a thing to prove, to speed a cost paid once per dependency; any parallelism would also be native-only, since `curios-js` compiles `curios-cert` to a target without threads. Narrowing what a compiler upgrade invalidates is sequential, outside the trusted base, and comes first. `curios-prelude-archive`'s `stored_prelude_measurements` retakes what certifying a unit costs.
