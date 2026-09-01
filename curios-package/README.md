# curios-package

What a Curios package is, and everything that reads one: the `curios.toml` manifest, the walk that decides which manifest governs an invocation, the resolver that turns a declared dependency into a module tree, and the store the results are filed in. It sits *beside* the compiler boundary rather than under it — `curios-pipeline` folds its stages over whatever scope it is handed, and deciding that scope is a product's job. The four laws the crate enforces are stated below, where its comments cite them by number; the subsystem's invariants and its refusal discipline belong to the crate rustdoc, and the command-line and manifest reference belongs to [documentation/usage.md](../documentation/usage.md).

## Design

### The four laws

Every refusal and every placement in this crate follows from four rules, cited by number throughout its comments. They were first stated in the projects specification, which was retired into the places that own its facts; this is where the list itself lives now.

1. **Declaration decides; location does not.** Modules exist because a header declares `mod`, artifacts because the manifest declares them, members because the umbrella enumerates them — a file nothing names is inert, wherever it sits. The two exceptions are a package's own `lib.crs` and `exe.crs`, whose presence beside the manifest *is* their declaration, for the reason `layout.rs` states.
2. **Identity is declared exactly once, by its owner.** A package names itself, every consumer refers to it by that name, and the filesystem spells structure, never names; nothing positional — no identity meaningful only in the compilation that assigned it — is ever stored.
3. **Membership organizes; dependency compiles.** The umbrella's tree decides where the store goes and what a marker may resolve to; only declared dependencies order compilation, and neither implies the other.
4. **A refusal fires early and names both parties.** Conflicts, collisions, cycles and missing obligations are diagnosed before elaboration, against the file somebody wrote, never surfaced as an unbound name or a conversion failure holding no span.

### Three things measured and not built

Each was considered while building the package manager and declined for a stated reason, and the reasons outlive the work that turned them up. The figures behind them are not restated here; they are reproduced by `curios-prelude-archive`'s `stored_prelude_measurements`, which carries what it last printed beside the code that would check it.

**The `O(scope)` per-compile prologues.** Erasure projects the whole predecessor Core and re-seeds the elaboration context with every one of its definitions, and `Globals::of` copies every registry and builds a map of every definition — both on every compile, today, with one predecessor. Read from the code and **not measured**. Recorded because dependencies multiply each by their number, and because measuring before designing is what removed three items from the caching work.

**Parallel per-item certification.** Split the certifier's walk into a serial define-all phase and a parallel check-all phase, one `Kernel` per item over a shared read-only environment, verdicts sorted by item index for determinism. Per-item kernels settle binder identity without arithmetic: each is seeded at the same derived floor, above every identity in the module. A shared counter is ruled out — nondeterministic under work stealing, and the archive must stay byte-reproducible. Any parallelism must be feature-gated native-only, because `curios-js` compiles `curios-cert` to `wasm32-unknown-unknown`, which has no threads.

*Declined on measurement, not merely parked.* The estimate that motivated it claimed a saving larger than the whole operation costs, which is how an unattributed figure fails: nobody had timed a certification, so nothing contradicted it. What the measurement changes is not the size of the prize but who pays: a verdict is now cached against the compiler that reached it and verified against the files it was compiled from, so a dependency is certified once when it is stored and never again while both hold — the argument for believing that verdict is [Cached verdicts](../documentation/soundness/admission-without-judgment/cached-verdicts.md). Spending concurrency **inside the trusted base** — where *parallel verdicts equal serial verdicts* becomes something to prove — to speed up a once-per-dependency cost is the wrong trade. **Revisit if** first-build latency for a dependency, or a compiler upgrade re-certifying every cached dependency at once, becomes the complaint; and try narrowing what an upgrade invalidates before reaching for threads, since that is sequential and outside the trusted base.

**Incrementality within a unit.** Not declined on the merits — a different objective. The package manager needs a unit reused whole or recompiled whole; per-declaration fingerprinting, which is GHC's model, answers a question about editing your own code that nothing there asks.
