# A unit the store holds is a baseline, not a hit or a miss

## Status

Built, but for the parser. A store slot whose record disagrees is a baseline for a question: the unit is compiled over it, reusing every item the edit did not reach (`curios-pipeline/src/recompile.rs`), and the archived `/std` is the baseline for the package named `std`, which is how a question about a standard-library module is answered. Elaboration recovers per item (`curios-elab/src/elaborate/module/recovery.rs`): every refusal of one run is reported, and the dependents of a refused item are withheld. Item-level resynchronization in the parser is the one step not built — a parse failure still ends the module, and `curios-wonder/src/diagnostics.rs` names it as pending. The equivalence argument under [The argument](#the-argument) graduates to `documentation/design/toolchain/`, beside [A module is a compilation unit, and the prelude is an environment](../design/toolchain/a-module-is-a-compilation-unit-and-the-prelude-is-an-environment.md), whose reasoning it extends one level down, with the retirement of this specification.

## Why it exists

An editor asks about a file on every edit, and the answer costs the whole unit the file belongs to. For a package library of a few modules that is tolerable; for the standard library it is not, and the standard library is the largest Curios program there is and the one edited most. The same cost is paid by `curios test` on a library after a one-line change, by `document`, and by every executable compiled against a library that moved.

The cost is not where a reader would guess. It is not the kernel alone, and it is not a few pathological declarations. It is spread across nineteen hundred declarations of which the median costs ten milliseconds, plus whole-unit passes that were assumed to cost milliseconds and do not.

## What was measured

Release builds on an idle machine, the worktree's own Cargo timings for the two prelude build scripts, and `curios-profile`'s stream for the split inside the first.

| Pass over `/sys` and `/std` | Wall clock |
| --- | --- |
| Archive build script: lower, elaborate, erase, serialize | 52.5 s |
| Certification build script: the kernel walk | 46.9 s |

Inside the archive script, instrumented and therefore slightly inflated: item elaboration 39.7 s over 2064 items, of which 16.4 s is `finalize_definition` and mostly the universe solver; the text stage 7.9 s, of which parsing 138 files is 5.3 s; `finalize_and_check` 6.6 s, of which the proof and type totality obligations are 4.6 s; erasure 3.7 s.

Per declaration group, 1884 of them: median 12 ms, p90 39 ms, p99 131 ms, maximum 3050 ms on `/std/BigNat/add/raw_assoc`. The top hundred are 38% of the total and the top five hundred 71%. A typical edit touches a ten-millisecond declaration and pays a hundred seconds for it.

The kernel is not instrumented per item. Its total is roughly elaboration's, and nothing here assumes its distribution differs.

## What is already built

Every mechanism the design needs exists for whole units and is applied here inside one.

- **A predecessor is replayed, never re-checked.** `Established::over` seeds a scope's registries, definitions and witness heads into a fresh `Context`, and `elaborate_and_zonk_unit` states why the result is identical: a metavariable's context excludes top-level definitions, so an item elaborates against the same local context with or without scope.
- **The kernel skips by name.** `Globals` answers for judged names and `recheck_module_verdicts` judges only fresh ones; its module-wide passes run regardless and cost milliseconds.
- **The dependency graph exists after elaboration.** `Definition::mentions` over the elaborated module is what the kernel's `dependency_order` sorts by, and after zonk every resolved witness, derivation and deferred goal is an ordinary mention.
- **A stored item is position-free.** `validate_stored_identities` refuses any identity meaningful only in the compilation that minted it, which is exactly the property that lets an item be dropped into another compilation. Term equality excludes spans, so a moved line is not a change.
- **The baseline already carries both forms.** A `Unit` holds the lowered module in its `PreparedText` and the elaborated one in `core`, so the diff and the replay need nothing that is not already serialized.
- **The store already records reads per file.** A slot's `Record` names every file the unit read with its digest, verified through the editor's overlay.

## The design

**A unit the cache hands back is a baseline.** `Cache::get` returns a unit whose record agrees, and that is the empty case of this design: nothing changed, reuse everything. When the record disagrees, the slot still holds a unit compiled from an earlier text of the same sources, and that unit is the baseline for an item-level recompile instead of being ignored.

**`compile_unit_over(baseline, …)` in `curios-pipeline`.** Lower the unit whole. Diff lowered items by declared name against the baseline's lowered module. Take the reverse transitive closure over the baseline's elaborated `mentions` of every changed, added or removed name; transitivity is required because conversion unfolds bodies, so a dependent's judgment reaches through what it names. Replay every item outside the closure through `Established`, as a predecessor. Elaborate the closure in the lowered order. Reassemble one `Module`, run the whole-module passes over it, and judge it with `Globals` seeded from the reused items, so the kernel walks the closure alone. The result is a `Unit` indistinguishable from a whole compile.

**The archive is the baseline for the prelude.** The image gains the same `Record` a slot carries, written by the build script from the read log it already holds, so the fixed prelude's provenance is a recorded fact rather than a belief. `standard.rs`, the one module licensed to name the prelude, withholds an archived root from the scope exactly when a unit in the fold claims its prefix from the directory the record names, grants that unit what the withheld root could see, and hands the archived unit over as its baseline. Any other unit claiming the prefix collides as today. This is what makes a question about a `/std` file cost its first answer in the time it takes to restore the archive.

**A broken item is skipped to the next anchor, and named if it can be.** A parse failure the item grammar has committed to no longer ends the module. The item loop records it, scans forward for the next line that begins at column 0 with `pub`, a `-- |` comment, or an item head word followed by whitespace, and resumes there. A candidate is never trusted: the loop parses an item at it, a failure that does not commit means it was not an item and is skipped silently, and one that commits is another broken item and resynchronizes again. The trigger is committed failures only, so an uncommitted failure still ends the loop and hands the text to an entrypoint's tail, and the item-or-tail ambiguity gains no heuristic. A broken item stays in the list as `TopItem::Broken` with its span, its error and the name its head declared where the head parsed that far, which is what lets lowering poison references to it instead of reporting them unbound. Inline module bodies get no anchor of their own: a committed failure inside one is the `mod` item's, and the loop resumes at column 0 after it. `Module::parse` succeeds with broken items and every one is reported; `run`, `compile` and the archive build refuse a module holding any, listing all; the formatter refuses one, which its reparse check already implies. Column 0 is the formatter's convention rather than a rule of the grammar, and the cost of code that ignores it is only that recovery finds no anchor and the parse ends where it ends today. A wrong anchor cannot produce a wrong tree, and inside a body the only head words that could stand at column 0 are `let`, `test`, `concept` and `satisfy`, which commit only after `pub` or a documentation comment, so a wrong anchor cannot produce a spurious diagnostic either. Once a baseline exists, the previous good parse's item spans bound each hole exactly, and reparsing between them is the second step; the anchor rule is what produces the first good parse and recovers the item being typed at the end of a file, which spans never can.

**Recovery is the prerequisite, and poisoning is its rule.** Elaboration continues past a refused item: universes roll back to the item's mark, parked and deferred work stamped with the item is dropped, a witness the item registered before its body is unregistered, its registry entries are removed before positivity, and every later item whose lowered form mentions a poisoned name is skipped and poisoned in turn. Every refusal is reported; the module never reaches the kernel or erasure with one. The poison set is this design's invalidation closure walked forwards, over the same graph.

**Two whole-unit passes narrow to the closure.** Zonk runs over the new items only, since reused ones are zonked. The type and proof totality obligations run over the closure only, since they are per definition over recorded positions. Totality classification, positivity and the witness-cycle check stay whole; together they cost under a second.

**Parsing is memoized by text.** A module is parsed once per distinct text at the seam every read passes through, keyed by the digest the record already computes, so an unchanged file costs no parse.

## The argument

Every fact an item's elaboration consumes from outside itself arrives through a name it mentions: a definition's type and body, a registry entry, a witness head in the table, or a derivation's vocabulary. The reverse closure over mentions therefore contains every item whose result can differ, provided the graph it is taken over is the one the previous elaboration produced, which is where witness resolution and deferral wrote their edges. Ordering does not enter: the global witness table is a keyed lookup, the local tiers see binders only, and a deferred goal resolves to the same registered head whenever it is retried. What remains unit-global is the universe seed table, which is rebuilt by re-lowering, and reused items carry closed universe contexts.

The claim is held to by evidence, not by this paragraph: a differential test serializes the incremental unit and the whole compile and requires byte equality, in the manner of the archive's own determinism check.

## What constrains any answer

- `curios-pipeline` learns nothing about projects. The baseline crosses the `Cache` seam as a `Unit`, never as a path or a record.
- Nothing supersedes an archived unit except the tree it was built from, by the compiler's own record. The invariant in `CLAUDE.md` narrows by that clause and no further.
- The trusted base is unchanged. A reused item was judged by the walk that filed the baseline, exactly as a whole-unit hit is, and the soundness argument is [Cached verdicts](../soundness/admission-without-judgment/cached-verdicts.md) applied per item under a key that covers the item's lowered form and everything its mentions reach. A closure that is not transitive would be a key that omits an input.
- Resource verdicts are already context dependent: the step budget is restored per declaration but the retention quota is compilation-scoped, and a memo hit is free. A partial walk runs in a different cache state and can move a budget-marginal declaration across the line in either direction. The differential test excludes resource verdicts and says so.
- The question path files nothing. Whether the fold may file an incrementally compiled unit is a decision below, not a default.

## Rejected

- **Suffix re-elaboration from the first change**, Lean's per-command model. The emitted order is a Kahn sort over the whole unit, so an edit to an early lemma re-elaborates most of the library.
- **Deriving the record from the spans already in the image.** The store's own lesson: inputs are recorded at the seam they pass through, never believed from a structure that happens to hold them.
- **A manifest key or a `RootKind` for the standard library.** Forgeable by any package, and it reintroduces the tier the mount design removed.
- **Recognizing the prelude in `curios-wonder` or `Membership`.** `curios-package` cannot reach the archive, and every other product would keep colliding.
- **On-disk per-declaration slots.** Redundant while the unit slot plus the item diff gives the same reuse with no new key.
- **Building this into the server as private state.** It would be the third place a fold is spelled, and `test`, `document` and the executables compiled against a moved library would gain nothing.
- **Resynchronizing a broken item at its terminator.** Consuming to the item's `;` or matching `end` needs a lexer for strings, comments and `end` nesting that the scannerless grammar does not have and that can drift from it, and `;` also ends a local `let` inside a body.
- **Reusing the editor grammar's recovery.** Tree-sitter recovers, but it is a second grammar owned by the editors, and the compiler would parse what the editor guessed.

## What has to be decided

- **Whether the fold files an incremental unit.** The argument says it is identical; the differential gate is what earns the right. Until it has run for a while, `test` and `compile` recompile whole and only questions take the baseline.
- **Where `Record` and the slot framing live.** They are `curios-verdicts`'s, above `curios-package`, which the archive crate may not depend on. The stored-unit format moves down, most plausibly to `curios-unit`, and `SCHEMA` bumps if the framing changes.
- **How the closure treats a removed or renamed declaration.** A name that disappears poisons its dependents like a refusal; whether the diagnostic names the removal is a wording decision.
- **The recovery diagnostic for a poisoned dependent.** Silence, or one line naming the root cause; the kernel's own recovery reports nothing for dependents and is the precedent.
- **Whether the parse memo lives in `RootSource` or in the overlay.** The former covers the CLI too; the latter is smaller.
- **Whether an inline module body ever gets an anchor column of its own.** The standard library holds one such body against 137 file-backed modules, so the first cut resumes after the `mod` item; the column of the body's first item is the candidate if that ever proves too coarse.

## Deliberately not specified

- Incremental resolution and lowering, the next floor at about 2.5 s for `/std`.
- Incremental erasure. The arena appends and tombstones, so re-erasing the closure onto the previous arena is structurally supported, and it is the 3.7 s after that.
- Interface-aware invalidation, where a `Prop` body invalidates nothing but itself. A refinement of the closure, not a change to the mechanism.
- Reparsing between the previous good parse's item spans. The exact form of resynchronization, and the second step once a baseline exists.
- Per-item instrumentation of the kernel. Worth taking before the first cut is sized, not a condition of it.

## How to retake the measurements

`cargo build --release --package curios --timings` after `cargo clean --release -p curios-prelude-archive -p curios-prelude` on an idle machine reports both build scripts' wall clock in `target/cargo-timings/`. The per-declaration split is the `declaration` group in the stream `cargo x profile` files under the archive crate's `.artifacts/`, folded by `curios_profile::fold`; the feature evicts the plain archive, so take it in a checkout kept for it.
