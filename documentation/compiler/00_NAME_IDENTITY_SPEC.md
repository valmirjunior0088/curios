# Name Identity in the Compiler

## Status

Investigated 2026-07-26. Stage 0 is implemented. Stage 1 is specified and deferred until the universe hierarchy lands; part of it (§ *Carry the fact*) is independent of the representation change and can be done at any time. The level-based redesign is analyzed and deliberately not adopted; the analysis is recorded here so the question does not have to be reopened from scratch.

## Summary

A free variable in `curios-core` is identified by a `String` (`curios-core/src/scope.rs:121`). That one string carries four distinct kinds of identity, and downstream code recovers the distinction by parsing the string back apart. Twelve sites do this. One of them, `Term::has_local_free`, was wrong because two independent parts of the compiler chose the same sigil without knowing about each other.

### The governing principle

The compiler must not re-derive from a name's spelling what it already knew when it built the name.

This is not a new rule. It is the stated purpose of an existing type: `Qualifier`'s module documentation (`curios-base/src/qualifier.rs:1`) describes it as

> a canonical, resolved identity … what `curios-core`'s `Structure`/`Context`/`Definition` use to track a binding's declaring/use-site module **without re-deriving structure from a flattened string**.

`Definition.island` (`curios-core/src/module.rs:50`) applies it — "`name`'s qualifier prefix, precomputed once by `into_core` (before `name` was flattened) rather than re-derived from it later" — and `Definition.root` (`module.rs:59`) applies it again, precomputed "so the orphan-rule check never has to re-derive it from `island` itself."

So the principle is not merely correct, it is already the tree's own. It was applied to three consumers and then abandoned at the `Var` boundary, where `Qualifier::join()` flattens it away and twelve sites reconstruct it. Everything below extends the existing rule to the sites that lost it.

The corollary is that the twelve sites are not one problem. Some genuinely ask about name identity and want a typed name; some re-derive a fact elaboration discarded and want that fact carried; some are rendering boundaries where the textual form is the point and nothing is wrong. Treating them uniformly is what makes the fix look larger and later than it is.

Stage 0 removed the sigil collision that made the current parse wrong. Stage 1 replaces the string with a typed `Free` value and carries the discarded facts.

## The defect

`VarType::Free(String)` conflates:

| Kind | Spelling | Minted by |
| --- | --- | --- |
| Global | `/a/b/c` | `Qualifier::join` at `curios-text/src/into_core.rs:1609` |
| Global, disambiguated | `/std/Nat/witness#0` | `curios-text/src/into_core.rs:1160` |
| Elaborator local | `x#7`, `#7` | `Context::fresh` at `curios-core/src/context.rs:363` |
| Authored local | `x`, `xs` | surface source |

`Definition.name` (`curios-core/src/module.rs:47`) is a `String` because it must match a `Var::free` label character-for-character, which `Qualifier::join`'s own documentation states. It is a symptom of `VarType::Free(String)`, not an independent problem.

### The sigil collision

Two places independently claimed `#`, each documenting an assumption that the other violates.

`curios-text/src/into_core.rs:483` names anonymous witnesses:

> Anonymous witnesses get deterministic compiler names — `witness#N` by per-module declaration ordinal, under the module prefix. Determinism matters (the cached-prelude replay compares by name); the `#` sigil is illegal in source identifiers, so no user name can collide.

`curios-core/src/term/tests.rs:146` asserts the invariant `has_local_free` depends on:

> `#` is the elaborator's minting marker (`Context::fresh`) and cannot occur in a written identifier, so a free var carrying it is the mark of a context-dependent local.

Both are correct that no *user* name can collide. Neither accounts for the other. A global anonymous witness carries `#`, so `Subterm::has_local_free` (`curios-core/src/term.rs:3066`) classifies any term mentioning one as containing a context-dependent local.

That bit gates three caches — `curios-core/src/context.rs:486`, `curios-core/src/context.rs:574`, and `curios-core/src/elaborate/apply.rs:524`. The failure direction is conservative: results stay correct, caching is lost. The existing test passes only because it checks `/std/Nat`, a global without a disambiguator.

## Evidence

Two throwaway probes, both reverted. Method and results recorded because the conclusions depend on them.

**Key-kind ledger.** A classifier at all thirteen keyed-table insertion points in `Context`, reporting the first time each table saw each kind, run over a full prelude build (all of `/std` and `/syn`).

| Table | Global | Global+`#` | Minted local | Authored local |
| --- | :-: | :-: | :-: | :-: |
| `assumptions` (`context.rs:216`) | yes | yes | yes | — |
| `assumption_universes` (`context.rs:217`) | yes | yes | yes | — |
| `definitions` (`context.rs:218`) | yes | yes | yes | — |
| `refinements` (`context.rs:222`) | — | — | yes | — |
| `induct_decls` (`context.rs:277`) | yes | — | — | — |
| `struct_decls` (`context.rs:280`) | yes | — | — | — |
| `concepts` (`context.rs:284`) | yes | — | — | — |
| `witness_declarations` (`context.rs:288`) | — | yes | — | — |

Three tables key both globals and locals. This is deliberate: `elaborate/module.rs` assumes globals into the same frames `elaborate/binding.rs` puts local `let` binders into, and the kernel treats anything in scope with a type uniformly.

No authored local name ever appeared. Lowering freshens every source binder before it reaches core. This is prelude-scope evidence, not a proof, and Stage 1 should assert it rather than rely on it.

**Misfire count.** A counter on the `has_local_free` false-positive path (a label both `/`-qualified and `#`-bearing), reporting at each power of two, over the same prelude build: more than 4,096 occurrences, every one a `/std/…/witness#N`.

## Site inventory

Twelve sites recover structure by parsing, in three distinct kinds. `curios/src/tests/runtime.rs:486,507` also match on `#`, but they *assert* that minted names never leak into diagnostics; they remain valid and get stronger under Stage 1. `curios-prelude/build.rs:231` is a build-script filter, not compiler logic.

### Kind A — genuine questions about name identity

The question is well-posed; only the answer is spelled wrong. A typed `Free` turns each into a `matches!`.

| Site | Parse | Question |
| --- | --- | --- |
| `curios-text/src/into_core.rs:1456` | `starts_with('/')` | is this a global? |
| `curios-text/src/into_core.rs:1552` | `strip_prefix('/')` | is this a global? |
| `curios-text/src/into_core.rs:1573` | `starts_with('/')` | is this a global? (gates `PrivateItemInPublicInterface`) |
| `curios-core/src/term.rs:504` | `starts_with('/')` | is this a global? |
| `curios-core/src/term.rs:3066` | `contains('#')` | is this an elaborator local? (was wrong; see Stage 0) |

### Kind B — re-deriving a fact elaboration discarded

The parse is a symptom. Typing the name makes these tidier but leaves the re-derivation standing; the fix is to carry the fact, exactly as `island` and `root` already do. **Each is independently actionable and does not wait on Stage 1.**

| Site | Parse | Fact being re-derived | Where it already exists |
| --- | --- | --- | --- |
| `curios-core/src/error.rs:1362` | `rsplit_once('/')` | declaring module of a duplicate witness | `Definition.island` |
| `curios-core/src/error.rs:1384` | `rsplit_once('/')` | declaring module of an orphan witness | `Definition.island` |
| `curios-core/src/erase_ir/function.rs:229` | `rsplit_once('/')` | this var denotes constructor `tag` of inductive family `F` | elaboration resolved it |
| `curios-text/src/into_core.rs:1548` | `trim_start_matches('/').split('/')` | the `Qualifier` the lowerer just flattened | `FlatLet.name` is a `Qualifier` |
| `curios-text/src/into_core.rs:1555` | `split('/')` | as above, for an alias target | as above |

The two `error.rs` sites re-derive `name`'s qualifier prefix by string surgery — verbatim what the `island` doc comment says not to do. They do it only because the error variants carry bare `String` names rather than the declaring module; `island` is already computed and already archived.

`erase_ir/function.rs:229` is the sharpest of the three. `is_proof_constructor` splits a name into family and tag, looks the family up in `induct_decls`, and scans `constructor_order()` for the tag — reconstructing by name shape plus a registry scan a fact that elaboration established when it resolved the constructor. A typed `Free` reduces the string surgery to `Qualifier` methods and leaves the scan.

Two further carriers hold flattened names where a `Qualifier` is the honest type, and belong to this kind even though neither currently parses:

| Site | Shape | Note |
| --- | --- | --- |
| `curios-core/src/module.rs:276` | `module_symbols() -> Vec<String>` | joined symbol names, re-split by `build_shorten` (`print.rs:322`) |
| `curios-text/src/into_core.rs:1523` | `Vec<(String, Vec<String>)>` | the alias-source map feeding `referent_audience`, which then re-splits both halves |

These are the supply side of the Kind B parses above: `into_core.rs:1548`/`1555` exist precisely because this map stores flattened strings. Fixing the carrier removes the consumer.

A caution on scope. Most `Vec<String>` in the tree is *not* a candidate — `error.rs:153/261/296/649/699` (`available` labels, constructor `order`), `concept.rs:36` (`fields`), `scope.rs:472`, `zonk.rs:46`, `print.rs:361/372` (telescope binder labels), `lowerer.rs`, and `into_core/context.rs:59/212/220` (a module's children) all hold *sets of sibling names*, not paths. A `Qualifier` is one path. Substituting it there would typecheck and mean the wrong thing.

### Kind C — legitimate

Textual form is the point. Nothing to fix; `Qualifier::segments()` supplies the structure without parsing.

| Site | Parse | Why it is fine |
| --- | --- | --- |
| `curios-text/src/names.rs:55` | `trim_start_matches('/')` | rendering a surface-relative spelling |
| `curios-core/src/print.rs:322` | `split('/')` | shortest unambiguous suffix for display |

`Qualifier` (`curios-base/src/qualifier.rs`) already answers Kind A and Kind C structurally — `segments`, `without_last`, `last`, and a segment-wise `is_within` — and is already rkyv-derivable. The structure exists; it is discarded at the boundary into core and reconstructed by parsing afterwards.

## Stage 0 — break the sigil collision (implemented)

Scope: one line plus tests. No representation change. Independently valuable and independently revertable.

Anonymous witnesses are now spelled `witness@N` (`curios-text/src/into_core.rs:1160`), not `witness#N`, so `#` marks exactly the elaborator-minted locals that `Term::has_local_free` means by it.

`@` satisfies the three requirements on the marker:

- Illegal in a source identifier, verified against `parse_identifier_raw` (`curios-text/src/parse.rs:76`), whose character class is `is_alphanumeric()` plus `_`. A bare ordinal (`witness0`) would not do: witness names enter `definitions`, so a user-written `let witness0` in the same module would collide silently.
- Not `/`, which would make the name parse as an extra qualifier segment at `error.rs:1362`, `erase_ir/function.rs:229`, and `print.rs:322`. The label is a single `Qualifier` segment (`into_core.rs:1204`), so `@` never reaches those splits.
- Deterministic by per-module declaration ordinal, unchanged — the cached-prelude replay compares by name.

`@` is also the implicit-plicity mark in surface syntax (`(@A : Type, …)`). That is a grammar token, never a character inside an identifier, so the two uses cannot collide — the overlap is one of reading, not of parsing. `$` was the alternative and carries no surface meaning at all; the choice between them is a diagnostic-legibility judgment, and either satisfies the constraints above.

Both comments now name the other convention, so the collision cannot be reintroduced by someone reading only one of them. The regression test in `curios-core/src/term/tests.rs` asserts that `/std/Nat/witness@0` does not satisfy `has_local_free`; the original test covered `/std/Nat` only — a global with no disambiguator — which is why this went unnoticed.

Two latent printing bugs fell out with it, both of which the alternative fix (teaching `has_local_free` to skip `/`-prefixed labels) would have left standing: `strip_fresh` (`print.rs:91`) was rendering `/std/Nat/witness#0` as `/std/Nat/witness`, and `build_rename` (`print.rs:286`) was treating witnesses as prettifiable locals. That is the argument for fixing a collision at its source rather than at the site that trips over it.

Measured: isolated prelude rebuild 27.34s → 24.61s, consistent with recovering the cache entries the misfire was discarding. The build succeeding is also the confirmation that the archive fingerprint machinery rebuilds cleanly through the name change.

## Stage 1 — stop re-deriving what elaboration knew

This is the fix; Stage 0 is a stopgap. It has two halves, and only the first waits on the universe hierarchy.

### Type the name (Kind A)

Deferred until the universe hierarchy lands.

```rust
enum Free {
    Global { qualifier: Qualifier, disambiguator: Option<u32> },
    Local  { hint: Option<String>, mint: u32 },
}

enum VarType {
    Free(Free),
    Bound(usize),
}
```

`disambiguator` is where the anonymous-witness ordinal lives, which keeps `Qualifier` purely textual segments and makes Stage 0's marker unnecessary. `hint` is for diagnostics only and is never part of identity.

Consequences:

- Every Kind A site becomes a match. `has_local_free` becomes `matches!(free, Free::Local { .. })` — exact, so the misfires vanish by construction and Stage 0's `$` marker becomes redundant rather than load-bearing.
- The two `into_core` Kind B sites disappear outright: the `Qualifier` they rebuild is the one the value already carries.
- `Definition.name` becomes `Qualifier`. `curios-prelude/src/archive.rs:9` bumps `SCHEMA` from 9 to 10.
- Keys: `assumptions`, `assumption_universes`, and `definitions` take `Free` (they genuinely mix, per the ledger); `refinements` takes the local form; the four registries take `Qualifier`.
- `Scope::close`'s label parameter changes type. `capture`, `release`, `shift`, and `reach` pruning are untouched.

Roughly twenty non-test `Var::free` construction sites, each unambiguously one kind: `resolve_name` for globals, and `match_compile`/`lowerer` synthetics plus telescope identity spines (`print.rs:369`, `reduce.rs:449`, `elaborate/binding.rs:98`) for locals. Twenty-six further sites are tests.

Risk is breadth, not subtlety. The compiler catches nearly all of it. The one thing to watch is a table that turns out to key both kinds in a way `Free` cannot express; the ledger found none, but it only covered the prelude.

One thing to watch — not a known cost. `Free::Global` holds a `Qualifier`, which is a two-or-three-element `Vec<String>`, where a label today is one `String`; `Free::Local` moves the other way, comparing a `u32` instead of a string. What makes this worth a look is not `Qualifier` itself but the call site: `island` and `root` are stamped once per item, whereas `Scope::close`/`capture` compare labels while walking every node of every term. Moving globals into that loop is a change of kind, not of degree.

Nothing here is measured, and `Qualifier`'s existing traffic is not in question — at its current call sites it is cheap. Profile the close/capture path with the built-in `tracing` mechanism once Stage 1 exists; there is nothing to measure before then. If it does show up, fix it inside `Qualifier` — interned segments, or an `Rc<[…]>` backing that makes `clone` a refcount bump and equality pointer-first — so every consumer benefits, rather than routing around it in `Free`.

### Carry the fact (Kind B)

**Not deferred.** These do not touch `VarType` and can land independently, in any order, before or after the hierarchy.

- `error.rs:1362`, `error.rs:1384` — give `DuplicateWitness` and `OrphanWitness` the declaring module instead of a bare name. `Definition.island` already holds it, is already precomputed by `into_core`, and is already archived. This is applying the `island` doc comment to two more consumers.
- `erase_ir/function.rs:229` — `is_proof_constructor` should be told, not asked. Elaboration resolved the constructor and knows its family; the erased IR should carry that rather than have erasure recover it from name shape and then scan `constructor_order()`. This one needs a design decision about where the fact rides, so it is the largest of the three and the only one that is not mechanical.

Doing these first also shrinks Stage 1 proper: it removes the two consumers that would otherwise need `Qualifier`-shaped rewrites during the representation change.

## Considered and deferred — de Bruijn levels for free variables

Bound variables are already de Bruijn indices and would stay so. The proposal was to identify *free* variables — opened binders — by de Bruijn level instead of by minted name, making context-dependence structural rather than a property of spelling.

Motivating observation: four separate structures already store binders in order and then key them by string.

| Structure | Shape |
| --- | --- |
| `Context::local` (`context.rs:115`) | `Vec<(String, Term)>` |
| `Opened` (`convert.rs:88`) | `Vec<(String, Term)>`, innermost last |
| `FrozenFrame::assumptions` (`context.rs:114`) | `Vec<(String, Term)>`, binding order |
| `FrozenFrame::witness_binders` | `Vec<(String, Term)>`, same order |

Levels are the representation these approximate.

### Why it was not adopted

Every correctness benefit is already delivered by Stage 1. Levels add elegance — deleting `Context::fresh`, dissolving the three-table mixing, turning `history_key` into arithmetic — but no correctness that a typed name does not give, while concentrating risk in the two least forgiving parts of the codebase.

**Conversion recurrence.** `Convert::history_key` (`convert.rs:519`) renames minted opening labels to placeholders in mint order, collapsing successive rounds of an unfolding cycle onto one history entry so the recurrence rule fires — this is what makes a cycle with no finite disagreement definitional equality. Under levels this plausibly becomes min-level normalization, which would be simpler; levels are canonically ordered by construction, which is what `Convert::minted` reconstructs by hand. But mint order is not depth order, and sibling branches at equal depth share a level. The current scheme appears to collide the same way, so the schemes may be equivalent — this could not be settled by reading. A false history hit is unsoundness in the conversion checker.

**Parked work.** `FrozenFrame` (`context.rs:114`) freezes a local frame and reapplies it on retry, potentially at a different depth. Names make this free; levels require rebasing every parked term, in both `FrozenFrame` and `ParkedWork`.

**Lowering.** Thirty-six of forty-six `Var::free` sites are in `curios-text`, which mints staging names (`MatchCompiler::fresh_binder`) to build a body and immediately `Scope::close` it. Under levels the lowerer must track depth through decision-tree compilation — columns, sub-columns, synthetic binders. This is where the work actually concentrates, and it is the most intricate lowering code in the tree.

Two earlier objections did not survive scrutiny and are recorded so they are not raised again. `Sort::of`'s `Opened` is kept out of the `Context` to avoid bumping `mutation_stamp` and starving the conversion deadline; a level is `context.depth() + position`, and depth is readable without mutation, so this is unaffected. The lookup at `convert.rs:107` is a reverse linear scan with string comparison and would become indexing, but `opened` holds one entry per telescope binder, so depths are small and the win was never measured — it is not evidence.

### Revisiting

Reopen only on a concrete problem, not on aesthetics. If the kernel is reworked for another reason, this is the design to adopt, and the first thing to settle is the `history_key` question above.

## Open questions

- Where the constructor fact should ride for `erase_ir/function.rs:229` — on the erased IR node, or as a resolved reference the erasure already holds. This is the only Kind B item that is not mechanical.
- Whether authored local names ever reach core as free variables outside the prelude. Stage 1 should assert this rather than assume it.
- Whether any table keys both kinds in a way `Free` cannot express. None found; coverage was prelude-only.
- Whether putting a `Qualifier` in the `Scope::close`/`capture` comparison loop costs more than `Free::Local`'s `u32` saves. Not measurable until Stage 1 exists; profile then, and fix it in `Qualifier` if it appears.
