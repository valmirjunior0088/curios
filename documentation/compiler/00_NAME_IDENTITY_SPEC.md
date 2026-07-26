# Names as identity only

Working implementation specification for removing every place the compiler reads a name's *spelling* to decide behavior, leaving a name with exactly two capabilities: it distinguishes one binding from another, and it can be rendered for a human.

This effort is not a cleanup of string handling. A name's spelling has become an undocumented wire format between compiler stages: five distinct structured facts are serialized into one `String`, and every consumer that needs one back deserializes it with a hand-rolled parser that no other site shares. Three of those parsers make semantic or cache decisions, and each is correct only under an invariant enforced in a different crate and stated nowhere near the parse. The work is to delete the format, not to tidy its readers.

When this work lands, fold the permanent rule and the resulting type contracts into the owning `curios-base`, `curios-core`, and `curios-text` module documentation, record the outcome in `ROADMAP.md`, move the rejected de Bruijn level analysis to `DESIGN.md`, and delete this working specification after no remaining document refers to it.

## Status

Investigated 2026-07-26. Phases 0, A, and B are implemented; `SCHEMA` is at 11. Phase C is specified and now unblocked — the universe hierarchy has landed and its specification is retired. Phase D is specified but follows C: its first two bullets need `Free`, while its two audits can run at any time (the `name!` half of the `Ord` audit landed with Phase B).

Phase C was never the only phase that changes the archive schema, which is what the sequencing was originally argued from — Phase A bumped it and Phase B bumped it again. The bump is a one-line constant and was never the real constraint; the reason to sequence after the universe work was that it was still churning the same Core types, `InductDecl`/`Definition`/`Module`. It no longer is.

Two things must be settled before Phase C is written, both because they fix the archive's shape: the staged migration (below), and the mint floors (under Phase C's consequences).

## Objective

A name value supports equality, hashing, and rendering. It supports nothing else — not ordering, not substructure extraction.

The done-condition is falsifiable, and that is the point of stating it this way:

> Reintroducing behavior-from-spelling requires adding a method to a name type. It cannot happen by accident, and it appears in review as what it is.

Every violation catalogued below exists because a name is a `String`, and `String` has `starts_with`, `rsplit_once`, and `split_once`. `Var::as_free() -> Option<&String>` (`curios-core/src/scope.rs`) is the hole the entire defect flows through. Closing it removes the class, not merely its present instances — which is why a phase that only retypes the sites is insufficient, and why Phase D exists.

## Permanent design decisions

**One fact, one field.** No field carries two meanings. Every violation in this specification is an instance of a single merge: a binder's label is simultaneously its display hint and its variable identity; a constructor's name is simultaneously its identity and its runtime tag ordering. Unmerging is the fix; removing the capability that permitted the merge is what makes it stay fixed.

**Identity is opaque; spelling is for humans.** A name distinguishes bindings and renders. Behavior never branches on its characters, its prefix, its collation order, or its relationship to another name's spelling.

**Structure is carried, never recovered.** A fact known at the site that built a name is stored as a value. This rule is not new: it is `Qualifier`'s stated purpose (`curios-base/src/qualifier.rs:1`), it is why `Definition.island` and `Definition.root` exist (`curios-core/src/module.rs:50,59`), and it is why `FlatItem::in_prelude` checks "the *structured* qualifier's root segment, before names are flattened to strings" (`curios-text/src/into_core/context.rs:47`). The rule was applied to three consumers and abandoned at the `Var` boundary.

**Names from different namespaces are different types.** Already the tree's rule, in `curios-base/src/macros.rs:5`: a `name!` type exists "so names from different namespaces cannot be confused however identical their text." Phase D extends it by removing the derives that let a name mean more than identity.

**No sigil convention is load-bearing.** A marker character inside a name is the wire format, not a fix for it. Phase 0 introduced one deliberately and Phase C retires it.

## The five facts encoded in a name

| # | Fact | Structural home | Present state |
| --- | --- | --- | --- |
| 1 | namespace path | `Qualifier` — exists, correct | flattened at `into_core/context.rs:30`; 5 decoders |
| 2 | global vs. local | a sum-type discriminant | does not exist; 4 decoders |
| 3 | minted vs. authored | the same discriminant | does not exist; 3 decoders |
| 4 | a minted local's authored hint | the binder | does not exist; 2 decoders |
| 5 | owner/family relation | `DefinitionKind.owner` — **exists, correct** | 3 encoders, 1 decoder |
| 6 | a constructor's runtime tag | an explicit index | *is* `Atom`'s lexicographic `Ord` |

Facts 1 and 5 already have correct homes and are re-derived anyway. Facts 2 and 3 are encoded by punctuation. Fact 4 exists only because facts 3 and 4 share storage. Fact 6 is not encoded in the characters at all — it is encoded in the collation order over them, which is why a spelling-only audit misses it.

### Why this is more than untidiness

Each parse is sound only under an invariant enforced elsewhere and never stated at the parse site. This is a failure mode, not a hypothetical: it has already produced one bug and the tree contradicts itself about a second.

**Already broken.** `Term::has_local_free`'s `#` test was safe only if no *global* carried `#`. `into_core` minted `witness#N` without knowing that. Both sites documented their assumption; neither knew the other existed. Phase 0 fixed it.

**Latent, and the tree disagrees with itself.** `is_proof_constructor` (`curios-core/src/erase_ir/function.rs:229`) splits a name into family and tag and looks the family up in `induct_decls`, deciding whether to **drop a call entirely** (`function.rs:164`). Its own doc comment justifies the parse: the family "is the name's qualifier prefix and the case is its last segment." `elaborate/module.rs:397` reaches the opposite conclusion about the identical fact — "re-deriving it by splitting `name` would misread an ordinary definition that merely happens to sit under a concept's namespace" — and again at `module.rs:441`, "not something recovered from `name`." Those two sites read `DefinitionKind::{ConceptMethod, InductiveConstructor}.owner` (`module.rs:404,442`) instead.

The misread was unreachable: `ModuleInfo::insert_child`/`insert_induct_child` (`into_core/context.rs:148,157`) keep namespace labels unique, and `induct`/`concept` occupy that namespace (`into_core.rs:335,346`), so no ordinary definition could sit under an inductive's qualifier — with one exception the argument never named, since `scan_module_info` gives a witness no namespace slot at all (`into_core.rs:349`) and its `witness@N` name is therefore unconstrained by those two guards. The parse was correct for a reason stated in a different crate, which the parallel code in its own crate documented as false. That is Phase 0's failure shape, one fact over, with a silently dropped call as the consequence instead of a lost cache. Phase A retired it; the shape is what remains instructive.

**A third instance of the same shape.** `Convert::imitate_flex_apply` reuses telescope binder labels as free-variable identities and records its dependence in a comment (`convert.rs:1789`): "elaborated labels are entropy-fresh." When there is no label it substitutes `"_"` (`convert.rs:1812`), so two unlabelled binders receive the same identity.

### What is already right

The majority of the pipeline honors the rule, and the fix is to generalize what is already here rather than invent a mechanism.

- `curios-ersd`, `curios-cont`, `curios-wasm`, `curios-runtime`, `curios-pipeline`, `curios`, and `curios-web` parse names nowhere. The naming-scheme conventions hold downstream because names there are opaque.
- `resolve.rs` and `HeadKey::Nominal` (`curios-core/src/concept.rs:103`) use a name as an opaque unique key and never inspect it.
- `elaborate_infix` (`curios-core/src/elaborate/binding.rs:405-425`) is the model: look the concept up by name as an opaque key, then resolve the method **positionally** by index into `concept.fields`. It never joins a concept name and a field name into a third name.

## Inventory

Decoders — behavior branches on spelling.

| Site | Reads | Fact | Retired by |
| --- | --- | --- | --- |
| `curios-core/src/term.rs:3072` | `contains('#')` | 3 | Phase C |
| ~~`curios-core/src/erase_ir/function.rs:229,237`~~ | `rsplit_once('/')` + registry scan | 5 | Phase A — done |
| `curios-core/src/elaborate/apply.rs:92` | `split_once('#')` | 4 | Phase C |
| `curios-core/src/print.rs:92` | `split_once('#')` | 4 | Phase C (deleted) |
| `curios-core/src/print.rs:286` | `split_once('#')` | 3 | Phase C (deleted) |
| ~~`curios-core/src/error.rs:1362`~~ | `rsplit_once('/')` | 1 | Phase A — done |
| ~~`curios-core/src/error.rs:1384`~~ | `rsplit_once('/')` | 1 | Phase A — done |
| `curios-core/src/term.rs:504` | `starts_with('/')` | 2 | Phase C |
| `curios-text/src/into_core.rs:1464` | `starts_with('/')` | 2 | Phase C |
| `curios-text/src/into_core.rs:1581` | `starts_with('/')` | 2 | Phase C |
| `curios-text/src/into_core.rs:1556,1560,1563` | `trim_start_matches`/`split`/`strip_prefix` | 1 | Phase C |

Encoders — the same convention written by hand, with no shared definition. A decoder is half a format; the current inventory of this defect has historically listed only the readers.

| Site | Writes | Fact | Retired by |
| --- | --- | --- | --- |
| `curios-text/src/into_core.rs:891` | `Qualifier::with(tag)` | 5 | remains (this is the mint site) |
| ~~`curios-core/src/zonk.rs:472`~~ | `format!("{name}/{tag}")` | 5 | Phase A — done |
| `curios-core/src/print.rs:974,977` | `format!("{name}/{tag}")` | 5 | remains (rendering) |
| `curios-core/src/context.rs:363` | `format!("{h}#{counter}")` | 3, 4 | Phase C |
| `curios-text/src/into_core.rs:1168` | `format!("witness@{ordinal}")` | — | Phase C |

Carriers holding a flattened name where a structured value is the honest type. These are the supply side of the decoders above: fixing the carrier deletes the consumer.

| Site | Shape | Becomes |
| --- | --- | --- |
| `curios-core/src/module.rs:47` | `Definition.name: String` | `Global` (Phase C) |
| ~~`curios-core/src/module.rs:28,31`~~ | `DefinitionKind` `owner: String` | `Qualifier` + `tag: Atom` (Phase A — done) |
| `curios-core/src/module.rs:276` | `module_symbols() -> Vec<String>` | `Vec<Global>` (Phase C — needs `Definition.name`) |
| `curios-text/src/into_core.rs:1523` | `Vec<(String, Vec<String>)>` alias map | `Global`-keyed (Phase C — needs Core `free_vars`) |
| `curios-core/src/concept.rs:103` | `HeadKey::Nominal(String)` | `Qualifier` (Phase C) |
| `curios-core/src/term.rs:2036,2056,2073` | `InductType`/`Variant`/`StructType` `name: String` | `Qualifier` (Phase C) |

Legitimate — the textual form is the deliverable. `Qualifier::segments()` supplies the structure without parsing, so these stop being parses without changing what they render.

| Site | Reads | Why it is fine |
| --- | --- | --- |
| `curios-core/src/print.rs:322` | `split('/')` | shortest unambiguous suffix for display |
| `curios-text/src/names.rs:55` | `trim_start_matches('/')` | rendering a surface-relative spelling |
| `curios-prelude/build.rs:231` | `rsplit('/')` | build-script diagnostic, not compiler logic |

`curios/src/tests/runtime.rs:486,507` also match on `#`, but they *assert* that minted names never reach diagnostics. They stay valid and become structurally impossible to fail under Phase C.

A caution on scope. Most `Vec<String>` in the tree is not a candidate. `error.rs:153,261,296,649,699` (`available` labels, constructor `order`), `concept.rs:36` (`fields`), `scope.rs:472`, `zonk.rs:46`, `print.rs:361,372` (telescope binder labels), `lowerer.rs`, and `into_core/context.rs:59,212,220` all hold *sets of sibling names*, not paths. A `Qualifier` is one path; substituting it there would typecheck and mean the wrong thing.

## Phase 0 — break the sigil collision (implemented)

Anonymous witnesses are spelled `witness@N` (`curios-text/src/into_core.rs:1168`), not `witness#N`, so `#` marks exactly the elaborator-minted locals that `Term::has_local_free` means by it.

`@` satisfies the three requirements on the marker: it is illegal in a source identifier, verified against `parse_identifier_raw` (`curios-text/src/parse.rs:76`), whose class is `is_alphanumeric()` plus `_` — a bare ordinal would not do, since witness names enter `definitions` where a user-written `let witness0` would collide silently; it is not `/`, so the label stays one `Qualifier` segment and never reaches the splits at `error.rs:1362`, `erase_ir/function.rs:229`, or `print.rs:322`; and it remains deterministic by per-module declaration ordinal, which the cached-prelude replay compares by name. Both comments now name the other convention, so the collision cannot be reintroduced by someone reading only one of them.

Two latent printing bugs fell out with it, both of which the alternative fix — teaching `has_local_free` to skip `/`-prefixed labels — would have left standing: `strip_fresh` (`print.rs:92`) rendered `/std/Nat/witness#0` as `/std/Nat/witness`, and `build_rename` (`print.rs:286`) treated witnesses as prettifiable locals. That is the argument for fixing a collision at its source rather than at the site that trips over it.

Measured at the time: isolated prelude rebuild 27.34s → 24.61s, consistent with recovering the cache entries the misfire was discarding.

**Phase 0 moves away from the objective and is kept anyway.** It makes a second field of the wire format — the sigil namespace — load-bearing, and documents it as a rule to maintain. It fixed a live bug and bought measured time, so keeping it is right, but it is debt paid down by taking on debt, not the first step of this work. Phase C retires it.

## Phase A — carry what is already computed (implemented)

Landed as `Carry definition origins instead of recovering them from names`. It retires fact 5 entirely — encoders included — and deletes three of the eleven decoder rows. Three claims this phase was specified under turned out to be false, and they are recorded here rather than corrected away, because each is the same mistake and the remaining phases can still make it.

**It was not archive-neutral.** `DefinitionKind` is archived (`module.rs:21-24`) and rides in `Definition` → `Item` → `Module` → `PreparedPrelude.core` (`curios-prelude/src/archive.rs:30`), so retyping `owner` alone forced `SCHEMA` 9 → 10. No phase that retypes a carrier is neutral; only the ones that add a *runtime* field, like `DefEntry`, are.

**Fact 5 is a pair, and only half of it had a home.** `InductiveConstructor { owner }` says which inductive a constructor belongs to but not which constructor it is, so `zonk.rs` could not stop synthesizing `format!("{name}/{tag}")` by reading the kind — the tag was not there to read. The variant carries `tag: Atom` as well, which deletes the synthesis and lets the two half-checks over the registry correspondence merge into one.

**An item that needs a structured value cannot be done before the value exists.** `module_symbols()` collects `Definition.name` and the registry keys, all `String`; returning `Vec<Qualifier>` would mean splitting them back into segments. The same is true of the alias-source rekeying, whose `referent` originates in Core `free_vars()`. Both moved to Phase C. The general rule: an item belongs in the phase that *creates* the carrier, not the phase that wants to read it.

- **`DefEntry` carries `DefinitionKind`.** `define_entry` dropped the kind on insert, which was the only reason erasure could not ask. `is_proof_constructor` now reads `InductiveConstructor { owner }`, and both the `rsplit_once` and the `constructor_order()` scan are gone. The field is `Option<DefinitionKind>` — `None` for a genuine local binding, which no module item declared — and every `define` call site passes it explicitly, so a site that has a kind cannot silently omit it.
- **`DefinitionKind` carries `owner: Qualifier` and `tag: Atom`.** The values `into_core` held before flattening (`into_core.rs:888,1153`).
- **`zonk.rs` stops synthesizing keys.** Both directions of the generated-member correspondence are read off the definition's own recorded origin, replacing the synthesized lookup and the separate owner-exists loop.
- **`DuplicateWitness` and `OrphanWitness` carry the declaring module.** `Definition.island` already held it, precomputed by `into_core` and already archived; `Witness` carries it too, so a collision reports both modules. Neither message is pinned by a test — the rewrite was verified by hand against the previous `rsplit_once` behavior for both the root-qualifier and nested-module cases, and a regression test for them is still owed.

## Phase B — make the constructor tag explicit

Independent of every other phase.

`Atom` is `name!(Atom; archive)` (`curios-core/src/names.rs:3`), and `name!` derives `Ord` lexicographically on the string (`curios-base/src/macros.rs:6`). `InductDecl::constructors` is a `BTreeMap<Atom, _>` (`curios-core/src/inductive.rs:61`), `constructor_order()` is its `keys()` (`inductive.rs:109`), and `constructor_index()` is a position in that order — documented as "the runtime tag `erase_variant` gives a value constructed with it" (`inductive.rs:113`). It is consumed at `erase_ir/aggregate.rs:125,296` and `erase_ir/eliminate.rs:601`.

All three consumers read the same ordering, so this is self-consistent and not a miscompile. It is, however, the only place a name's spelling reaches the emitted artifact.

**The tag becomes declaration order.** Predictable from the source, stable under the edit that is meant to be neutral (rename), and changing only under an edit that is visibly intentional (reordering a declaration). It matches this repository's written-order rule. Nothing depends on tag stability: the prelude archive is explicitly not an interchange format and is scoped to one compiler build, and `.cwasm` is version-pinned to its runtime.

The alternative — an explicit index preserving today's numbering — was rejected. It does not even buy rename-stability, since an index computed from collation order still renumbers on rename; it buys only the absence of test churn today, and pays for it by enshrining "tags are alphabetical" as a rule someone must know forever.

The mechanism matters more than the numbering:

- `InductDecl::constructors` stops being a `BTreeMap<Atom, _>`, because **the `BTreeMap` is the collation dependence.** Declaration order becomes the stored authority and the index is the position in it. `InductElim::cases` (`curios-core/src/term.rs:2216`) is the same shape and changes with it, keeping lookup-by-tag.
- `Atom` loses `PartialOrd`/`Ord`. Deriving `Ord` on a name is the statement that its spelling means something past uniqueness; once the derive is gone, collation-dependence is a compile error rather than a thing to remember.

Codegen expectations that assert tag values update with the renumbering. That is the intended cost, taken once.

## Phase C — type the name

The phase that waits on the universe hierarchy, for the reason given under Status — not because it owns the archive schema, which Phase A has already bumped once. Smaller than it would have been before Phase A, which removed its hardest consumers, and larger by the two carriers Phase A could not reach: `module_symbols()` and the alias-source map both need `Definition.name` to be a `Global` first.

```rust
/// A top-level definition's identity: either an authored path, or a
/// compiler-generated definition that has no source name at all.
enum Global {
    Authored(Qualifier),
    Witness(WitnessId),
}

/// An anonymous witness's identity. A `satisfy` declaration is nameless by
/// design, so it gets an identity rather than a manufactured name.
struct WitnessId(u32);

/// An opened binder's identity. Carries no hint: a hint belongs to the
/// binder, not to an occurrence of it.
struct Mint(u32);

enum Free {
    Global(Global),
    Local(Mint),
}

enum VarType {
    Free(Free),
    Bound(usize),
}
```

**`Qualifier` means one thing everywhere, with no exception.** The rejected alternative was a flat `Global { qualifier, disambiguator: Option<u32> }`, whose `qualifier` would have meant *module plus the item's own name* for an authored global but *the declaring module alone* for a witness — one field with two readings, selected by whether another field is populated. That is the merge this specification exists to remove, reintroduced by the fix for it. The sum states the two cases, and `Qualifier` stays exactly what `curios-base` says it is.

It also retires Phase 0's marker completely rather than relocating it. There is no `witness@N` text left to render, so nothing downstream can key on the sigil — which forces a live defect to be fixed rather than carried: the `while elaborating /witness@1:` prefix on a witness diagnostic is today the one place a compiler-minted name reaches a user, and under `WitnessId` it must become the module coordinate `Definition.island` already holds.

`WitnessId` needs the same archive discipline as `Mint`: the prelude's ids are fixed when it is archived, so entry-module lowering mints strictly above the restored floor. See the invariant below.

### Hint and identity are separated, not relocated

A binder's label today serves as both its display hint and its variable identity. `Context::fresh` uniquifies it to `x#7`, which *destroys the hint*, so the hint is recovered by parsing the marker back off. Fact 4 exists only because facts 3 and 4 share one field.

Putting a `hint` field inside the name is therefore rejected. It preserves the merge and forces a hand-written `Eq`/`Hash` that deliberately disagrees with the derive — which in this tree is a live hazard rather than a style point, because archived names derive their own comparisons (`curios-base/src/qualifier.rs:19`), so the live and archived orderings would disagree in a way nothing would surface.

Instead:

- `Context::fresh(hint) -> String` becomes `Context::mint() -> Mint`. A caller that passed a hint for display passes it to the binder it is building.
- `Scope::close` abstracts by identity and records the binder's hint separately; telescopes already carry per-binder labels (`Telescope::first_label`). `capture` (`curios-core/src/scope.rs:206`) compares identities.

This deletes sites rather than rewriting them. Every reader of fact 4 is already walking binder structure when it asks: `elaborate/apply.rs:92` receives the telescope's own label and becomes `label`; `strip_fresh` (`print.rs:92`) and `build_rename` (`print.rs:286`) **disappear**, since `build_rename` exists only to decide which names are prettifiable by testing for a non-empty hint before `#`.

The sites that build a free variable from an unminted label mint instead: `imitate_flex_apply` (`convert.rs:1812,1823`) mints from the `context` it already holds and keeps `first_label()` as the rebuilt binder's hint, which also removes the `"_"` collision above; `print.rs:368` needs no identity for display and keys off the `depth` it already tracks. Admitting an unminted case was rejected — it would put spelling back into identity, which is the property being removed.

### Consequences

- Every decoder of facts 2 and 3 becomes a `matches!`. `has_local_free` is `matches!(free, Free::Local(_))` — exact, so the misfires vanish by construction.
- `Definition.name` becomes `Global`: a bare `Qualifier` cannot name a witness, which has no authored path.
- Keys: `assumptions`, `assumption_universes`, and `definitions` take `Free` — they genuinely mix, per the ledger below; `refinements` takes `Mint`; `induct_decls`, `struct_decls`, and `concepts` take `Qualifier`; `witness_declarations` and `Module::witnesses` take `WitnessId`, and `Witness::name` becomes one, which also turns `update_witness_scheme`'s scan over the table into a lookup.
- `module_symbols()` and the alias-source map, orphaned by Phase A, land here — both were waiting for `Definition.name` and Core `free_vars()` to carry structure.
- `SCHEMA` bumps again (Phase A took it to 10, Phase B to 11).

**Mint floors are an archive invariant, not an implementation detail.** `Mint` and `WitnessId` counters are compilation-scoped, and the ledger shows minted locals reaching `assumptions`/`definitions` — so they are *in* the archived Core. Replay must resume both counters strictly above the restored floors, exactly as `next_metavar` is seeded by `metavar_floor` (`context.rs:270-273`) and as the universe hierarchy seeds its own floors. Without it a freshly minted `Mint(7)` silently aliases an archived one: no error, no diagnostic, a wrong binder. This is the single worst failure mode in the effort, it has two precedents to copy, and it must be asserted rather than assumed.

Risk is breadth, and the breadth is larger than a search for `Var::free` suggests: `Term::free_var` (`curios-core/src/term.rs:549`) is a thin wrapper over it and is the dominant spelling. Measured census below. The compiler catches nearly all of it; the one thing to watch is a table that keys both kinds in a way `Free` cannot express, which the ledger did not find but only covered the prelude.

`Scope::close`/`capture` moving from string comparison to identity comparison is the riskiest edit in this specification — it is the kernel's hot path — and is also the one place the change is expected to be *cheaper*, since a `Mint` comparison replaces a `String` comparison on every node of every term. Profile it with the built-in `tracing` mechanism after the phase exists; there is nothing to measure before then.

## Phase D — close the door

Phase C retypes the sites. Phase D is what makes the property hold for the next contributor, and its absence is why the defect could otherwise recur.

- `Var::as_free()` returns `Option<&Free>`, and **`Free` exposes no spelling accessor at all.** Rendering goes through the printer, which wants structure anyway: `build_shorten` (`print.rs:322`) takes `Qualifier::segments()` and stops splitting.
- **`Qualifier`'s textual accessors move behind the rendering boundary.** The bullet above closes `Var`; without this one the level beneath it stays open, and the objective is met only in letter. `Global::Authored` has to expose its `Qualifier` — the diagnostics and the visibility checks both need it — and `Qualifier` hands out `last`, `head`, `segments`, and `iter`, each yielding `&str`. So a site that matches out the qualifier and then prefix-tests `q.last()` compiles after Phase D as currently written, adds no method to any name type, and would pass review as ordinary code. Keep the *structural* half public — `with`, `without_last`, `without_first`, and `is_within`, which is the module system's visibility primitive — and make the *textual* half reachable only where text is the deliverable. `build_shorten` is exactly that boundary, which is why the bullet above routes through it.
- Two things are deliberately out of this step's scope, and saying so keeps it from being over-applied. `curios-text`'s surface `Name` holds a `Qualifier` and walks its segments to resolve a written `A/B/c` (`into_core/context.rs:474-499`, `interface.rs:776`); that is the front end parsing user input, not the compiler re-deriving a fact it discarded. And tuple-type field labels stay `String`, because they are authored data that `.label` resolution projects by — `TupleType` reasserts them in its own node identity (`scope.rs:465`). A survey of the remaining Core-side text callers found only the witness diagnostics' module renderer and one `segments().is_empty()` in `into_core/interface.rs:157` that wants to be a `Qualifier::is_root()` predicate.
- Audit `Ord` on the remaining name types. `Atom` is handled in Phase B. The name-keyed `BTreeMap`s in `Context` — `induct_decls`, `struct_decls`, `concepts`, `witness_declarations` — are queried by key rather than iterated for output, with one exception: `zonk`'s validation loops (`zonk.rs:450,485,571,601`) iterate them, so alphabetical order decides which error is reported first. Decide whether that becomes declaration order or stays arbitrary-but-deterministic.
- Audit `as_str` callers on `name!` types so that none performs substructure work. Equality against a spelling is uniqueness and is fine; splitting, prefix-testing, and ordering are not.

Verify by deletion: remove the accessors, then confirm nothing needed them. A site that resists is a fact that still has no home, and it belongs in the table above.

The phase is done when the objective's own test passes against the *reachable* API rather than against `Var` alone: starting from a `Free`, no path to a `&str` exists except through the printer. Until the second bullet lands, that path is two ordinary method calls long, which is why retyping the sites is necessary and not sufficient.

## Evidence

**Key-kind ledger.** A throwaway classifier at all thirteen keyed-table insertion points in `Context`, reporting the first time each table saw each kind, over a full prelude build (all of `/std` and `/syn`). Reverted; recorded because the Phase C key assignments depend on it.

The "disambiguated" column counts what were then `witness@N` names — the kind `Global::Witness(WitnessId)` now carries.

| Table | Global | Global, disambiguated | Minted local | Authored local |
| --- | :-: | :-: | :-: | :-: |
| `assumptions` (`context.rs:216`) | yes | yes | yes | — |
| `assumption_universes` (`context.rs:217`) | yes | yes | yes | — |
| `definitions` (`context.rs:218`) | yes | yes | yes | — |
| `refinements` (`context.rs:222`) | — | — | yes | — |
| `induct_decls` (`context.rs:277`) | yes | — | — | — |
| `struct_decls` (`context.rs:280`) | yes | — | — | — |
| `concepts` (`context.rs:284`) | yes | — | — | — |
| `witness_declarations` (`context.rs:288`) | — | yes | — | — |

Three tables key both globals and locals. This is deliberate: `elaborate/module.rs` assumes globals into the same frames `elaborate/binding.rs` puts local `let` binders into, and the kernel treats anything in scope with a type uniformly. No authored local name appeared, because lowering freshens every source binder before it reaches core; this is prelude-scope evidence, not a proof, and Phase C should assert it rather than rely on it.

**Misfire count.** A counter on the pre-Phase-0 `has_local_free` false-positive path — a label both `/`-qualified and `#`-bearing — over the same build: more than 4,096 occurrences, every one a `/std/…/witness#N`.

**Construction-site census.** Counting both `Var::free(` and its `Term::free_var(` wrapper, excluding comment lines: **113 non-test sites and 276 test sites.** Counting only the literal spelling `Var::free(` gives 46, of which 36 are in `curios-text` — which is why that narrower count understates the work and mislocates it. The real concentration is `curios-core` (91 of 113 non-test), led by `convert.rs` (21) and `elaborate/match_.rs` (19), then `into_core/match_compile.rs` (11), `print.rs` (7), `into_core.rs` (6), `resolve.rs` (6), and `erase_ir/eliminate.rs` (6).

**Tag ordering.** `/std/Result` declares `success` then `failure`; the emitted tags are `failure = 0`, `success = 1`. `/std/Option` declares `some` then `none`; the tags are `none = 0`, `some = 1`. Both are reversed from source, alphabetically ordered, and self-consistent across construction and elimination.

## Rejected alternative — de Bruijn levels for free variables

Bound variables are already de Bruijn indices and would stay so. The proposal was to identify *free* variables — opened binders — by de Bruijn level instead of by minted identity, making context-dependence structural. Four structures already store binders in order and then key them by string: `Context::local` (`context.rs:236`), `Opened` (`convert.rs:91`), and `FrozenFrame`'s `assumptions` and `witness_binders` (`context.rs:114`). Levels are the representation these approximate.

Not adopted. Every correctness benefit is delivered by Phase C. Levels add elegance — deleting the mint counter, dissolving the three-table mixing, turning `history_key` into arithmetic — but no correctness a typed name does not give, while concentrating risk in the two least forgiving parts of the tree.

- **Conversion recurrence.** `Convert::history_key` (`convert.rs:519`) renames minted opening labels to placeholders in mint order, collapsing successive rounds of an unfolding cycle onto one history entry so the recurrence rule fires. Under levels this plausibly becomes min-level normalization. But mint order is not depth order, and sibling branches at equal depth share a level; the current scheme appears to collide the same way, so the two may be equivalent. This could not be settled by reading, and a false history hit is unsoundness in the conversion checker.
- **Parked work.** `FrozenFrame` (`context.rs:114`) freezes a local frame and reapplies it on retry, potentially at a different depth. Identities make this free; levels require rebasing every parked term, in both `FrozenFrame` and `ParkedWork` (`context.rs:128`).
- **Lowering.** Under levels the lowerer must track depth through decision-tree compilation — columns, sub-columns, synthetic binders — which is the most intricate lowering code in the tree.

Two earlier objections did not survive scrutiny and are recorded so they are not raised again. `Sort::of`'s `Opened` is kept out of the `Context` to avoid bumping `mutation_stamp` and starving the conversion deadline; a level is `context.depth() + position`, and depth is readable without mutation, so this is unaffected. The lookup at `convert.rs:107` is a reverse linear scan with string comparison and would become indexing, but `opened` holds one entry per telescope binder, so depths are small and the win was never measured.

Reopen only on a concrete problem, not on aesthetics. If the kernel is reworked for another reason, this is the design to adopt, and the first thing to settle is the `history_key` question above. This analysis belongs in `DESIGN.md` when this specification is retired.

## Open questions

- Whether separating the hint from the identity changes `Telescope`'s shape or reuses `first_label()` as-is. Settle by reading the scope machinery before starting Phase C; it determines that phase's size more than the `Var` change does.
- Whether `zonk`'s validation order — which currently decides, alphabetically, which universe-invariant error surfaces first — should become declaration order. Phase D.
- Whether `Ord` removal extends past `Atom` to the other name-keyed `BTreeMap`s in `Context`. Phase D, and cheap to attempt: the compiler reports every site that depended on it.
- Whether authored local names ever reach core as free variables outside the prelude. Phase C should assert this rather than assume it; the ledger's coverage was prelude-only.
- Whether any table keys both kinds in a way `Free` cannot express. None found, prelude-only coverage.
