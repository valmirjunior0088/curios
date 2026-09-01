# Auto-derive: a witness whose body the compiler writes

## Status

The elaborator ground is prepared and pinned by tests in the tree; nothing of the mechanism is built. This specification is the build plan alone: each step carries its own contract, and a landed step is replaced by one checked line, its contracts moved to their owners. The `Spell` concept and its written witnesses are landed — the test harness shipped them (see [the decision](../design/toolchain/a-test-is-a-declared-description-run-by-a-synthesized-tail.md)); the steps below build on them and change no signature.

## Mission

A witness declaration may omit its body: `satisfy Spell(Point);`. The compiler then writes the body from the declaration of the type in the witness's key, or refuses compilation naming what it could not write. Derivability is a property of the concept, registered in the compiler against its registry slot; this specification registers two, `Spell` and `Eql`. Hand-written witnesses remain the norm everywhere else: carriers keep their written witnesses, `Show` stays human-facing and underivable, and tuple shapes take `/std/Tuple`'s written per-arity witnesses ([A tuple type is keyed by its shape](../design/language/a-tuple-type-is-keyed-by-its-shape.md)), not derivation.

## Steps

- [ ] 1. The body-less `satisfy` form
- [ ] 2. `Transient::Derive`
- [ ] 3. `Spell` moves to `/syn`
- [ ] 4. The `Spell` derivation
- [ ] 5. The `Eql` derivation
- [ ] 6. Derived witnesses in `/std`
- [ ] 7. Documentation, the decision file, and this file's deletion

Each step is one authorization and one commit, lands its tests before its mechanism, runs the full gate at its end, and needs the step before it.

### 1. The body-less `satisfy` form

**Lands.** After the concept application, `;` in place of `{ … }`, refused today. Both forms remain; a body-less member joins an `and` group as a written one does, and a group may mix the two. `TopWitness`'s body becomes written-entries-or-absent; the parser (`curios-text/src/parse/top_level.rs`), the printer (an absent body prints `;`), the `curios format` round trip, the tree-sitter grammar with regenerated committed `src/`, both editor extensions, and the `syntax.md` sentence. `satisfy Spell(Tree);` for a parameterized `Tree` stays refused as today, the report quoting the telescope form to write.

**Verification.** Parse and print round trips for the lone form, the group form and a mixed group; the formatter leaves the form unexpanded; the tree-sitter drift check passes.

### 2. `Transient::Derive`

**Lands.** Lowering carries a body-less witness into Core as the same anonymous definition a written witness produces, with `Transient::Derive` in body position (`curios-text/src/into_core.rs`). One new `elaborate` arm: checking a `Derive` against an expected type reduces it to a concept application, looks the concept up among registered derivations by registry slot, calls the derivation, and checks the returned Core body against the expected type; a concept with no derivation is refused — "`Foo` has no derivation; write the body" — and a `Derive` met in inference mode is unreachable from the surface and refused. Zonk treats a surviving `Derive` as the other transients, and the kernel refuses it as `NotCore` (`curios-cert/src/kernel/infer.rs`). No derivation registers yet: every body-less witness is refused by name until step 4.

**Verification.** `wonder stage core` shows the transient; a body-less witness for `Show` and for a user concept reports the no-derivation refusal at the declaration's span; orphan, duplicate-key and sealed-concept refusals fire on a body-less signature exactly as on a written one.

### 3. `Spell` moves to `/syn`

**Lands.** The concept relocates from `/std/Spell.crs` to `/syn/Spell.crs`, registered in `syn.crs`, the `/std` module becoming the facade (`pub use /syn/{Spell}; pub use /syn/Spell/{spell};`); a `SyntaxRegistry` slot, spelled in `curios-prelude-archive/src/syntax.rs`, covered by the prelude presence check. The carrier witnesses stay where the harness put them. The move happens because step 4's generated bodies name `Spell/spell`, and a compiler-emitted name lives in `/syn`.

**Verification.** The prelude builds; every harness test that spells passes unchanged; no consumer names `/syn/Spell` directly.

### 4. The `Spell` derivation

**Lands.** The derivation registered for `Spell`'s slot. Eligibility, per concept parameter, each refusal a hard error at the `satisfy` span in a frame naming concept, key and declaring module: a registered `induct` or `struct` — not an intrinsic carrier, not a concept's backing struct, not `Prop`-sorted; representation-transparent at the declaring island; fully applied, every parameter and index bound by the witness's telescope or given concretely.

The generated body is the Core the lowerer would produce for the equivalent surface program: one `match` arm per constructor in declaration order (omitted motive; index inversion prunes as for written matches), `Proj` for struct fields, `Str/concat` of the pieces. A value spells as its constructor's absolute path applied to its explicit payloads — `/Tree/node(/Tree/leaf(1), …)`, `/Point { x = 1, y = 2 }`, labels where fields have them — so the text re-parses from any module that sees the names. Field populations: an implicit payload is omitted (the re-parsed call infers it); an explicit payload of sort `Type` or `Prop` is refused; an explicit proof payload spells `?`, a written goal; every other payload is spelled by `Spell/spell(field)`, resolved by ordinary resolution in the witness's scope — a telescope variable's premise from its `use` binder, recursion through the witness's own entry, a mutual family through one body-less `and` group, an occurrence under another former through that former's witness. A missing field witness reports inside a derive frame naming constructor and payload, adding "add `use Spell(A)` to the telescope" when the failing type is a telescope variable.

**Verification.** A `curios/src/tests/derive.rs` in the `run(source)`/`error(source)` style: an enumeration, payloads, a parameterized family under a premise, a recursive family, a mutual group, nesting through `List`, a user struct and a tuple, labeled and positional structs, a `Prop` field spelling `?`, an indexed family; spelled text re-elaborated at the type and compared through `Eql`; every refusal asserting its frame text; `wonder stage core-elab` showing an expansion and `stage core` the transient.

### 5. The `Eql` derivation

**Lands.** The derivation registered for `Eql`'s slot, sharing step 4's eligibility and frames. The generated `eql` matches the two scrutinees pairwise — one arm per constructor pairing its payloads, a default arm answering `false` — comparing payloads through `Eql/eql`, resolved as step 4 resolves `Spell/spell`; `neq` is its negation. Implicit and erased payloads do not participate; a payload whose type has no `Eql` witness refuses in the frame.

**Verification.** The step-4 roster replayed for `Eql`: derived equality agrees with structural equality on constructed values, `!=` projects the derived `neq`, and the refusals assert their frames.

### 6. Derived witnesses in `/std`

**Lands.** The written `Spell` and `Eql` witnesses for `Option`, `Result` and `Order` are replaced by body-less declarations, so the prelude exercises both derivations on every workspace check. `List`'s written witnesses stay: intrinsic carriers are written by design.

**Verification.** The prelude archive build holds the derived witnesses; the harness suite passes unchanged; `wonder stage core-elab` of a prelude probe shows a derived body.

### 7. Documentation, the decision file, and this file's deletion

**Lands.** `syntax.md`'s witness section states the body-less form, the derivable-concept rule and the eligibility; `documentation/design/language/a-witness-body-may-be-written-by-the-compiler.md` records the design — the signature is the programmer's, the expansion is a checked term behind a transient, derivability is per concept, every derived body is re-checked by the kernel — with deriving clauses on declarations, premise inference, a Text-stage expansion, deriving `Show`, and a description-based `Generic`/`Mirror` route (nominal types here have dependent telescopes a first-order field description cannot express) recorded as rejected. The roadmap item is checked with a summary; this file is deleted with its contracts moved to `syntax.md`, the derive module's rustdoc, the `/syn` sources and the decision file.

## Completion criteria

- `satisfy C(T);` parses, prints and formats; the grammar agrees; `Transient::Derive` exists and cannot survive to the kernel.
- `/syn/Spell` has its slot and facade; `Spell` and `Eql` derive; `/std`'s structural types hold derived witnesses, exercised by every workspace check.
- Every step's Verification row is a test, and the gate passes.
- The decision file exists, the roadmap line is checked, and this file is deleted.

## Seam

What a later derivable concept needs that these two do not, recorded so the seam is not mistaken for complete: derivations that use other derivations (`Ord` reusing `Eql`), derivations that produce proofs for law fields, and the harness's generator and shrinker. Each is an arm and a slot at this seam.
