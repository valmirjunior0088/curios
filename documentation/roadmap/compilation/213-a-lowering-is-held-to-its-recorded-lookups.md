# A lowering is held to its recorded lookups

Not refined yet. A lowering is kept by the lookups it recorded, and nothing lowers a declaration again against those lookups alone to hold the record complete. The note a mismatch owes a binder that hides a module's name, its last stage, adds a lookup to that record and waits on this.

## What is missing

`curios-pipeline`'s `std_replay_census` elaborates each of `/std`'s declarations again alone and reports any that comes out another way, which holds an elaboration to its trace. A lowering is kept by the lookups it recorded ([A lowering is kept where its text and the names it resolved stand](../../design/compilation/a-lowering-is-kept-where-its-text-and-the-names-it-resolved-stand.md)), and nothing lowers each declaration again against only what its record names to hold the record complete: `std_recompile_reproduction` holds two edits, a leaf's and a hub's, to the bytes of a whole compile. Fix direction: the census's counterpart over the lowering, each declaration lowered with its baseline answering for the lookups its record holds and for no other. Size: a measurement test.

## Refinement

- **A scope that answers a record and nothing else.** `into_core_unit_over` (`curios-text/src/into_core.rs`) lowers a unit over a baseline, keeping a declaration whose every recorded name resolves as it did; nothing lowers one declaration with only its record's names in scope. Whether that scope is the lowering's own, behind its `test_support`, or a way of building a unit's source from a record, is not settled.
- **What is compared.** `std_replay_census` (`curios-pipeline/src/standard/tests.rs`) compares a declaration elaborated alone with what the archive holds for it and with what it asked; the counterpart compares a lowered item and its record. What of a lowered item is its own, a name minted anew for a kept witness among it, is not read yet.
- **Stages, once those are read**: the restricted scope; the census as an ignored measurement beside `std_replay_census`, its reading recorded with its commit; then the binder note.

## The binder note

**A lambda's binder spelled as a type in scope is reported as a mismatch that names neither the binder nor what it hides.** Against `(@n: Nat, x: Nat) -> Nat`, `(@Nat, x) => let y: Nat = x; y` reports `type mismatch`, `inferred: Nat`, `expected: Type`, its caret under the annotation of the `let`, probed again at `00627ea06`: after `@` a lambda reads a binder ([Plicity is part of function identity](../../design/theory/plicity-is-part-of-function-identity.md)), so `@Nat` binds a variable that hides `/std/Nat`, and the report shows that variable's type under the spelling the reader takes for the type itself. The `unused-binder` lint names the binder only where the body never mentions it.

- **The mismatch says it.** Where a mismatch stands at a reference that resolves to a local hiding a module binding of its name, the report holds the binder as a note, a place with a line saying whose the name is there and what it hides ([A diagnostic spells what its reader can write](../../design/tools/a-diagnostic-spells-what-its-reader-can-write.md)). No lint is added.
- **What the note reads is held nowhere a report reaches.** A written binder's place: the lowering has each one's span while it lowers (`Candidate`, `curios-text/src/into_core/lowerer.rs`) and keeps it only in the lint of one nothing mentions, a scope holding its binder's ordinal among its declaration's (`Scope::written`, `curios-core/src/scope.rs`) and no extent. And that the local hides a module binding: `Lowerer::resolve_name` answers from the binders in scope and asks the module's scope nothing where one of them matches.
- **So the lowering asks.** Where a binder in scope wins a name it asks the module's scope as well, and files for a binder that hides a binding what it hides and where it stands, which the mismatch reads by the binder's ordinal. The lookup is one the lowering's record then holds, so a name a module gains lowers again the declarations that bind it, which is why it waits on the census.
- **Check**: a fixture over the program above, its note placed at `@Nat`.
