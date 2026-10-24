# A type former is one term however it is reached

[The surface language](../../syntax.md#names-calls-and-projections) accepts `join(values)`, `join(@Nat, values)` and `join(@_, use custom_show, values)` as one call: an implicit argument written is the argument inferred. A written type former and an inferred one are two terms to elaboration, so a fact keyed by one does not answer the other, and writing an implicit argument refuses a program that leaving it out accepts.

**Not refined yet.** It waits on the choice under *Refinement* of the one spelling a type former takes, and on the measurement that chooses it.

## What is wrong

**A type former reaches a term two ways.** Written, `Point` is the global it names, `Var(/Point)`, and `Nat` is `/sys/Nat/Nat`, a definition whose body is the node. Reached by unification, it is whatever the side it was solved against held, and a type a signature states holds the node: `l: List(Point)` elaborates to `List` over `StructType { name: /Point, .. }`, and `l: List(Nat)` over `NatType`. The printer writes both as `Point` and `Nat`.

**A case equation's key is compared as it is spelled** ([Case equations and their key](../../design/soundness/case-equations-and-their-key.md)), so a guard keyed under one spelling meets no statement spelled the other. Probed at `25125304c`, with `every` a recursive `(@T: Type, l: List(T), pred: (T) -> Bool) -> Bool` and `checked` taking `Holds(every(l, small))`:

```crs
pub let f(l: List(Point)) -> Nat = match every(@Point, l, small) | true => checked(l, @True/qed()) | false => 1 end;
```

is refused, `True` inferred where `Holds(every(@Point, l, small))` is expected, and so is the converse, a guard inferred against a statement written `every(@Nat, l, small)`. The guard and the statement both left inferred, or both written, are accepted. Printed with their structure in a scratch copy, the guard's key holds `@Var(/Point)` and the statement's probe `@StructType { name: /Point, .. }`, so the probe finds no entry and the equation is never asked; the two print alike. A callee that does not recurse reduces past the argument and escapes. `/std/Tui/Glyph/of_cluster` is refused as a report prints it for this reason (`REFUSED`, `curios-pipeline/src/standard/tests.rs`): its guard's fact proves a field stated over `List/all(chars, printable)`, and the printer writes the implicit argument.

**`/sys`'s declarations hold the nodes and a program the names.** `projection` (`curios-text/src/sys_module/projected.rs`) reads each type core's table states back as the node it is, and the roster builds `Nat`, `Byte` and `Bool` the same way (`curios-text/src/sys_module/helpers.rs`), so `/sys/Cell/fill` returns `Io(Bool)` over the nodes `IoType` and `BoolType`, where a program's `let f(b: Bool) -> Bool` elaborates to `(b: /sys/Bool/Bool) -> /sys/Bool/Bool`. The two meet by unfolding the name, as conversion does and a key does not.

## What is known

- **One read-back by name was tried and taken out**, on top of `5b0ea16f9`: a type former read back by name, a node whose signature produces a sort written by `curios_core::intrinsic_path` over its operands, a bound left the node its row states, the three helpers written alike. The prelude elaborated and certified and the `curios-text` tests passed with one test restated, but `elaborate_and_zonk_with_prelude` took 52,486 to 52,727 ms over three runs of `cargo xtask profile-prelude elaboration` against 52,346 to 52,431 ms without it (medians 52,672 and 52,401), and `/sys`'s image grew from 821,108 to 940,758 bytes; certification's `recheck_module` took 16,826 to 17,138 ms against 16,793 to 17,355. The documentation record came out the same size, 899,357 bytes; the pages were not compared.
- **A solution keeps the spelling its problem wrote** ([`curios-elab`'s README](../../../curios-elab/README.md#a-solution-is-the-rigid-side-as-written)): an implicit met against a signature's `Nat` is the name `/sys/Nat/Nat`, and one met against a type `/sys` states, or synthesized from one, the node it holds.
- **The key's comparison is decided.** Structural equality under-approximates definitional equality and costs nothing to justify, and the kernel keys as it is handed; a key that read through a type former's name would be a key that unfolds, in both checkers.
- **A type former's entry is held by its definition** ([`curios-core`'s README](../../../curios-core/README.md#a-nominal-declarations-entry-is-held-by-its-type-former)); [compilation specification 305](../compilation/305-a-type-former-is-spelled-by-its-entry.md) reserves stating it once there, a question of what a declaration stores and not of how a term names it.
- **A guard bound as a hypothesis meets no key.** [A guard is a hypothesis its arm binds](../soundness/301-a-guard-is-a-hypothesis-its-arm-binds.md) would have an arm bind its guard's fact, which a statement meets by conversion, and conversion unfolds a name as no key does, so a guard and a statement spelled apart would meet. That redesign is a capability, and this fix waits on none of it.

## Refinement

- **The one spelling.** The name, which a report writes and a program reads, every node a solution or a reduct leaves read back to the former that holds it as the try above did; or the node, every name a program writes elaborated to the node its former holds, as `/sys` holds its own. Either is stated where a term is made, in both checkers' terms, so no key needs to read through it.
- **The measurement that chooses.** For each spelling, `profile-prelude`'s two halves three runs each way, the image's size, and a program typed against many `/sys` operations, against the read-back's 0.5% above.
- **Check**: the guards above, written and inferred in each combination, at a structure, at `Nat` and at `Str`; `/std/Tui/Glyph/of_cluster` read back, leaving `REFUSED`; `/std`'s image read against its census.
