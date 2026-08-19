# Reduction carries proofs, never constructs them

## Status

**Both halves have landed; the bound fields have not.** `curios-core`'s `peel_nat_terms` gates the cancellation on a shape rather than a carrier, so a floorless sum reaches it and the entry criterion below typechecks. Windows are `(start, length)` from `/sys` through Core, Ersd and Cont to the emitted rope helper, which lost its `end - start`. What remains is Stage 3: the four accessors carrying their bounds, and the kernel re-checking them.

Three things this specification did not anticipate, recorded because they change what Stage 3 rests on.

**The `/std` migration needed no end-style wrapper.** Counting the seventeen bounded call sites: seven were "the rest of the value" and became `Bytes/drop`/`List/drop`, which spend `Nat/Le/add_sub_cancel` once inside themselves; seven were already counts and merely dropped a now-unstatable ordering proof; and the three that genuinely computed an end had their *producers* rewritten instead — `Parse/take_while` carries `taken` under the single invariant `Le(pos + taken, len input)`, which *is* the slice's bound and is passed to it untouched. An adapter at the seam would have been ceremony at fourteen of seventeen sites.

**Two compiler-emitted slices are below erasure, where bounds do not exist.** `curios-elab`'s cons peel (`into_ersd/eliminate.rs`) and `curios-ersd`'s fold suffix (`into_cont.rs`) mint window operands themselves, and both spoke the old convention silently: the operand is a `Nat` either way, so the whole workspace and the kernel-certified prelude were green while every string program trapped. Stage 3 does not close that — a bound field buys kernel re-verification for what the *user* writes and nothing for what the compiler emits past erasure, which stays guarded by the cross-stage corpus alone.

**The cons-peel bounds do not need the widened peel.** They carry through `compare_nat`'s floor cancellation, which landed before this work. The widening is what *window fusion* needs.

An earlier revision of this specification recorded the definitional-equality half as an open rule-set design — a canonical form for symbolic sums, a "float the literal floor" rule, and a confluence argument across `Nat`'s two spellings of addition. The probes below falsified that framing: the decision machinery already exists in the tree, decides every equation this specification needs, and is withheld from the failing case by nothing but a carrier-shaped gate. What remained of that half was widening one gate under two stated contracts.

## Why it exists

Nine intrinsics state a precondition. Five of them carry it as a field the kernel re-checks; four do not, and the four are the ones `/std` is built on — `BinGet`, `BinSlice`, `ListGet`, `ListSlice`.

The blocker is a single fold law. `spine.rs` fuses adjacent windows of one base, `slice(b, s, m) ++ slice(b, m, e) = slice(b, s, e)`, and the fused slice needs a bound nobody proved: `within` is window₂'s and carries through unchanged, but `ordered : Le(s, e)` has to be composed from `Le(s, m)` and `Le(m, e)`. That is transitivity of `≤`, which no equality procedure supplies — so the reducer would have to emit a proof term at every fusion.

**A reducer that constructs proofs is the defect.** Reduction computes normal forms; discharging obligations is the elaborator's job and checking them is the kernel's. Every route that makes the reducer *able* to prove — moving a lemma into `/syn`, carrying a placeholder, deriving in `curios-core` — accommodates the defect instead of removing it. The two halves below remove the need.

## The two halves, and why neither is sufficient alone

**`(start, length)` instead of `(start, end)`.** A length is a `Nat`, so `start > end` becomes unrepresentable — `ordered` ceases to exist rather than being proved another way, and one bound remains: `Le(start + length, len b)`. This is the degree of freedom deleted rather than narrowed by a side condition.

**Widening the `Nat` peel to floorless sums.** Fusion then reads:

```text
window₁ = slice(b, s,      l₁)        bound: Le(s + l₁,        len b)
window₂ = slice(b, s + l₁, l₂)        bound: Le((s + l₁) + l₂, len b)
fused   = slice(b, s,      l₁ + l₂)   needs: Le(s + (l₁ + l₂), len b)
```

The needed bound and window₂'s differ only by reassociation, and conversion *already decides* such equations — through `Nat::cancel_common`'s floor-plus-summand-multiset cancellation, whose `Nat::summands` flattens nested `NatAdd` spines. It just never sees this pair: every reader admits two `Nat`s to the peel only when both heads are the `Intrinsic::Nat` carrier (`Succ`/`Zero`), and a floorless symbolic sum reduces to a bare `NatAdd` node, which falls through to shape congruence, where `s + l₁` against `s` refuses. Widen the gate to `Nat`-valued intrinsic pairs and window₂'s existing proof inhabits the fused bound: **proof construction becomes proof carrying** — the reducer moves a term it was handed and derives nothing.

**Neither half delivers this on its own.** Without the widening the surviving bound still differs by a reassociation the congruence refuses; without the reparameterisation `ordered` still needs transitivity, which is an implication rather than an equality and outside any peel's theory. Anyone picking up one half should not read it as sufficient.

## Known for certain

Every probe below was run on the compiler tree of `dd68792e` (2026-08-19; the commit carrying this specification changes documentation only).

- **The entry criterion, unchanged.** The work's definitional-equality half is done when this program typechecks:

  ```crs
  use /std/{Handle, Str, Nat, Eq};
  let assoc(s: Nat, l1: Nat, l2: Nat) -> Eq((s + l1) + l2, s + (l1 + l2)) =
      Eq/refl();
  /std/print("ok")
  ```

  ```text
  type mismatch
    inferred: Eq(@Nat, (s + l1) + l2, (s + l1) + l2)
    expected: Eq(@Nat, (s + l1) + l2, s + (l1 + l2))
  ```

- **The floored twins already decide, which is what locates the whole distance inside the gate.** The same program with a literal floor on both sides — `Eq(((s + l1) + l2) + 1, (s + (l1 + l2)) + 1)` — typechecks today, and so does the commuted pair `Eq((a + b) + 1, (b + a) + 1)`, while floorless `Eq(a + b, b + a)` refuses. Associativity and commutativity are both within `cancel_common`'s theory (it landed in `fb727aa5`, before any of these probes ran); the floor is what routes a pair past the carrier gate, and nothing else separates the passing probes from the failing ones.

- **The gate is spelled three times, and the widening belongs below all of them.** `curios-core/src/spine.rs`'s `peel_intrinsic` matches `(Intrinsic::Nat, Intrinsic::Nat)`, and `curios-elab/src/convert/intrinsic.rs` and `curios-cert/src/kernel/convert/intrinsic.rs` each carry a local `peel_nat_pair` doing the same. The readers are the two congruences and `curios-analysis/src/invert.rs`'s inverter; a widened entry point in `curios-core` that all three call is what keeps them incapable of disagreeing, and collapses the two hand-rolled copies.

- **"Float the literal floor out of a sum" already exists.** `NatAdd`'s fold hoists every literal floor outward — `curios-core/src/nat.rs` states it as the invariant `Nat::summands` relies on — so a reduced sum is one floor over floorless summands, and `Nat::decompose` plus `Nat::summands` read the `Succ` spelling and the `NatAdd` spelling into one view. No confluence question across the spellings arises, because nothing rewrites: the peel is a compare-side decision procedure, not a normal form.

- **The reduce-side alternative is rejected, and the tree itself rejected it twice.** `9800293f` deleted a reduce-side slice reassociation from this same family for non-confluence, and `Nat::cancel_common`'s no-progress arm documents why a rebuilt reordered sum never settles under a caller that re-reduces it. The settled pattern is that free-monoid equality is decided by the compare-side peel — `Bin`, `List`, and floored `Nat` all already work this way, as one enumerated family in `spine.rs` — and instituting a reduce-side canonical form for floorless `Nat` beside it would leave two overlapping equality mechanisms whose agreement nothing checks, plus the whole-program cost of normalising every sum. The compare-side route pays only at stuck comparisons of `Nat`-valued pairs, which the floored fragment already pays.

- **Only the fusion site is blocked.** `bin_locate`'s index rebasing and `bin_window`'s piece narrowing both go through `bin_segments`, which returns `None` unless every segment is a literal run — so their bounds are all-literal, decide to `True`, and are discharged with nothing written. Establishing this is what reduced the problem from three sites to one.

- **`curios-cont` is already count-based.** "A window is `(base, offset, length)`: any rope is its own whole window as `(r, 0, len r)`, a virtual slice is offset arithmetic behind a `WindowExtent` guard" (`cps/fields.rs`). Core and Ersd carry `(start, end)` and the backend converts, so the reparameterisation moves Core to where its own value already ends up rather than inventing a convention.

- **It widens the trusted base.** `curios-core`'s spine algebra is shared by `curios-elab` and `curios-cert` alike, and `documentation/soundness.md` grades intrinsic fold laws "argued in code comments only". The widening extends *where* the peel's `Equal` verdict fires rather than *what* it claims — the multiset cancellation and its soundness argument are `fb727aa5`'s, already landed — but a perimeter entry is owed for the extended coverage all the same.

## Implementation contracts

Two, and both are load-bearing rather than style:

- **No progress must fall through as `Stuck`, never `Continue`.** Today every `Continue` out of `peel_nat` strips a positive floor — both heads being `Succ` guarantees a shared one — and that structural decrease is the termination argument. A floorless pair sharing no summand comes back from `cancel_common` untouched; handed to the caller as `Continue` it re-enters the same congruence and loops. Returning `Stuck` falls through to shape congruence, exactly as `Bin`'s and `List`'s peels already do — and it changes `peel_nat`'s documented contract that `Stuck` stays unreachable for `Nat`, which the grid test asserts today and must stop asserting.

- **The grid test extends to what the widening newly decides.** `curios-core/src/reduce/intrinsic.rs`'s closed-instantiation grid (`4521b7b5`) checks every peel verdict against ground truth at closed instantiations of its symbols, which is the only thing that can distinguish a valid equation from a plausible one. Floorless reassociated sums, floorless commuted sums, and the no-shared-summand `Stuck` join it.

## The commutativity decision

The earlier revision deferred commutativity as a decision to be taken deliberately rather than arrive as a side effect. It had already half-arrived when that was written: `fb727aa5`'s cancellation is a multiset, a unit test asserts a commuted floored sum decides equal, and the probes above confirm it end-to-end. The real decision is therefore ratify or carve out, and this specification ratifies: delivering associativity without commutativity would mean building an order-respecting flattener beside `cancel_common` purely to avoid deciding something the landed mechanism decides, and would keep the asymmetry where adding `+ 1` to both sides of an equation changes whether it is decidable. Sound because `+` commutes; uniform because one procedure decides one theory.

## What "all slice functions" means

Two Core intrinsics, `BinSlice` and `ListSlice`, behind three `/sys` declarations — `Bits/slice`, `Bytes/slice` (the two grains of `bin_ops`) and `List/slice`. Those are the bounded ones, and they are the whole of the reparameterisation's subject.

`/std/Str/slice` is not one of them, and it is worth reading because it already argues the case:

```crs
pub let slice(s: Str, x: Nat, y: Nat) -> Str =
    take_n(y - x, drop_n(x, s));
```

It carries no bound — `take_n`/`drop_n` saturate — and it is not built on `BinSlice`. Its first act is `y - x`: it converts the end to a count immediately, because a count is what the implementation wants. `(start, end)` is a skin over a count-based body.

**It should change too, for surface consistency**, and it gets shorter doing so — `take_n(length, drop_n(start, s))`, with the subtraction gone. Leaving it end-based while the bounded ones become count-based would give the standard library two slice conventions, which is worse than either. It also removes a wart of its own: today `Str/slice(s, 5, 3)` computes `take_n(0, …)` on truncating subtraction and silently answers the empty string, where a count cannot express a reversed range at all.

It owes no bridge lemma, precisely because it is unbounded. That obligation belongs to the three bounded declarations alone.

## The `/std` bridge, which is the part that will surprise

Most callers hold an **end**, not a count — `Str/slice(digits_str, kn, len)` and its like throughout `/std/Str`, `/std/Flt` and `/std/BigNat`. Rewriting one as `slice(s, kn, len - kn)` needs `Le(kn + (len - kn), len s)`, and **`kn + (len - kn) ≡ len` is not definitional**: `Nat` subtraction truncates, so the identity holds only under `kn <= len` and cannot be a reduction law without that hypothesis.

So `/std` owes an end-style wrapper over the count-style intrinsic, and one inductive lemma about truncating subtraction under `Le`, written once rather than at every call site. Budget for it: the migration is not a mechanical rewrite of the call sites. The lemma's prerequisite has landed — `b13b98cb` nested the inductive order under the decided one as `Nat/Le/Ind`, which is the induction it runs on.

## What it unblocks

The four sequence accessors become a mechanical addition to the `signature` table that already carries the five, so the kernel re-verifies index and window bounds rather than trusting elaboration. Beyond them, every bound that does index arithmetic stops being special.

Independently of that, `(start, length)` is worth taking on its own merits: one bound instead of two at every call site, an invalid window that cannot be spelled, and Core agreeing with the backend.

## Deliberately not specified

The perimeter entry's text, owed as an extension of the peel's existing coverage rather than a fresh mechanism; which other associative operations follow `+` (`*`, the bitwise families and `Bool`'s connectives are all monoids, and the same widening pattern would close `*`'s known-missing unit and annihilator — but `Flt` must be excluded, because IEEE addition is genuinely not associative and a procedure equating reassociations would be false of the running program rather than merely incomplete); and distributivity, which is not a monoid law and gets no entry here.
