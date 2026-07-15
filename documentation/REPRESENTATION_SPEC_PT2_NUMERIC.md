# Representation specification PT2 — packed `BigNat`, `BigInt`, and conversion

Working implementation specification for moving Curios's arbitrary-precision natural and integer layers onto the packed `Bits` substrate from [packed Bits, Bytes, and Byte syntax](SYNTAX.md#literals), preserving their machine-checked algebra, keeping Dragon4 operational, and testing whether conversion can return to a smaller uniform coinductive bisimulation. [PT3](REPRESENTATION_SPEC_PT3_CHARACTER.md) is the subsequent character/string migration. [PT4](REPRESENTATION_SPEC_PT4_BIGFLT.md) deliberately postpones `BigFlt`, `Flt/of_le_bytes`, correctly rounded float boundaries, and every helper or proof obligation required only by that work.

This is a working implementation reference, not permanent architecture documentation. Once the work lands, fold durable conclusions into `AGENTS.md`, `ROADMAP.md`, relevant rustdoc, and standard-library documentation, then delete this working series.

## Motivation

Curios conversion is a **coinductive bisimulation**. Definitional equality is decided by observing one layer at a time, comparing it, recursing on subgoals, and cutting when a canonicalized goal recurs. Curios cannot normalize both sides and compare normal forms because general recursion is first-class, including at the type level.

The invariant is one-sided and load-bearing: conversion may succeed, fail, or time out with `Preempted`, but it must never report a false positive. Before arbitrary-precision naturals became a constructor-per-bit spine, the bisimulation decided the proof corpus within its deadline.

The current `BigNat` uses canonical binary constructors (`Pos = one | o(Pos) | i(Pos)`, with `BigNat = zero | pos(Pos)`) because constructor algebra is transparent to proofs while native limb `/`, `%`, and `-` on symbolic operands are opaque. That design made the algebraic corpus provable but allocates one heap object per bit. Dragon4 in `curios-text/std/Flt.crs` repeatedly walks roughly 150-bit values, and type-level reductions traverse the same deep spines. The result pushed conversion against its deadline.

The response, on the third attempt at the problem, was a recursion-specific performance subsystem: a `recursive` flag on definitions, match-guarded delta, per-group `RecId`, memoized recursive unfoldings, and raw-window/transient machinery. It spans roughly thirteen files across reduction, conversion, elaboration, erasure, terms, scopes, primitives, zonking, printing, and tests over `d08933e..HEAD`. The `RecId(0)` collision fixed by `ba9dfa6` and the transient leak fixed by `6387921` demonstrate the correctness surface created by those special paths.

PT1 supplies the upstream correction: structural bit computation remains transparent but runs over native packed storage with O(1) logical tails. PT2 ports the number layers and then measures whether the recursion-identity performance mechanisms are still necessary. It does not delete the elaboration boundary that keeps unfinished recursive bodies out of conversion unless an equally strong invariant replaces it.

## Scope and dependency boundary

PT2 begins only after PT1's packed B carrier and property tests pass. It owns:

- `BigNat` as a certified canonical interpretation of `Bits`;
- a packaged nonzero magnitude type;
- `BigInt` as the signed arbitrary-precision layer;
- the existing algebraic and order proof corpus for those types;
- the BigNat operations used by Dragon4 and other current consumers;
- performance verification and possible retirement of recursion-specific conversion machinery.

PT2 does not own:

- `Byte`, `Grain`, Bits/Bytes syntax, packed cursors, or carrier operations, all fixed by PT1;
- `Char` or `Str`, fixed by PT3;
- `BigFlt`, `Flt/of_le_bytes`, `widen_b`, `narrow_b`, `narrow_ratio_b`, float boundary theorems, BigFlt-specific order lemmas, or the rational denominator extension, all fixed by PT4.

Helpers such as top-bit extraction, guard/sticky computation, or any additional division loop are not PT2 obligations when their only consumer would be PT4. General BigNat and BigInt operations and laws remain in PT2 even where PT4 will later consume them, because they are part of the standalone numeric types' honest API.

## Design keystones

**Privilege the packed substrate, not the number tower.** `BigNat`, `NonZero`, and `BigInt` remain ordinary `.crs` types. The compiler privilege ends at `Bits`; no arbitrary-precision arithmetic primitive or decision procedure is introduced.

**The bit remains the arithmetic atom.** Addition, carry, subtraction, multiplication, comparison, and shifts bottom out in Bln case analysis and structural `Bits` views. Proofs never depend on native limb division, remainder, or subtraction on symbolic values.

**Canonicity is certified at the numeric boundary.** `Bits` is a sequence: `b\1` and `b\1\0` are distinct even if a little-endian interpretation assigns them the same number. `BigNat` adds a proof-irrelevant certificate excluding high zero bits.

**Equality remains structural identity.** A canonical representation is unique, so ordinary `Eq` and rewriting remain valid. Do not replace identity with a setoid equality over noncanonical bitstrings.

**Signed zero remains unrepresentable.** `BigInt` uses `neg(NonZero) | zero | pos(NonZero)`, never `neg(BigNat) | zero | pos(BigNat)`.

**Conversion cleanup is empirical and safety-gated.** Packed storage creates the opportunity to remove recursion-specific performance workarounds; it does not prove them unnecessary. Disable and measure before deleting, and separate performance mechanisms from unfinished-body isolation.

## Part 1 — delivered baseline being migrated

The current constructor-based implementation and its proof corpus are the semantic baseline. PT2 changes representation and proof routes, not public mathematical behavior.

### Current `BigNat`

The delivered `BigNat.crs` has `Pos` (`one`, `o`, `i`) and `BigNat` (`zero`, `pos`), canonical by construction. Its operations are constructor algebra:

- `pos_succ`;
- mutual `pos_add` and `pos_add_c`, with carry in the function rather than the data;
- high-bits-first `pos_cmp`, with the low bit breaking a tie only after recursive comparison;
- truncating subtraction through a `Pos.sub_mask` borrow recursion;
- shift-and-add `pos_mul`;
- `mul_pow2` as a Nat fold of doubling;
- O(1) head-based parity and structural halving;
- boundary `of_nat`, `mul_small`, and `to_str`, with decimal rendering using binary long division.

The machine-checked lemma base includes:

- Pos addition: `one_r`, `succ_l`, `succ_r`, `comm`, `assoc`, `cancel_l`, `cancel_r`, `no_fix`, and no-confusion/injectivity helpers;
- Pos multiplication: `one_r`, `o_r`, `i_r`, `comm`, `distrib_l`, `distrib_r`, and `assoc`;
- Pos comparison: `refl`, equality reflection to structural `Eq`, `flip`, `succ`, `lt_add`, the less-than witness Σ-lemma, and transitivity by witness composition;
- BigNat lifts: `add_comm`, `add_assoc`, `add_cancel_l`, `add_cancel_r`, `mul_comm`, `mul_assoc`, `distrib_l`, `distrib_r`, `cmp_refl`, `cmp_eq`, `cmp_flip`, `cmp_trans`, `mul_pow2_add`, `mul_pow2_mul_l`, and `mul_pow2_compose`.

These are permanent standard-library assets and part of a proof corpus containing roughly 600 existing `Eq` lemmas. PT2 must preserve the same theorem strength even where the proof statement or internal helper names need to follow the certified packed representation.

A known optimizer issue shaped `pos_cmp`: the ersd specializer can lose a minted `@s0` item for a recursive function applied to a literal inductive argument, surfacing as `into_cont lacks value .../pos_cmp_c@s0` under a switch-then-recurse form. The delivered combine-after-recursion shape avoids the trigger. PT2 must retest the equivalent packed recursion rather than assuming the cursor transform automatically removes the issue.

### Current `BigInt`

The delivered type is already the desired sign shape:

```crs
pub induct BigInt : Type
| neg(Pos)
| zero()
| pos(Pos)
end
```

Its operations include signed addition via `pos_sub(p, q) : BigInt`, a nine-row recursion over `dbl`, `dbl_succ`, and `dbl_pred`; subtraction by adding the negation; multiplication by constructor sign and positive magnitudes; `neg`, `abs`, `magnitude`; `Order`-valued `cmp`; Bln-valued `lt`, `lte`, `gt`, `gte`, and `eql`; conversions from Nat, BigNat, and native Int; parity and halving; and signed decimal rendering.

The delivered proof stack includes:

- `pos_sub_diag`, `pos_sub_succ`, mutual `pos_sub_add_l`/`pos_sub_succ_add`, `pos_sub_flip`, `pos_sub_of_lt`, and `pos_sub_of_gt`;
- the decomposition laws `pos_sub(p+q, r) = pos(p) + pos_sub(q, r)` and `pos_sub(x, y+z) = pos_sub(x, y) + neg(z)`;
- `add_zero_r`, `add_neg_l`, `add_neg_r`, `neg_neg`, `neg_add`, `add_comm`, `add_assoc`, and left/right additive cancellation;
- `mul_comm`, `mul_assoc`, `neg_mul_l`, multiplication over positive subtraction, left/right distributivity, and left/right multiplication cancellation with a positive-scalar premise;
- `cmp_refl`, `cmp_eq`, `cmp_flip`, and `cmp_trans`, from which antisymmetry follows because the carrier is canonical.

The sign split belongs here once. Downstream exact numeric types should consume BigInt's ring and order interface rather than repeating sign-case products.

Operator witnesses for BigNat and BigInt remain in the `/std` operator facades (`Add.crs`, `Sub.crs`, `Mul.crs`, `Eql.crs`, `Cmp.crs`, and related facades), not in each type module. The current witness-binding and topological-order rules remain binding.

## Part 2 — packed `BigNat`

`BigNat` becomes a representation-private certified wrapper over little-endian `Bits`:

```text
BigNat = struct {
    rep       : Bits,
    canonical : Canonical(rep),
}
```

Matching `rep` removes the least-significant bit first. Empty represents zero. A nonempty canonical representation ends in `1`; a logical suffix of high zero bits is forbidden.

The current `one`/`o`/`i` constructor algebra becomes a view over the packed sequence:

- empty corresponds to zero;
- a nonempty B head supplies the low bit;
- a positive/nonempty wrapper supplies the role previously played by `Pos`;
- recursive tails are O(1) packed views rather than allocated inductive nodes.

Arithmetic remains ordinary Curios code. Single-operand structural recursions may consume the carrier-provided induction hypothesis. Two-operand operations and carry/borrow families remain explicit `rec` groups where necessary.

The required PT2 computational surface is at least:

```text
add, sub, mul, cmp
mul_pow2, mul_small
of_nat, to_str
eql, lt, lte, gt, gte
```

Keep the Dragon4-facing names and contracts stable where possible. Additional helpers belong in PT2 only when required by these standalone operations, their proof corpus, or an existing non-BigFlt consumer. PT4 owns helpers introduced solely for float widening, narrowing, rounding, or rational extension.

### Smart construction

Every operation finishes through a proof-producing constructor that trims logical high zero bits and returns `Canonical`. Trimming must respect logical bit length; physical byte padding is never part of the value.

The wrapper erases to its single relevant `Bits` field. There must be no runtime certificate, wrapper tuple, per-number tag, or per-bit allocation.

## Part 3 — canonicity

The redundancy in a little-endian bit sequence is exactly a suffix of high zeros. Define:

```text
Canonical(bits) := bits = b\ or last(bits) = true
```

An equivalent inductive or boolean-reflection formulation is acceptable if it supports the same transparent structural proofs. The invariant means empty is the unique zero and every nonempty value has a high `1` at its logical end.

Requirements:

- `Canonical(rep) : Prop` is proof-irrelevant and erases;
- canonical representations have injective numeric interpretation;
- trimming preserves numeric value and produces `Canonical`;
- each arithmetic operation preserves its stated numeric behavior and returns a canonical result;
- equality of certified BigNat values reduces to equality of their data fields, with certificates discharged by proof irrelevance.

This certificate burden replaces canonicity-by-construction. It is the explicit cost of using a general packed sequence as the privileged substrate. The B view makes the proofs feasible because they remain bit-constructor algebra; the earlier trimmed-limb certificate failed because symbolic limb `/`, `%`, and `-` were opaque.

Prototype `Canonical`, trimming, and `add` end-to-end before porting the full corpus. Confirm proof elaboration, erasure to bare `Bits`, and absence of per-bit allocation. This is the go/no-go gate for PT2.

## Part 4 — `NonZero` and packed `BigInt`

Define a canonical nonzero magnitude without admitting zero. It may be a private nonempty certified `Bits` wrapper or a `BigNat` paired with a nonzero proof, provided it erases to the packed magnitude and gives structural access needed by proofs.

`BigInt` remains:

```text
BigInt = induct
    | neg(NonZero)
    | zero()
    | pos(NonZero)
```

This preserves unique representation and makes signed zero uninhabitable. `BigInt` receives no compiler privilege and inherits packed magnitudes from BigNat.

Port the delivered operations and theorem families from Part 1. Prefer preserving public names and statements. Proofs may use the new canonicality and nonempty certificates internally, but public algebraic laws must not require callers to thread representation premises.

Native `/sys/Int` remains the pragmatic signed i31 type for indices, counters, codes, and host boundaries. `BigInt` is the exact arbitrary-precision integer. Native `Int` is type-level ℤ but runtime i31 with overflow traps; do not conflate its contract with BigInt. The existing `BigInt/of_int` route uses `Int/abs`, whose carrier-minimum input `-2^30` traps; preserve or deliberately repair that documented boundary rather than silently changing it during the packed migration.

## Part 5 — Dragon4 and numeric boundaries

Dragon4 remains in `curios-text/std/Flt.crs` and continues to use exact BigNat operations for decimal rendering. Its current dependency surface is:

```text
BigNat/mul_pow2
BigNat/mul_small
BigNat/of_nat
BigNat/add
BigNat/sub
BigNat/lt, lte, gt, gte
```

The relevant functions are `pow2`, `divmod`, `scale_up`, `scale_down`, `generate`, and `setup`. Dragon4 carries no proofs at runtime. Port it after BigNat arithmetic and before conversion measurements so the representative 150-bit workload exercises the packed implementation.

`of_nat` and decimal `to_str` remain boundary computations allowed to use native arithmetic on runtime values. No algebraic proof should depend on open native `Nat` subtraction, division, or remainder reducing.

PT2 does not add correctly rounded `BigFlt` encoding or decoding. Native `Flt/to_str` through Dragon4 remains distinct from PT4's exact dyadic boundary work.

## Part 6 — conversion after the representation change

The desired endpoint is a small uniform coinductive-bisimulation surface. PT2 tests that endpoint; it does not assume it.

### Keep

- Coinductive goal canonicalization from `0858150` (`history_key`), which operates uniformly on goals and recognizes recurrence across fresh binder openings.
- Alpha-equivalence from `ced3ba5`.
- A mechanism preventing raw or not-yet-elaborated recursive bodies from entering reduction and conversion.
- Deadline behavior and the no-false-positive invariant.

### Performance candidates to retire after verification

- `DefEntry.recursive`, `Context::is_recursive`, and the `define_rec_members` special path from `6dfde58`;
- match-guarded delta in `reduce_apply`, including `unfold_guarded_apply` and `GuardedUnfold`;
- `RecId`, `Rec::id`, `fresh_rec`, `rec_unfolds`, `rec_unfold`, `remember_rec_unfold`, and memoized `unfold_rec` keyed by group identity (`6dfde58`, `ba9dfa6`, `6387921`);
- conversion parking or backstop branches whose sole remaining purpose is recursion-specific performance.

### Refactor or retain until replaced

The raw-window/transient apparatus includes `Term::contains_transient`, `Subterm::any_transient`, `DefEntry::withheld`, `Context::is_withheld_rec_member`, `refresh_parked_rec_members`, and conversion branches that consult them. Although coupled to the performance subsystem, it also protects conversion from incomplete recursive definitions during elaboration.

Delete those pieces only after the elaborator makes the same exclusion structural, for example by elaborating a recursive group in a private context and publishing only fully elaborated, zonked members. Add regression tests that expose raw or misfolded bodies before removing the old protection.

After safe retirement, a stuck recursive application may reduce to its natural stuck match and conversion may decide it through ordinary coinductive recurrence. If profiling still favors folded neutral heads, test **uniform lazy delta**: compare definition heads first and unfold on demand for all definitions, never through a recursive flag or per-group identity.

## Part 7 — proof and soundness discipline

Curios currently performs no termination or strict-positivity checking. `rec absurd : False = absurd` typechecks, so review discipline remains binding:

- every proposition is built through checked structural elimination, checked inductive case analysis, or recursion on an evident structural subterm;
- no proof is a bare or disguised self-reference;
- canonicity and arithmetic proofs remain manifestly positive;
- runtime computations may use general recursion, but no logical claim relies on an unjustified nonterminating computation producing evidence;
- certificates are never inspected for runtime content and are discharged by proof irrelevance.

Retain the elaborator idioms learned from the delivered corpus:

- use explicit-subject reflexivity where an implicit meets reducible indices;
- use explicit `@` arguments where constructor patterns meet reducible premise indices;
- use nested single-scrutinee motives rather than tuple scrutinees when refinement matters;
- generalize composed arguments through helper lemmas before convoying on a stuck match;
- prefer congruence plus `Eq/trans` chains to fragile match rewrites;
- write long partially reduced equality chains as pairwise let-ascribed accumulators so implicit inference does not have to propagate through nested `trans` calls;
- bind derived verdict evidence before passing it to a witness extractor, avoiding proof-term duplication through reduction.

The target corpus should remain acceptable to a future termination and positivity checker.

## Staging and ordering

The order is load-bearing:

1. **Confirm PT1.** Packed B views, cursor behavior, equality, hashing, and erasure pass PT1's gates while all conversion machinery remains enabled.
2. **Prototype canonicity and addition.** Implement `Canonical`, trimming, BigNat construction, and `add`; prove the essential preservation and identity results; inspect erased output.
3. **Port BigNat.** Move the required operations and the existing algebra/order corpus to packed views.
4. **Port NonZero and BigInt.** Preserve the signed representation, operations, witnesses, and theorem strength.
5. **Port Dragon4 and current consumers.** Confirm runtime output and representative performance.
6. **Disable performance workarounds without deleting them.** Measure full prelude elaboration, erasure, proof corpus completion, and Dragon4 runtime under pure coinductive conversion, including whether the roughly 2× elaboration/erasure regression that motivated the workaround is recovered.
7. **Audit other recursion.** Confirm no nonnumeric `rec` silently depends on guarded delta or recursive unfolding memoization. Test uniform lazy delta if required.
8. **Replace or retain transient isolation.** Establish the simpler publication invariant and its regressions, or retain the existing mechanism.
9. **Delete only verified-unnecessary machinery.** Run the complete done bar from `AGENTS.md`.
10. **Finish documentation.** Update permanent architecture and standard-library documentation; keep PT2 until PT4 no longer needs it as a dependency reference.

## Performance ceiling

Packed storage and O(1) structural tails remove per-bit heap allocation and reduce closed-conversion churn. Arithmetic remains O(bits) interpreted recursion, not native single-instruction bignum arithmetic. That is intentional: transparent kernel-checked arithmetic is the reason to retain the structural bit view.

The success criterion is not parity with a native bignum library. It is enough performance to restore uniform conversion within its deadline, recover the observed elaboration/erasure regression, and make Dragon4 practical without expanding the trusted arithmetic base.

## Goals

- `BigNat` and `BigInt` stored over packed `Bits` with erased certificates and no per-bit allocation.
- Unique canonical representations preserving structural `Eq` and ordinary rewriting.
- No signed zero.
- Existing algebraic, comparison, cancellation, and ordering theorem strength preserved.
- Dragon4 operational through its stable BigNat surface.
- Conversion returned, if measurements pass, to uniform coinductive bisimulation without recursion-specific identity.
- Unfinished recursive definitions kept unobservable throughout elaboration.
- PT4 able to consume the finished exact integer layers without forcing changes back into PT2's representation.

## Non-goals

- `BigFlt`, exact dyadic arithmetic, correctly rounded float boundaries, or rational denominators; see PT4.
- Native single-instruction bignum arithmetic or operation lowering.
- An annotation electing a library type into a compiler carrier.
- Treating decimal as a Bits fold; decimal rendering remains arithmetic.
- Unifying native `Nat` with `BigNat` or changing Nat's i31 runtime contract.
- Deleting raw-body isolation merely because packed values are faster.
- Termination checking, positivity checking, postulates, or a general arithmetic decision procedure.

## Background facts verified against the codebase

- Conversion lives in `curios-core/src/convert.rs` with a history set, canonicalized goals, recurrence cuts, pending/blocked work, and a deadline surfaced as `ReduceError::Preempted`.
- The conversion subsystem under review spans `6dfde58` (`RecId`, match-guarded delta, `DefEntry.recursive`), `ba9dfa6` (`RecId(0)` collision), and `6387921` (transient leak, raw window, and `contains_transient`). Keep `ced3ba5` and `0858150`.
- Commit `76c870f9` made stuck recursive applications reliable through match-guarded delta, stable memoized openings, and lazy-delta comparison; the delivered BigInt work also exposed that erasure's fresh context had to mark recursive definitions. These are the behaviors PT2 attempts to make unnecessary through representation performance, not regressions to reintroduce accidentally while disabling them.
- The current numbers are `curios-text/std/BigNat.crs` and `BigInt.crs`. Dragon4 is in `curios-text/std/Flt.crs` and calls only the BigNat surface listed above.
- `Prop` is definitionally proof-irrelevant in conversion. Erasure collapses a struct with one runtime-relevant field to the bare field, so a certified BigNat can erase to `Bits` alone.
- Native Nat uses `BigUint` at type level but i31 at runtime; native Int uses `BigInt` at type level but signed i31 at runtime. Workspace dependencies already include `num_bigint`.
- Open primitive applications are stuck. The useful definitional fragment of Nat includes literal successor floors and cancellation of shared floors, while symbolic `-`, `/`, and `%` remain opaque.
- Checked structural eliminators exist for Nat, Lst, Bits, and Bytes. Inductive matches provide checked case analysis and index-driven impossible-arm pruning but no automatic induction hypothesis.
- The stdlib provides `Eq/{sym,trans,cong,subst}` and Nat `Lte`. There is no user-code panic/unwrap trap primitive.

## Open questions and risks

- The canonicity proof burden is the primary go/no-go risk. Do not port the whole corpus before `Canonical`, trimming, and `add` succeed end to end.
- Two-argument packed recursion may expose optimizer or conversion behavior not exercised by single-cursor folds; test carry, borrow, multiplication, and comparison separately.
- Removing `RecId` may reveal nonnumeric recursive comparisons that still need a uniform performance strategy.
- A simpler unfinished-body publication discipline must be demonstrated, not assumed.
- Preserve proof theorem strength while allowing internal helper statements to change around certified wrappers; accidental public canonicity premises would be a regression.
- Verify that no helper retained in PT2 exists solely because the old BigFlt plan requested it. Such work belongs in PT4 unless it is independently part of the BigNat or BigInt API.
