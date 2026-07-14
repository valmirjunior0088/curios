# Numeral representation over `Bin`, and retiring the recursion-identity conversion machinery

Implementation specification for representing every arbitrary-precision number (`BigNat`, `BigInt`, `BigFlt`) as a certificate-carrying `Bin` folded through a single radix-parameterized eliminator, and — as the direct downstream consequence — retiring the `RecId`/match-guarded-delta conversion subsystem and reclaiming Curios's uniform coinductive-bisimulation conversion. This is a working implementation reference, not permanent architecture documentation: once the work lands, fold whatever survives into `AGENTS.md`/`ROADMAP.md` and delete this file.

## Motivation

Curios's conversion check is a **coinductive bisimulation**, and that is the load-bearing idea of the language: definitional equality is decided by observing one layer at a time, comparing, recursing on sub-goals, and cutting when a goal recurs (assuming it holds coinductively) — never by reducing both sides to a normal form and comparing structurally, which general recursion makes impossible. This is precisely what lets Curios offer **general recursion as a first-class citizen everywhere, including at the type level**. The invariant is one-sided and sacred: a comparison either succeeds, fails, or times out (`Preempted`) — it must never report a false positive. Well-behaved recursion always finds an answer within the deadline; some ill-behaved recursion does too; the rest times out. This bisimulation always terminated within the deadline.

That stopped being true when `BigNat` was rewritten as a canonical binary numeral built constructor-by-constructor (`induct Pos | one() | o(Pos) | i(Pos)`), for the sake of provability (pure constructor algebra, no proof-opaque `/`/`%`/`-`; see `BIG_FLT_SPEC.md`). Representing a number as one heap-allocated constructor **per bit** is pathologically inefficient — both at runtime (Dragon4 in `curios-text/std/Flt.crs` walks ~150-bit numerals node by node) and, worse, at the type level, where the proof corpus drove deep recursive reductions through conversion. The per-bit representation pushed the bisimulation against the deadline.

The response was a subsystem of performance workarounds layered onto conversion: a `recursive` flag on definitions (`DefEntry`), **match-guarded delta** (a stuck recursive application refolds to its application and is treated as a normal form so common comparisons are cheap spine checks instead of unfold-and-cut), a per-group **`RecId`** identity carrying a memoized unfolding, and a **raw-window / transient-leak** apparatus (`contains_transient`, `withheld`, `refresh_parked_rec_members`) to keep not-yet-elaborated recursive bodies out of comparisons. This machinery landed across roughly thirteen files (`reduce.rs`, `convert.rs`, `elaborate/binding.rs`, `elaborate/module.rs`, `erase.rs`, `context.rs`, `term.rs`, `scope.rs`, `prim.rs`, `zonk.rs`, `print.rs`, and their tests) over the commit range `d08933e..HEAD`, and it was the third attempt at the problem.

## Diagnosis

The `RecId` subsystem is **purely a performance workaround for the bit-by-bit representation**. It does no semantic work the bisimulation does not already do: the coinductive check (with the goal-canonicalization of `0858150`) decides well-behaved recursive conversion on its own, within the deadline, and preserves the no-false-positive invariant with a small, auditable surface. The workaround, by contrast, is non-uniform (recursion becomes a flagged special case rather than ordinary computation) and it *enlarges* the sacred correctness surface — every fold/unfold/refold/park/refresh decision must be exactly right or a raw or mis-folded term slips into a comparison and manufactures a false positive. The `RecId(0)` collision (`ba9dfa6`) and the transient leak (`6387921`) were exactly holes in that enlarged surface.

The corrective is therefore a single upstream move, not a better patch: **fix the representation so number computations are cheap again, and the entire workaround loses its reason to exist and comes back out**, restoring the uniform bisimulation and shrinking the correctness surface to the small one. Crucially, the arithmetic operations may remain ordinary `rec` library definitions — the bisimulation always handled `rec`; the only thing that broke was efficiency. Making reduction fast is sufficient; eliminating `rec` from the arithmetic is *not* required.

## Design keystones

**Privilege the substrate, not the number.** The compiler already treats `Bin` as a native, host-backed, variable-length primitive (`Prim::Bin(Vec<u8>)`) with a symbolic-reducing structural eliminator (the free monoid on bytes, `curios-core/src/free_monoid.rs`). Numbers should be *library code over `Bin`*, not privileged types of their own. Only `Bin` gains a new capability; `BigNat`/`BigInt`/`BigFlt` stay in `curios-text/std/`.

**The bit is the atom, so exactly one new primitive is ever needed.** A number's binary form bottoms out at bits — Boolean, irreducible. A bit-granularity fold is therefore the *terminal* eliminator, not the first of a family: every coarser power-of-two radix (nibble, byte, base-2ᵏ) is a derived library grouping of it, and non-power-of-two radices (decimal) are not folds at all — they are computed by arithmetic (Dragon4 already does this). The bit is also the unique granularity at which arithmetic is *purely structural*: add/multiply/compare are bit-by-bit case analysis with carry logic and no `/`/`%`, whereas any coarser unit needs arithmetic *within* the unit, which is proof-opaque or itself bottoms out at bits. This is why a bit-fold makes bignum arithmetic provable where byte/limb arithmetic could not.

**Representation lives below erasure; proofs live in the library.** The bit-fold is a compiler primitive (representation + reduction); the arithmetic and the ~600 `Eq` lemmas stay `.crs`, kernel-checked. The trusted base grows only by the fold's decode/fold, a small auditable piece of the same kind already trusted for `Nat`/`Bin`/`Lst` — not by an arithmetic oracle.

**Canonicity is a separate certificate, not a property of `Bin`.** A `Bin` is a byte string (`[5]` and `[5,0]` are distinct values), so it cannot be canonical *as a number*; canonicity is added back as a proof-irrelevant certificate, exactly the `/syn/Str` pattern (`record Str { bytes : Bin, valid : Valid(bytes) }`). The transparent bit-fold is what makes maintaining that certificate *provable* — the reason the earlier "trimmed-limbs certificate" was abandoned (opaque limb `/`/`%`/`-`) no longer applies.

**Reclaim the uniform bisimulation.** Once numbers are cheap, delete the `recursive` flag, match-guarded delta, `RecId`, the unfold memo, and the raw-window/transient apparatus, and let conversion be the pure coinductive bisimulation again. If the folded-neutral *speed* is still wanted anywhere, obtain it through **uniform lazy delta** (compare heads first, unfold on demand) applied to *all* definitions — a property of "definition applied to arguments," never of "recursion."

## Part 1 — The radix-parameterized fold over `Bin`

Generalize the existing free-monoid `Bin` eliminator (which peels one **byte**) into a fold parameterized by **radix = bits consumed per step**. The current byte eliminator becomes the radix-8 instance; the new bit eliminator is radix-1. One mechanism, not two parallel ones.

- **Allowed radices**: power-of-two divisors of 8, i.e. `{1, 2, 4, 8}`, so every step aligns to byte storage. Hardcode `{1, 8}` first (the atomic bit and the storage-natural byte); `{2, 4}` are optional later accelerations of groupings that are otherwise derivable in the library. Do not admit non-divisors (e.g. 3): they misalign with byte storage.
- **Head type**: a step at radix `r` peels a `Nat` in `0 .. 2^r` — a bit (`0`/`1`) at `r = 1`, a byte (`0..255`) at `r = 8`, reflected into the eliminator the way the byte carrier already reflects its head as a `Nat`.
- **Eliminator shape**: mirror `Carrier::{Nat, Bin, Lst}` with a `Carrier::Radix { radix, empty_case, cons_case }` (or extend `Carrier::Bin` with a radix field). `cons_case` binds `(head : Nat, tail, ih)`, a genuine dependent recursor supplying the induction hypothesis at the tail — the same shape as `Carrier::Bin`'s `Scope<Three>`.
- **Reduction (`uncons` at granularity `r`)**: on a `Bin` literal, peel the low `r` bits as `head` and advance a **bit-offset cursor** by `r` for the tail (O(1) per step; materialize no shifted copy); on a symbolic `Bin`, produce `Layer::Stuck` and rebuild, exactly as the byte carrier does. Closed folds compute natively; open folds are stuck-aware. This is the property that keeps the bisimulation in-deadline.
- **`refine_head`**: in the `cons_case` arm, refine the scrutinee to `head + 2^r · tail` (the numeral reconstruction), the generalization of `Carrier::Nat`'s refinement to `pred + 1`, so dependent motives reduce.

Wiring follows the existing carrier template across the ~13 files that already pattern-match `Carrier`/`Cases::FreeMonoid` (`reduce.rs`, `convert.rs`, `erase.rs`, `elaborate/match_.rs`, `free_monoid.rs`, `print.rs`, `term.rs`, `zonk.rs`, plus the `ersd`/`cont` optimizers and tests). The module doc's promise holds — "a new carrier is one variant and its `uncons` arm" — with the one genuinely new piece being the bit-offset cursor over `Bin`'s bytes.

## Part 2 — The number tower

All three types are library `.crs` over `Bin` + the radix-1 fold, registered per the three-touch-point rule (`curios-text/std/*.crs`, `std.crs`, `curios-text/src/prelude.rs`).

- **`BigNat`** = `struct { rep : Bin, canonical : Canonical(rep) }`. The `induct Pos`/`induct BigNat` disappear; a number *is* a `Bin`, and its `one`/`o`/`i` bit-structure is a *view* obtained by folding, not a separate inductive. Arithmetic (`add`, `sub`, `mul`, `cmp`, `mul_pow2`, `mul_small`, `of_nat`, `to_str`) is ordinary `rec`/`match` over the bit-fold; single-number recursions may instead use the fold's `ih`. Operations end in a smart constructor that trims and produces the certificate (the `/syn/Str` pattern).
- **`BigInt`** = `induct neg(BigNat) | zero() | pos(BigNat)` (a three-tag, non-recursive layer over `BigNat`) or an equivalent sign+magnitude struct. Inherits speed and provability from `BigNat`; no privilege of its own.
- **`BigFlt`** = `struct { mantissa : BigInt, exponent : BigInt, cert }` (unchanged from `BIG_FLT_SPEC.md`), a struct over `BigInt`; the certificate erases; no privilege of its own.

**Ceiling to state honestly**: this gives native storage and cheap per-step reduction — killing the per-bit heap allocation and the closed-conversion churn — but the operations remain O(bits) *interpreted* recursion, not native single-instruction arithmetic. That is exactly enough: the goal is to make the bisimulation cheap, not to make bignum `+` a machine add (which would require op-lowering and re-open the proof/trust question). Two-argument operations and their mutual carry recursion (`pos_add`/`pos_add_c`) may remain `rec`; they are handled by the bisimulation as they always were, now over a fast representation.

## Part 3 — Canonicity

The only redundancy in a `Bin` numeral is **trailing zero bytes** (`[5]` = `[5, 0]`; within a byte the value is fixed). The canonical form is "top byte nonzero, or empty = zero":

```
Canonical(b) := (b = \\) ∨ (last(b) ≠ 0)
```

- Represent this as a proof-irrelevant certificate field (`Canonical(rep) : Prop`), so structural `Eq` on the certified `BigNat` recovers value equality and `refl` works — avoiding setoid equality, which would forfeit the identity type and its rewriting.
- Every operation's smart constructor trims trailing zero bytes and discharges `Canonical`. With the transparent bit-fold, `Canonical(add a b)` and friends are provable by induction — the specific obstruction that sank the earlier trimmed-limbs certificate (opaque limb `/`/`%`/`-`) is gone.
- **Cost, stated plainly**: unlike the per-bit `Pos` inductive, which was canonical *by construction* (zero canonicity proof), the `Bin` carrier pays a per-operation canonicity proof. That burden is intrinsic to choosing a byte string as the substrate, is bounded, and follows an in-repo precedent (`/syn/Str`). It is the price of privileging the general substrate instead of a number type.

## Part 4 — Reclaiming the uniform bisimulation

With numbers cheap, remove the performance workaround and restore uniform conversion. Keep the parts faithful to the bisimulation; delete the parts that special-case recursion.

**Keep** — the coinductive goal-canonicalization of `0858150` (`history_key`), which is uniform (it operates on goals, not on a `recursive` notion) and simply makes the pre-existing recurrence cut robust to fresh binder openings. Keep α-equivalence of terms (`ced3ba5`), which is independently correct.

**Delete** —
- the `recursive` flag on `DefEntry` and `Context::is_recursive`/`define_rec_members` special path (`6dfde58`);
- match-guarded delta in `reduce_apply` and `unfold_guarded_apply`/`GuardedUnfold` in `reduce.rs`;
- `RecId`, `Rec::id`, `fresh_rec`, `rec_unfolds`, `rec_unfold`/`remember_rec_unfold`, and the memoized `unfold_rec` keyed on it (`6dfde58`, `ba9dfa6`, `6387921`);
- the raw-window/transient apparatus: `Term::contains_transient`, `Subterm::any_transient`, `DefEntry::withheld`, `Context::is_withheld_rec_member`, `refresh_parked_rec_members`, and the conversion backstops/park arms that consult them (`6387921`).

After deletion, a stuck recursive application reduces to its stuck `match` (its natural normal form) and conversion decides it by the coinductive bisimulation, terminating within the deadline as it did before the per-bit representation. If profiling later shows the folded-neutral speed is worth keeping, reintroduce it as **uniform lazy delta** over all definitions (no flag, no per-group identity).

## Staging and ordering

The order is load-bearing; doing the deletion before the representation would time the corpus out.

1. **Representation.** Add the radix fold (`{1, 8}`) on `Bin`; reimplement `BigNat`/`BigInt`/`BigFlt` as certified-`Bin` library code over it; port `Flt.crs`/Dragon4 to the new `BigNat` API (the API surface — `mul_pow2`, `mul_small`, `of_nat`, `add`, `sub`, comparisons — is unchanged, so Dragon4's source should not need to change). Keep the `RecId` machinery in place throughout this step.
2. **Verify.** Confirm the coinductive bisimulation decides the full proof corpus within the deadline over the new representation, with the `RecId` machinery *disabled* (feature-gate or temporary revert) — the empirical check that the representation, not the workaround, is what keeps conversion in-deadline. Measure elaboration/erase wall-clock to confirm the ~2× regression is recovered and Dragon4 runtime is restored.
3. **Retire.** Delete the `RecId` subsystem per Part 4. Re-run the done bar (`AGENTS.md`).

## Goals

- One privileged substrate (`Bin`, gaining a radix-parameterized fold); every arbitrary-precision number is unprivileged library code above it.
- All existing `Eq` lemmas preserved unchanged in kind (structural induction over the bit view), with canonicity via a proof-irrelevant certificate.
- Native `Bin` storage and cheap per-step reduction, eliminating per-bit heap allocation (runtime) and closed-conversion churn (compile time).
- Conversion returned to the pure uniform coinductive bisimulation; the sacred no-false-positive invariant carried by the small auditable surface, not the enlarged one.
- General recursion uniform and first-class everywhere, including the type level — by construction, not by special-case machinery.

## Non-goals

- Native single-instruction bignum arithmetic (op-lowering). Out of scope: it re-opens the proof/trust question the constructor-algebra design exists to avoid. The fold gives cheap *steps*, which is sufficient.
- A new privileged number type, an annotation/pragma to elect a `.crs` inductive into a carrier, or an arithmetic decision procedure in the trusted kernel. The privilege is the `Bin` fold, defined directly in the compiler.
- Radices beyond `{1, 8}` initially; `{2, 4}` only if profiling justifies them (they are otherwise derivable in the library). No non-power-of-two "radix"; decimal is arithmetic, not a fold.
- Unifying `Nat` and `BigNat`, or changing `Nat`'s i31/trapping machine-integer contract. `Nat` remains the bounded machine natural; `BigNat` is the unbounded number. (A future unified `ℕ` is a separate, larger decision; see the conversation history, not this spec.)

## Background facts (verified against the codebase; do not re-derive)

- Conversion is a coinductive bisimulation in `curios-core/src/convert.rs`: `Convert { history : HashSet<Goal>, pending, blocked, ... }`, `in_history`/`history_key`, cutting on recurring goals; the deadline surfaces as `ReduceError::Preempted`. The recurrence cut and `history` set predate the `d08933e..HEAD` range; `0858150` only added `history_key` canonicalization so recurrences are recognized across fresh openings.
- The free-monoid carrier machinery is `curios-core/src/free_monoid.rs` (`FreeMonoid::{Unary, Bin, Lst}`, `Layer::{Empty, Cons, Stuck}`, `uncons`) plus `Cases::FreeMonoid`/`Carrier::{Nat, Bin, Lst}` in `term.rs`, elaborated by `elaborate_{nat,bin,lst}_match` in `elaborate/match_.rs`. `Carrier::Nat` binds `(pred, ih)` via `Scope<Two>` and refines the scrutinee to `pred + 1`; `Bin`/`Lst` bind `(head, tail, ih)` via `Scope<Three>`. The carrier is handled across `reduce.rs`, `convert.rs`, `erase.rs`, `elaborate/match_.rs`, `free_monoid.rs`, `print.rs`, `term.rs`, `zonk.rs`, and the `curios-ersd`/`curios-cont` optimizers.
- `Bin` is a native primitive: `Prim::Bin(Vec<u8>)`, with `/sys/Bin/{len, eql, slice, append, concat, get}`; `Bin.crs` is a thin facade. Prim operations on open terms are stuck (`reduce` fires on literals; `convert/prim.rs` is congruence-only).
- `Nat` is `BigUint` at the type level (`curios-base/src/int.rs` doc: "the type level pretends ℤ, the way `Nat`'s `BigUint` pretends ℕ") but an `i31ref` at runtime that **traps on overflow** past 31 bits (`curios-cont/src/into_wasm/expr_emitter.rs`, `code_emitter.rs`). `num_bigint::{BigUint, BigInt}` are already workspace dependencies.
- The current numbers: `curios-text/std/BigNat.crs` (`induct Pos | one | o | i`, `induct BigNat | zero | pos`; `pos_add`/`pos_add_c` a mutual `rec` group), `BigInt.crs` (`neg(Pos) | zero | pos(Pos)`), and `BigFlt` per `BIG_FLT_SPEC.md`. Dragon4 lives in `curios-text/std/Flt.crs` (`pow2`, `divmod`, `scale_up`, `scale_down`, `generate`, `setup`); it carries no proofs and calls only `BigNat/{mul_pow2, mul_small, of_nat, add, sub, lt, lte, gt, gte}`.
- The certified-record precedent is `/syn/Str` (`curios-text/syn/Str.crs`, `record { bytes : Bin, valid : Valid(bytes) }`). `Prop` is definitionally proof-irrelevant (`convert.rs`), so a certificate field never obstructs equality goals and erases.
- Curios has **no termination or positivity checking** (worked around by discipline; see `BIG_FLT_SPEC.md`). General recursion is therefore not sound in the metatheory (`rec loop : False = loop` typechecks as a term), but manifests only as a conversion timeout, never a canonical false proof. This is orthogonal to representation and unchanged by this work.
- The `RecId` subsystem to be retired is the commit range `6dfde58` (RecId + match-guarded delta + `DefEntry.recursive`), `ba9dfa6` (`RecId(0)` collision), and `6387921` (transient leak / raw window / `contains_transient`). Keep `ced3ba5` (α-equivalence) and `0858150` (coinductive canonicalization).

## Open questions and risks

- **In-deadline termination without the workaround (the go/no-go).** Part 4's deletion is justified only if step 2 confirms the pure bisimulation decides the corpus within the deadline over the new representation. If some comparison still spins, first check whether uniform lazy delta closes it before concluding any recursion-specific machinery is required.
- **Other consumers of the workaround.** Confirm no non-bignum `rec` in `std`/`syn` relies on match-guarded delta or the `RecId` memo to converge. The mechanisms are general, but the stressor was bignum; verify nothing else silently depends on them.
- **Canonicity proof burden.** The per-operation `Canonical` obligations are new work relative to `Pos`'s free canonicity. Prototype `Canonical`, the smart constructor, and one operation (`add`) end-to-end before committing to the whole tower, to size the burden.
- **Bit-offset cursor correctness.** The radix-1 `uncons` over byte storage (peel low bit, advance a sub-byte offset, cross byte boundaries) is the one genuinely new piece of carrier code; it must preserve canonicity semantics and the stuck-rebuild contract. Property-test it against the byte carrier.
- **Erasure/codegen of the certified struct.** Confirm the `Canonical` field erases cleanly and the runtime representation is the bare `Bin`, so no per-number-type backend privilege leaks in.
