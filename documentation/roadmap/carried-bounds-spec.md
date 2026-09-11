# A bound is read off the node that carries it

## Status

Designed and probed. Every claim below was put to the compiler on 2026-09-11 and is reproducible from a heredoc, except the `seam_window` measurement, which names its harness instead. The free-monoid locator — one segment decomposition shared with conversion, which is what would let `get` and `slice` locate a symbolic position — is deliberately outside this specification and is surveyed after it lands.

## Why it exists

`nat_bound` is the reducer's bounds oracle: a monotone homomorphism from `Nat`'s term structure into ℕ, where `bound(x + y)` is `bound x + bound y`, `bound(x · y)` is the product, and `bound(x % k)` is `k - 1`. Two rules turn its answer into a definitional equation — `nat_euclid_split`, which peels a quotient off a sum whose remainder cannot reach the divisor, and `compare.rs`'s two bounded-against-literal guards. It has no other consumer.

**The oracle is a list of arms rather than a closure.** `Nat`'s zero and its additive floor, `ByteToNat`, `NatRem`, `NatAnd`, `NatAdd` and `NatMul` have arms; the floor arm recurses into a symbolic inner, which is how `x % 16 + 3` reaches 18. `NatSub` has none, though truncated subtraction can only shrink a bound and the arm needs the left operand alone. Neither do `NatDiv`, `NatShr`, `NatOr` or `NatXor`, each of which is monotone in operands the oracle already bounds. What a bound is computed for is therefore what somebody needed, which is not a property anything states.

**`Byte` has no constructor, so nothing inverts it.** `/std/Byte/of_nat` is a 256-entry literal table indexed by the value — `get(atoms, n, @ok)` — and an index into a literal run is located by a `usize`, so a literal index folds and a symbolic one is stuck. `Byte/to_nat(Byte/of_nat(n))` therefore has no reduct, and `Nat/to_byte`, the one intrinsic that produces a `Byte` from a `Nat`, masks with `0xff` rather than refusing — a narrowing that changes a value, which [numeric carriers narrow by refusing](../design/toolchain/numeric-carriers-narrow-by-refusing-never-by-changing-a-value.md) forbids, and the only such row on the carrier. `curios-ersd`'s constant folder masks with it, so the erased stage agrees with Core only because both are wrong in the same direction.

**A trip through `Byte` destroys a bound.** `nat_bound`'s `ByteToNat` arm answers 255 whatever its operand is, and with nothing to invert the constructor the term never reduces past it, so a tighter bound established in `Nat` does not survive the round trip. `Byte` is opaque to the algebra rather than transparent to it.

**`Key(Nat)` is unwritable in consequence.** The witness owes injectivity of `Bytes/of_nat`, a base-256 encoding, and the round trip is the only compiler-level fact missing. Euclid's recombination is *not* missing: over a record carrying a quotient, a residual and the equation joining them, it is one substitution, and it elaborates today.

**`seam_window` is superquadratic.** The one locator that accepts a symbolic position walks a concatenation accumulating a prefix sum, rebuilding it as a `Term` and re-reducing it at every operand — so each step re-normalizes a sum of every length before it — and compares whole terms against the target.

## What was probed

Each claim was run against the tree's own binary, one claim per program because a diagnostics snapshot stops at the first failure, and each absence was re-run beside a control that must hold.

**The division family already sees a bound it can read.** `(256·q + m % 256) / 256` is `q` and the remainder twin holds; `(m + 256) / 256` is `m / 256 + 1`. A residual carried in a `Byte` splits the same way — `(256·q + Byte/to_nat(r)) / 256` is `q` for a bare binder `r` — which is the seam `Key(Nat)`'s proof rests on, and it holds for a struct projection too.

**A bound the context holds is invisible.** `m / 256` does not reduce to `0` under `@ok: Nat/Lt(m, 256)`, and a symbolic divisor is wholly stuck. Reduction reads terms, not scopes. The same blindness applies to a match binder: no typing of a function's result restores a bound once the result is bound to a name.

**The missing arms, read through the split.** `(256·q + X) / 256` is `q` for `X` of `n % 256` and `Nat/and(n, 255)`, and declines for `n % 256 - 5`, `(n % 1024) / 4`, `Nat/shr(n % 1024, 2)`, `Nat/or(n % 16, m % 16)` and `Nat/shl(n % 4, 2)`.

**The byte round trip.** `Nat/to_byte(Byte/to_nat(b))` is `b`; `Byte/to_nat(Nat/to_byte(n))` is not `n` under any bound. `Byte/to_nat(Byte/of_nat(5))` is `5`, and the same at a symbolic index is stuck.

**Transparency, stated as a pair.** `(16·q + Nat/and(n, 15)) / 16` is `q`, and the same expression with the operand passed through `Byte/of_nat` and `Byte/to_nat` declines. The arithmetic is identical; the `Byte` is the difference.

**The word operations already discharge their own bounds.** `Byte/of_nat(Nat/and(to_nat a, to_nat b))`, the masked shift `Byte/of_nat(Nat/and(Nat/shl(to_nat a, k), 255))` and the rotation `Byte/of_nat(Nat/and(Nat/or(Nat/shl(to_nat a, j), Nat/shr(to_nat a, 8 - j)), 255))` at `j = k % 8` all elaborate with the precondition filled silently. `Byte/of_nat(255 - to_nat a)` declines, naming the subtraction it cannot bound.

**`Key(Nat)`'s induction elaborates** with the round trip supplied as a hypothesis parameter: a `Nat`-indexed struct, dependent successor induction with that motive, the base case through the literal table, and both arithmetic steps by congruence. The bump branch's bound is discharged by the arm's own refinement and needs no lemma; the carry branch needs two that `/std` lacks.

**The quadratic, measured.** A window on the last seam of a symbolic concatenation, at 16, 32, 64 and 128 operands, against a baseline of the same program without the window. Two independent runs put each doubling between three and five and a half times the one before it, the last doubling above five in both. The harness generates the operand spine and its prefix sum; reproduce it before and after rather than quoting these figures.

## The design

**The oracle closes, and the criterion is a soundness criterion.** An arm exists where the result is bounded by operands the oracle has *already* bounded — bounded in every operand the value is not antitone in, which is the reading that forbids an under-report. Monotonicity alone does not license an arm: a product is monotone in each factor separately and still needs both bounded. The arms this admits are `NatSub`, taking the left bound alone; `NatDiv` at a **literal** divisor, taking the dividend's bound over it; `NatShr` at a **literal** amount, taking the operand's bound shifted; and `NatOr` and `NatXor` with **both** operands bounded, taking `2^max(bits) - 1` over the bit-lengths of the bounds rather than of the operands. Over-reporting withholds the split and under-reporting is a false definitional equation, which is the direction `bound_upper_bounds_every_closed_instantiation` asserts — and that gate samples two shapes rather than iterating the arms, so every arm added owes it a closed-instantiation block or the gate stays empty while passing.

Three candidates are refused. `NatShl` is a **resource hazard**: `nat_bound` takes no reducer and therefore cannot `spend`, while a left shift is the one fold whose result size is not bounded by its operands' — `Nat/shl(1, 400000000)` builds fifty megabytes of magnitude from three lines of surface Curios, and an arm would reproduce that allocation where the charge cannot be made. `BinGet` at grain `X` and `BinLen` are **dead code**: the first has type `Byte` and reaches the oracle as `ByteToNat(…)`, the second folds to a literal exactly when it is measurable and is unbounded otherwise.

**The `ByteToNat` arm stays unconditional, and its justification changes.** It answers 255 for every operand because the *carrier* is `0..=255`, which is a fact about the type and not about how a value was produced. That invariant is what the producers establish — the literal, `NatToByte`, `BinGet` at grain `X`, and `ListGet` over a `List(Byte)` — but the oracle performs no case analysis over them, and must not: the operand under a stuck `ByteToNat` is normally a bare binder or a projection, which is exactly the seam `Key(Nat)` rests on. The arm's doc comment currently justifies the bound by "`Nat/to_byte` wraps", which this specification makes false, and is rewritten to the carrier invariant.

**`NatToByte` carries its domain.** The row gains a proof field, taking the shape the proof-carrying rows in `Intrinsic::signature` already have, and staying where its two analogues are — `IntToNat` at `/sys/Int/to_nat` and `FltToNat` at `/sys/Flt/to_nat` are narrowings filed under the carrier they narrow *from*, and this is the third:

```rust
/// `below` proves `nat < 256`, the domain the narrowing to `Byte` has.
/// Carried for the reason `Intrinsic::NatDiv`'s bound is.
NatToByte { nat: Term, below: Term },
```

`IntToNat`'s own comment states the reason: a bound stated only on `/sys`'s wrapper stops constraining anything the moment that wrapper unfolds, leaving the kernel a bare narrowing to admit. It reaches `curios-cert` because `reduce_intrinsic` is shared with the elaborator rather than restated, and because the signature table is total.

The fold is `ByteToNat(NatToByte { nat, .. }) → nat` — inversion of a constructor, not an equation about arithmetic. `NatToByte(ByteToNat(b)) → b` stays. The pair terminates, since both rules strictly decrease term size, and is confluent: the one critical pair reduces to two terms differing only in a proof at a computed `Holds`, which proof irrelevance equates, and it never arises operationally because the `NatToByte` arm reduces its operand before matching.

`/std/Byte`'s table and `Byte/of_nat` are **deleted** rather than rewritten; the name existed only because the table had to live in `/std`, and its one caller writes `Nat/to_byte`. `curios-ersd`'s constant folder stops masking and declines instead, so the erased stage agrees with Core by construction rather than by both truncating. The emitted mask in the lowering stays, now provably redundant rather than load-bearing.

What this buys past the round trip is the transparency pair: with the constructor invertible, a term reduces past the `Byte` before the oracle is asked, so a bound established in `Nat` survives. `Byte` participates in one algebra instead of wanting one of its own. The limit to state beside it is that the reduct's bound is whatever the oracle can read of `nat`, which for a symbolic operand carrying its bound only in a proof is nothing — the construct-and-immediately-destruct shape trades a coarse bound for a precise term.

**The call sites carry their own bounds, two ways.** The `/std/Char` sites are respelled to the encoding's own field widths: `0xC0 + (codepoint / 0x40) % 0x20` is the two-byte lead's five payload bits, `% 0x10` the three-byte lead's four, `% 0x08` the four-byte lead's three, and `% 0x80` ASCII's seven. Each mask is an identity under the guard that already stands there, each makes the site correct on its face rather than correct-if-you-read-the-guard, and the four-byte arm gains the most because it has no guard at all. `/std/Str`'s case conversion takes the same treatment. `/std/http/Url`'s percent decoder does not: `hex_value` returns an `Option(Nat)` carrying no bound, and a mask there would be a compensation rather than a field width, so it is **re-typed** to carry `Lt(v, 16)` with its digit. That needs bound arithmetic over `Nat/Lt` — `a < 16 → b < 16 → a·16 + b < 256` — which `/std` does not have in any form, and which the `Key(Nat)` carry branch needs two more of.

**`Key(Nat)` follows in `/std`.** A record indexed by the dividend carries a quotient, a `Byte` residual and the equation joining them; successor induction builds one for every `Nat`, the step deciding whether the residual has filled. Rewriting along that equation is Euclid's recombination, and it needs no round trip. The round trip is what builds the record: the bump branch discharges its own bound by arm refinement, and the carry branch needs `le_of_not_lt` and `antisym` over the decided order, which is the same missing family the percent decoder wants.

**`Byte` is the width-8 word, and the operations are a library.** `Grain` is a closed two-variant enum whose `bits` are 1 and 8, so 8 is a width the language already has, and `Byte` is what the byte grain's eliminator yields. `and`, `or`, `xor`, `not`, `shl`, `shr`, `rotl` and `rotr` are `/std` definitions over `to_nat` and `to_byte` whose bounds the closed oracle discharges, each one erased instruction, and none of them a row in the trusted base. The width has to be restated at each one, because the runtime carrier is an `i31` and will not narrow on its own: a masked result everywhere, a guarded count on the left shift — which otherwise builds the full product before the mask and refuses at the envelope — and a count taken modulo eight on the rotations, where the remainder puts the bound in the term and no guard is needed.

**The prefix walk is one pass.** A running prefix over a spine's measures is held as the linear combination `Nat::linear` produces, so extending it costs the operand's own summands rather than every summand before it, and a target is matched by *consumption* — decremented as the prefix grows, matching when it reaches zero, with a coefficient that would go negative reporting an overshoot rather than a wrong answer. Measuring an operand still reaches the reducer; the accumulation and the comparison do not, which is where the cost went. It is a change to an admission surface and not only a cost: the comparison it replaces is whole-term structural equality, and a linear combination keys its summands up to erased universes, so two spellings that decline today would match. That is the admitting direction, it is the same licence the refinement key records for `Nat` summands, and the perimeter entry that states the structural criterion as its evidence is rewritten with it.

## The sequence

Each step is one commit and each is independently revertable.

0. `documentation/roadmap.md`'s pointer, which dangles at a deleted file.
1. The prefix walk and `seam_window` over it, with the perimeter entry rewritten. First because it is orthogonal to everything after it and the likeliest to surprise: if the widening needs more than the entry, that is worth knowing before the rest is built.
2. The refused rows. `curios/src/tests/laws.rs` carries no `Nat`/`Byte` pair at all; it gains one, the transparency pair, and the locator rows the survey will later move. They are spelled over a bounded subject, because a row over a bare `Nat` stops elaborating once the constructor takes a proof.
3. The oracle's closure, with a closed-instantiation block for every arm added.
4. `/std/Byte`'s word operations. They compile today, they are the only user-visible deliverable here, and stopping after them leaves a closed oracle and eight library functions.
5. `/std/Nat/Lt`'s bound arithmetic, and the decided-order bridges the carry branch wants.
6. The call-site respellings and `hex_value`'s re-typing, green on their own, which take the one non-bisectable commit from about thirty files to twenty.
7. `NatToByte`'s proof field, `/std/Byte`'s table and `of_nat` deleted, the erased folder's decline, and `documentation/roadmap.md`'s count of proof-carrying primitives, which this makes thirteen. Atomic: two enums, the signature row, the `/sys` declaration and every prelude call site are checked by one build script, so a partial move fails `cargo check` rather than a test.
8. `Key(Nat)`, and the six copies of the prose that calls the witness unprovable put into the tense of the run they describe.

## Rejected

**`Byte` as a refinement struct** — `{ code: Nat, ok: Holds(code < 256) }` in `/sys`. It closes the round trip by projection and eta, and it is the shape `/std/Char` already uses. It is refused because `nat_bound` takes a term with no context and no registry and returns immediately on anything that is not an intrinsic: for a bare binder the field is a projection out of a symbolic struct, so `(256·q + r.code) / 256` stops *reducing*. The bound would still be provable and the equation would still be true; it would stop being definitional, which is what `Key(Nat)`'s recombination needs. The design breaks the proof it exists to enable, and it would ripple through the `Bin` grain element and the comparison rows besides.

**A `Word(n : Nat)` family.** The language has two widths, they are a closed enum, and their elements are `Bool` and `Byte` — so the family would restate what `Grain` already says while adding an index the untyped reducer cannot read, leaving the width to ride the node regardless. An arbitrary-width word is `Bits`, whose width is its length.

**`ByteToNat(NatToByte(n)) → NatRem(n, 256)`.** It would deliver the transparency pair today with no proof field and no migration, since the remainder folds back through the split. It is refused because it is an arithmetic law admitted to describe a mask, and the mask is the value-changing narrowing this specification removes. The constructor's own inversion is taken instead.

**`Byte` with its own arithmetic.** A decoder accumulates `acc · 256 + digit` where `acc` grows without bound, so the computation lives in `Nat` whatever `Byte` can do.

## Open

Whether `programs/spines` should key on `Nat` once the witness exists. It would remove a confound the benchmark was published with, and it would also end the comparison with the runs already recorded, so it is a change that owes a re-run.

Whether `ByteEql`, `ByteLt` and `ByteLe` should become `/std` definitions over `to_nat`, which would take three rows out of the trusted base at the cost of two conversions and a `Nat` comparison where there is one arm today, on a path the parsers and `Bytes/eql` reach constantly. Gated on measuring it.
