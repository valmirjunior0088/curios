# `Byte` is a `Nat` below 256

## Status

Investigated and probed, not designed. Every claim below was put to the compiler on 2026-09-10 and is reproducible from a heredoc; what is missing is the sequencing, the decision on `Intrinsic::Byte`, and a prototype of the one invasive piece.

## Why it exists

`Byte` is the only intrinsic carrier whose meaning Core does not state. `Nat` and `Int` are unbounded with a sum normal form and stated arithmetic; `Bits`, `Bytes` and `List` are free monoids with a stated peel; `Flt` is specified by a model that computes exactly over `Natural`. `Byte` is an opaque 256-element set with seven operations — `Nat/to_byte`, `Byte/to_nat`, and five comparisons that land in `Bool` — and **no eliminator at all**. The eliminable carriers are exactly `Nat`, `Bin` and `List` (`curios-core/src/term/shape.rs`), so an arbitrary `Byte` cannot be taken apart by any means: `match b | 0x0 => … end` is refused with "expected Nat but got Byte".

Its only characterization is a bound *asserted* in `nat_bound` (`curios-core/src/reduce/intrinsic/nat.rs`), whose own doc justifies it in prose — "A `Byte` is `0..=255` by its carrier — `Nat/to_byte` wraps and `Byte` is not a wire type". `nat_euclid_split` then rests on that bound, which is what makes `(256·x + Byte/to_nat(b)) / 256` reduce to `x`. So the trusted base already depends on a truncation it has no way to express, and no program can name the fact it depends on.

The asymmetry in one sentence: a `Bin` at grain `B` has element `Bool`, which has an eliminator; at grain `X` it has `Byte`, which has none.

**This is not primarily about unblocking a proof.** The `Key(Nat)` witness is the only consumer that becomes writable, and framing the work around it would be out of all proportion. The reason to do it is that `Byte` joins the carriers whose semantics Core owns, and seven intrinsic rows leave the trusted base in the process.

## What was probed

Each of these was run against the tree's own binary, and each absence-only result was re-run beside a control that must fail.

**A proof-carrying refinement erases to nothing.** `f(s: Str) -> Nat = Bytes/len(s.bytes)` erases to `apply ~f65$/sys/Bytes/len(~v$s)` — the projection is gone, and `Str` never appears as a `product` row in the erased output at all. `/std/Str` is `{bytes: Bytes, valid: Valid(bytes)}`, so the pattern is load-bearing in the standard library today.

**`Byte` already rides the `Nat` carrier.** `curios-ersd/src/into_cont.rs` states the encoding table: "`Unit`, `Bool`, and `Byte` ride the `Nat` carrier … `NatToByte` a mask, `ByteToNat` the identity". `CpsSlot` has no `Byte` variant. So `{code: Nat, ok: …}` erases to exactly the representation `Byte` already has, and no stage below Ersd changes at all.

**Struct eta holds.** `Eq(Char { code = c.code, scalar = c.scalar }, c)` closes by `Eq/refl()`, while `Eq(c, d)` over two distinct binders fails. Rebuilding a record from its own projections is definitionally itself.

**The language already models this exact thing.** `/std/Char` is the proposed shape: `{code: Nat, scalar: Scalar(code)}` with `to_nat(c) = c.code`. Curios already represents "a `Nat` with a bound" as a proof-carrying refinement, for the neighbouring concept. `Byte` is the inconsistent one.

## The design

Declare `Byte` in `/sys`, not `/std`. `/sys` names nothing above it, so `/sys/Bytes/get` cannot return a `/std` struct; but `/sys` carries its own precondition propositions and `Holds(b: Bool) -> Prop`, described in `sys_module.rs` as "the reflection of a decision into a claim, which is what every decided bound is made of". A `/sys` declaration of the shape `struct Byte { code: Nat, ok: Holds(Nat/lt(code, 256)) }` is therefore expressible, and it keeps `Byte` as the `Bin` eliminator's head type — so the `/std` surface does not move.

What follows from that:

- `Byte/to_nat` becomes a projection and `Nat/to_byte` a constructor taking the bound. Both `Nat`/`Byte` round-trip laws become **definitional**: one is projection of a struct literal, the other is eta. Neither needs a reduction rule.
- `ByteType`, `ByteToNat`, `NatToByte`, `ByteEql`, `ByteLt` and `ByteLe` leave `Intrinsic`. The comparisons become `Nat` comparisons on `.code`.
- `/std/Byte.crs`'s 256-entry literal table disappears. It exists only because there is no constructor, and it is what makes `Byte/of_nat` opaque at a symbolic index.
- `nat_bound`'s `ByteToNat` arm is replaced by one over `BinGet` at grain `X`, so that `Bytes/get` can discharge the `ok` field it now has to fill.

## What it costs, stated plainly

**The axiom relocates; it does not vanish.** Something must still assert that a byte peeled out of a packed run is below 256. What changes is its character: an invisible arm of an oracle becomes evidence a program can hold, pass and match on. That is a real improvement in kind and it should not be sold as removing an assumption.

Measured blast radius: 69 sites in `curios-core`, 35 in `curios-elab`, 32 in `curios-text`, 29 in `curios-ersd`, 7 in `curios-analysis`, 3 in `curios-cert`, and **0** in `curios-cont` and `curios-wasm`, which already see a `Nat`. About 25 `/std` modules name `Byte` as a type, heaviest `Json/decode.crs`, `Parse.crs`, `Str.crs` and `http/Url.crs`. `Byte` does not cross the host ABI, so `curios-abi`, the runtime and the JavaScript implementation are untouched.

**The one invasive piece, and the thing to prototype first:** the free-monoid peel must construct a struct. `Head::into_atom` (`curios-core/src/free_monoid.rs`) returns `Intrinsic::Byte(byte)` for a literal head and would have to return the refinement, which needs a `SyntaxRegistry` that module deliberately does not take — its documentation states that neither the measure nor the walks take a `Reducer`, "and that is enforced by the signatures rather than asserted in a comment". Threading a registry through `peel_front`/`uncons` weakens exactly the property those signatures were shaped to hold. `Intrinsic::signature` already takes a registry, so the precedent for Core reaching a declared name exists; the peel is where it gets uncomfortable.

## Rejected

**Adding a reducer law instead** — `ByteToNat(NatToByte(n)) → NatRem(n, 256)`, beside the existing `NatToByte(ByteToNat(b)) → b`. It is true by the same `& 0xff` that already defines `NatToByte`, it is three lines, and it closes the one proof that wants it: with `Byte/of_nat` respelled over `Nat/to_byte`, the chain runs `Byte/to_nat(Byte/of_nat(n % 256))` → `NatRem(n % 256, 256)` → `n % 256`, the last step already working today because `nat_euclid_split` collapses a remainder whose summands are bounded below the divisor. Rejected because it *grows* the trusted base to paper over a carrier that is under-specified, which is the shape of fix that relocates a problem rather than closing it. Recorded here because it is the cheap route if this spec is never taken, and because it is genuinely correct.

**Giving `Byte` its own arithmetic.** It does not help the consumer: a decoder computes `acc * 256 + digit` where `acc` grows without bound, so no `Byte` can hold the accumulator and the computation lives in `Nat` whatever `Byte` can do. It would be fresh trusted-base surface with no consumer.

**A bit-structured eliminator** — `Byte` as eight `Bool`s. A real alternative, but a round trip then needs eight bit-level facts instead of one, and `Bits` does not carry its length in its type, so "exactly eight" is not expressible. That is the `Word(n)` design, already rejected on its own terms.

**Making the grain element `Nat` outright**, so Core never mentions `Byte`. Simpler in Core, but every `x[h, ..t]` binder across `/std` would bind a `Nat`, and `Byte` would stop being what a `Bytes` eliminator yields — worse than the defect being fixed.

## The cheap fallback, if this is not taken

`curios/src/tests/laws.rs` has **no rows at all** for the `Nat`/`Byte` pair, held or refused. `Byte` appears there only as an element type in the `Bytes` free-monoid rows. That grid exists so that "the refused set is a record rather than a rumor", and so that taking a law later "is a row moving, not a test appearing". Adding the pair's rows costs nothing, closes the gap in the record, and stops the next person rediscovering it while trying to prove an encoding injective.

## Open

- Whether `Intrinsic::Byte(u8)` must survive for packed-literal folding, or whether `PackedBin` alone suffices.
- The cost of threading a registry into `free_monoid`, which decides whether the campaign is worth starting.
