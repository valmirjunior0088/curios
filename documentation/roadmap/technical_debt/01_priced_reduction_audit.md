# The reducer allocation audit

The inventory [A reduction step costs what it builds](01_priced_reduction_spec.md)'s M0 calls for: every place a reduction can allocate, classified as *construction* or *sharing*, with the price-list row that will charge it. M1 ticks the boxes; this file is deleted with the specification when the last one is ticked.

## How to read a row

**Construction** is storage that did not exist before the operation and that the operation's result — or the operation itself, transiently — now occupies. **Sharing** is storage that already existed and was reached again: a reference count bumped, a slice borrowed, a window taken over a buffer somebody else owns. The specification's rule is that construction is charged and sharing is not, so the classification *is* the work; the charge follows from it.

Two things a row does not decide. It does not decide whether a site is a *defect* — most of these are ordinary and simply need a price. And it does not decide the constant: the price list states the formula, and the fixed headers are named constants justified beside themselves.

A site is **temporary** when its storage does not survive the operation. Temporary storage is charged anyway, per the specification: a charge covers an operation's peak, not its residue, and a reduction that allocates a gigabyte and frees it has still allocated a gigabyte.

## `curios-utilities` — the payload carrier

`PackedBin` is where both `Bin` grains keep their bytes, so it is where most byte payload is built. It is not named in the specification's crate list, but every packed-payload charge lands on one of these methods, and three of the four sites the specification names by hand are here.

| Site | Allocates | Class | Priced as |
| --- | --- | --- | --- |
| `PackedBin::from_bytes` | one `Arc<[u8]>` copy of the whole payload | construction | packed bytes |
| `PackedBin::from_bits` | a `Vec<bool>` at **one byte per bit**, then a `Vec<u8>`, then the `Arc<[u8]>` copy | construction | packed bits, ×3 buffers |
| `PackedBin::window` | `Arc` clone and three integers | **sharing** | value header only |
| `PackedBin::slice` | `window` | **sharing** | value header only |
| `PackedBin::as_bytes` | borrow | **sharing** | nothing |
| `PackedBin::to_packed_bytes` | aligned: one `to_vec`; unaligned: a zeroed `Vec<u8>` filled per bit | construction, temporary | temporary buffer |
| `PackedBin::to_bytes` | one `to_vec` | construction, temporary | temporary buffer |
| `PackedBin::concat` | a `Vec` of operand refs, a `Vec<u8>` of the whole result, then the `Arc<[u8]>` copy of it | construction | packed bytes **×2** |
| `PackedBin::append_bit` | the whole value rebuilt through `from_bits` | construction | packed bits, ×3 buffers |
| `PackedBin::append_byte` | `to_bytes` copy, then `from_bytes` copy | construction | packed bytes **×2** |
| `PackedBin::hash` | unaligned arm allocates `to_packed_bytes` | construction, temporary | **to be removed** — the specification requires streaming |
| `PackedBin::eq` | nothing | sharing | — |

- [ ] `from_bytes` charged from the operand length
- [ ] `from_bits` charged at three buffers, the first at eight units per logical bit
- [ ] `window`/`slice`/`as_bytes` shown to charge a value header and no payload
- [ ] `to_packed_bytes` / `to_bytes` precharged as temporaries
- [ ] `concat` charged twice, per the price list's last paragraph
- [ ] `append_bit` / `append_byte` charged for the whole rebuilt value
- [ ] `hash` streams the unaligned contents rather than materializing them

**`from_bits` is worse than the price list's bit row suggests and the row is still right.** The logical charge is `ceil(bit_length / 64)` units for the value; what this method physically takes is that plus a `Vec<bool>` eight times the payload. The price list prices the *value*, and the specification's temporary-buffer row is what covers the rest — so this site charges a value plus two temporaries rather than one value.

## `curios-num` — the numeric carriers

| Site | Allocates | Class | Priced as |
| --- | --- | --- | --- |
| `Natural`/`Integer` `Add`, `Sub`, `Mul`, `BitAnd`, `BitOr`, `BitXor` | a result bounded by the operands' limb counts | construction | bigint limbs |
| `Natural::checked_div` / `checked_rem`, `Integer`'s | a result no wider than the dividend | construction | bigint limbs |
| `Natural::checked_shl`, `Integer::checked_shl` | a result of `bits(operand) + amount` — **the operand does not bound it** | construction | bigint limbs, precharged from operand *and* shift amount |
| `Natural::checked_shr`, `Integer::checked_shr` | a result no wider than the operand | construction | bigint limbs |
| `Natural::pow` | a result of `bits(operand) · exponent` | construction | bigint limbs |
| `Natural::to_bytes_le` | a `Vec<u8>` of the magnitude | construction, temporary | temporary buffer |
| `Natural::parse_bytes` | the parsed magnitude | construction | **out of scope** — the lexer's, not reduction's |

- [ ] every closed arithmetic fold precharged from its operands' limb counts
- [ ] `checked_shl` refuses a shift amount whose result would exceed the affordable bound, *before* `num-bigint` is asked
- [ ] `pow` precharged from `bits(operand) · exponent`, with the multiplication checked
- [ ] `to_bytes_le` precharged as a temporary

**The shift is the sharpest site anywhere in this audit, and it is the one the specification singles out.** `checked_shl` converts the shift amount with `to_usize()` and hands it to `BigUint`, so the result is `bits(operand) + amount` and the amount is a number the program writes. The result size is exactly computable from the operand and the amount without performing the shift, which is what makes charge-first straightforward here rather than conservative.

It is reachable from a surface program in three lines, and it was measured rather than reasoned about:

```crs
use /std/{Handle, Nat, Bool};
let big : Nat = Nat/shl(1, 400000000);
let check = match Nat/lte(1, big) | true => () | false => () end;
/std/print("ok")
```

```sh
/usr/bin/time -l target/release/curios compile shift.crs -o out.wasm
```

Taken **2026-08-15**, release, `aarch64-apple-darwin`. Peak process memory against a 74.8 MiB baseline: **76.0 MiB** at a shift of 8 000 000, **80.5 MiB** at 80 000 000, **232.9 MiB** at 400 000 000. The compile succeeds every time and takes well under a second, because the step counter charges the same handful of transitions whichever it is running — which is the defect in one line. Four hundred million bits is fifty megabytes of magnitude; nothing in the language stops the numeral being larger.

This is the acceptance suite's "single oversized construction" candidate, and it is a better one than the accumulator the specification retired: it has no loop to make linear, so no representation change can flatten it.

## `curios-core` — the free monoid and the spine

| Site | Allocates | Class | Priced as |
| --- | --- | --- | --- |
| `free_monoid::bin_segments` / `list_segments` | a `Vec<(&Term, usize)>` of one entry per spine segment, plus a `pending` worklist | construction, temporary | temporary collection, sized by spine depth |
| `free_monoid::normalize_concat` | a `kept: Vec<Term>` of the surviving operands, and a `Vec<&C>` of their runs | construction, temporary | collection slots |
| `normalize_concat`'s `merge` | the fused literal — `PackedBin::concat` or a flattened element vector | construction | delegated to the carrier's row |
| `spine::bin_atoms` | flattens **both** operands into merged literal runs, one entry per generator, on every conversion between two sequence values | construction, temporary | **named in the specification** |
| `Nat::decompose` | a floor clone and a term clone | **sharing** | nothing new |

- [ ] `bin_segments`/`list_segments` precharged from the spine's operand count
- [ ] `normalize_concat` charges its kept vector, and its fusing closure becomes fallible so `merge` can charge at the allocation point
- [ ] `bin_atoms` precharged, or narrowed so a comparison that decides on the first generator does not flatten both subjects

**`normalize_concat` is the representative shape the specification warns will recur.** Its fusing closure returns a `Subterm` infallibly, so charging where the allocation happens makes the closure and the function fallible across both the binary and list callers. It still fuses and still copies when it does: `FUSION_CAP` bounds *how much* it copies at once, which changes how often this site is reached rather than whether it must charge.

## `curios-core` — the shared intrinsic folds

| Site | Allocates | Class | Priced as |
| --- | --- | --- | --- |
| `bin_shape` | the whole run materialized into a `Vec<u8>` — one element per **bit** at `Grain::B`, one per byte at `Grain::X` | construction, temporary | **named in the specification** |
| `list_shape` | moves the element vector out of an owned node, clones it out of a shared one | construction where shared | collection slots |
| `reduce_homomorphism` | one rebuilt term per operand, collected | construction | term nodes plus collection slots |
| `BinConcat` fold | a `Vec<Term>` of reduced operands, then `normalize_concat`'s payload | construction | collection slots plus the carrier's row |
| `ListConcat` fold | a `Vec<Term>` of reduced operands, then `runs.into_iter().flatten().cloned().collect()` — **every element cloned** into the fused literal | construction | collection slots |
| `List` fold | one reduced element vector | construction | collection slots |
| `ListSlice` | `slice.to_vec()` on the literal path; a `Vec<Term>` of window pieces on the segment path | construction | collection slots |
| `ListAppend` | pushes onto the element vector, reallocating on growth | construction | collection slots |
| `ListMap` | one `Term::apply` node per element | construction | term nodes |
| `BinSlice` / `BinGet` segment path | a `Vec<Term>` of window pieces | construction | collection slots |
| `nat_euclid_split` | a `quotient` and a `residual` vector, one entry per summand | construction, temporary | temporary collection |
| `reduce_foreign` | a `Vec<Term>` of reduced arguments | construction | collection slots |

- [ ] every fold above charges before it allocates, through the new `Reducer` operation
- [ ] `bin_shape` precharged, or made to answer a measurable question without materializing the run
- [ ] `ListConcat`'s flatten charged per cloned element

**`bin_shape` is the site whose traffic changed and whose cost did not.** `Bin/len` no longer reaches it for a wholly-literal value, which now answers from the free monoid's measure — but every symbolic shape still falls through to the homomorphism and still pays a full materialization to compute a result that is one `Nat`.

## `curios-core` — the representation

Reference counting is what keeps this section short: a new node is charged for its own variant and its own slots, and its children are shared rather than reconstructed, which is complete construction pricing.

| Site | Allocates | Class | Priced as |
| --- | --- | --- | --- |
| `Subterm` → `Term` | one refcounted node and its cached scalars | construction | term node |
| `Term::unwrap_or_clone` | nothing when the node is uniquely held; one node's fields when it is shared | construction where shared | term node |
| `Bound::release` (substitution) | rebuilds every node on a path it touches; pruned where `reach <= depth` | construction | term nodes |
| `Bound::capture` | the same, in the closing direction | construction | term nodes |
| `Scope::open` | one `release` | construction | term nodes |
| `Telescope::open` | **clones the whole telescope** — one box per level — then `release`s once per binder | construction | term nodes plus collection slots |
| `Telescope::open_params` / `walk` | the same shape, per binder | construction | term nodes |
| `Term::instantiate_universes` | a rebuilt term | construction | term nodes |
| `Bound::free_vars` | a `BTreeSet<Free>` over the whole term | construction, temporary | temporary collection |
| `Scope::map_body` / `try_map_body` | one scope node, and whatever `f` builds | construction | term node |

- [ ] term construction charged at the node, by variant and retained slot count
- [ ] `release`/`capture` charged for the nodes they rebuild, from a bound computed before the walk
- [ ] `Telescope::open` charged for its clone as well as its substitutions
- [ ] `free_vars` precharged or shown not to be reducer-reachable

**`Telescope::open` is quadratic in the binder count and nothing says so.** It clones the boxed chain and then substitutes once per binder, so a beta step against an `n`-ary lambda costs `n` boxes and `n` substitution passes over the body. It is called at every beta step in both checkers.

## `curios-elab` — the elaborator's strategy

| Site | Allocates | Class | Priced as |
| --- | --- | --- | --- |
| `unfold_rec` | a member vector, a ref vector, and the opened tail | construction | collection slots plus term nodes |
| eta and arm probes | a fresh-binder vector, an occurrence vector, and a ref vector per probe | construction | collection slots plus term nodes |
| `Apply` rebuild | a reduced parameter vector | construction | collection slots |
| `normalize_each` | `spine.to_vec()` and a collected result | construction | collection slots |
| reduction-cache insertion | the key term and the retained result | **retention** | retention quota |

- [ ] each construction site above charged
- [ ] cache insertion charged against the retention quota alone, never against work

## `curios-cert` — the kernel's strategy

Written separately from the elaborator's on purpose, so the same shapes appear twice and both have to be charged. The rows below are the ones that differ, not the ones that repeat.

| Site | Allocates | Class | Priced as |
| --- | --- | --- | --- |
| `step_let` | a `values` vector, **a fresh ref vector per binding**, and one `release` per binding | construction, partly temporary | collection slots plus term nodes |
| eta probe | a binder vector, an occurrence vector, and a ref vector | construction | collection slots plus term nodes |
| `step_apply` | a parameter ref vector, then `Telescope::open` | construction | delegated |
| `whnf`'s `recurse` bracket | a native stack segment when a level crosses the guard | construction | **recursion level** |
| `whnf`/`forced`/`unfold` memo insertion | the key term and the `Replay` | **retention** | retention quota |

- [ ] `step_let` charged, including its per-binding ref vectors
- [ ] the `recurse` bracket charges a frame, from a constant justified beside the deepest measured guarded frame
- [ ] memo insertion charged against the retention quota alone

**`step_let` substitutes where the elaborator binds.** The kernel copies each value into every use, on the stated ground that a substitution is visibly the rule and an environment is a second place a variable's meaning could come from. That is a judgment worth keeping and a cost worth pricing: the two checkers will legitimately spend different amounts here, which is why the specification compares verdicts rather than sums.

## What this audit found that the specification did not name

Four sites the specification names by hand were its seed. These are what reading the rest turned up, and each is a place where the result's size is not bounded by the operands' sizes — the class the counter is least able to see.

**`Natural::checked_shl` converts a shift amount and hands it to the allocator.** Named above; it is the one site where a single well-typed term can ask for an arbitrary allocation with no loop at all.

**`Natural::pow` is the same shape with a multiplication instead of an addition.**

**`PackedBin::from_bits` allocates eight times the logical payload before it packs.** Every bit-grain construction path runs through it, `append_bit` included.

**`Telescope::open` clones its chain per beta step.** Not unbounded, but per-step and unpriced, and it is on the hottest path either checker has.

**`step_let` allocates a ref vector per binding.** Quadratic in a `let` run's length, which the surface language makes as long as a program likes.

## Baselines

Priced work and retention do not exist yet, so what M0 records is what a reduction costs *before* pricing, taken from probes that already exist rather than from new ones.

| What | Probe | Command |
| --- | --- | --- |
| Type-level accumulation, three arms per carrier, with peak process memory | `curios`' `type_level_sequence_cost_measurements` | `cargo test --release --package curios -- --ignored --nocapture type_level_sequence_cost_measurements` |
| Elaborator and kernel budget floors for the same program | `curios`' `kernel_memo_charge_measurements` | `cargo test --release --package curios -- --ignored --nocapture kernel_memo_charge_measurements` |
| Whole-unit certification, and restore and re-erasure beside it | `curios-prelude-archive`'s `stored_prelude_measurements` | `cargo test --release --package curios-prelude-archive -- --ignored --nocapture stored_prelude_measurements` |
| Prelude elaboration and erasure: retained, allocated, allocation count, reported peak | the prelude build's own capture | `touch curios-prelude-archive/std.crs && cargo build --package curios-prelude-archive --features profile`, then `target/debug/build/curios-prelude-archive-*/out/profile.tsv` |

Each probe carries its own last-printed figures, per the rule that a figure lives beside the thing that would check it. What this table adds is that the four together are M0's baseline, so M1 and M3 have one list to re-take rather than four to rediscover.
