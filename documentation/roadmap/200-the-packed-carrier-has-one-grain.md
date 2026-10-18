# The packed carrier has one grain

Working specification for naming the compiler's packed internals after `Bytes`, the one packed carrier left now that `Bits`, the bit-grained carrier, is gone from the language, `/sys`, `/std`, the host boundary and the run-time representation. It is the second half of a removal: nothing `Bits` did is rebuilt elsewhere. What is left is a `Grain` of one variant, a `Binary` kept at bit granularity, and the internals' names.

## What this builds on

- **One packed carrier, still spelled at a grain.** `curios_num::Grain` has the one variant `X`, an atom a byte (`curios-num/src/binary.rs`), and every packed intrinsic still takes it as data: `BinType(Grain)`, `Bin(Grain, Binary)`, `BinLen`, `BinEql`, `BinGet`, `BinSlice`, `BinAppend`, `BinConcat`, `BinReplicate`, `BinAnd`, `BinOr` and `BinXor` (`curios-core/src/intrinsic.rs`), instantiated once, as `/sys/Bytes` (`bin_ops`, `curios-text/src/sys_module.rs`). The surface writes one packed literal, `x[…]` ([`syntax.md`](../syntax.md), *Packed literals*), whose tree holds no grain.
- **`Binary` is a run of bits.** Its windows are kept at a bit offset and a bit length (`curios-num/src/binary.rs`), so its byte-grain readers convert: `len`, `byte` and `to_bytes` assert an alignment nothing can break any more, and `concat`, `to_packed_bytes`, `and`, `or` and `xor` keep the bit-by-bit walks only an unaligned window reached.
- **The run-time representation is the byte grain's.** A run of at most three bytes rides an `i31` (`ImmediateLayout::of`, `curios-emit/src/immediate.rs`, which still derives its stride and unit from `Grain::bits`), and a longer one is a `$rope/bin` whose length field counts bytes, through the `bytes_*` helpers of `curios-emit/src/table.rs`.

## The gap

**The two-grain machinery names the carrier twice.** What served both grains is spelled `Bin` — the intrinsics, `$rope/bin`, `emit_bin_len`, `LoadAs::Bin`, `Carrier::Packed(Grain)` — and the surface says `Bytes`. With one grain left the split stands for nothing, and `Grain` is a type of one inhabitant that every packed reader still matches on.

## Prior art

- **Erlang keeps both, as a type and its aligned subtype.** "A bitstring is a sequence of zero or more bits, where the number of bits does not need to be divisible by 8. If the number of bits is divisible by 8, the bitstring is also a binary"; the default unit of a binary segment is 8, and building or matching one that is not byte-aligned raises an error ([Bit Syntax](https://www.erlang.org/doc/system/bit_syntax.md)). Erlang's bit syntax serves protocol parsing at the bit level, a consumer Curios does not have.
- **Lean 4 keeps a byte buffer and a fixed-width bit vector, apart.** `ByteArray` and `BitVec` are sibling modules of its core library ([Lean API](https://lean-lang.org/doc/api/navbar.html)), and `BitVec n` is a bit vector of a width its type states, whose proofs `bv_decide` automates ([Lean 4.12.0](https://lean-lang.org/blog/2024-10-3-lean-4120)) — a fixed-width word for reasoning about machine arithmetic, not a variable-length bit string. Curios's bit-level reasoning goes through `Nat`'s bitwise operations, which are already intrinsics.

## Decisions

1. **`Bytes` is the one packed carrier.** No bit-granular carrier remains in the language, `/sys`, `/std`, the host ABI or the run-time representation. A program that needs a bit string later gets one as a `/std` type built over `Bytes` with its length beside it, not as a second carrier.
2. **It is a removal.** Nothing `Bits` does is ported to `Bytes`. A test that named `Bits` was one of three kinds, each treated by its kind: a test of the bit grain itself went with it; a test over every carrier kept its other carriers; and a test of machinery the grains shared whose one fixture was a `Bits` program kept its subject, its fixture rewritten over `Bytes`, or over `List(Bool)` where it needed an element of one bit. No test names `Bits` or `Grain::B` any more.
3. **The surface keeps `Bytes` and `x[…]`.** The internals take the surface's name once one grain remains, the whole family that spells the packed carrier `Bin`: the intrinsics (`BinType`, `Bin`, `BinLen`, `BinEql`, `BinGet`, `BinSlice`, `BinAppend`, `BinConcat`, `BinReplicate`, `BinAnd`, `BinOr`, `BinXor`), `BinSegment` and `Literal::Bin`, `BinPattern` and `MatchPattern::Bin`, `FreeMonoid::Bin`, `Constant::Bin`, `SequenceGrain::Bin`, `BinChunk`, `BinRest` and `BinLevel`, `Repr::Bin`, `Carrier::Bin` and `Carrier::Packed`, `LoadAs::Bin`, `$rope/bin`, and the emitter's `bin_*` helpers — about 1,600 spellings at `1a8c6cedf`, `git grep -o -E "\bbin_[a-z_]+|\bBin[A-Z][A-Za-z]*|Bin\(" -- '*.rs'`, counted — and every other variant that names the packed carrier beside its sibling carriers: `FieldShape::Packed` (`curios-ersd/src/schema.rs`), `IntrinsicForm::Packed` (`curios-core/src/print.rs`) with its reader in `curios-text/src/from_core.rs`, and the law grid's `Value::Packed` (`curios/src/tests/laws/semantics.rs`). `packed` stays where it describes the payload's form — `packed_bound`, `packed_bytes`, a packed run — and `curios-num`'s `Binary` keeps its own name, the payload's representation and not the carrier. `Radix::Bin` (`curios-text/src/nat.rs`) is a numeral's base and is no part of the family.
4. **Bit-level work goes through `Nat`.** `Nat`'s `and`, `or`, `xor`, `shl` and `shr`, and `Byte`'s conversions to and from `Nat`, are what a program has; no bit accessor is added, since nothing asks for one.
5. **`Binary` becomes byte-granular.** Its windows are kept at a bit offset and a bit length (`curios-num/src/binary.rs`), and nothing reads one at a bit: the byte grain's readers take `bit_length` for a charge, a comparison or an offset scaled by `Grain::bits`, and the charge comes out in bytes already — `packed_bound(Grain::X, bits)` is `Cost::packed_bytes(bits / 8)` (`curios-core/src/reduce/intrinsic/cost.rs`) — so a window kept in bytes charges the same units. What only an unaligned window reaches goes with it: the alignment assertions of `len`, `byte` and `to_bytes`, and the bit-by-bit walks `concat`, `to_packed_bytes`, `and`, `or` and `xor` fall back to. `Binary` is archived, so its layout change advances the store's `SCHEMA` and rebuilds the prelude's images; the emitted code does not change, since a `Bytes` rope already counts bytes.
6. **`Grain` is removed outright.** Nothing reads it but `bits`, which is 8, and debug text — the erasure evaluator's spine key (`d{grain:?}`, `curios-ersd/src/optimize/evaluate/spine.rs`), `curios-ersd`'s test support strings and `curios-emit`'s immediate tests. `curios-abi`, `curios-runtime` and `curios-js` never name it.

## The components

| Component | Where | Treatment |
| --- | --- | --- |
| `Grain` and the shapes that carry it | `curios-num`, and every reader of it | Removed |
| The `Bin*` family, `$rope/bin`, `bin_*`, `LoadAs::Bin`, and the variants that name the carrier `Packed` | `curios-core`, `curios-emit`, `curios-algebra`, `curios-ersd`, `curios-text`, `curios`'s law grid, and their readers | Renamed after `Bytes` (decision 3) |
| `Binary`'s windows and their readers' charges | `curios-num/src/binary.rs`, `curios-core/src/reduce/intrinsic.rs` and `cost.rs`, `curios-algebra/src/word.rs`, `curios-ersd`, `curios-emit` | Byte offsets and lengths, every charge the same (decision 5) |
| The store's unit schema | `SCHEMA` in `curios-package/src/store.rs` | Advanced, as the archived Core changes |
| The documents that name a grain or `Bin` | `curios-num`'s README, [A field is declared at the carrier its shape names](../design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md), and every rustdoc that names a grain, `b[…]` or a `Bin` name | Corrected in place |

## The stage

**Rename.** `Grain` removed, the internals named after `Bytes`, and `Binary` byte-granular (decisions 3, 5 and 6). Check: the workspace tests, with the codegen and emitter tests that match `$rope/bin` or `bin_len`, and the test support that prints a grain, following the rename; `cargo xtask instrumented lint`, `cargo xtask docs build` and `cargo xtask bundle test`; the store's `SCHEMA` advanced; the emitted Wasm of every program under `programs/` the same as before the stage apart from the names, counted by comparing the modules with names stripped (`wasm-tools parse`, then `wasm-tools strip --all`); `/std`'s elaboration units the same as before the stage, counted.

## Verification

- **Nothing of the grain is left.** `git grep -n -E "Grain::B|WireLeaf::Bits|WireType::Bits|/std/Bits|/sys/Bits|\bGrain\b|\bBin[A-Z]"` over the tree finds nothing but this specification, and `\bb\[` nothing but Rust's indexing of a variable `b` and the parse test that holds `b[1]` no literal.
- **What `Bytes` does is unchanged.** Every program under `programs/` and every corpus unit compiles and prints what it printed at `a274348aa`, the commit before `/std/Bits` was removed — each program run with `1000` as its argument and on standard input (`trees` with `10`, which grows exponentially in it), and each corpus unit through `curios test`. The erased form of every `/std` item that survives is the one it was there: each item's erased form printed by `curios-ersd`'s printer under the names it declares, dumped by a scratch test over `curios_prelude_archive::with_prelude` in a copy of the tree, built for both commits at one path, since `std.rkyv` holds absolute paths. Counted.
- **The cost goes.** `/std`'s elaboration units (`cargo xtask profile-prelude elaboration`'s `budget::consumed` in all) went from 59,662,520 at `a274348aa` to 59,265,023 at the commit that removed `/std/Bits` and its witnesses, counted. `git grep -c -E "Grain::B|\bBits\b|bits_" -- '*.rs'` went from 648 lines across seventeen crates at `a274348aa` to 28 at the commit that removed the bit grain, every one another name the pattern also matches (`SequenceGrain::Bin`, `inhabits_its`, a word's bits) or `Binary`'s comparison, which the rename restates; and `curios-emit/src`'s and `curios-core/src`'s line counts from 21,503 and 44,114 to 20,411 and 43,801, counted.

## Design decisions this overturns or corrects

- [A field is declared at the carrier its shape names](../design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md): the packed field shape named after `Bytes`.

## Rejected

- **`Bits` kept as a carrier for a consumer that may come.** It costs every reader of a packed value a grain to dispatch on and the host boundary a wire type, for a program no one has written; a bit string a program needs later is a `/std` type over `Bytes` (decision 1).
- **Porting `Bits`'s tests and fixture to `Bytes`.** A test of the bit grain tests what is gone; what a test of shared machinery tested keeps its subject under a fixture of another carrier (decision 2).
- **Renaming `Bytes` to `Bin`.** The internals' name would reach about 1,070 spellings in `/std` and 910 in the programs and the compiler's tests, and either leave the functions named for bytes — `Str/to_bytes`, `Str/of_bytes`, `Flt/to_le_bytes` — describing a `Bin`, or rename them away from the vocabulary their standards use; `Byte` and `Bytes` read as element and container as `Char` and `Str` do.
- **Respelling the literal `b[…]`, or giving it no letter.** `b[` would rewrite every `x[` in the tree and every pattern to mean what `x[` means; a bare `[…]` typed by its expected type gives one bracket two meanings and leaves a packed literal unable to elaborate without one.
- **`Grain` kept as a one-variant extension point.** Nothing reads a second grain, and a type with one inhabitant states nothing a reader of it can use (decision 6).
- **A bit accessor on `Nat` or `Byte`.** Nothing asks for one (decision 4).

## Completion and retirement

Done when `Bytes` is the only packed carrier and the internals are named after it. Decisions 1 and 5 go to [`curios-num`'s README](../../curios-num/README.md), which states what a packed run is and how `Binary` holds one, and decision 3's literal to [`syntax.md`](../syntax.md); the documents listed above are corrected as the stage lands. Verify that nothing references this filename, and delete it.
