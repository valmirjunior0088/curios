# Representation specification PT1 — primitive `Byte` and packed `Bin`

Working implementation specification for the shared packed-sequence substrate used by the numeric and text layers. This is the first of four ordered specifications: [PT2](REPRESENTATION_SPEC_PT2_NUMERIC.md) builds `BigNat` and `BigInt` over `Bin/B`, [PT3](REPRESENTATION_SPEC_PT3_CHARACTER.md) builds `Char` and the Unicode-scalar view of `Str` over `Bin/X`, and [PT4](REPRESENTATION_SPEC_PT4_BIGFLT.md) adds the postponed `BigFlt` layer. Durable conclusions must be folded into `AGENTS.md`, `ROADMAP.md`, `SYNTAX.md`, and module documentation as the work lands; delete this working series when all four parts are complete.

## Motivation

The existing primitive `Bin` is physically a byte vector but logically exposes each byte as an unrestricted `Nat`. The compiler already knows more than its type admits: literals are parsed into `u8`, storage is `Vec<u8>`, `get` returns a value in `0..255`, and structural elimination can only peel a byte. A distinct primitive `Byte` makes that existing invariant explicit rather than asking every consumer to remember a refinement erased from the type.

The numeric representation work needs an analogous sequence whose logical generator is one bit. Implementing that as a constructor-per-bit inductive defeats the purpose: arbitrary-precision arithmetic and type-level proof reduction allocate and traverse one heap node per bit. The substrate should instead remain physically packed while exposing two distinct logical constructor steps.

The result is one packed implementation with two saturated surface types:

- `/sys/Bin/B`, the free monoid on `Bln`, with one structural step per bit;
- `/sys/Bin/X`, the free monoid on primitive `Byte`, with one structural step per byte.

The two types share storage and implementation but not eliminators. This removes both carrier mismatches: a bit tail is genuinely `Bin/B`, and a byte head is genuinely `Byte` rather than an informally bounded `Nat`.

## Ordering and ownership

PT1 is implemented first and owns every decision about `Byte`, `Grain`, packed literals, packed matching, storage, primitive operations, and downstream representation. PT2 and PT3 depend on this contract and must link here rather than restating it.

PT1 does not introduce `Char`, arbitrary-precision arithmetic, numeric canonicity, or conversion cleanup. It preserves the current conversion machinery while changing the carrier beneath it.

## Design keystones

**One core family, two saturated surface types.** Core represents `Bin/B` and `Bin/X` with a closed Rust-side `Grain::{B, X}`. The object language exposes neither `Grain` nor a width-indexed `Bin` type former.

**`Byte` is primitive because it is a primitive generator.** `Byte` does not add a new runtime representation: it erases to the same i31 scalar used by `Nat`. Its primitive status exists to express the range invariant already guaranteed by `Bin/X`, not to privilege byte arithmetic.

**Grain fixes the logical constructor step.** A B step consumes one bit and returns `Bln`; an X step consumes eight bits and returns `Byte`. A caller never chooses a runtime radix for an otherwise ambiguous fold.

**Storage sharing does not imply type equality.** `Bin/B` and `Bin/X` are distinct primitive types. There is no implicit conversion between them, even where a B value happens to have a bit length divisible by eight.

**Sequence order is structural.** The first written atom is the first atom removed by a match. PT1 assigns no numeric significance to bit order; PT2 privately interprets the first B atom as the least-significant bit.

## Part 1 — primitive `Byte`

Core gains a distinct scalar type and literal:

```rust
Prim::ByteType
Prim::Byte(u8)
```

The minimal primitive conversion surface is:

```rust
Prim::ByteToNat(Term)
Prim::NatToByteWrapping(Term)
```

`Byte/to_nat` is the injective reflection into `Nat`. `Byte/wrapping` takes the low eight bits of a `Nat`; its wrapping behavior is explicit in the name and total at both type level and runtime. Reduction must establish `Byte/to_nat(Byte/wrapping(n)) = n % 256` on closed inputs. The standard facade defines the checked conversion `Byte/of_nat : Nat -> Option(Byte)` by testing the range before calling the wrapping primitive.

Prelude seeding exposes `/sys/Byte` and the minimal operations needed by the facade. `/std/Byte` re-exports the type and supplies ordinary library conveniences and witnesses:

```crs
Byte/to_nat   : Byte -> Nat
Byte/of_nat   : Nat -> Option(Byte)
Byte/wrapping : Nat -> Byte
Byte/eql      : Byte -> Byte -> Bln
Byte/cmp      : Byte -> Byte -> Order
```

Equality and ordering may erase to the existing Nat scalar operations. Do not duplicate a byte arithmetic family in the compiler; arithmetic on reflected values produces `Nat`, while intentional truncation returns through `Byte/wrapping`.

### Numeric literals

The existing elaboration-transient numeric literal mechanism extends its concrete scalar targets with `Byte`. A nonnegative literal checked against `Byte` succeeds only when its magnitude is at most `255`; a larger literal is a compile-time range error. Infer mode and an unsolved expected type continue to default an unsigned literal to `Nat`.

```crs
let a : Byte = 0;
let b : Byte = 0xFF;
let n = 0xFF;          -- Nat
let bad : Byte = 256;  -- rejected
```

There is no new standalone byte-literal token. The element grammar of `x\FF` is already byte-specific, while ordinary numeric syntax is sufficient when a first-class `Byte` is expected.

### Erasure and ABI

`Byte` erases to the existing Nat-shaped scalar in ersd and cont, so no new Wasm GC representation is introduced. Core conversion must still distinguish `Byte` from `Nat` before erasure. A closed `Prim::Byte(u8)` lowers to the existing scalar literal operation.

Do not add `Byte` to `curios-abi::WireType` as part of PT1. Current host operations exchange whole `Bin/X` buffers or general `Nat` codes; an ABI-level byte scalar should be added only when a host operation genuinely requires one.

## Part 2 — surface syntax

Packed literals carry an explicit grain prefix, making empty and spread-only values self-describing without expected-type inference:

```crs
b\                         -- empty Bin/B
b\0\1\0\1
b\..foo
b\0\1\..foo\0\1

x\                         -- empty Bin/X
x\00\AB
x\..foo
x\00\AB\..foo
```

`b\` selects a one-bit element grammar. Each literal atom is `0` or `1`, and each bound head is `Bln`. `x\` selects a one-byte element grammar. Each literal atom is exactly two hexadecimal digits, and each bound head is `Byte`.

The following forms are distinct without contextual typing:

```crs
b\1\0                      -- two bits
x\10                       -- one Byte, 0x10
b\..foo                    -- foo : Bin/B
x\..foo                    -- foo : Bin/X
```

A literal may contain any number of literal atoms and spreads, including zero. Every spread must have the selected grain. Mixed-grain content is rejected. The lexical form remains whitespace-free and glued, following the existing bytestring precedent.

Matches use the same prefix and the existing empty/cons-with-optional-IH shape:

```crs
match bits
| b\ => empty
| b\head\..tail; ih => step(head, tail, ih)
end

match bytes
| x\ => empty
| x\head\..tail; ih => step(head, tail, ih)
end
```

Literal bit heads are accepted and compile as the B cons view followed by a nested `Bln` match:

```crs
match bits
| b\ => empty
| b\0\..tail; ih => zero_bit(tail, ih)
| b\1\..tail; ih => one_bit(tail, ih)
end
```

For `Bin/B`, `head : Bln` and `tail : Bin/B`. For `Bin/X`, `head : Byte` and `tail : Bin/X`. The optional induction hypothesis has the motive instantiated at `tail`, exactly as in the current byte and list matches.

The old `\48\69` and `\\` spellings migrate to `x\48\69` and `x\`. Bare `Bin` migrates to `Bin/X`. No compatibility alias or unprefixed literal remains.

## Part 3 — the closed core carrier

The two surface types are saturated views of a closed core family:

```rust
enum Grain {
    B,
    X,
}

Prim::BinType(Grain)
```

Prelude seeding maps `/sys/Bin/B` to `Prim::BinType(Grain::B)` and `/sys/Bin/X` to `Prim::BinType(Grain::X)`. `/std/Bin` re-exports both items, so ordinary source imports `/std/Bin` and writes `Bin/B` or `Bin/X`. There is no surface `Bin(g)`, `Bin(1)`, `Bin(8)`, `Bits`, or `Bytes` alias.

Generalize the existing free-monoid byte carrier to `Carrier::Bin { grain }` or an equivalent exhaustive representation. There is one family mechanism with two closed `uncons` implementations:

- B view: empty or `(head : Bln, tail : Bin/B)`;
- X view: empty or `(head : Byte, tail : Bin/X)`;
- both cons cases bind `(head, tail, ih)` through `Scope<Three>`;
- refinement reconstructs `Bin/cons(grain, head, tail)`, never a numeric interpretation;
- a symbolic value produces `Layer::Stuck` and rebuilds the match exactly as the existing carrier does.

Closed eliminations compute natively. Open eliminations remain stuck-aware. PT2 numeric operations recurse on the B view; PT3 UTF-8 operations recurse on the X view and reflect a `Byte` to `Nat` only where arithmetic is required.

## Part 4 — packed representation

The runtime representation must support an O(1) logical tail at either grain without materializing shifted copies. A suitable logical model is a shared packed byte buffer with `bit_offset` and `bit_length`:

- B `uncons` reads the bit at `bit_offset`, advances by one bit, and decrements `bit_length` by one;
- X `uncons` reads the aligned byte, advances by eight bits, and decrements `bit_length` by eight;
- every `Bin/X` value remains byte-aligned with a bit length divisible by eight;
- concatenation and slicing preserve the selected grain's alignment invariant;
- unused padding bits are zeroed but remain outside the logical value;
- erasure and code generation never allocate one object per bit or per byte head.

The current `Prim::Bin(Vec<u8>)` remains suitable for owned aligned X literals and as the underlying buffer for B literals. Core reduction also needs a view/cursor form, or an equivalent representation, for sub-byte B tails. Structural equality and hashing operate on the logical sequence: different cursors denoting the same atom sequence compare and hash identically.

No per-value runtime grain tag is required. Types and primitive operation variants determine the grain before or during erasure, and the specialized backend operations carry the corresponding units.

## Part 5 — primitive operations

The Rust implementations of equality, concatenation, length, slicing, append, and get may share grain-parameterized helpers, but object-language declarations are saturated. Result types and units remain explicit.

```text
Bin/B/get    : Bin/B -> Nat -> Bln
Bin/X/get    : Bin/X -> Nat -> Byte
Bin/B/append : Bin/B -> Bln -> Bin/B
Bin/X/append : Bin/X -> Byte -> Bin/X
```

Each grain also receives saturated `len`, `eql`, `slice`, and `concat` operations. `Bin/B/len` counts bits and `Bin/X/len` counts bytes. Never silently interchange those units.

The existing byte-oriented `/sys/Bin/{len,eql,slice,append,concat,get}` implementation becomes the X operation set under the new namespace. B operations receive their own saturated declarations. Host I/O, IEEE float byte reinterpretation, UTF-8, strings, maps keyed by byte sequences, and foreign `Bin` values use `Bin/X`.

The standard facade should retain proof-carrying safe indexing:

```crs
Bin/X/at : (b : Bin/X) -> (i : Nat) -> Nat/Lt(i, Bin/X/len(b)) -> Byte
Bin/B/at : (b : Bin/B) -> (i : Nat) -> Nat/Lt(i, Bin/B/len(b)) -> Bln
```

Checked `get` functions may return `Option(Byte)` and `Option(Bln)`. Folds expose the grain's honest head type.

## Part 6 — compiler wiring

Wiring follows the existing carrier template across every exhaustive match on primitives, `Carrier`, or `Cases::FreeMonoid`: `curios-core/src/{prim,term,scope,free_monoid,reduce,convert,erase,zonk,print}.rs`, `curios-core/src/elaborate/match_.rs`, and their tests. Surface work touches the lexer/parser, text AST, lowering, match compiler, and printer.

The byte-indexed cursor in `curios-ersd/src/optimize/worker_wrapper/cursor.rs` must become grain-aware or gain a B counterpart so optimized structural recursion advances a cursor instead of allocating tails. Ersd evaluators and optimizers, cont scalar evaluation and constant folding, and Wasm emission must preserve B bit lengths and X byte lengths.

The current module documentation promises that a new free-monoid carrier is largely one variant plus one `uncons` arm. That should remain substantially true. The genuinely new representation work is the bit cursor, the closed grain index, and the primitive `Byte` head.

## Migration

Migrate the compiler and prelude in dependency order:

1. Add core `Byte`, its erasure, conversions, and contextual literal checking.
2. Change the existing byte carrier's head, `get`, and `append` contracts from bounded-by-convention `Nat` to `Byte`; use explicit `Byte/to_nat` and checked/wrapping construction at arithmetic boundaries.
3. Add `Grain`, split the saturated Bin types and operations, and preserve X behavior.
4. Add `b\` and `x\` literals and matches, then migrate all old Bin syntax and types.
5. Add the packed B cursor and downstream bit-aware operations.
6. Run PT1's property and integration tests before beginning PT2.

Changing the existing X contract before adding B is an acceptable implementation sequence within PT1, but it must not ship as an enduring intermediate public design.

## Verification

Property-test B and X values against simple reference sequences. Cover:

- empty and nonempty views;
- every sub-byte offset and transition across byte boundaries;
- B padding outside the logical bit length;
- X alignment and divisibility by eight;
- slicing, concatenation, append, get, equality, and hashing;
- distinct cursor views of the same logical value;
- literal and spread order;
- stuck reconstruction and dependent induction hypotheses;
- contextual Byte literal acceptance and range rejection;
- `Byte/to_nat`, checked conversion, and wrapping behavior;
- optimizer cursor advancement without tail allocation;
- host, float-byte, string, and I/O compatibility through `Bin/X`.

Inspect erased and cont output for representative B recursions to confirm that neither `Byte` wrappers nor per-bit heap nodes survive lowering.

## Goals

- One privileged packed implementation behind distinct `Bin/B` and `Bin/X` types.
- Primitive `Byte` as the honest X generator, erased to the existing scalar representation.
- Syntax whose grain is known even for empty and spread-only literals and patterns.
- O(1) B and X tails with no per-bit allocation.
- Explicit bit-length and byte-length units.
- No runtime grain tag and no object-language width index.
- A stable substrate on which PT2 and PT3 can proceed without revisiting carrier types.

## Non-goals

- An object-language `Grain`, `Bin(n)`, `Bin(1)`, `Bin(8)`, arbitrary widths, radices `{2,4}`, a general `Word(n)`, or `Packed(n)`.
- Implicit coercions among `Byte`, `Nat`, `Bln`, `Bin/B`, and `Bin/X`.
- A compiler-primitive character type or any Unicode policy; PT3 owns characters.
- Native arbitrary-precision arithmetic; PT2 keeps arithmetic in library code.
- A `Byte` scalar in the foreign wire ABI without a concrete host use case.
- Compatibility aliases for the old bare `Bin` type or unprefixed bytestring syntax.

## Background facts verified against the codebase

- The current byte carrier is `Prim::Bin(Vec<u8>)`, with `/sys/Bin/{len,eql,slice,append,concat,get}` and a thin `curios-text/std/Bin.crs` facade. `Bin/get` reflects a byte as `Nat`; `Bin/append` currently truncates a `Nat` to a byte in the backend.
- The free-monoid machinery is in `curios-core/src/free_monoid.rs` (`FreeMonoid::{Unary,Bin,Lst}`, `Layer::{Empty,Cons,Stuck}`) and `Cases::FreeMonoid`/`Carrier::{Nat,Bin,Lst}`. Nat binds `(pred, ih)` through `Scope<Two>`; Bin and Lst bind `(head, tail, ih)` through `Scope<Three>`.
- Primitive operations on open terms are stuck. Reduction folds closed operands, while `convert/prim.rs` is congruence-only.
- Numeric literals already survive parsing and lowering as elaboration-transient `NumLit` nodes whose expected type selects `Nat`, `Int`, or `Flt`. Adding `Byte` is an extension of that existing decision point.
- `Byte` values fit the runtime i31 carrier. Type-level `Nat` is unbounded, while runtime `Nat` is i31 and traps outside its range; byte checking must therefore happen during literal elaboration and checked dynamic conversion.
- The worker/wrapper optimizer already has a byte-indexed Bin cursor. PT1 generalizes that mechanism rather than inventing an unrelated bit recursion lowering.

## Open questions and risks

- Choose final public operation names consistently across `/sys/Byte`, `/std/Byte`, `/sys/Bin/{B,X}`, and `/std/Bin`. The semantic contracts above are fixed; naming is still reviewable.
- Confirm whether the compact core value is best represented by a shared buffer cursor directly or by normalized literal/slice/concat forms with an equivalent O(1) view.
- Audit every place that currently assumes Bin length is a byte count, especially optimizer cursor arithmetic and float reinterpretation.
- Ensure logical equality and hashing exclude B padding and normalize equivalent cursors without copying.
- Keep the X migration source-compatible only where the new type remains honest. Insert explicit conversions rather than weakening `Byte` back to a convention on `Nat`.
