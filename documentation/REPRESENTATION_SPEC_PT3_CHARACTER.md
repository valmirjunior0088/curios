# Representation specification PT3 — certified `Char` and Unicode-scalar `Str`

Working implementation specification for introducing a real `Char` type in Curios and changing `Str`'s logical element from an unrefined `Nat` code point to a certified Unicode scalar value. This is the third specification in the representation series: [packed Bits, Bytes, and Byte syntax](SYNTAX.md#literals) supplies primitive `Byte` and `Bytes`; [PT2](REPRESENTATION_SPEC_PT2_NUMERIC.md) completes the packed integer layers and conversion work; [PT4](REPRESENTATION_SPEC_PT4_BIGFLT.md) subsequently handles `BigFlt` and its exclusive obligations.

This file owns character and string semantics only. Durable conclusions must move into `SYNTAX.md`, `ROADMAP.md`, `AGENTS.md`, and standard-library/module documentation when the implementation lands; delete this working series after PT4.

## Motivation

Curios currently has a `/std/Char` module but no `Char` type. Its functions accept and return `Nat`, character literals lower to `Nat`, and `Str/{fold,get,at,find}` expose decoded UTF-8 code points as `Nat`. The UTF-8 certificate proves those decoded values are Unicode scalars, but the public types discard that fact.

The same `Nat` type consequently denotes unrelated domains: sequence indices, counts, arbitrary machine naturals, bytes, and Unicode scalar values. PT1 removes the byte ambiguity with primitive `Byte`. PT3 removes the character ambiguity while keeping Unicode validation and encoding transparent library code.

`Char` is a Unicode scalar value: a code point in `0..0x10FFFF` excluding the surrogate range `0xD800..0xDFFF`. It is not a grapheme cluster, glyph, UTF-8 code unit, or locale-dependent notion of a user-perceived character.

## Ordering and dependencies

PT3 begins after PT1 and PT2 are stable. The hard dependency is PT1's final X contract:

```text
Bytes = empty | Byte × Bytes
```

PT2 is ordered before PT3 to keep the numeric/conversion experiment isolated from Unicode proof migration. PT3 must not modify conversion machinery or the packed carrier contract.

PT4 may use `Str` for presentation but must consume the finished PT3 API rather than forcing character representation decisions back into the numeric layers.

## Design keystones

**`Char` is certified Curios code, not a compiler scalar primitive.** Unicode scalar validity is a transparent predicate over `Nat`. Curios can prove and erase it without adding Unicode semantics to core reduction, conversion, ersd, cont, or Wasm.

**Syntax-owned representation lives under `/syn`.** The compiler emits character literal construction, so the nominal type and minimal proof vocabulary live in `/syn/Char`, following the existing `/syn/Str` precedent. `/std/Char` re-exports the type and supplies its public operations and witnesses.

**The wrapper is zero-cost.** A `Char` contains one runtime-relevant `Nat` plus a `Prop` certificate. Proof erasure and single-field struct collapse make its runtime representation the same i31 scalar currently used for decoded code points.

**`Str` remains UTF-8-backed.** A string is still certified `Bytes`, not `Lst(Char)`. Logical folds decode `Char`; storage, equality, slicing infrastructure, literals, and I/O remain compact UTF-8 bytes.

**Unicode scalar semantics are stable; Unicode databases are library policy.** Scalar validity belongs to the foundational type. Character categories, case mappings, normalization, grapheme segmentation, and their Unicode-version policy remain standard-library data and algorithms.

## Part 1 — `/syn/Char`

Define scalar validity transparently over a code point:

```text
Scalar(c) := c <= 0x10FFFF and not (0xD800 <= c and c <= 0xDFFF)
```

The concrete Curios formulation may use boolean reflection into `True`/`False`, an indexed positive `Prop`, or a small conjunction vocabulary. It must satisfy:

- closed scalar literals reduce to an immediately checkable proposition;
- the UTF-8 proof can derive `Scalar(decoded)` structurally;
- proof irrelevance discharges equality between certificates;
- the certificate erases completely.

The type is representation-private:

```crs
pub struct Char : Type {
    code : Nat,
    scalar : Scalar(code)
}
```

The exact field name may be `code` or `codepoint`, chosen once and used consistently. External code does not construct or project the struct directly; `/std/Char` is the public facade.

`Char` values fit native Nat's runtime carrier because the maximum scalar `0x10FFFF` is well below the i31 limit.

### Identity

Two `Char` values with the same code point are propositionally equal. Their proof fields are definitionally irrelevant, so no setoid equality is needed. Provide the usual injectivity/reflection theorem between `Char` equality and equality of `Char/to_nat` where useful.

## Part 2 — character literals

Character literal syntax becomes genuinely character-typed:

```crs
'a'    : Char
'λ'    : Char
'\n'   : Char
```

It no longer means a monomorphic Nat code point. Numeric code points use numeric syntax explicitly:

```crs
0x61 : Nat
```

The surface parser already retains a Rust `char` before lowering, and Rust `char` is a Unicode scalar. Move character literals out of `NatLiteral::Char` into a syntax-owned literal form such as `Syn::Char(char)`. The lowerer's meta-emitter constructs the private `/syn/Char` value and a closed scalar-validity derivation, just as the current string meta-emitter constructs `/syn/Str` and its UTF-8 derivation.

Compiler-generated references belong under `/syn`, in accordance with the existing rule that `/syn` contains names emitted by lowering. `/std/Char` source must use numeric code point literals internally where using `'…'` would create an initialization cycle.

Future Unicode escape syntax, if added, must reject surrogates and values above `0x10FFFF` during parsing. It is not required for the initial migration.

## Part 3 — public `Char` API

`/std/Char` re-exports `/syn/Char` and replaces the current Nat-namespace functions with typed operations:

```crs
Char/to_nat : Char -> Nat
Char/of_nat : Nat -> Option(Char)
Char/to_utf8 : Char -> Bytes

Char/eql : Char -> Char -> Bln
Char/cmp : Char -> Char -> Order

Char/is_whitespace : Char -> Bln
Char/is_digit : Char -> Bln
Char/is_lower : Char -> Bln
Char/is_upper : Char -> Bln
Char/is_alpha : Char -> Bln
Char/is_alphanumeric : Char -> Bln
```

`Char/of_nat` performs the scalar check and returns the certificate on success. `Char/to_utf8` is total because invalid scalar values cannot inhabit `Char`; this corrects the current API, whose `Nat -> Bytes` signature can be called with a surrogate or an out-of-range value.

ASCII-oriented helpers become typed:

```crs
Char/hex_digit : Nat -> Char
Char/of_hex_digit : Char -> Option(Nat)
```

Case conversion requires an explicit policy:

- If the initial implementation remains ASCII-only, name it `to_ascii_lower`/`to_ascii_upper` or document the restricted contract precisely.
- Simple Unicode scalar mappings may have type `Char -> Char`.
- Full Unicode case folding and some full case mappings can expand one scalar into multiple scalars, so their honest result is `Str` or `Lst(Char)`, not `Char`.

Do not encode a Unicode database or version into compiler primitives.

### Byte and ASCII boundaries

`Byte` and `Char` remain distinct. Provide explicit conversions where useful:

```crs
Char/of_ascii : Byte -> Option(Char)
Char/to_ascii : Char -> Option(Byte)
```

`Char/of_ascii` succeeds only below `0x80`. UTF-8 encoding uses `Char/to_utf8`, not a general Char-to-Byte coercion. Byte-oriented parsers may reflect with `Byte/to_nat` or convert ASCII explicitly.

## Part 4 — UTF-8 certificate migration

The existing `/syn/Str` state machine already enforces valid Unicode scalar sequences:

- ASCII lead bytes are `0x00..0x7F`;
- multibyte leads begin at `0xC2`, excluding overlong two-byte encodings;
- `0xE0` restricts the next byte to exclude overlong three-byte encodings;
- `0xED` restricts the next byte to exclude UTF-16 surrogates;
- `0xF0` restricts the next byte to exclude overlong four-byte encodings;
- `0xF4` caps values at `U+10FFFF`;
- leads above `0xF4`, stray continuations, and incomplete sequences are rejected.

PT1 changes each structural Bytes head from Nat to `Byte`. The validation state machine should reflect a byte to Nat only at the arithmetic/classification boundary. Its public proof indices continue to describe `Bytes`.

Add the theorem that a successfully decoded character is scalar-valid. The existing `classify`, continuation bounds, `Utf8`, `peel_byte`, `cont_len`, and `take_conts` structure contains the necessary evidence; the work is to preserve and expose it in the decoded result rather than discarding it.

The preferred decoder boundary returns a certified value directly:

```text
decode_head : valid UTF-8 head and tail evidence -> Char
```

Do not add a trusted primitive UTF-8 decoder. Closed literals may still reduce through ordinary Curios definitions and the compiler-generated proof spine.

## Part 5 — `Str` API

`Str` remains conceptually:

```crs
record Str : Type {
    bytes : Bytes,
    valid : Valid(bytes)
}
```

Its runtime representation remains only the packed byte sequence. `Str/to_bytes` and `Str/of_bytes` become `Bytes` boundaries.

Change logical character operations to use `Char`:

```crs
Str/fold : (@A : Type, s : Str, init : A, f : (Char, A) -> A) -> A
Str/get : Str -> Nat -> Option(Char)
Str/at : (s : Str) -> (i : Nat) -> Nat/Lt(i, Str/len(s)) -> Char
Str/find : Str -> Char -> Option(Nat)
Str/find_index : Str -> (Char -> Bln) -> Option(Nat)
```

`Str/len` continues to count Unicode scalar values, not bytes or grapheme clusters. `Str/slice` continues to index scalar boundaries and preserve its UTF-8 certificate.

Concatenation remains byte concatenation plus proof composition. Equality remains byte equality because valid UTF-8 has a unique byte encoding for each scalar sequence. No decoding is needed for case-sensitive equality.

Case-insensitive comparison must not lowercase raw bytes. The current `lower_bytes` applies character casing to each byte and is only meaningful for ASCII. Replace it with an explicitly ASCII contract or decode characters and apply the selected Unicode case-folding policy before comparison.

### String literals

String literals remain compiler-meta-emitted UTF-8 `Str` values. Their runtime cost remains one packed `Bytes` literal; the proof spine erases. PT1 changes emitted byte heads in the derivation to `Byte`, and PT3 changes decoded logical values to `Char`, but the source string syntax and UTF-8 storage do not change.

## Part 6 — standard-library migration

Audit every current code point and byte use. Important consumers include:

- `Nat/of_str`, `Int/of_str`, and `Flt/of_str`;
- JSON string parsing, escaping, hexadecimal digits, and surrogate-pair handling;
- parser combinators whose predicates currently accept Nat bytes or code points;
- HTTP parsing and ASCII classification;
- I/O line handling, which is byte-oriented until a `Str` is validated;
- string search, whitespace splitting, casing, and tests.

Use the domain-specific type rather than inserting indiscriminate conversions:

- raw I/O, HTTP framing, and encoded JSON bytes use `Byte`;
- decoded JSON characters and `Str` folds use `Char`;
- indices, lengths, numeric values, and Unicode code point arithmetic use `Nat`;
- encoding and decoding boundaries perform explicit conversions.

JSON surrogate-pair decoding computes a Nat scalar and then calls `Char/of_nat`; invalid or unpaired surrogates remain parse errors. JSON output calls `Char/to_utf8` for decoded scalar values and uses `Byte` for escape bytes.

## Part 7 — syntax, concepts, and presentation

Add `Eql(Char)`, `Cmp(Char)`, `Ord(Char)`, and `Show(Char)` witnesses as appropriate, following the standard operator-facade placement rules. `Show(Char)` should produce the one-character UTF-8 `Str`, not the source-literal spelling with quotes.

Infix comparison between a `Char` and a `Nat` or `Byte` is rejected. Users write `Char/to_nat(c)` or an explicit ASCII conversion when crossing domains.

Printers must preserve character literal spelling and escaping. Diagnostics should print Char literals recognizably where the core term still carries syntax-level presentation; certified library values reduced after elaboration may print through their nominal type or ordinary term form.

Update `SYNTAX.md` to state that `'…'` has type `Char`, numeric literals remain polymorphic numeric scalars, `x\HH` atoms are bytes within `Bytes`, and strings are UTF-8 `Str` values indexed by Unicode scalar rather than grapheme.

## Verification

Test at least:

- literal typing for ASCII, BMP, supplementary-plane, escapes, and quote/backslash characters;
- rejection of invalid future Unicode escapes;
- `Char/of_nat` at `0`, surrogate boundaries, `0x10FFFF`, and `0x110000`;
- `Char/to_nat` round-trip through `of_nat`;
- UTF-8 encoding for one-, two-, three-, and four-byte scalars;
- UTF-8 rejection of overlong sequences, surrogates, out-of-range values, stray continuations, and truncation;
- `Str/fold`, `get`, `at`, `find`, `len`, and `slice` over mixed-width strings;
- proof erasure and newtype collapse for Char and Str;
- no regression in string literal runtime representation;
- explicit Byte/Char/Nat boundary use across JSON, parsers, HTTP, and I/O;
- chosen casing semantics, including expansion cases if full Unicode folding is implemented.

Property-test `Char/of_nat` and `Char/to_utf8` against Rust's Unicode scalar and UTF-8 behavior for the full scalar range where practical. The reference verifies behavior; Curios's certificate remains the source-language proof.

## Staging

1. Define `/syn/Char` scalar validity, certified representation, and constant proofs.
2. Add `/std/Char` conversions and basic classification/equality operations.
3. Change `'…'` lowering from Nat to certified Char and update parser/printer tests.
4. Migrate `/syn/Str` to PT1 `Byte` heads and prove decoded scalar validity.
5. Change `Str` logical APIs from Nat to Char.
6. Migrate standard-library consumers by domain.
7. Resolve casing names and semantics explicitly.
8. Run the complete done bar and update permanent documentation.

## Goals

- A real `Char` type whose inhabitants are exactly Unicode scalar values.
- `'…' : Char` with compiler-generated, kernel-checked constant evidence.
- Zero runtime overhead relative to the current Nat code point.
- `Str` logically folds and indexes `Char` while retaining packed UTF-8 storage.
- Explicit separation of bytes, Unicode scalars, and naturals.
- Total `Char/to_utf8` and checked `Char/of_nat`.
- Unicode-version-sensitive behavior confined to standard-library policy.

## Non-goals

- A compiler-primitive `Char` type or Unicode decoder.
- Grapheme clusters, glyph shaping, display width, normalization, collation, or locale-sensitive text processing.
- Treating a Unicode scalar as a user-perceived character.
- Changing `Str` storage to `Lst(Char)` or UTF-32.
- Implicit coercions between `Byte`, `Char`, and `Nat`.
- Requiring full Unicode case data in the first implementation; any ASCII-only surface must simply be named honestly.

## Background facts verified against the codebase

- `/std/Char` currently exists only as a namespace of Nat functions. `Char/to_utf8` currently accepts any Nat, including invalid scalar values.
- `/syn/Str` stores `Bytes` plus `Utf8` evidence. `/std/Str` decodes code points as Nat through `fold`, `get`, and `at`.
- The current validator's lead and continuation bounds already exclude overlong encodings, surrogate values, and values above `U+10FFFF`.
- Character literals are parsed separately but lower to `Prim::Nat`; the text AST retains `NatLiteral::Char` long enough to change this cleanly.
- String literals already use a `/syn/Str` meta-emitter that constructs a private certified value and one `Utf8/more` proof node per byte. This is the precedent for syntax-emitted `/syn/Char` construction.
- Erasure drops `Prop` fields and collapses a struct with one relevant field to that bare field. Both Char and Str therefore have zero-cost certified representations.
- The existing `Str/eql_ci` lowercases raw bytes. That behavior is ASCII-specific and cannot serve as general Unicode case-insensitive equality.

## Open questions and risks

- Choose a transparent `Scalar` formulation that makes both literal proofs and UTF-8-derived proofs tractable without bloating reduction.
- Decide whether initial case helpers are explicitly ASCII-only or backed by a selected Unicode version; do not retain ambiguous names with narrower behavior.
- Full case folding may expand and requires string-level APIs, not merely Char-to-Char functions.
- Migrating `'…'` from Nat is intentionally breaking and will expose places that were using character spelling as an ASCII numeric convenience. Resolve each according to whether it actually wants Byte, Char, or Nat.
- Keep the compiler's knowledge limited to literal construction. If ordinary Char computation starts requiring new primitive cases, revisit the library proof design before expanding the trusted base.
