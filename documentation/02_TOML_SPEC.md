# Native-width TOML codec

Working implementation specification for `curios-prelude/std/Toml.crs`: a useful TOML 1.1-oriented decoder and encoder over Curios's existing native `Int` and binary32 `Flt` values. This is the next planned standard-library effort, before dyadic `BigFlt`.

The first implementation explicitly does not claim full TOML conformance. It accepts and emits TOML syntax while rejecting integers outside native `Int`, rounding finite floats to binary32, and inheriting the current `Flt` decimal parser's limits. It does not depend on `BigInt` or `BigFlt`.

## Objective

Provide a deterministic, UTF-8-safe `std/Toml` codec suitable for ordinary configuration files and capable of representing every TOML value category. Keep all numeric limitations explicit in the API, tests, and roadmap so the later general numeric work may improve the implementation without changing the `Toml` module's ownership.

## Module and naming

The PascalCase file and module own a same-named nominal value type, following `/std/Json`:

```crs
pub mod Toml;
pub use Toml/{let Toml};
```

`curios-prelude/std/Toml.crs` owns its data model, parser, semantic table builder, and encoder. Constructors are lowercase; functions and fields are snake_case; conversions use `of_*` and `to_*` only when they are genuine type conversions.

Nothing in TOML syntax is compiler-emitted, so `/syn` and `curios-prelude/src/syntax.rs` remain unchanged.

## Data model

Use native `Int` and `Flt` directly:

```crs
pub struct Date : pub Type {
    year : Nat,
    month : Nat,
    day : Nat,
}

pub struct Time : pub Type {
    hour : Nat,
    minute : Nat,
    second : Nat,
    nanosecond : Nat,
}

pub struct OffsetDateTime : pub Type {
    date : Date,
    time : Time,
    offset_minutes : Int,
}

pub struct LocalDateTime : pub Type {
    date : Date,
    time : Time,
}

pub induct Toml : pub Type
| str(Str)
| int(Int)
| flt(Flt)
| bln(Bln)
| offset_date_time(OffsetDateTime)
| local_date_time(LocalDateTime)
| local_date(Date)
| local_time(Time)
| arr(Lst(Toml))
| table(Map(Toml))
end

pub use Toml/*;
```

`offset_minutes` is validated to the TOML/RFC 3339 offset range. `nanosecond` stores at most nine fractional digits; additional valid digits are truncated rather than rounded. Calendar constructors remain public data, while parsing and encoding validate month, day, leap-year, clock, and offset bounds.

A decoded document always yields `Toml/table`. Arrays may contain mixed value types. Tables use `Map(Toml)` with the existing `Key(Str)` witness; source order is not semantic.

## Public codec

Follow the established JSON and parser-combinator vocabulary:

```text
decode : Parse(Toml)
encode : Toml -> Result(Str, Str)
```

`decode` parses exactly one complete UTF-8 TOML document, consumes trailing permitted whitespace and comments, and rejects trailing non-comment input. It returns a root table.

`encode` accepts only a root `Toml/table`; other top-level values return `Result/failure`. It emits one deterministic equivalent TOML document rather than preserving source spelling, comments, whitespace, key quoting, table-header choices, or numeric lexemes.

## Lexical coverage

The decoder covers:

- LF and CRLF newlines;
- comments outside strings;
- bare, basic quoted, literal quoted, and dotted keys;
- basic, multiline basic, literal, and multiline literal strings;
- TOML escapes and Unicode scalar validation through `Char` and `Str`;
- decimal, hexadecimal, octal, and binary integers with valid underscore placement;
- decimal floats, exponents, signed zero, `inf`, and `nan` with valid underscore placement;
- lowercase booleans;
- offset date-time, local date-time, local date, and local time;
- arrays with comments, newlines, mixed types, and trailing commas;
- inline tables;
- standard table headers and arrays of tables.

Reject forbidden control characters, malformed escapes, invalid UTF-8, invalid leading zeroes, malformed numeric underscores, invalid calendar or clock components, and incomplete tokens.

## Native integer policy

`Toml/int` stores `/std/Int`, not `BigInt`. The current runtime represents `Int` in a signed i31 carrier even though downstream primitive operations use i32 instructions. The codec therefore accepts only values that materialize safely in the actual native carrier and rejects every otherwise-valid TOML integer outside that range.

Numeric scanning checks range before each multiply and add; it must never use overflow traps as ordinary parse control flow. Non-decimal bases observe the same signed target range. This limitation is deliberate and covered by boundary tests.

A future native-i32 representation change may widen the accepted range without changing `Toml/int`. A future arbitrary-precision TOML profile would be a separate design decision rather than silently changing the stored type.

## Binary32 float policy

`Toml/flt` stores `/std/Flt`, preserving binary32 bit identity for signed zero, infinities, and the chosen NaN encodings.

The initial finite decimal path:

1. validates the complete TOML float grammar and underscore placement;
2. removes separators into a certified `Str`;
3. calls `Flt/of_str` only after lexical validation;
4. rejects a valid lexeme if the native conversion helper cannot produce a result.

Special values and signed zero use explicit four-byte patterns through `Flt/of_le_bytes`, so spelling does not depend on native arithmetic. Encoding inspects `Flt/to_le_bytes`, emits lowercase `inf` and `nan`, preserves the sign of zero, and uses a valid TOML decimal spelling for finite values.

Binary32 precision is intentionally smaller than TOML's recommended binary64 support. The first codec does not promise correctly rounded conversion for every decimal lexeme beyond what `Flt/of_str` establishes, and it does not preserve the original decimal value or spelling after binary32 rounding.

## Statement parsing and table construction

Parse syntax into a private statement list before building the final root table. Statements distinguish:

- key/value assignments;
- standard table headers;
- array-of-table headers.

The semantic fold tracks the active table path and enough private state to distinguish implicit tables, explicitly declared tables, inline tables, and arrays of tables. It must reject:

- duplicate keys;
- a table defined more than once;
- extension of a sealed inline table;
- table/value and table/array conflicts;
- appending array-of-table entries to a statically defined array;
- invalid parent selection through an array of tables;
- dotted-key redefinitions and every other path conflict required by the targeted grammar.

Persistent nested updates belong to private TOML helpers. Do not weaken `/std/Map` or expose TOML-specific mutation through its public API merely to simplify construction.

## Encoding policy

The encoder favors one simple deterministic form:

- root scalar and array entries are emitted as key/value lines;
- nested tables use standard table headers where unambiguous;
- tables nested inside arrays may use inline tables;
- keys use bare spelling only when every character is valid for a bare key, otherwise basic quoted spelling;
- strings use basic quoted or multiline basic spelling with the required escapes;
- map traversal order is deterministic but not source-preserving;
- every emitted document must decode to the same `Toml` value, modulo the implementation-defined NaN encoding.

If a value cannot be expressed under these rules, `encode` returns a descriptive failure rather than emitting invalid TOML.

## Explicit conformance limits

The first implementation does not claim TOML 1.1 conformance because:

- integer range is limited to native `Int`, currently signed i31 at runtime;
- finite floats are binary32 rather than the recommended binary64 precision;
- decimal-to-binary32 conversion inherits the current `Flt/of_str` correctness envelope;
- NaN payload and sign preservation are implementation-defined;
- the project is not complete until every accepted and rejected grammar family is compared against the language-agnostic TOML test corpus.

These are documented product limits, not permission to accept malformed TOML or ignore table-definition semantics.

## Verification

- Add focused Curios integration tests for every value type and string form.
- Pin native integer minimum, maximum, and one-step-outside rejection in every supported radix.
- Pin positive and negative zero, normals, subnormals, overflow, underflow, infinities, and NaNs by comparing `Flt/to_le_bytes`.
- Test duplicate keys, dotted paths, implicit and explicit tables, inline-table sealing, and nested arrays of tables.
- Test UTF-8, Unicode escapes, invalid scalars, comments, CRLF, trailing input, and malformed numeric separators.
- Run the TOML language-agnostic valid and invalid corpus through an adapter, classifying only documented numeric-range or precision differences as expected limitations.
- Verify `decode(encode(value))` for generated representable values.
- Run the repository done bar.

## Non-goals

- `BigInt`, `BigFlt`, binary64, arbitrary-precision decimal, or exact rational storage.
- Preserving comments, whitespace, source order, table spelling, numeric lexemes, or string quote style.
- A lossless editable TOML syntax tree.
- Schema validation, deserialization into arbitrary user types, or a derive mechanism.
- Changing native `Int`, native `Flt`, `/std/Map`, `/std/Parse`, or compiler syntax to accommodate the codec.

## Completion criteria

- `std/Toml` is registered and exposes the documented native-width data model and codec.
- The parser consumes complete documents and enforces table construction conflicts rather than merely recognizing tokens.
- The encoder emits deterministic valid documents that round-trip every representable generated value.
- Numeric values outside the native policy fail explicitly without overflow traps or silent wrapping.
- Every known deviation from the targeted TOML version is documented and represented in tests.
- Before this specification is deleted, the public data model, codec contract, native numeric policy, and conformance limits are recorded in the owning `/std/Toml` documentation and tests; remaining plans refer to the landed module rather than this file; the roadmap entry is a checked unlinked summary; and no reference to this filename remains.
