# Text read as text, and numbers by each format's grammar

Working specification for the input `/std` reads as bytes, or by another format's grammar, where it began as text: the numbers `Json` reads by `Flt`'s grammar and writes as `null`, the text formats and `Fmt` that walk the bytes of input that began as a `Str`, and `Flt`'s readers cutting text where a grammar belongs. The rule is [`curios-prelude-archive`'s README](../../../curios-text/README.md#std-invents-no-value-where-a-proof-belongs-and-text-is-addressed-by-position)'s: where a branch cannot be reached its unreachability is proved, where it can the caller sees an `Option`, a `Result` or a refusal, and a default stays only where it is the specified answer, documented as such.

It is independent of every other spec. Stage 4 rewrites a parser the type-level benchmark set measures — `Flt/of_str`, a row of `type_level_claim_measurements` (`curios/src/tests/reduction.rs`) — and retakes that row.

## What this builds on

- **Positions.** `Str/At(s)` is a byte offset into `s` with an erased proof that a character begins there, decided by the one byte at it. `At/next` and `At/prev` step a character, `Str/before`, `Str/after` and `Str/between` cut in O(1), and `Str/At/forward` and `backward` recurse over positions by well-founded recursion.
- **One parser type, indexed by its carrier.** `Parse(Bytes, A)` reads bytes and `Parse(Str, A)` reads text, whose positions are boundaries of valid text. The combinators are written once; each carrier's primitives are a module of their own, `Parse/bytes` and `Parse/text`, one spelling per operation (`lib.crs:177`). The text primitives — `char`, `char_if`, `expect`, `literal`, `take_while`, `peek` and `rest` — have no consumer in `/std` yet.
- **Text grammars compute in a type.** A `Parse(Str, A)` grammar — an optional sign, a run of digits, the end — run on `"-1234567"` in an `Eq/refl` claim reduces in about 3 s, and the claim with a wrong count is refused with the value the checker computed (the probes below).
- **One conversion per number type.** `Flt/of_decimal(negative, d, e, digits)` (`Flt.crs:547`, with a rounding-mode twin at `Flt/rounded.crs:141`) converts a decimal significand and its exponent, correctly rounded, and `Toml/numbers` reads TOML's own grammar and converts through it (`Toml/numbers.crs:245`). `Flt/rounded/of_dyadic` (`Flt/rounded.crs:95`) is the one way from an exact binary value to a float, and answers every exponent itself: `3 · 2^-100000000` rounds to `+0.0`, `3 · 2^100000000` to `+inf.0`, and each directed rounding to its neighbour, all four in about 4 s in a type. A `Dyadic`'s zero has no sign.
- **One escaping walk.** `Str/escape` copies the runs between the characters a table rewrites, and `Json/encode`, `Toml/encode` and `http/Url/encode` write through it.

## The gap

**`Json` reads numbers by `Flt`'s grammar, and writes one JSON cannot hold as `null`.** `Json/decode.crs:25` hands a run of number bytes to `Flt/of_str`, so `01` and `1.` decode, though RFC 8259 §6 refuses both, and `1e400` decodes as infinity, which no JSON spells. It reads the bytes through `Option/unwrap_or(Str/of_bytes(chunk), "")`, a default no run of ASCII number bytes reaches. `Json/num` holds any `Flt` (`Json.crs:10`), which is what leaves `encode` a `null` to invent (`Json/encode.crs:4`): `Json/num(+inf.0)` reads back as `Json/null()`.

**Text parsers drop the proof on entry.** `Toml/decode`, `http/Url/of_str` and `Html/of_str` receive a `Str`, parse its bytes, and build text back from byte slices, either with a runtime check whose refusal cannot fire on input that began as a `Str` (`Toml/strings.crs:238`, `Toml/keys.crs:10`, `http/Url.crs:39`) or with `""` (`Html/parse.crs:18`); `Html/parse.crs:33` reads a literal's first byte with a default of `0`. `Json/decode` is a bytes parser its callers hand bytes — `Str/to_bytes` of a string, or a reply's body in `Response/json` — and checks the UTF-8 of each string body it builds (`Json/decode.crs:97`). An escape decodes to bytes and the whole string is scanned again at its end. `Fmt/parse` (`Fmt.crs:81`) walks a format string's bytes under a fuel of three times their length and answers `Fmt/nil()`, the end of the format, where the fuel runs out, where a slice falls outside the bytes, and where a slice is not UTF-8 (`Fmt.crs:24`) — none of which a `Str` reaches. It runs in every type that mentions `Fmt/render(s)`: a 42-character format renders in a type in about 3 s. `Html/parse`'s `until_literal` and `http/Url/decode`'s walk count down a fuel too, where a position or the bytes themselves would descend. And the edge that should check a body's text once has no position to report: `Str/of_bytes` answers `none` alone.

**`Flt` reads its own syntax by cutting text.** `Flt/of_str` and `Flt/of_hex_str` compose `Str`'s cuts — `strip_prefix` for the sign and the `0x`, `split_once` for the point, the exponent and a NaN's payload, `eql_ascii_ci` for the specials — where a grammar would state the syntax and a parser would carry the positions. `marked` cuts at `e` before `E` and leans on a later refusal when both occur. `of_hex_body` (`Flt.crs:745`) clamps its exponent ahead of `exact/round`, which answers every exponent itself.

## Prior art

- **JSON.** RFC 8259 §6 lets a decoder limit the range it accepts and names `1E400` as a number that may not interoperate; I-JSON asks senders to stay within binary64 ([RFC 7493 §2.2](https://www.rfc-editor.org/rfc/rfc7493#section-2.2)). Go refuses both a number that overflows its target and a non-finite float to encode ([`encoding/json`](https://pkg.go.dev/encoding/json)); serde_json refuses an overflow to infinity as "number out of range" ([`de.rs`](https://github.com/serde-rs/json/blob/master/src/de.rs)) and answers `None` from `Number::from_f64` for a non-finite one, since "Infinite or NaN values are not JSON numbers" ([`number.rs`](https://github.com/serde-rs/json/blob/master/src/number.rs)). JavaScript writes `Infinity` and `NaN` as `null` ([MDN, `JSON.stringify`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify)), and Python writes `Infinity` and `NaN`, which are not JSON, unless `allow_nan=False` ([`json`](https://docs.python.org/3/library/json.html)).
- **Parser vocabularies.** attoparsec keeps one primitive module per carrier, `Data.Attoparsec.ByteString` and `Data.Attoparsec.Text` ([Hackage](https://hackage.haskell.org/package/attoparsec)). megaparsec's `Stream` class has associated `Token` and `Tokens` types for its generic primitives, beside `Text.Megaparsec.Char` and `Text.Megaparsec.Byte`, which share names ([`Stream`](https://hackage-content.haskell.org/package/megaparsec-9.8.3/docs/Text-Megaparsec-Stream.html)); Lean 4's `Std.Internal.Parsec` makes the same split, an `Input` class whose `elem` out-parameter serves `any`, `satisfy` and `peek?`, with `String` and `ByteArray` modules for the rest ([`Std/Internal/Parsec`](https://github.com/leanprover/lean4/tree/master/src/Std/Internal/Parsec)).
- **Float readers.** Rust's `dec2flt` reads text into a `Decimal` — digits and an exponent — and hands it to one conversion, with the infinities and NaN read apart ([`dec2flt`](https://github.com/rust-lang/rust/tree/master/library/core/src/num/imp/dec2flt)). Rust's `Utf8Error::valid_up_to` is what a failed UTF-8 check reports: the length of the longest valid prefix ([`error.rs`](https://github.com/rust-lang/rust/blob/master/library/core/src/str/error.rs)).

## Decisions

Taken before stage 1, each with its reason, so a stage meets none of them as a fork.

1. **`Json/num` holds a finite number.** The constructor carries the `Flt/Finite` proof, so a literal discharges it by reduction and a computed float passes the check that answers it, as serde_json's `Number::from_f64` answers an `Option`. The decoder refuses a number beyond binary64's range; one that rounds to zero reads as zero of its sign, which is IEEE's rounding. `encode` has no `null` to invent.
2. **Each carrier keeps its own primitives.** `Parse/bytes` and `Parse/text` stay modules of their own, and `Parse/text` gains what stage 3's consumers use and nothing more. No `/std` parser reads both carriers, and `expect(Byte)` and `expect(Char)` are two operations the module tells apart.
3. **`Str/of_bytes` answers where text stops.** It answers `Result(Nat, Str)`, the failure the length of the longest prefix of the bytes that is valid text — Rust's `valid_up_to` — which its scan already reaches. `Path/to_str` and `Response/text` answer what it answers; `http/Url/decode` keeps its `Option`, whose failure also covers a malformed escape. `Response/json` refuses a body that is not text with a `Parse/Error` at that offset.
4. **`Flt`'s readers convert through `of_decimal` and `rounded/of_dyadic`.** The hexadecimal reader hands a nonzero significand to `Flt/rounded/of_dyadic` and writes the zero's sign itself, as `of_decimal_in` does, and `of_hex_body`'s clamps go. Nothing new is public.

## Stages

Each lands alone, in any order, except that stage 3 follows stages 1 and 2.

1. **`Json`'s grammar.** RFC 8259's number rule — `-? (0 | [1-9][0-9]*) (\.[0-9]+)? ([eE][+-]?[0-9]+)?` — is read by `Json`'s own parser and converted through `Flt/of_decimal`, as `Toml/numbers` converts, and the `""` default goes with the `Flt/of_str` call. `Json/num` takes decision 1's proof, and `encode` loses its `null` case.
2. **`Str/of_bytes` answers where text stops.** Decision 3, at every caller: the `/std` sites, the tests and corpus, and `programs/`. `Response/json` waits for stage 3.
3. **Text consumers read text.** `Json/decode`, `Toml`, `http/Url/of_str` and `Html` read their input as `Parse(Str, A)`: string bodies are pieces of their input and escapes are characters, and `Toml/strings`'s and `Toml/keys`' checks, `http/Url`'s `text`, and `Html/parse`'s `""` and first-byte default go. `Html`'s `until_literal` becomes a `Parse/text` primitive reading the piece before a literal's next occurrence. `Fmt/parse` walks its format's positions with `Str/At/forward` and cuts its pieces between them, so `flush`'s `nil` arms and the fuel go. `http/Url/decode` walks its bytes structurally rather than under fuel. `Response/json` decodes its body's text once, at the edge. `http/Request` and `http/Response` stay `Parse(Bytes, A)`: an HTTP head is octets, not text.
4. **`Flt`'s grammars.** `Flt`'s readers — decimal, hexadecimal, the specials and a NaN's payload — become `Parse(Str, A)` grammars over `of_decimal` and `rounded/of_dyadic` (decision 4). `Flt/of_str` and `Flt/of_hex_str`, and their rounding-mode twins in `Flt/rounded.crs`, run them to the end, and the cuts they replace — `signed`, `marked`, `pointed`, `hex_digits`, and `special_of_str`'s and `nan_of_str`'s — are deleted. `Flt/of_str` keeps computing in a type. The literal reader's `decimal_parts` (`curios-text/src/parse/literals.rs:190`) documents itself as the decomposition `/std/Flt/of_str` performs, and still says so truthfully.

## Verification

- Stage 1: RFC 8259's edges — `0`, `-0`, `1E+2`, `0.5` and `1e-400` read, the last as `+0`; `01`, `1.`, `.5`, `+1`, `1e`, `-`, `1e400` and `-1e400` are refused. `a_non_finite_number_encodes_as_null` (`curios/src/tests/corpus/strings/json.crs`) gives way to rows showing that a non-finite float has no `Json/num` and that `Json/num(+1.5)` is built from a literal.
- Stage 2: `Str/of_bytes`'s failure agrees with Rust's `valid_up_to` over a truncated sequence, an overlong one, a surrogate, a value past U+10FFFF and a stray continuation byte, each at the start, in the middle and at the end.
- Stage 3: a JSON body that is not UTF-8 is refused at `Response/json` at its offset. Each text parser reads a multi-byte character and an escape as the character it is. Every scalar value at a width boundary or beside the surrogate range, read by `Parse/text/char`, gives the character Rust decodes. `wonder stage ersd` of a text parser shows its positions as offsets and its proofs erased. `http/Url/lit` still discharges `Valid` by reduction, the `Fmt` claim below keeps its time, and the TOML codec's compile is retaken.
- Stage 4: `Flt/of_str` and `Flt/of_hex_str` agree with Rust's reading on the rounding tests' corpus (`curios/src/tests/big_num.rs`, `curios/src/tests/numeric/flt_tests.rs`). `-0x0p+0` reads `-0.0`, and `0x3p-100000000` and `0x3p+100000000` read, in every direction, what `rounded/of_dyadic` answers. `Flt/of_str("-Infinity")` in a type keeps its row of `type_level_claim_measurements` (`curios/src/tests/reduction.rs`), or the stage accounts for what moved it.
- Every stage: the prelude build is measured before and after, naming the stage.

**To retake the measurements.** The prelude build's rows are read from `curios-prelude-archive/.artifacts/profile.tsv` (elaboration) and `curios-prelude/.artifacts/profile.tsv` (certification) after `cargo xtask clippy`, folded by `target/debug/curios profile <file>`, whose first data row is the total. The TOML codec's compile is `cargo test --release --package curios --lib --all-features -- --ignored --nocapture fixpoint_pass_measurements` (`curios/src/tests/fixpoint.rs`). A claim in a type is a program handed to `cargo run --package curios -- wonder diagnostics -` on standard input and timed whole with `time`; a claim that holds reports nothing but lints. The figures above are indicative, taken on a debug build while another build ran: retake them on an otherwise idle machine before the stage that moves them. The text grammar, about 3 s:

```crs
use /std/{Parse, Result, Eq, Char, Str, Option, Bool, Nat};
let _number: Parse(Str, {Bool, Nat}) =
    let sign = Parse/optional(Parse/text/expect('-'))!;
    let run = Parse/text/take_while(Char/is_digit)!;
    let _ = Parse/eof()!;
    Parse/pure((Option/is_some(sign), Str/len(run)));
let _walk: Eq()(Parse/run(_number, "-1234567"), Result/success((true, 7))) = Eq/refl();
```

`rounded/of_dyadic` at exponents no reader clamps, about 4 s:

```crs
use /std/{Flt, Dyadic, Eq, Option};
use /std/Flt/{Rounding};
let _tiny: Eq()(Flt/rounded/of_dyadic(Rounding/ties_to_even(), Dyadic { mantissa = +3, exponent = -100000000 }), +0.0) = Eq/refl();
let _huge: Eq()(Flt/rounded/of_dyadic(Rounding/ties_to_even(), Dyadic { mantissa = +3, exponent = +100000000 }), +inf.0) = Eq/refl();
let _down: Eq()(Flt/rounded/of_dyadic(Rounding/toward_zero(), Dyadic { mantissa = -3, exponent = +100000000 }), Flt/rounded/of_dyadic(Rounding/toward_zero(), Dyadic { mantissa = -3, exponent = +2000 })) = Eq/refl();
let _neg_tiny: Eq()(Option/some(Flt/rounded/of_dyadic(Rounding/toward_negative(), Dyadic { mantissa = -3, exponent = -100000000 })), Flt/of_hex_str("-0x0.0000000000001p-1022")) = Eq/refl();
```

`Fmt` rendering a format in a type, about 3 s:

```crs
use /std/{Fmt, Eq};
let _line: Eq()(Fmt/render("request % from # took % ms, \\% of budget")(3)("peer")(12), "request 3 from \"peer\" took 12 ms, % of budget") = Eq/refl();
```

## Rejected

- **A bytes parser that checks its text at the end.** It proves nothing, scans every string twice, and keeps a refusal its `Str` input cannot reach.
- **Handing a format's number text to `Flt/of_str`.** It reads the format by `Flt`'s grammar, which is how `Json` came to accept `01`.
- **Reading `1e400` as infinity and writing it as `null`**, JavaScript's pair, which loses the value in both directions; and **a refusal in `encode`**, Go's, which leaves `Json/num` holding a value no JSON spells.
- **Generic token primitives over an associated `Token` type**, as megaparsec and Lean 4 have them, and **every primitive generic**, as nom and winnow have them. No `/std` parser reads both carriers, and a character literal checked against `Input/Token(@?I)` waits on `I`.
- **A public `Flt/of_binary`.** For every value but zero it is `rounded/of_dyadic` spelled again, and zero's sign is the reader's to write.
- **Refusing a JSON body at offset 0, or with an error of `Response/json`'s own.** The first points nowhere, and the second gives one reader a second refusal type.

## Completion criteria

- `Str/of_bytes` appears only where bytes enter and says where they stop being text, and every text parser carries its input's validity from end to end.
- Each format reads numbers by its own grammar, through one conversion per type, and writes only what its grammar spells.

## Retirement

Move the contracts to the module documentation of `Json`, `Toml`, `http/Url`, `Html`, `Fmt`, `Flt` and `Str`, and extend the README's decision with what this spec settles. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
