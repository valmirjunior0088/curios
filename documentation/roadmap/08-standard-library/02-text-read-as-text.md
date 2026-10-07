# Text read as text, and numbers by each format's grammar

Working specification for the input `/std` reads as bytes, or by another format's grammar, where it began as text: a bytes walk that reads what is left of its input where it means what it takes, the numbers `Json` reads by `Flt`'s grammar and writes as `null`, the text formats and `Fmt` that walk the bytes of input that began as a `Str`, `Flt`'s readers cutting text where a grammar belongs, and a `Str/of_bytes` that says no more than that bytes are not text. The rule is [`curios-std`'s README](../../../curios-std/README.md#std-invents-no-value-where-a-proof-belongs-and-text-is-addressed-by-position)'s: where a branch cannot be reached its unreachability is proved, where it can the caller sees an `Option`, a `Result` or a refusal, and a default stays only where it is the specified answer, documented as such.

It shares two files with [HTTP framing](01-http-framing.md): both rewrite the text readers of `http/Request.crs` and `http/Response.crs`, and stage 2 changes the type of the `Str/of_bytes` that spec hands a caller who wants a field value as text, so whichever lands second is rebased over the other. What a type pays for a position is a theory finding, [the closed machine evaluates a proof no computation reads](../02-theory/00-findings.md), which this spec names and does not fix: the stages a type runs land last, each under a counted gate that holds without it.

## What this builds on

- **Positions.** `Str/At(s)` is a byte offset into `s` with an erased proof that a character begins there, decided by the one byte at it. `At/next` and `At/prev` step a character, `Str/before`, `Str/after` and `Str/between` cut in O(1) at run time, and `Str/At/forward` and `backward` recurse over positions by well-founded recursion.
- **A fold carries validity.** `Str/fold` walks a string's bytes by structural recursion from the scan state it has reached, and hands each character built with its proof, so a walk that reads all of its text in order needs no position and no fuel.
- **One parser type, indexed by its carrier.** `Parse(Bytes, A)` reads bytes and `Parse(Str, A)` reads text, whose positions are boundaries of valid text. The combinators are written once; each carrier's primitives are a module of their own, `Parse/bytes` and `Parse/text`, one spelling per operation (`lib.crs:214`). The text primitives — `char`, `char_if`, `expect`, `literal`, `take_while`, `peek` and `rest` — have no consumer in `/std` yet.
- **A parser a type runs is total.** `http/Url/lit` discharges `Valid(s)` by running `of_str` in a type, `Fmt/render`'s type is `format_type_with` over `parse(s)`, and `Flt/of_str` is claimed in types, so everything those reach is total ([Totality of the erased program](../../design/soundness/totality-of-the-erased-program.md)). `many0` and the `sep_by` family recurse generally, which a decoder that only runs may use and a grammar a type runs may not.
- **Text grammars compute in a type.** A `Parse(Str, A)` grammar — an optional sign, a run of digits, the end — run on `"-1234567"` in an `Eq/refl` claim costs 275 034 units, and the claim with a wrong count is refused with the value the checker computed (the probes below).
- **One conversion per number type.** `Flt/of_decimal(negative, d, e, digits)` (`Flt.crs:547`, with a rounding-mode twin at `Flt/rounded.crs:141`) converts a decimal significand and its exponent, correctly rounded, and `Toml/numbers` reads TOML's own grammar and converts through it (`Toml/numbers.crs:245`). `Flt/rounded/of_dyadic` (`Flt/rounded.crs:95`) is the one way from an exact binary value to a float, and answers every exponent itself: `exact/round_in` shifts by nothing that grows with the exponent, so `3 · 2^-100000000` rounds to `+0.0`, `3 · 2^100000000` to `+inf.0`, and each directed rounding to its neighbour. A `Dyadic`'s zero has no sign.
- **One escaping walk.** `Str/escape` copies the runs between the characters a table rewrites, and `Json/encode`, `Toml/encode` and `http/Url/encode` write through it.
- **Every other reader already reads by its own grammar.** `http/Url`'s port and HTTP's status code hand `Nat/of_str` a run their own grammar has already taken as digits; `Html`'s decimal references and `Content-Length` are one or more digits, which is all `Nat/of_str` reads; `Flt`'s exponents are a sign and digits, which is all `Int/of_str` reads; and `Toml` reads its own. `Json/encode` and `Toml/encode` write `Flt/to_str`'s positional decimal, which both grammars spell.

## The gap

**A bytes walk costs what is left of its input.** `Parse/bytes/take_while` (`Parse/bytes.crs:102`) recurses on `Bytes/len(input) - at.offset` and passes a flag through every level past the byte it stopped at, so each call walks all that remains, and a document of many tokens costs the square of its length: `Json/decode` and `Html/of_str` each take about four times as long when their input doubles (the doubling measurement below). It is written by induction because `http/Url/lit` runs it in a type, where it must be total, and there its cost multiplies with each token a grammar reads, the count being the induction's hypothesis read back: a total grammar of four runs, each closed by a comma, is refused at 16 000 000 units.

**A match keeps every character it passes waiting.** `Str/meets`, through which `Str/follows`, `strip_prefix`, `starts_with` and `Parse/text/literal` match a piece, and `Str/occurrence`, through which every search and split finds its needle, rewrap what their recursion answers to restate that the answer does not lie before where they began, so neither recurs by a tail call. A prefix match and a search take a frame for each character they pass, and stop on [the stack a module runs on](../../../curios-runtime/README.md#a-module-runs-on-a-stack-of-its-own) between four and eight million of them, a prefix match over a million peaking at 287 MiB where a walk that keeps no frame peaks at 132 (one reading each, at `a490a9092` under stages 1 and 2). `Str/split`, `lines`, `replace` and `escape` build what they answer around what their recursion answers, a frame for each piece.

**`Json` reads numbers by `Flt`'s grammar, and writes one JSON cannot hold as `null`.** `Json/decode.crs:25` hands a run of number bytes to `Flt/of_str`, so `01`, `-01`, `00`, `1.`, `1.e5` and `-.5` decode, though RFC 8259 §6 refuses each, and `1e400` decodes as infinity, which no JSON spells. It reads the bytes through `Option/unwrap_or(Str/of_bytes(chunk), "")`, a default no run of ASCII number bytes reaches. `Json/num` holds any `Flt` (`Json.crs:10`), which is what leaves `encode` a `null` to invent (`Json/encode.crs:4`): `Json/num(+inf.0)` reads back as `Json/null()`.

**Text parsers drop the proof on entry.** `Toml/decode`, `http/Url/of_str` and `Html/of_str` receive a `Str`, parse its bytes, and build text back from byte slices, either with a runtime check whose refusal cannot fire on input that began as a `Str` (`Toml/strings.crs:238`, `Toml/keys.crs:10`, `http/Url.crs:39`) or with `""` (`Html/parse.crs:18`); `Html/parse.crs:33` reads a literal's first byte with a default of `0`. `Json/decode` is a bytes parser its callers hand bytes — `Str/to_bytes` of a string, or a reply's body in `Response/json` — and checks the UTF-8 of each string body it builds (`Json/decode.crs:97`). An escape decodes to bytes and the whole string is scanned again at its end. `Html/parse`'s `until_literal` and `http/Url/decode`'s walk count down a fuel, where the bytes themselves would descend.

**`Fmt` walks its format's bytes under fuel.** `Fmt/parse` (`Fmt.crs:81`) walks a format string's bytes under a fuel of three times their length and answers `Fmt/nil()`, the end of the format, where the fuel runs out, where a slice falls outside the bytes, and where a slice is not UTF-8 (`Fmt.crs:24`) — none of which a `Str` reaches. It runs in every type that mentions `Fmt/render(s)`, and a 40-character format rendered in a type costs 267 570 units.

**`Flt` reads its own syntax by cutting text.** `Flt/of_str` and `Flt/of_hex_str` compose `Str`'s cuts — `strip_prefix` for the sign and the `0x`, `split_once` for the point, the exponent and a NaN's payload, `eql_ascii_ci` for the specials — where a grammar would state the syntax and a parser would carry the positions. `marked` cuts at `e` before `E` and leans on a later refusal when both occur. `of_hex_body` (`Flt.crs:745`) clamps its exponent ahead of `exact/round`, which answers every exponent itself.

**`Str/of_bytes` answers `none` alone.** The edge that should check a body's text once has no position to report: `Str/Valid/try` folds the scan to its final state, which carries no offset and does not stop at the byte it refuses.

**A position costs its offset in a type.** The finding this spec names: a step of `Str/At/skip` reaches `Bytes/drop`, whose bound is a lemma proved by induction on the offset, and the closed machine runs that induction, as it runs the proof that a concatenation is text. A `Fmt/parse` written over `Str/At/forward` costs 1 814 560 units on the format the fuel walk reads for 267 570, and no respelling within `/std` moves it: `drop` matching on its bound's decision no longer unfolds to its window, which `Str/At/of_after` reads, and deciding the bound alone moves that figure by a hundredth.

## Prior art

- **JSON.** RFC 8259 §6 states the number rule, "allows implementations to set limits on the range and precision of numbers accepted", and names `1E400` as a number that may not interoperate ([RFC 8259 §6](https://www.rfc-editor.org/rfc/rfc8259#section-6)); I-JSON asks senders to stay within binary64 ([RFC 7493 §2.2](https://www.rfc-editor.org/rfc/rfc7493#section-2.2)). serde_json answers "Error instead of +/- infinity" for an exponent that overflows and zero of the number's sign for one that underflows ([`de.rs`](https://github.com/serde-rs/json/blob/master/src/de.rs)), and `None` from `Number::from_f64` for a non-finite float, since "Infinite or NaN values are not JSON numbers" ([`number.rs`](https://github.com/serde-rs/json/blob/master/src/number.rs)). Go refuses both a number that overflows its target and a non-finite float to encode ([`encoding/json`](https://pkg.go.dev/encoding/json)), and converts a number by handing `strconv.ParseFloat` the text its scanner has validated ([`decode.go`](https://github.com/golang/go/blob/master/src/encoding/json/decode.go)). JavaScript writes `Infinity` and `NaN` as `null` ([MDN, `JSON.stringify`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify)). JSONTestSuite names its cases by what a parser owes them — `y_` accepted, `n_` refused, `i_` free — and holds 51 `n_number_*` cases, 19 `y_number*` ones and 10 `i_number_*` ones, overflow and underflow among the free ([JSONTestSuite](https://github.com/nst/JSONTestSuite)).
- **Where text stops.** Rust's `Utf8Error::valid_up_to` is "the maximum index such that `from_utf8(&input[..index])` would return `Ok(_)`", beside an `error_len` that tells an input cut short from one that is wrong ([`error.rs`](https://github.com/rust-lang/rust/blob/master/library/core/src/str/error.rs)). Haskell's `validateUtf8Chunk` answers the end of the longest valid prefix and a state to resume from ([`Data.Text.Encoding`](https://hackage-content.haskell.org/package/text-2.1.2/docs/Data-Text-Encoding.html)), and Swift's `UTF8.EncodingError` a kind and a range of offsets ([SE-0464](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0464-utf8span-safe-utf8-processing.md)). All three report the prefix length.
- **Positions.** Lean 4's `String.Pos s` is "a byte offset in `s` together with a proof that this position is at a UTF-8 character boundary", with slices and termination helpers of its own ([Strings](https://lean-lang.org/doc/reference/latest/Basic-Types/Strings/)).
- **Parser vocabularies.** attoparsec keeps one primitive module per carrier, `Data.Attoparsec.ByteString` and `Data.Attoparsec.Text` ([Hackage](https://hackage.haskell.org/package/attoparsec)). megaparsec's `Stream` class has `takeWhile_` among its methods because it "allows for significant performance improvements, although it is not strictly necessary from conceptual point of view" ([`Stream`](https://hackage-content.haskell.org/package/megaparsec-8.0.0/docs/Text-Megaparsec-Stream.html)); Lean 4's `Std.Internal.Parsec` is a generic `Basic` module with `String` and `ByteArray` modules beside it ([`Std/Internal/Parsec`](https://github.com/leanprover/lean4/tree/master/src/Std/Internal/Parsec)).
- **Float readers.** Rust's `dec2flt` reads text into digits and an exponent — "Conceptually, `12.34e56` turns into `1234e54`" — and hands them to one conversion, with the infinities and NaN read apart ([`dec2flt`](https://github.com/rust-lang/rust/tree/master/library/core/src/num/imp/dec2flt)).
- **A format a type computes.** Idris's `printf` takes its type from `toFormat (unpack str)`, a structural recursion over the format's characters ([formatting with first class types](https://www.innoq.com/en/blog/formatting-with-first-class-types/)).

## Decisions

Taken before stage 1, each with its reason, so a stage meets none of them as a fork.

1. **`Json/num` holds a finite number.** The constructor carries the `Flt/Finite` proof, `num(value: Flt, @finite: Flt/Finite(value))`, so a literal discharges it by reduction and a computed float passes `Flt/finite`, which answers it, as serde_json's `Number::from_f64` answers an `Option`. The decoder refuses a number beyond binary64's range; one that rounds to zero reads as zero of its sign, which is IEEE's rounding and serde_json's answer. `encode` has no `null` to invent.
2. **Each carrier keeps its own primitives.** `Parse/bytes` and `Parse/text` stay modules of their own, and `Parse/text` gains one primitive, the piece before a literal's next occurrence, which `Html` reads a comment and a raw-text element through; every other consumer is served by what is there. No `/std` parser reads both carriers, and `expect(Byte)` and `expect(Char)` are two operations the module tells apart.
3. **`Str/of_bytes` answers where text stops.** It answers `Result(Nat, Str)`, the failure the length of the longest prefix of the bytes that is valid text — Rust's `valid_up_to` — found by a walk of its own that only a failure runs, since the scan that decides validity keeps no offset. `Path/to_str` and `Response/text` answer what it answers; `http/Url/decode` keeps its `Option`, whose failure also covers a malformed escape. `Response/json` refuses a body that is not text with a `Parse/Error` at that offset. Nothing in `/std` reads more of a failure than where it lies.
4. **`Flt`'s readers convert through `of_decimal` and `rounded/of_dyadic`.** The hexadecimal reader hands a nonzero significand to `Flt/rounded/of_dyadic` and writes the zero's sign itself, as `of_decimal_in` does, and `of_hex_body`'s clamps go. The grammars accept exactly the text the cuts accept, every spelling `to_str` and `to_hex_str` write among it. Nothing new is public.
5. **A walk stops where its predicate does, and is total.** It recurses on the bytes it reads — `x[h, ..t]`, as `Bytes/fold` and `Str/fold` do — and answers how many it took as a number, the bound of the position it hands on decided where that position is built and argued only in the arm the decision refutes. So it costs what it takes at run time, and in a type it is run once, whatever reads the position after it. It never recurses on what is left of its input, and never on a fuel: where the bytes descend, they are the measure. A walk over positions that owes a proof about where it began is handed that proof with its position, so what it answers is what its recursion answers and it keeps nothing waiting on a character it passes, and one that builds its answer hands on what it has built.
6. **`Fmt/parse` is a fold over its format's characters.** A format is read whole, in order, and never returned to, which is what `Str/fold` is: the state is the tree built so far, the literal run and whether a backslash is pending, and every piece is text because every character it is built from is. It costs 478 473 units on the 40-character format, against 1 814 560 for a walk over positions.
7. **A stage a type runs lands under a counted gate.** `Flt`'s grammar does not raise what `Flt/of_str("-1234.5678e-3")` costs in a type; `Fmt`'s fold keeps the 40-character format under twice what the fuel walk costs it; and `http/Url/lit` on a 32-character literal stays under twice what it costs once stage 1 has landed. A gate that fails stops its stage: the consumer stays as it is, and this spec keeps that stage, named as waiting on the theory finding.
8. **Each format is converted once, whole.** A format's grammar, its number rule and its edge move in one stage, so no grammar is written over bytes and written again over text.

## Stages

Each lands alone. Stage 1 comes first, since `http` keeps reading bytes whatever follows. Stage 3 follows stage 2, whose offset `Response/json` reports. Between them `Str/meets` and `Str/occurrence` are respelled by decision 5, since every text parser that follows matches and searches through them, and `Str/split`, `lines`, `replace` and `escape` after them. Stages 5 to 7 are the ones a type runs, and land in that order behind decision 7's gates.

1. **A walk that stops.** `Parse/bytes/take_while` counts the bytes `pred` accepts at the front of what `Bytes/drop` leaves, by decision 5, and builds its position and its piece from the count and its bound.
2. **`Str/of_bytes` answers where text stops.** Decision 3, at every caller: the `/std` sites, the tests and corpus, and `programs/`. `Parse/bytes`'s `byte_spelling` spells its byte through `Char/of_ascii`, so `Str/of_bytes` is left to the edges. `Response/json` waits for stage 3.
3. **`Json`, whole.** `Json/decode` reads `Parse(Str, Json)`: string bodies are pieces of their input, escapes are characters, and RFC 8259's number rule — `-? (0 | [1-9][0-9]*) (\.[0-9]+)? ([eE][+-]?[0-9]+)?` — is read by `Json`'s own parser and converted through `Flt/of_decimal`, as `Toml/numbers` converts. `Json/num` takes decision 1's proof, `encode` loses its `null` case, and `Response/json` decodes its body's text once, at the edge.
4. **`Toml` and `Html`.** Both read their input as `Parse(Str, A)`: `Toml/strings`'s and `Toml/keys`' checks go, and so do `Html/parse`'s `""`, its first-byte default and its fuel, `until_literal` becoming decision 2's primitive.
5. **`Flt`'s grammars.** `Flt`'s readers — decimal, hexadecimal, the specials and a NaN's payload — become `Parse(Str, A)` grammars over `of_decimal` and `rounded/of_dyadic` (decision 4), built from total combinators alone. `Flt/of_str` and `Flt/of_hex_str`, and their rounding-mode twins in `Flt/rounded.crs`, run them to the end, and the cuts they replace — `signed`, `marked`, `pointed`, `hex_digits`, and `special_of_str`'s and `nan_of_str`'s — are deleted. The literal reader's `decimal_parts` (`curios-text/src/parse/literals.rs:190`) documents itself as the decomposition `/std/Flt/of_str` performs, and still says so truthfully.
6. **`Fmt` as a fold.** Decision 6: `flush`, the three walkers and the fuel go, and `parse`, `format_type_with`, `render`, `print` and `print_err` keep their names and types.
7. **`http/Url`.** `of_str` reads `Parse(Str, A)` and `text` goes; `decode` walks its component's bytes by decision 5 rather than under fuel. `http/Request` and `http/Response` stay `Parse(Bytes, A)`: an HTTP head is octets, not text.

## Verification

- Stage 1: a total grammar of eight tokens costs under eight times what one of four costs in a type, as a test beside `a_packed_fold_costs_linearly_in_its_length` (`curios/src/tests/reduction.rs`), where the induction's cost multiplies with each token; `http/Url/lit` still discharges `Valid` by reduction, and its cost is read once the stage has landed. The doubling measurement is added beside the string-walk ladder (`curios/src/tests/codegen/ladder.rs`).
- Stage 2: `Str/of_bytes`'s failure agrees with Rust's `valid_up_to` over a truncated sequence, an overlong one, a surrogate, a value past U+10FFFF and a stray continuation byte, each at the start, in the middle and at the end.
- The matchers: a search and a prefix match each pass `2^24` characters (`curios-std/src/tests/strings/decomposition.crs`), more than [the stack a module runs on](../../../curios-runtime/README.md#a-module-runs-on-a-stack-of-its-own) holds a frame apiece for. In a type a match costs within a twentieth of what it did, `Str/starts_with` over 32 characters 641 160 units against 623 824 and `Str/has_substr` passing 32 characters 459 303 against 442 109, and `Parse/text/literal` costs less, 465 537 over four characters against 672 335 (counted, at `a490a9092` under stages 1 and 2, with the change and without it); [what a literal costs beside its parts](00-findings.md) is filed.
- The builders: a text is split, read by lines, replaced in and escaped over `2^17` pieces (the same file), which holds them to answer at that size and not to the stack they use: the spelling before this one still reads `2^22` lines, peaking at 1 352 MiB, and a test past that is not one the file can afford. One reading each over `2^20` pieces, at `a490a9092` under the matchers, with the change and without it: `split` peaks at 260 MiB against 439 and `lines` at 260 against 344, and `replace` peaks at 516 against 472 and `escape` at 516 against 440, more and not less. In a type a split of 32 pieces costs 1 668 703 units and a replacement at 31 places 1 271 273, each no more than it did (counted, the same commit).
- Stage 3: the rows of `curios-std/src/tests/strings/json.crs` take JSONTestSuite's `n_number_*` cases as refusals and its `i_number_*` ones as this spec decides them — an overflow refused, an underflow read as zero of its sign — beside `0`, `-0`, `1E+2` and `0.5`, which read, and `-01`, `00`, `1.e5` and `-.5`, which no longer do. `a_non_finite_number_encodes_as_null` gives way to rows showing that a non-finite float has no `Json/num` and that `Json/num(+1.5)` is built from a literal. A JSON body that is not UTF-8 is refused at `Response/json` at its offset. Every refusal the corpus pins by its message keeps it.
- Stage 4: each text parser reads a multi-byte character and an escape as the character it is, and `curios/src/tests/toml.rs` and `curios-std/src/tests/strings/html.crs` expect what they expected. The primitive that reads up to a literal answers the whole rest where the literal does not occur.
- Stage 5: `Flt/of_str` and `Flt/of_hex_str` agree with Rust's reading on the rounding tests' corpus (`curios/src/tests/big_num.rs`, `curios/src/tests/numeric/flt_tests.rs`). `-0x0p+0` reads `-0.0`, and `0x3p-100000000` and `0x3p+100000000` read, in every direction, what `rounded/of_dyadic` answers. The decimal claim holds its gate, and `type_level_claim_measurements` (`curios/src/tests/reduction.rs`) is retaken with its `Flt/of_str` row accounted for: `"-Infinity"` no longer reads the floor.
- Stage 6: `curios-std/src/tests/data/fmt.crs` and `curios/src/tests/fmt.rs` expect what they expected, and the claim below holds its gate.
- Stage 7: `http/Url/lit` discharges `Valid` by reduction and holds its gate; a percent-escape that is cut short or is no hexadecimal pair is still `none`.
- Every stage: `/std` elaborates and certifies, and the counted column of the prelude build is read before and after, naming the stage.

**To retake the measurements.** A counted figure in this document is taken at `bf21a9065`, unless it names another commit, and is the least `N` under which `cargo run --release --package curios -- wonder diagnostics --budget N -` accepts the program on standard input, neither checker reporting an exhaustion, found by bisection: what the program's heaviest declaration costs the checker that spends more on it. A claim is therefore measured in a program that declares nothing heavier, a definition written for the measurement is measured with the claim and without it, and an answer that is a panic is neither a refusal nor an acceptance ([a budget refusal can panic the printer](../02-theory/00-findings.md)). The prelude build's counted column is `budget::consumed`, which each checker samples once per declaration, summed over its `V` rows in `curios-prelude-archive/.artifacts/profile.tsv` (elaboration) and `curios-prelude/.artifacts/profile.tsv` (certification) after `cargo xtask clippy`, before another build of the prelude writes over them: 36 326 309 and 13 654 779 units. The doubling measurement is timed: it runs `Json/decode` over `[1, 1, …]`, `Html/of_str` over `<p>x</p>…` and a `take_while` and `expect` token walk over `abcdefgh,…`, each at three sizes that double, and reports the ratio between neighbours as a median over interleaved rounds; readings taken once at `bf21a9065` while other builds ran, 0.55 s, 2.1 s and 11 s for `Json/decode` at 15, 30 and 60 KB, show its shape and are not its figures.

The text grammar, 275 034 units:

```crs
use /std/{Parse, Result, Eq, Char, Str, Option, Bool, Nat};
let _number: Parse(Str, {Bool, Nat}) =
    let sign = Parse/optional(Parse/text/expect('-'))!;
    let run = Parse/text/take_while(Char/is_digit)!;
    let _ = Parse/eof()!;
    Parse/pure((Option/is_some(sign), Str/len(run)));
let _walk: Eq()(Parse/run(_number, "-1234567"), Result/success((true, 7))) = Eq/refl();
/std/print("ok")
```

`Fmt` rendering a 40-character format in a type, 267 570 units, the gate of stage 6 at twice that:

```crs
use /std/{Fmt, Eq};
let _line: Eq()(Fmt/render("request % from # took % ms, \\% of budget")(3)("peer")(12), "request 3 from \"peer\" took 12 ms, % of budget") = Eq/refl();
/std/print("ok")
```

`Flt/of_str` reading a decimal in a type, 357 208 units, the gate of stage 5:

```crs
use /std/{Flt, Option, Eq};
let _claim: Eq()(Flt/of_str("-1234.5678e-3"), Option/some(-1.2345678)) = Eq/refl();
/std/print("ok")
```

`http/Url/lit` on a 32-character literal, the gate of stage 7 at twice what it costs once stage 1 has landed:

```crs
use /std/{Str, Nat};
use /std/http/{Url};
let home: Url =
    Url/lit("https://example.com:8443/a/b?x=1");
/std/print(Url/to_str(home))
```

`rounded/of_dyadic` at exponents no reader clamps:

```crs
use /std/{Flt, Dyadic, Eq, Option};
use /std/Flt/{Rounding};
let _tiny: Eq()(Flt/rounded/of_dyadic(Rounding/ties_to_even(), Dyadic { mantissa = +3, exponent = -100000000 }), +0.0) = Eq/refl();
let _huge: Eq()(Flt/rounded/of_dyadic(Rounding/ties_to_even(), Dyadic { mantissa = +3, exponent = +100000000 }), +inf.0) = Eq/refl();
let _down: Eq()(Flt/rounded/of_dyadic(Rounding/toward_zero(), Dyadic { mantissa = -3, exponent = +100000000 }), Flt/rounded/of_dyadic(Rounding/toward_zero(), Dyadic { mantissa = -3, exponent = +2000 })) = Eq/refl();
let _neg_tiny: Eq()(Option/some(Flt/rounded/of_dyadic(Rounding/toward_negative(), Dyadic { mantissa = -3, exponent = -100000000 })), Flt/of_hex_str("-0x0.0000000000001p-1022")) = Eq/refl();
/std/print("ok")
```

## Rejected

- **A bytes parser that checks its text at the end.** It proves nothing, scans every string twice, and keeps a refusal its `Str` input cannot reach.
- **A walk on what is left of its input, or on a fuel.** The first is total and costs the square of a document; the second invents an answer for the fuel's end, which its input never reaches.
- **A walk that recurses by a comparison alone**, `go(taken + 1)` while a byte is left. Nothing descends, so the definition is partial and `http/Url/lit` is refused the type it reduces in.
- **Handing a format's number text to `Flt/of_str`.** It reads the format by `Flt`'s grammar, which is how `Json` came to accept `01`. **Validating by the format's grammar and then handing the same text over**, as Go does, reads every digit twice and rests on one grammar being inside the other, which nothing checks.
- **Reading `1e400` as infinity and writing it as `null`**, JavaScript's pair, which loses the value in both directions; and **a refusal in `encode`**, Go's, which leaves `Json/num` holding a value no JSON spells.
- **Generic token primitives over an associated `Token` type**, as megaparsec and Lean 4 have them, and **every primitive generic**, as nom and winnow have them. No `/std` parser reads both carriers, and a character literal checked against `Input/Token(@?I)` waits on `I`.
- **A public `Flt/of_binary`.** For every value but zero it is `rounded/of_dyadic` spelled again, and zero's sign is the reader's to write.
- **Refusing a JSON body at offset 0, or with an error of `Response/json`'s own.** The first points nowhere, and the second gives one reader a second refusal type.
- **A failure of `Str/of_bytes` that carries more than where it lies** — whether the bytes were cut short or wrong, as Rust's `error_len` and Swift's kind say, or the valid prefix as a `Str`. Nothing in `/std` reads either: `Tui/input` decodes one scalar from a slice it has already measured.
- **`Fmt/parse` over `Str/At/forward`.** It cuts its pieces where the fold concatenates them, and costs nearly seven times the fuel walk in every type that mentions a format, growing with the square of the format's length, until the theory finding is fixed; a format is read once and in order, so it has no use for a position.
- **`Bytes/drop` respelled so its bound costs a type nothing.** Matching on the bound's decision keeps `drop` from unfolding to its window, which the proofs of `Str/At` read; deciding the bound alone in a helper is a workaround for one instance of what the machine does to every proof.

## Completion criteria

- A bytes walk costs what it takes, and every `/std` decoder's time grows with its input and not with its square.
- `Str/of_bytes` appears only where bytes enter and says where they stop being text, and every text parser carries its input's validity from end to end.
- Each format reads numbers by its own grammar, through one conversion per type, and writes only what its grammar spells.
- No walk over text or bytes descends on a fuel.

## Retirement

Move the contracts to the module documentation of `Json`, `Toml`, `http/Url`, `Html`, `Fmt`, `Flt`, `Str` and `Parse`, and extend the README's decision with what this spec settles. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it. A stage a gate stopped stays: the spec shrinks to it.
