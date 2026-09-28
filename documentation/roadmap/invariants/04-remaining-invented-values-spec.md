# Standard-library invariants, part 4: the library's remaining invented values

Working specification for the values `/std` still makes up where a proof or a refusal belongs, after the invariants work removed the rest: the text formats that re-check the UTF-8 of input that began as a `Str`, the numbers two formats read by `Flt`'s grammar, the requests HTTP says to refuse and the library reads as values, and a branch that waits on a decision. The rule is the one the invariants work settled, recorded in [`curios-prelude-archive`'s README](../../../curios-prelude-archive/README.md): where a branch cannot be reached its unreachability is proved, where it can the caller sees an `Option`, a `Result` or a refusal, and a default stays only where it is the specified answer, documented as such.

Its first two stages are the campaign's first wave and need nothing. The text and number stages follow [part 1](01-checkers-agree-spec.md)'s third stage, the change to solving, so they are written against the solver it leaves rather than around the one it replaces. It is otherwise independent of the other parts.

## What this builds on

- **Positions.** `Str/At(s)` is a byte offset into `s` with an erased proof that a character begins there, decided by the one byte at it. `At/next` and `At/prev` step a character, `Str/before`, `Str/after` and `Str/between` cut in O(1), and `Str/At/forward` and `backward` recurse over positions by well-founded recursion.
- **One parser type.** `Parse/Of(P, A)` is indexed by what its positions know: `Parse(A)` over bytes, whose positions lie within the input, and `Parse/Text(A)` over text, whose positions are boundaries of valid text. The combinators are written once. The text primitives — `char`, `char_if`, `expect`, `literal`, `take_while` answering a `Str` piece, `peek`, `rest`, `position`, `back` — are `Parse/Text`'s, exercised by `curios/src/tests/corpus/strings/parse.crs`.
- **One conversion per number type.** `Flt/of_decimal(negative, d, e, digits)` (`Flt.crs`, with a rounding-mode twin in `Flt/rounded.crs`) converts a decimal significand and its exponent, correctly rounded. `Toml/numbers` already reads TOML's own number grammar and converts through it.
- **One escaping walk.** `Str/escape` copies the runs between the characters a table rewrites, and `Json/encode`, `Toml/encode` and `http/Url/encode` write through it.

## The gap

**`Json` reads numbers by `Flt`'s grammar.** `Json/decode.crs:25` hands a run of number bytes to `Flt/of_str`, so `01` and `1.` decode, though RFC 8259 refuses both. It reads the bytes through `Option/unwrap_or(Str/of_bytes(chunk), "")`, a default no run of ASCII number bytes reaches.

**HTTP reads as values what it says to refuse.** A request whose `Content-Length` is not a number is read as having no body, by `http/Server`'s `needed` (`Option/unwrap_or(…, 0)`) and by `http/Request`'s `body_of` (its `none()` arm); RFC 9112 makes that framing invalid and asks for a 400. A request without `Host` is read as addressed to `"localhost"` (`http/Request.crs:126`), where RFC 9112 asks a server to answer an HTTP/1.1 request that lacks one with a 400. A request without `Content-Length` has no body, and that half is specified and documented.

**Text parsers drop the proof on entry.** `Json/decode`, `Toml/decode`, `http/Url` and `Html/parse` receive a `Str`, parse its bytes, and build text back from byte slices, either with a runtime check whose refusal cannot fire on input that began as a `Str` (`Json/decode.crs:97`, `Toml/strings.crs:238`, `Toml/keys.crs:10`, `http/Url.crs:39`) or with `""` (`Html/parse.crs:18`). `Html/parse.crs:33` reads a literal's first byte with a default of `0`. An escape decodes to bytes and the whole string is scanned again at its end.

**`Flt` reads its own syntax by walking positions.** `Flt/of_str` and `Flt/of_hex_str` recurse over `Str/At` positions by hand — the specials, the sign, the digits, a NaN's payload — where a grammar would state the syntax and a parser would carry the positions.

**A branch that waits on a decision.** `Tui/Session`'s read (`Tui/Session.crs:100`) ends a burst of input on `_ => settle()`, which covers a timeout, the end of standard input and a failed read alike, so a read error reads as silence. Whether a failed read should end the session or be reported is a decision this part makes before changing it.

## Stages

Each lands alone.

1. **`Json`'s numbers.** RFC 8259's grammar — `-? (0 | [1-9][0-9]*) (\.[0-9]+)? ([eE][+-]?[0-9]+)?` — read as a bytes parser and converted through `Flt/of_decimal`, as `Toml/numbers` converts. `0`, `-0`, `1E+2` and `0.5` read; `01`, `1.`, `.5`, `+1` and `1e` are refused. The `""` default goes with the `Flt/of_str` call.
2. **HTTP's refusals.** A malformed `Content-Length` is a refusal in `Request/decode` and in the server's framing, answered with a 400; a missing `Host` on an HTTP/1.1 request is a refusal answered with a 400, and an HTTP/1.0 request without one keeps an authority the server states rather than one the library invents. `needed`'s and `body_of`'s documentation says which half is specified.
3. **Text consumers.** `Json/decode`, `Toml` and `http/Url` read text through `Parse/Text`: string bodies are pieces of their input, and escapes are characters. `Html/parse` moves with them, and its `""` and its first-byte default go. `Response/json` checks its body's UTF-8 once, at the edge, where invalid UTF-8 is its refusal. `http/Request` and `http/Response` stay bytes parsers under the bound: an HTTP head is octets, not text.
4. **`Flt`'s grammars.** `Flt`'s readers — decimal, hexadecimal, the specials and a NaN's payload — become `Parse/Text` grammars over `of_decimal` and its binary twin, which is drawn out of `of_hex_str`'s tail (`exact/round` under its clamps) and made public. `Flt/of_str` and `Flt/of_hex_str` run them to the end, and the position walks they replace are deleted. `Flt/of_str` must keep computing in a type, as it does today.
5. **The session's read.** `Tui/Session`'s read settles the decision above and follows it.

## Verification

- Stage 1's rows at RFC 8259's edges, each the refusal or the value the grammar gives, and a row per default removed showing what the input now answers.
- Stage 2: a malformed `Content-Length` and a missing `Host` are each answered 400 by a running server, over the mock host, and a well-formed request is unchanged.
- A JSON body that is not UTF-8 is refused at `Response/json`, and a text parser reads a multi-byte character and an escape as the character it is.
- Every scalar value at a width boundary or beside the surrogate range, read through `Parse/Text`, gives the character Rust decodes.
- `Flt/of_str` and `Flt/of_hex_str` agree with Rust's reading on the rounding tests' corpus, and `Flt/of_str` still closes a claim in a type.
- `wonder stage ersd` of a text parser shows its positions as offsets and its proofs erased.
- The prelude build is measured before and after each stage, naming the stage.

## Rejected

- **A bytes parser that checks its text at the end.** It proves nothing, scans every string twice, and keeps a refusal its `Str` input cannot reach.
- **Handing a format's number text to `Flt/of_str`.** It reads the format by `Flt`'s grammar, which is how `Json` came to accept `01`.
- **Reading an HTTP head as text.** A header may carry obsolete octets outside UTF-8, and the head's framing is bytes; text is what a body declared to be text is.

## Completion criteria

- No `/std` declaration answers a value it made up where a proof or a refusal belongs: every remaining `unwrap_or`, defaulted `none()` arm and catch-all arm is a specified answer, and its documentation says which.
- `Str/of_bytes` appears only where bytes enter, and every text parser carries its input's validity from end to end.
- Each format reads numbers by its own grammar, through one conversion per type.

## Retirement

Move the contracts to the module documentation of `Json`, `Toml`, `http`, `Html` and `Flt`, and extend the README's decision with what this part settles. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
