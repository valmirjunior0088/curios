# Standard-library invariants, part 4: the library's remaining invented values

Working specification for the values `/std` still makes up where a proof or a refusal belongs, after the invariants work removed the rest: the requests and replies HTTP says to refuse and the library reads as values, the numbers `Json` reads by `Flt`'s grammar, the text formats that re-check the UTF-8 of input that began as a `Str`, and a branch that waits on a decision. The rule is the one the invariants work settled, recorded in [`curios-prelude-archive`'s README](../../../curios-prelude-archive/README.md): where a branch cannot be reached its unreachability is proved, where it can the caller sees an `Option`, a `Result` or a refusal, and a default stays only where it is the specified answer, documented as such.

It follows the change that indexes `Parse` by the carrier it reads, which the campaign lands first. The text and number stages also follow [part 1](01-checkers-agree-spec.md)'s fourth stage, the change to solving, so they are written against the solver it leaves rather than around the one it replaces. It is otherwise independent of the other parts.

## What this builds on

- **Positions.** `Str/At(s)` is a byte offset into `s` with an erased proof that a character begins there, decided by the one byte at it. `At/next` and `At/prev` step a character, `Str/before`, `Str/after` and `Str/between` cut in O(1), and `Str/At/forward` and `backward` recurse over positions by well-founded recursion.
- **One parser type, indexed by its carrier.** `Parse(Bytes, A)` reads bytes and `Parse(Str, A)` reads text, whose positions are boundaries of valid text; the combinators are written once for both, and each carrier has its primitives — the text ones read characters and pieces of the input by position.
- **One conversion per number type.** `Flt/of_decimal(negative, d, e, digits)` (`Flt.crs`, with a rounding-mode twin in `Flt/rounded.crs`) converts a decimal significand and its exponent, correctly rounded. `Toml/numbers` already reads TOML's own number grammar and converts through it.
- **One escaping walk.** `Str/escape` copies the runs between the characters a table rewrites, and `Json/encode`, `Toml/encode` and `http/Url/encode` write through it.
- **A refused request is answered.** `http/Server`'s `answer` sends a 400 for any request `Request/decode` refuses, so a refusal in the decoder is the whole of what a server needs to reject one.

## The gap

**No inventory says which defaults are specified.** The rule needs a verdict for every value the library answers where its input gave none — 18 `unwrap_or` sites in 12 modules, and the arms answering a literal in place of a missing value — and none has been taken, so what remains is known only from the cases below.

**HTTP reads as values what it says to refuse.** RFC 9112's framing rules, against what the library does today, both directions measured:

| Input | The library | RFC 9112 |
| --- | --- | --- |
| A request whose `Content-Length` is not a number | no body (`body_of`'s `none()` arm; the server's `needed` reads it as `0`) | an unrecoverable framing error, answered 400 (§6.3) |
| A request whose body is shorter than its `Content-Length` | the bytes that arrived, as the whole body (`Parse/take` clamps) | incomplete; a server may answer 400 before closing (§8), and does not hand it on as complete |
| A request with `Transfer-Encoding: chunked` | no body: the field is not read | the chunked body decoded, a coding it does not understand answered 501, and chunked other than last answered 400 (§6.1, §6.3) |
| An HTTP/1.1 request without `Host` | addressed to `"localhost"` (`http/Request.crs:126`) | 400 (§3.2) |
| A request with two `Host` lines | the first | 400 (§3.2) |
| A reply whose `Content-Length` is not a number | everything the peer sent | discarded, the connection closed (§6.3) |
| A reply shorter than its `Content-Length` | the bytes that arrived, documented as clamped | recorded as incomplete (§8) |

A request without `Content-Length` or `Transfer-Encoding` has no body, and a `Content-Length` that is a list of one repeated value is that value (§6.3); both are specified.

**`Json` reads numbers by `Flt`'s grammar.** `Json/decode.crs:25` hands a run of number bytes to `Flt/of_str`, so `01` and `1.` decode, though RFC 8259 refuses both. It reads the bytes through `Option/unwrap_or(Str/of_bytes(chunk), "")`, a default no run of ASCII number bytes reaches.

**Text parsers drop the proof on entry.** `Toml/decode`, `http/Url/of_str` and `Html/of_str` receive a `Str`, parse its bytes, and build text back from byte slices, either with a runtime check whose refusal cannot fire on input that began as a `Str` (`Toml/strings.crs:238`, `Toml/keys.crs:10`, `http/Url.crs:39`) or with `""` (`Html/parse.crs:18`); `Html/parse.crs:33` reads a literal's first byte with a default of `0`. `Json/decode` is a bytes parser its callers hand bytes — `Str/to_bytes` of a string, or a reply's body in `Response/json` — and checks the UTF-8 of each string body it builds (`Json/decode.crs:97`). An escape decodes to bytes and the whole string is scanned again at its end.

**`Flt` reads its own syntax by cutting text.** `Flt/of_str` and `Flt/of_hex_str` compose `Str`'s cuts — `strip_prefix` for the sign and the `0x`, `split_once` for the point, the exponent and a NaN's payload, `eql_ascii_ci` for the specials — where a grammar would state the syntax and a parser would carry the positions. `marked` cuts at `e` before `E` and leans on a later refusal when both occur.

**A branch that waits on a decision.** `Tui/Session`'s read (`Tui/Session.crs:100`) ends a burst of input on `_ => settle()`, which covers a timeout, the end of standard input and a failed read alike, so a read error reads as silence. Whether a failed read should end the session or be reported is a decision this part makes before changing it.

## Stages

Each lands alone.

1. **The inventory.** Every `unwrap_or`, and every arm answering a literal where its input gave no value, gets its verdict where it stands: its unreachability proved and the default gone, the standard or contract that specifies it cited in its documentation, or the later stage of this part that changes it. What a later stage changes is listed here until it lands.
2. **HTTP's framing.** Each row of the table above takes RFC 9112's answer. A request `Request/decode` refuses is answered 400 by the path that exists, and one whose coding the server does not understand 501; the chunked decoding `Response` already has serves requests as well. A reply the client cannot frame is a refusal of the exchange, and an incomplete one is reported as incomplete rather than returned. `needed`'s, `body_of`'s and `Response/decode`'s documentation says which answers are specified.
3. **`Json`'s numbers.** RFC 8259's grammar — `-? (0 | [1-9][0-9]*) (\.[0-9]+)? ([eE][+-]?[0-9]+)?` — is read by `Json`'s own parser and converted through `Flt/of_decimal`, as `Toml/numbers` converts. `0`, `-0`, `1E+2` and `0.5` read; `01`, `1.`, `.5`, `+1` and `1e` are refused. The `""` default goes with the `Flt/of_str` call.
4. **Text consumers.** `Json/decode`, `Toml`, `http/Url` and `Html` read text as `Parse(Str, A)`: string bodies are pieces of their input, and escapes are characters. `Html/parse`'s `""` and its first-byte default go. `Response/json` checks its body's UTF-8 once, at the edge, where invalid UTF-8 is its refusal. `http/Request` and `http/Response` stay `Parse(Bytes, A)`: an HTTP head is octets, not text. Whether the two carriers' primitives share one vocabulary — one `expect`, `literal` and `take_while` read by either carrier — is decided at the start of this stage, with its options presented before either is built.
5. **`Flt`'s grammars.** `Flt`'s readers — decimal, hexadecimal, the specials and a NaN's payload — become `Parse(Str, A)` grammars over `of_decimal` and its binary twin, which is drawn out of `of_hex_str`'s tail (`exact/round` under its clamps) and made public. `Flt/of_str` and `Flt/of_hex_str`, and their rounding-mode twins in `Flt/rounded.crs`, run them to the end, and the cuts they replace are deleted. `Flt/of_str` must keep computing in a type, as it does today.
6. **The session's read.** `Tui/Session`'s read settles the decision above and follows it.

## Verification

- Stage 1's verdicts, one per site, each where it stands.
- Stage 2: each row of the table, read by `Request/decode` or `Response/decode`, gives RFC 9112's answer, and a running server over the mock host answers the request rows with their status; a well-formed request and reply are unchanged.
- Stage 3's rows at RFC 8259's edges, each the refusal or the value the grammar gives, and a row per default removed showing what the input now answers.
- A JSON body that is not UTF-8 is refused at `Response/json`, and a text parser reads a multi-byte character and an escape as the character it is.
- Every scalar value at a width boundary or beside the surrogate range, read by a `Parse(Str, A)` primitive, gives the character Rust decodes.
- `Flt/of_str` and `Flt/of_hex_str` agree with Rust's reading on the rounding tests' corpus, and `Flt/of_str` still closes a claim in a type.
- `wonder stage ersd` of a text parser shows its positions as offsets and its proofs erased.
- The prelude build is measured before and after each stage, naming the stage.

## Rejected

- **A bytes parser that checks its text at the end.** It proves nothing, scans every string twice, and keeps a refusal its `Str` input cannot reach.
- **Handing a format's number text to `Flt/of_str`.** It reads the format by `Flt`'s grammar, which is how `Json` came to accept `01`.
- **Reading an HTTP head as text.** A header may carry obsolete octets outside UTF-8, and the head's framing is bytes; text is what a body declared to be text is.
- **Clamping a short body as the operation's contract.** `Parse/take`'s "everything that remains" is a fine answer for a parser primitive and the wrong one for a framed message, where the length is a promise the peer broke.

## Completion criteria

- No `/std` declaration answers a value it made up where a proof or a refusal belongs: every remaining default is a specified answer, and its documentation says which.
- `Str/of_bytes` appears only where bytes enter, and every text parser carries its input's validity from end to end.
- Each format reads numbers by its own grammar, through one conversion per type, and HTTP frames a message by RFC 9112 in both directions.

## Retirement

Move the contracts to the module documentation of `Json`, `Toml`, `http`, `Html` and `Flt`, and extend the README's decision with what this part settles. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
