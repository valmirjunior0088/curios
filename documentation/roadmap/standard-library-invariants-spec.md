# Explicit invariants in the standard library

Working specification for removing every value `/std` makes up where a proof belongs. A decoded code point that is not a scalar value becomes `'?'`, re-encoded text that is not UTF-8 becomes `""`, and a list index that is out of range reads a default element. Each such branch is unreachable for a reason the code knows and the types do not state, so the fallback is a guarantee nothing checks, and a defect in the invariant it covers would surface as a plausible wrong value rather than a refusal. Most of it is text: `Char` and `Str` prove what they carry, but decoding, encoding, cutting and every text parser lose that proof at the byte level and restore it with a default, a second scan, or a walk that counts characters from the start.

## What this builds on

- `Char` carries `Char/Valid(code)` and `Str` carries `Str/Valid(bytes)`, both decided propositions ([A bound is stated in a decided proposition and discharged by reduction](../design/language/a-bound-is-stated-in-a-decided-proposition-and-discharged-by-reduction.md)). `Str/Valid/from(s, b)` is validity from any scan state, walked byte by byte.
- `Bytes` are ropes: `slice` is an O(1) view and concatenation is an O(1) node (`curios-emit/src/into_wasm/types.rs`), so a piece of a string is a string, with no copy.
- Conversion decides `Nat`'s semiring laws, Euclid's identity and the remainder's bound, the cancellation of common summands, and the cons peels of `get` and `slice` ([Open fold laws and the sum normal form](../soundness/per-term-rules/open-fold-laws-and-the-sum-normal-form.md)): `Char/Valid(n % 0x10 + 0x30)` holds by `Bool/True/qed()`, and `n / 0x40 * 0x40 + n % 0x40` converts with `n`.
- A guard on a single comparison is in scope in its arm, inhabited by `Bool/True/qed()`. Conversion does not consult a hypothesis, so a bound a guard established reaches a goal through the `Nat/Lt` and `Nat/Le` lemmas.
- A structure may be indexed by a proposition-valued family, and `Monad` resolves through a partially applied one, `(A: Type) => P(At, A)`, behind a type alias too, with `!` sequencing across it.
- `Toml` already reads numbers by its own grammar and converts through `Flt/of_decimal`.
- `curios-parse`, the Rust engine behind the surface grammar, holds a byte offset beside the remaining `&str`, scans by `char`, and reports the further of two uncommitted failures: the same theory, with the boundary enforced by Rust's slicing rather than proved.

## The gap

**Decoding.** `Str/certify_decoded` turns a decoded code point into a `Char` through `Char/of_nat` and answers `'?'` where that fails. `decode_head` and `fold` both go through it, and `fold`'s walk does not carry the string's validity, so its `bad` arms are written as if they could run.

**Encoding.** `Str/of_char` answers `""` when `Char/to_utf8(c)` is not UTF-8. `to_utf8` masks each lead byte into range, and nothing states that the bytes it emits are well-formed.

**`Char`.** `to_ascii_lower` and `to_ascii_upper` answer the character unchanged when `of_nat` fails, and `hex_digit` answers `'0'`, though each call is already guarded or masked into range.

**`Str` cuts by counting characters.** `slice`, `get`, `try_get`, `index_of`, `find_index`, `trim`, `split_once`, `split`, `lines`, `starts_with`, `ends_with` and `replace` address text by character index, and cut through `take_n` and `drop_n`, which walk from the start counting characters with `drop_width`. A character count was the one cut that carried its proof: a byte-offset cursor was built in June and dropped when eliminating the then-inductive validity proof into data was guarded, and validity became decided only later. That reason is outgrown. Every cut now re-walks what the walk that found it crossed, and every consumer's index — a sign check, a prefix strip, `Tui/Field`'s cursor — is a position spelled as a count. `Tui/Field`'s `place` answers the end of the text for an out-of-range cursor, which an insert or a backspace never produces.

**Text parsers drop the proof on entry.** `Html/parse`, `Toml/decode` and `http/Url/parse` receive a `Str`, parse its bytes, and build text back from byte slices, either with a runtime check whose refusal cannot fire on input that began as a `Str` (`Json/decode.crs:97`, `Toml/strings.crs:238`, `Toml/keys.crs:10`, `http/Url.crs:39`) or with `""` (`Html/parse.crs:18`, `Json/decode.crs:25`). An escape decodes through `Char/to_utf8` into `Bytes`, and the whole string is scanned again at its end.

**`Parse` itself.** A position is a bare `Nat`, so `take_while` and `rest` refuse with "position past the end of input", which no parser can reach, and `take_literal` reads the literal's bytes through `Option/unwrap_or(…, 0)`. `or` reports the second alternative's refusal when neither committed, whatever the first one reached.

**Numbers read by another format's grammar.** `Json/decode` hands a run of number bytes to `Flt/of_str`, whose syntax is `Flt`'s, so `01` and `1.` decode, though RFC 8259 refuses both.

**Encoders.** `Json/encode` and `Toml/encode` escape bytewise, re-read the result with `Str/of_bytes` defaulting to `""`, and write hex digits through `Char/to_ascii(Char/hex_digit(…))` defaulting to byte `0`. `http/Url` has a second hex-digit function defaulting to `'0'`, and turns an unreserved byte into text through `Str/of_bytes` defaulting to `""`. `Show(Bytes)` re-reads its own hex through `Str/of_bytes`, defaulting to `""`.

**Indices known to be in range.** The three `join`s (`Str`, `Bytes`, `Bits`) read `List/try_get(parts, i)` with a default, because `List/balanced` never hands `single` the `i < n` it establishes. `Nat`'s `single_digit` has a `_ => "0"` arm no caller reaches. `http/Response`'s `hex_number` re-reads digits `take_while(is_hex)` already accepted, defaulting to `0`. `Path/name` takes `List/last` of a list that is never empty.

**Promises the types do not state.** `Cli`'s `name_at`, `wants_a_value` and `fill` index `spec` and `slots`, which have the same length only by construction. `Flt/to_str` reads eight bytes of `to_le_bytes`, whose length is not in the intrinsic's signature. `Io/read_line`, `Async/read_until` and `Async/read_line` assume a host read of one byte is not empty; `Async`'s `poll_ready` assumes the poll's results line up with its waiters; and `tcp/resolve` answers the address `x[]` when a resolution that succeeded carries none.

## Permanent decisions

**No value is invented where a proof belongs.** Where a branch cannot be reached, its unreachability is proved and the branch is gone. Where it can be reached, the answer is an `Option`, a `Result` or a refusal the caller sees. A default stays only where it is the specified answer — an absent header, an IEEE substitution, a terminal's default parameter — and its documentation says so.

**Text is addressed by position.** A position is `Str/At(s)`: a byte offset into `s` with an erased proof that it is a boundary, typed by the string it belongs to. A boundary is a one-byte fact — the offset is the end, or its byte is not a continuation byte — decided, so a literal's positions discharge by reduction and an arbitrary offset is checked in O(1). UTF-8's self-synchronization, proved once, gives the one-byte fact its meaning: in valid bytes, the scan of the bytes before a boundary lands between characters. A piece of a string is a string, cut between two positions in O(1). Characters are counted only where the count is the meaning.

**The carrier vocabulary holds at `Str`.** `len` counts what `fold` visits; `find` answers the element; `find_index` and `index_of` answer where, which at `Str` is an `At(s)`; `contains` answers whether. A search starts at the beginning of the string it is given, and searching after a position is searching the piece after it.

**One parser, indexed by what its positions know.** `Parse` takes a proposition over the input and a position, and every position it produces carries it, erased. Over bytes it is the bound; over text it is `Str/At`'s. The combinators are written once and the primitives are each carrier's own. `Parse(A)` names the bytes parser and `Parse/Text(A)` the text parser. When neither of two alternatives committed, the refusal that reached further is reported.

**Text APIs speak characters; implementations may skip bytes.** A text primitive consumes and answers characters and strings. Its implementation may scan bytes, stopping only where the one-byte rule makes the stop a position.

**Validity is checked once, where bytes enter.** A text format that arrives as bytes, such as a JSON body, is read with `Str/of_bytes` at the edge, and invalid UTF-8 is that format's refusal there. Nothing downstream checks again. An HTTP head is octets, not text, and stays a bytes parse.

**One conversion per number type, one grammar per syntax.** A format reads its own number syntax and hands the parts to its target type's conversion — `Flt/of_decimal` and its hexadecimal twin. `Flt/of_str` is `Flt`'s own grammar run to the end.

**A bound is passed, not masked.** A value built under a bound it is proved to satisfy is built from the proof. A mask stays only where it selects bits.

## Stages

Each lands alone, in this order where a stage needs an earlier one.

1. **Lemmas.** `Char/Valid` introduced from `code < 0xD800` and from `0xE000 ≤ code ≤ 0x10FFFF`. `Nat/in_range` eliminated to its two `Le`. Quotient bounds in `Nat/div_mod`: `m · d ≤ n` gives `m ≤ n / d`, and `n < m · d` gives `n / d < m`. The bound on a digit `x − lo` from `lo ≤ x ≤ hi`. Equality reflection for `Nat` and `Byte`: `a == b` gives `Eq(a, b)`, as `Int/eq_of_eql` gives it for `Int`. In `Bytes`, `slice(b, s, m + n)` as `slice(b, s, m) ++ slice(b, s + m, n)`, by induction over `b` on the peels conversion already takes.
2. **`Char`.** `hex_digit`, `to_ascii_lower` and `to_ascii_upper` construct their `Char` with its proof.
3. **Decoding.** `Pending(rem, lo, hi, partial)` is decided: every way the scan could still finish this character is a scalar value. Since those finishes form one contiguous range, that means the range lies below `0xD800` or within `0xE000..=0x10FFFF`. It is established at a lead byte, family by family — `ED`'s `9F` excludes the surrogates, `F0`'s `90` lifts a four-byte value past `0xFFFF`, and `F4`'s `8F` caps it at `U+10FFFF` — preserved by a continuation byte in `[lo, hi]`, and, with no bytes remaining, is `Char/Valid`. One step, `partial · 0x40 + (byte − 0x80)`, is shared by two walkers: `decode_head`, rewritten as a structural walk over its evidence, and `fold`, which keeps its accumulator, carries `Pending` erased, and threads the string's validity. `certify_decoded`, `take_continuations` and `cont_len` are deleted.
4. **Encoding.** `to_utf8` builds each lead byte from the range its guard and `c.valid` establish. `Str/Valid` proves its output well-formed, with one lemma per UTF-8 family stating that a byte in the family's range classifies as that family. `of_char` builds its `Str` directly.
5. **`Str/At`.** Self-synchronization, through an invariant on the scan states reachable from `lead` in valid bytes: each is `lead` or `cont(k, lo, hi)` with `1 ≤ k ≤ 3` and `0x80 ≤ lo ≤ hi ≤ 0xBF`, so a byte that is not a continuation byte meets a `cont` state only by going `bad`, which valid bytes never reach. Before a boundary, a run of continuation bytes is at most three long and follows a lead byte. From these: `At/start`, `At/end`, `At/of_offset` and `At/try_of_offset`, `At/to_offset`, `At/next` and `At/prev` (the character on either side and the position past it), the order on positions, and positions under concatenation. `Str/before`, `Str/after` and `Str/between` cut in O(1). `find`, `find_index`, `index_of`, `contains` and `index_of_substr` search — a substring by skipping to its first byte, which the one-byte rule makes a position, and confirming by stepping both strings. `split`, `split_once`, `lines`, `trim`, `trim_start`, `trim_end`, `starts_with`, `ends_with`, `strip_prefix`, `strip_suffix`, `has_substr` and `replace` are each one walk. `slice`, `get`, `try_get`, `take_n`, `drop_n`, `drop_width`, `drop_width_within`, `Valid/drop` and `Valid/take` are deleted. Every consumer moves in the same stage: `Cli`, `Int`, `Flt`'s readers until stage 8, `Tui/Field`'s cursor to an `At(text)` stepped by `next` and `prev`, `Tui/input`, the sign strips in `Json/encode` and `Toml/encode`, `Html/parse`, `http/Client`, and `Toml/encode`'s fractional-second trim.
6. **`Parse`.** The indexed type; its generic combinators (`pure`, `fail`, `map`, `bind`, `or`, `commit`, `label`, `optional`, `and`, `lookahead`, `many0`, `sep_by0`, `eof`), with `or` reporting the further uncommitted refusal; the bytes primitives under the bound; and the text primitives over `Str/At`: `char`, `char_if`, `expect`, `literal`, `take_while` over `Char` answering a `Str`, `peek` and `rest`. Refusals keep byte offsets.
7. **Text consumers.** `Json/decode`, `Toml`, `http/Url` and `Html/parse` read text: string bodies are pieces of their input, and escapes are characters. `Response/json` checks its body once. `http/Request` and `http/Response` stay bytes parsers under the bound.
8. **Numbers.** `Flt`'s readers — decimal, hexadecimal, the specials and a NaN's payload — become `Parse/Text` grammars over `of_decimal` and its hexadecimal twin, and `Flt/of_str` and `Flt/of_hex_str` run them to the end. `Json/decode` reads RFC 8259's number grammar and converts through `of_decimal`, as `Toml` does, so `01` and `1.` are refused.
9. **Encoders.** One walk in `Str`, which copies unchanged runs as pieces and replaces chosen characters with a `Str`, serves `Json/encode` and `Toml/encode`; a hex escape is `Str/of_char(Char/hex_digit(…))`. `http/Url` spells its hex digits through `Char` and its unreserved bytes as the characters they are, and `Show(Bytes)` is built from `Str`.
10. **Indices.** `List/balanced` hands `single` its `i < n`, and the `join`s read `List/get` under it. `single_digit` is the digit of its argument modulo ten. `hex_number` keeps each digit's value as it reads it. `Path`'s components answer their last one separately.
11. **Declared lengths and aligned lists.** `Cli`'s specification and slots travel as one list. `to_le_bytes` states its length of eight in its signature: `Intrinsic::signature`, and the `/sys` declaration that restates it.
12. **Host promises.** For each promise above, both hosts' behavior is established first. Where both keep it, the ABI row states it and the native and JavaScript implementations are held to it; where a host can answer otherwise, `/std` handles that answer instead of assuming it away. `tcp/resolve` goes first, because it answers a value that means something else.

## Design decisions this overturns or corrects

Each is revised in the change that makes it true.

- `Char/to_utf8`'s documentation argues for masks — "in range on its face rather than because a guard three lines up says so". The proof replaces that argument, and the unguarded fourth arm's width comes from `c.valid`.
- The module documentation of `Str`, `Str/Valid`, `Char`, `Flt`'s readers, `Parse` and `Parse/Error`.
- `curios/src/tests/strings/utf8_tests.rs` pins `Str/take_continuations` and `Str/Valid/cont_len` by name, and pins the character-count proof of `slice` in `slice_proof_aligns_with_byte_walk` and `slice_closed_peels_codepoints`; `curios/src/tests/matching/refinement_tests.rs` cites `cont_len` as production's example of a refinement on a concept dispatch. Each changes with the stage that removes its subject.
- `curios/src/tests/numeric/bound_tests.rs`, `curios/src/tests/soundness/totality_tests.rs` and `corpus/strings/str.crs` use `Str/get` as an example of a bounded accessor. They test the bound, not `Str`, and move to `List/get` or `Bytes/get`.

## Rejected

- **Character indices as the public currency**, as Haskell's `Text` has them. Every access is O(i), and a loop over indices is quadratic.
- **Unproven byte offsets.** Go has no boundary at all; Rust panics on a slice off a boundary; Lean 4's `String.get` answers a default character there — the invented value this specification removes.
- **A separate substring type**, as Rust, Swift and Lean have. `Str`'s ropes already make a piece a view.
- **Positions not tied to their string by type**, which leaves using one string's position on another to a runtime check or to nothing.
- **"The scan lands between characters" as the stored invariant.** It can't be checked in O(1) and gives no `prev`; the one-byte rule with self-synchronization proved gives both.
- **Character and position APIs side by side**: two spellings of one operation.
- **Searches taking a starting position.** A piece is O(1), so searching after a position is searching the piece, and the signatures stay those of every other carrier.
- **A reflection law for `Bytes/eql`.** It does not reduce over a list, so a law would be a new intrinsic law — the [algebra part 2](algebra-pt2-spec.md)'s to declare if speed ever asks. Stepping confirms a match without one.
- **A bytes parser that checks its text at the end.** It proves nothing, scans every string twice, and keeps a refusal its `Str` input cannot reach.
- **Two parser types, one per carrier.** The combinators' commitment and progress rules are subtle, and two copies would drift.
- **Keeping `to_utf8`'s masks** and proving each one changes nothing under its guard: a second proof for an operation that then does nothing.
- **Decoding in `fold` through `decode_head` at each lead byte.** It is simpler to prove, but it changes the walk that the string ladder in `curios/src/tests/codegen/ladder.rs` measures.
- **Handing a format's number text to `Flt/of_str`.** It reads the format by `Flt`'s grammar, which is how `Json` came to accept `01`.

## Non-goals

Grapheme clusters, case mapping beyond ASCII and normalization, which need Unicode's tables. Line and column positions in parse refusals, which a byte offset and its input determine, so they can be derived later without changing `Parse/Error`.

## Verification

- Every lemma is a declaration that `cargo x clippy` elaborates and certifies with the rest of `/std`.
- The existing runtime tests pin behavior across the change: `char_to_utf8_matches_rust_across_widths_and_boundaries` for encoding at every width boundary; `corpus/strings/decomposition.crs`, `str.crs`, `utf8.crs`, `json.crs`, `html.crs`, `data/http.crs` and `curios/src/tests/toml.rs` for `Str` and the consumers.
- New tests:
  - Every scalar value at a width boundary or next to the surrogate range, read through `fold`, `At/next`, `At/prev` and `Parse/Text`, gives the character Rust decodes.
  - `At/try_of_offset` agrees with Rust's `is_char_boundary` at every offset of a mixed-width string.
  - `next` then `prev`, and `prev` then `next`, return to their position.
  - A JSON body that is not UTF-8 is refused at `Response/json`.
  - RFC 8259's number grammar is held at its edges: `0`, `-0`, `1E+2` and `0.5` read; `01`, `1.`, `.5`, `+1` and `1e` are refused.
  - Each index site is exercised at its first and last element.
- `wonder stage ersd` of `Str/fold`, of an `At/next` walk and of a text parser shows the proofs erased: `fold`'s runtime shape is today's, and a position is an offset.
- `str_literal_cost_measurements` still shows a cut literal at the bare literal's cost. Retake it with `cargo test --release --package curios -- --ignored --nocapture str_literal_cost_measurements`.
- The prelude build is measured before stage 1 and after each stage, naming the stage. `cargo build --package curios-prelude-archive --features profile` files `curios-prelude-archive/.artifacts/profile.tsv`, and `cargo run --package curios --features profile -- profile curios-prelude-archive/.artifacts/profile.tsv` folds it.

## Completion criteria

- No `/std` declaration answers a value it made up where a proof belongs: every remaining `unwrap_or`, defaulted `none()` arm and catch-all arm is a specified answer, and its documentation says which.
- `Str` addresses text by `Str/At` alone, and `Char`, `Str` and every text parser carry their proofs from end to end; `Str/of_bytes` appears only where bytes enter.
- Every `Parse` position carries its bound or its boundary, and no refusal in `Parse` is unreachable.
- Each format reads numbers by its own grammar, through one conversion per type.
- Before this specification is deleted, the decisions and their rejected alternatives are a design decision in `curios-prelude-archive/README.md`, the contracts are in the documentation of `Str`, `Char`, `Parse` and `Flt`, the roadmap entry is a checked summary, and no reference to this filename remains.
