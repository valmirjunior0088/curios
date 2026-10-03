# HTTP messages as RFC 9110 and RFC 9112 frame them

Working specification for the HTTP messages `/std` reads and writes other than as RFC 9110 and RFC 9112 frame them, and for the head's opaque octets it refuses where they are data. The rule is [`curios-prelude-archive`'s README](../../../curios-text/README.md#std-invents-no-value-where-a-proof-belongs-and-text-is-addressed-by-position)'s: where a branch cannot be reached its unreachability is proved, where it can the caller sees an `Option`, a `Result` or a refusal, and a default stays only where it is the specified answer, documented as such.

It is independent of every other spec. [Text read as text](02-text-read-as-text.md) leaves `http/Request` and `http/Response` reading `Parse(Bytes, A)`: an HTTP head is octets, not text.

## What this builds on

- **A refused request is answered.** `http/Server`'s `answer` sends a 400 for any request `Request/decode` refuses (`http/Server.crs:97`).
- **The mock host drives both ends of HTTP.** Scripted inbound connections, whole or in segments (`inbound_chunks`), drive `serve` (`curios/src/tests/host/net_tests.rs`'s `serve_handles_a_scripted_inbound_connection`), a scripted reply drives `http/perform` (`http_perform_parses_a_scripted_response`), and `test_support`'s `Lying` host fails one host call on demand.

## The gap

**HTTP reads as values what it says to refuse, and writes what it says not to send.** Read from the decoders, the server's read loop and the renderers, against RFC 9112 — a bare § is its — and RFC 9110. The server's read loop counts 1 024 reads rather than bytes (`http/Server.crs:73`), and finds the head's end with `blank_line`, which counts down a fuel where the bytes themselves would descend. `Request` and `Response` each define `lookup`, `render_header` and `until`, identically.

A request, as `Request/decode` and `http/Server` read it:

| Input | The library | The RFCs |
| --- | --- | --- |
| `Content-Length` not a number | no body (`body_of`'s `none()` arm, `http/Request.crs:119`); the server's `needed` reads it as `0` (`http/Server.crs:44`) | an unrecoverable framing error, answered 400 (§6.3 rule 5) |
| Two `Content-Length` values that differ, on two lines or in one list | the first line's; a list is not a number, so no body | 400 (§6.3 rule 5) |
| A body shorter than its `Content-Length` | the bytes that arrived, as the whole body: `Parse/bytes/take` clamps (`Parse/bytes.crs:138`), and the read loop hands on what arrived when the peer closes | incomplete; a server may answer 400 before closing, and does not hand it on as complete (§8) |
| `Transfer-Encoding: chunked` | no body: the field is not read | the chunked body decoded, overriding any `Content-Length` (§6.3 rules 3 and 4) |
| A transfer coding other than `chunked` | no body | 501 (§6.1) |
| `chunked` other than last | no body | 400 (§6.1) |
| `Transfer-Encoding` in an HTTP/1.0 request | read as in HTTP/1.1: the version is discarded (`http/Request.crs:86`) | faulty framing, 400 (§6.1) |
| HTTP/1.1 without `Host` | addressed to `"localhost"` (`http/Request.crs:126`) | 400 (§3.2) |
| HTTP/1.0 without `Host` | addressed to `"localhost"` | refused, or a configured default applied (§3.3) |
| Two `Host` lines | the first | 400 (§3.2) |
| An absolute-form target, `GET http://a/b HTTP/1.1` | `http://`, the `Host` value and the target spliced into one URL (`url_of`, `http/Request.crs:125`) | the target's authority, `Host` ignored (§3.2.2) |
| Whitespace between a field name and its colon | a name ending in whitespace, so a spaced `Content-Length :` is not found and the request reads as bodiless | 400 (§5.1) |
| A folded field line | read on into the next line, up to a colon, as a field name | 400, or each fold read as a space (§5.2) |
| LF or NUL inside a field value | kept in the value | refused, or each replaced by a space (RFC 9110 §5.5) |
| A field value holding octets past ASCII | 400: a field value is read as `Str` (`text`, `http/Request.crs:75`), so obs-text that is not UTF-8 refuses the request | opaque data (RFC 9110 §5.5) |
| A field name that is not a token | read as a name | 400 (RFC 9110 §5.1, §15.5.1) |
| An unrecognized method | 400 | 501 (RFC 9110 §9.1) |
| A malformed version | accepted: it is not read | 400 (§2.3, §3.2) |
| A major version other than 1 | accepted: it is not read | 505 (RFC 9110 §15.6.6) |
| A request past the read limit | what arrived within 1 024 reads, as the whole request | a header section too large, 431 (RFC 6585 §5); content too large, 413 (RFC 9110 §15.5.14) |

A request without `Content-Length` or `Transfer-Encoding` has no body (§6.3 rule 7), which is specified.

A reply, as `Response/decode` and `http/Client` read it:

| Input | The library | The RFCs |
| --- | --- | --- |
| `Content-Length` not a number, or two values that differ | everything the peer sent, or the first line's value | an unrecoverable error: the reply discarded (§6.3 rule 5) |
| Shorter than its `Content-Length` | the bytes that arrived, documented as clamped | recorded as incomplete (§8) |
| Chunked, ending before its last chunk | refused as malformed, by whichever primitive met the end | recorded as incomplete (§8) |
| A last coding other than `chunked` | decoded as chunked, since `chunked` is looked for anywhere in the list (`http/Response.crs:120`) | framed by the connection's close (§6.3 rule 4) |
| Any coding besides `chunked` | the body returned still coded | unacceptable: the client sends no `TE`, which leaves `chunked` the only acceptable coding (§7.4) |
| A reply to `HEAD`, or a 1xx, 204 or 304 reply, with a nonzero `Content-Length` | no body, by the accident that `take` clamps to what arrived | no body, whatever the fields say (§6.3 rule 1) |
| A 1xx reply ahead of the final one | taken as the reply | skipped (RFC 9110 §15.2) |
| A folded field line | read on into the next line | each fold read as a space (§5.2) |
| LF or NUL inside a field value | kept | refused, or each replaced by a space (RFC 9110 §5.5) |
| A field value or reason phrase holding octets past ASCII | refused as malformed: the head is read as `Str` (`as_text`, `http/Response.crs:33`) | opaque data (RFC 9110 §5.5), and a reason phrase a client ignores (§4) |
| A field name that is not a token | read as a name | malformed (RFC 9110 §5.1) |

The `HEAD` row matters beyond itself: refusing a short body, as the second row asks, would refuse every well-formed reply to `HEAD` that states its length, so the reply's reader must know the request's method.

A message, as `Request/render`, `Response/render` and the server write it:

| Message | The library | The RFCs |
| --- | --- | --- |
| A reply to `HEAD` | the handler's body sent | no content (RFC 9110 §9.3.2); `Content-Length` may state what a `GET` would carry (RFC 9110 §8.6) |
| A 1xx, 204 or 304 reply | `Content-Length` always written (`http/Response.crs:149`) | none on 1xx or 204, and none on 304 unless it states the 200's length (RFC 9110 §8.6) |
| A body on a 1xx, 204 or 304 reply | written after the head | none (§6.3 rule 1) |
| An empty `POST`, `PUT` or `PATCH` | no `Content-Length` | `Content-Length: 0` (RFC 9110 §8.6) |
| A `Content-Length` or `Transfer-Encoding` among the caller's fields | written beside the rendered one | one field line per field, and never both fields (RFC 9110 §5.3, §6.2) |

## Prior art

- **HTTP stacks.** Go's `net/http` refuses a missing `Host` at HTTP/1.1 and later only, answers an unsupported transfer coding with 501 and every other framing error with 400, and discards what a handler writes in reply to `HEAD` ([`server.go`](https://github.com/golang/go/blob/master/src/net/http/server.go)); it accepts repeated `Content-Length` values only when they are identical, understands `chunked` alone, treats `Transfer-Encoding` in HTTP/1.0 as faulty framing, gives a request with neither field no body, and answers a short body with `io.ErrUnexpectedEOF` ([`transfer.go`](https://github.com/golang/go/blob/master/src/net/http/transfer.go)). Python's `http.client` raises `IncompleteRead(partial, expected)`, carrying what arrived, reads no body for `HEAD`, 1xx, 204 and 304, and skips `100 Continue` ahead of the final reply ([`client.py`](https://github.com/python/cpython/blob/main/Lib/http/client.py)). hyper reports an `IncompleteMessage`, "connection closed before message completed", as a kind of its own ([hyper#3659](https://github.com/hyperium/hyper/issues/3659)), and libcurl `CURLE_PARTIAL_FILE` ([error codes](https://curl.se/libcurl/c/libcurl-errors.html)). picohttpparser's `phr_decode_chunked` decodes chunks in place as they arrive, answering −2, "incomplete", until the last one ([`picohttpparser.h`](https://github.com/h2o/picohttpparser/blob/master/picohttpparser.h)); nom's streaming parsers answer `Err::Incomplete(Needed)` beside `Error` and `Failure`, which is the whole-message alternative ([`internal.rs`](https://github.com/rust-bakery/nom/blob/main/src/internal.rs)). A field value is octets: hyper's `HeaderValue` holds bytes and yields a `&str` only when they are visible ASCII ([`value.rs`](https://github.com/hyperium/http/blob/master/src/header/value.rs)), Go's `ValidHeaderFieldValue` admits obs-text ([`httplex.go`](https://github.com/golang/net/blob/master/http/httpguts/httplex.go)), and Fetch defines a header value as a byte sequence ([Fetch](https://fetch.spec.whatwg.org/#concept-header-value)); Python's `http.client` alone decodes the head as ISO-8859-1 ([`client.py`](https://github.com/python/cpython/blob/main/Lib/http/client.py)).

## Decisions

Taken before stage 1, each with its reason, so a stage meets none of them as a fork.

1. **The server reads a request as head, framing and body.** It reads until the head's blank line has arrived and decodes the head, and the framing decides how the body is read: a length is counted, and a chunked body is read a piece at a time — a size line once its line end has arrived, a chunk once its size and its line end have, the trailer section once its blank line has — each piece parsed once, the reading resuming after the last whole chunk. `read_request` answers `Result(Response, Request)`, whose failure is the reply the server owes.
2. **One reading of framing, for both directions.** One function reads a message's framing from its start line and fields by §6.3's rules in order — no body, a length, chunked, or for a reply the connection's close — or says why it cannot: unframeable, which a request answers 400, or a coding not understood, which a request answers 501. A reply that cannot be framed is `http/Error/malformed`. The chunk grammar `Response` has serves both directions, and so does decision 1's piece-by-piece reading. `Transfer-Encoding` beside `Content-Length` is read by the former alone, which §6.1 lets a server choose; a `Content-Length` list of one repeated value is that value, which RFC 9110 §8.6 lets a recipient choose; and the codings must be exactly `chunked`.
3. **A request's size is bounded in bytes.** Its head within 64 KiB, answered 431 past it, and its content within 4 MiB, today's ceiling of 1 024 reads of 4 KiB, answered 413 past it. A peer that closes before its request is whole is answered 400, as §8 allows. `http/Server`'s documentation states both limits.
4. **A short reply is `http/Error/incomplete(Response)`.** A reply shorter than its `Content-Length`, or chunked without its last chunk, raises the reply as far as it arrived — its status, its fields and the body received — as Python's `IncompleteRead` carries its `partial`. `Response/decode` takes the request's method, so a reply to `HEAD` has no body, and answers the reply with whether it arrived whole; the client raises `incomplete` for one that did not, and skips a 1xx reply ahead of the final one.
5. **A request with no authority is refused.** No `Host`, or two, is 400 at HTTP/1.0 as at 1.1: §3.3 lets a server refuse an empty authority, the alternative is the server's configured name, and a server bound to a wildcard address has none to offer. An absolute-form target's authority is the request's, and `Host` is ignored (§3.2.2).
6. **A field line is refused, not repaired.** Whitespace before the colon, a fold, and an LF or NUL in a value refuse a request with 400; RFC 9110 §5.5 and §5.2 let a recipient replace them with spaces, which makes a value up. A reply's fold is read as a space, as §5.2 requires of a user agent, and a reply's LF or NUL refuses it.
7. **The renderer owns framing.** `Request/render` and `Response/render` write `Content-Length` and `Transfer-Encoding` from the body, and a caller's own field of either name is not sent, which `with_header` and both renderers document. A reply to `HEAD` is sent as its head alone, its `Content-Length` the length of the body the handler produced. A 1xx, 204 or 304 reply carries no `Content-Length`, and a handler that gives one a body is answered 500 in its place. `POST`, `PUT` and `PATCH` always state their length.
8. **A head's opaque octets are bytes.** Field values and a reply's reason phrase are `Bytes`, the octets the wire carried, as RFC 9110 §5.5 asks a recipient to treat obs-text: `Request/header` and `Response/header` answer `Option(Bytes)`, `with_header` and the reply makers take a value as bytes, and a caller wanting text reads it through `Str/of_bytes`. Field names stay `Str`, since a name is a token (RFC 9110 §5.1), and one that is not refuses a request with 400 and a reply as malformed.

## Stages

Each lands alone.

1. **HTTP reads a message as RFC 9112 frames it.** The request and reply tables' rows take the RFCs' answers under decisions 1 to 6 and 8, and `Request` and `Response` hold their field values and reason phrase as bytes, with the makers and readers that build and read them. `needed` and `blank_line` go with the read loop's count of reads, `lookup`, `render_header` and `until` are written once for both messages, `http/Error` gains `incomplete`, `Response/reason_of` names 413, 431, 501 and 505, and `body_of`'s, `Request/decode`'s and `Response/decode`'s documentation says which answers are specified.
2. **HTTP writes a message as RFC 9110 and RFC 9112 frame it.** The writing table's rows, under decision 7.

## Verification

- Stage 1: each row of the request and reply tables, read by `Request/decode` or `Response/decode`, gives the answer the decisions take. A server over the mock host answers each request row with its status — 400, 413, 431, 501 and 505 — and reads whole a chunked request sent in segments cut inside a size line, inside a chunk and inside the trailer section. `http/perform` over a scripted reply raises `incomplete` with the part that arrived, reads a reply to `HEAD` that states a length as whole, and skips a `100 Continue`. A request and a reply whose field value holds octets past ASCII read, the value the octets sent, as does a reply whose reason phrase holds them; a field name that is not a token is answered 400. A well-formed request and reply decode as before (`curios/src/tests/corpus/data/http.crs`).
- Stage 2: each row of the writing table, read from `Request/render`, `Response/render` and the bytes a server over the mock host sends; `Request/render` then `Request/decode` round-trips.
- Every stage: the prelude build is measured before and after, naming the stage, its rows read from `curios-prelude-archive/.artifacts/profile.tsv` (elaboration) and `curios-prelude/.artifacts/profile.tsv` (certification) after `cargo xtask clippy`, folded by `target/debug/curios profile <file>`, whose first data row is the total.

## Rejected

- **Reading an HTTP head as text.** A header may carry obsolete octets outside UTF-8, and the head's framing is bytes; text is what a body declared to be text is.
- **Clamping a short body as the operation's contract.** `Parse/take`'s "everything that remains" is a fine answer for a parser primitive and the wrong one for a framed message, where the length is a promise the peer broke.
- **Reading the whole request again until a refusal says the input ended**, as nom's `Incomplete` would have it. Every primitive would have to mark the end of its input, and each read would parse the request again from its start, quadratic up to the size limit.
- **The server's bound name for a missing `Host`**, §3.3's configured default. A server bound to a wildcard address has no name to give, and `"localhost"` is the invented value this spec removes.
- **A short reply as `malformed`, or as an `incomplete` with nothing attached.** The first is told apart from a garbled reply only by its message; the second discards what arrived, as Go, hyper and curl do and Python does not.
- **Replacing a field's LF or NUL, or a request's fold, with spaces.** RFC 9110 §5.5 and §5.2 allow it, and it makes a value up.
- **Reading a head's octets past ASCII as ISO-8859-1 into `Str`**, as Python does. It never refuses and loses nothing, but it reads a UTF-8 value as other characters, `é` as `Ã©`, which is invented text. **A field-value type of its own**, as hyper's `HeaderValue` is: `Bytes` and `Str/of_bytes` already say what it would.

## Completion criteria

- HTTP frames a message by RFC 9110 and RFC 9112 in both directions, reading and writing, and holds a head's opaque octets as octets.
- No HTTP declaration answers a value it made up where a refusal belongs: every remaining default is a specified answer, and its documentation says which.

## Retirement

Move the contracts to `http`'s module documentation, and extend the README's decision with what this spec settles. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
