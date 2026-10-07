# HTTP messages as RFC 9110 and RFC 9112 frame them

Working specification for the HTTP messages `/std` reads and writes other than as RFC 9110 and RFC 9112 frame them, for the messages it lets a program make that no wire can carry, and for the connection it closes under the answer it sent. The rule is [`curios-std`'s README](../../../curios-std/README.md#std-invents-no-value-where-a-proof-belongs-and-text-is-addressed-by-position)'s: where a branch cannot be reached its unreachability is proved, where it can the caller sees an `Option`, a `Result` or a refusal, and a default stays only where it is the specified answer, documented as such.

It is independent of every other spec. [Text read as text](02-text-read-as-text.md) leaves a head read as octets: an HTTP head is octets, not text.

## What this builds on

- **A hidden field carries a decided proposition.** A structure's `@Holds(…)` field is filled by reduction where the value is a literal and by the guard of the arm it is built in otherwise, is a fact wherever the value is in scope, and takes no part in a derived `Eql` or `Spell`, as `Char`'s `@valid` shows (`Char.crs`). A literal that fails it is refused where it is written, by the proposition it states.
- **Reading is generic over the monad it happens in.** `Async/drain`, `read_until` and `read_line` take their `Monad(M)` and a read answering `M(Io/Chunk)` (`Async.crs`), so one reader runs over a socket in `Async` and over a list of segments in `State`.
- **`Parse(Bytes, A)` reads a piece that has arrived whole**, with its refusal's offset (`Parse.crs`, `Parse/bytes.crs`).
- **A stream's sending half can be ended.** `Handle/finish` ends it and leaves the receiving half as it is — a socket's write side shut down, a TLS stream's closure alert ahead of it — answering `would_block` as `Handle/flush` does and parked on the same way (`Handle.crs`; [Only a fiber waits](../../design/standard-library/only-a-fiber-waits.md)). `tcp/Socket/with` and `tcp/Listener/serve` call it before they close, and a peer that resets is `Io/Error/reset()`.
- **The hosts are scriptable at both ends.** The mock host serves inbound connections whole or in segments (`inbound_chunks`) and outbound replies (`net`, `net_chunks`), a peer that keeps its sending half open until the server finishes its own (`inbound_waiting`) and one that resets (`net_reset`), and records each connection's writes, end, finish and close in order (`socket_events`); one compiled program runs under many of them (`curios/src/tests.rs`'s `Compiled`), and the native host's own tests reach a real peer on a loopback port the kernel picks (`curios-runtime/src/os_host/tests.rs`'s `loopback_listener`).
- **A refused request is answered.** `http/Server`'s `answer` sends a 400 for any request its decoder refuses.

## The gap

**HTTP reads as values what it says to refuse, writes what it says not to send, lets a program make a message no wire can carry, and closes a connection under its answer.** Read from the decoders, the server's read loop, the client and the renderers, against RFC 9112 — a bare § is its — and RFC 9110, and run through this tree's compiler wherever a whole message or a loopback peer shows the row. No test drives `http/Server`.

A request, as `Request/decode` and `http/Server` read it:

| Input | The library | The RFCs |
| --- | --- | --- |
| `Content-Length` not a number | no body (`body_of`'s `none()` arm); the server's `needed` reads it as `0` | an unrecoverable framing error, answered 400 (§6.3 rule 5) |
| Two `Content-Length` values that differ, on two lines or in one list | the first line's; a list is not a number, so no body | 400 (§6.3 rule 5) |
| `Content-Length` between whitespace that is no SP or HTAB, a vertical tab say | that number: `Str/trim` trims every whitespace character | 400: optional whitespace is SP and HTAB (RFC 9110 §5.6.3) |
| A body shorter than its `Content-Length` | the bytes that arrived, as the whole body: `Parse/bytes/take` clamps, and the read loop hands on what arrived when the peer closes | incomplete; a server may answer 400 before closing, and does not hand it on as complete (§8) |
| `Transfer-Encoding: chunked` | no body: the field is not read | the chunked body decoded, overriding any `Content-Length` (§6.3 rules 3 and 4) |
| Codings ending in `chunked` with another before it | no body | 501 (§6.1) |
| Codings not ending in `chunked` | no body | 400 (§6.3 rule 4) |
| `Transfer-Encoding` in an HTTP/1.0 request | read as in HTTP/1.1: the version is discarded | faulty framing, 400 (§6.1) |
| HTTP/1.1 without `Host` | addressed to `"localhost"` (`url_of`) | 400 (§3.2) |
| HTTP/1.0 without `Host` | addressed to `"localhost"` | refused, or a configured default applied (§3.3) |
| Two `Host` lines | the first | 400 (§3.2) |
| A `Host` that is no `uri-host [":" port]`, `a/b` say | spliced into the URL, so what follows its slash becomes the path | 400 (§3.2) |
| An absolute-form target, `GET http://a/b HTTP/1.1` | `http://`, the `Host` value and the target spliced into one URL, whose host is then `Host`'s with `http` appended | the target's authority, `Host` ignored (§3.2.2) |
| A target that is neither origin-form nor absolute-form, `foo` or `:81/x` | spliced after `Host`, so it extends the host or sets the port | 400 (§3.2) |
| Whitespace between a field name and its colon | a name ending in whitespace, so a spaced `Content-Length :` is not found and the request reads as bodiless | 400 (§5.1) |
| A field line that begins with whitespace, a fold or the first line | read as a field name, a fold on into the next line up to a colon | 400, or a fold read as a space (§2.2, §5.2) |
| LF or NUL inside a field value | kept in the value | refused, or each replaced by a space (RFC 9110 §5.5) |
| A field value holding octets past ASCII | 400: a field value is read as `Str` (`text`), so obs-text that is not UTF-8 refuses the request | opaque data (RFC 9110 §5.5) |
| A field name that is not a token | read as a name | 400 (RFC 9110 §5.1, §15.5.1) |
| An unrecognized method | 400 | 501 (RFC 9110 §9.1) |
| A malformed version, or whitespace inside the target | accepted: what follows the target's first space is not read | 400 (§2.3, §3.2) |
| A major version other than 1 | accepted: it is not read | 505 (RFC 9110 §15.6.6) |
| An empty line before the request line | 400 | skipped (§2.2) |
| `Expect: 100-continue` | not answered, so the peer waits out its own patience before it sends its content | 100, or a final status, before the content is read (RFC 9110 §10.1.1) |
| A request past the read limit | what arrived within 1 024 reads (`request_bytes`), as the whole request | a target too long, 414 (§3); a header section too large, 431 (RFC 6585 §5); content too large, 413 (RFC 9110 §15.5.14) |
| A peer that stops sending and stays | waited on with no end | 408 (RFC 9110 §15.5.9) |

A request without `Content-Length` or `Transfer-Encoding` has no body (§6.3 rule 7), which is specified.

A reply, as `Response/decode` and `http/Client` read it:

| Input | The library | The RFCs |
| --- | --- | --- |
| `Content-Length` not a number, or two values that differ | everything the peer sent, or the first line's value | an unrecoverable error: the reply discarded (§6.3 rule 5) |
| A `Content-Length` list of one repeated value, `5, 5` | everything the peer sent | that value (§6.3 rule 5) |
| Whitespace between `Content-Length` and its colon | everything the peer sent: the name is not found | a field name is a token (RFC 9110 §5.1) |
| Shorter than its `Content-Length` | the bytes that arrived, documented as clamped | recorded as incomplete (§8) |
| Chunked, ending before its last chunk, or with a size line that is no size | refused as malformed, by whichever primitive met it | recorded as incomplete (§8) |
| Ending inside its head, or empty | refused as malformed | incomplete (§8) |
| A size line with more after its digits than an extension, `5 xyz`, or a bare LF inside one | ignored up to the line's end (`chunk_size`) | no chunk (§7.1, §7.1.1) |
| A last coding other than `chunked` | decoded as chunked, since `chunked` is looked for anywhere in the list | framed by the connection's close (§6.3 rule 4) |
| Any coding besides `chunked` | the body returned still coded | unacceptable: the client sends no `TE`, which leaves `chunked` the only acceptable coding (§7.4) |
| `Transfer-Encoding` on two field lines | the first line's alone | one list, the lines joined (RFC 9110 §5.2) |
| `Transfer-Encoding` in an HTTP/1.0 reply | decoded as chunked | faulty framing (§6.1) |
| A reply to `HEAD`, or a 1xx, 204 or 304 reply, with a nonzero `Content-Length` | no body, by the accident that `take` clamps to what arrived | no body, whatever the fields say (§6.3 rule 1) |
| A 1xx, 204 or 304 reply with no length, and bytes after it | those bytes, as its body | no body (§6.3 rule 1) |
| A 1xx reply ahead of the final one | taken as the reply | skipped (RFC 9110 §15.2) |
| A status code that is not three digits, or a version that is no HTTP version | accepted: `20`, `2000` and `FOO 200 OK` read | a status line is `HTTP-version SP 3DIGIT SP` (§4, §2.3) |
| A folded field line | read on into the next line | each fold read as a space (§5.2) |
| LF or NUL inside a field value | kept | refused, or each replaced by a space (RFC 9110 §5.5) |
| A field value or reason phrase holding octets past ASCII | refused as malformed: the head is read as `Str` (`as_text`) | opaque data (RFC 9110 §5.5), and a reason phrase a client ignores (§4) |
| A field name that is not a token | read as a name | malformed (RFC 9110 §5.1) |
| A peer that answers and keeps the connection open | the deadline, with the reply it sent lost: the client reads to the close (`Socket/call`) | whole where its framing ends (§6.3) |
| A reply of any size | held whole, with no bound | — |
| Framed by the close, over TLS, ending without a closure alert | whole: the host reads a missing alert as the end of the stream | incomplete (§9.8) |

The `HEAD` row matters beyond itself: refusing a short body, as the fourth row asks, would refuse every well-formed reply to `HEAD` that states its length, so the reply's reader must know the request's method.

A message, as `Request/render`, `Response/render` and the server write it:

| Message | The library | The RFCs |
| --- | --- | --- |
| A field name or value, or a reason phrase, holding CR LF | written through, so what follows is a field line of its own | never sent (RFC 9110 §5.5; §2.2, §11.1) |
| A URL holding CR LF or a space, which `Url/of_str` accepts | written into the request line, and into `Response/redirect`'s `Location` | a request target holds no whitespace (§3.2) |
| A status whose version is any text, or whose code is any number | written | `HTTP-version SP 3DIGIT` (§4) |
| A reply to `HEAD` | the handler's body sent | no content (RFC 9110 §9.3.2); `Content-Length` may state what a `GET` would carry (RFC 9110 §8.6) |
| A 1xx, 204 or 304 reply | `Content-Length` always written | none on 1xx or 204, and none on 304 unless it states the 200's length (RFC 9110 §8.6) |
| A body on a 1xx, 204, 205 or 304 reply | written after the head | none (§6.3 rule 1; RFC 9110 §15.3.6) |
| An empty `POST`, `PUT` or `PATCH` | no `Content-Length` | `Content-Length: 0` (RFC 9110 §8.6) |
| A `Content-Length` or `Transfer-Encoding` among the caller's fields | written beside the rendered one | one field line per field, and never both fields (RFC 9110 §5.3, §6.2) |
| Any reply | no `Connection: close`, though the server closes after each | `close` in every reply of a server that keeps no connection (§9.3) |

A connection, as `http/Server`, `tcp/Listener` and the native host end it:

| Event | The library | The RFCs |
| --- | --- | --- |
| A reply sent before the request is read whole, as every refusal is | its sending half ended and the connection closed at once over the unread bytes: a peer still sending fails its write and never reads the reply, `other(32)` over loopback with 20 MB unread, where one that had finished writing reads it | the request read on until the peer closes (§9.6) |
| A handler that fails | the connection closed with no reply | 500 (RFC 9110 §15.6.1) |
| A server over TLS | none: `http/Server/serve` binds its own listener in the clear, and `http/Server/listen` answers a `Server` nothing accepts from | — |

The last row of the reply table is the host's to answer: it is the entry of [the runtime's findings](../06-runtime/00-findings.md), which this spec's last stage fixes.

## Prior art

- **One framing core under both directions.** [h11](https://github.com/python-hyper/h11) holds no IO: bytes go in through `receive_data`, events come out of `next_event`, which answers `NEED_DATA` until a piece is whole, and one `_body_framing(request_method, event)` chooses the framing a body is read and written by ([`_connection.py`](https://github.com/python-hyper/h11/blob/master/h11/_connection.py)). Lean 4's standard library has the same machine indexed by its direction, `Machine (dir : Direction)` with `feed` and `step`, whose `checkReceivingMessageHead` and `checkSendingMessageHead` each answer a framing or an error ([`H1.lean`](https://github.com/leanprover/lean4/blob/master/src/Std/Http/Protocol/H1.lean)). hyper's `Server::parse` and `Client::decoder` choose among a length, chunked and the close in one module, the client's skipping every informational reply in a loop ([`role.rs`](https://github.com/hyperium/hyper/blob/master/src/proto/h1/role.rs)). picohttpparser's `phr_decode_chunked` decodes chunks as they arrive, answering −2, "incomplete", until the last one ([`picohttpparser.h`](https://github.com/h2o/picohttpparser/blob/master/picohttpparser.h)); nom's streaming parsers answer `Err::Incomplete(Needed)` beside `Error` and `Failure`, which is the whole-message alternative ([`internal.rs`](https://github.com/rust-bakery/nom/blob/main/src/internal.rs)).
- **Validity carried by the type.** Lean's `Header.Value` is a string with `isValidHeaderValue : IsValidHeaderValue value := by decide`, a literal's proof decided where it is written and a run-time one answered by `ofString?` ([`Value.lean`](https://github.com/leanprover/lean4/blob/master/src/Std/Http/Data/Headers/Value.lean)), and `Header.Name` the same over a token. hyper's `HeaderValue::from_bytes` refuses every control octet but the tab ([`value.rs`](https://github.com/hyperium/http/blob/master/src/header/value.rs)); h11 runs one `normalize_and_validate` over the fields it reads and the fields it is asked to send ([`_headers.py`](https://github.com/python-hyper/h11/blob/master/h11/_headers.py)); Go refuses a field value holding a control octet on both sides ([`httplex.go`](https://github.com/golang/net/blob/master/http/httpguts/httplex.go)). RFC 9112 §11.1 names the remedy for response splitting itself: restricting the output of header fields to an interface that filters bad octets.
- **A field value is octets.** hyper's `HeaderValue` holds bytes and yields a `&str` only when they are visible ASCII, Go's `ValidHeaderFieldValue` admits obs-text, and Fetch defines a header value as a byte sequence ([Fetch](https://fetch.spec.whatwg.org/#concept-header-value)); Python's `http.client` alone decodes the head as ISO-8859-1 ([`client.py`](https://github.com/python/cpython/blob/main/Lib/http/client.py)). h11 keeps a name's casing for the wire beside its lowercase for comparison; hyper and Lean keep the lowercase alone.
- **The framing fields are the library's.** Fetch forbids a script `Connection`, `Content-Length`, `Host`, `Transfer-Encoding` and their kin, a forbidden one being dropped where it is set ([Fetch](https://fetch.spec.whatwg.org/#forbidden-request-header)); Lean's `normalizeFramingHeaders` erases the caller's `Content-Length` and `Transfer-Encoding` and writes its own; Go discards what a handler writes in reply to `HEAD` and answers a body on a 204 or a 304 with `ErrBodyNotAllowed` ([`server.go`](https://github.com/golang/go/blob/master/src/net/http/server.go)).
- **Framing read strictly.** Go understands one `Transfer-Encoding` line holding `chunked` alone, "one of the most security sensitive surfaces in HTTP/1.1", accepts repeated `Content-Length` lines only when they are identical, ignores `Transfer-Encoding` in HTTP/1.0 and closes after, gives a request with neither field no body, and answers a short body with `io.ErrUnexpectedEOF` ([`transfer.go`](https://github.com/golang/go/blob/master/src/net/http/transfer.go)); hyper refuses `Transfer-Encoding` in HTTP/1.0 outright. [llhttp](https://github.com/nodejs/llhttp) is strict unless a named flag says otherwise: spaces after a chunk size, a missing line end after a chunk, a lone LF, a version that is none, `Transfer-Encoding` beside `Content-Length`.
- **Bounds, each with its status.** nginx answers a request line past one buffer with 414, a field past one with 400, a body past `client_max_body_size` with 413 and a head or a body that stops arriving with 408 ([core module](https://nginx.org/en/docs/http/ngx_http_core_module.html)). Go answers a head past its limit with 431, an unsupported coding with 501, a version it does not serve with 505, every other refusal with 400, and refuses a missing `Host` at HTTP/1.1 and later only. hyper tells a head too large, 431, from a target too long, 414. Lean's `Config` bounds the header section at 64 KiB, the start line and a chunk's size line at 8 KiB each, the trailer fields and the body, the chunk lines counted in it ([`Config.lean`](https://github.com/leanprover/lean4/blob/master/src/Std/Http/Protocol/H1/Config.lean)); warp bounds the header section at 50 KiB and h11 an unfinished event at 16 KiB.
- **The end of a connection.** nginx lingers by default — it reads and ignores what the client still sends, for at most 30 s and 5 s of quiet — and says of turning it off that it "breaks the protocol". Go ends its sending half and sleeps half a second before the close wherever a request body was left unread (`closeWriteAndWait`). warp reads up to 8 KiB of an unconsumed body to keep its connection and closes past that ([`Settings.hs`](https://github.com/yesodweb/wai/blob/master/warp/Network/Wai/Handler/Warp/Settings.hs)), and Lean's machine drains a request's body itself once its reply is sent.
- **A reply cut short.** Python raises `IncompleteRead(partial, expected)`, carrying what arrived, for a short length and for a chunk size that does not read, reads no body for `HEAD`, 1xx, 204 and 304, and skips `100 Continue`. hyper reports `IncompleteMessage`, "connection closed before message completed", as a kind of its own ([`error.rs`](https://github.com/hyperium/hyper/blob/master/src/error.rs)), libcurl `CURLE_PARTIAL_FILE` ([error codes](https://curl.se/libcurl/c/libcurl-errors.html)), and http-client `ResponseBodyTooShort`, `IncompleteHeaders` and `NoResponseDataReceived` each apart ([`Types.hs`](https://github.com/snoyberg/http-client/blob/master/http-client/Network/HTTP/Client/Types.hs)).

## Decisions

Taken before the first stage, each with its reason, so a stage meets none of them as a fork.

1. **One framing core reads both directions, and holds no IO.** Its pieces are pure functions from the bytes held to a piece and what follows it, or to "more": a head once its blank line has arrived, parsed once by `Parse(Bytes, A)`; a chunk's size line once its line end has; a chunk once its size and its line end have; the trailer section once its blank line has. One driver, generic over its monad as `Async/drain` is, asks a `fill` for more bytes and runs the pieces: over a socket for the server and the client, and over a list of segments for `Request/of_bytes`, `Response/of_bytes` and the library's own tests, which therefore read every message at every cut with no host. `Request/decode` and `Response/decode`, parsers over a whole message, go: a parser cannot say that its input has not all arrived.
2. **Framing is read once, by §6.3's rules in order.** One function per direction answers no body, a length, chunked or, for a reply, the connection's close, or says why it cannot. `Transfer-Encoding` is every line of that name joined; its codings must be exactly `chunked`, with a last coding that is another answering a request 400 and a coding before `chunked` 501; beside `Content-Length` it is read alone, which §6.1 lets a server choose; and in an HTTP/1.0 message it is faulty framing. A `Content-Length` is digits between SP and HTAB, and a list of one repeated value is that value. A reply that cannot be framed is malformed.
3. **A message a program makes is one a wire can carry, by its type.** A field is a `Name` and a `Value`: a name's text is a token and a value's bytes hold no control octet but the tab, each a hidden proposition its literal discharges by reduction and a run-time one by the check that answers it as an `Option`. A status's code is three digits and its reason phrase a value's bytes; a reply holds content only where its status carries any, so a body on a 1xx, 204, 205 or 304 reply is not written anywhere. `render` is therefore total and refuses nothing. A name is kept as written and compared without case, since the wire's is the sender's to choose and its meaning is not. Field values and the reason phrase are the octets the wire carried, as RFC 9110 §5.5 asks a recipient to treat obs-text, and a caller wanting text reads one through `Str/of_bytes`.
4. **A URL reaches the wire percent-encoded.** `Url/of_str` refuses a control octet, a space and an octet past ASCII in any part, and the writers of a request line, a `Host` and a `Location` percent-encode any such octet of a `Url` a program built by hand, which is what the encoding is for. `Url` stays a record a program reads by field.
5. **The renderer owns the framing fields.** `Content-Length`, `Transfer-Encoding`, `Connection` and a request's `Host` are written from the message, and a caller's own field of one of those names is not sent, which `with_header` and both renderers document; a message that was read keeps every field line it arrived with. Every message says `Connection: close`, since each end keeps no connection (§9.3). A reply to `HEAD` is sent as its head alone, its `Content-Length` the length of the body the handler produced. A 1xx or 204 reply carries no `Content-Length`. `POST`, `PUT` and `PATCH` always state their length.
6. **A message is bounded in bytes and a request in time, and the bounds are a value.** `http/Limits`: a request line within 8 KiB, answered 414 past it, as §3 asks a server to support at least; a head within 64 KiB, answered 431; content within 4 MiB, today's ceiling of 1 024 reads of 4 KiB, answered 413; a chunk's size line within 4 KiB and a trailer section within the head's bound, each answered 400; and the whole request within 30 s, answered 408. A client bounds a reply's head by the same figure and its content by 64 MiB, and reports one past either as an error of its own. `http/Server`'s and `http/Client`'s documentation state them.
7. **A refusal is a value with its status.** `http/Refusal` says why a request is not read — malformed, a target too long, a head too large, content too large, a method or a coding not implemented, a version not served, the deadline — and maps each to the reply the server owes: 400, 414, 431, 413, 501, 505, 408. A handler that fails is answered 500. A peer that closes before its request is whole is answered 400, as §8 allows.
8. **A request line is `method SP target SP HTTP-version`, and a request with no authority is refused.** A method the library does not name is 501; a version that is no `HTTP/1.x` is 400 and a major version other than 1 is 505; empty lines before the request line are skipped. A target is origin-form or absolute-form and anything else is 400. `Host` is one field line holding `uri-host [":" port]`; none, two or one that is not is 400 at HTTP/1.0 as at 1.1, since §3.3 lets a server refuse an empty authority, the alternative is the server's configured name, and a server bound to a wildcard address has none to offer. An absolute-form target's authority is the request's and `Host` is ignored (§3.2.2), though HTTP/1.1 still owes one.
9. **A field line is refused, not repaired.** Whitespace before the colon, a line that begins with whitespace, a name that is no token, and a CR, LF or NUL in a value refuse a request with 400 and a reply as malformed; RFC 9110 §5.5 and §5.2 let a recipient replace them with spaces, which makes a value up. A reply's fold alone is read as a space, as §5.2 requires of a user agent. A chunk's size line is hexadecimal digits and then nothing or an extension.
10. **A server answers `Expect: 100-continue` from the head.** An HTTP/1.1 request that expects it and has content to send is answered `100 Continue` once its head is read and framed, or its refusal at once, before any content is read (RFC 9110 §10.1.1).
11. **A connection's end is part of the protocol.** A server writes its reply, ends its sending half, reads and discards what the peer still sends until the peer closes, the request's bound is spent or the peer falls quiet, and then closes, so no refusal is lost to a reset (§9.6). The row that ends a sending half is the host's, `Handle/finish`, and sends a TLS stream's closure alert first (§9.8); `tcp/Socket/with` and `tcp/Listener/serve` call it before they close, for every protocol over them. The server's connection handler is a function of a `Socket`, so a program serves it in the clear, under TLS or over a listener of its own.
12. **A reply cut short is `http/Error/incomplete`, carrying what arrived.** A reply shorter than its `Content-Length`, chunked without its last chunk or with framing that does not read, or ending inside its head, raises the reply as far as it arrived — its status and fields where the head was whole, and the body received — as Python's `IncompleteRead` carries its `partial`. The client reads a reply by its framing and stops where it ends, knows its request's method, so a reply to `HEAD` has no body, and skips every 1xx reply ahead of the final one.
13. **The host says how a TLS stream ended.** Once the client stops where a reply's framing ends, a stream that ends without its closure alert is read as a failure, `tls`, and no longer as the end of the stream, so a reply framed by the close and cut short is not handed on as whole (§9.8).

## Stages

Each lands alone, and its commit removes from the tables the rows it gave their RFC's answer.

1. **A made message is one a wire can carry.** `http/Field`, its `Name` and `Value`, and `http/Status`; `Request` and `Response` over them, with the makers and readers that build and read them; both renderers under decision 5; `Url/of_str`'s refusals and the encoding writers of decision 4. The writing table's rows.
2. **The framing core.** `http/Version`, `http/Framing`, `http/Limits`, `http/Refusal`, the head grammar under decisions 8 and 9, the chunk reader and the driver; `Request/of_bytes` and `Response/of_bytes` over it, in place of both `decode`s and of the `lookup`, `render_header` and `until` each message defines. The request and reply tables' rows that a whole message shows.
3. **The server.** `http/Server`'s connection handler over the driver under decisions 6, 7, 10 and 11, with `serve` and `serve_tls` over it; `Server`, `listen`, `close`, `needed`, `blank_line` and the count of reads go. The request table's remaining rows and the connection table's.
4. **The client.** `http/Client`'s exchange over the driver under decisions 6 and 12, `http/Error`'s `incomplete` and its bound's arm, and decision 13 in the native host. The reply table's remaining rows.

## Verification

- Stage 1: each row of the writing table, read from `Request/render` and `Response/render`; a literal name or value that is none is refused where it is written, and `Name/of_str` and `Value/of_bytes` answer `none` for one at run time; `Url/of_str` refuses each octet decision 4 names, and a hand-built `Url` holding one is written encoded.
- Stage 2: each row of the request and reply tables that a whole message shows, read by `Request/of_bytes` or `Response/of_bytes`, gives the answer the decisions take. A well-formed request and reply, by length and by chunks, read whole at every two-way cut of their bytes and want more at every truncation. A request and a reply whose field value holds octets past ASCII read, the value the octets sent, as does a reply whose reason phrase holds them. `Request/render` then `Request/of_bytes` gives the request back with the fields the renderer owns beside its own.
- Stage 3: a server compiled once and run over a scripted inbound host per row answers each with its status — 400, 408, 413, 414, 431, 500, 501 and 505 — reads whole a chunked request sent in segments cut inside a size line, inside a chunk and inside the trailer section, stops reading at the frame's end before a peer that stays open, answers `100 Continue` before it reads an expected body, sends a reply to `HEAD` as its head alone, and writes, finishes, drains and closes in that order.
- Stage 4: `http/perform` over a scripted reply raises `incomplete` with the part that arrived, for a short length, a missing last chunk and a head cut short; reads a reply to `HEAD` that states a length as whole; skips a `100 Continue`; returns a reply whose peer stays open; and reports a reset as `net(reset())`. The native host answers a TLS stream ended without its alert as `tls`.
- Every stage: the prelude build is measured before and after, naming the stage, its rows read from `curios-prelude-archive/.artifacts/profile.tsv` (elaboration) and `curios-prelude/.artifacts/profile.tsv` (certification) after `cargo xtask clippy`, folded by `target/debug/curios profile <file>`, whose first data row is the total.

## Rejected

- **Giving the decoders and the read loop their answers row by row.** It leaves two decoders, a client that reads to the close, renderers total over messages no wire carries and a connection closed under its answer, each a place the next row is missed.
- **Reading an HTTP head as text.** A header may carry obsolete octets outside UTF-8, and the head's framing is bytes; text is what a body declared to be text is.
- **Clamping a short body as the operation's contract.** `Parse/take`'s "everything that remains" is a fine answer for a parser primitive and the wrong one for a framed message, where the length is a promise the peer broke.
- **Reading the whole message again until a refusal says the input ended**, as nom's `Incomplete` would have it. Every primitive would have to mark the end of its input, and each read would parse the message again from its start, quadratic up to the size limit.
- **A machine indexed by its direction, emitting events**, as Lean's and h11's are. Each serves bodies in pieces and connections kept alive, neither of which `/std` has; two drivers over shared pieces say what this library does, and the pieces are what a streaming body would read through.
- **`render` answering a `Result`, or a server answering 500 for a reply it cannot write.** Either reports where the message is sent a mistake made where it was built, to a caller that did not build it.
- **A field value as bare `Bytes`.** It says what a value read off the wire is and nothing of what one about to be written must be, which is what hyper's `HeaderValue` and Lean's `Header.Value` are for.
- **Names kept in lowercase alone**, as hyper and Lean keep them. A literal written as the RFCs spell it, `Content-Type`, would be refused, and the casing a peer chose would be lost to a program that forwards it.
- **Dropping the framing fields from a message that was read**, as Go deletes them from a request's header. A handler reading what arrived would find less than arrived.
- **The server's bound name for a missing `Host`**, §3.3's configured default. A server bound to a wildcard address has no name to give, and `"localhost"` is the invented value this spec removes.
- **A short reply as `malformed`, or as an `incomplete` with nothing attached.** The first is told apart from a garbled reply only by its message; the second discards what arrived, as Go, hyper and curl do and Python does not.
- **Replacing a field's LF or NUL, or a request's fold, with spaces.** RFC 9110 §5.5 and §5.2 allow it, and it makes a value up.
- **Reading a head's octets past ASCII as ISO-8859-1 into `Str`**, as Python does. It never refuses and loses nothing, but it reads a UTF-8 value as other characters, `é` as `Ã©`, which is invented text.
- **Closing at once after an early answer, or draining with the sending half left open.** The first resets the connection under the answer; the second leaves a peer that reads to the close waiting out the server's quiet bound, and sends a TLS peer no alert.
- **Proofs on `Url`'s parts.** A URL a program assembles — a query from its pairs, a redirect's target from a base — would owe a proof about text it computed, where encoding on the way out costs none and is the encoding's purpose.
- **A test over real sockets at a port the test names.** Two runs on one machine contend for it; the host's rows are held to a real peer at a port the kernel picks, and the library to a scripted one.

## Completion criteria

- HTTP frames a message by RFC 9110 and RFC 9112 in both directions, reading and writing, through one core, and holds a head's opaque octets as octets.
- No value of `Request` or `Response` is a message the renderers write and a peer cannot read.
- No HTTP declaration answers a value it made up where a refusal belongs: every remaining default is a specified answer, and its documentation says which.
- A reply the server sends reaches a peer that is still sending.

## Retirement

Move the contracts to `http`'s module documentation, and add to `curios-std`'s README the decision this spec settles with its rejected alternatives. Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
