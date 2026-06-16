# Curios Standard Library

The standard library lives under `/std`, prepended to every program automatically. Its sources are ordinary Curios in `std/*.crs`, declared by the `std.crs` manifest and embedded into the compiler binary at build time. The primitives it builds on live in an internal `/sys` module that `/std` re-exports; user code reaches them only through `/std`.

Each scalar and collection module re-exports its `/sys` counterpart (`pub use /sys/Nat/*;` and the type itself), so `use /std/{Nat};` brings in both the primitives and the helpers below. The `/sys` paths are internal — user code always goes through `/std`. The manifest also re-exports each module's principal type at the `/std` root.

This file is the canonical reference for the `/std` public surface and lists every binding each module exposes, including the re-exported `/sys` primitives. (`SYNTAX.md` documents the language itself and points here for the library.) The `std/*.crs` sources are comment-free by policy; their documentation lives here.

- [Scalars](#scalars) — `Nat`, `Int`, `Flt`, `Bln`
- [Bytes and arrays](#bytes-and-arrays) — `Bin`, `Arr`, `Char`, `Str`
- [IO and system](#io) — `Io`, `File`, `Net`, `Http`, `Time`, `Rand`, `Proc`
- [Data types](#data-types) — `Option`, `Result`, `Lst`, `Vec`
- [Proofs](#proofs) — `Eq`, `Void`
- [Parsing and formatting](#parsing-and-formatting) — `Parse`, `Json`, `Fmt`

## Scalars

### `/std/Nat`

The natural numbers — unbounded at the type level (an unsigned i31 at runtime). Literals are decimal digits (`0`, `42`); structural induction and sparse dispatch are written with [`match`](SYNTAX.md#match). `of_str` and `min` are library helpers; the rest are `/sys` primitives re-exported by `pub use /sys/Nat/*`. The bitwise ops are total and never trap: `and`/`or`/`xor`/`shr` are the usual operations on the binary digits, while `shl` is the unbounded `a * 2^b` — a `Nat` has no top, so no bits are shifted off. There is no `not`: complement has no meaning on an unbounded `Nat` (it would name the runtime word width), so use `Int/not` or `xor` against an explicit mask.

| Binding       | Type                | Description                          |
| ------------- | ------------------- | ------------------------------------ |
| `succ(a)`     | `(Nat) -> Nat`      | Successor (`a + 1`)                  |
| `add(a, b)`   | `(Nat, Nat) -> Nat` | Addition                             |
| `sub(a, b)`   | `(Nat, Nat) -> Nat` | Subtraction                          |
| `mul(a, b)`   | `(Nat, Nat) -> Nat` | Multiplication                       |
| `div(a, b)`   | `(Nat, Nat) -> Nat` | Division                             |
| `rem(a, b)`   | `(Nat, Nat) -> Nat` | Remainder                            |
| `eql(a, b)`   | `(Nat, Nat) -> Bln` | Equality                             |
| `neq(a, b)`   | `(Nat, Nat) -> Bln` | Inequality                           |
| `lt(a, b)`    | `(Nat, Nat) -> Bln` | Less than                            |
| `gt(a, b)`    | `(Nat, Nat) -> Bln` | Greater than                         |
| `lte(a, b)`   | `(Nat, Nat) -> Bln` | Less than or equal                   |
| `gte(a, b)`   | `(Nat, Nat) -> Bln` | Greater than or equal                |
| `and(a, b)`   | `(Nat, Nat) -> Nat` | Bitwise AND                          |
| `or(a, b)`    | `(Nat, Nat) -> Nat` | Bitwise OR                           |
| `xor(a, b)`   | `(Nat, Nat) -> Nat` | Bitwise XOR                          |
| `shl(a, b)`   | `(Nat, Nat) -> Nat` | Left shift by `b` (the unbounded `a * 2^b`) |
| `shr(a, b)`   | `(Nat, Nat) -> Nat` | Logical right shift by `b`           |
| `to_int(a)`   | `(Nat) -> Int`      | Convert to `Int`                     |
| `to_flt(a)`   | `(Nat) -> Flt`      | Convert to `Flt`                     |
| `to_str(a)`   | `(Nat) -> Str`      | Decimal text                         |
| `of_str(s)`   | `(Str) -> Option(Nat)` | Parse a decimal numeral; `none` unless every character is a digit |
| `min(a, b)`   | `(Nat, Nat) -> Nat` | Minimum                              |

### `/std/Int`

Signed integers — unbounded at the type level (a signed i31 at runtime). `of_str`, `not`, and `abs` are library helpers; the rest are `/sys` primitives. The bitwise ops are total and never trap: `and`/`or`/`xor` are the usual operations on the two's-complement bits, `shl` is the unbounded `a * 2^b`, and `shr` is arithmetic — it preserves the sign. `not` exposes the runtime word — there is no machine complement instruction, so it is the library `xor` against `-1` (the all-ones word), which is also the complement `-a - 1`.

| Binding       | Type                | Description                                       |
| ------------- | ------------------- | ------------------------------------------------- |
| `add(a, b)`   | `(Int, Int) -> Int` | Addition                                          |
| `sub(a, b)`   | `(Int, Int) -> Int` | Subtraction                                       |
| `mul(a, b)`   | `(Int, Int) -> Int` | Multiplication                                    |
| `div(a, b)`   | `(Int, Int) -> Int` | Division                                          |
| `rem(a, b)`   | `(Int, Int) -> Int` | Remainder                                         |
| `eql(a, b)`   | `(Int, Int) -> Bln` | Equality                                          |
| `neq(a, b)`   | `(Int, Int) -> Bln` | Inequality                                        |
| `lt(a, b)`    | `(Int, Int) -> Bln` | Less than                                         |
| `gt(a, b)`    | `(Int, Int) -> Bln` | Greater than                                      |
| `lte(a, b)`   | `(Int, Int) -> Bln` | Less than or equal                                |
| `gte(a, b)`   | `(Int, Int) -> Bln` | Greater than or equal                             |
| `and(a, b)`   | `(Int, Int) -> Int` | Bitwise AND                                       |
| `or(a, b)`    | `(Int, Int) -> Int` | Bitwise OR                                        |
| `xor(a, b)`   | `(Int, Int) -> Int` | Bitwise XOR                                       |
| `not(a)`      | `(Int) -> Int`      | Bitwise complement (`xor` against `-1`)           |
| `shl(a, b)`   | `(Int, Int) -> Int` | Left shift by `b` (the unbounded `a * 2^b`)       |
| `shr(a, b)`   | `(Int, Int) -> Int` | Arithmetic right shift by `b` (sign-preserving)   |
| `to_nat(a)`   | `(Int) -> Nat`      | Convert to `Nat`                                  |
| `to_flt(a)`   | `(Int) -> Flt`      | Convert to `Flt`                                  |
| `to_str(a)`   | `(Int) -> Str`      | Decimal text                                      |
| `of_str(s)`   | `(Str) -> Option(Int)` | Parse a decimal numeral with optional leading `+`/`-`; `none` on invalid input |
| `abs(n)`      | `(Int) -> Nat`      | Absolute value                                    |

### `/std/Flt`

Floating-point numbers. `of_str` is a library helper; the rest are `/sys` primitives.

| Binding        | Type                | Description                                                       |
| -------------- | ------------------- | ----------------------------------------------------------------- |
| `add(a, b)`    | `(Flt, Flt) -> Flt` | Addition                                                          |
| `sub(a, b)`    | `(Flt, Flt) -> Flt` | Subtraction                                                       |
| `mul(a, b)`    | `(Flt, Flt) -> Flt` | Multiplication                                                    |
| `div(a, b)`    | `(Flt, Flt) -> Flt` | Division                                                          |
| `min(a, b)`    | `(Flt, Flt) -> Flt` | Minimum                                                           |
| `max(a, b)`    | `(Flt, Flt) -> Flt` | Maximum                                                           |
| `eql(a, b)`    | `(Flt, Flt) -> Bln` | Equality                                                          |
| `neq(a, b)`    | `(Flt, Flt) -> Bln` | Inequality                                                        |
| `lt(a, b)`     | `(Flt, Flt) -> Bln` | Less than                                                         |
| `gt(a, b)`     | `(Flt, Flt) -> Bln` | Greater than                                                      |
| `lte(a, b)`    | `(Flt, Flt) -> Bln` | Less than or equal                                                |
| `gte(a, b)`    | `(Flt, Flt) -> Bln` | Greater than or equal                                             |
| `neg(a)`       | `(Flt) -> Flt`      | Negation                                                          |
| `abs(a)`       | `(Flt) -> Flt`      | Absolute value                                                    |
| `sqrt(a)`      | `(Flt) -> Flt`      | Square root                                                       |
| `floor(a)`     | `(Flt) -> Flt`      | Round down                                                        |
| `ceil(a)`      | `(Flt) -> Flt`      | Round up                                                          |
| `trunc(a)`     | `(Flt) -> Flt`      | Truncate toward zero                                              |
| `nearest(a)`   | `(Flt) -> Flt`      | Round to nearest                                                  |
| `to_nat(a)`    | `(Flt) -> Nat`      | Convert to `Nat`                                                  |
| `to_int(a)`    | `(Flt) -> Int`      | Convert to `Int`                                                  |
| `to_le_bin(a)` | `(Flt) -> Bin`      | Little-endian byte encoding                                       |
| `to_str(a)`    | `(Flt) -> Str`      | Decimal text                                                      |
| `of_str(s)`    | `(Str) -> Option(Flt)` | Parse a `digits.digits` numeral with optional sign and `e`/`E` exponent; `none` on invalid input |

### `/std/Bln`

Booleans. The values are the literals `true`/`false`, eliminated with [`match`](SYNTAX.md#match) (`| true => … | false => …`). `Bln` rides the same i31 carrier as `Nat`, with `false`/`true` as `0`/`1`, so the three logic ops (`and`/`or`/`xor`) are `/sys` primitives — bitwise machine ops on that single bit — re-exported by `pub use /sys/Bln/*`; `not` (the library `xor(b, true)`), `eql`, `to_str`, and `of_str` are library helpers.

| Binding     | Type                   | Description                                            |
| ----------- | ---------------------- | ------------------------------------------------------ |
| `not(b)`    | `(Bln) -> Bln`         | Logical negation                                       |
| `and(a, b)` | `(Bln, Bln) -> Bln`    | Conjunction                                            |
| `or(a, b)`  | `(Bln, Bln) -> Bln`    | Disjunction                                            |
| `xor(a, b)` | `(Bln, Bln) -> Bln`    | Exclusive or                                           |
| `eql(a, b)` | `(Bln, Bln) -> Bln`    | Equality                                               |
| `to_str(b)` | `(Bln) -> Str`         | `"true"` or `"false"`                                  |
| `of_str(s)` | `(Str) -> Option(Bln)` | `some` of the bool for `"true"`/`"false"`, else `none` |

## Bytes and arrays

### `/std/Bin`

Raw byte sequences. `fold`, `concat_all`, and `join` are library helpers; the rest are `/sys` primitives.

| Binding             | Type                                      | Description                                |
| ------------------- | ----------------------------------------- | ------------------------------------------ |
| `len(b)`            | `(Bin) -> Nat`                            | Byte length                                |
| `eql(a, b)`         | `(Bin, Bin) -> Bln`                       | Equality                                   |
| `get(b, i)`         | `(Bin, Nat) -> Nat`                       | Byte at index `i` (traps if out of bounds) |
| `slice(b, s, e)`    | `(Bin, Nat, Nat) -> Bin`                  | Subsequence from `s` to `e` (traps if out of range) |
| `append(b, x)`      | `(Bin, Nat) -> Bin`                       | Append a single byte (`x` taken mod 256)   |
| `concat(a, b)`      | `(Bin, Bin) -> Bin`                       | Concatenate two sequences                  |
| `fold(b, init, f)`  | `(@A : Type, Bin, A, (Nat, A) -> A) -> A` | Left fold over the bytes                   |
| `concat_all(parts)` | `(Arr(Bin)) -> Bin`                       | Concatenate every part                     |
| `join(sep, parts)`  | `(Bin, Arr(Bin)) -> Bin`                  | Concatenate with a separator between parts |

### `/std/Arr`

Homogeneous arrays, written with literal syntax `[a, b, c]`. `fold` and `map` are library helpers; the rest are `/sys` primitives.

| Binding              | Type                                                  | Description                 |
| -------------------- | ----------------------------------------------------- | --------------------------- |
| `len(a)`             | `(@T : Type, Arr(T)) -> Nat`                          | Element count               |
| `get(a, i)`          | `(@T : Type, Arr(T), Nat) -> T`                       | Element at index `i`        |
| `slice(a, s, e)`     | `(@T : Type, Arr(T), Nat, Nat) -> Arr(T)`             | Subarray from `s` to `e`    |
| `append(a, x)`       | `(@T : Type, Arr(T), T) -> Arr(T)`                    | Append a single element     |
| `concat(a, b)`       | `(@T : Type, Arr(T), Arr(T)) -> Arr(T)`               | Concatenate two arrays      |
| `fold(arr, init, f)` | `(@T : Type, @A : Type, Arr(T), A, (T, A) -> A) -> A` | Left fold over the elements |
| `map(f, arr)`        | `(@A : Type, @B : Type, (A) -> B, Arr(A)) -> Arr(B)`  | Elementwise map             |

### `/std/Char`

Byte classifiers over ASCII code points (`(Nat) -> Bln`): `is_whitespace`, `is_digit`, `is_lower`, `is_upper`, `is_alpha`, `is_alphanumeric`. Plus the case mappers `to_lower` and `to_upper` (`(Nat) -> Nat`), which shift ASCII letters and pass every other byte through unchanged.

### `/std/Str`

`Str` is the UTF-8 string type (`"..."` literals have this type). It shares `Bin`'s _runtime representation_ — `to_bin` is a no-op carrier projection onto those bytes — but is a distinct type at the surface. Only `to_bin` is a primitive (re-exported from the internal `/sys`); `concat`, `eql`, `len`, and the rest are ordinary library definitions built on `to_bin` and the `Bin` ops. `of_bin` is the single, checked bridge from arbitrary bytes into text; within `Str`-to-`Str` code it is never needed. (There is no unchecked constructor in this API — the lone trust is the internal `/sys/Str/of_bin` substrate, used by the checked `of_bin` behind an `is_utf8` gate and by `concat`/`slice`, which is sound because UTF-8 is closed under concatenation and codepoint-boundary slicing.) The read side — `get`, `slice`, `len`, and `fold` — indexes by Unicode scalar value, not byte, so `slice` cuts only on codepoint boundaries and always yields valid text.

| Binding             | Type                     | Description                                                    |
| ------------------- | ------------------------ | -------------------------------------------------------------- |
| `to_bin(s)`         | `(Str) -> Bin`           | The underlying UTF-8 bytes (total)                             |
| `of_bin(b)`         | `(Bin) -> Option(Str)`   | Checked construction: `some` iff `b` is well-formed UTF-8      |
| `is_utf8(b)`        | `(Bin) -> Bln`           | Whether `b` is well-formed UTF-8                               |
| `concat(a, b)`      | `(Str, Str) -> Str`      | Concatenate two strings                                        |
| `concat_all(parts)` | `(Arr(Str)) -> Str`      | Concatenate every part                                         |
| `join(sep, parts)`  | `(Str, Arr(Str)) -> Str` | Concatenate with a separator between parts                     |
| `eql(a, b)`         | `(Str, Str) -> Bln`      | String equality (byte equality; UTF-8 is canonical)            |
| `len(s)`            | `(Str) -> Nat`           | Codepoint count (Unicode scalar values, _not_ bytes/graphemes) |
| `get(s, i)`         | `(Str, Nat) -> Nat`      | Codepoint at index `i` (traps if out of bounds)                |
| `slice(s, x, y)`    | `(Str, Nat, Nat) -> Str` | Codepoints `[x, y)` (traps if out of range)                    |
| `fold(s, init, f)`  | `(@A : Type, Str, A, (Nat, A) -> A) -> A` | Left fold over the codepoints                  |
| `trim(s)`           | `(Str) -> Str`           | Strip leading and trailing ASCII whitespace                    |

## IO

The handle-based IO primitives (`read`/`write`/`open`/`connect`/`close`) and the ambient host services (clocks, randomness, process environment) all live in `/sys/Io`, but `/std/Io` re-exports only the byte-stream operations on the standard handles. The rest surface through dedicated modules: file handles through [`/std/File`](#stdfile), sockets through [`/std/Net`](#stdnet), time through [`/std/Time`](#stdtime), randomness through [`/std/Rand`](#stdrand), and process access through [`/std/Proc`](#stdproc).

### `/std/Io`

The byte-stream handle type — an opaque runtime token, like a file descriptor. `stdin`/`stdout`/`stderr` and the primitive `read`/`write` are re-exported from `/sys`; the rest are buffered-reader conveniences built on top.

| Binding       | Type                                         | Description                                                    |
| ------------- | -------------------------------------------- | -------------------------------------------------------------- |
| `stdin`       | `Io`                                         | The standard input handle                                      |
| `stdout`      | `Io`                                         | The standard output handle                                     |
| `stderr`      | `Io`                                         | The standard error handle                                      |
| `read(h, n)`  | `(Io, Nat) -> { status : Nat, bytes : Bin }` | Read up to `n` bytes, blocking until at least one is available |
| `write(h, b)` | `(Io, Bin) -> Nat`                           | Write `b` to `h`; returns a status                             |

Failable operations report through a status code — errors are data; traps stay reserved for programmer errors. This is the shared contract across every handle (files, sockets):

| Status | Meaning                                      |
| ------ | -------------------------------------------- |
| 0      | ok                                           |
| 1      | end of input (`read` only; `bytes` is empty) |
| 2      | not found                                    |
| 3      | permission denied                            |
| 4      | already exists                               |
| 5      | other                                        |
| 6      | connection refused (`connect` only)          |

The safe conveniences below layer a buffered reader over the primitive handle.

| Binding     | Type                                             | Description                                                                                                                               |
| ----------- | ------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------- |
| `print(s)`  | `(Str) -> {}`                                    | Write a string to stdout, best-effort: the write status is dropped, like printing to a closed pipe                                        |
| `Reader`    | `Type` (`= { Io, Bin }`)                         | A buffered reader: the handle plus bytes already read but not yet consumed                                                                |
| `reader(h)` | `(Io) -> Reader`                                 | A fresh reader with an empty buffer                                                                                                       |
| `Buf(A)`    | `(Type) -> Type` (`= (Reader) -> { Reader, A }`) | The buffered-reader state monad — the reader threads explicitly through actions, exactly like `Parse` threads its (input, position) state |
| `pure(a)`   | `(@A : Type, A) -> Buf(A)`                       | Lift a value                                                                                                                              |
| `bind`      | `(@A, @B) -> (Buf(A), (A) -> Buf(B)) -> Buf(B)`  | Sequence two actions (use with `let ! = Io/bind;` blocks)                                                                                 |
| `run(m, h)` | `(@A : Type, Buf(A), Io) -> A`                   | Run an action against a fresh reader on `h`                                                                                               |
| `read_line` | `Buf(Option(Bin))`                               | The next line, including its trailing `\n`; `none` means end of input                                                                     |

`read_line` delivers a final unterminated line before EOF as `some`; any non-ok refill status (EOF or an IO error) ends the stream — an error-propagating reader is future work.

### `/std/File`

`File` is an **abstract handle** — its own opaque type, distinct from a bare `Io` handle (stdin/stdout, a socket) and reachable only through the operations below. It is a zero-cost newtype over `Io`, so the abstraction is free at runtime. There is no public `open` or `close` anywhere: `with`/`read_all` bracket them automatically, so a handle can never leak from the safe layer or be closed twice.

```
union Mode  | read() | write() | append() end   -- File/Mode/read() etc.
union Error | not_found() | permission_denied() | exists() | other(Nat) end

File/with(path, mode, body)    -- (@A : Type, Str, Mode, (File) -> A) -> Result(A, Error)
File/read_all(path)            -- (Str) -> Result(Bin, Error)
File/read(f, n)                -- (File, Nat) -> { status : Nat, bytes : Bin }
File/write(f, b)               -- (File, Bin) -> Nat
```

`with` is the one doorway to a file handle: open, run `body` on the `File` it yields, close. Inside the body, `read`/`write` are the operations on that handle. The handle never outlives the bracket — an effect delayed past it, such as a `body` result that is itself a closure performing IO, would touch a closed handle. `Error` mirrors the `/std/Io` status contract (0 ok, 1 eof, 2 not found, 3 permission denied, 4 exists, 5+ other). Programs run with the invoking user's filesystem access — there is no sandbox.

### `/std/Net`

A TCP client. `Socket` is an **abstract handle** — like `/std/File`, a zero-cost newtype over `Io`, kept distinct so a socket is never confused with stdin/stdout or a file. There is no public `close`: `with`/`call` bracket the connection so a handle never outlives the body and cannot be used after close. It builds on the `/sys/Io/connect` primitive; TLS (`https://`) and a server side are future work.

```
union Error | refused() | other(Nat) end

struct Settings pub {
    connect_timeout : Option(Time/Duration),
    read_timeout : Option(Time/Duration),
    write_timeout : Option(Time/Duration)
}
```

`Settings` has a public representation — build one as `Settings { ... }` or start from `default`. Each timeout is optional; `none` blocks forever. `connect_timeout` bounds the connect itself; `read_timeout`/`write_timeout` bound subsequent reads and writes on the handle. `Error` decodes the connect status: `refused` (status 6) or `other(status)`.

| Binding                               | Type                                                                 | Description                                                    |
| ------------------------------------- | -------------------------------------------------------------------- | -------------------------------------------------------------- |
| `default`                             | `Settings`                                                           | All timeouts `none` (block forever)                            |
| `with(settings, host, port, body)`    | `(@A : Type, Settings, Str, Nat, (Socket) -> A) -> Result(A, Error)` | Connect, run `body` on the `Socket`, then close                |
| `call(settings, host, port, request)` | `(Settings, Str, Nat, Bin) -> Result(Bin, Error)`                    | Connect, send `request`, read the whole response to EOF, close |
| `read(c, n)`                          | `(Socket, Nat) -> { status : Nat, bytes : Bin }`                     | Read up to `n` bytes from the socket                           |
| `write(c, b)`                         | `(Socket, Bin) -> Nat`                                               | Write `b` to the socket; returns a status                      |

As with `File`, the `Socket` must not outlive the `with` body — a delayed effect would touch a closed connection.

### `/std/Http`

A plain (cleartext, `http://`) HTTP/1.1 client layered on `/std/Net`: there is no new host primitive, a request is just bytes written to a socket and a response is bytes read back, so the module is request formatting plus a `/std/Parse` parser over the reply. The surface is value-centric — build a `Request`, hand it to `perform`, get back a `Result(Response, Error)`. TLS (`https://`) is a separate future phase needing a host-side primitive.

```
union Method | get() | post() end
union Error  | net(Net/Error) | malformed(Str) end

struct Request pub {
    method : Method,
    host : Str,
    port : Nat,
    path : Str,
    headers : Arr({ Str, Str }),
    body : Bin,
    settings : Net/Settings
}

struct Status pub   { version : Str, code : Nat, reason : Str }
struct Response pub { status : Status, headers : Arr({ Str, Str }), body : Bin }
```

`Request`, `Status`, and `Response` all have public representations. In a `Request`, `headers` are sent verbatim and in order after the automatic `Host`/`Connection: close`/`Content-Length` lines; `body` is sent as-is (its `Content-Length` is added automatically when non-empty). A failed round trip is `Error/net` (a transport failure surfaced by `/std/Net`) or `Error/malformed` (a response that did not parse).

| Binding                 | Type                                   | Description                                                                          |
| ----------------------- | -------------------------------------- | ------------------------------------------------------------------------------------ |
| `get(host, port, path)` | `(Str, Nat, Str) -> Request`           | A bare GET with default settings and no extra headers or body                        |
| `perform(request)`      | `(Request) -> Result(Response, Error)` | Drive one request end to end: render, send through `/std/Net`, parse the reply       |
| `render(request)`       | `(Request) -> Bin`                     | Serialize a request to the raw bytes written on the wire (pure; testable on its own) |
| `header(resp, name)`    | `(Response, Str) -> Option(Str)`       | The first header whose name matches `name`, case-insensitively                       |
| `status_line`           | `Parse(Status)`                        | Parse one status line (`HTTP/1.1 200 OK\r\n`) and its terminating CRLF               |
| `decode`                | `Parse(Response)`                      | Parse a complete response                                                            |

`decode` reads the status line, the header lines, the blank line ending the header block, then the body — framed by `Content-Length` when that header is present and numeric, otherwise everything to EOF (the `Connection: close` fallback). Response header names are kept verbatim (case is not normalised in storage; use `header` for case-insensitive lookup).

### `/std/Time`

Wall-clock and monotonic time. `now` and `elapsed` are **0-arity functions** — each call performs a fresh read. (A value binding would be a CAF: read once, then shared.) `Instant` and `Duration` are **opaque**, observed only through the operations below.

An `Instant` is seconds since the Unix epoch — split into two base-10⁹ limbs so it clears the 2³¹-second (≈ year 2038) ceiling of the i31 runtime `Nat` — plus sub-second nanoseconds. A `Duration` is whole seconds plus nanoseconds, a span of up to ≈ 68 years.

```
Instant    -- opaque: a wall-clock timestamp
Duration   -- opaque: a span of time
```

| Binding                 | Type                             | Description                                                              |
| ----------------------- | -------------------------------- | ------------------------------------------------------------------------ |
| `now()`                 | `() -> Instant`                  | 0-arity: current wall-clock time (may jump when the system clock is set) |
| `elapsed()`             | `() -> Duration`                 | 0-arity: monotonic time since program start; never goes backward         |
| `diff(a, b)`            | `(Instant, Instant) -> Duration` | `a − b`, saturating to zero when `a` precedes `b`                        |
| `before(a, b)`          | `(Instant, Instant) -> Bln`      | `a` strictly earlier than `b`                                            |
| `secs(d)`               | `(Duration) -> Nat`              | Whole seconds                                                            |
| `nanos(d)`              | `(Duration) -> Nat`              | Sub-second nanoseconds (0–999999999)                                     |
| `zero`                 | `Duration`                       | The zero duration                                                        |
| `of_secs(secs)`         | `(Nat) -> Duration`              | A `Duration` of whole seconds                                            |
| `of_millis(ms)`         | `(Nat) -> Duration`              | A `Duration` from milliseconds                                           |

Time a section by reading `now()` (or `elapsed()`) on each side and taking the `diff`. Calendar decomposition of an `Instant` and general `Duration` arithmetic are future work.

### `/std/Rand`

```
Rand/bin(n)   -- (Nat) -> Bin : n random bytes from the host's OS randomness source
```

A uniform `Nat`, a bounded range, and a seedable generator are future work, built on `bin` in pure Curios.

### `/std/Proc`

Access to the process environment. `args` is a value — an immutable snapshot taken once — while `env` and `exit` are functions.

| Binding      | Type                   | Description                                                      |
| ------------ | ---------------------- | ---------------------------------------------------------------- |
| `args`       | `Arr(Bin)`             | The command-line arguments                                       |
| `env(name)`  | `(Str) -> Option(Bin)` | The value of environment variable `name`, if set                 |
| `exit(code)` | `(Nat) -> Void`        | Terminate the process with status `code`; never returns (`Void`) |

## Data types

### `/std/Option`

```
pub union Option(A : Type)
| some(A)
| none()
end
```

| Binding                 | Type                                                     | Description                                                   |
| ----------------------- | -------------------------------------------------------- | ------------------------------------------------------------- |
| `bind`                  | `(@A, @B) -> (Option(A), (A) -> Option(B)) -> Option(B)` | Sequence two options (use with `let ! = Option/bind;` blocks) |
| `map(f, m)`             | `(@A, @B, (A) -> B, Option(A)) -> Option(B)`             | Map the contained value, if any                               |
| `unwrap_or(m, default)` | `(@A, Option(A), A) -> A`                                | The contained value, or `default` when `none`                 |
| `is_some(m)`            | `(@A, Option(A)) -> Bln`                                 | Whether `m` is `some`                                         |
| `is_none(m)`            | `(@A, Option(A)) -> Bln`                                 | Whether `m` is `none`                                         |

### `/std/Result`

```
pub union Result(A : Type, E : Type)
| success(A)
| failure(E)
end
```

| Binding                 | Type                                                                  | Description                                                               |
| ----------------------- | --------------------------------------------------------------------- | ------------------------------------------------------------------------- |
| `map(f, r)`             | `(@A, @B, @E, (A) -> B, Result(A, E)) -> Result(B, E)`                | Map the `success` value, leaving a `failure` untouched                    |
| `map_err(f, r)`         | `(@A, @E, @F, (E) -> F, Result(A, E)) -> Result(A, F)`                | Map the `failure` value, leaving a `success` untouched                    |
| `bind`                  | `(@A, @B, @E) -> (Result(A, E), (A) -> Result(B, E)) -> Result(B, E)` | Sequence, short-circuiting on `failure` (use with `let ! = Result/bind;`) |
| `unwrap_or(r, default)` | `(@A, @E, Result(A, E), A) -> A`                                      | The `success` value, or `default` on `failure`                            |
| `is_ok(r)`              | `(@A, @E, Result(A, E)) -> Bln`                                       | Whether `r` is `success`                                                  |

### `/std/Lst`

The linked list, `nil()` / `cons(A, Lst(A))`, with:

| Binding     | Type                            | Description                |
| ----------- | ------------------------------- | -------------------------- |
| `len(l)`    | `(@A : Type, Lst(A)) -> Nat`    | Length                     |
| `rev(l)`    | `(@A : Type, Lst(A)) -> Lst(A)` | Reversal                   |
| `to_arr(l)` | `(@A : Type, Lst(A)) -> Arr(A)` | Conversion to a flat array |

### `/std/Vec`

The length-indexed vector — the canonical indexed union (see SYNTAX.md's Indices section):

```
pub union Vec(T : Type) : (n : Nat)
| nil() : (0)
| cons(@m : Nat, x : T, xs : Vec(T, m)) : (Nat/succ(m))
end
```

`cons`'s length binder is implicit at the constructor (`@m` is recoverable from `xs`), so values are written `Vec/cons(x, xs)`.

| Binding        | Type                                                          | Description                                                             |
| -------------- | ------------------------------------------------------------- | ----------------------------------------------------------------------- |
| `len(v)`       | `(@T, @n : Nat, Vec(T, n)) -> Nat`                            | The length is the index: `n` rides in implicitly and is simply returned |
| `append(v, w)` | `(@T, @n, @m, Vec(T, n), Vec(T, m)) -> Vec(T, Nat/add(n, m))` | Concatenation, length-summing by type                                   |
| `map(f, v)`    | `(@A, @B, @n, (A) -> B, Vec(A, n)) -> Vec(B, n)`              | Length-preserving map                                                   |
| `first(v)`     | `(@T, @n, Vec(T, Nat/succ(n))) -> T`                          | Head — only accepts non-empty vectors                                   |
| `rest(v)`      | `(@T, @n, Vec(T, Nat/succ(n))) -> Vec(T, n)`                  | Tail — only accepts non-empty vectors                                   |

In `first`/`rest` the `nil` arm is provably impossible at length `Nat/succ(n)` and simply omitted: the checker verifies the omission, and `cons`'s length binder is pinned to `n`.

## Proofs

### `/std/Eq`

Propositional equality: `Eq(x, y)` is inhabited exactly when `x` and `y` are equal, and its only constructor pins both indices to the same value. The parameter is `@`-marked — implicit at the type, recoverable from the indices, pinned with `Eq(@Nat, x, y)` when wanted. Matching on a proof refines the indices inside the arm (`x := z`, `y := z`), which is what makes the eliminators typecheck. See PROOFS_101.md for the full story.

```
pub union Eq(@A : Type) : (x : A, y : A)
| refl(@z : A) : (z, z)
end
```

| Binding          | Type                                         | Description                                                |
| ---------------- | -------------------------------------------- | ---------------------------------------------------------- |
| `sym(p)`         | `(Eq(x, y)) -> Eq(y, x)`                     | Symmetry                                                   |
| `trans(p, q)`    | `(Eq(x, y), Eq(y, z)) -> Eq(x, z)`           | Transitivity                                               |
| `cong(f, p)`     | `(f : (A) -> B, Eq(x, y)) -> Eq(f(x), f(y))` | Congruence                                                 |
| `subst(P, p, v)` | `(P : (A) -> Type, Eq(x, y), P(x)) -> P(y)`  | Transport: rewrite `x` to `y` under an arbitrary predicate |

(All `@A`/`@x`/`@y`/`@z` parameters elided above; they infer.)

### `/std/Void`

The uninhabited type: a union with zero cases. No value of `Void` can be constructed, so holding one is itself a contradiction — and eliminating it is a match with zero arms, which checks at any motive.

| Binding                 | Type                               | Description                              |
| ----------------------- | ---------------------------------- | ---------------------------------------- |
| `absurd(contradiction)` | `(@A : Type, Void) -> A`           | From the absurd, anything follows        |
| `Not(P)`                | `(Type) -> Type` (`= (P) -> Void`) | Negation: a proof of `P` would be absurd |

## Parsing and formatting

### `/std/Parse`

Byte-level parser combinators over a `(input : Bin, position : Nat)` state:

```
Parse(A) = (Bin, Nat) -> Result({ Nat, A }, Str)
```

— success carries the new position and the value; failure carries a message (`Str`). The input being parsed stays a `Bin` (raw bytes).

| Binding                             | Description                                         |
| ----------------------------------- | --------------------------------------------------- |
| `run(p, input)`                     | Run from position 0; `Result(A, Str)`               |
| `pure(a)` / `fail(msg)`             | Constant success / failure                          |
| `map(f, p)`                         | Map the result                                      |
| `bind`                              | Sequencing, shaped for `let ! = Parse/bind;` blocks |
| `or(p, q)`                          | Try `p`, fall back to `q`                           |
| `and(p, q)`                         | Both in sequence; pairs the results as `{ A, B }`   |
| `any_byte` / `peek_byte`            | Next byte, consuming / not consuming                |
| `take_byte(expected)`               | Exactly the given byte                              |
| `take_while(pred)`                  | Longest run of bytes satisfying `pred`              |
| `many0(p)`                          | Zero or more `p`, collected in an `Arr`             |
| `sep_by0(elem, sep)`                | Zero or more `elem` separated by `sep`              |

### `/std/Json`

A JSON tree and a byte-level codec:

```
pub union Json
| null() | bln(Bln) | num(Flt) | str(Str)
| arr(Arr(Json)) | obj(Arr({ Str, Json }))
end
```

| Binding     | Type            | Description                                      |
| ----------- | --------------- | ------------------------------------------------ |
| `encode(v)` | `(Json) -> Str` | Serialize                                        |
| `decode`    | `Parse(Json)`   | Parser for a JSON value, built from `/std/Parse` |

(`decode` parses a `Bin`; `encode` returns a `Str` — feed it back in via `Str/to_bin` to round-trip.)

`examples/crs_json_codec.rs` round-trips a tree through both.

### `/std/Fmt`

Typed format strings: the argument list of `printf` is computed from the format string at compile time by an ordinary dependent function — no macro.

| Binding                  | Description                                                                           |
| ------------------------ | ------------------------------------------------------------------------------------- |
| `Fmt`                    | The parsed format AST: `nil()`, `lit(Str, Fmt)`, `str(Fmt)` (`%s`), `nat(Fmt)` (`%d`) |
| `parse(s)`               | `(Str) -> Fmt` — parse a format string                                                |
| `format_type_with(T, f)` | The dependent-type computation: the curried function type a format demands            |
| `format(s)`              | Build a `Str` by applying the demanded arguments (`s : Str`)                          |
| `printf(s)`              | Like `format`, but also prints the result                                             |

`/std/Fmt/printf("%s is %d")` has type `(Str) -> (Nat) -> Str`; `%s` takes a `Str`, and passing a `Bin` where `%d` expects a `Nat` is a compile-time `TypeMismatch`. See `examples/crs_printf.rs`.
