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
| `max(a, b)`   | `(Nat, Nat) -> Nat` | Maximum                              |

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

Raw byte sequences. `fold` and `join` are library helpers; the rest are `/sys` primitives.

| Binding             | Type                                      | Description                                |
| ------------------- | ----------------------------------------- | ------------------------------------------ |
| `len(b)`            | `(Bin) -> Nat`                            | Byte length                                |
| `eql(a, b)`         | `(Bin, Bin) -> Bln`                       | Equality                                   |
| `get(b, i)`         | `(Bin, Nat) -> Nat`                       | Byte at index `i` (traps if out of bounds) |
| `slice(b, s, e)`    | `(Bin, Nat, Nat) -> Bin`                  | Subsequence from `s` to `e` (traps if out of range) |
| `append(b, x)`      | `(Bin, Nat) -> Bin`                       | Append a single byte (`x` taken mod 256)   |
| `concat(a, b)`      | `(Bin, Bin) -> Bin`                       | Concatenate two sequences                  |
| `flatten(parts)`    | `(Arr(Bin)) -> Bin`                       | Concatenate every part in one allocation   |
| `fold(b, init, f)`  | `(@A : Type, Bin, A, (Nat, A) -> A) -> A` | Left fold over the bytes                   |
| `join(sep, parts)`  | `(Bin, Arr(Bin)) -> Bin`                  | Concatenate with a separator between parts |

### `/std/Arr`

Homogeneous arrays, written with literal syntax `[a, b, c]`. `fold` and `balanced` are library helpers; the rest are `/sys` primitives. `map` is a primitive with an eliminator reduction (it distributes over the `[]`/`++`/`append` spine, so it reduces symbolically in proofs the way a structural `foldr` would) and erases to a single O(n) fill loop.

| Binding              | Type                                                  | Description                 |
| -------------------- | ----------------------------------------------------- | --------------------------- |
| `len(a)`             | `(@T : Type, Arr(T)) -> Nat`                          | Element count               |
| `get(a, i)`          | `(@T : Type, Arr(T), Nat) -> T`                       | Element at index `i`        |
| `slice(a, s, e)`     | `(@T : Type, Arr(T), Nat, Nat) -> Arr(T)`             | Subarray from `s` to `e`    |
| `append(a, x)`       | `(@T : Type, Arr(T), T) -> Arr(T)`                    | Append a single element     |
| `concat(a, b)`       | `(@T : Type, Arr(T), Arr(T)) -> Arr(T)`               | Concatenate two arrays      |
| `flatten(a)`         | `(@T : Type, Arr(Arr(T))) -> Arr(T)`                  | Concatenate every inner array in one allocation |
| `fold(arr, init, f)` | `(@T : Type, @A : Type, Arr(T), A, (T, A) -> A) -> A` | Left fold over the elements |
| `map(f, arr)`        | `(@A : Type, @B : Type, (A) -> B, Arr(A)) -> Arr(B)`  | Elementwise map             |
| `balanced(n, empty, single, combine)` | `(@A : Type, Nat, A, (Nat) -> A, (A, A) -> A) -> A` | Balanced fold over indices `0..n`: `single` per index, `combine` to merge halves, `empty` when `n = 0` |

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
| `flatten(parts)`    | `(Arr(Str)) -> Str`      | Concatenate every part                                         |
| `join(sep, parts)`  | `(Str, Arr(Str)) -> Str` | Concatenate with a separator between parts                     |
| `eql(a, b)`         | `(Str, Str) -> Bln`      | String equality (byte equality; UTF-8 is canonical)            |
| `len(s)`            | `(Str) -> Nat`           | Codepoint count (Unicode scalar values, _not_ bytes/graphemes) |
| `get(s, i)`         | `(Str, Nat) -> Nat`      | Codepoint at index `i` (traps if out of bounds)                |
| `slice(s, x, y)`    | `(Str, Nat, Nat) -> Str` | Codepoints `[x, y)` (traps if out of range)                    |
| `fold(s, init, f)`  | `(@A : Type, Str, A, (Nat, A) -> A) -> A` | Left fold over the codepoints                  |
| `trim(s)`           | `(Str) -> Str`           | Strip leading and trailing ASCII whitespace                    |

## IO

The handle-based IO primitives (`read`/`write`/`open`/`connect`/`close`) and the ambient host services (clocks, randomness, process environment) all live in `/sys/Io`, but `/std/Io` re-exports only the byte-stream operations on the standard handles. The rest surface through dedicated modules: file handles through [`/std/File`](#stdfile), sockets through [`/std/Net`](#stdnet), time through [`/std/Time`](#stdtime), randomness through [`/std/Rand`](#stdrand), and process access through [`/std/Proc`](#stdproc).

Asynchronous, non-blocking IO and concurrency are layered on top by [`/std/Task`](#stdtask) — a free-monad scheduler with fire-and-forget fibers, awaitable futures, and a finalizer guarantee. The `File` and `Net` operations are `Task`s built on its non-blocking `read`/`write`/`accept` leaves, so they multiplex over a single poll and are driven by `Task/block_on`.

### `/std/Io`

The byte-stream handle type — an opaque runtime token, like a file descriptor. `stdin`/`stdout`/`stderr` are re-exported from `/sys`; `read`/`write` are typed **blocking** operations on a handle, and `print`/`print_err`/`input` are the console conveniences over them. The three handles plus blocking `read`/`write` are the whole synchronous IO doorway — everything else (files, sockets, concurrency) rides the asynchronous [`/std/Task`](#stdtask) layer. (The raw status-record primitives, the `poll` multiplexer, and the pure `eql` handle-identity op stay internal to `/sys`, reached only by the scheduler.)

| Binding        | Type                             | Description                                                                  |
| -------------- | -------------------------------- | ---------------------------------------------------------------------------- |
| `stdin`        | `Io`                             | The standard input handle                                                    |
| `stdout`       | `Io`                             | The standard output handle                                                   |
| `stderr`       | `Io`                             | The standard error handle                                                    |
| `read(h, n)`   | `(Io, Nat) -> Read`              | Read up to `n` bytes, blocking; yields `chunk` / `eof` / `error`             |
| `write(h, b)`  | `(Io, Bin) -> Result({}, Error)` | Write all of `b`, looping on partial writes, blocking; also the binary-stdout channel |
| `print(s)`     | `(Str) -> {}`                    | Write a string to stdout, result discarded (best-effort, like a closed pipe) |
| `print_err(s)` | `(Str) -> {}`                    | Write a string to stderr, result discarded                                   |
| `input()`      | `() -> Option(Bin)`              | Read the next line from stdin (trailing `\n` stripped); `none` at end of input |

Failable operations report through a status code — errors are data; traps stay reserved for programmer errors. This is the shared contract across every handle (files, sockets):

| Status | Meaning                                                       |
| ------ | ------------------------------------------------------------- |
| 0      | ok                                                            |
| 1      | end of input (`read` only; `bytes` is empty)                  |
| 2      | not found                                                     |
| 3      | permission denied                                             |
| 4      | already exists                                                |
| 5      | connection refused (`connect` only)                           |
| 6      | would block (non-blocking op; the async layer parks and retries) |
| 7      | TLS error                                                     |

Codes are the host's `Status` discriminants; any code without a typed form decodes to `other(status)`.

`/std/Io` also defines the typed forms every IO operation returns in place of raw status codes — the blocking `read`/`write` above, and the [`/std/Task`](#stdtask)/`/std/File`/`/std/Net` layer:

```
union Error | not_found() | permission_denied() | exists() | refused() | tls() | other(Nat) end
union Read  | chunk(Bin) | eof() | error(Error) end
union Mode  | read() | write() | append() end
```

`error_of(status) : (Nat) -> Error` maps a raw status to a typed `Error` (status 2 → `not_found`, 3 → `permission_denied`, 4 → `exists`, 5 → `refused`, 7 → `tls`, else `other`). `Read` is the result of a read: a `chunk` of bytes, the distinct `eof`, or an `error`. `Mode` is the open mode (`read`/`write`/`append`); `of_mode(mode) : (Mode) -> Nat` is the wire tag `/std/File/open` marshals.

### `/std/Reader`

A buffered, line-oriented reader layered over [`/std/Io`](#stdio): a small state monad threading a `Buffer` (a handle plus bytes read ahead but not yet consumed) through actions, exactly as [`/std/Parse`](#stdparse) threads its (input, position).

| Binding     | Type                                               | Description                                                              |
| ----------- | -------------------------------------------------- | ------------------------------------------------------------------------ |
| `Buffer`    | `Type` (`= {Io, Bin}`)                           | A handle plus bytes already read but not yet consumed                    |
| `Reader(A)` | `(Type) -> Type` (`= (Buffer) -> {Buffer, A}`)   | The buffered-reader state monad                                          |
| `buffer(h)` | `(Io) -> Buffer`                                   | A fresh buffer over `h`, empty                                           |
| `pure(a)`   | `(@A : Type, A) -> Reader(A)`                       | Lift a value                                                             |
| `bind`      | `(@A, @B) -> (Reader(A), (A) -> Reader(B)) -> Reader(B)` | Sequence two actions (use with `let ! = Reader/bind;` blocks)       |
| `run(m, h)` | `(@A : Type, Reader(A), Io) -> A`                   | Run an action against a fresh buffer on `h`                              |
| `read_line` | `Reader(Option(Bin))`                               | The next line, including its trailing `\n`; `none` means end of input    |

`read_line` delivers a final unterminated line before EOF as `some`; any non-ok refill (EOF or an IO error) ends the stream — an error-propagating reader is future work.

### `/std/Task`

The asynchronous effect and concurrency layer. A `Task(A)` is a **free monad**: building one performs nothing — every effect is reified as data and fires only when a scheduler (`run` or `block_on`) interprets it — so a task is a first-class, inert description of work, composed with `bind`/`map` and handed to a runner. Internally it is a two-constructor type (a finished value, or a suspended effect the scheduler steps), but those constructors are private: `Task` is an **opaque** type, built only through the combinators below.

The surface is two tiers. The **application API** is what programs use — the monad, spawning and awaiting, cancellation, the resource bracket, and the runners. The **SPI** is the seam for building *new* effects and resources on the engine — the leaf IO primitives, the effect and blocking constructors, the unbalanced finalizer pair, and the promise primitive; both [`/std/File`](#stdfile) and [`/std/Net`](#stdnet) are built entirely on it.

**Application API:**

| Binding | Type | Description |
| ------- | ---- | ----------- |
| `pure(a)` | `(@A : Type, A) -> Task(A)` | Lift a value into a finished task |
| `bind(m, f)` | `(@A, @B) -> (Task(A), (A) -> Task(B)) -> Task(B)` | Sequence two tasks (use with `let ! = Task/bind;` blocks) |
| `map(f, m)` | `(@A, @B, (A) -> B, Task(A)) -> Task(B)` | Map a task's result with a pure function |
| `go(body)` | `(Fiber) -> Task({})` | Spawn `body` as a fire-and-forget fiber — no handle, not cancelable |
| `spawn(body)` | `(@A : Type, () -> Task(A)) -> Task(Handle(A))` | Spawn `body` as a fiber, returning a `Handle` to await or cancel it |
| `await(f)` | `(@A : Type, Future(A)) -> Task(A)` | Park until `f` is fulfilled, then yield its value |
| `join_all(tasks)` | `(@A, Arr(() -> Task(A))) -> Task(Arr(A))` | Spawn every task, await them all, collect results positionally |
| `select(tasks)` | `(@A, Arr(() -> Task(A))) -> Task({Nat, A})` | Run all; the first to finish wins (its index and value); the losers are cancelled |
| `race(tasks)` | `(@A, Arr(() -> Task(A))) -> Task(A)` | `select`'s winning value, dropping the index |
| `cancel(t)` | `(Token) -> {}` | Flag a token cancelled; its fiber is reaped at its next step, finalizers running |
| `using(h, release, body)` | `(@A, Io, Finalizer, Task(A)) -> Task(A)` | Bracket `h`: register `release`, run `body`, release exactly once — on completion or on cancel/drop |
| `run(main)` | `(Task({})) -> {}` | Drive `main` as the program root (`block_on` at the unit result) |
| `block_on(t)` | `(@A : Type, Task(A)) -> A` | Drive `t` to its value, multiplexing every fiber over one `Io/poll` |
| `Handle(A)` | `Type` (`= {result : Future(A), token : Token}`) | A spawned fiber's result future paired with its cancellation token |
| `Future(A)` | `Type` (`= Cell(Fut(A))`) | A one-shot result cell, awaited with `await` |
| `Token` | `Type` (`= Cell(Bln)`) | A cooperative-cancellation flag |
| `Finalizer` | `Type` (`= () -> {}`) | A synchronous cleanup action, run at release or on cancellation |
| `Fiber` | `Type` (`= () -> Task({})`) | A fiber body: a thunk producing a unit task |

**SPI** — for building new effects and resources on the scheduler:

| Binding | Type | Description |
| ------- | ---- | ----------- |
| `defer(body)` | `(@A : Type, () -> Task(A)) -> Task(A)` | Suspend `body` so its construction effects fire only when served — what keeps the leaves inert |
| `wait(h, ev)` | `(Io, Nat) -> Task({})` | Yield until handle `h` is ready for interest bitmask `ev` (`READ = 1`, `WRITE = 2`, plus `ERR`/`HUP`) |
| `park(register)` | `((Waker) -> {}) -> Task({})` | Yield, handing `register` a one-shot waker that re-queues the fiber |
| `acquire(h, fin)` | `(Io, Finalizer) -> Task({})` | Register finalizer `fin` keyed to handle `h` on the running fiber |
| `release(h)` | `(Io) -> Task({})` | Run and drop the finalizer keyed to `h` |
| `new_future()` | `(@A : Type) -> Future(A)` | A fresh, unfulfilled future |
| `fulfill(f, a)` | `(@A : Type, Future(A), A) -> {}` | Fulfill `f` with `a` (idempotent), waking everyone parked on it |
| `nonblocking(h)` | `(Io) -> {}` | Put a handle into non-blocking mode |
| `read(h, n)` | `(Io, Nat) -> Task(Io/Read)` | Read up to `n` bytes, yielding on would-block; `chunk` / `eof` / `error` |
| `write(h, b)` | `(Io, Bin) -> Task(Result({}, Io/Error))` | Write all of `b`, yielding on would-block and resending only the unwritten tail |
| `accept(l)` | `(Io) -> Task(Result(Io, Io/Error))` | Accept the next connection on listener `l`, yielding until one is pending |
| `Waker` | `Type` (`= () -> {}`) | The one-shot resume callback handed to `park`'s registrar |

**Inert construction.** Evaluation is strict, so a leaf like `read(h, n)` built eagerly would fire its syscall the instant it is *constructed*, not when it is served. Every leaf and resource-allocating combinator is therefore wrapped in `defer`, which reifies its body as a suspended step the scheduler forces only on arrival. Building a task tree — even one you never run — thus performs no IO and allocates no scheduler state.

**Resource brackets and the finalizer guarantee.** Finalizers are **handle-keyed**: `acquire(h, fin)` registers `fin` against handle `h` on the running fiber, and `release(h)` runs and drops the one keyed to `h` (LIFO among that fiber's guards). The guarantee is **exactly-once** — a finalizer runs on the fiber's normal completion, on an explicit `release`, on cancellation, or, for a fiber still parked when the program ends, when `block_on` drains it at shutdown. So a handle acquired this way is released on every path and never twice. `using(h, release, body)` is that bracket as a single combinator; `File/open` and `Net/connect` register their `close` as a finalizer the instant they hand back the handle, so even a flat open/close pair is leak-safe.

**Spawning, futures, cancellation.** `go` launches a fire-and-forget fiber — nothing to await, nothing to cancel. `spawn` launches one and hands back a `Handle`: its result `Future` and a cancellation `Token`, so `await(h.result)` collects the value and `cancel(h.token)` stops it. A `Future` is a one-shot cell (`new_future` / `fulfill` / `await`); awaiting one parks the fiber until something fulfills it. Cancellation is **cooperative** — `cancel` sets the token and the scheduler reaps the fiber at its next step, running its finalizers, rather than interrupting it mid-step. `join_all` spawns every task and awaits all of them (positional results); `select` runs them and lets the first finisher win, cancelling the losers; `race` is `select` without the index.

**Running.** `block_on(t)` is the scheduler loop: it drives the root `t` while multiplexing every fiber's IO waits over a single `/sys/Io/poll`, waking fibers as their handles become ready and resuming fibers parked on futures as those fulfill. When the root produces its value, every still-outstanding fiber is drained and its finalizers run — a stuck background fiber can never leak a resource or hang shutdown. If the program reaches a state where nothing is runnable and nothing is waiting on IO (a genuine deadlock — the root can never finish), `block_on` runs all outstanding finalizers and then terminates the process with a non-zero exit. `run` is `block_on` at the unit result, the normal program entry. The leaf actions `read`/`write`/`accept` are the non-blocking primitives every higher layer (`/std/File`, `/std/Net`) builds on: each yields on would-block and resumes when the handle is ready, surfacing a typed `Io/Read` or `Result(_, Io/Error)` rather than a raw status.

### `/std/File`

`File` is an **abstract handle** — its own opaque type, distinct from a bare `Io` handle (stdin/stdout, a socket) and reachable only through the operations below. It is a zero-cost newtype over `Io`, so the abstraction is free at runtime. The operations are asynchronous [`Task`](#stdtask)s (the handle is configured non-blocking before it is ever read or written, so its reads and writes yield to the scheduler). `open` and `close` are public and flat — but `open` registers the close as a handle-keyed finalizer the moment it hands back the `File`, so a handle dropped or cancelled before its `close` is still closed by the scheduler (and never closed twice). `with`/`read_all` are the bracketed sugar over that pair.

```
File/open(path, mode)          -- (Str, Io/Mode) -> Task(Result(File, Io/Error))
File/close(f)                  -- (File) -> Task({})
File/with(path, mode, body)    -- (@A : Type, Str, Io/Mode, (File) -> Task(A)) -> Task(Result(A, Io/Error))
File/read_all(path)            -- (Str) -> Task(Result(Bin, Io/Error))
File/read(f, n)                -- (File, Nat) -> Task(Io/Read)
File/write(f, b)               -- (File, Bin) -> Task(Result({}, Io/Error))
```

`open` hands back a `File` with its `close` already registered as a finalizer; `close` runs and drops it. `with` is the sugar that pairs them around `body` — open, run `body`, close — returning the body's value or the open failure. Because the close is a scheduler-tracked finalizer, it runs whether `body` completes or its fiber is dropped first, so a flat `open`/`close` is as leak-safe as `with`. Inside the body, `read`/`write` are the operations on that handle. The handle must not outlive its `close` — an effect delayed past it, such as a `body` result that is itself a closure performing IO, would touch a closed handle. Failures are typed [`Io/Error`](#stdio); a read yields [`Io/Read`](#stdio) (`chunk`/`eof`/`error`). Programs run with the invoking user's filesystem access — there is no sandbox.

### `/std/Net`

A TCP client and a concurrent TCP server, in cleartext or over TLS. Every operation is an asynchronous [`Task`](#stdtask). `Socket` is an **abstract handle** — like `/std/File`, a zero-cost newtype over `Io`, kept distinct so a socket is never confused with stdin/stdout or a file. `connect` and `close` are public and flat: `connect` registers the close as a handle-keyed finalizer when it hands back the `Socket`, so a connection dropped or cancelled before its `close` is still closed (and never twice); `with`/`call`/`serve`/`serve_tls` are the bracketed forms over that pair. It builds on the `/sys/Io/connect`, `/sys/Io/listen`, and `/sys/Io/accept` primitives, with TLS layered on the conduit-upgrade primitives `/sys/Io/start_tls` (client) and `/sys/Io/tls_server_config` + `/sys/Io/start_tls_server` (server): the socket connects (or is accepted) in cleartext, then the handshake upgrades it in place to an encrypted stream the same `read`/`write` serve. The client trusts a bundled root set with verification on; the SNI is taken from `host`. Custom roots and client certificates are future work.

```
struct Settings pub {
    connect_timeout : Option(Time/Duration),
    read_timeout : Option(Time/Duration),
    write_timeout : Option(Time/Duration),
    tls : Bln
}
```

`Settings` has a public representation — build one as `Settings { ... }` or start from `default`. Each timeout is optional; `none` blocks forever. `connect_timeout` bounds the connect itself; `read_timeout`/`write_timeout` bound subsequent reads and writes on the handle. `tls` upgrades the connection to TLS right after connect (verification on, SNI from `host`); `default` leaves it `false`. Failures are the typed [`Io/Error`](#stdio) — a connect surfaces `refused` (status 5), `tls` (status 7, a failed certificate verification, handshake, or upgrade), or `other(status)`.

| Binding                               | Type                                                                 | Description                                                    |
| ------------------------------------- | -------------------------------------------------------------------- | -------------------------------------------------------------- |
| `default`                             | `Settings`                                                                  | All timeouts `none` (block forever)                            |
| `connect(settings, host, port)`       | `(Settings, Str, Nat) -> Task(Result(Socket, Io/Error))`                    | Connect (registering its `close`); pair with `close`, or use `with` |
| `close(c)`                            | `(Socket) -> Task({})`                                                      | Close a connected socket — runs its registered finalizer       |
| `with(settings, host, port, body)`    | `(@A, Settings, Str, Nat, (Socket) -> Task(A)) -> Task(Result(A, Io/Error))`| Connect, run `body` on the `Socket`, then close                |
| `call(settings, host, port, request)` | `(Settings, Str, Nat, Bin) -> Task(Result(Bin, Io/Error))`                  | Connect, send `request`, read the whole response to EOF, close |
| `read(c, n)`                          | `(Socket, Nat) -> Task(Io/Read)`                                            | Read up to `n` bytes from the socket (`chunk`/`eof`/`error`)   |
| `write(c, b)`                         | `(Socket, Bin) -> Task(Result({}, Io/Error))`                              | Write all of `b` to the socket                                 |
| `serve(host, port, handler)`          | `(Str, Nat, (Socket) -> Task({})) -> Task(Result({}, Io/Error))`           | Bind `host`:`port`, then accept in a loop, running each connection's `handler` as its own fire-and-forget fiber |
| `serve_tls(host, port, cert, key, handler)` | `(Str, Nat, Bin, Bin, (Socket) -> Task({})) -> Task(Result({}, Io/Error))` | Like `serve` but **sequential**, terminating TLS on each accepted connection with the PEM `cert` chain and `key` |

As with `File`, the `Socket` must not outlive the `with` or `handler` body — a delayed effect would touch a closed connection. A TLS `with`/`call` (`settings.tls`) upgrades the connection right after connect; `read`/`write` then transparently serve the encrypted stream.

`serve` is a **concurrent** server: it binds a listening socket (private and fully bracketed — closed when the loop ends), then loops `accept` → spawn the connection's bracketed handler as its own fiber → accept again, so a slow connection never blocks the accept loop or its peers. Each handler is spawned with `Task/go` and brackets its socket with `using`, so the connection's `close` is a scheduler-tracked finalizer: it runs when the handler completes, and at program shutdown the scheduler drains every still-running handler and runs its `close`. A failed `accept` ends the loop; a failed `listen` (e.g. the port is in use) is returned as `Io/Error`. `serve_tls` is **sequential** — the rustls handshake and record layer are driven synchronously by the host, so each accepted connection completes its handshake and runs its handler before the next `accept`. It builds a server config from the PEM `cert`/`key` once; a connection whose handshake fails is dropped while the loop continues (unlike a failed `accept`, which ends it). Concurrent TLS handling is future work.

### `/std/Http`

An HTTP/1.1 client layered on `/std/Net`, over cleartext (`http://`) or TLS (`https://`): a request is just bytes written to a socket and a response is bytes read back, so the module is request formatting plus a `/std/Parse` parser over the reply. TLS is handled entirely by `/std/Net` through the request's `settings.tls` flag — there is no HTTP-specific crypto machinery. The surface is value-centric — build a `Request`, hand it to `perform`, get back a `Result(Response, Error)`. `secure` flips a request to TLS; `get_tls`/`post_tls` are the shorthands.

```
union Method | get() | post() end
union Error  | net(Net/Error) | malformed(Str) end

struct Request pub {
    method : Method,
    host : Str,
    port : Nat,
    path : Str,
    headers : Arr({Str, Str}),
    body : Bin,
    settings : Net/Settings
}

struct Status pub   { version : Str, code : Nat, reason : Str }
struct Response pub { status : Status, headers : Arr({Str, Str}), body : Bin }
```

`Request`, `Status`, and `Response` all have public representations. In a `Request`, `headers` are sent verbatim and in order after the automatic `Host`/`Connection: close`/`Content-Length` lines; `body` is sent as-is (its `Content-Length` is added automatically when non-empty). A failed round trip is `Error/net` (a transport failure surfaced by `/std/Net`) or `Error/malformed` (a response that did not parse).

| Binding                 | Type                                   | Description                                                                          |
| ----------------------- | -------------------------------------- | ------------------------------------------------------------------------------------ |
| `get(host, port, path)` | `(Str, Nat, Str) -> Request`           | A bare GET with default settings and no extra headers or body                        |
| `secure(request)`       | `(Request) -> Request`                 | Flip a request to TLS (`settings.tls = true`); `perform` then speaks `https://`       |
| `get_tls(host, port, path)` | `(Str, Nat, Str) -> Request`       | `secure(get(...))` — a GET over TLS (use port 443)                                    |
| `post_tls(host, port, path, body)` | `(Str, Nat, Str, Bin) -> Request` | `secure(post(...))` — a POST over TLS (use port 443)                              |
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

Across these modules a deliberate argument-order convention holds: `bind` takes the container first (`bind(m, f)`), so it fits the `let ! = …/bind;` do-notation, while `map`/`fold` take the function first (`map(f, m)`), matching `Arr/map`. The asymmetry is intentional, not an oversight.

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

### `/std/Order`

The three-way comparison result, `lt()` / `eq()` / `gt()` — the shape returned by total comparisons such as `BigNat/compare`.

### `/std/BigNat`

Arbitrary-precision non-negative integers, needed because the runtime `Nat` is a 31-bit carrier that traps on overflow. A `BigNat` is a list of base-10⁴ limbs held least-significant-first, kept canonical (no high zero limbs; zero is the empty list), so the supported algorithm — shortest-round-trip float rendering (`Flt/to_str`) — can do the wide arithmetic it needs without ever exceeding `Nat`.

```
pub struct BigNat pub { limbs : Lst(Nat) }
```

| Binding             | Type                          | Description                                                       |
| ------------------- | ----------------------------- | ---------------------------------------------------------------- |
| `zero`              | `BigNat`                      | The canonical zero (empty limb list)                             |
| `of_nat(n)`         | `(Nat) -> BigNat`             | Lift a 31-bit `Nat` to a `BigNat`                                |
| `is_zero(x)`        | `(BigNat) -> Bln`             | Test against the canonical zero                                  |
| `compare(a, b)`     | `(BigNat, BigNat) -> Order`   | Total magnitude comparison                                       |
| `add(a, b)`         | `(BigNat, BigNat) -> BigNat`  | Addition                                                         |
| `sub(a, b)`         | `(BigNat, BigNat) -> BigNat`  | Truncated subtraction; assumes `a ≥ b` (otherwise saturates low) |
| `mul_small(a, c)`   | `(BigNat, Nat) -> BigNat`     | Multiply by a small scalar (`c < 100000`)                        |
| `mul_pow2(a, k)`    | `(BigNat, Nat) -> BigNat`     | Multiply by `2^k`                                                |
| `to_str(a)`         | `(BigNat) -> Str`             | Decimal rendering                                                |

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
Parse(A) = (Bin, Nat) -> Result({Nat, A}, Str)
```

— success carries the new position and the value; failure carries a message (`Str`). The input being parsed stays a `Bin` (raw bytes).

| Binding                             | Description                                         |
| ----------------------------------- | --------------------------------------------------- |
| `run(p, input)`                     | Run from position 0; `Result(A, Str)`               |
| `pure(a)` / `fail(msg)`             | Constant success / failure                          |
| `map(f, p)`                         | Map the result                                      |
| `bind`                              | Sequencing, shaped for `let ! = Parse/bind;` blocks |
| `or(p, q)`                          | Try `p`, fall back to `q`                           |
| `and(p, q)`                         | Both in sequence; pairs the results as `{A, B}`   |
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
| arr(Arr(Json)) | obj(Arr({Str, Json}))
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
