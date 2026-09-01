# The standard library's indispensable tier

## Status

Researched, not designed. This file records a survey of what nine standard libraries agree on, the distance between that and `/std` — read from source and probed through `wonder` on 2026-09-01 — the items that close it in the order they land, and the decisions taken while planning them. A second survey, of the same libraries' filesystem and subprocess surfaces, was taken on 2026-09-01 and is folded in as items eight and nine, and the two terminal rows the terminal specification found it needed are item ten: together they are the host half the command-line and terminal specifications presuppose, and every host row this campaign adds is stated here and nowhere else. The modules keep their own contracts in `curios-prelude-archive/std/` and nothing here restates one. Nothing is started.

## Why it exists

`/std` grew by consumer: every module exists because a codec, a benchmark or a proof needed it. That made it deep where a consumer pushed — a certified `BigNat` algebra, a crit-bit `Map` with an injectivity law, a poll-driven `Async` — and thin where none did. Nothing in it sorts. A string cannot be compared. `List` has ten functions. `Vec` cannot be indexed. There is no `Set`. A proof cannot recurse along a measure.

A survey of Lean 4 (`Init`, `Std`, Batteries), Agda (`agda-stdlib` 2.x), Idris 2 (`base`), Rocq (`Stdlib`), Haskell (`base`), OCaml (`Stdlib` 5.x), Rust (`core`, `alloc`, `std`), Gleam and Zig was taken to separate what every one of them ships from what only some do. The tier every library agrees on is the one specified here: a capability is in it when each of the nine has it and a program of ordinary shape meets its absence early. Two further tiers — what most libraries ship, and what a consumer must first ask for — came out of the same survey and are deliberately not in this file.

The same thinness holds where a program meets the machine. The host implements open, read, write and close on files, sockets and TLS, the clocks, randomness, arguments, one environment variable at a time, and exit — so a program can read a file whose path it knows and replace it whole, and cannot list a directory, remove or rename anything, ask whether a path exists, learn its working directory, run another program, take its terminal out of canonical mode, or learn how wide that terminal is. The command-line specification ([command-line-interface-spec.md](command-line-interface-spec.md)) and the terminal one ([terminal-ui-spec.md](terminal-ui-spec.md)) describe programs that act on a machine they cannot inspect. A second survey took the host surface of the same nine — Lean's `IO.FS` and `IO.Process`, Agda's `System.Directory.Primitive` and `System.Process.Primitive`, Idris's `System`, `System.Directory` and `System.File`, Haskell's `directory` and `process`, OCaml's `Sys` and `Unix`, Rust's `std::fs` and `std::process`, Zig's `std.fs` and `std.process` — and WASI preview 1 beside them as the floor a WebAssembly host offers. Two of the nine forced the counting rule to bend, and the bend is stated rather than hidden: Rocq's `Stdlib` has no host interaction at all, and its de facto `coq-simple-io` binds `Sys.command`, `getenv` and `argv` and nothing of the filesystem; Gleam's standard library has none either, and `simplifile` and `shellout` are what every Gleam program uses. So for the host, a capability is in the tier when every library that has host interaction has it, Gleam's de facto packages counted and Rocq excused.

## What is certain

Read from source and probed through `wonder`, in the order the items below land.

- **Ordering stops at the numerics.** `Cmp` and `Ord` have witnesses on `Nat`, `Int`, `Flt`, `Char`, `Byte`, `BigNat`, `BigInt`, `{}` and tuples up to eight, and none on `Str`, `Bytes`, `Bool`, `List`, `Option` or `Result`. `"a" < "b"` is refused with `no witness of Cmp(Str) found`. `Key(Str)` lets a string be a map key and not be compared. `Vec` and `Map` have no `Eql` or `Show`.
- **`/std/List` holds `get`, `len`, `slice` and `map` from `/sys` — appending is the spread literal — and `try_get`, `drop`, `flatten`, `fold`, `find` and `balanced`.** Nothing sorts, filters, reverses, zips, takes, partitions, searches by index or traverses under a monad. `find` is its own index loop under the proof-carrying `Nat/Lt` invariant `fold` also carries, and every further predicate function written the same way would repeat both.
- **Every fold walks left to right with `(elem, acc)`.** `List`, `Bits`, `Bytes`, `Str` and `Map` agree, eight `/std` callers rely on the order, and `List.crs` states none of it. The right fold is the language's: the `; ih` binding on a `[head, ..tail]` arm is the structural recursion, on every carrier, and the optimizer works on it.
- **`/std/Str` holds `concat`, `slice`, `flatten`, `eql`, `eql_ascii_ci`, `repeat`, `fold`, `len`, `is_empty`, `find_index`, `find`, `try_get`, `get`, `join` and `trim`.** It composes and walks; it does not decompose. Its bytes are UTF-8 under a validity proof, and UTF-8 bytewise order is scalar-value order, so a code-point comparison needs no decoding.
- **`/std/Option` and `/std/Result` have `bind`, `map`, `unwrap_or` and their predicates; `/std/Vec` has `len`, `append`, `map`, `first` and `rest`.** `Vec` cannot be indexed, folded, zipped or built from a list. Indexing a `Str` or `Bytes` takes `(i: Nat, @ok: Nat/Lt(i, len))` with the bound discharged by reduction, which is the shape a `Vec` index takes.
- **Symbolic `Nat` arithmetic is largely definitional.** `n + 0 = n`, `0 + n = n`, `(a + b) + c = a + (b + c)`, `a + b = b + a`, `a * b = b * a`, `a * (b + c) = a * b + a * c`, `n * 0 = 0`, `n - n = 0`, `(n + m) - m = n`, `n * 2 = n + n`, `n / 1 = n`, `n < n = false`, `n < n + 1 = true` and `Nat/Le(n, n + m)` all close by `Eq/refl()` or `True/qed()`. `n < m = not(m <= n)` and `Nat/Le(x / 256, x)` do not. The lemma corpus Rocq's `Arith` and Agda's `Data.Nat.Properties` carry is therefore mostly unnecessary; what is missing is what reduction cannot do — an induction principle stronger than the successor's — and `pow`, `gcd`, `lcm`, `log2`, `sqrt` and `Int/sign`, which every peer has and `/std` does not.
- **There is no `Set`, and `Map`'s keys are `Bytes` through `Key`.** `Map.crs` records that `Key(Nat)` was attempted and abandoned because its base-256 encoding recursed on `x / 256` and "intrinsic `Nat` eliminates only by unary successor, so no induction principle reaches that recursion". `Map` has `get`, `has`, `insert`, `remove`, `fold`, `entries`, `keys`, `values` and `of`, and no update, map, filter or union.
- **Well-founded recursion can be declared, and the analysis now reads the descent a proof needs.** An accessibility predicate declared as `induct Accessible(@A: Type, R: (A, A) -> Prop): (A) -> Prop | intro(@x: A, below: (y: A, r: R(y, x)) -> Accessible(R, y)): (x) end` is accepted, passes positivity, and a fixpoint over it elaborates at a `Type`-valued result. At a `Prop`-valued result the same fixpoint was refused — `a proof in '/strong' is a proof position but reaches '/strong', which is not known to terminate` — because the recursive call goes through `below(m, lt)`, a function-typed constructor payload, which size-change termination did not count as a decrease. It does now: an application of a constructor payload reads as the payload, and `documentation/soundness/whole-module-passes/record_totality-t.md` records the rule and its probes. Programs never needed the predicate — general recursion is unrestricted where erasure keeps the result — and proofs, which do, can use it; the library half is item seven.
- **The host ABI has one filesystem row, no process row and no terminal row.** `curios-abi`'s table (`curios-abi/src/host/ops.rs`) holds `file/open` over a `Mode` of read, write-truncating or append, the handle rows `read`, `write`, `poll` and `close`, the socket, DNS and TLS rows, `clock/wall`, `clock/mono`, `rand/bytes`, `proc/args` and `proc/env`; `/std/File` wraps the first into `open`, `with`, `read`, `write`, `close` and `read_all`, and `/std/proc` holds `args`, `env` and `exit`. Every row is implemented four times — the table, `/sys`'s mirror, `OsHost` with `MockHost` beside it, and the browser harness, where `open` answers `permission_denied` and `args` is unsupported. A `Handle` is an fd token, so a pipe from a child would be read, written, polled and closed by rows that already exist. The named status codes are `ok`, `eof`, `not_found`, `permission_denied`, `exists`, `refused`, `tls` and `would_block`, with an errno passthrough lane below them that the browser cannot fill.
- **Every library that has a host surface agrees on the same short list.** Remove a file; rename a file or directory; create a directory; remove an empty one; list one by name; ask whether a path exists and whether it is a file or a directory; read the working directory; write a whole file in one call; run a program with an argument list, wait for it, and read its exit code with its output captured or inherited. WASI preview 1 has every one of these except the working directory, which it replaces with preopens, and process creation, which it has none of — the browser harness's answer too. Beyond the list agreement stops: `chdir` is missing from `simplifile`; seek from Idris; exclusive create from Lean, Haskell and Idris; recursive create and remove from Idris and OCaml; symbolic links from Idris; a temporary directory from Zig and Idris; a shell-string runner from Lean, Rust and Zig; and killing a child from Idris, Agda and Gleam.
- **Nothing sets a terminal mode or reads a window size, and stdin is served through a buffer `poll` cannot see.** Raw mode and the terminal's dimensions are unreachable from Curios by any spelling. `curios-runtime/src/os_host.rs` builds a `PollFd` for `Handle::Stdin`, so a fiber can wait for stdin to become readable, and `set_nonblocking` on it is a recorded no-op — the setter answers `Ok` without touching fd 0, because the flag would change every other user of the descriptor. A read on stdin is `stdin().lock().read(&mut buffer)`, Rust's shared buffered reader: a request smaller than what arrived leaves the remainder in a buffer `poll` cannot see, so a loop that waits on fd 0 and then reads a small count can stall with input already inside the process. That is a finding for the runtime, unprobed, and the runtime's answer is to serve handle reads on stdin unbuffered; until it does, a reader asks for a chunk at least as large as that buffer. The native host restores no state at a trap today, and where `OsHost` is dropped relative to `instantiate`'s trap path is unread — item ten's termios record would be the first host state with an exit obligation. The release targets are Linux and macOS (`.github/workflows/release.yml` builds two), so there is no Windows console to serve and termios is the whole of raw mode. The browser harness holds stdin at EOF (`curios-js/src/harness.js`), so a terminal program cannot run there whatever the rows answer.


## The items, in landing order

Cheapest and most consumed first. Each item depends on the ones before it and on nothing after. Items eight to ten are the host half: they depend on nothing above and nothing above depends on them, and they are appended rather than interleaved because a row costs four implementations where a library function costs one.

### 1. Ordering witnesses

`Cmp` and `Ord` on `Str`, `Bytes`, `Bool`, `List`, `Option` and `Result`; `Eql` and `Show` on `Vec` and `Map`. `Cmp(Str)` is `Bytes` comparison, justified by the one line above: UTF-8 bytewise order is scalar-value order, so `sort` on strings means code-point order and decodes nothing. `List` compares lexicographically, `Option` with `none` below `some`, `Result` with `failure` below `success`, `Bool` with `false` below `true`, each stated once in its module. Everything below sorts on this.

### 2. `List`

The functions every peer has: `reverse`, `filter`, `filter_map`, `any`, `all`, `zip`, `unzip`, `take`, `take_while`, `drop_while`, `partition`, `contains`, `index_of`, `find_index`, `last`, `range`, `replicate`, `concat_map`, `intersperse`. Three that structure the rest:

- **`fold_until`**, a left fold whose step returns `Step/continue(acc)` or `Step/stop(acc)` and ends at the first stop. `any`, `all`, `contains`, `index_of` and `find_index` are each one line over it, and the `Nat/Lt` invariant `find` carries today is proved once. `Step` is its own inductive rather than `Result`, because a stop is not a failure.
- **`traverse` and `each`** over any `Monad`: `traverse(@M, use Monad(M), l, f: (T) -> M(B)) -> M(List(B))`, and `each` discarding the results. Plain functions, one per container, not a concept.
- **`sort` over `Ord` and `sort_by` over a comparator**, a stable merge sort, with `List/Sorted` — the proposition that a list is in order under `Ord` — and beneath it the proof that `sort` yields one. `Sorted` is stated over adjacent pairs, as Rocq's `Sorting` and Lean's `List.Sorted` state it, so the merge proof needs no law `Ord` does not carry: totality is built into `Order`, and transitivity is what gives the adjacent statement its meaning, not what the proof consumes. It is the first proof in `/std` about a collection rather than a carrier; the permutation half of a certified sort is not in it.

`fold` keeps its name and its `(elem, acc)` order, and `List.crs` gains the line stating its direction. No `fold_right` is added: the `; ih` arm is the right fold, and a library twin would spend a stack frame per element on what the syntax already optimizes.

### 3. `Str` decomposition

`split`, `split_once`, `starts_with`, `ends_with`, `contains`, `replace`, `lines`, `to_list`, `of_char`, `trim_start`, `trim_end`, `pad_start`, `pad_end`. Every result is a `Str` carried under the validity proof `slice` already threads; nothing re-validates.

### 4. `Option`, `Result` and `Vec`

`Option`: `or`, `or_else`, `filter`, `flatten`, `to_result`, `unwrap_or_else`, and `get(o, @ok: IsSome(o))` with `IsSome` reducing to `True` or `False` by a match — the shape `Nat/Lt` has. `Result`: `to_option`, `unwrap_or_else`. `Vec`: `get(v, i, @ok: Nat/Lt(i, n))`, `to_list`, `of_list`, `replicate`, `zip`, `fold`. `of_list` returns the dependent pair `{n: Nat, Vec(T, n)}`, since the length is not known statically; it is not an `Option`.

### 5. Numerics and strong induction

`Nat`: `pow`, `gcd`, `lcm`, `log2`, `sqrt`, `is_even`. `Int`: `of_nat`, `min`, `max`, `sign`. And `Nat/Lt/strong`, course-of-values induction — `strong(P, step: (n, ih: (m, Nat/Lt(m, n)) -> P(m)) -> P(n), n) -> P(n)` — proved by ordinary induction on a bound `k` under `Nat/Le(n, k)`, which needs no new judgment. It is named under `Nat/Lt` because the hypothesis it hands the step is `Lt(m, n)`; the bound it descends on is the mechanism, not the principle. It is what item six's `Key(Nat)` proof uses, and what any proof along `x / 256` uses until item seven.

### 6. `Set` and `Map`

`Set` is a struct over `Map({})` and mirrors `Map`'s surface with the value elided: `empty`, `len`, `has`, `insert`, `remove`, `fold`, `to_list`, `of`. `Map` gains `update`, `map`, `filter`, `union` and `get_or`, and `Set` gains `union` with it; set algebra exists exactly where `Map` has the counterpart, so there is no `intersect` or `difference` until a `Map` consumer wants one. `Key(Nat)` lands here, its injectivity proved by item five's `Nat/Lt/strong`, so numeric sets and maps work.

### 7. `/std/WellFounded`

The module's namesake is `WellFounded(R)`: every element is accessible under `R`, the constructive definition Rocq, Agda, Idris and Lean all use, and the one that is the induction principle. Beneath it, `WellFounded/Accessible(R, x)` as the inductive predicate above, `WellFounded/recurse` as the fixpoint, and `WellFounded/lt` proving `<` on `Nat` well-founded. The names are chosen over `Acc` because `/std` names its propositions as adjectives about the thing they qualify — `Finite`, `Valid`, `Canonical` — and `WellFounded` is the claim a user makes about their own relation.

The item's second half is in the compiler and has landed: totality counts a call as decreasing when its argument is a constructor payload bound by the pattern on the scrutinee, applied to anything. That is the rule under which `recurse` is usable in a proof position; it lives in `curios-analysis`, so both drivers and the kernel's re-check run it. What remains is the library half above; item five's `Nat/Lt/strong` serves the proofs that do not want a relation.

### 8. Filesystem

Seven rows in the wire vocabulary the table already has, each a `Status`-bearing row over `Bytes` paths as `file/open` is, mirrored into `/sys` and implemented by `OsHost`, `MockHost` and the browser harness, which answers `permission_denied` exactly as `open` does today:

```
stat        as file/stat   [path: Bytes] [status: Status, kind: Nat, size_hi: Nat, size_lo: Nat, mtime_hi: Nat, mtime_lo: Nat, mtime_nanos: Nat];
remove_file as file/remove [path: Bytes] [status: Status];
rename      as file/rename [from: Bytes, to: Bytes] [status: Status];
list        as dir/list    [path: Bytes] [status: Status, names: ListBytes];
create_dir  as dir/create  [path: Bytes] [status: Status];
remove_dir  as dir/remove  [path: Bytes] [status: Status];
cwd         as proc/cwd    [] [status: Status, path: Bytes];
```

`kind` is a tag for file, directory, symbolic link or other. The size and the modification time are split as `clock/wall` splits its seconds, so `/std/time` decodes them and the i31 envelope is never asked to hold a file size. Three named codes join the status table — `not_empty`, `is_directory` and `not_directory` — because a program removing a directory has to tell them apart portably, and the browser has no errno to pass through; every other failure stays in the passthrough lane.

The library half is `/std/fs`, lowercase as `proc`, `time` and `tcp` are, since it names a host service rather than a type: `stat` returning a `Metadata` struct whose `Kind` is the tag above; `exists`, `is_file` and `is_dir` as one-liners over it, reading `not_found` as `false` and every other failure as the failure it is; `list` returning `List(Str)`; `create_dir`, `remove_dir`, `remove_file`, `rename` and `cwd`; and `create_dir_all` and `remove_all`, written over the rows with no row of their own, since a majority ships them and the host pays nothing for them. `/std/File` gains `write_all`, the twin `read_all` has lacked. Four pure functions over `Str` sit in `fs` beside the rest, because a listing returns names and opening one needs the join: `join`, `parent`, `name`, `extension`. The release targets are Linux and macOS, so the separator is `/` and the functions say so.

### 9. Subprocess

Three rows:

```
spawn as proc/spawn [program: Bytes, args: ListBytes, cwd: Bytes, env: ListBytes, stdin: Nat, stdout: Nat, stderr: Nat] [status: Status, child: Handle, stdin: Handle, stdout: Handle, stderr: Handle];
wait  as proc/wait  [child: Handle] [status: Status, code: Nat, signal: Nat];
kill  as proc/kill  [child: Handle] [status: Status];
```

Each standard stream takes a tag of inherit, pipe or null — the shape Lean's `Stdio`, Haskell's `StdStream`, Rust's `Stdio` and Zig's `StdIo` share — and a stream that is not piped comes back as the empty handle a failed `open` already returns. A piped stream is an fd, so `read`, `write`, `poll`, `close`, `Async/nonblocking` and `Async/wait` work on it unchanged. An empty `cwd` inherits the parent's; `env` is a list of `NAME=VALUE` entries laid over the inherited environment, which is `execve`'s own shape. The child handle is pollable: the native host reaps on a thread and signals a pipe whose read end is the handle, the pattern `dns/lookup` already uses, so the scheduler waits on a child with the `wait` it has and the deadlock detector sees a fiber blocked on a handle. `wait` after the handle is readable returns at once with the exit code, or the signal that ended the child.

The library half lives in `/std/proc`, which already holds `args`, `env` and `exit`: a `Command` struct of program, arguments, an optional working directory, environment overrides and the three stream settings, with `Command/new(program, args)` as the all-inherit default and the rest written as struct updates; `spawn(cmd) -> Async(Result(Child, Error))`, the child acquired with `kill` as its finalizer so a cancelled task kills what it started, and `Child/wait` releasing it; `Child/stdin`, `Child/stdout` and `Child/stderr` as the piped handles; `Exit` as `exited(Nat)` or `signaled(Nat)`; `run(cmd) -> Async(Result({exit: Exit, stdout: Bytes, stderr: Bytes}, Error))`, capturing both outputs by draining both pipes under `select`, since draining one to its end while the other fills is the deadlock every `process` library documents; and `status(cmd) -> Async(Result(Exit, Error))`, inheriting everything.

### 10. Terminal

Two rows, found necessary by [the terminal specification](terminal-ui-spec.md) and stated here because every host row of this campaign is:

```
raw  as tty/raw  [h: Handle, on: Bool] [status: Status];
size as tty/size [h: Handle] [status: Status, cols: Nat, rows: Nat];
```

`raw(h, true)` records the descriptor's termios on first use and applies the raw settings — no canonical mode, no echo, no signal keys, no output post-processing, `VMIN` 1, `VTIME` 0; `raw(h, false)` restores the record. The native host also restores the record when it is dropped, so a trap or an `exit` leaves the terminal usable; a status of `other(ENOTTY)` is how a program learns it has no terminal. `size` is `TIOCGWINSZ`. Both are `Status`-bearing rows over the existing slot vocabulary, so the change is the table, `/sys`'s mirror, `OsHost`, `MockHost` and the JavaScript harness — the four-place obligation the ABI invariant states — and nothing more. The browser answers both with `permission_denied`, as it answers items eight and nine.

This item has no library half of its own. The rows are reached as `/sys/tty/raw` and `/sys/tty/size`, and what wraps them — the session bracket, the decoder, the renderer and the loop — is the terminal specification's to shape, written over `/sys/tty` directly.

Rejected: learning the size by writing `CSI 18 t` and parsing the reply from stdin, which needs no row but leaks the reply to the inner shell under tmux and ssh and puts a parser in the path of every keystroke. Rejected: a resize signal row, which would be the first signal in the ABI and carries the race that in-band resize reports were introduced to remove; a library polls `size` on a tick instead, and the interval is the terminal specification's to choose.

## Decisions taken

- Sort lands stable, with `Sorted` and the proof that it sorts. The permutation statement is a later item with a consumer.
- `Set` is `Map({})` and mirrors `Map`. An `Ord`-keyed tree waits for a consumer with non-`Bytes` keys.
- `traverse` is a function per container. No `Functor`, `Applicative`, `Traversable` or `Monoid` concept: `map` is per type, `/sys/List/map` is an intrinsic, and a concept costs resolution on every call for no consumer.
- `fold` keeps its name and order; `fold_until` uses `Step`; no `fold_right`.
- `Vec/get` and `Option/get` take a decided-proposition bound, as `Str/get` does; `Vec/of_list` returns a dependent pair.
- `Nat/Lt/strong` lands before `WellFounded`, and the totality rule is this campaign's, not a blocker deferred elsewhere.
- A capability every library reaches by one of two mechanisms is in the tier by one of them. Every library runs a child in another directory — half through `chdir`, half through a spawn argument — and the argument is the one that survives fibers, so there is no `chdir` row. The same holds for a child's environment: half set the process's, half pass a table to the spawn, and the spawn takes it.
- A listing returns names, not entries. Five of the seven return names and leave the kind to `stat`; the system call per entry is the accepted cost, and a `kind` beside each name is the row a consumer asks for.
- A name that is not UTF-8 fails the listing. The row carries `Bytes`, so a consumer that wants raw names is served without a new row.
- `stat` follows symbolic links, and reports the `symlink` kind only where the target is missing. A `stat` that does not follow is a second row with no consumer.
- A path is a `Str`, as `File/open` already takes. No `Path` type: the four functions in `fs` are what a listing needs, and a type would owe a witness roster before its first consumer.
- Cancellation kills the child. `Async` releases what a task acquired on both exits, and a child that outlives the task that spawned it is the one thing that rule cannot mean.
- The terminal's size is a row, not an escape query, and resize is polled, not a signal: the tmux and ssh reason for the first, and the race the second would reintroduce, are stated under item ten.
- The browser harness answers every row of items eight to ten with `permission_denied`, as it answers `open`, rather than gaining a virtual filesystem or a terminal: WASI has no process creation either, and the harness's job is to run the playground.

## Deliberately not specified

The permutation half of a certified sort. `Dec` keeping the refutation beside `Option(P)`. Derivation slots for `Ord`, `Show` or `Draw`. `Functor` and its family. The ASCII predicates on `Char` under Unicode names. The JSON and TOML number carriers, which are decided together beside the TOML specification. The two tiers below this one. Spellings beyond the ones the items fix. Performance: a `sort` written over `fold` is correct first.

Of the host surface, everything the second survey found in most libraries and not all, each with the library that lacks it named above: `chdir`; seek, and with it the read-write open modes that are only useful with one; exclusive create; symbolic links, in either direction; a temporary file or directory; permissions and settable timestamps; a shell-string runner, which is `sh -c` over item nine for the program that wants one; the process's own id and executable path; an environment setter, which item nine's overrides cover for the consumer that had one; a `Path` type; and a child whose exit is observed without a thread per child, which is a native-host optimization with no row.
