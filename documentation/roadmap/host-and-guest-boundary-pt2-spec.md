# Host and guest boundary, part 2: host operations and outcomes

Pending implementation specification, building on [guest coordination](../design/language/guest-coordination-uses-write-once-cells-and-bounded-channels.md) and the existing [ordinary `/sys/Option`](../../curios-prelude-archive/README.md#optional-values-belong-to-the-guest-foundation). Give each host operation one precise contract and enforce it through checked host and guest adapters around a small wire vocabulary. This part moves `Result` into `/sys` and changes the host boundary. The architecture, the operation decisions and every row's contract are settled below; the checkpoints implement them in order.

## Scope and foundations

The authored operation roster in [`curios-abi/src/host/ops.rs`](../../curios-abi/src/host/ops.rs) already projects wire signatures and `HostOps`. It prevents disagreement about slot order and types, but does not fully specify argument domains, progress, resource transitions or ownership. Native, mock and browser implementations can agree on a signature while behaving differently. Fallible replies also put status and placeholder payloads beside one another, leaving standard-library wrappers to interpret them repeatedly.

Extend that existing ownership boundary. Preserve the wire's scalars and arrays where they express the operation's contract; add `Byte`, explicit divergence, a flush row and checked adaptation. Keep Core foreign calls wire-shaped and construct ordinary guest outcomes in `/sys`. Move `Result` into `/sys`, reusing the existing `Option`; retain `Io/Error`, `Io/Chunk`, modes, readiness records, child exit vocabulary and other domain types in `/std`.

This is not a general serialization framework, a guest-type schema, a new Core outcome calculus or an adoption of the WebAssembly Component Model. Ordinary user foreign declarations retain their wire-shaped signatures. Plugins remain limited to their supported scalar and byte-string vocabulary, with `Byte` added. The browser's public `compile` and `run` change shape, and a Node-run browser suite joins the gate. Lightweight processes, cross-instance transport, selection fairness, TLS `close_notify` and half-close, and repository-wide maintenance remain outside this effort. Changes required by an operation's reviewed contract are in scope; unrelated fixes are not.

## One contract, enforced at its owning seams

Each builtin has one canonical identity whose contract fixes its signature and checks. A term cannot attach a weaker contract to that identity, because a term carries only the identity. User foreign functions retain their declared wire contracts and cannot impersonate builtin operations.

| Contract dimension | Required content | Enforcement |
| --- | --- | --- |
| Arguments | Wire shapes, closed codes, scalar ranges and relationships between inputs | Checked decoding and named operation checks |
| Outcomes | Returning or diverging; permitted success, absence, EOF and failure cases | Host encoding and guest reply validation |
| Payload | Active fields, bounds relative to the request and successful result shape | Encoder checks and guest validation |
| Resource transition | Creation, preservation, replacement or consumption for each outcome | Host implementation and shared behavioral fixtures |
| Ownership | Copies, borrows or transfers, including when a buffer may be retained or changed | Adapter APIs and their implementations |
| Violation behavior | Ordinary unsuccessful request, malformed boundary value or compiler invariant failure | The layer owning that condition |

Keep mechanically consumed facts in the ABI roster and a small closed supporting vocabulary. Derive signatures, outcome conventions and mechanical adapter structure from those facts. Put operation-specific behavioral clauses beside the row and implement relational checks as named checks, rather than inventing a constraint language. Platform errno mappings and operating-system resource handling remain in the runtime. Binding generation must respect crate dependency boundaries; generated agreement does not require Wasmtime types in `curios-abi`.

Representation checks establish valid values, per-call checks establish relationships such as `written <= input_length`, and behavioral tests establish resource transitions. None proves that an arbitrary host told the truth about the outside world. State the host assumptions in the soundness documentation instead of claiming that validation verifies external effects.

## Builtin identity and signatures

`host_ops!` generates `HostOp`, an archived `Copy` enum with one variant per row, carrying `ALL`, `name`, `subject`, `label`, `signature`, `description`, `contract` and `diverges`. `ForeignFunction` becomes `Builtin(HostOp)` or `Declared(DeclaredForeign)`, read through accessors; only a declared `ffi` row carries its own signature. A builtin's signature and contract are read from the roster wherever they are needed, so no copy exists to disagree with it: equality, interning, linking and cache admission need no agreement check, because a conflicting description cannot be written. `host_ops()` remains the store of every builtin, and the runtime links by the same identity.

Both checkers type a foreign call through a `Signature` in the vocabulary `Intrinsic::signature` already uses. `curios-core`'s `foreign_signature` states a returning row's operands as `Operand::At(wire_term(..))` and its result as `Produced::Fixed(Io(wire_results_term(..)))`; the diverging exit row states `[Operand::IsType, Operand::At(Byte)]` producing `Io(A)` for the type operand `A`. The kernel and the elaborator walk it with the operand handling they already share with intrinsics, and erasure drops the type operand as it drops an intrinsic's. The kernel keeps refusing a foreign call with the wrong number of operands.

## Host adaptation

The checked host interface presents meaningful arguments and outcomes. `HostOps` methods return outcome types generated from each row: `Result<Handle, Failure>` for opening, `Result<Option<Vec<u8>>, Failure>` for reading, `Option<Vec<u8>>` for environment lookup, `Result<(), Failure>` for a status-only row, tuples for several success fields, `ChildExit` for a child's end and `Termination` for exit. These are schematic shapes around ordinary buffers, not new wrapper types for them. Use existing domain types where they establish a useful invariant; do not generate guest declarations from the Rust host vocabulary.

`Failure` contains failure cases only: neither success nor EOF. The adapter translates those cases through the existing status encoding. Only the adapter manufactures raw padding. A nominal Rust type alone is insufficient: a successful handle must not be the absence token, counts must fit their operation's bounds, and enum or flag constructors must not bypass their declared domains. Enforce these through constructors or encoder checks at the narrowest useful owner.

`Lift` and `Lower` each state the wire shape they decode or encode, and `ForeignBindings::define` asserts that shape against the row it binds, for builtin and embedder bindings alike. `bool` replaces `u32` for a `Bool`. Closed argument codes decode into their own types — `Mode`, `StdioMode`, `SerialParity`, `SerialFlow`, `SerialOp` — and a poll's interest bytes decode as masks within `read | write`. Decode and check raw arguments before invoking the host method. Keep raw binding access internal to the runtime.

A malformed representation or closed code causes boundary refusal of the guest invocation, not an embedding-process panic or a fallback value. A valid request that the host cannot fulfil returns the operation's documented failure. Resource existence and resource-kind checks belong to the host. Closed mode tags and platform-supported numeric settings are different domains: an unknown enum code is malformed, while a validly represented setting the platform cannot support follows the operation's failure contract. An outcome encoder that finds its own host's reply outside the row's contract refuses the invocation: that is a host implementation fault.

An inconsistent compiler-owned contract is an invariant failure. An actual host or operating-system failure must not be presented as such merely because a method lacks a failure result. For an operation without a failure lane, the invocation-failure policy is a refusal naming the operating-system error. Do not silently invent success, empty data or inactivity.

## Wire and invocation behavior

### `Byte`

Add `Byte` to wire types, scalar shapes, slot mappings, foreign parsing, signatures and marshalling. It uses an `i32` lane. Incoming values must be in `0..=255` before conversion or boxing: reject negative values and values above 255 without truncation. Cover native codecs, builtin dispatch, mock hosts, plugins and browser foreign calls. Keep `List(Byte)` unsupported and preserve existing scalar and byte-string behavior.

Audit existing scalar and aggregate conversions as part of the same boundary work. In particular, Boolean decoding must establish `0` or `1`, including lists and plugin results; narrowing cannot silently change values. A count representable on the wire does not automatically fit a platform allocation or array length. Check size conversions and allocation arithmetic, and apply operation-specific allocation policy.

### Diverging exit

Register `proc_exit as proc/exit` with a `Byte` parameter, no wire results and explicit divergence. Divergence is distinct from an ordinary returning zero-result operation. Generate `(@A: Type, code: Byte) -> Io(A)`; both checkers and the certifier's partiality obligations read divergence from `HostOp::diverges`. `Intrinsic::ProcExit`, the `EXIT` import constant and their compiler and runtime branches are deleted.

Below Core, a diverging call is a terminator of its own: `Terminator::Halt` in the erased IR and `Node::Halt` in the continuation IR replace `Exit`, as distinct variants rather than a returning call with an optional continuation, so every pass decides it explicitly. The emitter calls the row's import with no result reconstruction and then refuses with `Panic::HostReply`: a host that returns from exit has broken its contract and cannot resume the guest. Native dispatch reports typed termination with a `u8` code through the existing guest-exit mechanism: `Lower` for `Termination` produces the exit trap, so no Rust host can return into the guest. Browser dispatch implements the same row. Neither terminates the embedding process. Panic remains the emitter-only import outside the store.

### Physical validity and selected payloads

The guest adapter lives in `curios-emit`'s host-call lowering, because Curios source cannot refuse. It captures the raw results, checks physical representations, validates the discriminator and checks the active payload before resuming guest code. Every foreign row, `ffi` included, has its physical representations checked: a `Bool` is `0` or `1`, a `Byte` is at most 255, and a `Bool` list's elements are checked. A builtin row additionally has its status checked against the row's permitted set and the errno range, its outcome's payload rules applied on the success branch only, and its named checks. Any violation refuses with one new refusal class, `Panic::HostReply`, whose sentence says the host answered outside its contract.

Preserve primitive representation invariants even for inactive fields; ignoring a failure's payload semantics does not permit malformed references or invalid Boolean values. Conversely, do not reject a legitimate failure because its padding fails a success-only condition such as nonempty handle or positive progress. Encoders supply canonical physically valid padding.

All argument conversion and reply validation executes inside the foreign `Io` action. Merely constructing that action performs no conversion, refusal or host effect. The executed call retains its validation even when its result is unused. Audit optimizer effects, verifier assumptions and emission together so validation cannot be discarded, hoisted or separated from the call it protects.

Boundary refusal does not undo an external effect that already occurred. It does not authorize automatic retry. Resource cleanup on invocation failure follows the runtime's stated lifecycle policy, not an implied transaction around every host call.

## Core, `/sys` and `/std`

Returning Core foreign calls keep signatures derived from `WireSignature`: zero results give unit, one gives its bare carrier, and multiple give a record of raw fields. The canonical builtin contract supplies validation; Core terms do not carry guest wire handlers or a generated semantic outcome language. Divergence adds the checked and erased type operand described above.

Generate `/sys` adaptation as ordinary AST using the `IoBind` and `IoPure` intrinsics, `NatEql` against `/sys/status` codes, `Bool` matches, record projections and the registered `Result` and `Option` constructors. It must not depend on `/std` monad witnesses, overloaded operators or declaration-order accidents. Inspect status before projecting successful payloads into guest bindings. A checked raw return is not converted to a custom compiler IR merely to build `Result` or `Option`.

| Operation class | Core result inside `Io` | `/sys` result inside `Io` |
| --- | --- | --- |
| Fallible, no success payload | Raw status `Nat` | `Result(Nat, {})` |
| Fallible, one success payload `T` | Record containing status and payload | `Result(Nat, T)` |
| Fallible, several success fields | Record containing status and labelled fields | `Result(Nat, {labelled success fields})` |
| `Handle/read` | `{status: Nat, bytes: Bytes}` | `Result(Nat, Option(Bytes))`, with `none` for EOF |
| `Handle/write` | `{status: Nat, written: Nat}` | `Result(Nat, Nat)` |
| `proc/env` | `{status: Nat, value: Bytes}` | `Option(Bytes)`, with `none` for absence |
| `proc/stream` | `{status: Nat, handle: Handle}` | `Result(Nat, Handle)`; an unpiped stream is a failure |
| `proc/exit` | Diverges | `Io(A)` for any `A` |
| Ordinary infallible rows, `Handle/poll` included | Existing raw shape | Existing shape |

For example, opening yields `Io(Result(Nat, Handle))`, flushing `Io(Result(Nat, {}))` and terminal size `Io(Result(Nat, {cols: Nat, rows: Nat}))`. Child waiting retains numeric code and signal payload fields with their active-branch contract; this specification does not move or redesign the guest's child-exit induct.

Declare the ordinary polymorphic family directly at `/sys/Result`, preserving its parameters, universes and constructor order, `success` then `failure`. Re-export it through `pub use /sys/{Result};` and `pub use /sys/Result/{success, failure};` before ordinary imports in `/std/Result`, without globs or another module nesting. Keep functions, witnesses and derivations in `/std`. Resolve compiler identities through the syntax registry, validate actual declaration shapes, instantiate through normal machinery and rebuild both archives. Reuse the existing approach and regression coverage for [`Option`](../../curios-prelude-archive/README.md#optional-values-belong-to-the-guest-foundation) and the existing polymorphic `List` implementation.

`/std` retains `Io/Error`, `Io/Chunk`, modes, interest/readiness records, serial vocabulary and child exit types, together with their conversions, convenience operations and retry policies. The ABI owns code constants; `/std` cites them rather than duplicating numeric literals. Cover these remaining conversions with independent code-value fixtures so keeping them simple does not leave them unchecked.

Migrate every affected wrapper to the new outcomes, preserving EOF, would-block retry, partial progress and cleanup according to the contracts. Successful read data constructs `Chunk/chunk(bytes)` directly; EOF constructs its EOF case; failure codes are decoded separately. Delete the status-plus-payload `Io/Chunk/of`. `Io/Error/of` becomes `Nat -> Error` and stays total over every `Nat` a caller can fabricate: named failure codes map to their cases and every other code to `other(code − other_base)`, so the two non-failure codes read as the errno-less `other(0)` the host already reports. The guarantee on a particular `/sys` result does not make `Nat` a refined failure-code type, and no new guest error family is added to bypass that.

The `Async/Write` concept gains `flush`: `Handle`'s witness calls the flush row, and `Output`, `File`, `Serial` and `Socket` forward to it. `tcp/Socket/with` and `tcp/Listener`'s per-connection bracket flush before closing, so a response written just before close is not lost.

`Command` joins each environment pair as `key=value`, and the host splits an entry at its first `=`, so a key that is empty or contains `=` would set a different variable than the one named. `Command/spawn` refuses such a key with `EINVAL`, as POSIX `setenv` does, before anything crosses the wire; the wire keeps its joined entries.

## Ownership and resource contracts

Checked host callbacks must not gain a retained mutable alias to guest immutable data. Arguments are copied out of the guest before the host acts and results are copied in after; no host retains a reference to guest memory, and a returned buffer is the guest's alone. Native adapters already copy. In the browser, `compile` returns `{ program, foreigns }`, the program's `ffi` signatures as plain data beside the module, as the native bundle carries its `ForeignStore` beside its payload; `run` takes that object and checks that every `ffi` import has a signature and every signature an import. Checked browser hooks receive copied `Uint8Array`s, `BigInt`s, numbers and booleans, and return values the harness validates and copies in. Raw Wasm-reference hooks are removed.

Handle identity is exact token identity. The browser compares tokens as their exact bytes and never through JavaScript number conversion, so the empty token, padded encodings and large tokens stay distinct. Preserve the native distinction between the empty absence token and stdin's token. No successful result is ever the empty token. A token's encoding alone does not prove that it names a live resource of the required kind.

The host keeps one raw operating-system identity the system recycles: a child's pid. Every other resource it names is held by an owned descriptor or by a token that is never reused. So the reaper waits with `waitid(..., WEXITED | WNOWAIT)`, which leaves the exited child a zombie whose pid stays reserved, then reaps it under the child's state lock and records the outcome. `proc/kill` takes the same lock: it signals only a child with no recorded end, whose pid is still reserved, and answers `ok` without signaling once an end is recorded. The runtime states the assumption this rests on: the embedding process leaves `SIGCHLD` at its default and reaps nothing itself.

Every fallible step of spawning precedes the child: pipes are created with the parent's ends non-blocking and close-on-exec, the reaper's pipe exists, and the reaper thread is started and waiting for the child to be handed to it. Once the child exists nothing can fail, so there is no cleanup path to get wrong. A failed `wait` is recorded, and `proc/wait` reports it as a failure rather than a successful zero exit. `Child/with` closes the child handle after killing, and the reaper still reaps the process. The mock host models children that run until killed as well as children that have already ended.

Use the existing resource-table mechanisms; do not introduce a general resource framework.

## Operation decisions

**Write progress.** One progress-reporting attempt per call, with the same contract for every handle, the standard streams included. A nonempty success accepts between one byte and all of them; a failure accepts nothing from that attempt; an empty write validates the handle and succeeds with `0`. A host that can accept nothing now answers `would_block` — rustls reports a full send buffer as zero bytes accepted, and that becomes `would_block`. A descriptor that accepts nothing and reports no error fails with the errno-less `other(0)`. An interruption before any progress is retried inside the host. Whole-buffer writes are composed in `/std`, whose `Io` loop never yields mid-message, so `print` cannot interleave with another fiber.

**Buffered-output completion.** A write's success means the host accepted the bytes. The standard output streams are written through their raw descriptors, so the host holds no buffer for them. `Handle/flush` drains whatever the host still holds for a handle: a TLS stream's pending records, answering `would_block` until rustls holds nothing and waking through the write readiness the TLS interest already reports; every other kind answers `ok` at once. Flushing promises that the host's own buffers are drained, not peer receipt or durable storage. Close stays non-blocking and discards what was never flushed; `/std` brackets over buffering streams flush before they close.

**Poll-wide failure.** A malformed call — unequal array lengths, or interest bits outside `read | write` — is refused. A failure of one handle — unknown, closed, a resource with no descriptor, or a descriptor the operating system reports invalid — reports `err` for that slot; the scheduler already wakes a waiter on `err`, and the committing call then reports `not_found`. An interruption is retried inside the host against a deadline taken at entry, so `Handle/poll` keeps meaning "something is ready, or the timeout passed". Each distinct descriptor is polled once and its readiness copied to every slot naming it, which removes the descriptor-limit `EINVAL` route. Any remaining mechanism failure refuses the invocation, naming the operating-system error. The browser cannot sleep inside a synchronous import, so its ignored timeout is a documented platform difference.

## Operation contracts

These clauses apply to every row:

- **Failures.** A fallible row may fail with the operating-system statuses — `not_found`, `permission_denied`, `exists`, `refused`, `not_empty`, `is_directory`, `not_directory` — and the errno lane. `would_block` appears only on rows marked *may block*, and `tls` only on rows marked *TLS*. `ok` never appears as a failure and `eof` never at all outside a read. The errno lane carries `other(errno)` for `errno` in `0..=2³¹ − 1`; a status code above `other_base + 2³¹ − 1` is malformed.
- **Handles.** An unknown handle or a resource of the wrong kind — one that cannot perform the operation at all — fails with `not_found` and preserves every resource. A stream used in a direction it is not open for — reading `stdout` or `stderr`, writing `stdin`, reading a file opened to write or writing one opened to read — fails with `EBADF` through the errno lane on every host, as native files already do; nothing invents an end of stream for it. A successful handle is never the empty token.
- **Ownership.** Every argument is copied in and every result copied out; nothing is retained across a call.
- **Violations.** A malformed argument is refused by the host. A reply outside the contract is refused by the host's encoder for its own host and by the guest adapter for any host. A row without a failure lane that the host cannot fulfil is refused, naming the operating-system error.

| Row | Outcome | Arguments | Success payload and checks | Resource transition |
| --- | --- | --- | --- | --- |
| `Handle/read(h, n)` | Stream; may block; TLS | Any `n` | `some` of `1..=n` bytes for `n > 0`, `some(x[])` for `n = 0` without I/O, `none` at EOF; the native host caps one read at 64 KiB | A TLS read drives a pending handshake |
| `Handle/write(h, b)` | Fallible `Nat`; may block; TLS | — | `1..=len(b)` for nonempty `b`, `0` for empty `b` without I/O | — |
| `Handle/flush(h)` | Fallible `{}`; may block; TLS | — | Host buffers for `h` drained | New row |
| `Handle/poll(hs, es, t)` | Returns `Bytes` | `len(es) = len(hs)`; interest bits within `read \| write`; `t < 0` waits forever | One mask per handle, bits within `read \| write \| err \| hup` | — |
| `Handle/close(h)` | Returns `{}` | — | — | Removes any kind; unknown handles and the standard streams are no-ops; unflushed output is discarded; closing a child leaves its streams filed and the process is still reaped |
| `file/open(path, mode)` | Fallible `Handle` | `mode` a closed code | Nonempty handle | Mints a file |
| `dns/lookup(host, port)` | Fallible `Handle`; may block (resolver saturated) | A host that is not UTF-8 fails `not_found`; `port > 65535` fails `EINVAL` | Nonempty handle | Mints a pending lookup |
| `dns/resolve(h)` | Fallible `List(Bytes)`; may block (pending) | — | Nonempty list | Pending preserves; completion consumes once, success or failure |
| `socket/open(addr)` | Fallible `Handle` | An unparseable address fails `not_found` | Nonempty handle | Mints an unconnected socket |
| `socket/bind(h, addr)` | Fallible `{}` | As `socket/open` | — | Unconnected stays unconnected |
| `socket/connect(h, addr)` | Fallible `{}`; may block (in progress) | As `socket/open` | — | Success connects; `would_block` leaves it connecting; failure consumes the socket |
| `socket/finish_connect(h)` | Fallible `{}`; may block | — | — | Connecting becomes connected; an already connected socket answers `ok`; failure consumes the socket |
| `socket/listen(h, backlog)` | Fallible `{}` | `backlog` clamped | — | Unconnected becomes a listener; failure preserves the unconnected socket |
| `socket/accept(h)` | Fallible `Handle`; may block | — | Nonempty handle | Mints a connected stream; the listener is preserved |
| `socket/set_reuseaddr(h, on)` | Fallible `{}` | — | — | A resource with no socket records nothing |
| `tls/start(h, sni)` | Fallible `{}`; TLS | An invalid server name fails `tls` | — | Connected becomes a client TLS stream; failure preserves the connected socket |
| `tls/server_config(cert, key)` | Fallible `Handle`; TLS | — | Nonempty handle | Mints a configuration |
| `tls/start_server(h, cfg)` | Fallible `{}`; TLS | — | — | Connected becomes a server TLS stream; the configuration is preserved; failure preserves the connected socket |
| `clock/wall()`, `clock/mono()` | Returns `{secs, nanos}` | — | `nanos < 10⁹`; a wall clock before the epoch reads zero | — |
| `rand/bytes(n)` | Returns `Bytes` | — | Exactly `n` bytes; unobtainable memory or entropy refuses the call | — |
| `proc/args()` | Returns `List(Bytes)` | — | — | — |
| `proc/env(name)` | Lookup `Bytes`: `ok` or `not_found` only | A name that is empty or contains `=` or NUL names nothing | `some(value)`, possibly empty | — |
| `proc/exit(code)` | Diverges | — | — | Ends the instance; a host that returns is refused |
| `tty/raw(h, on)` | Fallible `{}` | — | — | Records the terminal settings on the first switch |
| `tty/size(h)` | Fallible `{cols, rows}` | — | — | — |
| `serial/open(path, baud, data_bits, parity, stop_bits, flow)` | Fallible `Handle` | `parity` and `flow` closed codes; data bits, stop bits and a speed the platform cannot set fail `EINVAL` | Nonempty handle | Mints a descriptor |
| `serial/control(h, op, on)` | Fallible `{}` | `op` a closed code | — | — |
| `file/stat(path)` | Fallible `{kind, size, mtime_secs, mtime_nanos}` | — | `kind` a `file_kind` code; `mtime_nanos < 10⁹`; a dangling link reports `symlink` with zeros | — |
| `file/remove`, `file/rename`, `dir/create`, `dir/remove` | Fallible `{}` | — | — | — |
| `dir/list(path)` | Fallible `List(Bytes)` | — | Sorted names | — |
| `proc/cwd()` | Fallible `Bytes` | — | — | — |
| `proc/spawn(argv, cwd, env, stdin, stdout, stderr)` | Fallible `Handle` | Stdio closed codes; each `env` entry is `NAME=VALUE`, split at its first `=`, with a nonempty name; an empty `argv`, NUL in any string, or an entry without a name fails `EINVAL` | Nonempty handle | Every fallible step precedes the child; mints the child and its piped streams |
| `proc/stream(child, which)` | Fallible `Handle` | `which` a closed code in `0..=2` | Nonempty handle; an unpiped stream fails `not_found` | Preserves the child |
| `proc/wait(child)` | Fallible `{code, signal}`; may block (running) | — | Exactly one active: `signal = 0` and `code <= 255`, or `signal > 0` and `code = 0` | Running preserves; completion consumes once; an end that could not be observed fails with its errno |
| `proc/kill(child)` | Fallible `{}` | — | — | Signals only a child with no recorded end; an ended child answers `ok` unsignaled |

Each row's clauses move into its `host_ops!` rustdoc in the checkpoint that enforces them, and this table shrinks accordingly.

The browser keeps answering `permission_denied` for the filesystem, network, terminal, serial and process rows. `dir/list` and `dns/resolve` answer that failure instead of trapping, since both now have a failure lane; `proc/args` has none and keeps refusing the call.

## Implementation checkpoints

1. **Contracts and decisions.** This document.
2. **`Byte` transport.** Codecs, parsing, plugins and emitter lanes; `Panic::HostReply` arrives here for an out-of-range `Byte` result.
3. **Builtin identity and signatures.** `HostOp`, `ForeignFunction`'s two cases and `foreign_signature` walked by both checkers, with no behavior change.
4. **Diverging exit row.** The row, `Halt` in both IRs, totality from the contract, `Termination`, and deletion of `ProcExit` and `EXIT`, with the exit, totality, effect-perimeter and refusal decisions revised.
5. **`Result` ownership.** The declaration, registry group and explicit re-exports, without changing any row's shape; then the measurement baseline.
6. **Checked host adapter.** The contract vocabulary, outcome-typed `HostOps`, shape-checked `Lift` and `Lower`, closed-code decoding and outcome encoders, with wire and guest shapes unchanged.
7. **Operation behavior and lifecycle.** The three decisions, the flush row, reads, randomness, spawning, reaping and killing, child streams, stream direction, DNS arguments, TLS and listen transitions, serial codes, plugin results and mock parity.
8. **Guest reply validation.** The emitter's checks, the optimizer audit and a raw-reply test host.
9. **Browser.** The new `compile` and `run`, checked hooks, token identity, parity with the native contract and the `cargo x js-test` gate step.
10. **`/sys` outcomes and consumers.** Generated adapters, `Io/Error/of`, `Io/Chunk/of`'s removal, `Async/Write/flush`, bracket flushing, environment keys and every `/std`, program and test consumer.
11. **Measurement, documentation and retirement.**

Each checkpoint must be buildable and reviewed, with its focused tests plus `cargo x clippy` and `cargo x fmt` before proceeding. Update permanent documentation in the checkpoint that changes its claims. Follow [the contributor validation rules](../../CLAUDE.md#build-and-validation); run the complete hand-off gate once the implementation and documentation are final. A timeout, interrupted command or unavailable prerequisite is an outstanding check. No commit or publication is implied by this specification.

Immediately after checkpoint 5 and after checkpoint 10, measure the Tui listing fixture, a chunked standard-input-to-output copy through the scripted host and `programs/monad_io.crs` with built-in profiling, following the instrument in `curios/src/tests/coordination.rs`. Keep workload and profiling configuration consistent, record the individual measurements in the instrument's documentation, and investigate regressions before retirement.

## Verification and completion criteria

- [ ] Every row's contract clauses live in its rustdoc, and all projections and bindings agree with its canonical identity by construction. Mismatched codec shapes are rejected at registration.
- [ ] `Byte` round trips cover 0 and 255 through native, plugin and browser paths. Negative values and 256 or greater refuse without truncation. Existing scalar, list and byte-string fixtures remain green; `List(Byte)` remains unsupported. Malformed Booleans and aggregate elements are exercised independently of safe encoders.
- [ ] Exit-code, deadlock-exit and CLI `exited N` fixtures pass. Both checkers reject inappropriate totality claims; certification and proof erasure preserve divergence obligations. An exit has no resumable continuation, and a host that returns from it is refused. Source searches find no `ProcExit` or `EXIT` in implementation code.
- [ ] Existing `/std/Result` and `/std/Option` paths, constructor matching, witnesses and derived `Spell` retain their behavior. Repeated generated type occurrences, higher universes, registered shapes and consumers such as `Cli` are covered through elaboration and certification.
- [ ] Each fallible row has a scripted failure fixture; every successful payload shape has a success fixture. EOF, would-block, empty versus absent environment values, an unpiped stream's failure and both child-exit branches are covered. Inspected lowering shows failed payloads are not projected into successful guest bindings.
- [ ] Malformed-host fixtures exercise row-inappropriate statuses, closed codes, unknown bits, invalid lengths and violated request/result relationships. Failure padding remains physically valid but deliberately fails success-only constraints and is accepted. Separate fixtures refuse physically malformed inactive fields.
- [ ] Executed calls still validate discarded results; unused inert `Io` descriptions neither call the host nor perform boundary conversion. Terminal calls cannot acquire returning continuations. No optimizer removes a required boundary check.
- [ ] Read fixtures cover zero requests, positive progress, EOF, would-block and the allocation cap. Write fixtures cover single attempts, partial progress, zero progress and empty writes. The standard output streams hold no host buffer: a test running `curios run` observes stdout and stderr in the order written. Flush drains a TLS stream, and the `/std` brackets flush before close.
- [ ] Poll fixtures cover unequal arrays, interest bits, repeated handles, unknown handles reporting `err`, stale readiness, interruption and a refused mechanism failure. Native, mock and browser agree on the portable contract while documenting permitted platform differences.
- [ ] Resource fixtures cover wrong-kind preservation, pending versus completed consumption, repeated close and wait, socket and TLS transitions including preserved sockets after failed `listen` and TLS setup, DNS names and ports, spawn arguments and environment keys, reaping errors, killing a running child, and killing an ended child, which answers `ok` without a signal. A bracket that never waits releases its child. Host-returned failure never fabricates success or silently drops an unrelated resource.
- [ ] Direction fixtures on native, mock and browser: reading `stdout` or `stderr`, writing `stdin`, and using a file against its open mode each fail with `EBADF`; none reads as an end of stream, panics or traps. The browser's `dir/list` and `dns/resolve` fail with `permission_denied`.
- [ ] Ownership fixtures retain and mutate callback buffers to establish isolation. Handle tests distinguish empty, padded, standard-stream and large tokens.
- [ ] Fixed protocol fixtures pin numeric codes independently of generated expectations, including errno offsets and the declared range. Tests of `/std` conversions establish coverage without moving domain types into `/sys`.
- [ ] The comparison measurements are recorded, with any regression resolved or explicitly reviewed.
- [ ] All native runtime, compiler, test, documentation, browser, grammar and editor steps of the final hand-off gate pass, `cargo x js-test` included. Any unavailable check remains a blocker to completion.

## Rejected alternatives

- **Move all host vocabulary into `/sys` and generate semantic Core operations.** The small guest outcomes need only `Option` and `Result`; domain declarations and their helpers can remain together in `/std` without introducing a second compiler type vocabulary.
- **A universal codec selected by the guest result type.** Environment absence and a failed read use different wire conventions. The operation contract chooses the encoding.
- **Make `/std` defensive around unchecked replies.** That repeats validation across callers and allows raw calls or discarded results to evade it. Boundary checks belong to executing the call.
- **Treat a typed host return as sufficient validation.** Numeric bounds, absence tokens, request/result relationships and resource transitions require additional enforcement.
- **Preserve raw status records as a parallel public `/sys` API.** This retains two public interpretations of one operation. Core's raw representation is an implementation seam, not a compatibility wrapper.
- **Generate a general guest-type or constraint schema.** The roster needs a closed set of operation conventions and named checks; behavioral clauses and tests cover what signatures cannot express.
- **Put guest sums on the wire by default.** Existing discriminator and payload slots express the selected outcomes. A wire change needs an operation-specific reason, not a desire to mirror the guest heap.
- **Treat invalid tags as defaults, failures as success, or every polling error as inactivity.** These erase distinctions callers rely on and can cause unintended effects or nontermination.
- **Assume an error rolls back effects, or automatically retry a refused reply.** External work may already have occurred. Progress and lifecycle contracts must state what the caller can rely on.
- **Keep exit as a dedicated intrinsic.** Row divergence describes its behavior without a compiler and runtime exception; panic remains separate because it is emitter-only.
- **Carry each builtin's row on every term and check agreement where terms meet.** Equality, interning, linking and cache admission would each need a check someone must remember; an identity leaves nothing to disagree.
- **A separate result-type field on the foreign term.** It admits a type for a returning row or none for a diverging one; an operand stated by the signature is checked by the walk both checkers already run.
- **A returning call with an optional continuation for exit.** Passes that match a returning call would silently accept the terminal case; a distinct variant makes each decide.
- **Aggregate writes that report progress beside a failure.** `write_all` cannot report progress when it fails after a prefix, so the caller resends bytes already written; a live payload on failure would be the one such row in the outcome vocabulary.
- **Flush inside close, or keep buffered output as a documented limitation.** Close must not wait on a peer, so flushing there either blocks or fails silently; the limitation truncates responses written just before close.
- **Return poll-wide failure to the guest.** Every consumer would need a policy for a broken readiness mechanism, and the only sound one is to stop.
- **Report `Option(Handle)` from `proc/stream`, through a presence field or a handle predicate.** The caller already knows which streams it piped; absence in a success position keeps the empty token alive there, and a predicate would let guest code observe tokens.
- **Carry browser signatures in a custom section.** It is a second encoding of the same rows for one consumer; the native bundle already carries them beside the program.
- **Keep raw Wasm-reference browser hooks.** JavaScript cannot read the guest's arrays through them, and an unchecked path beside the checked one weakens its default.
- **Kill by pid with only a table lookup, or by pidfd.** The reaper frees the pid, and the operating system recycles it; pidfd is not portable to macOS.
- **Keep `Io/Error/of` returning `Option(Error)`.** Every migrated caller would unwrap a `none` no `/sys` failure produces.
- **Pass standard streams through to their descriptors in either direction.** A terminal's read-write descriptor makes a write to `stdin` succeed where a pipe refuses it, and the mock and the browser have no descriptor to pass through to; the portable contract must be directional.
- **Report a wrong direction as `not_found`.** The resource exists and native files already answer `EBADF`; `not_found` stays reserved for a resource that cannot perform the operation at all.
- **Split environment names and values into two wire lists.** It changes the wire for a check `/std` can make before anything crosses.

## Survey references

These comparisons informed the contract boundaries; they do not make Curios an implementation of another interface. The WASI links are pinned to 0.2.0 so later interface revisions do not silently change the comparison.

- [Rust `Write`](https://doc.rust-lang.org/std/io/trait.Write.html) distinguishes a progress-reporting write from a whole-buffer loop; [Go I/O](https://pkg.go.dev/io) demonstrates the alternative of reporting progress and an error together and explicitly constrains buffer retention.
- [WASI streams](https://github.com/WebAssembly/wasi-io/blob/v0.2.0/wit/streams.wit) specifies zero-length reads, practical allocation limits and flushing separately from acceptance. Its write-permit protocol is not adopted here.
- [WASI poll](https://github.com/WebAssembly/wasi-io/blob/v0.2.0/wit/poll.wit) routes source errors through readiness; [Mio polling](https://docs.rs/mio/latest/mio/struct.Poll.html#spurious-events) makes readiness advisory.
- [WASI TCP](https://github.com/WebAssembly/wasi-sockets/blob/v0.2.0/wit/tcp.wit) documents operation states and transitions; [Wasmtime's resource table](https://docs.wasmtime.dev/api/wasmtime/component/struct.ResourceTable.html) explicitly enforces resource lifetime relationships. Reuse the principle of stated lifetimes without importing their resource framework.
- [The Canonical ABI](https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md) separates flat values from lifted values and specifies selected-variant payload decoding and allocation cleanup. Its full vocabulary and encoding policies are not requirements for this boundary.

## Permanent documentation and retirement

This file owns the pending acceptance checklist and the contracts not yet enforced. Place permanent facts in their narrowest owners as implementation lands, with links elsewhere rather than copied explanations.

| Owner | Required durable information |
| --- | --- |
| [`curios-abi/README.md`](../../curios-abi/README.md) and operation rustdoc | Builtin identity, the contract vocabulary, codecs, `Byte`, divergence and every row's contract |
| [`curios-prelude-archive/README.md`](../../curios-prelude-archive/README.md) | `Result` ownership, explicit re-exports, ordinary `/sys` adaptation and what `/std` wrappers may do |
| [`curios-core/README.md`](../../curios-core/README.md) and checking documentation | `foreign_signature`, wire-shaped foreign typing and the erased type operand of divergence |
| [`curios-runtime/README.md`](../../curios-runtime/README.md) and rustdoc beside the code | Checked bindings, shapes, refusal classes, the child lifecycle and the `SIGCHLD` assumption |
| [`curios-js/README.md`](../../curios-js/README.md) | `compile` and `run`, checked hooks, buffer ownership and token identity |
| [`curios-emit/README.md`](../../curios-emit/README.md) | Validation placement, `Panic::HostReply`, representation and costs |
| [`documentation/syntax.md`](../syntax.md#foreign-declarations) | Public foreign syntax, including `Byte` |
| A new decision in `documentation/design/toolchain/` | One contract per host operation checked at both ends, with the decisions and rejected alternatives above |
| Existing soundness and language decisions | Boundary assumptions, effect timing, totality, exit, refusal, host blocking and the gate |
| `/std` documentation beside its code | Domain conversions, retry, flush and completion policies, including public converter domains |

Revise [exit](../design/language/an-exit-yields-any-io-and-there-is-no-never.md), [totality](../design/language/totality-of-the-erased-program.md), [the effect perimeter](../soundness/per-term-rules/a-term-outside-io-performs-no-effect.md), [emitter refusal](../design/toolchain/a-refusal-is-a-panic-the-emitter-renders.md), [the host never waits on a peer](../design/toolchain/the-host-never-waits-on-a-peer.md), [the foreign wire contract](../soundness/per-term-rules/foreign-wire-contract.md) and [the gate](../design/toolchain/every-gate-step-catches-what-no-other-step-does.md) in the checkpoint that changes their claims. Remove statements that exit is outside the roster; preserve the rationale for emitter-only panic. Correct comments and checked roadmap summaries in place when their behavior changes.

After all completion criteria have evidence, replace the pending roadmap entry with checked descriptions linking to those owners. Audit removed APIs — `ProcExit`, `EXIT`, `Io/Chunk/of`, raw browser hooks — historical status-record claims and specification references. Delete this file only when no durable decision depends on it, then verify that no filename references or broken replacement links remain.
