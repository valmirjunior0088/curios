# Host and guest boundary, part 2: host operations and outcomes

Pending implementation specification, following [part 1: guest coordination](host-and-guest-boundary-pt1-spec.md). Give each host operation one precise contract and enforce it through checked host and guest adapters around a small wire vocabulary. Part 1 supplies `/sys/Option`; this part moves `Result` and changes the host boundary. The architecture is agreed; the three operation decisions below must be resolved before their dependent implementation.

## Scope and foundations

The authored operation roster in [`curios-abi/src/host/ops.rs`](../../curios-abi/src/host/ops.rs) already projects wire signatures and `HostOps`. It prevents disagreement about slot order and types, but does not fully specify argument domains, progress, resource transitions or ownership. Native, mock and browser implementations can agree on a signature while behaving differently. Fallible replies also put status and placeholder payloads beside one another, leaving standard-library wrappers to interpret them repeatedly.

Extend that existing ownership boundary. Preserve the wire's scalars and arrays where they express the operation's contract; add `Byte`, explicit divergence and checked adaptation. Keep Core foreign calls wire-shaped and construct ordinary guest outcomes in `/sys`. Move `Result` into `/sys`, reusing part 1's `Option`; retain `Io/Error`, `Io/Chunk`, modes, readiness records, child exit vocabulary and other domain types in `/std`.

This is not a general serialization framework, a guest-type schema, a new Core outcome calculus or an adoption of the WebAssembly Component Model. Ordinary user foreign declarations retain their wire-shaped signatures. Plugins remain limited to their supported scalar and byte-string vocabulary, with `Byte` added. Lightweight processes, cross-instance transport, selection fairness and repository-wide maintenance remain outside this effort. Changes required by an operation's reviewed contract are in scope; unrelated fixes are not.

## One contract, enforced at its owning seams

Each builtin has one canonical identity whose contract fixes its signature and checks. A term cannot attach a weaker contract to that identity. Establish agreement before equality, interning, linking or cache admission can discard a conflicting description. User foreign functions retain their declared wire contracts and cannot impersonate builtin operations.

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

## Host adaptation

The checked host interface presents meaningful arguments and outcomes. Typical returning forms are `Result<Handle, Failure>` for opening, `Result<Option<Bytes>, Failure>` for reading, and `Option<Bytes>` for environment lookup. These are schematic Rust shapes, not requirements for new wrapper types around ordinary buffers. Use existing domain types where they establish a useful invariant; do not generate guest declarations from the Rust host vocabulary.

`Failure` contains failure cases only: neither success nor EOF. The adapter translates those cases through the existing status encoding. Only the adapter manufactures raw padding. A nominal Rust type alone is insufficient: a successful handle must not be the absence token, counts must fit their operation's bounds, and enum or flag constructors must not bypass their declared domains. Enforce these through constructors or encoder checks at the narrowest useful owner.

Decode and check raw arguments before invoking the host method. Validate the argument/result codec shapes against the registered wire signature when bindings are installed; checking name membership and the eventual Wasm function type is insufficient to establish that the closure interprets the slots correctly. Keep raw binding access internal or explicitly outside the checked interface.

A malformed representation or closed code causes boundary refusal of the guest invocation, not an embedding-process panic or a fallback value. A valid request that the host cannot fulfil returns the operation's documented failure. Resource existence and resource-kind checks belong to the host. Closed mode tags and platform-supported numeric settings are different domains: an unknown enum code is malformed, while a validly represented setting the platform cannot support follows the operation's failure contract.

An inconsistent compiler-owned contract is an invariant failure. An actual host or operating-system failure must not be presented as such merely because a current method lacks a failure result. For operations without a returned failure lane, document the invocation-failure policy or review a signature change. Do not silently invent success, empty data or inactivity.

## Wire and invocation behavior

### `Byte`

Add `Byte` to wire types, scalar shapes, slot mappings, foreign parsing, signatures and marshalling. It uses an `i32` lane. Incoming values must be in `0..255` before conversion or boxing: reject negative values and values above 255 without truncation. Cover native codecs, builtin dispatch, mock hosts, plugins and browser foreign calls. Keep `List(Byte)` unsupported and preserve existing scalar and byte-string behavior.

Audit existing scalar and aggregate conversions as part of the same boundary work. In particular, Boolean decoding must establish `0` or `1`, including lists; narrowing cannot silently change values. A count representable on the wire does not automatically fit a platform allocation or array length. Check size conversions and allocation arithmetic, and apply operation-specific allocation policy.

### Diverging exit

Register `proc_exit as proc/exit` with a `Byte` parameter, no wire results and explicit divergence. Divergence is distinct from an ordinary returning zero-result operation. Generate `(@A: Type, code: Byte) -> Io(A)`, carry the result-type argument through Core checking and erase it before execution. Both checkers and the certifier's partiality obligations read divergence from the operation contract.

Native dispatch reports typed termination with a `u8` code through the existing guest-exit mechanism. Browser dispatch implements the same row. Neither terminates the embedding process. Lower the call as terminal control flow with no result reconstruction or resumable continuation; an implementation that unexpectedly returns cannot resume guest code. Delete `Intrinsic::ProcExit`, the `EXIT` import constant and their dedicated compiler/runtime branches. Panic remains the emitter-only import outside the store.

### Physical validity and selected payloads

The guest adapter captures the raw results, checks physical representations, validates the discriminator and checks the active payload before resuming guest code. Preserve primitive representation invariants even for inactive fields; ignoring a failure's payload semantics does not permit malformed references or invalid Boolean values. Conversely, do not reject a legitimate failure because its padding fails a success-only condition such as nonempty handle or positive progress. Encoders supply canonical physically valid padding.

All argument conversion and reply validation executes inside the foreign `Io` action. Merely constructing that action performs no conversion, refusal or host effect. The executed call retains its validation even when its result is unused. Audit optimizer effects, verifier assumptions and emission together so validation cannot be discarded, hoisted or separated from the call it protects.

Boundary refusal does not undo an external effect that already occurred. It does not authorize automatic retry. Resource cleanup on invocation failure follows the runtime's stated lifecycle policy, not an implied transaction around every host call.

## Core, `/sys` and `/std`

Returning Core foreign calls keep signatures derived from `WireSignature`: zero results give unit, one gives its bare carrier, and multiple give a record of raw fields. The canonical builtin contract supplies validation; Core terms do not carry guest wire handlers or a generated semantic outcome language. Divergence adds the explicitly checked and erased result-type argument described above.

Generate `/sys` adaptation as ordinary AST using direct `Io` binding, pure construction, comparisons, branches and registered constructors. It must not depend on `/std` monad witnesses, overloaded operators or declaration-order accidents. Inspect status before projecting successful payloads into guest bindings. A checked raw return need not be converted to a custom compiler IR merely to build `Result` or `Option`.

| Operation class | Core result inside `Io` | `/sys` result inside `Io` |
| --- | --- | --- |
| Fallible, no success payload | Raw status `Nat` | `Result(Nat, {})` |
| Fallible, one success payload `T` | Record containing status and payload | `Result(Nat, T)` |
| Fallible, several success fields | Record containing status and labelled fields | `Result(Nat, {labelled success fields})` |
| `Handle/read` | `{status: Nat, bytes: Bytes}` | `Result(Nat, Option(Bytes))`, with `none` for EOF |
| `proc/env` | `{status: Nat, value: Bytes}` | `Option(Bytes)`, with `none` for absence |
| `proc/stream` | `{status: Nat, handle: Handle}` | `Result(Nat, Option(Handle))`, distinguishing an unpiped stream from failure |
| Ordinary infallible rows | Existing raw shape | Existing shape |

For example, opening yields `Io(Result(Nat, Handle))` and terminal size yields `Io(Result(Nat, {cols: Nat, rows: Nat}))`. A single-attempt write would yield `Io(Result(Nat, Nat))`; this remains conditional on the write decision below. Polling retains `Io(Bytes)` only if its reviewed failure policy permits it. Child waiting retains numeric code/signal payload fields with their active-branch contract; this specification does not move or redesign the guest's child-exit induct.

Declare the ordinary polymorphic family directly at `/sys/Result`, preserving its parameters, universes and constructor order, `success` then `failure`. Re-export it through `pub use /sys/{Result};` and `pub use /sys/Result/{success, failure};` before ordinary imports in `/std/Result`, without globs or another module nesting. Keep functions, witnesses and derivations in `/std`. Resolve compiler identities through the syntax registry, validate actual declaration shapes, instantiate through normal machinery and rebuild both archives. Reuse part 1's approach and regression coverage for `Option` and the existing polymorphic `List` implementation.

`/std` retains `Io/Error`, `Io/Chunk`, modes, interest/readiness records, serial vocabulary and child exit types, together with their conversions, convenience operations and retry policies. The ABI owns code constants; `/std` cites them rather than duplicating numeric literals. Cover these remaining conversions with independent code-value fixtures so keeping them simple does not leave them unchecked.

Migrate every affected wrapper to the new outcomes, preserving EOF, would-block retry, partial progress and cleanup according to the reviewed contracts. Successful read data constructs `Chunk/chunk(bytes)` directly; EOF constructs its EOF case; failure codes are decoded separately. Retire the obsolete status-plus-payload form of `Io/Chunk/of` and update `Io/Error/of` according to its documented public domain. A public converter accepting any `Nat` must remain defined for values a caller can fabricate: the guarantee on a particular `/sys` result does not make `Nat` a refined failure-code type. Do not add a new guest error family solely to bypass that distinction.

## Ownership and resource contracts

Checked host callbacks must not gain a retained mutable alias to guest immutable data. Native adapters already use copied buffers; make ownership equally explicit in the browser. Checked browser hooks exchange independent data buffers, with validation and copying performed before guest adoption of a reply. Raw Wasm-reference hooks, if retained, are a separately documented trusted escape hatch and are not evidence that the checked interface enforces ownership. Preserve enough foreign-signature information at the browser boundary to check and marshal its callbacks; Wasm lanes alone cannot distinguish all wire carriers.

Handle identity is exact token identity. The browser must not collapse empty, padded or large byte encodings through JavaScript number conversion. Preserve the native distinction between the empty absence token and stdin's token. Supported adapters do not mint absence as a successful resource; child-stream absence is translated under its explicitly specified convention. A token's encoding alone does not prove that it names a live resource of the required kind.

Audit each consuming or transitioning operation. State how it treats missing handles, wrong resource kinds, pending work, terminal failure, repeated calls and outstanding related resources. For child waiting, wrong-kind or unknown handles preserve all existing resources; a pending child remains available; completion consumes its result once. DNS resolution distinguishes pending work from a completed success or failure. Socket and TLS operations state their resulting resource state for each outcome. Closing and reaping a child must state what happens to its separately filed streams and background work.

Use the existing resource-table mechanisms where appropriate; do not introduce a general resource framework. The survey found a mock child wait that removes before checking resource kind, fallible setup after a native child has started, and a native reaper that converts a wait error to exit code zero. The reviewed lifecycle contracts must determine the correct behavior and supply focused fixtures for these cases. Preparation failure must have an explicit cleanup path, and inability to observe an exit must not be encoded as a successful zero exit.

## Progress and operation audit

For reads, define zero-length requests explicitly: after validating that the handle is readable, a request for zero bytes succeeds with empty bytes and consumes nothing. For positive requests, data success contains `1..n` bytes; EOF and `WouldBlock` remain distinct. A host may cap an individual read allocation because `n` is a maximum, not a required allocation size. Failure and EOF padding do not undergo the success-length check.

Readiness is advisory. A resumed operation may still answer `WouldBlock`, and callers retry their committing operation accordingly. Error or terminal readiness must permit the operation to report its outcome. Neither an invalid handle nor a permanent polling failure may be silently treated as endless inactivity.

Before migrating an operation, complete its six contract dimensions in the roster's documentation and supporting checks. The audit includes infallible operations, not only status-bearing rows. Establish exact request/output lengths for random bytes; nanosecond ranges for clocks and metadata; permitted input/output poll masks and equal parallel lengths; nonempty successful DNS results; active child-exit fields; optional stream encoding; errno width and offset arithmetic; serial and spawn argument domains; and the distinction between an empty environment value and absence. Do not validate inactive payloads as though they were successful values.

Preserve explicit `Other`/errno cases and the existing status vocabulary. Specify the supported errno range consistently across native, mock, browser and guest checks instead of assuming that native `u32`, wire `u64` and guest `Nat` imply the same bound. Closed-code rejection must not erase an explicitly open extension range.

### Decisions required before dependent implementation

| Decision | Recommended direction | Alternative and consequence |
| --- | --- | --- |
| Write progress, including stdout/stderr | One progress-reporting attempt: accepted bytes produce a successful count; a returned failure accepts no bytes from that attempt. Compose whole-buffer writes above it. Review terminal-output serialization before changing it. | Preserve aggregate writes and represent progress alongside failure. The existing wire has both slots, but its logical result and failure-payload rule must change. Do not hide partial progress behind `Result<Count, Failure>`. |
| Buffered-output completion | Define write success as acceptance, and decide whether an explicit flush operation belongs in this effort. A flush promises draining the relevant host buffers, not peer receipt or durable storage. | Retain the current operation set with an explicit limitation on buffered output at close. This is a behavioral limitation to review, not an implicit delivery guarantee. |
| Poll-wide failure | Distinguish malformed calls, per-handle failures, interruption and failure of the polling mechanism. Use readiness for source failures; handle interruption with a stated deadline policy. | Choose an invocation refusal or a returned failure for persistent poll-wide failure. A returned failure requires reviewing the wire and every consumer. Empty readiness forever is not an acceptable substitute. |

These choices must be recorded here before the affected signatures and implementations land. In particular, settle nonempty writes that make zero progress so library loops cannot spin indefinitely, and distinguish a bytes-accepted guarantee from other internal work such as advancing a TLS handshake. The architecture above does not depend on choosing a larger outcome vocabulary or a new wire in advance.

## Implementation checkpoints

1. **Contract audit.** Complete operation clauses, classify malformed values versus ordinary failures, resolve the three decisions, and record any narrowly justified wire changes. Review the changes against native, mock, browser and standard-library consumers before implementation.
2. **`Byte` transport.** Extend all relevant codecs and compiler paths, add boundary fixtures, and update the foreign syntax and wire-vocabulary documentation.
3. **Diverging exit row.** Implement contract divergence, both checking paths, certification, terminal lowering and runtime dispatch together; remove the dedicated exit machinery and revise its permanent decisions.
4. **`Result` ownership.** Move the declaration, preserve explicit re-exports and validate polymorphic consumers. This checkpoint does not yet change returning row shapes.
5. **Canonical contracts and checked adapters.** Implement identity agreement, codec/signature checks, meaningful host outcomes, wire padding, validation and ownership on both sides. Preserve the existing public guest shapes until the next checkpoint. Test raw malformed traffic independently of checked encoders.
6. **`/sys` outcomes and consumers.** Change generated wrapper shapes and all affected `/std`, example and test consumers together. Remove obsolete status-record documentation and helpers, and exercise progress and lifecycle behavior against the final contract.

Each checkpoint must be buildable and reviewed, with its focused tests plus `cargo x clippy` and `cargo x fmt` before proceeding. Update permanent documentation in the checkpoint that changes its claims. Follow [the contributor validation rules](../../CLAUDE.md#build-and-validation); run the complete hand-off gate once the implementation and documentation are final. A timeout, interrupted command or unavailable prerequisite is an outstanding check. No commit or publication is implied by this specification.

## Verification and completion criteria

- [ ] Every builtin has the six contract dimensions stated, the open decisions are resolved, and all projections and bindings agree with its canonical identity. Reject conflicting builtin descriptions before interning or linking; reject mismatched codec signatures at registration.
- [ ] `Byte` round trips cover 0 and 255 through native, plugin and browser paths. Negative values and 256 or greater refuse without truncation. Existing scalar, list and byte-string fixtures remain green; `List(Byte)` remains unsupported. Exercise malformed Booleans and aggregate elements independently of safe encoders.
- [ ] Exit-code, deadlock-exit and CLI `exited N` fixtures pass. Both checkers reject inappropriate totality claims; certification and proof erasure preserve divergence obligations. An exit has no resumable continuation, and an unexpectedly returning host implementation cannot continue the guest. Source searches find no `ProcExit` or `EXIT` constant in implementation code.
- [ ] Existing `/std/Result` and `/std/Option` paths, constructor matching, witnesses and derived `Spell` retain their behavior. Cover repeated generated type occurrences, higher universes, registered shapes and consumers such as `Cli` through elaboration and certification.
- [ ] Each fallible row has a scripted failure fixture; every successful payload shape has a success fixture. Cover EOF, would-block, empty versus absent environment values, optional child streams and active child-exit branches. Inspect lowering to establish that failed payloads are not projected into successful guest bindings.
- [ ] Malformed-host fixtures exercise row-inappropriate statuses, closed codes, unknown bits, invalid lengths and violated request/result relationships. Failure padding remains physically valid but deliberately fails success-only constraints. Separate fixtures reject physically malformed inactive fields.
- [ ] Executed calls still validate discarded results; unused inert `Io` descriptions neither call the host nor perform boundary conversion. Terminal calls cannot acquire returning continuations. No optimizer removes a required boundary check.
- [ ] Read fixtures cover zero requests, positive progress, EOF, would-block and bounded allocation. Write fixtures cover the chosen partial-progress and zero-progress contract. Buffered output and close follow the reviewed completion policy.
- [ ] Poll fixtures cover unequal arrays, input versus output flags, repeated handles, unknown handles, stale readiness, interruption and persistent mechanism failure under the chosen policy. Native, mock and browser agree on the portable contract while documenting permitted platform differences.
- [ ] Resource fixtures cover wrong-kind preservation, pending versus completed consumption, repeated close/wait, socket and TLS transitions, child setup failure and reaping errors. Host-returned failure never fabricates success or silently drops an unrelated resource.
- [ ] Ownership fixtures retain and mutate callback buffers to establish isolation in checked interfaces. Handle tests distinguish empty, padded, standard-stream and large tokens. Raw escape hatches state their obligations and cannot weaken checked defaults.
- [ ] Fixed protocol fixtures pin numeric codes independently of generated expectations, including errno offsets and the declared range. Tests of `/std` conversions establish coverage without moving domain types into `/sys`.
- [ ] All native runtime, compiler, test, documentation, browser, grammar and editor steps of the final hand-off gate pass. Any unavailable check remains a blocker to completion.

## Rejected alternatives

- **Move all host vocabulary into `/sys` and generate semantic Core operations.** The small guest outcomes need only `Option` and `Result`; domain declarations and their helpers can remain together in `/std` without introducing a second compiler type vocabulary.
- **A universal codec selected by the guest result type.** Environment absence and child-stream absence use different wire conventions. The operation contract chooses the encoding.
- **Make `/std` defensive around unchecked replies.** That repeats validation across callers and allows raw calls or discarded results to evade it. Boundary checks belong to executing the call.
- **Treat a typed host return as sufficient validation.** Numeric bounds, absence tokens, request/result relationships and resource transitions require additional enforcement.
- **Preserve raw status records as a parallel public `/sys` API.** This retains two public interpretations of one operation. Core's raw representation is an implementation seam, not a compatibility wrapper.
- **Generate a general guest-type or constraint schema.** The roster needs a closed set of operation conventions and named checks; behavioral clauses and tests cover what signatures cannot express.
- **Put guest sums on the wire by default.** Existing discriminator and payload slots express the selected outcomes. A wire change needs an operation-specific reason, not a desire to mirror the guest heap.
- **Treat invalid tags as defaults, failures as success, or every polling error as inactivity.** These erase distinctions callers rely on and can cause unintended effects or nontermination.
- **Assume an error rolls back effects, or automatically retry a refused reply.** External work may already have occurred. Progress and lifecycle contracts must state what the caller can rely on.
- **Keep exit as a dedicated intrinsic.** Explicit row divergence describes its behavior without a compiler/runtime exception; panic remains separate because it is emitter-only.

## Survey references

These comparisons informed the contract boundaries; they do not make Curios an implementation of another interface. The WASI links are pinned to 0.2.0 so later interface revisions do not silently change the comparison.

- [Rust `Write`](https://doc.rust-lang.org/std/io/trait.Write.html) distinguishes a progress-reporting write from a whole-buffer loop; [Go I/O](https://pkg.go.dev/io) demonstrates the alternative of reporting progress and an error together and explicitly constrains buffer retention.
- [WASI streams](https://github.com/WebAssembly/wasi-io/blob/v0.2.0/wit/streams.wit) specifies zero-length reads, practical allocation limits and flushing separately from acceptance. Its write-permit protocol is not adopted here.
- [WASI poll](https://github.com/WebAssembly/wasi-io/blob/v0.2.0/wit/poll.wit) routes source errors through readiness; [Mio polling](https://docs.rs/mio/latest/mio/struct.Poll.html#spurious-events) makes readiness advisory. Neither by itself settles Curios's poll-wide failure policy.
- [WASI TCP](https://github.com/WebAssembly/wasi-sockets/blob/v0.2.0/wit/tcp.wit) documents operation states and transitions; [Wasmtime's resource table](https://docs.wasmtime.dev/api/wasmtime/component/struct.ResourceTable.html) explicitly enforces resource lifetime relationships. Reuse the principle of stated lifetimes without importing their resource framework.
- [The Canonical ABI](https://github.com/WebAssembly/component-model/blob/main/design/mvp/CanonicalABI.md) separates flat values from lifted values and specifies selected-variant payload decoding and allocation cleanup. Its full vocabulary and encoding policies are not requirements for this boundary.

## Permanent documentation and retirement

This file owns the pending acceptance checklist and unresolved decisions. Place permanent facts in their narrowest owners as implementation lands, with links elsewhere rather than copied explanations.

| Owner | Required durable information |
| --- | --- |
| [`curios-abi/README.md`](../../curios-abi/README.md) and operation rustdoc | Canonical identity, vocabulary, codecs, divergence and operation contracts |
| [`curios-prelude-archive/README.md`](../../curios-prelude-archive/README.md) | `Result` ownership, explicit re-exports and ordinary `/sys` adaptation |
| [`curios-core/README.md`](../../curios-core/README.md) and checking documentation | Wire-shaped foreign typing and the erased type argument for divergence |
| Runtime and browser documentation beside their code | Checked bindings, buffer ownership, raw escape hatches and resource lifecycle |
| [`curios-emit/README.md`](../../curios-emit/README.md) | Validation placement, representation and allocation costs |
| [`documentation/syntax.md`](../syntax.md#foreign-declarations) | Public foreign syntax, including `Byte` |
| Existing soundness and language decisions | Boundary assumptions, effect timing, totality and exit behavior |
| `/std` documentation beside its code | Domain conversions, retry and completion policies, including public converter domains |

Revise [exit](../design/language/an-exit-yields-any-io-and-there-is-no-never.md), [totality](../design/language/totality-of-the-erased-program.md), [the effect perimeter](../soundness/per-term-rules/a-term-outside-io-performs-no-effect.md) and [emitter refusal](../design/toolchain/a-refusal-is-a-panic-the-emitter-renders.md) in the checkpoint that changes their claims. Remove statements that exit is outside the roster; preserve the rationale for emitter-only panic. Correct comments and checked roadmap summaries in place when their behavior changes. Preserve the decisions and rejected alternatives above in their permanent owners before retirement.

After all completion criteria have evidence, replace the pending roadmap entry with checked descriptions linking to those owners. Audit removed APIs, historical status-record claims and specification references, including the link from part 1. Delete this file only when no durable decision depends on it, then verify that no filename references or broken replacement links remain. If part 1 retires first, replace this file's dependency link with its permanent decision links.
