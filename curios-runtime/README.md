# curios-runtime

The runtime-only Curios engine: deserialize a precompiled `.cwasm` module and run it on embedded Wasmtime with the `sys.*` host imports, plus the slim launcher the native compiler embeds into bundled executables. Host bindings and the bundle payload format belong to the crate rustdoc.

## Design

### The launcher is slim by exclusion

**Decision.** This crate never depends on Binaryen and by default does not reach Cranelift — it deserializes, and compiling is an opt-in the native product turns on through the `cranelift` feature. It owns the Wasmtime pin: the version lives in this manifest and nothing else in the workspace names wasmtime, so the compiler that precompiles a `.cwasm` and the launcher that deserializes it cannot drift. The launcher is built in isolation (`cargo xtask runtime`), outside workspace feature unification, and the guards in `curios/src/bundle.rs` scan the embedded image itself, validated by building a Cranelift-linked launcher and watching both refuse it.

**Rationale.** Bundled-executable startup should do no compilation work, and slimness is a dependency-graph property: it holds because the capability is absent, not because a code path declines to use it. A workspace build cannot witness it, since feature unification can pull a backend into the graph, and it produces a same-named binary that carries Cranelift, so the evidence is the embedded image rather than a build.

### Compilation runs across threads, and only where compilation exists

**Decision.** The `cranelift` feature brings Wasmtime's `parallel-compilation` with it, so the native product compiles a module's functions across a thread pool, while the launcher, which never enables `cranelift`, links no pool: `cargo tree -p curios-runtime -e normal` names no `rayon`, as it names no `cranelift-codegen`.

**Rationale.** Wasmtime compiles each function on its own and joins the results in order, so the artifact does not depend on the thread count — `curios compile programs/monad_async.crs` writes the same executable byte for byte either way, and a payload's store address needs no new component. On a sixteen-thread machine in a debug build, that program's `precompile` span took 1.61–1.64 s serially and 0.38–0.42 s in parallel; retake it with `curios --profile <PATH> run programs/monad_async.crs` and the fold's `precompile` row.

### The heap is sized ahead of its churn

**Decision.** The shared engine sets `gc_heap_initial_size` to sixteen mebibytes; the collector stays the semi-space copier `Collector::Auto` resolves to under `gc-copying`. The constant is engine-wide because the knob is: Wasmtime bakes the tunable into the `.cwasm` compatibility stamp, so an artifact precompiled under the size only runs under it, and the single pin extends over the knob.

**Rationale.** The engine grows its heap only when an allocation still does not fit after a collection, so under death-birth churn the heap parks within a doubling of the live set and the collector recopies the whole live set continually — most of each churn workload's cost, and superlinear per insert over a growing map — which pre-growing removes. Sixteen mebibytes is the smallest measured size past that knee for both, the cold-page tax registered only far above it, and commit is lazy, so `hello_world`'s resident set does not move. `chain_collection_decomposition` and `spines_collection_decomposition` in `curios/src/tests/codegen/churn.rs` hold the figures and their retake recipes. The birth path, the other third, is the compiler's ([A value costs when it is kept, not when it is named](../documentation/design/lowering/a-value-costs-when-it-is-kept-not-when-it-is-named.md)).

**Rejected.** A maximal pre-grow, which gives the win back to cold pages; an environment variable choosing the size, which would disagree with the measurement at its call site and fork `.cwasm` compatibility per environment. A constant is a floor — a small-live churner wants tens of mebibytes and an all-live tree hundreds, and no engine hook chooses per program.

### The native host never waits on a peer

**Decision.** Every handle whose progress another party decides is non-blocking from creation, and a row that cannot progress answers `would_block` ([Only a fiber waits](../documentation/design/effects/only-a-fiber-waits.md)). Standard input is read only after a zero-timeout poll of its descriptor, whose flags are never changed, since the descriptor is shared with the parent and the terminal; a regular file and the output streams are synchronous, the output streams written through their raw descriptors with no buffered writer, their flags never changed either. `Handle/poll` on a TLS handle asks the kernel for what rustls needs — read while it wants to read, write while it wants to write — and reports the guest's own interest ready when the substituted one is, so a fiber parked on either side wakes when the handshake can move; a TLS stream's records are handed on by `Handle/flush`, which answers `would_block` while the socket takes no more. `Handle/poll` reports a handle that is unknown, has no descriptor or names an invalid one as `err` for its slot, polls each distinct descriptor once and retries an interruption against a deadline taken at the call; a malformed poll, or one the system refuses outright, refuses the invocation.

**Rationale.** A socket is always writable, so a write parked on writability would spin while rustls waited for the peer's hello; translating the interest is what lets the handshake be driven lazily by reads and writes. A stale handle is an ordinary state a scheduler meets — a peer's close, a cancelled task — and its waiter learns the reason from the call it was waiting to make, while a broken readiness mechanism has no such waiter and every consumer's only sound policy would be to stop.

**Rejected.** Signalling readiness for the guest's requested interest alone on a TLS handle, which spins during the handshake; returning a poll-wide failure to the guest.

### A binding is held to its row, and a violation refuses the call

**Decision.** Every import is bound through `Lift` and `Lower`, which each state the wire shape they decode or encode, and `ForeignBindings::define` asserts those shapes against the row it binds — for a builtin, whose bindings `declare_sys_impls` generates from the table, and for an embedder's `ffi` binding alike. A builtin's binding decodes its operands, refusing a malformed one — a closed code outside its table, a `Bool` past `1`, interest bits a guest cannot ask for — checks the row's requirements, calls the host, and holds the reply to the row before encoding it through `Replied`. Any violation refuses the invocation as a `wasmtime::Error` naming what was wrong; nothing panics the embedding process.

**Rationale.** A binding written against plain Rust types is where a host can drift from its row unnoticed, and a shape asserted at registration fails before any program runs. Refusing rather than panicking keeps an embedder's process alive through a guest it hosts, and refusing rather than defaulting keeps a malformed value from becoming a valid one. The reply check is the host's half of [A host operation has one contract, checked at both ends](../documentation/design/effects/a-host-operation-has-one-contract-checked-at-both-ends.md).

**Rejected.** Decoding a closed code to a default, or a `Bool` past `1` as `true`; panicking on a malformed operand; trusting a typed host return as the whole check, when bounds relative to a request and the empty token are not in the type.
