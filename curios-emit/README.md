# curios-emit

The Curios WebAssembly emission: `into_wasm` takes the CPS graph `curios-ersd` built and `curios-cont` optimized, performs delayed closure conversion, and structurizes control into Wasm blocks and loops. How a field's carrier is declared is [A field is declared at the carrier its shape names](../documentation/design/compilation/a-field-is-declared-at-the-carrier-its-shape-names.md), and how a refusal is spelled [A refusal is a panic the emitter renders](../documentation/design/compilation/a-refusal-is-a-panic-the-emitter-renders.md); the backend pipeline belongs to the crate rustdoc.

## Design

### Cells and channels occupy the guest heap

**Decision.** A write-once cell is one GC struct with a nullable payload field: null is empty, a successful fill stores a non-null value, and every later fill preserves it. A channel is a GC struct holding a preallocated nullable-reference array — its length is the capacity, checked at creation to fit the unsigned 32-bit length — a head index, a count and a closed flag. Push appends at `(head + count) % capacity`, take clears the consumed slot before advancing the head, and close preserves queued values for draining.

**Rationale.** The ring bounds retained storage and releases removed references without retaining a message chain; creation allocates in proportion to capacity and every later attempt uses constant storage. Neither object allocates a host handle, and storing one transfers no ownership of its resource. Both complete inline without suspension, and Ersd's lowering builds the ordinary `Option`, `Push` and `Take` layouts from the private operation results.

**Rejected.** A linked queue rooted at freely copied old ends, which retains consumed messages; rewritable cells, a second mutation primitive.

### A closure carries its code as a table index

**Decision.** A closure environment's code field is an `i32`, the body's 1-based slot in the dispatch table for its arity, never a funcref. One table per arity, typed `(ref null $clsr/N)`, is filled by one active element segment in the module's ordered closure walk, so indices are reproducible. An unknown call dispatches with `call_indirect` or `return_call_indirect` against that table, and since the call site expects exactly the table's element type, the engine proves the signature match statically. Slot 0 stays null, so a zeroed code field traps at dispatch with no stub and no check. With the code field an ordinary `i32`, a closure whose captures are all interned constants is a constant aggregate the hoister materializes once at instantiation, one `ConstKey` arm of target plus canonicalized captures; a closure capturing a knot's cell never is one.

**Rationale.** On the engine this pipeline ships, writing a funcref into a GC struct pays a per-store funcref-to-GC-heap intern, a hashing libcall, at every construction: a profile of `programs/rng_state.crs` attributes most of a monadic loop to it. `call_indirect (type $clsr/N)` accepts an entry typed `sub final $clsr/N`, so the final closure subtypes stay, and `wasm_function_references` stays, since the GC proposal layers on it. `closure_index_dispatch_measurements` in `curios`'s codegen probes holds the product figures with their protocol: the swap pays where one description closure is built and forced per bind, and closure-free controls are flat. The decision is emitter-only; a known callee remains the specializers' subject.

**Rejected.** Hoisting a loop-invariant `ref.func` while the field stays a funcref, which recovers a fraction since the intern is per store into the struct; reinstate on an engine whose conversion stops being per store. One module-level table, which leaves the `call_indirect` type check to run per dispatch. A declarative element segment, whose only role is making a `ref.func` eligible, when emitted code holds none.

### Absence is null

**Decision.** A slot a constructor does not write holds the field's zero — `0` for a register slot, null for a reference slot — decided at the Ersd door's construction through `curios_cont::Module::pad`, so every transfer a split copies the slot into carries the same value. Every reference position a value can cross admits null: `func/N` and `clsr/N` parameters and results are `(ref null any)`, a parameter is a nullable local, and a call, return or edge loads its arguments without asserting non-nullness. A `ref.as_non_null` or non-null cast is emitted only where a value is genuinely read — a cell's contents, a closure's target and environment, a host operand, a rope — and the closure environment parameter alone stays non-null.

**Rationale.** A filler materialized as a zero at a guessed carrier is misread at every boundary that guesses differently — an `i31` in a raw `Flt` slot, an `i31` in a concrete reference slot, a null into a non-null parameter. A nullable position costs nothing: the cast at a genuine read is one instruction either way, and a null-admitting signature removes the `ref.as_non_null` every call argument would pay.

**Rejected.** A tolerant load at a rebuild, which makes the one construction that cannot trap on a compiler fault and needs rebuild identities threaded through cloning; declining the worker split for families, which forgoes the split that clears the UTF-8 walk's per-character path.

### A structural tuple is read at its own final type

**Decision.** The `$tuple/N` heap types of the structural rows are final and unrelated to one another, and a field read finds an object's exact type by testing every roster arity that could hold the field, widest first, casting on the last. A family read is a single exact cast, since a family is one final struct keyed by identity.

**Rationale.** Wasmtime's `ref.cast` and `ref.test` to a concrete type compare the object's type index against the target and, on inequality, call the `is_subtype` libcall through a per-store cache (its `func_environ/gc.rs` and `runtime/store/gc.rs`, issue #13484); only a final target short-circuits. A prefix chain `$tuple/4 <: … <: $tuple/1` read every real object through a narrower prefix and so made every read a host call. Testing every arity assumes nothing about which width arrived, and the module-global roster is 2 to 5 arities across the corpus. `a_tuple_is_read_at_its_own_final_type` in `curios`'s codegen tests pins the shape.

**Rejected.** A width hint on the IR to order and prune the cascade: pruning is a correctness claim dressed as a hint, and a width list makes `curios_cont::Intrinsic` lose `Copy`, `Ord` and `Hash`. Keeping the prefix chain under the cascade, since a failed `ref.test` on a non-final type is itself the libcall. Dispatching on arity alone, since the tag stays the discriminant. Waiting for the engine to store supertypes inline, which still walks supertype arrays and arrives on the engine's schedule; JavaScriptCore already inlines the check with Cohen's display.

### Big numbers are a helper library the emitter writes

**Decision.** The boxed half of every `Nat` and `Int` operation is `big_emitter.rs`: `big/` helpers over signed values and `mag/` helpers over bare limb arrays, emitted on demand — a call site names a helper through the table, which marks it used, and the module emitter adds the marked set in `BigHelper::ALL`'s order, callers before callees. Each operation keeps its fast path on two i31s inline and calls one helper otherwise, and every helper is a loop, so an operand's size never reaches the Wasm stack ([Nat and Int are an i31 until they outgrow it](../documentation/design/arithmetic/nat-and-int-are-an-i31-until-they-outgrow-it.md)).

**Rationale.** The fast path is the common case and costs what arithmetic on an i31 costs, at the use; the slow path is long, cold and shared, so a module's size tracks the operations it performs rather than the places it performs them.

**Rejected.** Inlining the slow path, a multiplied division and float conversion at every site.

### Floats past ties to even are a helper library the emitter writes

**Decision.** A `Flt` operation Wasm has an instruction for — ties-to-even arithmetic, `sqrt`, `min`, `max`, the roundings to an integral value it spells — stays that instruction, followed by `r != r` and, only for a NaN, a call to `flt/nan`, which recomputes the model's NaN rule from the operands. What Wasm has no instruction for — the four other directions, `fma` in every direction, ties to away to an integral value, and the exact remainder `rem` — is `flt_emitter.rs`: integer soft-float helpers over the operands' bits, emitted on demand in `FltHelper::ALL`'s order as `big_emitter` emits its own, sharing `wasm!` splicing through `shorthand.rs`. A direction is static on each operation, so the choice is made once, at emission.

**Rationale.** The default direction runs at hardware speed and the directed ones are rare, long and cold. The check after an instruction is what makes the instruction usable: Wasm leaves a computed NaN's sign and payload to the engine, and the model pins them. It costs about 1.4%: `programs/flt_hot_loop.crs` runs five checked instructions a round, every check taken and none firing, and at `N = 300000000` the median of seven alternating runs was 3.572 s against 3.522 s on x86_64-unknown-linux-gnu. To retake it, compile the program to `with_check`, delete `emit_flt_checked`'s store of the result, its self-comparison and the `either` after it, compile again to `without_check`, restore the emitter, and time both alternately on one input.

**Rejected.** Error-free transformations over the hardware operations, which fail near underflow and overflow; a run-time direction operand, a branch the static tag decides at emission.

### A host's reply is read as its row types it, and nothing is checked

**Decision.** A host call loads its operands, calls the import and boxes or embeds what comes back, with no check between: a scalar result is boxed as the number it crossed as, a reference embedded into a rope, a `Bool` or a `Byte` read as the word it is. Outbound, a `Nat` or an `Int` is narrowed to the wire's `i64`, and one the wire cannot carry refuses as `nat_wire` or `int_wire`. A call to a row that does not return is followed by `unreachable`, and no continuation is made for it.

**Rationale.** What stands in an import's results was written there by an adapter from a value of the row's types ([A host operation is a foreign declaration whose types are its whole contract](../documentation/design/runtime/a-host-operation-is-a-foreign-declaration-whose-types-are-its-whole-contract.md)), so a comparison here would test the adapter against itself on every call, in every program. The outbound narrowing is the guest's own half: only the guest holds the number before it is truncated.

**Rejected.** A refusal class for a reply outside its row, with the comparisons that feed it after every call; a return from a row that does not return refused by name, a second ending for a path `unreachable` already ends; leaving the outbound narrowing to the host, which receives an `i64` already wrapped.

### A read of a run answers from a part already flat

**Decision.** A `Bits`, a `Bytes` and a `List` are ropes, whose cost model `types.rs` states: an append is one node, and a whole-value read flattens a node once into a payload the node keeps. A read of one element at a node that was never flattened answers from the child that holds the element where that child is flat-available — a leaf, a window, a flattened node — and flattens nothing. Where the child is itself a node never flattened, the read flattens the node it was asked of, as a whole-value read does: one level, and no further. A window is cut as it was, an unflattened node flattened first, so that every window's base stays flat. The three carriers read alike, bytes and lists by one helper and bits by its twin, their node arm written once.

**Rationale.** A loop that appends and then reads what it appended — a stream's reader searching an arrival, a builder that looks at its last element — would otherwise copy everything it holds at each turn, its time growing with the square of what it reads. The part just appended is a leaf, and the part before it was flattened by an earlier read or is not read at all, so one comparison of the index with the left child's length finds a flat child, and the read costs one array access more than a flattened run's. The test stands in the arm a read takes only where the node has no cache, so a read of a flattened run pays nothing for it.

**Rejected.** A descent to any depth, the fetch of a rope that never flattens: a walk over a deep chain that was never read would pay its depth at every read, where one flattening makes each later read one array access. A balanced sequence in place of the rope, where every read is a descent. A search answered by the run itself, which serves one piece's search and no other read.
