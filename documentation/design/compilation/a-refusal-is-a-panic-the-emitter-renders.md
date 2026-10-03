# A refusal is a panic the emitter renders

**Decision.** Every refusal a compiled program can stop with is a call to one `sys.panic` host import with a byte string, followed by the `unreachable` that keeps the block's type. The classes are `curios-cont`'s `Panic` roster — a `Nat` or `Int` argument past what the host wire carries, a read past the end of a packed value or list, a `Flt` decoded from the wrong number of bytes, a recursive value read while its own initializer runs, a host reply outside its call's contract, and a compiler invariant — and `curios-emit`'s `refusal.rs` is the one place each class's sentence is spelled, naming the rule, the carrier and the remedy, never the operation. Each class a module can reach gets one helper, `$refuse/<class>`, which builds its sentence from a passive data segment when the refusal fires, so nothing is allocated for a message until it is printed. A lowering that refuses a state the program can reach seats a `Node::Panic` of its class; `Node::Unreachable` keeps one meaning, an arm the theory proved impossible, and renders as `Panic::Invariant`. Both runtimes report `panicked: <sentence>` and the wasm frames the build kept, and exit 1. The import's name is wire, `curios-abi`'s `PANIC`; the sentences are ordinary byte strings. No `/sys` declaration names it, so no program can spell a panic.

**Rationale.**

- **A bare `unreachable` for every trap makes an overflow, a bad index and a compiler bug one engine message with no Curios in it**, under a backtrace inlining collapses to `func/main`. A call to an import is a side effect no optimizer removes, so the message survives Binaryen, shows in `wonder stage wasm`, and reaches both runtimes by construction.
- **Classes are grouped by what a reader can act on.** A code per site is knowable and not actionable and would pin the emitter's layout into a public table; the operation is never named, since by emission `x * 2` may be a shift and a folded literal no operation at all. The IR carries the class, so a sentence is spelled once.
- **There is no user-level `panic`.** A pure `(@A: Type, message: Str) -> A` is an axiom inhabiting every type, and an `Io` one is an exit with a message, which a program writes as a write and an exit, as `/std/Cli`'s `fail_with` does.

**Rejected.**

- **An exported mutable global read off the store after the trap**, which needs a proof that Binaryen keeps a store it may consider dead and a code table both runtimes render from.
- **A core panic intrinsic or host row**; **one `Unreachable` node for both a proved-impossible arm and the knot's forcing state**, when the second is the program's doing and says so; **a message on the IR node**, prose in three representations.
- **Sentences minted as module constants**: `array.new_data` may trap, so Binaryen cannot remove the start function's allocation of a message nothing reads, and every program would pay one allocation per class at start-up.
- **A distinct exit code**, a meaning the message does not need; **relying on the backtrace**, which inlining empties.
