# A failing program names what failed

## Status

Refined; not started. The mechanism, the layering and the message classes are decided below, and what remains is the implementation in three commits: the emitter with the ABI name, both runtimes with their tests, then this file's retirement.

## Why it exists

Every trap the backend emits is a bare `Instr::Unreachable`, so an arithmetic overflow, an out-of-range packed read and an internal invariant violation reach the user as one message with no Curios in it — `execution failed: wasm trap: wasm \`unreachable\` instruction executed` — followed by a wasm backtrace that Binaryen's inlining has collapsed to `func/main`, so not even the function survives. [Numeric carriers narrow by refusing, never by changing a value](../design/toolchain/numeric-carriers-narrow-by-refusing-never-by-changing-a-value.md) makes the first of those reachable from ordinary arithmetic just past `2³⁰`, so the failure a user is likeliest to meet first is also the least legible one.

## Decided

**A refusal is a call to a host import that traps with a message, as `proc/exit` traps with a code.** The emitter declares one `sys.panic` import taking a byte string and, at every site where it refuses, loads a constant message and calls it before the `unreachable`. A call to an import is a side effect no optimizer may remove, so Binaryen is not a variable; the message lives in the module, where `wonder stage wasm` shows it; and the two runtimes get it by construction rather than through a table kept in step.

**Nothing above the emitter learns the word.** No core intrinsic, no Ersd terminator, no CPS node. The test for a node in an intermediate representation is whether some pass produces or reasons about one, and none does: both constant folders already decline a computation that would leave the carrier so the trap stays an instruction at its materialization point, and `Terminator::Unreachable` and `CpsNode::Unreachable` already carry an arm the theory proved impossible, which the emitter renders as the invariant class. The mechanism lives with the twenty-five `Instr::Unreachable` sites, in `curios-cont/src/into_wasm`.

**There is no user-level `panic`.** A pure `(@A: Type, message: Str) -> A` is an axiom inhabiting every type, and its soundness would rest on the totality analysis instead of on typing — the move [An exit yields any `Io`, and there is no `Never`](../design/language/an-exit-yields-any-io-and-there-is-no-never.md) rejects twice over. An `Io`-typed one is `exit` with a message, and buys, against a library function over `print_err` and `exit`, only that `curios test` says `trapped` rather than `exited`; not enough for a core intrinsic and a `/sys` declaration. A program that wants to stop with a message writes it and exits, as `/std/Cli`'s `fail_with` does. Nothing here forecloses a later decision on its own merits.

**The name is wire; the messages are not.** `PANIC` sits beside `EXIT` in `curios-abi`, by the same argument the crate's README makes for `exit`: nothing comes back, so no `WireSignature` describes it, but the import string is stamped by the emitter and matched by both runtime linkers, so it is spelled once where both read it. The message crosses as an ordinary byte string the runtime prints verbatim, so the text is the emitter's alone.

**Five classes, by what a reader can act on, spelled once.** One `enum Refusal` in the emitter whose `Display` is the sentence: `Nat` left the carrier; `Int` left the carrier; a packed or list read past the end; a `Flt` decoded from a byte string that is not four bytes; a compiler invariant. A message names the rule, the carrier and the remedy, never the operation — by the time a refusal is emitted `x * 2` may be a shift and a folded literal is no operation at all, and the rule is what the design note asks a user to learn: "Nat arithmetic produced a value past 2³¹ − 1, which the carrier refuses; /std/BigNat holds larger values". The invariant class asks for a bug report. One code per site is knowable and not actionable, and would pin the emitter's layout into a public table.

**What each site becomes.** Fifteen classed sites: the `Nat` and `Int` envelope checks in `code_emitter.rs` (checked add, sub, mul, the widened shift, the conversions and negation), the literal materializations in `expr_emitter.rs` (boxed and register-held, each carrier), the packed immediate read and the two rope-helper bounds checks in `rope_emitter.rs`, the window guard, and the four-byte `Flt` decode. Four invariant sites: the empty match and the missing default in `context.rs`, `EmissionTail::Unreachable`, and the tuple cascade's fallthrough. The three fillers after a loop or after the `exit` call stay bare, being unreachable by construction. The five messages are minted once by the module emitter as `$bytes` globals, through the data-segment path a `Bin` constant already takes, and the import is declared unconditionally, since every module refuses somewhere.

**What the runtimes do.** Native: a `PanicTrap(Vec<u8>)` beside `ExitTrap`, linked directly as `exit` is, the bytes read through the same lift every `Bytes` operand takes, rendered as `panicked: <message>` in the error the CLI prints and `curios test` indents under `path: trapped`. No backtrace, since the message is the point and the frames are `func/main`. Browser: a `PanicSignal` beside `ExitSignal`, thrown from the `sys` import after decoding the bytes through the bridge, surfacing in the harness result's `trap` field. Exit code 1, as every trap already is under the tri-state in `usage.md`; a distinct code would be a fourth meaning for tooling to learn and would buy nothing the message does not.

**Which programs trap does not change.** This item changes what a failure says and nothing else; the numeric fixtures that assert a trap sharpen to the carrier's sentence, and the knot fixture that pins the bare `unreachable` text asserts the invariant sentence instead.

## Rejected

An exported mutable global written before the `unreachable` and read off the store after the trap: cheaper on paper, but it obliged a proof that Binaryen keeps a store it may consider dead, a code table both runtimes render from, and a decision about whether a global read is a wire contract at all. Naming the operation in the message, for the reason above. A distinct exit code. Relying on the backtrace, which inlining empties. A `Panic` node in Ersd or the CPS IR, which no pass would create or read.

## Deliberately not specified

Source locations, which are a project to thread spans through two intermediate representations and should be priced as one. A user-level `panic`, for the reason under **Decided**.
