# A failing program names what failed

## Status

Not refined yet. This file records what the backend emits, what the host can see, and the one mechanism question that decides the shape. Nothing is started.

## Why it exists

Every trap the backend emits is a bare `Instr::Unreachable`, so an arithmetic overflow, an out-of-range packed read and an internal invariant violation reach the user as one message with no Curios in it. [Numeric carriers narrow by refusing, never by changing a value](../design/toolchain/numeric-carriers-narrow-by-refusing-never-by-changing-a-value.md) makes the first of those reachable from ordinary arithmetic just past `2³⁰`, so the failure a user is likeliest to meet first is also the least legible one.

## What is certain

Read from source.

- **The trap sites are enumerable, and each one knows its class.** The `Nat` envelope check (`curios-cont/src/into_wasm/code_emitter.rs:131`), the signed `Int` one (`:176`), a shift leaving the envelope (`:267`), a packed read out of bounds (`:613`), and the empty-branch fallbacks. Every one of them is `Instr::Unreachable` with nothing beside it.
- **What the host reports is the wasm failure.** `curios-runtime/src/engine.rs` renders `execution failed:` with the error's root cause and the wasm backtrace, because the trap is the root of the chain and the frames are the context wrapped around it.
- **A trap can already carry a value out.** `proc/exit` traps with `ExitTrap(i32)`, and `instantiate` catches it and recovers the code, which is what distinguishes a clean exit from a real trap. Carrying information out of a wasm call through a trap is proven here rather than hypothetical.
- **No span survives the lowering.** Neither `curios-ersd` nor `curios-cont` names `Span` anywhere, so a failure's *class* is knowable at the trap site and its source location is not.
- **Two failure modes are already distinguishable and are not this item's.** Stack exhaustion, which the engine reports as call-stack exhaustion, and `proc/exit`, which is recovered as an exit code rather than a trap.
- **`curios-abi` is the source of truth for what compiler and runtime agree on**, and its `codes.rs` already carries a named-code table for host status.

## What has to be decided

- **How the code leaves the guest.** A mutable global written immediately before the `unreachable` and read off the store after the trap; or an imported function that traps, as `proc/exit` does. The global costs nothing on the happy path — the write sits inside a branch the emitter already emits — adds no ABI row, and obliges the two runtimes only to read it. The import can carry more than a code, and is the shape to grow into if a message ever needs one.
- **Who owns the code table.** `curios-abi` is the obvious owner, by the same argument that puts the status codes there. What decides it is whether a global read is a *wire* contract at all, since it is not a host operation.
- **What the classes are.** One code per trap site is the most that is knowable; fewer, grouped by what a reader can act on, may be the better report.
- **Whether the JavaScript harness owes parity in the same change.** The ABI invariant says a host operation is complete only when its row, the compiler's use, the native implementation and the JavaScript one agree — and a global read may not be a host operation.
- **What the message says**, and whether it names the operation, the carrier, or the rule that refused.

## Deliberately not specified

Source locations, which are a project to thread spans through two intermediate representations and should be priced as one. Any change to *which* programs trap: this item changes what a failure says and nothing else.
