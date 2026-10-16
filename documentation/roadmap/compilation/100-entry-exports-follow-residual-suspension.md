# Entry exports follow residual suspension

The implementation falls short of [An entry exports only the turns it can return](../../design/compilation/an-entry-exports-only-the-turns-it-can-return.md). Cont registers a suspension row during lowering, and emission selects the turn ABI from that registration even when optimization removes every escaping suspension.

## Goal and decisions

Keep suspension representation separate from the entry's residual return capability. Extend the existing row-flow analysis with function-return positions, then derive the capability at the verified Cont-to-machine boundary. Unknown callback, reference-read and mutable-operation results stay conservative. No host-synchrony assumptions, optimizer-budget changes or new effect system are involved.

## Remaining stages

1. Carry residual capability through machine lowering and structurization, selecting entry exports and turn-only dispatch from it. Check emitter structure and raw/optimized native execution with registered-but-unused rows, internally consumed suspensions, escaping suspensions and unknown results.
2. Check browser direct execution, promised foreign replies, repeated resumption and cancellation that suspends. Run the hand-off gate and retire this specification.

## Verification and retirement

An immediate entry omits the advance export even when its suspension row remains registered. An internally returned suspension consumed before entry returns does not require turns. An escaping or uncertain suspension retains them. Nominal-row metadata remains available in either case. Native and browser execution preserve asynchronous reply, deadlock, cancellation and GC-between-turns behavior. Existing copy-growth and raw-size guards remain unchanged.

Each stage carries its focused tests, workspace formatting and lint; backend changes also run the standard-library corpus. Historical size and timing measurements are not retaken. As stages land, this file describes only the remaining work. Once the host checks and hand-off gate pass, update references to the implemented contract and delete this file.
