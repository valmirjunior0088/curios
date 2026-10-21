# A program spreads its work over instances that share nothing

**Not refined yet.** This specification reserves running one program as several instances of its module, each with a heap of its own, exchanging messages and failing alone, so that a program uses more than one core and a fault ends less than the whole program. It is not an implementation plan: it waits on the design decisions under *Open questions*, and on a consumer. It is not [item tasks](../compilation/302-item-tasks.md), which runs the compiler's own work on several threads.

## What is missing

**A program is one instance on one thread.** The native host enters a module on a thread built for it ([`curios-native`'s decision](../../../curios-native/README.md#a-module-runs-on-a-stack-of-its-own)) and advances it turn by turn, waiting between turns on what its suspension names ([A wait is a suspension, and only the host waits](../../design/standard-library/a-wait-is-a-suspension-and-only-the-host-waits.md)); a page runs it on its main thread (`documentation/usage.md`). `/std/Io/Scope`'s children are branches of that one instance, advanced by one driver, so they wait together and compute one at a time: no program computes on two cores at once.

**A fault ends everything.** A trap in any branch ends the instance, and the instance is the program. Nothing a program writes can keep one part running while another has failed.

## What is known

- **What it builds on has landed.** A wait is a suspension the host drives, so an instance is a step function a host can advance beside others. Branches coordinate through `/std/Cell`'s write-once cells and `/std/Channel`'s bounded queues, and both live on the guest's heap, so neither can cross to another instance. A row may be answered later, natively by `ForeignBindings::define_async` and in a page by a hook's promise ([`curios-native`'s decision](../../../curios-native/README.md)).
- **A heap boundary exists already.** A foreign module is instantiated once per row and shares nothing with the program: every value crosses as a copy between the guest's GC arrays and the module's linear memory (`curios-native/src/plugin.rs`).
- **Shared GC objects are not available.** Wasmtime lists the shared-everything-threads proposal as unimplemented ([Wasm proposals](https://docs.wasmtime.dev/stability-wasm-proposals.html)), and the proposal itself is still being merged from its first phase ([Overview](https://github.com/WebAssembly/shared-everything-threads/blob/main/proposals/shared-everything-threads/Overview.md)). In a browser, workers exchange what `postMessage` copies or transfers.
- **Peers that share nothing.** Erlang's processes share no heap, exchange copied messages and are the unit of failure, a monitor or a link telling another of an exit (Armstrong, *Making reliable distributed systems in the presence of software errors*, 2003). Lunatic runs each such process as a WebAssembly instance of its own on Wasmtime, scheduled by a work-stealing executor ([Lunatic](https://docs.rs/crate/lunatic/latest)). The Component Model's asynchronous ABI drives each instance from its host's one event loop ([Concurrency](https://github.com/WebAssembly/component-model/blob/main/design/mvp/Concurrency.md)).
- **A resource belongs to a scope.** Nothing a program opens outlives the scope that opened it ([A wait is a suspension, and only the host waits](../../design/standard-library/a-wait-is-a-suspension-and-only-the-host-waits.md)), so an instance a program starts is opened into a scope as a child or a file is, unless a decision says otherwise.

## The direction discussed

A discussion on 2026-09-23, before waits became suspensions, converged on the following and settled none of it:

- **Shared nothing.** Each instance has its own heap, and the boundary between two is the one a foreign row crosses: a value on the wire, copied by the host, an instance per party. `foreign` declarations and manifest rows stay what they are, for code outside Curios.
- **An instance's entry is a witness of a `/std` concept**, `Service(S)` being one name proposed, with syntax only as a last resort.
- **A message crosses by a derived witness**, `Transport`, as `{payload: Bytes, handles: List(Handle)}`.
- **The host drives each instance** as it drives a program today, stackless, and decides once, from facts it holds exactly, whether an instance runs on a thread, a worker or a process of the operating system.
- **The instance is the unit of failure.**

Ruled out in the same discussion: a placement primitive a program calls, `place`; promotion or forwarding markers carrying a value from one heap to another; migration of a running instance by a budget; and a closure crossing as an index into the code with its captures.

## Open questions

- **The names.** `Task` is `/std/Io/Scope`'s child and `/std/Process` a child process of the operating system, so neither word is free; the concept's name with them.
- **What an entry may hold.** A witness whose methods need premises: entries allowed only premise-free, or the dictionaries a start site resolved recorded and sent with the start.
- **How a message wakes its receiver.** A cell is filled on one heap and the host reads none, so an inbox is a handle the host files, and the host table gains its rows: start, send, receive and an exit's report, each stated against [A host operation is a foreign declaration](../../design/runtime/a-host-operation-is-a-foreign-declaration-whose-types-are-its-whole-contract.md).
- **What a handle that crosses is.** Moved, so that its sender holds it no more, or shared, and what closing it then means on each side.
- **What a parent is told of a child's failure**, and how that meets `Scope`'s cancellation and `defer`.
- **Placement.** Which facts the host decides by, and what a page does: a worker per instance, each loading the module and the harness.
- **What an instance costs**: one started, its prelude's top-level values and its first turn, natively and in a page, counted before the design is chosen.
- **Its consumer.** A program that needs a second core or a part that fails alone, which says which of the above it asks first.
