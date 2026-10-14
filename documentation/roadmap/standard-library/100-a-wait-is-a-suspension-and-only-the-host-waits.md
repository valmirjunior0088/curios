# A wait is a suspension, and only the host waits

[Only a fiber waits](../../design/standard-library/only-a-fiber-waits.md) states that `Monad/Lift(Io, Async)` "is correct only if no `Io` row can block on what a fiber waits for", and the code breaks it: an `Io` action can wait, a fiber sequences one through the lift, and every other fiber stops until it is done.

## What is missing

**A lifted `Io` action is one atomic step of its fiber.** `Async(A)` is `Io(Step(A))` (`curios-std/src/Async.crs`): a fiber runs its `Io` until it reaches a `Step/step(pause, next)`, and `Async/lift` runs a whole `Io` action inside one step, so a lifted action that waits holds every fiber of the one host thread. This is the shape of a resumption monad, whose first description says that "any monadic action that is lifted into the new monad will be considered an atomic action in a concurrent setting" (Claessen, [A Poor Man's Concurrency Monad](https://www.cse.chalmers.se/edu/course.2018/pfp/Papers/PMCJFP.pdf), JFP 1999). The atomicity holds only because an `Io` description cannot stop part-way.

**The host enters a program once, so it lends the program a row to sleep in.** The runtime calls `func/main` and gets control back only at the program's end (`curios-runtime/src/engine.rs`), so a program with nothing to run sleeps inside that call, in `handle_poll`, "the one row on which the host sleeps". The scheduler's driver is therefore an `Io` action that waits, and so is anything built on the row.

**Six paths reach a wait from a fiber, and the type checker accepts each:**

- **The read of standard input at `Io`.** `Read(Io, Io/Input)`, `Io/read` and a `Reader` read at `M = Io` reach `read_waiting` (`curios-std/src/Handle.crs`), which polls with no timeout.
- **`Read` and `Write` at `Io` for a raw `Handle`.** `Handle/stdin` is public, and the same waiting helpers serve it.
- **`Handle/poll` with a timeout**, a public `Io` action and the sleeping row itself.
- **`Async/run` inside a fiber**, an `Io` action that runs a whole scheduler of its own: it holds every fiber around it while it waits, and reports a deadlock and ends the process when what it waits for is a future only a fiber around it fulfils.
- **A FIFO or a device opened as a `File`.** The host opens every file blocking (`curios-runtime/src/os_host.rs`, `file_open`), so the open or the read waits in the host itself.
- **`print` to a pipe its reader has stopped draining.** The host writes the raw standard output descriptor, whose flags it never touches, so the write waits for the reader.

**Two of them were shown**, timed at `f795b018c` on a machine in use: a fiber printing every 200 ms beside one that reads, run as `(sleep 5; echo hi) | curios run <file>` with each output line stamped on arrival. Reading standard input through `Io/read`, the ticker due at 0.93 s printed nothing until the input arrived at 5.00 s; through `Async/Read`'s `read` it ticked every 200 ms throughout. A FIFO read through `File/read_all` silenced the ticker from 0.86 s to 5.00 s.

**The browser host cannot sleep, so its row does not.** `curios-js/src/harness.js` answers `handle_poll` at once and ignores the timeout, so a fiber's `Async/sleep` in the playground runs the scheduler round until its deadline passes, holding the page.

**An embedder's answer to a `foreign` row can wait and has no other way to do slow work.** A module answering a row can only compute, since a module that imports anything is refused (`curios-runtime/src/plugin.rs`), and the host hands its own slow work back as a handle a fiber parks on (`dns_lookup`, `serial_drain`, a child's handle). A closure bound by `ForeignBindings::define` can block and can mint no such handle; a browser hook cannot block, and a promise is no value of a row's type.

## What is known

**Who owns the wait.** WASI 0.2 gave every component its own event loop over a blocking `poll`, and "there was no way for those event loops to coordinate with one another"; in 0.3 "the host is now the one in charge of managing the one event loop that is shared by all components" ([WASI 0.3](https://bytecodealliance.org/articles/WASI-0.3)). The Component Model's callback ABI has an export return EXIT, YIELD or WAIT, and "the runtime will repeatedly call the callback until a value of `0` is returned" ([Concurrency.md](https://github.com/WebAssembly/component-model/blob/main/design/mvp/Concurrency.md)). Go's `js` port yields its scheduler to the JavaScript event loop when every goroutine is idle, and its `wasip3` port has each export "return control to the host until one or more events are ready" ([golang/go#76775](https://github.com/golang/go/pull/76775)). SDL3's main callbacks replace an application's own loop, which Emscripten "absolutely requires" ([README-main-functions](https://wiki.libsdl.org/SDL3/README-main-functions/raw)); Erlang's `enif_select` hands a descriptor to the VM's poll set and returns ([checkio](https://www.erlang.org/doc/apps/erts/checkio)); Pony has "zero blocking operations" ([ASIO](https://tutorial.ponylang.io/runtime-basics/asio)). The principle is [sans-I/O](https://sans-io.readthedocs.io/how-to-sans-io.html)'s: a step function that neither performs I/O nor waits, driven by whoever does.

**Suspension compiled without stacks.** A Kotlin suspend function "returns a special marker value COROUTINE_SUSPENDED to signal its suspended state", and its caller checks, so "such implementation does not require specific runtime support" ([Kotlin specification](https://kotlinlang.org/spec/asynchronous-programming-with-coroutines.html)). Lua's `coroutine.resume` runs a coroutine until it yields or finishes, and the values passed to `resume` become the results of the pending `yield` ([Programming in Lua](https://www.lua.org/pil/9.1.html)). A freer monad is a value or an effect request with its continuation, which an interpreter resumes ([Kiselyov and Ishii](https://www.springerprofessional.de/doi/10.1145/2887747.2804319)). Koka compiles its effects to C the same way: a bind tests what its action answered and, on a yield, extends the resumption with its continuation; the resumption is kept as a list of continuations, "using a queue or array representation", so resuming does not rebuild it; every bind is inlined, with "a fully inlined fast path" and a shared slow path, because a bind that allocates its continuation up front pays for it "even for non-yielding code"; and the test after each call "seems quite cheap … and the condition can be predicted well", left out where a function is typed never to yield ([Xie and Leijen, Generalized Evidence Passing](https://www.microsoft.com/en-us/research/uploads/prod/2021/03/multip-tr-v2.pdf), §2.6–2.8). The runtimes that park a thread at a blocking call instead — GHC ([Mio](https://unpaywall.org/10.1145%2F2503778.2503790)), Go ([runtime/HACKING](https://go.dev/src/runtime/HACKING)), Java's virtual threads ([JEP 444](https://openjdk.org/jeps/444)) — own stacks or threads to do it.

**Structured concurrency, and cancellation at a suspension point.** Eio's `Fiber.both` cancels the other branch when one raises, `Fiber.first` cancels the loser, and "any operation that can switch fibers can also raise a Cancelled exception" ([Eio](https://github.com/ocaml-multicore/eio)); Trio's nurseries run inside cancel scopes whose cancellation is level-triggered at checkpoints ([Trio](https://trio.readthedocs.io/en/stable/reference-core.html)); Java's task scope cancels unfinished subtasks and waits for them to finish when it closes ([Structured Concurrency](https://docs.oracle.com/en/java/javase/24/core/structured-concurrency.html)). Each delivers cancellation where the cancelled code is suspended, and runs its cleanup there.

**An embedder's slow work.** Node-API runs an addon's work on libuv's thread pool and completes it on the loop's thread, resolving a promise ([nodejs/node#13512](https://github.com/nodejs/node/issues/13512)); Deno's operations are synchronous or asynchronous, an asynchronous one a Rust future its event loop polls ([runtime internals](https://www.mintlify.com/denoland/deno/contributing/runtime-internals)); GLib's main loop is an executor for Rust futures, and a library tied to Tokio is spawned there and awaited ([`spawn_future_local`](https://gtk-rs.org/gtk-rs-core/stable/0.21/docs/glib/fn.spawn_future_local.html)); napi-rs runs `async fn` on a Tokio runtime of its own ([async runtime](https://napi.rs/docs/concepts/async-runtime.md)); Wasmtime's asynchronous host functions suspend the whole guest while the embedder's executor drives the call ([`func_wrap_async`](https://docs.wasmtime.dev/api/wasmtime/component/struct.LinkerInstance.html)).

A single-threaded event loop writes its standard output synchronously where the descriptor is a file or a terminal and warns that "pipes that are not being read at the receiving end" can block the loop ([Node.js, a note on process I/O](https://nodejs.org/api/process.html#a-note-on-process-io)).

## Decisions

Each settled on 2026-10-14.

- **Nothing blocks: a wait is a suspension.** A forced `Io` description answers its value, or a suspension: the handles it waits on, the description that abandons it, and the rest of it. Whoever advances a description decides what a suspension means — the host waits on its handles, a combinator runs something else — so a lifted action is atomic only between its suspensions, and the lift stays.
- **Only the host waits.** No host row sleeps. The host runs a program by advancing it, waiting between suspensions on the handles each names, and nothing in a program waits on its thread.
- **Core states two operations, `Io/suspend` and `Io/advance`, over types it already has.** `Io` keeps no eliminator: `advance` answers an `Io`.
- **A handle is the one thing a suspension waits on.** A deadline is a timer's handle, a stream's readiness a watch's, an answer a pending handle; a guest condition is the empty list, "until a sibling runs".
- **Cancellation is structural.** A combinator that no longer needs a suspended branch runs the branch's abandon and drops its rest; a bracket's finalizer rides each suspension that leaves it and learns whether its body finished or was cancelled. No code is resumed with a cancellation, so a wait cannot fail by one.
- **Concurrency is structured combinators in `Io`**, under `/std/Async`: `both`, `all`, `first`, `race`, `timeout` and `scope`, a scope ending when its body and every child it spawned have ended. The `Async` type retires.
- **A `foreign` row stays one row, and may be answered later.** A row whose answer may come later carries a pending lane: its first call answers the reply or a pending handle, and the call suspends on the handle and asks again. The rows that carry it are the host's rows its table marks and a package's rows no module answers. An embedder answers with `define` at once or with `define_async` through a future the run loop drives; a browser hook answers a value or a promise.
- **A write to standard output or standard error waits by suspending**, gated as a read of standard input is, so a slow reader holds only the branch that writes.
- **A FIFO or a device opened by path is opened without blocking**, and its reads and writes suspend as any stream's do.
- **A measured program reads its size from the command line**, as the bench's Rust contestants do, so no measured binary links a suspension it does not take.
- **A program closes as before**: `Program/perform` answers an `Io({})`, the compiler writes it around each tail, and `()` closes a program that has done what it does by its last `!`.

## The design

- **The two operations.** `Io/suspend(on: List(Handle), abandon: Io({}), rest: Io(A)) -> Io(A)` answers a suspension; `Io/advance(action: Io(A), done: (A) -> Io(R), suspended: (List(Handle), Io({}), Io(A)) -> Io(R)) -> Io(R)` forces `action` and hands its value to `done` or its suspension to `suspended`. `advance(pure(a), d, s)` is `d(a)`, `advance(suspend(on, ab, r), d, s)` is `s(on, ab, r)`, and `advance(bind(m, k), d, s)` advances `m`, binding a suspension's rest to `k`. `/std/Io/wait(on)` is `suspend(on, pure(()), pure(()))`.
- **Compilation.** A forced description answers a value or a `$suspension` no value can be, told apart by one type test. `IoBind` tests its first force, and a suspension goes back out with the bind's continuation appended to its frames through one out-of-line step every bind shares, so the path a bind takes when nothing suspends stays small enough to inline; its tail force passes a suspension through. Resuming runs what the suspension holds first, then its frames in order, and wraps the frames still pending around whatever suspends again, so a resume costs what it runs. A test folds away wherever no flow can bring its operand a suspension — a number, a host's reply, an action that never suspends — and where no reachable function constructs a suspension every test does, so the program compiles as it does today.
- **The host.** `func/main` forces the program and `func/advance` a description it was handed — a suspension's rest, or its abandon — each answering a kind, a value, the handles, the abandon and the rest. The host waits until one handle is ready — a timer's moment, a watch's readiness, a pending answer, the ready token, or a handle closed or unknown — and advances the rest. A suspension on no handle at the top is a deadlock: the host advances its abandon, reports, and ends the program with status `1`. Natively the wait is `poll(2)` and a timer list; in the browser it is the event loop.
- **The rows.** `timer(at)` and `watch(h, interest)` mint handles; `handle_ready` says which of a list are ready, at once; one constant token is always ready, a yield. A stream answers `would_block` as it does today, and its reader suspends on a watch and retries.
- **Brackets.** `using(fin, body)` advances its body; each suspension it passes out carries `fin(cancelled)` after the body's own abandon, and a body that finishes calls `fin(finished)`.
- **Combinators.** A combinator advances its branches in turn; when none can move, it suspends outward on the union of their handles carrying the sequence of their abandons, and when every branch waits on a guest condition and none moved, on no handle. Where an outcome makes a suspended branch unneeded — a winner, a timer, a failure under `Try` — it runs that branch's abandon and drops its rest before it answers.
- **Rows answered later.** A pending lane adds a token operand and a token result to the row's import. The first call answers the reply or a pending handle; the call suspends on the handle with its closing as the abandon, and when advanced calls again with the token. Closing a pending handle drops the future or fires the promise's `AbortSignal`. Name lookup and a serial port's drain are such rows, and `dns_resolve` goes.
- **Embedders.** `ForeignBindings::define_async` binds a closure answering a future, which the run loop polls once where the row is called and again whenever its waker fires; `curios_runtime::blocking(f)` runs blocking code on the runtime's bounded pool. A module answers at once.

## What waits

- **Completion for streams**: a stream's reads and writes answered later by the host, should readiness cost too much.
- **A test through an indirect call**: the analysis that folds a test follows known calls and their returns, so an action forced through a closure it cannot resolve keeps its test.
- **A turn that yields while work remains**, the callback ABI's YIELD, so a long computation hands the browser back its page.
- **A WASI 0.3 target**: a suspension maps onto the callback ABI's WAIT, and a row answered later onto an `async` import.
- **Reusable watches**, one handle a stream's interest instead of one a wait.
- **WebAssembly stack switching** is not planned: the rest of a suspension is a value.

## Stages

1. **A description suspends, and the host advances it.** `IoSuspend` and `IoAdvance`, their erasure, the `$suspension` test and its folding, the two exports, the native, browser and mock loops, `timer`, `watch`, `handle_ready` and the ready token. `hello_world` and a corpus program compile to the function count and size they have before it.
2. **Waiting in `Io`.** `wait`, `sleep`, `yield`, `using`, reads and writes over `watch`, and the gated standard output write.
3. **Fibers over suspensions.** `/std/Async`'s combinators, futures, channels and selection in `Io`, `Read(S)`, `Write(S)` and `Reader` over `Io`, every user of the `Async` type and every test moved to `Io`, and the type retired. The concepts move with the fibers rather than before them: a `Reader` over `Io` alone would read inside a fiber as a lifted action, which holds every fiber while it waits.
4. **The host stops lending a sleep.** `handle_poll` leaves the table.
5. **A `foreign` row may be answered later.** The pending lane, `define_async`, `blocking`, promises in the browser, name lookup and a serial drain.
6. **A pipe or a device opens without blocking.** `file_open` reads the file's kind; macOS through CI.

## Verification

- **A branch keeps its turn while another waits**: on standard input, on a FIFO, and on an embedder's row answered later, natively and in the browser.
- **A browser sleep leaves the page free**: the harness's event loop runs timers while a program sleeps.
- **Cancellation runs cleanup once**: `timeout` drops a branch after running its `using` finalizer with `cancelled`, and a cancelled pending call drops its future.
- **A program that never suspends carries no test**: `hello_world` compiles as it does before the campaign.

## Rejected

- **Keeping waits out of `Io` and guarding the driver**: an opaque `Entry` answered by `perform`, a turn row carrying a continuation, an entry judged at `Io(Turn)`, a second carrier `Run` — each moves the bridge between the library's continuations and the host's wait, and none removes it.
- **A blocking monad beside `Async`**, in which a scoped scheduler runs and returns its deadlock: a thread that waits, which the host's ownership of every wait rules out.
- **Walking back the lift, or making it explicit**: a lifted wait stalls as the inserted one does.
- **A host that refuses a nested scheduler at run time**, as Tokio and Python do: a run-time report of what a suspension makes harmless.
- **Setting a handle non-blocking inside a lift**: the lift cannot see which handles an action touches, and a standard stream's flags are shared with the processes beside it.
- **Stack switching**: a proposal behind a flag in Wasmtime and unshipped in browsers, for a suspension a description already affords.
- **Completion for every stream now**: every row redesigned, and an abandoned read in flight to cancel or keep, where readiness has nothing in flight while suspended.
- **A mark on a `foreign` declaration for a row answered later**, or a row split into a start and an answer: a suspension already lets one call wait, and splitting it is what suspension was for.
- **Cancellation delivered as a value at a suspension**: the suspended code would have to answer what it does not have, so every wait would gain a failure and a row its own `cancelled`.
- **An explicit `cancel`**: a combinator already decides when a branch is no longer needed.
- **A completer an embedder fulfils**, a Tokio runtime inside `curios-runtime`, or a run the embedder's executor drives: an advisory cancellation, a dependency the slim launcher shares, and a loop the host has just been given.

## Completion and retirement

`documentation/design/standard-library/a-wait-is-a-suspension-and-only-the-host-waits.md` replaces [Only a fiber waits](../../design/standard-library/only-a-fiber-waits.md). [Effects are descriptions](../../design/theory/effects-are-descriptions-and-the-carrier-has-no-eliminator.md) states `suspend` and `advance`; [A host operation is a foreign declaration whose types are its whole contract](../../design/runtime/a-host-operation-is-a-foreign-declaration-whose-types-are-its-whole-contract.md) states the pending lane and each answerer's contract; [the lifting decision](../../design/standard-library/a-fallible-operation-returns-try-and-bang-lifts-along-declared-edges.md) loses the `Async` edge. `curios-runtime`'s, `curios-abi`'s and `curios-std`'s READMEs and the harness say what each owns. The line in `documentation/roadmap.md` becomes a checked summary linking the decision, and this file is deleted.
