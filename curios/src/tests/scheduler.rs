//! The `/std/Async` scheduler through its public surface: parking on a handle, a timer or a waker and resuming, tasks and fibers, brackets released on both exits, and the deadlock report.

use {
    super::{run, run_text},
    curios_runtime::MockHost,
};

/// A host whose monotonic clock climbs one second per reading, so a sleeper's deadline is passed by the scripted ramp rather than by waiting.
fn ticking() -> (MockHost, curios_runtime::MockIo) {
    MockHost::builder().mono((0..40u32).map(|s| (s, 0))).build()
}

// A response scripted in two chunks reaches the reader only through the park-poll-resume path: the first read spends chunk one, the second answers would-block and parks the fiber on the socket, `poll` arms chunk two and wakes it, and the drain runs on to the end. A host that is always ready never takes that path, which is why the chunked script exists.
#[test]
fn a_read_parks_and_resumes_across_a_chunk_boundary() {
    let (system, io) = MockHost::builder()
        .net_chunks([("example.com:80", vec!["ab", "cd"])])
        .build();
    run_text(
        r#"
        use /std/{Str, Show, Try, Async, Io};
        use /std/tcp/{Socket};
        let fiber: Async({}) =
            let r = Try/run(Socket/call(Socket/connect("example.com", 80), Str/to_bytes("GET /\r\n\r\n")))!;
            match r
            | success(response) => Io/write(Io/stdout, response)
            | failure(e) => /std/print(Show/show(e))
            end;
        Async/run(fiber)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"abcd");
}

#[test]
fn task_bind_reads_and_echoes() {
    // The monad surface: a `!` region over `Async`, sequencing the standard input's stream read (which completes without parking under the mock) into a write, driven to its value by `run`.
    let (system, io) = MockHost::builder().stdin_lines(["hello"]).build();
    run_text(
        r#"
        use /std/{Async, Io, stream};
        let prog : Async({}) =
            let r = stream/Read/read(Io/stdin, 1024)!;
            match r : (_) => Async({})
            | chunk(bytes) => Io/write(Io/stdout, bytes)
            | eof() => Async/pure(())
            | error(_) => Async/pure(())
            end;
        Async/run(prog)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"hello\n");
}

#[test]
fn block_on_returns_a_typed_value_and_awaits_a_spawned_child() {
    // `block_on` returns a typed value AND a spawned child runs because the root explicitly `join`s it: the child takes its first turn ahead of the root and yields, the root writes "root;" and joins — parking on the child's future — so the child resumes, writes "child;", and fulfils it with 5; the root resumes and `block_on` hands back 5 + 2 = 7.
    assert_eq!(
        run(r#"
        use /std/{Async, Str, Nat, Io};
        let child : Async(Nat) =
            let _ = Async/yield_now!;
            let _ = Io/write(Io/stdout, Str/to_bytes("child;"))!;
            Async/pure(5);
        let root : Async(Nat) =
            let f = Async/spawn(child)!;
            let _ = Io/write(Io/stdout, Str/to_bytes("root;"))!;
            let c = Async/join(f)!;
            Async/pure(Nat/add(c, 2));
        let _ = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(/std/Result/unwrap_or(Async/block_on(root)!, 0))))!;
        /std/Io/pure(())
        "#),
        b"root;child;7"
    );
}

#[test]
fn join_all_runs_children_concurrently_and_collects_in_order() {
    // `join_all` spawns every task as its own fiber (they run concurrently) and collects their results positionally regardless of completion order. Here both children complete synchronously when scheduled, writing "a;" then "b;", and the gathered results [1, 2] sum to 3.
    assert_eq!(
        run(r#"
        use /std/{Async, Str, Nat, List, Io};
        let main : Async({}) =
            let rs = Async/join_all([
                Async/bind(Async/lift(Io/write(Io/stdout, Str/to_bytes("a;"))), (_) => Async/pure(1)),
                Async/bind(Async/lift(Io/write(Io/stdout, Str/to_bytes("b;"))), (_) => Async/pure(2))
            ])!;
            Io/write(Io/stdout, Str/to_bytes(Nat/to_str(Nat/add(/std/Option/unwrap_or(List/try_get(rs, 0), 0), /std/Option/unwrap_or(List/try_get(rs, 1), 0)))));
        Async/run(main)
        "#),
        b"a;b;3"
    );
}

#[test]
fn map_transforms_a_tasks_result() {
    // `Async/map` applies a pure function to a task's result — here turning the Nat 42 into its decimal string, with no explicit `bind`/`pure` at the call site.
    assert_eq!(
        run(r#"
        use /std/{Async, Str, Nat, Io};
        let main : Async({}) =
            let s = Async/map(Async/pure(42), Nat/to_str)!;
            Io/write(Io/stdout, Str/to_bytes(s));
        Async/run(main)
        "#),
        b"42"
    );
}

#[test]
fn race_returns_the_first_and_runs_a_cancelled_losers_finalizer() {
    // Multi-way `race`: the fast branch completes synchronously and wins, returning 10. The slow branch registers a finalizer with `using`, then sleeps far past the test — so it never writes "slow;". `race` cancels the loser, and because the loser holds a resource its finalizer still runs (here writing "released;") when the scheduler reclaims it on exit. Output proves the winner's value AND that the loser's cleanup fired without the loser's body completing.
    let (system, io) = ticking();
    run_text(
        r#"
        use /std/{Async, Str, Nat, Io};
        use /std/time/{Duration};
        let slow : Async(Nat) =
            let _ = Async/sleep(Duration/of_secs(60))!;
            let _ = Io/write(Io/stdout, Str/to_bytes("slow;"))!;
            Async/pure(20);
        let main : Async({}) =
            let v = Async/race([
                Async/bind(Async/lift(Io/write(Io/stdout, Str/to_bytes("fast;"))), (_) => Async/pure(10)),
                Async/using(/std/print("released;"), slow)
            ])!;
            Io/write(Io/stdout, Str/to_bytes(Nat/to_str(v)));
        Async/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"fast;10released;");
}

#[test]
fn constructing_a_leaf_task_performs_no_effect() {
    // Async values are inert until served. Building a stream read and discarding it must not touch stdin — the syscall fires only when the scheduler forces it. We construct (and drop) a read of stdin, then read stdin directly: the direct read still sees "hello" because the discarded Async never ran.
    let (system, io) = MockHost::builder().stdin_lines(["hello"]).build();
    run_text(
        r#"
        use /std/{Async, Str, Io, stream};
        let discarded : Async(stream/Chunk) = stream/Read/read(Io/stdin, 100);
        let r = Io/read(Io/stdin, 100)!;
        match r : (_) => /std/Io({})
        | chunk(bytes) => Io/write(Io/stdout, bytes)
        | eof() => Io/write(Io/stdout, Str/to_bytes("<eof>"))
        | error(_) => Io/write(Io/stdout, Str/to_bytes("<err>"))
        end
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"hello\n");
}

#[test]
fn finalizer_runs_for_a_child_parked_on_an_unwoken_fiber() {
    // A `go` child registers a finalizer with `using` (it writes "released;"), then joins a task that sleeps past the test — so it parks on the task's future, a waker nothing will fire before the root is done. The root writes "root;" and finishes. Because the scheduler retains ownership of every parked fiber, `block_on`'s shutdown drains the registry and runs the child's finalizer exactly once.
    let (system, io) = ticking();
    run_text(
        r#"
        use /std/{Async, Str, Io};
        use /std/time/{Duration};
        let main : Async({}) =
            let never = Async/spawn(Async/sleep(Duration/of_secs(1000)))!;
            let _ = Async/go(Async/using(/std/print("released;"), Async/join(never)))!;
            Io/write(Io/stdout, Str/to_bytes("root;"));
        Async/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"root;released;");
}

#[test]
fn a_brackets_finalizer_runs_when_its_body_completes() {
    // The success path: a fiber brackets a body ("body;") with a finalizer (writes "closed;") and finishes. The finalizer runs at the bracket's exit, so the output is "body;closed;" — cleanup happens for free on the success path.
    assert_eq!(
        run(r#"
        use /std/{Async, Str, Io};
        let main : Async({}) =
            Async/using(/std/print("closed;"), Async/lift(Io/write(Io/stdout, Str/to_bytes("body;"))));
        Async/run(main)
        "#),
        b"body;closed;"
    );
}

#[test]
fn a_brackets_finalizer_runs_once_and_completion_does_not_repeat_it() {
    // No double close: the bracket runs its finalizer ("closed;") on the way out and the fiber continues ("after;"); the guard is marked done, so the completion drain does not run it again. The single "closed;" between "body;" and "after;" proves it fired exactly once — at the exit, not again at the end.
    assert_eq!(
        run(r#"
        use /std/{Async, Str, Io};
        let main : Async({}) =
            let _ = Async/using(/std/print("closed;"), Async/lift(Io/write(Io/stdout, Str/to_bytes("body;"))))!;
            Io/write(Io/stdout, Str/to_bytes("after;"));
        Async/run(main)
        "#),
        b"body;closed;after;"
    );
}

#[test]
fn heterogeneous_existential_task_list_through_a_generic_map() {
    // An `List` of existential-boxed tasks of DIFFERENT result types, mapped by a generic HOF whose body does an indirect closure call on a continuation pulled out of the box. The arity-1 closure definition is inlined away by the specializer, leaving the `call_ref` with no surviving definition — the codegen path that needs the call-site arity registered for `envr`/`clsr`.
    assert_eq!(
        run(r#"
        use /std/{Str, Nat, List, Io};
        induct Susp(A : Type) : Type
        | now(A)
        | later(() -> Susp(A))
        end
        let Box : Type = { A : Type, t : Susp(A) };
        let boxes : List(Box) =
            [(Nat, Susp/now(7)), ({}, Susp/now(()))];
        let stepped = List/map(boxes, (b : Box) =>
            match b.t : (_) => Box
            | now(a) => (b.A, Susp/now(a))
            | later(k) => (b.A, k())
            end);
        let _ = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(List/len(stepped))))!;
        /std/Io/pure(())
        "#),
        b"2"
    );
}

#[test]
fn sleep_parks_until_the_clock_passes_the_deadline() {
    // The timer half of the poll contract: the root sleeps five seconds, so the scheduler parks it in the `sleeping` registry and drives `Handle/poll` with a finite timeout instead of `-1`. The mock's poll returns instantly and each `clock_mono` reading pops one scripted value, so the fiber resumes exactly when the scripted ramp passes the deadline — no readiness event involved.
    let (system, io) = ticking();
    run_text(
        r#"
        use /std/{Async, Str, Io};
        use /std/time/{Duration};
        let main : Async({}) =
            let _ = Async/sleep(Duration/of_secs(5))!;
            Io/write(Io/stdout, Str/to_bytes("woke;"));
        Async/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"woke;");
}

#[test]
fn sleepers_wake_in_deadline_order() {
    // Two spawned children sleep three and six seconds; the scheduler must pick the earliest deadline for each poll timeout and expire the timers in due order even though the six-second child was pushed onto `sleeping` later.
    let (system, io) = ticking();
    run_text(
        r#"
        use /std/{Async, Str, Io};
        use /std/time/{Duration};
        let mark(m : Str) -> Async({}) =
            Io/write(Io/stdout, Str/to_bytes(m));
        let main : Async({}) =
            let ha = Async/spawn(Async/bind(Async/sleep(Duration/of_secs(3)), (_) => mark("a;")))!;
            let hb = Async/spawn(Async/bind(Async/sleep(Duration/of_secs(6)), (_) => mark("b;")))!;
            let x = Async/join(ha)!;
            let y = Async/join(hb)!;
            mark("done");
        Async/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"a;b;done");
}

#[test]
fn timeout_returns_some_when_the_body_finishes_first() {
    // The body completes synchronously, so `select` resolves before the deadline fiber's timer matters; the cancelled timer is reclaimed on exit without ever waking. The result carries the body's value through `some`.
    let (system, io) = ticking();
    run_text(
        r#"
        use /std/{Async, Str, Nat, Io};
        use /std/time/{Duration};
        let main : Async({}) =
            let r = Async/timeout(Duration/of_secs(5), Async/pure(42))!;
            match r : (_) => Async({})
            | some(v) => Io/write(Io/stdout, Str/to_bytes(Nat/to_str(v)))
            | none() => Io/write(Io/stdout, Str/to_bytes("none"))
            end;
        Async/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"42");
}

#[test]
fn timeout_returns_none_and_runs_the_cancelled_bodys_finalizer() {
    // The deadline elapses first: the two-second timer wakes, wins the `select`, and the fifty-second body — which holds a resource via `using` — is cancelled while still sleeping. Its finalizer runs when the scheduler reclaims it on exit, after the root has already reported `none`.
    let (system, io) = ticking();
    run_text(
        r#"
        use /std/{Async, Str, Nat, Io};
        use /std/time/{Duration};
        let body : Async(Nat) =
            let _ = Async/sleep(Duration/of_secs(50))!;
            let _ = Io/write(Io/stdout, Str/to_bytes("body;"))!;
            Async/pure(0);
        let main : Async({}) =
            let r = Async/timeout(Duration/of_secs(2), Async/using(/std/print("released;"), body))!;
            match r : (_) => Async({})
            | some(v) => Io/write(Io/stdout, Str/to_bytes("some"))
            | none() => Io/write(Io/stdout, Str/to_bytes("none;"))
            end;
        Async/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"none;released;");
}

#[test]
fn race_of_two_sleeps_wakes_the_earlier_and_reclaims_the_later() {
    // Pure timer race: both branches sleep, so both land in `sleeping` and the poll timeout must track the earlier deadline. The two-second branch wakes, writes, and wins with 1; the sixty-second loser is cancelled and its `using` finalizer fires on reclamation — its body never runs.
    let (system, io) = ticking();
    run_text(
        r#"
        use /std/{Async, Str, Nat, Io};
        use /std/time/{Duration};
        let quick : Async(Nat) =
            let _ = Async/sleep(Duration/of_secs(2))!;
            let _ = Io/write(Io/stdout, Str/to_bytes("quick;"))!;
            Async/pure(1);
        let slow : Async(Nat) =
            let _ = Async/sleep(Duration/of_secs(60))!;
            let _ = Io/write(Io/stdout, Str/to_bytes("slow;"))!;
            Async/pure(2);
        let main : Async({}) =
            let v = Async/race([quick, Async/using(/std/print("released;"), slow)])!;
            Io/write(Io/stdout, Str/to_bytes(Nat/to_str(v)));
        Async/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"quick;1released;");
}

#[test]
fn block_on_drops_a_sleeping_child_when_root_done() {
    // Prompt drop and no deadlock: a fire-and-forget child holds a resource and sleeps far past the test, but the root finishes immediately. `block_on` must return without waiting out the timer, running the child's finalizer as it drains the `sleeping` registry, and never blocking on work nothing will ever join.
    let (system, io) = ticking();
    run_text(
        r#"
        use /std/{Async, Str, Io};
        use /std/time/{Duration};
        let child : Async({}) =
            let _ = Async/sleep(Duration/of_secs(100))!;
            Io/write(Io/stdout, Str/to_bytes("child;"));
        let main : Async({}) =
            let _ = Async/go(Async/using(/std/print("released;"), child))!;
            Io/write(Io/stdout, Str/to_bytes("root;"));
        Async/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"root;released;");
}

#[test]
fn a_deadlock_reports_how_many_fibers_wait_on_a_waker() {
    // Nothing runnable, nothing blocked on a handle, no sleeper: a `select` over no tasks awaits a winner no task will ever fulfil, so the root parks on a waker nothing fires. `block_on` reports the deadlock rather than hanging, and the report counts that one parked fiber.
    assert_eq!(
        run(r#"
        use /std/{Async, Str, Nat, Io};
        let stuck : Async(Nat) =
            let w = Async/select(@Nat, [])!;
            Async/pure(w.1);
        let outcome = Async/block_on(stuck)!;
        match outcome
        | success(_) => /std/print("finished")
        | failure(d) => /std/print(Str/concat("deadlock ", Nat/to_str(d.parked)))
        end
        "#),
        b"deadlock 1"
    );
}

// A signal is the one park a program may take on its own account: the root waits, a fiber gone to sleep under the ticking clock notifies, and the root resumes after it. A notify that lands before anyone waits is kept, so the second wait answers at once rather than parking on a flag already raised.
#[test]
fn a_signal_parks_the_waiter_until_it_is_notified_and_keeps_an_early_notify() {
    let (system, io) = ticking();
    run_text(
        r#"
        use /std/{Str, Async, Io};
        use /std/Async/{Signal};
        use /std/time/{Duration};
        let fiber: Async({}) =
            let s = Signal/new()!;
            let _ = Async/go(
                let _ = Async/sleep(Duration/of_secs(1))!;
                let _ = /std/print("notify ")!;
                Signal/notify(s))!;
            let _ = /std/print("wait ")!;
            let _ = Signal/wait(s)!;
            let _ = /std/print("woken ")!;
            let _ = Signal/notify(s)!;
            let _ = Signal/wait(s)!;
            /std/print("again");
        Async/run(fiber)
        "#,
        system,
    )
    .expect("expected result");
    // Which of the two runs first is the scheduler's ordering, not the signal's contract: a notify that lands before the wait is kept and answered at once, and one that lands after wakes the parked waiter. Either way the root comes through both waits, which is the claim.
    let output = io.output();
    assert!(
        output == b"wait notify woken again" || output == b"notify wait woken again",
        "{:?}",
        String::from_utf8_lossy(&output)
    );
}
