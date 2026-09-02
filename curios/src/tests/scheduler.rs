use {
    super::{run, run_text},
    curios_runtime::MockHost,
};

#[test]
fn task_scheduler_parks_polls_and_resumes() {
    // The `/std/Async` event loop end to end: the root fiber yields a `wait` on stdin-READ and parks, `run` marshals the parked handle/interest into `Handle/poll` (the mock reports it ready), and resumes the continuation — which performs the write. Exercises the novel path of an inductive variant carrying a closure through erasure and codegen.
    assert_eq!(
        run(r#"
        use /std/{Async, Handle, Str, Io, stream};
        let prog : Async({}) =
            Async/bind(Async/wait(Handle/stdin, 1), (_) =>
                let wrote = Async/lift(Io/write(Io/stdout, Str/to_bytes("ok")))!;
                Async/pure(()));
        Async/run(prog)
        "#),
        b"ok"
    );
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
    // The monad surface: a `with`-bind do-block over `Async/bind`, sequencing the `read` leaf (which completes without parking under the mock) into `write`, driven to its value by `block_on`. Exercises `bind`, the leaf actions, and do-notation against the new module.
    let (system, io) = MockHost::builder().stdin_lines(["hello"]).build();
    run_text(
        r#"
        use /std/{Async, Handle, Io, stream};
        let prog : Async({}) =
            let r = Async/raw/read(Handle/stdin, 1024)!;
            match r : (_) => Async({})
            | chunk(bytes) =>
                let wrote = Async/lift(Io/write(Io/stdout, bytes))!;
                Async/pure(())
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
    // `block_on` returns a typed value AND a spawned child runs because the root explicitly `await`s it: the root spawns a child (which parks on stdin), writes "root;", then awaits the child's future. Awaiting parks the root on the future, so the child is polled awake, writes "child;", and fulfils the future with 5; the root resumes and `block_on` hands back 5 + 2 = 7.
    assert_eq!(
        run(r#"
        use /std/{Async, Handle, Str, Nat, Io, stream};
        let root : Async(Nat) =
            let f = Async/spawn(
                Async/bind(Async/wait(Handle/stdin, 1), (_) =>
                    let w = Async/lift(Io/write(Io/stdout, Str/to_bytes("child;")))!;
                    Async/pure(5)))!;
            let w = Async/lift(Io/write(Io/stdout, Str/to_bytes("root;")))!;
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
        use /std/{Async, Handle, Str, Nat, List, Io, stream};
        let main : Async({}) =
            let rs = Async/join_all([
                Async/bind(Async/lift(Io/write(Io/stdout, Str/to_bytes("a;"))), (_) => Async/pure(1)),
                Async/bind(Async/lift(Io/write(Io/stdout, Str/to_bytes("b;"))), (_) => Async/pure(2))
            ])!;
            let s = Async/lift(Io/write(Io/stdout, Str/to_bytes(Nat/to_str(Nat/add(/std/Option/unwrap_or(List/try_get(rs, 0), 0), /std/Option/unwrap_or(List/try_get(rs, 1), 0))))))!;
            Async/pure(());
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
        use /std/{Async, Handle, Str, Nat, Io, stream};
        let main : Async({}) =
            let s = Async/map(Async/pure(42), Nat/to_str)!;
            let w = Async/lift(Io/write(Io/stdout, Str/to_bytes(s)))!;
            Async/pure(());
        Async/run(main)
        "#),
        b"42"
    );
}

#[test]
fn race_returns_the_first_and_runs_a_cancelled_losers_finalizer() {
    // Multi-way `race`: the fast branch completes synchronously and wins, returning 10. The slow branch acquires a finalizer with `using`, then parks on stdin — so it never writes "slow;". `race` cancels the loser, and because the loser holds a resource its finalizer still runs (here writing "released;") when the scheduler reclaims it on exit. Output proves the winner's value AND that the loser's cleanup fired without the loser's body completing.
    assert_eq!(
        run(r#"
        use /std/{Async, Handle, Str, Nat, Io, stream};
        let main : Async({}) =
            let v = Async/race([
                Async/bind(Async/lift(Io/write(Io/stdout, Str/to_bytes("fast;"))), (_) => Async/pure(10)),
                Async/using(Handle/stdin, /std/print("released;"),
                        Async/bind(Async/wait(Handle/stdin, 1), (_) =>
                let y = Async/lift(Io/write(Io/stdout, Str/to_bytes("slow;")))!;
                Async/pure(20)))
            ])!;
            let z = Async/lift(Io/write(Io/stdout, Str/to_bytes(Nat/to_str(v))))!;
            Async/pure(());
        Async/run(main)
        "#),
        b"fast;10released;"
    );
}

#[test]
fn block_on_drops_a_parked_child_when_root_done() {
    // Prompt drop and no deadlock: a fire-and-forget `go` child parks on stdin, but the root writes and finishes first. `block_on` returns the instant the root is done, dropping the still-parked child instead of blocking forever in `Handle/poll` on work nothing will ever join. Only "root;" is written, and `run` returns rather than hanging.
    assert_eq!(
        run(r#"
        use /std/{Async, Handle, Str, Io, stream};
        let child : Async({}) =
            Async/bind(Async/wait(Handle/stdin, 1), (_) =>
                let w = Async/lift(Io/write(Io/stdout, Str/to_bytes("child;")))!;
                Async/pure(()));
        let main : Async({}) =
            Async/bind(Async/go(child), (started) =>
                let w = Async/lift(Io/write(Io/stdout, Str/to_bytes("root;")))!;
                Async/pure(()));
        Async/run(main)
        "#),
        b"root;"
    );
}

#[test]
fn constructing_a_leaf_task_performs_no_effect() {
    // Async values are inert until served. Building a `Async/read` and discarding it must not touch stdin — the syscall is wrapped in `defer`, so it fires only when the scheduler forces it. We construct (and drop) a read of stdin, then read stdin directly: the direct read still sees "hello" because the discarded Async never ran. Before leaves were deferred, constructing the Async ate stdin eagerly and the direct read saw EOF.
    let (system, io) = MockHost::builder().stdin_lines(["hello"]).build();
    run_text(
        r#"
        use /std/{Async, Handle, Str, Io, stream};
        let discarded : Async(stream/Chunk) = Async/raw/read(Handle/stdin, 100);
        let r = Io/read(Io/stdin, 100)!;
        match r : (_) => /std/Io({})
        | chunk(bytes) => let _ = Io/write(Io/stdout, bytes)!; /std/Io/pure(())
        | eof() => let _ = Io/write(Io/stdout, Str/to_bytes("<eof>"))!; /std/Io/pure(())
        | error(_) => let _ = Io/write(Io/stdout, Str/to_bytes("<err>"))!; /std/Io/pure(())
        end
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"hello\n");
}

#[test]
fn finalizer_runs_for_a_child_parked_on_an_unwoken_fiber() {
    // The previously-leaking path, now closed. A `go` child acquires a resource via `using` (its finalizer writes "released;"), then `park`s with a register that drops its waker — so it lands in the scheduler's `parked` registry and nothing can ever wake it. The root writes "root;" and finishes. Because the scheduler now retains ownership of every parked fiber (rather than handing it off to a waker list, where it was invisible), `block_on`'s shutdown drains the registry and runs the child's finalizer exactly once. Before the fix the "released;" marker leaked and the output was just "root;".
    assert_eq!(
        run(r#"
        use /std/{Async, Handle, Str, Io, stream};
        use /std/Async/{Waker};
        let main : Async({}) =
            let started = Async/go(
                Async/using(Handle/stdin, /std/print("released;"),
                    Async/park((w : Waker) => /std/Io/pure(()))))!;
            let w = Async/lift(Io/write(Io/stdout, Str/to_bytes("root;")))!;
            Async/pure(());
        Async/run(main)
        "#),
        b"root;released;"
    );
}

#[test]
fn an_acquired_finalizer_runs_when_the_fiber_completes() {
    // "Open and trust it", normal path: a fiber `acquire`s a finalizer (writes "closed;"), runs its body ("body;"), and finishes without ever calling `release`. The scheduler runs the finalizer on completion, so the output is "body;closed;" — cleanup happens for free on the success path.
    assert_eq!(
        run(r#"
        use /std/{Async, Handle, Str, Io, stream};
        let main : Async({}) =
            let _ = Async/acquire(Handle/stdin, /std/print("closed;"))!;
            let _ = Async/lift(Io/write(Io/stdout, Str/to_bytes("body;")))!;
            Async/pure(());
        Async/run(main)
        "#),
        b"body;closed;"
    );
}

#[test]
fn manual_release_runs_a_finalizer_once_and_completion_does_not_repeat_it() {
    // "Close it yourself, no double close": a fiber `acquire`s a finalizer (writes "closed;"), runs its body ("body;"), then manually `release`s and continues ("after;"). `release` runs the finalizer AND dequeues the guard, so the completion drain does not run it again. The single "closed;" between "body;" and "after;" proves it fired exactly once — at the release, not again at the end.
    assert_eq!(
        run(r#"
        use /std/{Async, Handle, Str, Io, stream};
        let main : Async({}) =
            let _ = Async/acquire(Handle/stdin, /std/print("closed;"))!;
            let _ = Async/lift(Io/write(Io/stdout, Str/to_bytes("body;")))!;
            let _ = Async/release(Handle/stdin)!;
            let _ = Async/lift(Io/write(Io/stdout, Str/to_bytes("after;")))!;
            Async/pure(());
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
        use /std/{Handle, Str, Nat, List, Io, stream};
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
    let (system, io) = MockHost::builder().mono((0..40u32).map(|s| (s, 0))).build();
    run_text(
        r#"
        use /std/{Async, Handle, Str, Io, stream};
        use /std/time/{Duration};
        let main : Async({}) =
            Async/bind(Async/sleep(Duration/of_secs(5)), (_) =>
                let w = Async/lift(Io/write(Io/stdout, Str/to_bytes("woke;")))!;
                Async/pure(()));
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
    let (system, io) = MockHost::builder().mono((0..40u32).map(|s| (s, 0))).build();
    run_text(
        r#"
        use /std/{Async, Handle, Str, Io, stream};
        use /std/time/{Duration};
        let mark(m : Str) -> Async({}) =
            let w = Async/lift(Io/write(Io/stdout, Str/to_bytes(m)))!;
            Async/pure(());
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
    let (system, io) = MockHost::builder().mono((0..40u32).map(|s| (s, 0))).build();
    run_text(r#"
        use /std/{Async, Handle, Str, Nat, Io, stream};
        use /std/time/{Duration};
        let main : Async({}) =
            let r = Async/timeout(Duration/of_secs(5), Async/pure(42))!;
            match r : (_) => Async({})
            | some(v) => let w = Async/lift(Io/write(Io/stdout, Str/to_bytes(Nat/to_str(v))))!; Async/pure(())
            | none() => let w = Async/lift(Io/write(Io/stdout, Str/to_bytes("none")))!; Async/pure(())
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
    let (system, io) = MockHost::builder().mono((0..40u32).map(|s| (s, 0))).build();
    run_text(r#"
        use /std/{Async, Handle, Str, Nat, Io, stream};
        use /std/time/{Duration};
        let main : Async({}) =
            let r = Async/timeout(Duration/of_secs(2),
                Async/using(Handle/stdin, /std/print("released;"),
                    Async/bind(Async/sleep(Duration/of_secs(50)), (_) =>
                        let w = Async/lift(Io/write(Io/stdout, Str/to_bytes("body;")))!;
                        Async/pure(0))))!;
            match r : (_) => Async({})
            | some(v) => let w = Async/lift(Io/write(Io/stdout, Str/to_bytes("some")))!; Async/pure(())
            | none() => let w = Async/lift(Io/write(Io/stdout, Str/to_bytes("none;")))!; Async/pure(())
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
    let (system, io) = MockHost::builder().mono((0..40u32).map(|s| (s, 0))).build();
    run_text(
        r#"
        use /std/{Async, Handle, Str, Nat, Io, stream};
        use /std/time/{Duration};
        let main : Async({}) =
            let v = Async/race([
                Async/bind(Async/sleep(Duration/of_secs(2)), (_) =>
                    let w = Async/lift(Io/write(Io/stdout, Str/to_bytes("quick;")))!;
                    Async/pure(1)),
                Async/using(Handle/stdin, /std/print("released;"),
                    Async/bind(Async/sleep(Duration/of_secs(60)), (_) =>
                        let w = Async/lift(Io/write(Io/stdout, Str/to_bytes("slow;")))!;
                        Async/pure(2)))
            ])!;
            let z = Async/lift(Io/write(Io/stdout, Str/to_bytes(Nat/to_str(v))))!;
            Async/pure(());
        Async/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"quick;1released;");
}

#[test]
fn block_on_drops_a_sleeping_child_when_root_done() {
    // The sleeping counterpart of the parked-child drop: a fire-and-forget child holds a resource and sleeps far past the test, but the root finishes immediately. `block_on` must return without waiting out the timer, running the child's finalizer as it drains the `sleeping` registry.
    let (system, io) = MockHost::builder().mono((0..40u32).map(|s| (s, 0))).build();
    run_text(
        r#"
        use /std/{Async, Handle, Str, Io, stream};
        use /std/time/{Duration};
        let child : Async({}) =
            Async/using(Handle/stdin, /std/print("released;"),
                Async/bind(Async/sleep(Duration/of_secs(100)), (_) =>
                    let w = Async/lift(Io/write(Io/stdout, Str/to_bytes("child;")))!;
                    Async/pure(())));
        let main : Async({}) =
            Async/bind(Async/go(child), (_) =>
                let w = Async/lift(Io/write(Io/stdout, Str/to_bytes("root;")))!;
                Async/pure(()));
        Async/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"root;released;");
}
