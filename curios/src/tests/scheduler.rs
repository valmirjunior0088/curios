use {curios_runtime::MockHost, std::time::Duration};

#[test]
fn task_scheduler_parks_polls_and_resumes() {
    // The `/std/Task` event loop end to end: the root fiber yields a `wait` on
    // stdin-READ and parks, `run` marshals the parked handle/interest into
    // `Io/poll` (the mock reports it ready), and resumes the continuation — which
    // performs the write. Exercises the novel path of an inductive variant carrying a
    // closure through erasure and codegen.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Task, Io, Str};
        let prog : Task({}) =
            Task/bind(Task/wait(Io/stdin, 1), (_) =>
                let wrote = Io/write(Io/stdout, Str/to_bytes("ok"));
                Task/pure(()));
        Task/run(prog)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"ok");
}

#[test]
fn task_bind_reads_and_echoes() {
    // The monad surface: a `with`-bind do-block over `Task/bind`, sequencing the
    // `read` leaf (which completes without parking under the mock) into `write`,
    // driven to its value by `block_on`. Exercises `bind`, the leaf actions, and
    // do-notation against the new module.
    let (system, io) = MockHost::builder().stdin_lines(["hello"]).build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Task, Io};
        let prog : Task({}) =
            let r = Task/read(Io/stdin, 1024)!;
            match r : Task({})
            | chunk(bytes) =>
                let wrote = Io/write(Io/stdout, bytes);
                Task/pure(())
            | eof() => Task/pure(())
            | error(_) => Task/pure(())
            end;
        Task/block_on(prog)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"hello\n");
}

#[test]
fn block_on_returns_a_typed_value_and_awaits_a_spawned_child() {
    // `block_on` returns a typed value AND a spawned child runs because the root
    // explicitly `await`s it: the root spawns a child (which parks on stdin),
    // writes "root;", then awaits the child's future. Awaiting parks the root on
    // the future, so the child is polled awake, writes "child;", and fulfils the
    // future with 5; the root resumes and `block_on` hands back 5 + 2 = 7.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Task, Io, Str, Nat};
        let root : Task(Nat) =
            let f = Task/spawn(() =>
                Task/bind(Task/wait(Io/stdin, 1), (_) =>
                    let w = Io/write(Io/stdout, Str/to_bytes("child;"));
                    Task/pure(5)))!;
            let w = Io/write(Io/stdout, Str/to_bytes("root;"));
            let c = Task/await(f.result)!;
            Task/pure(Nat/add(c, 2));
        Io/write(Io/stdout, Str/to_bytes(Nat/to_str(Task/block_on(root))))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"root;child;7");
}

#[test]
fn join_all_runs_children_concurrently_and_collects_in_order() {
    // `join_all` spawns every task as its own fiber (they run concurrently) and
    // collects their results positionally regardless of completion order. Here both
    // children complete synchronously when scheduled, writing "a;" then "b;", and
    // the gathered results [1, 2] sum to 3.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Task, Io, Str, Nat, Lst};
        let main : Task({}) =
            let rs = Task/join_all([
                () =>
                    let w = Io/write(Io/stdout, Str/to_bytes("a;"));
                    Task/pure(1),
                () =>
                    let w = Io/write(Io/stdout, Str/to_bytes("b;"));
                    Task/pure(2)
            ])!;
            let s = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(Nat/add(/std/Option/unwrap_or(Lst/get(rs, 0), 0), /std/Option/unwrap_or(Lst/get(rs, 1), 0)))));
            Task/pure(());
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"a;b;3");
}

#[test]
fn map_transforms_a_tasks_result() {
    // `Task/map` applies a pure function to a task's result — here turning the Nat
    // 42 into its decimal string, with no explicit `bind`/`pure` at the call site.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Task, Io, Str, Nat};
        let main : Task({}) =
            let s = Task/map(Task/pure(42), Nat/to_str)!;
            let w = Io/write(Io/stdout, Str/to_bytes(s));
            Task/pure(());
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"42");
}

#[test]
fn race_returns_the_first_and_runs_a_cancelled_losers_finalizer() {
    // Multi-way `race`: the fast branch completes synchronously and wins, returning
    // 10. The slow branch acquires a finalizer with `using`, then parks on stdin —
    // so it never writes "slow;". `race` cancels the loser, and because the loser
    // holds a resource its finalizer still runs (here writing "released;") when the
    // scheduler reclaims it on exit. Output proves the winner's value AND that the
    // loser's cleanup fired without the loser's body completing.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Task, Io, Str, Nat};
        let main : Task({}) =
            let v = Task/race([
                () =>
                    let x = Io/write(Io/stdout, Str/to_bytes("fast;"));
                    Task/pure(10),
                () =>
                    Task/using(Io/stdin, () => let r = Io/write(Io/stdout, Str/to_bytes("released;")); (),
                        Task/bind(Task/wait(Io/stdin, 1), (_) =>
                            let y = Io/write(Io/stdout, Str/to_bytes("slow;"));
                            Task/pure(20)))
            ])!;
            let z = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(v)));
            Task/pure(());
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"fast;10released;");
}

#[test]
fn block_on_drops_a_parked_child_when_root_done() {
    // Prompt drop and no deadlock: a fire-and-forget `go` child parks on stdin, but
    // the root writes and finishes first. `block_on` returns the instant the root
    // is done, dropping the still-parked child instead of blocking forever in
    // `Io/poll` on work nothing will ever join. Only "root;" is written, and `run`
    // returns rather than hanging.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Task, Io, Str};
        let child : Task({}) =
            Task/bind(Task/wait(Io/stdin, 1), (_) =>
                let w = Io/write(Io/stdout, Str/to_bytes("child;"));
                Task/pure(()));
        let main : Task({}) =
            Task/bind(Task/go(() => child), (started) =>
                let w = Io/write(Io/stdout, Str/to_bytes("root;"));
                Task/pure(()));
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"root;");
}

#[test]
fn constructing_a_leaf_task_performs_no_effect() {
    // Tasks are inert until served. Building a `Task/read` and discarding it must not
    // touch stdin — the syscall is wrapped in `defer`, so it fires only when the
    // scheduler forces it. We construct (and drop) a read of stdin, then read stdin
    // directly: the direct read still sees "hello" because the discarded Task never
    // ran. Before leaves were deferred, constructing the Task ate stdin eagerly and
    // the direct read saw EOF.
    let (system, io) = MockHost::builder().stdin_lines(["hello"]).build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Task, Io, Str};
        let discarded : Task(Io/Read) = Task/read(Io/stdin, 100);
        let r = Io/read(Io/stdin, 100);
        match r : {}
        | chunk(bytes) => let _ = Io/write(Io/stdout, bytes); ()
        | eof() => let _ = Io/write(Io/stdout, Str/to_bytes("<eof>")); ()
        | error(_) => let _ = Io/write(Io/stdout, Str/to_bytes("<err>")); ()
        end
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"hello\n");
}

#[test]
fn finalizer_runs_for_a_child_parked_on_an_unfulfilled_future() {
    // The previously-leaking path, now closed. A `go` child acquires a resource via
    // `using` (its finalizer writes "released;"), then `await`s a future that nothing
    // ever fulfils — so it parks in the scheduler's `parked` registry and is never
    // woken. The root writes "root;" and finishes. Because the scheduler now retains
    // ownership of every parked fiber (rather than handing it off to the future's
    // waker list, where it was invisible), `block_on`'s shutdown drains the registry
    // and runs the child's finalizer exactly once. Before the fix the "released;"
    // marker leaked and the output was just "root;".
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Task, Io, Str};
        let main : Task({}) =
            let f : Task/Future({}) = Task/new_future(@{});
            let started = Task/go(() =>
                Task/using(Io/stdin, () => let r = Io/write(Io/stdout, Str/to_bytes("released;")); (),
                    Task/await(f)))!;
            let w = Io/write(Io/stdout, Str/to_bytes("root;"));
            Task/pure(());
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"root;released;");
}

#[test]
fn an_acquired_finalizer_runs_when_the_fiber_completes() {
    // "Open and trust it", normal path: a fiber `acquire`s a finalizer (writes
    // "closed;"), runs its body ("body;"), and finishes without ever calling
    // `release`. The scheduler runs the finalizer on completion, so the output is
    // "body;closed;" — cleanup happens for free on the success path.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Task, Io, Str};
        let main : Task({}) =
            let _ = Task/acquire(Io/stdin, () => let r = Io/write(Io/stdout, Str/to_bytes("closed;")); ())!;
            let _ = Io/write(Io/stdout, Str/to_bytes("body;"));
            Task/pure(());
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"body;closed;");
}

#[test]
fn manual_release_runs_a_finalizer_once_and_completion_does_not_repeat_it() {
    // "Close it yourself, no double close": a fiber `acquire`s a finalizer (writes
    // "closed;"), runs its body ("body;"), then manually `release`s and continues
    // ("after;"). `release` runs the finalizer AND dequeues the guard, so the
    // completion drain does not run it again. The single "closed;" between "body;"
    // and "after;" proves it fired exactly once — at the release, not again at the end.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Task, Io, Str};
        let main : Task({}) =
            let _ = Task/acquire(Io/stdin, () => let r = Io/write(Io/stdout, Str/to_bytes("closed;")); ())!;
            let _ = Io/write(Io/stdout, Str/to_bytes("body;"));
            let _ = Task/release(Io/stdin)!;
            let _ = Io/write(Io/stdout, Str/to_bytes("after;"));
            Task/pure(());
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"body;closed;after;");
}

#[test]
fn heterogeneous_existential_task_list_through_a_generic_map() {
    // An `Lst` of existential-boxed tasks of DIFFERENT result types, mapped by a
    // generic HOF whose body does an indirect closure call on a continuation
    // pulled out of the box. The arity-1 closure definition is inlined away by
    // the specializer, leaving the `call_ref` with no surviving definition — the
    // codegen path that needs the call-site arity registered for `envr`/`clsr`.
    let (system, io) = MockHost::builder().build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Io, Str, Nat, Lst};
        induct Susp(A : Type) : Type
        | now(A)
        | later(() -> Susp(A))
        end
        let Box : Type = { A : Type, t : Susp(A) };
        let boxes : Lst(Box) =
            [(Nat, Susp/now(7)), ({}, Susp/now(()))];
        let stepped = Lst/map(boxes, (b : Box) =>
            match b.t : Box
            | now(a) => (b.A, Susp/now(a))
            | later(k) => (b.A, k())
            end);
        Io/write(Io/stdout, Str/to_bytes(Nat/to_str(Lst/len(stepped))))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"2");
}

#[test]
fn sleep_parks_until_the_clock_passes_the_deadline() {
    // The timer half of the poll contract: the root sleeps five seconds, so the
    // scheduler parks it in the `sleeping` registry and drives `Io/poll` with a
    // finite timeout instead of `-1`. The mock's poll returns instantly and each
    // `clock_mono` reading pops one scripted value, so the fiber resumes exactly
    // when the scripted ramp passes the deadline — no readiness event involved.
    let (system, io) = MockHost::builder().mono((0..40u32).map(|s| (s, 0))).build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Task, Io, Str};
        use /std/time/{Duration};
        let main : Task({}) =
            Task/bind(Task/sleep(Duration/of_secs(5)), (_) =>
                let w = Io/write(Io/stdout, Str/to_bytes("woke;"));
                Task/pure(()));
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"woke;");
}

#[test]
fn sleepers_wake_in_deadline_order() {
    // Two spawned children sleep three and six seconds; the scheduler must pick
    // the earliest deadline for each poll timeout and expire the timers in due
    // order even though the six-second child was pushed onto `sleeping` later.
    let (system, io) = MockHost::builder().mono((0..40u32).map(|s| (s, 0))).build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Task, Io, Str};
        use /std/time/{Duration};
        let mark(m : Str) -> Task({}) =
            let w = Io/write(Io/stdout, Str/to_bytes(m));
            Task/pure(());
        let main : Task({}) =
            let ha = Task/spawn(() => Task/bind(Task/sleep(Duration/of_secs(3)), (_) => mark("a;")))!;
            let hb = Task/spawn(() => Task/bind(Task/sleep(Duration/of_secs(6)), (_) => mark("b;")))!;
            let x = Task/await(ha.result)!;
            let y = Task/await(hb.result)!;
            mark("done");
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"a;b;done");
}

#[test]
fn timeout_returns_some_when_the_body_finishes_first() {
    // The body completes synchronously, so `select` resolves before the deadline
    // fiber's timer matters; the cancelled timer is reclaimed on exit without
    // ever waking. The result carries the body's value through `some`.
    let (system, io) = MockHost::builder().mono((0..40u32).map(|s| (s, 0))).build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Task, Io, Str, Nat};
        use /std/time/{Duration};
        let main : Task({}) =
            let r = Task/timeout(Duration/of_secs(5), () => Task/pure(42))!;
            match r : Task({})
            | some(v) => let w = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(v))); Task/pure(())
            | none() => let w = Io/write(Io/stdout, Str/to_bytes("none")); Task/pure(())
            end;
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"42");
}

#[test]
fn timeout_returns_none_and_runs_the_cancelled_bodys_finalizer() {
    // The deadline elapses first: the two-second timer wakes, wins the `select`,
    // and the fifty-second body — which holds a resource via `using` — is
    // cancelled while still sleeping. Its finalizer runs when the scheduler
    // reclaims it on exit, after the root has already reported `none`.
    let (system, io) = MockHost::builder().mono((0..40u32).map(|s| (s, 0))).build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Task, Io, Str, Nat};
        use /std/time/{Duration};
        let main : Task({}) =
            let r = Task/timeout(Duration/of_secs(2), () =>
                Task/using(Io/stdin, () => let w = Io/write(Io/stdout, Str/to_bytes("released;")); (),
                    Task/bind(Task/sleep(Duration/of_secs(50)), (_) =>
                        let w = Io/write(Io/stdout, Str/to_bytes("body;"));
                        Task/pure(0))))!;
            match r : Task({})
            | some(v) => let w = Io/write(Io/stdout, Str/to_bytes("some")); Task/pure(())
            | none() => let w = Io/write(Io/stdout, Str/to_bytes("none;")); Task/pure(())
            end;
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"none;released;");
}

#[test]
fn race_of_two_sleeps_wakes_the_earlier_and_reclaims_the_later() {
    // Pure timer race: both branches sleep, so both land in `sleeping` and the
    // poll timeout must track the earlier deadline. The two-second branch wakes,
    // writes, and wins with 1; the sixty-second loser is cancelled and its
    // `using` finalizer fires on reclamation — its body never runs.
    let (system, io) = MockHost::builder().mono((0..40u32).map(|s| (s, 0))).build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Task, Io, Str, Nat};
        use /std/time/{Duration};
        let main : Task({}) =
            let v = Task/race([
                () => Task/bind(Task/sleep(Duration/of_secs(2)), (_) =>
                    let w = Io/write(Io/stdout, Str/to_bytes("quick;"));
                    Task/pure(1)),
                () => Task/using(Io/stdin, () => let w = Io/write(Io/stdout, Str/to_bytes("released;")); (),
                    Task/bind(Task/sleep(Duration/of_secs(60)), (_) =>
                        let w = Io/write(Io/stdout, Str/to_bytes("slow;"));
                        Task/pure(2)))
            ])!;
            let z = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(v)));
            Task/pure(());
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"quick;1released;");
}

#[test]
fn block_on_drops_a_sleeping_child_when_root_done() {
    // The sleeping counterpart of the parked-child drop: a fire-and-forget child
    // holds a resource and sleeps far past the test, but the root finishes
    // immediately. `block_on` must return without waiting out the timer, running
    // the child's finalizer as it drains the `sleeping` registry.
    let (system, io) = MockHost::builder().mono((0..40u32).map(|s| (s, 0))).build();
    crate::run_text(
        Duration::from_secs(10),
        r#"
        use /std/{Task, Io, Str};
        use /std/time/{Duration};
        let child : Task({}) =
            Task/using(Io/stdin, () => let w = Io/write(Io/stdout, Str/to_bytes("released;")); (),
                Task/bind(Task/sleep(Duration/of_secs(100)), (_) =>
                    let w = Io/write(Io/stdout, Str/to_bytes("child;"));
                    Task/pure(())));
        let main : Task({}) =
            Task/bind(Task/go(() => child), (_) =>
                let w = Io/write(Io/stdout, Str/to_bytes("root;"));
                Task/pure(()));
        Task/run(main)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"root;released;");
}
