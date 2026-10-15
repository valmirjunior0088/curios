//! Descriptions run side by side through `/std/Io`'s public surface: waiting on a handle, a timer or a cell and resuming, a scope and its children, the combinators and the branches they cancel, brackets released on every exit, and the deadlock the host reports.

mod waiting_tests;

use {
    super::{run, run_text},
    curios_runtime::MockHost,
};

/// A host whose monotonic clock climbs one second per reading, so a sleeper's deadline is passed by the scripted ramp rather than by waiting.
fn ticking() -> (MockHost, curios_runtime::MockIo) {
    MockHost::builder().mono((0..40u64).map(|s| (s, 0))).build()
}

// A response scripted in two chunks reaches the reader only through a wait: the first read spends chunk one, the second answers would-block and the read suspends on a watch of the socket, the host's wait arms chunk two, and the drain runs on to the end. A host that is always ready never takes that path, which is why the chunked script exists.
#[test]
fn a_read_waits_and_resumes_across_a_chunk_boundary() {
    let (system, io) = MockHost::builder()
        .net_chunks([("example.com:80", vec!["ab", "cd"])])
        .build();
    run_text(
        r#"
        use /std/{Str, Show, Try, Io};
        use /std/tcp/{Socket};
        let program: Io({}) =
            let r = Try/run(Socket/call(Socket/connect("example.com", 80), Str/to_bytes("GET /\r\n\r\n")))!;
            match r
            | success(response) => Io/write(Io/stdout, response)
            | failure(e) => /std/print(Show/show(e))
            end;
        program
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"abcd");
}

// A stream read sequenced into a write: the standard input's read completes without waiting under the mock.
#[test]
fn a_stream_read_sequences_into_a_write() {
    let (system, io) = MockHost::builder().stdin_lines(["hello"]).build();
    run_text(
        r#"
        use /std/{Io};
        let program: Io({}) =
            let r = Io/Read/read(Io/stdin, 1024)!;
            match r : (_) => Io({})
            | chunk(bytes, @_) => Io/write(Io/stdout, bytes)
            | eof() => Io/pure(())
            | error(_) => Io/pure(())
            end;
        program
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"hello\n");
}

// A scope answers with its body, and a child the body joins runs to its answer first: the body spawns the child, writes "root;" and waits on the child's task; the child, started at the scope's next pass, yields, writes "child;" and answers 5; the body resumes and answers 5 + 2.
#[test]
fn a_scope_answers_with_its_body_after_the_child_it_joins() {
    assert_eq!(
        run(r#"
        use /std/{Str, Nat, Io};
        let child: Io(Nat) =
            let _ = Io/yield()!;
            let _ = Io/write(Io/stdout, Str/to_bytes("child;"))!;
            Io/pure(5);
        let root(s: Io/Scope) -> Io(Nat) =
            let f = Io/spawn(s, child)!;
            let _ = Io/write(Io/stdout, Str/to_bytes("root;"))!;
            let c = Io/join(f)!;
            Io/pure(c + 2);
        let n = Io/scope(root)!;
        Io/write(Io/stdout, Str/to_bytes(Nat/to_str(n)))
        "#),
        b"root;child;7"
    );
}

// `all` answers each action's answer in the actions' order: both complete when first advanced, writing "a;" then "b;", and the answers [1, 2] sum to 3.
#[test]
fn all_answers_in_the_order_of_its_actions() {
    assert_eq!(
        run(r#"
        use /std/{Str, Nat, List, Io};
        let program: Io({}) =
            let rs = Io/all([
                Io/bind(Io/write(Io/stdout, Str/to_bytes("a;")), (_) => Io/pure(1)),
                Io/bind(Io/write(Io/stdout, Str/to_bytes("b;")), (_) => Io/pure(2))
            ])!;
            Io/write(Io/stdout, Str/to_bytes(Nat/to_str(Nat/add(/std/Option/unwrap_or(List/try_get(rs, 0), 0), /std/Option/unwrap_or(List/try_get(rs, 1), 0)))));
        program
        "#),
        b"a;b;3"
    );
}

// `race` cancels a loser it started before it answers: the slow branch enters its bracket and sleeps, the fast one answers 10, and the slow one's release runs where it waits — before the race answers — while its body never writes.
#[test]
fn race_cancels_a_started_loser_and_runs_its_release_before_answering() {
    let (system, io) = ticking();
    run_text(
        r#"
        use /std/{Str, Nat, Io};
        use /std/time/{Duration};
        let slow: Io(Nat) =
            let _ = Io/sleep(Duration/of_secs(60))!;
            let _ = Io/write(Io/stdout, Str/to_bytes("slow;"))!;
            Io/pure(20);
        let program: Io({}) =
            let v = Io/race([
                Io/using((_) => /std/print("released;"), slow),
                Io/bind(Io/write(Io/stdout, Str/to_bytes("fast;")), (_) => Io/pure(10))
            ])!;
            Io/write(Io/stdout, Str/to_bytes(Nat/to_str(v)));
        program
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"fast;released;10");
}

// A description is inert until performed: building a stream read and discarding it does not touch the standard input, so the read performed after it still sees "hello".
#[test]
fn constructing_a_read_performs_no_effect() {
    let (system, io) = MockHost::builder().stdin_lines(["hello"]).build();
    run_text(
        r#"
        use /std/{Str, Io};
        let discarded: Io(Io/Chunk) = Io/Read/read(Io/stdin, 100);
        let r = Io/read(Io/stdin, 100)!;
        match r : (_) => /std/Io({})
        | chunk(bytes, @_) => Io/write(Io/stdout, bytes)
        | eof() => Io/write(Io/stdout, Str/to_bytes("<eof>"))
        | error(_) => Io/write(Io/stdout, Str/to_bytes("<err>"))
        end
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"hello\n");
}

// A child still waiting when its scope's body answers is cancelled where it waits, and the bracket it entered is released: the child raises a signal from inside its bracket and sleeps far past the test, the body waits for the signal, writes "root;" and answers, and the child's release runs before the scope does.
#[test]
fn a_scope_cancels_a_child_still_waiting_when_its_body_answers() {
    let (system, io) = ticking();
    run_text(
        r#"
        use /std/{Str, Io};
        use /std/Io/{Signal};
        use /std/time/{Duration};
        let child(started: Signal) -> Io({}) =
            let _ = Signal/notify(started)!;
            let _ = Io/sleep(Duration/of_secs(1000))!;
            Io/write(Io/stdout, Str/to_bytes("child;"));
        let body(s: Io/Scope) -> Io({}) =
            let started = Signal/new()!;
            let _ = Io/spawn(s, Io/using((_) => /std/print("released;"), child(started)))!;
            let _ = Signal/wait(started)!;
            Io/write(Io/stdout, Str/to_bytes("root;"));
        let _ = Io/scope(body)!;
        Io/write(Io/stdout, Str/to_bytes("ended"))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"root;released;ended");
}

// `join_all` holds the body until every child it spawned has ended, so nothing is left for the scope to cancel: both sleepers write, in deadline order, before the scope answers.
#[test]
fn join_all_holds_the_body_until_every_child_has_ended() {
    let (system, io) = ticking();
    run_text(
        r#"
        use /std/{Str, Nat, Io};
        use /std/time/{Duration};
        let child(seconds: Nat, mark: Str) -> Io({}) =
            let _ = Io/sleep(Duration/of_secs(seconds))!;
            Io/write(Io/stdout, Str/to_bytes(mark));
        let body(s: Io/Scope) -> Io({}) =
            let _ = Io/spawn(s, child(5, "late;"))!;
            let _ = Io/spawn(s, child(2, "early;"))!;
            Io/join_all(s);
        let _ = Io/scope(body)!;
        Io/write(Io/stdout, Str/to_bytes("done"))
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"early;late;done");
}

// The success path: a bracket's release runs at its exit, after the body.
#[test]
fn a_brackets_release_runs_when_its_body_completes() {
    assert_eq!(
        run(r#"
        use /std/{Str, Io};
        let program: Io({}) =
            Io/using((_) => /std/print("closed;"), Io/write(Io/stdout, Str/to_bytes("body;")));
        program
        "#),
        b"body;closed;"
    );
}

// A bracket's release runs once, at the exit: the program goes on after it, and nothing runs the release again.
#[test]
fn a_brackets_release_runs_once() {
    assert_eq!(
        run(r#"
        use /std/{Str, Io};
        let program: Io({}) =
            let _ = Io/using((_) => /std/print("closed;"), Io/write(Io/stdout, Str/to_bytes("body;")))!;
            Io/write(Io/stdout, Str/to_bytes("after;"));
        program
        "#),
        b"body;closed;after;"
    );
}

#[test]
fn heterogeneous_existential_task_list_through_a_generic_map() {
    // An `List` of existential-boxed tasks of DIFFERENT result types, mapped by a generic HOF whose body does an indirect closure call on a continuation pulled out of the box. The arity-1 closure definition is inlined away by the specializer, leaving the `call_indirect` with no surviving definition — the codegen path that needs the call-site arity among `clsr_arities`.
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

// A sleep is a wait on a timer of its own: the program suspends on it, and the host's wait ends once the scripted clock passes the deadline.
#[test]
fn sleep_waits_until_the_clock_passes_the_deadline() {
    let (system, io) = ticking();
    run_text(
        r#"
        use /std/{Str, Io};
        use /std/time/{Duration};
        let program: Io({}) =
            let _ = Io/sleep(Duration/of_secs(5))!;
            Io/write(Io/stdout, Str/to_bytes("woke;"));
        program
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"woke;");
}

// Two branches sleep three and six seconds side by side: each wakes when its own deadline passes, in deadline order, and `both` answers once both have.
#[test]
fn both_wakes_its_sleepers_in_deadline_order() {
    let (system, io) = ticking();
    run_text(
        r#"
        use /std/{Str, Io};
        use /std/time/{Duration};
        let mark(m: Str) -> Io({}) =
            Io/write(Io/stdout, Str/to_bytes(m));
        let program: Io({}) =
            let _ = Io/both(
                Io/bind(Io/sleep(Duration/of_secs(6)), (_) => mark("b;")),
                Io/bind(Io/sleep(Duration/of_secs(3)), (_) => mark("a;")))!;
            mark("done");
        program
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"a;b;done");
}

// The body completes when first advanced, so the timeout answers it through `some`, and the deadline's timer, started beside it, is cancelled.
#[test]
fn timeout_answers_some_when_the_body_finishes_first() {
    let (system, io) = ticking();
    run_text(
        r#"
        use /std/{Str, Nat, Io};
        use /std/time/{Duration};
        let program: Io({}) =
            let r = Io/timeout(Duration/of_secs(5), Io/pure(42))!;
            match r : (_) => Io({})
            | some(v) => Io/write(Io/stdout, Str/to_bytes(Nat/to_str(v)))
            | none() => Io/write(Io/stdout, Str/to_bytes("none"))
            end;
        program
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"42");
}

// The deadline passes first: the two-second timer wakes and wins, and the fifty-second body, which holds a bracket, is cancelled while it sleeps — its release runs before the timeout answers `none`.
#[test]
fn timeout_cancels_the_body_and_runs_its_release_before_answering_none() {
    let (system, io) = ticking();
    run_text(
        r#"
        use /std/{Str, Nat, Io};
        use /std/time/{Duration};
        let body: Io(Nat) =
            let _ = Io/sleep(Duration/of_secs(50))!;
            let _ = Io/write(Io/stdout, Str/to_bytes("body;"))!;
            Io/pure(0);
        let program: Io({}) =
            let r = Io/timeout(Duration/of_secs(2), Io/using((_) => /std/print("released;"), body))!;
            match r : (_) => Io({})
            | some(v) => Io/write(Io/stdout, Str/to_bytes("some"))
            | none() => Io/write(Io/stdout, Str/to_bytes("none;"))
            end;
        program
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"released;none;");
}

// Both branches sleep: the two-second branch wakes, writes and wins with 1, and the sixty-second loser is cancelled where it sleeps, its release running before the race answers and its body never.
#[test]
fn race_of_two_sleeps_answers_the_earlier_and_cancels_the_later() {
    let (system, io) = ticking();
    run_text(
        r#"
        use /std/{Str, Nat, Io};
        use /std/time/{Duration};
        let quick: Io(Nat) =
            let _ = Io/sleep(Duration/of_secs(2))!;
            let _ = Io/write(Io/stdout, Str/to_bytes("quick;"))!;
            Io/pure(1);
        let slow: Io(Nat) =
            let _ = Io/sleep(Duration/of_secs(60))!;
            let _ = Io/write(Io/stdout, Str/to_bytes("slow;"))!;
            Io/pure(2);
        let program: Io({}) =
            let v = Io/race([quick, Io/using((_) => /std/print("released;"), slow)])!;
            Io/write(Io/stdout, Str/to_bytes(Nat/to_str(v)));
        program
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"quick;released;1");
}

// A selection over no offers waits on nothing, so at the top of a program nothing can ever end it: the host reports the deadlock rather than waiting for good.
#[test]
fn a_selection_over_no_offers_at_the_top_is_a_deadlock() {
    let (system, io) = MockHost::builder().build();
    let _ = run_text(
        r#"
        use /std/{Nat, Io};
        let stuck: Io(Nat) =
            let w = Io/select(@Nat, [])!;
            Io/pure(w.1);
        let _ = stuck!;
        /std/print("finished")
        "#,
        system,
    );
    assert_eq!(io.errors(), curios_abi::DEADLOCK.as_bytes());
}

// A signal wakes its waiter once notified, and a notify that lands before anyone waits is kept, so the second wait answers at once rather than waiting on a flag already raised.
#[test]
fn a_signal_wakes_its_waiter_once_notified_and_keeps_an_early_notify() {
    let (system, io) = ticking();
    run_text(
        r#"
        use /std/{Str, Io};
        use /std/Io/{Signal};
        use /std/time/{Duration};
        let notifier(s: Signal) -> Io({}) =
            let _ = Io/sleep(Duration/of_secs(1))!;
            let _ = /std/print("notify ")!;
            Signal/notify(s);
        let body(scope: Io/Scope) -> Io({}) =
            let s = Signal/new()!;
            let _ = Io/spawn(scope, notifier(s))!;
            let _ = /std/print("wait ")!;
            let _ = Signal/wait(s)!;
            let _ = /std/print("woken ")!;
            let _ = Signal/notify(s)!;
            let _ = Signal/wait(s)!;
            /std/print("again");
        Io/scope(body)
        "#,
        system,
    )
    .expect("expected result");
    assert_eq!(io.output(), b"wait notify woken again");
}

// A selection takes nothing out of a source that does not win it. Both channels hold a value and both offers would answer, so the losing offer is what proves the discipline: a selection that started both and cancelled one would have taken from both.
#[test]
fn a_selection_keeps_the_value_of_the_offer_it_did_not_take() {
    assert_eq!(
        run(r#"
        use /std/{Str, Nat, Option, Io};
        use /std/Io/{Channel};
        let shown(o: Option(Nat)) -> Str =
            match o | some(n) => Nat/to_str(n) | none() => "none" end;
        let left(t: Channel/Take(Nat)) -> Str =
            match t | item(n) => Nat/to_str(n) | empty() => "empty" | ended() => "ended" end;
        let program: Io({}) =
            let a = Channel/new(@Nat, 4)!;
            let b = Channel/new(@Nat, 4)!;
            let _ = Channel/send(a.0, 10)!;
            let _ = Channel/send(b.0, 20)!;
            let w = Io/select([Channel/recv_offer(a.1), Channel/recv_offer(b.1)])!;
            let rest_a = Channel/try_recv(a.1)!;
            let rest_b = Channel/try_recv(b.1)!;
            /std/print(
                Str/flatten(
                    [Nat/to_str(w.0), " ", shown(w.1), " ", left(rest_a), " ", left(rest_b)]));
        program
        "#),
        b"0 10 empty 20"
    );
}

// Enqueueing is complete before receipt. A reply cell makes the acknowledgement explicit, and joining the feeder keeps the body alive until it observes that fill. The body runs on to its receive before the child it spawned starts, so the receiving comes first.
#[test]
fn a_sender_waits_for_acknowledgement_after_its_message_is_taken() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Cell, Option, Io, print};
        use /std/Io/{Channel};
        let feeder(s: Channel/Sender({Nat, Cell({})})) -> Io({}) =
            let ack = Cell/new()!;
            let _ = Channel/send(s, (7, ack))!;
            let _ = print("queued ")!;
            let _ = Io/wait_filled([ack])!;
            print("acknowledged");
        let body(scope: Io/Scope) -> Io({}) =
            let c = Channel/new(@{Nat, Cell({})}, 1)!;
            let task = Io/spawn(scope, feeder(c.0))!;
            let _ = print("receiving ")!;
            let got = Channel/recv(c.1)!;
            let _ = match got
                | some((n, ack)) =>
                    let _ = print(Nat/to_str(n))!;
                    let _ = print(" ")!;
                    let _ = Cell/fill(ack, ())!;
                    Io/pure(())
                | none() => Io/pure(())
                end!;
            Io/join(task);
        Io/scope(body)
        "#),
        b"receiving queued 7 acknowledged"
    );
}

// Closing hands back what is already queued before it ends the stream, so a reader drains rather than losing the tail. Beside it, each attempt that does not wait answers its own outcome: a send at capacity is `full` and one after the close `closed`, and a take from an open channel with nothing queued is `empty` where one from a closed and drained channel is `ended`.
#[test]
fn a_closed_channel_drains_before_it_ends() {
    assert_eq!(
        run(r#"
        use /std/{Str, Nat, Bool, Option, Io};
        use /std/Io/{Channel};
        let shown(o: Option(Nat)) -> Str =
            match o | some(n) => Nat/to_str(n) | none() => "end" end;
        let said(b: Bool) -> Str =
            match b | true => "true" | false => "false" end;
        let pushed(p: Channel/Push) -> Str =
            match p | taken() => "taken" | full() => "full" | closed() => "closed" end;
        let left(t: Channel/Take(Nat)) -> Str =
            match t | item(n) => Nat/to_str(n) | empty() => "empty" | ended() => "ended" end;
        let program: Io({}) =
            let c = Channel/new(@Nat, 2)!;
            let nothing_yet = Channel/try_recv(c.1)!;
            let _ = Channel/send(c.0, 1)!;
            let _ = Channel/send(c.0, 2)!;
            let full = Channel/try_send(c.0, 3)!;
            let _ = Channel/close(c.0)!;
            let refused = Channel/try_send(c.0, 3)!;
            let first = Channel/recv(c.1)!;
            let second = Channel/recv(c.1)!;
            let ended = Channel/recv(c.1)!;
            let drained = Channel/try_recv(c.1)!;
            let after = Channel/send(c.0, 4)!;
            /std/print(
                Str/flatten(
                    [
                        left(nothing_yet),
                        " ",
                        pushed(full),
                        " ",
                        pushed(refused),
                        " ",
                        shown(first),
                        " ",
                        shown(second),
                        " ",
                        shown(ended),
                        " ",
                        left(drained),
                        " ",
                        said(after)]));
        program
        "#),
        b"empty full closed 1 2 end ended false"
    );
}

// The channel a program reaches is the one behind its two ends: the operations `/sys` declares over the bare channel are no part of `/std/Io/Channel`, so no program holds a channel whose changes wake nothing.
#[test]
fn the_bare_channel_operations_are_not_reachable() {
    let (system, _io) = MockHost::builder().build();
    let refusal = run_text(
        r#"
        use /std/{Nat, Io};
        use /std/Io/{Channel};
        let c = Channel/Channel/new(@Nat, 1)!;
        Io/pure(())
        "#,
        system,
    )
    .expect_err("the bare channel has no public constructor");
    assert!(refusal.contains("Channel"), "{refusal}");
}
