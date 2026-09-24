//! Level observations, shared park claims, stale readiness, and abandoned selection registrations.

use crate::tests::run;

#[test]
fn a_lifted_fill_wakes_a_job_once_across_duplicate_waits() {
    assert_eq!(
        run(r#"
        use /std/{Cell, Nat, Option, Io, Async, print};
        let fiber: Async({}) =
            let c = Cell/new(@Nat)!;
            let waiter() -> Async(Nat) =
                let _ = Async/park([Async/Wait/filled(c), Async/Wait/filled(c)])!;
                let _ = print("once ")!;
                Async/pure(Option/unwrap_or(Cell/poll(c)!, 0));
            let task = Async/spawn(waiter())!;
            let _ = Async/lift(Cell/fill(c, 7))!;
            let n = Async/join(task)!;
            let _ = Async/yield_now!;
            print(Nat/to_str(n));
        Async/run(fiber)
    "#),
        b"once 7"
    );
}

#[test]
fn two_ready_receivers_retry_after_only_one_can_take() {
    assert_eq!(
        run(r#"
        use /std/{Cell, Nat, Option, Async, print};
        use /std/Async/{Channel};
        let fiber: Async({}) =
            let c = Channel/new(@Nat, 1)!;
            let first = Cell/new(@{})!;
            let receive() -> Async(Nat) =
                let got = Channel/recv(c.1)!;
                let _ = Cell/fill(first, ())!;
                Async/pure(Option/unwrap_or(got, 0));
            let a = Async/spawn(receive())!;
            let b = Async/spawn(receive())!;
            let _ = Channel/send(c.0, 1)!;
            let _ = Async/park([Async/Wait/filled(first)])!;
            let _ = Channel/send(c.0, 2)!;
            let x = Async/join(a)!;
            let y = Async/join(b)!;
            let _ = print(Nat/to_str(x))!;
            let _ = print(" ")!;
            print(Nat/to_str(y));
        Async/run(fiber)
    "#),
        b"1 2"
    );
}

#[test]
fn a_losing_registration_neither_takes_a_later_message_nor_acknowledges_it() {
    assert_eq!(
        run(r#"
        use /std/{Cell, Nat, Option, Async, print};
        use /std/Async/{Channel, Offer};
        let fiber: Async({}) =
            let messages = Channel/new(@{Nat, Cell({})}, 1)!;
            let other = Channel/new(@Nat, 1)!;
            let select_one() -> Async({}) =
                let _ = Async/select([
                    Offer/map(Channel/recv_offer(messages.1), (_) => ()),
                    Offer/map(Channel/recv_offer(other.1), (_) => ())])!;
                print("selected ");
            let selecting = Async/spawn(select_one())!;
            let _ = Channel/send(other.0, 1)!;
            let _ = Async/join(selecting)!;
            let ack = Cell/new(@{})!;
            let _ = Channel/send(messages.0, (7, ack))!;
            let _ = Async/yield_now!;
            let _ = print(match Cell/poll(ack)! | none() => "unacknowledged " | some(_) => "acknowledged " end)!;
            let got = Channel/recv(messages.1)!;
            match got
            | none() => print("lost")
            | some((n, reply)) =>
                let _ = Cell/fill(reply, ())!;
                print(Nat/to_str(n))
            end;
        Async/run(fiber)
    "#),
        b"selected unacknowledged 7"
    );
}

#[test]
fn signals_coalesce_and_futures_preserve_their_first_answer() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Option, Async, print};
        use /std/Async/{Signal, Future};
        let fiber: Async({}) =
            let s = Signal/new()!;
            let _ = Signal/notify(s)!;
            let _ = Signal/notify(s)!;
            let _ = Signal/wait(s)!;
            let again = Async/lift(Signal/offer(s).try)!;
            let _ = print(match again | none() => "coalesced " | some(_) => "extra " end)!;
            let f = Future/new(@Nat)!;
            let _ = Future/fulfill(f, 7)!;
            let _ = Future/fulfill(f, 99)!;
            print(Nat/to_str(Async/await(f)!));
        Async/run(fiber)
    "#),
        b"coalesced 7"
    );
}

#[test]
fn cancelling_a_probe_waiter_releases_its_guard_once() {
    assert_eq!(
        run(r#"
        use /std/{Cell, Async, print};
        let fiber: Async({}) =
            let c = Cell/new(@{})!;
            let task = Async/spawn(Async/using(print("released "), Async/park([Async/Wait/filled(c)])))!;
            let _ = Async/cancel(task)!;
            let _ = Cell/fill(c, ())!;
            let _ = Async/park([Async/Wait/filled(c)])!;
            print("done");
        Async/run(fiber)
    "#),
        b"released done"
    );
}
