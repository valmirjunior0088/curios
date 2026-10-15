//! Waiting on the program's own state: a fulfilment woken once whatever waits on it twice, receivers woken together where one can take, an offer that loses a selection, coalescing signals, and a waiter cancelled where it waits.

use crate::tests::run;

// One fulfilment wakes a waiter once, however many of its offers wait on the same future.
#[test]
fn a_fulfilment_wakes_a_waiter_once_across_duplicate_offers() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Io, print};
        use /std/Io/{Future};
        let body(s: Io/Scope) -> Io({}) =
            let f = Future/new(@Nat)!;
            let waiter() -> Io(Nat) =
                let w = Io/select([Future/offer(f), Future/offer(f)])!;
                let _ = print("once ")!;
                Io/pure(w.1);
            let task = Io/spawn(s, waiter())!;
            let _ = Io/yield()!;
            let _ = Future/fulfill(f, 7)!;
            let n = Io/join(task)!;
            print(Nat/to_str(n));
        Io/scope(body)
    "#),
        b"once 7"
    );
}

// A send wakes every receiver waiting on the channel, and the one that does not take waits again: the second send reaches it.
#[test]
fn two_woken_receivers_retry_after_only_one_can_take() {
    assert_eq!(
        run(r#"
        use /std/{Cell, Nat, Option, print, Io};
        use /std/Io/{Channel};
        let body(s: Io/Scope) -> Io({}) =
            let c = Channel/new(@Nat, 1)!;
            let first = Cell/new(@{})!;
            let receive() -> Io(Nat) =
                let got = Channel/recv(c.1)!;
                let _ = Cell/fill(first, ())!;
                Io/pure(Option/unwrap_or(got, 0));
            let a = Io/spawn(s, receive())!;
            let b = Io/spawn(s, receive())!;
            let _ = Io/yield()!;
            let _ = Channel/send(c.0, 1)!;
            let _ = Io/wait_filled([first])!;
            let _ = Channel/send(c.0, 2)!;
            let x = Io/join(a)!;
            let y = Io/join(b)!;
            let _ = print(Nat/to_str(x))!;
            let _ = print(" ")!;
            print(Nat/to_str(y));
        Io/scope(body)
    "#),
        b"1 2"
    );
}

// An offer that loses a selection has taken nothing and waits on nothing after it: a message sent once the selection has answered stays in its channel, unacknowledged, for the next receive.
#[test]
fn a_losing_offer_neither_takes_a_later_message_nor_acknowledges_it() {
    assert_eq!(
        run(r#"
        use /std/{Cell, Nat, Option, print, Io};
        use /std/Io/{Channel, Offer};
        let body(s: Io/Scope) -> Io({}) =
            let messages = Channel/new(@{Nat, Cell({})}, 1)!;
            let other = Channel/new(@Nat, 1)!;
            let select_one() -> Io({}) =
                let _ = Io/select([
                    Offer/map(Channel/recv_offer(messages.1), (_) => ()),
                    Offer/map(Channel/recv_offer(other.1), (_) => ())])!;
                print("selected ");
            let selecting = Io/spawn(s, select_one())!;
            let _ = Io/yield()!;
            let _ = Channel/send(other.0, 1)!;
            let _ = Io/join(selecting)!;
            let ack = Cell/new(@{})!;
            let _ = Channel/send(messages.0, (7, ack))!;
            let _ = Io/yield()!;
            let _ = print(match Cell/poll(ack)! | none() => "unacknowledged " | some(_) => "acknowledged " end)!;
            let got = Channel/recv(messages.1)!;
            match got
            | none() => print("lost")
            | some((n, reply)) =>
                let _ = Cell/fill(reply, ())!;
                print(Nat/to_str(n))
            end;
        Io/scope(body)
    "#),
        b"selected unacknowledged 7"
    );
}

#[test]
fn signals_coalesce_and_futures_preserve_their_first_answer() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Option, print, Io};
        use /std/Io/{Signal, Future};
        let program: Io({}) =
            let s = Signal/new()!;
            let _ = Signal/notify(s)!;
            let _ = Signal/notify(s)!;
            let _ = Signal/wait(s)!;
            let again = Signal/offer(s).try!;
            let _ = print(match again | none() => "coalesced " | some(_) => "extra " end)!;
            let f = Future/new(@Nat)!;
            let _ = Future/fulfill(f, 7)!;
            let _ = Future/fulfill(f, 99)!;
            print(Nat/to_str(Future/await(f)!));
        program
    "#),
        b"coalesced 7"
    );
}

// A branch waiting on a cell is cancelled where it waits once another branch wins, and its bracket is released then, once: the cell filled afterwards wakes nothing.
#[test]
fn a_waiter_cancelled_on_a_cell_is_released_once() {
    assert_eq!(
        run(r#"
        use /std/{Cell, print, Io};
        let program: Io({}) =
            let c = Cell/new(@{})!;
            let _ = Io/race([Io/using((_) => print("released "), Io/wait_filled([c])), Io/pure(())])!;
            let _ = Cell/fill(c, ())!;
            let _ = Io/wait_filled([c])!;
            print("done");
        program
    "#),
        b"released done"
    );
}
