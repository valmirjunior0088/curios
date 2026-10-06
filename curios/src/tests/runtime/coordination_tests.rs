//! Guest storage outcomes, erased payloads, and the bounds and privacy behind coordination.

use crate::tests::{run, typecheck};

#[test]
fn a_channel_preserves_fifo_through_wraparound_and_close() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Io, print};
        use /std/Async/{Channel};
        let pushed(p: Channel/Push) -> Io({}) =
            print(match p | taken() => "taken " | full() => "full " | closed() => "closed " end);
        let taken(t: Channel/Take(Nat)) -> Io({}) =
            print(match t | item(n) => Str/concat(Nat/to_str(n), " ") | empty() => "empty " | ended() => "ended " end);
        let c = Channel/new(@Nat, 2)!;
        let _ = taken(Channel/try_recv(c.1)!)!;
        let _ = pushed(Channel/try_send(c.0, 10)!)!;
        let _ = pushed(Channel/try_send(c.0, 20)!)!;
        let _ = pushed(Channel/try_send(c.0, 30)!)!;
        let _ = taken(Channel/try_recv(c.1)!)!;
        let _ = pushed(Channel/try_send(c.0, 30)!)!;
        let _ = Channel/close(c.0)!;
        let _ = Channel/close(c.0)!;
        let _ = pushed(Channel/try_send(c.0, 40)!)!;
        let _ = taken(Channel/try_recv(c.1)!)!;
        let _ = taken(Channel/try_recv(c.1)!)!;
        taken(Channel/try_recv(c.1)!)
    "#),
        b"empty taken taken full 10 taken closed 20 30 ended "
    );
}

#[test]
fn proof_payloads_still_occupy_storage() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Eq, Cell, Option, Io, print};
        use /std/Async/{Channel};
        let proof = Cell/new(@Eq()(1, 1))!;
        let _ = Cell/fill(proof, Eq/refl())!;
        let _ = print(match Cell/poll(proof)! | some(_) => "proof " | none() => "missing " end)!;
        let p = Channel/new(@Eq()(1, 1), 1)!;
        let _ = Channel/try_send(p.0, Eq/refl())!;
        print(match Channel/try_recv(p.1)! | item(_) => "proof" | empty() => "empty" | ended() => "ended" end)
    "#),
        b"proof proof"
    );
}

/// Explicit binds let each intermediate result choose its own universe, including `Cell(Type)` and `Take(Type)`, before the final ground-level print.
#[test]
fn type_payloads_still_occupy_storage() {
    assert_eq!(
        run(r#"
        use /std/{Cell, Nat, Io, print};
        use /std/Async/{Channel};
        Io/bind(Cell/new(@Type), (c) =>
        Io/bind(Cell/fill(c, Nat), (_) =>
        Io/bind(Cell/poll(c), (held) =>
        Io/bind(print(match held | some(_) => "type " | none() => "missing " end), (_) =>
        Io/bind(Channel/new(@Type, 1), (q) =>
        Io/bind(Channel/try_send(q.0, Nat), (_) =>
        Io/bind(Channel/try_recv(q.1), (first) =>
        Io/bind(print(match first | item(_) => "item " | empty() => "empty " | ended() => "ended " end), (_) =>
        Io/bind(Channel/try_recv(q.1), (second) =>
        print(match second | item(_) => "item" | empty() => "empty" | ended() => "ended" end))))))))))
    "#),
        b"type item empty"
    );
}

#[test]
fn a_zero_capacity_is_refused_at_the_public_constructor() {
    let error = typecheck("let c = /std/Async/Channel/new(@/std/Nat, 0)!; /std/Io/pure(())")
        .expect_err("zero has no positive-capacity evidence");
    assert!(error.contains("Holds") || error.contains("Lt"), "{error}");
}

#[test]
fn arbitrary_wait_probes_are_private() {
    let error = typecheck(
        "let w = /std/Async/Wait/probe(/std/Async/Probe { /std/Io/pure(true) }); /std/Io/pure(())",
    )
    .expect_err("a program cannot manufacture a probe");
    assert!(error.contains("private"), "{error}");
}
