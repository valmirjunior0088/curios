//! Guest storage outcomes, erased payloads, and the bounds and privacy behind coordination.

use crate::tests::{run, typecheck};

#[test]
fn a_channel_preserves_fifo_through_wraparound_and_close() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Io, print};
        use /std/Async/Channel/{Channel};
        let pushed(p: Channel/Push) -> Io({}) =
            print(match p | taken() => "taken " | full() => "full " | closed() => "closed " end);
        let taken(t: Channel/Take(Nat)) -> Io({}) =
            print(match t | item(n) => Str/concat(Nat/to_str(n), " ") | empty() => "empty " | ended() => "ended " end);
        let c = Channel/new(@Nat, 2)!;
        let _ = taken(Channel/take(c)!)!;
        let _ = pushed(Channel/push(c, 10)!)!;
        let _ = pushed(Channel/push(c, 20)!)!;
        let _ = pushed(Channel/push(c, 30)!)!;
        let _ = taken(Channel/take(c)!)!;
        let _ = pushed(Channel/push(c, 30)!)!;
        let _ = print(Nat/to_str(Channel/count(c)!))!;
        let _ = print("/")!;
        let _ = print(Nat/to_str(Channel/capacity(c)!))!;
        let _ = print(" ")!;
        let _ = Channel/close(c)!;
        let _ = Channel/close(c)!;
        let _ = pushed(Channel/push(c, 40)!)!;
        let _ = taken(Channel/take(c)!)!;
        let _ = taken(Channel/take(c)!)!;
        let _ = taken(Channel/take(c)!)!;
        let _ = print(Nat/to_str(Channel/count(c)!))!;
        print(match Channel/closed(c)! | true => " closed" | false => " open" end)
    "#),
        b"empty taken taken full 10 taken 2/2 closed 20 30 ended 0 closed"
    );
}

#[test]
fn proof_payloads_still_occupy_storage() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Eq, Cell, Option, Io, print};
        use /std/Async/Channel/{Channel};
        let proof = Cell/new(@Eq(1, 1))!;
        let _ = Cell/fill(proof, Eq/refl())!;
        let _ = print(match Cell/poll(proof)! | some(_) => "proof " | none() => "missing " end)!;
        let p = Channel/new(@Eq(1, 1), 1)!;
        let _ = Channel/push(p, Eq/refl())!;
        print(match Channel/take(p)! | item(_) => "proof" | empty() => "empty" | ended() => "ended" end)
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
        use /std/Async/Channel/{Channel};
        Io/bind(Cell/new(@Type), (c) =>
        Io/bind(Cell/fill(c, Nat), (_) =>
        Io/bind(Cell/poll(c), (held) =>
        Io/bind(print(match held | some(_) => "type " | none() => "missing " end), (_) =>
        Io/bind(Channel/new(@Type, 1), (q) =>
        Io/bind(Channel/push(q, Nat), (_) =>
        Io/bind(Channel/take(q), (first) =>
        Io/bind(print(match first | item(_) => "item " | empty() => "empty " | ended() => "ended " end), (_) =>
        Io/bind(Channel/take(q), (second) =>
        print(match second | item(_) => "item" | empty() => "empty" | ended() => "ended" end))))))))))
    "#),
        b"type item empty"
    );
}

#[test]
fn a_zero_capacity_is_refused_at_both_public_constructors() {
    for source in [
        "let c = /std/Async/Channel/new(@/std/Nat, 0)!; /std/Io/pure(())",
        "use /std/Async/Channel/{Channel}; let c = Channel/new(@/std/Nat, 0)!; /std/Io/pure(())",
    ] {
        let error = typecheck(source).expect_err("zero has no positive-capacity evidence");
        assert!(error.contains("Holds") || error.contains("Lt"), "{error}");
    }
}

#[test]
fn arbitrary_wait_probes_are_private() {
    for name in ["/std/Async/Waiting/probe", "/std/Async/Wait/probe"] {
        let source = format!("let w = {name}(/std/Io/pure(true)); /std/Io/pure(())");
        let error = typecheck(&source).expect_err("a program cannot manufacture a probe");
        assert!(
            error.contains("private") || error.contains("not found") || error.contains("unbound"),
            "{error}"
        );
    }
}
