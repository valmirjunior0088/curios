//! Guest storage outcomes, erased payloads, and the bounds and privacy behind coordination.

use crate::test_support::*;

#[test]
fn a_channel_preserves_fifo_across_takes_and_a_finish() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Str, Io, print, Channel};
        let pushed(p: Channel/Push) -> Io({}) =>
            print(match p | taken() => "taken " | full() => "full " | closed() => "closed " end);
        let taken(t: Channel/Take(Nat)) -> Io({}) =>
            print(match t | item(n) => Str/concat(Nat/to_str(n), " ") | empty() => "empty " | ended() => "ended " end);
        let entry() -> /std/Io({}) =>
            let c = Channel/new(@Nat, 2)!;
            let _ = taken(Channel/try_take(c)!)!;
            let _ = pushed(Channel/try_push(c, 10)!)!;
            let _ = pushed(Channel/try_push(c, 20)!)!;
            let _ = pushed(Channel/try_push(c, 30)!)!;
            let _ = taken(Channel/try_take(c)!)!;
            let _ = pushed(Channel/try_push(c, 30)!)!;
            let _ = Channel/finish(c)!;
            let _ = Channel/finish(c)!;
            let _ = pushed(Channel/try_push(c, 40)!)!;
            let _ = taken(Channel/try_take(c)!)!;
            let _ = taken(Channel/try_take(c)!)!;
            taken(Channel/try_take(c)!);
        entry()
    "#),
        b"empty taken taken full 10 taken closed 20 30 ended "
    );
}

#[test]
fn evidence_payloads_still_occupy_storage() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Eq, Option, Box, Io, print, Channel};
        let entry() -> /std/Io({}) =>
            let proof = Box/new(@Option(Eq()(1, 1)), Option/some(Eq/refl()))!;
            let _ = print(match Box/get(proof)! | some(_) => "proof " | none() => "missing " end)!;
            let p = Channel/new(@Option(Eq()(1, 1)), 1)!;
            let _ = Channel/try_push(p, Option/some(Eq/refl()))!;
            print(match Channel/try_take(p)! | item(_) => "proof" | empty() => "empty" | ended() => "ended" end);
        entry()
    "#),
        b"proof proof"
    );
}

/// Explicit binds let each intermediate result choose its own universe, including `Box(Type)` and `Take(Type)`, before the final ground-level print.
#[test]
fn type_payloads_still_occupy_storage() {
    assert_eq!(
        run(r#"
        use /std/{Box, Nat, Io, print, Channel};
        Io/bind(Box/new(@Type, Nat), (b) =>
        Io/bind(Box/get(b), (_) =>
        Io/bind(print("type "), (_) =>
        Io/bind(Channel/new(@Type, 1), (q) =>
        Io/bind(Channel/try_push(q, Nat), (_) =>
        Io/bind(Channel/try_take(q), (first) =>
        Io/bind(print(match first | item(_) => "item " | empty() => "empty " | ended() => "ended " end), (_) =>
        Io/bind(Channel/try_take(q), (second) =>
        print(match second | item(_) => "item" | empty() => "empty" | ended() => "ended" end)))))))))
    "#),
        b"type item empty"
    );
}

#[test]
fn a_zero_capacity_is_refused_at_the_public_constructor() {
    let error = typecheck("let main() -> /std/Io({}) => let c = /std/Channel/new(@/std/Nat, 0)!; /std/Io/pure(()); main()")
        .expect_err("zero has no positive-capacity evidence");
    assert!(error.contains("Holds") || error.contains("Lt"), "{error}");
}

// Only `/std` advances a description: the driver every combinator is written over holds each branch's rest and performs each dropped branch's cancel, and a program reaches it through the combinators alone.
#[test]
fn the_driver_of_branches_is_private() {
    let error = typecheck("let d = /std/Io/drive; /std/Io/pure(())")
        .expect_err("a program cannot reach the driver");
    assert!(error.contains("private"), "{error}");
}

// The pair a description suspends and is advanced by is `/sys`'s, which `/std` names and `Io` does not re-export: a program waits through the waits, the brackets and the combinators, which perform a rest once and a dropped rest's cancel.
#[test]
fn a_program_names_neither_suspend_nor_advance() {
    for name in [
        "/std/Io/suspend",
        "/std/Io/advance",
        "/sys/Io/suspend",
        "/sys/Io/advance",
    ] {
        typecheck(&format!("let d = {name}; /std/Io/pure(())"))
            .expect_err("a program cannot name the pair");
    }
}
