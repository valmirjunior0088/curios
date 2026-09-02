//! List and binary spreads: the segments they concatenate, and the operands they hoist.

use crate::tests::{error, run};

#[test]
fn list_spread_concats_segments() {
    // `[1, ..xs, 4]` splices `xs` between the literal runs. The non-commutative foldr probe (see `list_match_is_a_foldr`) distinguishes the spliced order `[1, 2, 3, 4]` from any permutation or grouping artifact.
    let source = r#"
        use /std/{Handle, Str, Nat, List, Io};
        let xs : List(Nat) = [2, 3];
        let ys : List(Nat) = [1, ..xs, 4];
        let digits : Nat =
            match ys : (_) => Nat
            | [] => 0
            | [h, ..t]; ih => Nat/add(Nat/mul(ih, 10), h)
            end;
        let _ = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(digits)))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"4321");
}

#[test]
fn list_spread_identity_and_multi() {
    // `[..xs]` is an identity copy (reduction collapses the lone operand), and spreads repeat: `[..ys, ..ys]` doubles the sequence in written order.
    let source = r#"
        use /std/{Handle, Str, Nat, List, Io};
        let xs : List(Nat) = [2, 3];
        let ys : List(Nat) = [..xs];
        let zs : List(Nat) = [..ys, ..ys];
        let digits : Nat =
            match zs : (_) => Nat
            | [] => 0
            | [h, ..t]; ih => Nat/add(Nat/mul(ih, 10), h)
            end;
        let _ = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(digits)))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"3232");
}

#[test]
fn list_spread_borrows_expected_element_type() {
    // The `ListConcat` bidirectionality case in `elaborate_intrinsic`: checking `[1, ..xs]` against `List(Int)` must solve the lowering-minted element slot from the expected type BEFORE the literal chunk elaborates, so the unsigned `1` lands at `Int`. Without the borrow, `1` would default-solve the slot to `Nat` and this program would be rejected.
    let source = r#"
        use /std/{Handle, Str, Nat, Int, List, Io};
        let xs : List(Int) = [-1, +2];
        let ys : List(Int) = [1, ..xs];
        let _ = Io/write(Io/stdout, Str/to_bytes(Nat/to_str(List/len(ys))))!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"3");
}

#[test]
fn list_spread_of_non_list_is_rejected() {
    // A spread operand must itself be a list of the element type — `..2` in a `List(Nat)` literal is an ordinary type mismatch (Nat vs List(Nat)).
    let source = r#"
        use /std/{Handle, Str, Nat, List, Io};
        let bad : List(Nat) = [1, ..2];
        let _ = Io/write(Io/stdout, Str/to_bytes("unreachable"))!;
        /std/Io/pure(())
        "#;
    error(source);
}

#[test]
fn list_spread_element_type_clash_is_rejected() {
    let source = r#"
        use /std/{Handle, Str, Nat, List, Io};
        let ss : List(Str) = ["a"];
        let bad : List(Nat) = [..ss];
        let _ = Io/write(Io/stdout, Str/to_bytes("unreachable"))!;
        /std/Io/pure(())
        "#;
    error(source);
}

#[test]
fn list_spread_operand_hoists_bangs() {
    // A bang inside a spread operand hoists into the enclosing region exactly like one inside a plain element — the literal is collected, not sealed.
    assert_eq!(
        run(r#"
        use /std/{Async, Handle, Str, Nat, List, Io};
        let prog : Async({}) =
            let ys : List(Nat) = [1, ..Async/pure([2, 3])!, 4];
            let digits : Nat =
                match ys : (_) => Nat
                | [] => 0
                | [h, ..t]; ih => Nat/add(Nat/mul(ih, 10), h)
                end;
            let wrote = Async/lift(Io/write(Io/stdout, Str/to_bytes(Nat/to_str(digits))))!;
            Async/pure(());
        Async/run(prog)
        "#),
        b"4321"
    );
}

#[test]
fn bin_spread_concats_segments() {
    // `x[0x01, ..b, 0x04]` splices the bytes of `b` between the literal runs, and the glued suffix chain admits a call operand (`\..Bytes/slice(...)`).
    let source = r#"
        use /std/{Handle, Nat, Bytes, Io};
        let b : Bytes = x[0x02, 0x03];
        let _ = Io/write(Io/stdout, x[0x01, ..b, 0x04, ..Bytes/slice(b, 1, 1)])!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"\x01\x02\x03\x04\x03");
}

#[test]
fn bin_spread_identity_and_multi() {
    let source = r#"
        use /std/{Handle, Bytes, Io};
        let b : Bytes = x[0x48, 0x65];
        let c : Bytes = x[..b];
        let _ = Io/write(Io/stdout, x[..c, ..c])!;
        /std/Io/pure(())
        "#;
    assert_eq!(run(source), b"HeHe");
}

#[test]
fn bin_spread_of_non_bin_is_rejected() {
    // A spread operand must itself be a `Bytes` — a list is an ordinary type mismatch.
    let source = r#"
        use /std/{Handle, Str, Nat, List, Bytes, Io};
        let xs : List(Nat) = [1, 2];
        let bad : Bytes = x[0x00, ..xs];
        let _ = Io/write(Io/stdout, Str/to_bytes("unreachable"))!;
        /std/Io/pure(())
        "#;
    error(source);
}

#[test]
fn bin_spread_operand_hoists_bangs() {
    // The `Bytes` sibling of `list_spread_operand_hoists_bangs`, through the dedicated `Intrinsic::Bytes` collect arm — the glued `!` binds to the operand.
    assert_eq!(
        run(r#"
        use /std/{Async, Handle, Bytes, Io};
        let prog : Async({}) =
            let out : Bytes = x[0x3e, ..Async/pure(x[0x68, 0x69])!, 0x3c];
            let wrote = Async/lift(Io/write(Io/stdout, out))!;
            Async/pure(());
        Async/run(prog)
        "#),
        b">hi<"
    );
}
