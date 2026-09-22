//! What erasure keeps computes the value the checker proved: each value this program prints is one a proof in the same program states, with one row per place erasure deletes something beside the data it keeps.

use crate::tests::run;

/// Every row states its value twice — as a proof both checkers must accept, and as the printed result of the same expression — so a row fails exactly when the compiled program disagrees with a theorem the compiler certified.
///
/// Erasure is the one stage below Core with no semantic check, so this is the ledger for it: a deletion site that drops the wrong thing leaves the proof standing and changes the print. The first three rows are pinned payloads of a `Prop` family, whose constructor is itself deleted, so the value survives only as the scrutinee's index; binding those payloads to the unit constant printed `1`, `1` and `0` while the checkers proved `42`, `42` and `21`. The remaining rows are the other deletion sites `documentation/design/language/totality-of-the-erased-program.md` names: an erased parameter, an erased field beside a kept one, and a proof-valued callee applied and dropped.
#[test]
fn every_printed_value_is_the_one_its_proof_states() {
    let source = r#"
        use /std/{Eq, Nat, Str, Io, print, WellFounded};
        use /std/Nat/{Le};
        use /std/WellFounded/{Accessible};

        -- `refl(@z) : (z, z)` pins its payload twice; the payload is the index.
        let z_of(@a: Nat, @b: Nat, e: Eq(a, b)) -> Nat = match e | refl(@z) => z + 1 end;

        -- A pinned payload beside a proof payload: `below` erases, `w` is the index.
        let point(@x: Nat, acc: Accessible((a: Nat, b: Nat) => Nat/Lt(a, b), x)) -> Nat =
            match acc | intro(@w, _below) => w + 1 end;

        -- Targets that swap their binders: each payload is read from its own position.
        induct Swap: (Nat, Nat) -> pub Prop
        | mk(@x: Nat, @y: Nat): (y, x)
        end
        let digits(@a: Nat, @b: Nat, s: Swap(a, b)) -> Nat = match s | mk(@x, @y) => x * 10 + y end;

        -- A type and a proof parameter, erased beside the data they do not affect.
        let pick(T: Type, p: Eq(1, 1), n: Nat) -> Nat = n + 1;

        -- An erased field beside a kept one.
        struct Bounded: pub Type { n: Nat, ok: Nat/Lt(n, 100) }

        -- A proof-valued callee, applied and dropped.
        let lemma(n: Nat) -> Nat/Le(n, n) = Le/refl(n);
        let keep(n: Nat, _p: Nat/Le(n, n)) -> Nat = n + 1;

        let _refl_payload: Eq(z_of(Eq/refl(@Nat, @41)), 42) = Eq/refl();
        let _accessible_payload: Eq(point(WellFounded/lt(41)), 42) = Eq/refl();
        let _swapped_payloads: Eq(digits(Swap/mk(@2, @1)), 21) = Eq/refl();
        let _erased_parameters: Eq(pick(Nat, Eq/refl(), 41), 42) = Eq/refl();
        let _erased_field: Eq(Bounded { n = 41, ok = /std/Bool/True/qed() }.n + 1, 42) = Eq/refl();
        let _proof_valued_callee: Eq(keep(41, lemma(41)), 42) = Eq/refl();

        let show(n: Nat) -> Io({}) = print(Str/concat(Nat/to_str(n), "\n"));

        let _ = show(z_of(Eq/refl(@Nat, @41)))!;
        let _ = show(point(WellFounded/lt(41)))!;
        let _ = show(digits(Swap/mk(@2, @1)))!;
        let _ = show(pick(Nat, Eq/refl(), 41))!;
        let _ = show(Bounded { n = 41, ok = /std/Bool/True/qed() }.n + 1)!;
        show(keep(41, lemma(41)))
        "#;

    assert_eq!(
        String::from_utf8_lossy(&run(source)),
        "42\n42\n21\n42\n42\n42\n"
    );
}
