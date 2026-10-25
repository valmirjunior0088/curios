//! What erasure keeps computes the value the checker proved: each value this program prints is one a proof in the same program states, with one row per place erasure deletes something beside the data it keeps.

use crate::tests::run;

/// Every row states its value twice — as a proof both checkers must accept, and as the printed result of the same expression — so a row fails exactly when the compiled program disagrees with a theorem the compiler certified.
///
/// Erasure is the one stage below Core with no semantic check, so this is the ledger for it: a deletion site that drops the wrong thing leaves the proof standing and changes the print. The first six rows are pinned payloads of a `Prop` family, whose constructor is itself deleted, so the value survives only as the scrutinee's index; binding the first two to the unit constant would print `1` and `0` where the checkers prove `42` and `21`. The remaining rows are the other deletion sites `documentation/design/soundness/totality-of-the-erased-program.md` names: an erased parameter, an erased field beside a kept one, and a proof-valued callee applied and dropped.
#[test]
fn every_printed_value_is_the_one_its_proof_states() {
    let source = r#"
        use /std/{Eq, Nat, Str, Io, print};
        use /std/Nat/{le};
        use /std/Bool/{Holds};

        -- `refl(@z) : (z, z)` pins its payload twice; the payload is the index.
        let z_of(@a: Nat, @b: Nat, e: Eq()(a, b)) -> Nat = match e | refl(@z) => z + 1 end;

        -- Targets that swap their binders: each payload is read from its own position.
        induct Swap: (Nat, Nat) -> pub Prop
        | mk(@x: Nat, @y: Nat): (y, x)
        end
        let digits(@a: Nat, @b: Nat, s: Swap(a, b)) -> Nat = match s | mk(@x, @y) => x * 10 + y end;

        -- A payload pinned by a later index alone is read from the position that pins it.
        induct Later: (Nat, Nat) -> pub Prop
        | mk(@z: Nat): (z + 1, z)
        end
        let tens(@a: Nat, @b: Nat, l: Later(a, b)) -> Nat = match l | mk(@z) => z * 10 end;

        -- A pinned payload that is a tuple, one that is a function, and one stated by an index the algebra computes.
        let pair(@t: {Nat, Nat}, e: Eq()(t, (3, 4))) -> Nat = match e | refl(@z) => z.0 * 100 + z.1 end;
        let apply(@f: (Nat) -> Nat, @g: (Nat) -> Nat, e: Eq()(f, g)) -> Nat = match e | refl(@h) => h(5) end;
        let both(@x: Nat, @y: Nat, e: Eq()(x * 2, y + y)) -> Nat = match e | refl(@z) => z + 1 end;

        -- A type and a proof parameter, erased beside the data they do not affect.
        let pick(T: Type, p: Eq()(1, 1), n: Nat) -> Nat = n + 1;

        -- An erased field beside a kept one.
        struct Bounded: pub Type { n: Nat, ok: Holds(n < 100) }

        -- A proof-valued callee, applied and dropped.
        let lemma(n: Nat) -> Holds(n <= n) = le/refl(n);
        let keep(n: Nat, _p: Holds(n <= n)) -> Nat = n + 1;

        let _refl_payload: Eq()(z_of(Eq/refl(@Nat, @41)), 42) = Eq/refl();
        let _swapped_payloads: Eq()(digits(Swap/mk(@2, @1)), 21) = Eq/refl();
        let _later_payload: Eq()(tens(Later/mk(@4)), 40) = Eq/refl();
        let _tuple_payload: Eq()(pair(@(3, 4), Eq/refl()), 304) = Eq/refl();
        let _function_payload: Eq()(apply(@(n: Nat) => n + 1, @(m: Nat) => m + 1, Eq/refl()), 6) = Eq/refl();
        let _computed_index: Eq()(both(@6, @6, Eq/refl()), 13) = Eq/refl();
        let _erased_parameters: Eq()(pick(Nat, Eq/refl(), 41), 42) = Eq/refl();
        let _erased_field: Eq()(Bounded { n = 41, ok = /std/Bool/True/qed() }.n + 1, 42) = Eq/refl();
        let _proof_valued_callee: Eq()(keep(41, lemma(41)), 42) = Eq/refl();

        let show(n: Nat) -> Io({}) = print(Str/concat(Nat/to_str(n), "\n"));

        let entry() -> /std/Io({}) =
            let _ = show(z_of(Eq/refl(@Nat, @41)))!;
            let _ = show(digits(Swap/mk(@2, @1)))!;
            let _ = show(tens(Later/mk(@4)))!;
            let _ = show(pair(@(3, 4), Eq/refl()))!;
            let _ = show(apply(@(n: Nat) => n + 1, @(m: Nat) => m + 1, Eq/refl()))!;
            let _ = show(both(@6, @6, Eq/refl()))!;
            let _ = show(pick(Nat, Eq/refl(), 41))!;
            let _ = show(Bounded { n = 41, ok = /std/Bool/True/qed() }.n + 1)!;
            show(keep(41, lemma(41)));
        entry()
        "#;

    assert_eq!(
        String::from_utf8_lossy(&run(source)),
        "42\n21\n40\n304\n6\n13\n42\n42\n42\n"
    );
}
