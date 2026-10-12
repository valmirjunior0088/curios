//! What is judged of a metavariable is judged where it was born.
//!
//! A metavariable holds the frames it was born under, and a solution's check stands on them where it is made elsewhere: each guard in the frame it was registered in, so a guard read under another is read under it there as it is where the metavariable was born, and nothing of the arm the judgment is made in is read.

use crate::tests::run;

/// An implicit born under two guards is solved from an arm inside them. `same`'s `w` is born where the inner guard, read under the outer, is `y <= z`, and is solved inside the arm of `b`: its candidate holds the proof of `y <= z`, which checks where `w` was born, the inner guard under the outer. With the birth's guards registered again in one frame the inner one said nothing there, and the call was refused where `same(r)` compiled; it was refused the same from the arm of another guard, and compiled under one guard.
#[test]
fn an_implicit_born_under_two_guards_is_solved_from_an_arm_inside() {
    let output = run(r#"
        use /std/{Eq, Bool, Nat, print};
        use /std/Bool/{Holds};

        let p(x: Nat) -> Bool = x % 2 == 0;
        let q(c: Bool, y: Nat) -> Nat = match c | true => y | false => 0 end;

        struct Le(z: Nat): Type {
            low: Nat,
            @ok: Holds(low <= z),
        }

        let same(@z: Nat, @w: Le(z), e: Eq(@Le(z))(w, w)) -> Nat = w.low;

        let where_born(x: Nat, y: Nat, z: Nat) -> Nat =
            match p(x)
            | true =>
                match q(p(x), y) <= z
                | true => (let r: Eq(@Le(z))(Le { low = y }, Le { low = y }) = Eq/refl(); same(r))
                | false => 0
                end
            | false => 0
            end;

        let from_an_arm(x: Nat, y: Nat, z: Nat, b: Bool) -> Nat =
            match p(x)
            | true =>
                match q(p(x), y) <= z
                | true =>
                    (let r: Eq(@Le(z))(Le { low = y }, Le { low = y }) = Eq/refl();
                    same(match b | true => r | false => r end))
                | false => 0
                end
            | false => 0
            end;

        let from_a_guards_arm(x: Nat, y: Nat, z: Nat, k: Nat) -> Nat =
            match p(x)
            | true =>
                match q(p(x), y) <= z
                | true =>
                    (let r: Eq(@Le(z))(Le { low = y }, Le { low = y }) = Eq/refl();
                    same(match p(k) | true => r | false => r end))
                | false => 0
                end
            | false => 0
            end;

        let under_one_guard(y: Nat, z: Nat, b: Bool) -> Nat =
            match y <= z
            | true =>
                (let r: Eq(@Le(z))(Le { low = y }, Le { low = y }) = Eq/refl();
                same(match b | true => r | false => r end))
            | false => 0
            end;

        print(Nat/to_str(
            where_born(2, 1, 3)
                + from_an_arm(2, 1, 3, true)
                + from_a_guards_arm(2, 1, 3, 4)
                + under_one_guard(1, 3, true)))
        "#);

    assert_eq!(output, b"4");
}
