//! A parked problem's retry: decided in the context the problem parked in.
//!
//! What parks holds the frames it parks under, and its retry stands on them wherever it runs: each guard in the frame it was registered in, so a guard read under another is read under it on a retry as it is in place.

use crate::tests::run;

/// A call read on a retry is read under both guards of its arm. `need(y, z)` asks for `y <= z`, which the inner guard gives once the outer one has made `p(x)` true: `q(p(x), y) <= z` is `y <= z` there. Behind `late`'s lambda the call is read on a retry, which stands on the two arms' frames, the inner guard under the outer. Registered again side by side in one frame, the inner guard was settled with nothing of the outer and the declaration was refused, `a proof was found and did not check`; it was refused the same where the retry ran in another arm than the lambda's.
#[test]
fn a_call_read_on_a_retry_is_read_under_both_guards_of_its_arm() {
    let output = run(r#"
        use /std/{Bool, Nat, print};
        use /std/Bool/{Holds};

        let p(x: Nat) -> Bool = x % 2 == 0;
        let q(c: Bool, y: Nat) -> Nat = match c | true => y | false => 0 end;
        let need(y: Nat, z: Nat, @ok: Holds(y <= z)) -> Nat = y;
        let late(@A: Type, @B: Type, f: (A) -> B, a: A) -> B = f(a);

        let in_place(x: Nat, y: Nat, z: Nat) -> Nat =
            match p(x)
            | true => match q(p(x), y) <= z | true => need(y, z) | false => 0 end
            | false => 0
            end;

        let on_a_retry(x: Nat, y: Nat, z: Nat) -> Nat =
            match p(x)
            | true => match q(p(x), y) <= z | true => late((v) => need(y, z), 0) | false => 0 end
            | false => 0
            end;

        let in_another_arm(x: Nat, y: Nat, z: Nat, b: Bool) -> Nat =
            match p(x)
            | true =>
                match q(p(x), y) <= z
                | true => late((v) => need(y, z), match b | true => y | false => z end)
                | false => 0
                end
            | false => 0
            end;

        let under_one_guard(y: Nat, z: Nat) -> Nat =
            match y <= z | true => late((v) => need(y, z), 0) | false => 0 end;

        print(Nat/to_str(
            in_place(2, 1, 3)
                + on_a_retry(2, 1, 3)
                + in_another_arm(2, 1, 3, true)
                + under_one_guard(1, 3)))
        "#);

    assert_eq!(output, b"4");
}
