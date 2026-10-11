//! What the kernel types of a type the elaborator derived by substitution: one typed where a reader wrote it is typed at every value put in a variable's place, an arm no reduction takes there being typed by no rule.

use crate::tests::run;

// An arm its scrutinee's indices rule out is not typed. `h`'s result matches on `t: Tag(n)`, and its `zero()` arm holds a proof that `n == 0`, typed under that case's `n := 0`. A `let` at `h(1, t)` is written at the result at `1`, where `Tag/zero()`'s target clashes with the index; at `0` the arm is the one taken and is typed.
#[test]
fn a_result_is_typed_at_an_index_that_rules_an_arm_of_it_out() {
    let at = |index: &str| {
        format!(
            r#"
        use /std/{{Nat, Bool}};
        use /std/Bool/{{True, Holds}};
        induct Tag: (Nat) -> Type
        | zero(): (0)
        | other(n: Nat): (n + 1)
        end
        let needs_zero(n: Nat, @ok: Holds(n == 0)) -> Nat = 7;
        let h(n: Nat, t: Tag(n)) -> Holds((match t | zero() => needs_zero(n) | other(_) => 0 end) <= 7) =
            match t
            | zero() => True/qed()
            | other(_) => True/qed()
            end;
        let used(t: Tag({index})) -> Nat =
            let x = h({index}, t);
            0;
        /std/print("held")
        "#
        )
    };

    assert_eq!(run(&at("1")), b"held");
    assert_eq!(run(&at("0")), b"held");
}
