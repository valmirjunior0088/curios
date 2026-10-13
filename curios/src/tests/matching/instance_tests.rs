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

/// `g`, whose bound a guard's arm proves by standing under it.
const BOUNDED: &str = r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};
        let g(c: Bool, @ok: Holds(c)) -> Nat = 1;
        "#;

// An arm under a scrutinee that computes to another case is not typed. `h`'s result matches on `c`, and its `true` arm holds a proof that `c` holds, which the arm's case is. A `let` at `h(Bool/not(true))` is written at the result at a scrutinee that names no local, so no equation is recorded for the arm, and that computes to `false`; at `Bool/not(false)` the arm is the one taken, and its proof holds by reduction.
#[test]
fn a_result_is_typed_at_a_value_that_computes_to_another_case() {
    let at = |argument: &str| {
        format!(
            r#"{BOUNDED}
        let h(c: Bool) -> Holds((match c | true => g(c, @True/qed()) | false => 0 end) <= 1) =
            match c
            | false => True/qed()
            | true => True/qed()
            end;
        let used() -> Nat =
            let x = h({argument});
            0;
        /std/print("held")
        "#
        )
    };

    assert_eq!(run(&at("Bool/not(true)")), b"held");
    assert_eq!(run(&at("Bool/not(false)")), b"held");
    assert_eq!(run(&at("false")), b"held");
}

// The same instance wherever the elaborator writes it: as the domain of a lambda checked at an arm's goal, as the goal of a match inside an arm, over a comparison a match on its operand decides, and in a local function elaborated again.
#[test]
fn an_arms_goal_is_typed_wherever_it_is_written() {
    let domain = format!(
        r#"{BOUNDED}
        let k(c: Bool) -> (x: Holds((match Bool/not(c) | true => g(Bool/not(c), @True/qed()) | false => 0 end) <= 1)) -> Nat =
            match c
            | false => (x) => 0
            | true => (x) => 1
            end;
        /std/print("held")
        "#
    );
    let goal = format!(
        r#"{BOUNDED}
        let f(c: Bool, k: Nat) -> Holds((match Bool/not(c) | true => g(Bool/not(c), @True/qed()) | false => 0 end) <= 1) =
            match c
            | false => True/proved()
            | true => match k | 0 => True/qed() | _ + 1 => True/qed() end
            end;
        /std/print("held")
        "#
    );
    let comparison = r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};
        let positive(n: Nat, @ok: Holds(0 < n)) -> Nat = 1;
        let f(n: Nat, k: Nat) -> Holds((match 0 < n | true => positive(n, @True/qed()) | false => 0 end) <= 1) =
            match n
            | 0 => match k | 0 => True/qed() | _ + 1 => True/qed() end
            | _ + 1 => True/proved()
            end;
        /std/print("held")
        "#;
    let again = format!(
        r#"{BOUNDED}
        use /std/{{Eq}};
        let h(c: Bool) -> Holds((match c | true => g(c, @True/qed()) | false => 0 end) <= 1) =
            match c
            | false => True/qed()
            | true => True/qed()
            end;
        let used(n: Nat) -> Eq()(0, 0) =
            let of(k: Nat) -> Nat =
                match k
                | 0 =>
                    let x = h(Bool/not(true));
                    0
                | m + 1 => of(m)
                end;
            let same: Eq()(of(n), of(n)) = Eq/refl();
            Eq/refl();
        /std/print("held")
        "#
    );

    assert_eq!(run(&domain), b"held");
    assert_eq!(run(&goal), b"held");
    assert_eq!(run(comparison), b"held");
    assert_eq!(run(&again), b"held");
}

// An arm is typed under the equations around it, and an instance may leave one of those computing to another case. In `h2` the inner arm solves `d`, and the outer equation it restates names no local at `c := true`. In `h3` the scrutinee still names `d` at the instance and computes to `false` all the same, where the proof spells it with its operands the other way round.
#[test]
fn a_result_is_typed_where_an_equation_around_an_arm_computes_to_another_case() {
    let restated = format!(
        r#"{BOUNDED}
        let h2(c: Bool, d: Bool) -> Holds((match Bool/not(c) && d | true => (match d | true => g(Bool/not(c) && d, @True/qed()) | false => 0 end) | false => 0 end) <= 1) =
            match c
            | true => True/qed()
            | false => match d | true => True/qed() | false => True/qed() end
            end;
        let used(d: Bool) -> Nat =
            let x = h2(true, d);
            0;
        /std/print("held")
        "#
    );
    let respelled = format!(
        r#"{BOUNDED}
        let h3(c: Bool, d: Bool) -> Holds((match Bool/not(c) && d | true => g(d && Bool/not(c), @True/qed()) | false => 0 end) <= 1) =
            match c
            | true => True/qed()
            | false => match d | true => True/qed() | false => True/qed() end
            end;
        let used(d: Bool) -> Nat =
            let x = h3(true, d);
            0;
        /std/print("held")
        "#
    );

    assert_eq!(run(&restated), b"held");
    assert_eq!(run(&respelled), b"held");
}

// A row that a binder of its own puts under cases it does not state is elaborated once, at its own pattern, and each leaf that takes it holds that body with the leaf's binders in its variables' places, typed by the kernel where it stands. `(_, true)` is reached under `some` and under `none`, and its body holds a match of its own.
#[test]
fn a_row_reached_at_two_leaves_is_typed_at_each() {
    let source = r#"
        use /std/{Bool, Nat, Option};
        let f(m: Option(Nat), c: Bool, d: Bool) -> Nat =
            match (m, c)
            | (_, true) => (match d | true => 1 | false => 2 end)
            | (some(x), false) => x
            | (none(), false) => 0
            end;
        /std/print(Nat/to_str(f(Option/some(5), true, true) + 10 * f(Option/none(), true, false) + 100 * f(Option/some(5), false, true)))
        "#;

    assert_eq!(run(source), b"521");
}
