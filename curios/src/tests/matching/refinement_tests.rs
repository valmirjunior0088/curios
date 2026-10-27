//! What an arm learns from a guard: the dual its false arm proves, a bound across the successor seam, and the definition the guard answers.

use crate::tests::{core_elab, error, run};

// A refinement on a boolean connective reaches every spelling of the scrutinee that reduces to it. `x && g(7)` is the scrutinee; the occurrence is spelled `x && h(7)`, with `h` a different function folding to the same `true`, so the written key misses and the two have to meet through their canonical forms — every operand reduced, on both sides. That is the form each checker's `refined_reduct` brings a probed value and an entry's key to, which is what keeps them reaching the same occurrences when `&&` leaves its right operand as written behind a stuck left. The connectives are tagged in `Term::head_key`: untagged, an operator-spelled scrutinee would register a key nothing looks up, and not even `x && g(7)` itself would refine.
//
// Two occurrences, two routes to the same intrinsic. `x && h(7)` arrives at each reducer as the witness projection the scrutinee was written as; `Bool/and(x, h(7))` arrives under the wrapper's own head, which no key is gated on, and becomes the intrinsic only once the wrapper unfolds. In both checkers each is decided at the stuck reduct, brought to operand-canonical form by `refined_reduct`.
//
// Both checkers run this. Mutation-checked: dropping `BoolAnd` from `head_key` refuses it at `p`, and so does comparing either checker's probed value uncanonicalized.
#[test]
fn a_boolean_refinement_reaches_an_occurrence_spelled_differently_on_its_right() {
    let source = r#"
        use /std/{Eq, Bool, Nat, Str};

        let g(n : Nat) -> Bool = n == 7;
        let h(n : Nat) -> Bool = n == 7;

        let refined(x : Bool) -> Str =
            match x && g(7)
            | true =>
                let p : Eq()(x && h(7), true) = Eq/refl();
                let q : Eq()(Bool/and(x, h(7)), true) = Eq/refl();
                "refined"
            | false => "unrefined"
            end;

        /std/print(refined(true))
        "#;

    assert_eq!(run(source), b"refined");
}

// The false arm of `n < m` is the fact `m <= n`, read the other way. The arm records its equation on the guard as written, and the goal spells the dual, so the reducer asks the dual spelling with the literal negated — in both checkers, since the program certifies.
#[test]
fn the_false_arm_of_a_comparison_proves_its_dual() {
    let source = r#"
        use /std/{Nat, Option};
        use /std/Bool/{True, Holds};

        let at_least(n : Nat, m : Nat) -> Option(Holds(m <= n)) =
            match n < m
            | true => Option/none()
            | false => Option/some(True/qed())
            end;

        let shown(n : Nat, m : Nat) -> Nat =
            match at_least(n, m) | some(_) => n | none() => m end;

        /std/print(Nat/to_str(shown(7, 3)))
        "#;
    assert_eq!(run(source), b"7");
}

// The false arm proves its dual across the `<`/`<=` seam as well. The false arm of `n <= 4` is the fact `4 < n`, and `5 <= n` is that fact spelled with the successor, so the reducer asks the dual of the successor spelling with the literal negated — at `Nat` and at `Int`, in both checkers, since the program certifies. `/std/Str/Valid`'s decoder narrows a quotient's range this way.
#[test]
fn the_false_arm_of_a_comparison_proves_its_dual_across_the_successor_seam() {
    let source = r#"
        use /std/{Nat, Int, Option};
        use /std/Bool/{True, Holds};

        let above(n : Nat) -> Option(Holds(5 <= n)) =
            match n <= 4
            | true => Option/none()
            | false => Option/some(True/qed())
            end;

        let below(i : Int) -> Option(Holds(i <= +4)) =
            match +5 <= i
            | true => Option/none()
            | false => Option/some(True/qed())
            end;

        let shown(n : Nat, i : Int) -> Nat =
            match above(n)
            | some(_) => match below(i) | some(_) => n | none() => 0 end
            | none() => 0
            end;

        /std/print(Nat/to_str(shown(7, +3)))
        "#;
    assert_eq!(run(source), b"7");
}

// A zero check proves the order it rules out, and an order check the zero it rules out: at `Nat` the false arm of `n == 0` is the fact `0 < n`, and the true arm of `0 < n` is the fact `n != 0`, each the arm's own equation read through the alignment of a natural against zero — in both checkers, since the program certifies.
#[test]
fn a_zero_check_and_the_order_it_rules_out_prove_each_other() {
    let source = r#"
        use /std/{Nat, Option};
        use /std/Bool/{True, Holds};

        let positive(n : Nat) -> Option(Holds(0 < n)) =
            match n == 0
            | true => Option/none()
            | false => Option/some(True/qed())
            end;

        let apart(n : Nat) -> Option(Holds(n != 0)) =
            match 0 < n
            | true => Option/some(True/qed())
            | false => Option/none()
            end;

        let shown(n : Nat) -> Nat =
            match positive(n)
            | some(_) => match apart(n) | some(_) => n | none() => 0 end
            | none() => 0
            end;

        /std/print(Nat/to_str(shown(7)))
        "#;
    assert_eq!(run(source), b"7");
}

// The alignment gives both sides of a pair the one reading, so an implicit standing in a comparison against zero is still solved by the fact handed to it: through the equality it is written as, and through the order comparison the same fact is.
#[test]
fn an_implicit_is_solved_through_a_comparison_against_zero() {
    let source = r#"
        use /std/{Nat};
        use /std/Bool/{True, Holds};

        let same(@a : Nat, h : Holds(a == 0)) -> Nat = a;
        let apart(@a : Nat, h : Holds(a != 0)) -> Nat = a;

        let through(n : Nat, zero : Holds(n == 0), least : Holds(n <= 0)) -> Nat =
            same(zero) + same(least);
        let across(n : Nat, positive : Holds(0 < n)) -> Nat =
            apart(positive);

        /std/print(Nat/to_str(across(7, True/qed()) + through(0, True/qed(), True/qed())))
        "#;
    assert_eq!(run(source), b"7");
}

// The control: one step past the dual is another fact, not another spelling. `n <= 4` failing leaves `n` at 5, so `6 <= n` does not follow, and a retry that answered it would be deciding a proposition rather than looking one up.
#[test]
fn the_false_arm_of_a_comparison_proves_nothing_one_step_past_its_dual() {
    let source = r#"
        use /std/{Nat, Option};
        use /std/Bool/{True, Holds};

        let past(n : Nat) -> Option(Holds(6 <= n)) =
            match n <= 4
            | true => Option/none()
            | false => Option/some(True/qed())
            end;

        let shown(n : Nat) -> Nat = match past(n) | some(_) => n | none() => 0 end;

        /std/print(Nat/to_str(shown(7)))
        "#;
    let message = error(source);
    assert!(
        message.contains("type mismatch") && message.contains("Holds(6 <= n)"),
        "the probe decided a fact one step past the guard's dual:\n{message}"
    );
}

// The dead arm of a dispatched guard proves its dual too. `<=` is dispatched through `Cmp`, so the guard's intrinsic spelling is one each checker reaches only by resolving the witness, and the reducer can decide it besides: the quotient is below 64, so the procedure folds `63 < cp % 4096 / 64` to `false` and the false arm is never reached. It is still checked, and the arm's equation answers the dual `true` before the procedure folds it — in both checkers, which is what this asserts by certifying: a kernel meeting the resolved spelling only as the reduct of the written one, after the fold, would refuse what the elaborator accepts. `/std/Str/Valid`'s three-byte decoding is this shape.
#[test]
fn the_dead_arm_of_a_dispatched_guard_proves_its_dual() {
    let source = r#"
        use /std/{Nat};
        use /std/Bool/{True, Holds};

        let nested(cp : Nat) -> Nat =
            match cp % 4096 / 64 <= 63
            | false =>
                let _e : Holds(63 < cp % 4096 / 64) = True/qed();
                0
            | true => 1
            end;

        /std/print(Nat/to_str(nested(70000)))
        "#;
    assert_eq!(run(source), b"1");
}

// A guard answers its own definition one unfolding down. `small(k)` is `k < 10` by definition, so the arm's hypothesis `Holds(k < 10)` is the guard itself, and each checker answers it from the guard's reduced spelling, where comparing keys only as written, with their arguments reduced and their heads never opened, would refuse it. Certifying is what asserts the two agree.
#[test]
fn a_guard_answers_its_definition_one_unfolding_down() {
    let source = r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};

        let small(n : Nat) -> Bool = n < 10;

        let below(k : Nat) -> Nat =
            match small(k)
            | true => (let _p : Holds(k < 10) = True/qed(); k)
            | false => 10
            end;

        /std/print(Nat/to_str(below(3)))
        "#;
    assert_eq!(run(source), b"3");
}

// The same unfolding met late: a struct literal whose parameter is inferred checks its fields before the parameter is known, so the proof's check parks on a metavariable already unfolded past the guard's head, and is retried as `k < 10` once the parameter is solved. Writing `Below(k) { … }` would avoid the unfolding this pins.
#[test]
fn a_field_checked_before_its_struct_parameter_is_inferred_meets_the_guard() {
    let source = r#"
        use /std/{Nat, Bool, Option};
        use /std/Bool/{True, Holds};

        let small(n : Nat) -> Bool = n < 10;

        struct Below(k : Nat) : pub Type {
            value : Nat,
            proof : Holds(small(k)),
        }

        let below(k : Nat) -> Option(Below(k)) =
            match small(k)
            | true => Option/some(Below { value = k, proof = True/qed() })
            | false => Option/none()
            end;

        /std/print(Nat/to_str(Option/unwrap_or(Option/map(below(3), (b) => b.value), 10)))
        "#;
    assert_eq!(run(source), b"3");
}

// A guard whose definition unfolds to a `match` rather than a comparison — `Nat/in_range`'s shape — answers the same `match` reached through another definition. The reduced spellings are compared as terms, so the two bodies must agree even in the result their `match` states, which two definitions elaborated alike do.
#[test]
fn a_guard_whose_definition_is_a_match_answers_that_match() {
    let source = r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};

        let between(n : Nat) -> Bool =
            match n >= 3 | true => n <= 9 | false => false end;

        let also_between(n : Nat) -> Bool =
            match n >= 3 | true => n <= 9 | false => false end;

        let inside(k : Nat) -> Nat =
            match between(k)
            | true => (let _p : Holds(also_between(k)) = True/qed(); k)
            | false => 0
            end;

        /std/print(Nat/to_str(inside(5)))
        "#;
    assert_eq!(run(source), b"5");
}

// A guard decides a bound spelled across the `<`/`<=` seam. `List/slice`'s precondition is `s + l <= len`, so slicing one element at `i` asks for `i + 1 <= len(l)`, while the guard a program writes to establish it is `i < len(l)` — one proposition, two spellings, and the arm records only the one the author wrote. Both reducers retry a miss on the successor spelling, so a bound discharges without the author having to spell the comparison the way the standard library's signature happens to.
//
// It certifies, which is the half that matters: the seam is a rule only when both checkers look in the same two places.
#[test]
fn a_guard_discharges_a_bound_across_the_successor_seam() {
    let source = r#"
        use /std/{Nat, List};

        let one_at(@T : Type, l : List(T), i : Nat) -> List(T) =
            match i < List/len(l)
            | false => []
            | true => List/slice(l, i, 1)
            end;

        /std/print(Nat/to_str(List/len(one_at([1, 2, 3], 1))))
        "#;
    assert_eq!(run(source), b"1");
}

// The seam in the other direction, where the guard is the `<=` and the bound the `<`: `List/get` asks for `i < len(l)`, and a program that established `i + 1 <= len(l)` has proved it.
#[test]
fn a_bound_below_a_length_is_decided_by_the_successor_guard() {
    let source = r#"
        use /std/{Nat, List, Option};

        let get_at(@T : Type, l : List(T), i : Nat) -> Option(T) =
            match i + 1 <= List/len(l)
            | false => Option/none()
            | true => Option/some(List/get(l, i))
            end;

        let shown(l : List(Nat), i : Nat) -> Nat =
            match get_at(l, i) | some(x) => x | none() => 0 end;

        /std/print(Nat/to_str(shown([4, 5, 6], 2)))
        "#;
    assert_eq!(run(source), b"6");
}

// **The seam is a second spelling, not a second fact.** A retry looks up another spelling of a guard and decides no proposition, so a bound the guards do not imply stays stuck — `i < n` and `len(l) == n * k` bound `i * k + k` by `len(l)` and say nothing of `i * k + k + 1`. A bound they do imply through arithmetic is the procedure that proves a bound from the facts in scope, which `tests::bounds` holds: its grid fills `i * k + k <= len(l)` from these two guards by a product.
#[test]
fn a_bound_the_guards_do_not_imply_is_still_stuck() {
    let source = r#"
        use /std/{Nat, List};

        let chunk(@T : Type, n : Nat, k : Nat, l : List(T), i : Nat) -> List(T) =
            match i < n
            | false => []
            | true =>
                match List/len(l) == n * k
                | false => []
                | true => List/slice(l, i * k + 1, k)
                end
            end;

        /std/print("unreached")
        "#;
    let message = error(source);
    assert!(
        message.contains("nothing discharged"),
        "a bound the guards do not imply was discharged:\n{message}"
    );
}

// A guard's fact holds under a second match on a variable the guard names. The kernel checks that arm with the variable substituted by its case value, so the guard's equation has to follow the substitution, or the kernel refuses what the elaborator, which keeps the variable spelled, accepts: the `0` arm states the bound as the guard wrote it and with the case value written, and both discharge.
//
// Mutation-checked: without `Kernel::restate_refinements` at `assume_case_value` the kernel refuses the program, its expected type reading `0` for `i`.
#[test]
fn a_guards_fact_holds_under_a_match_on_a_variable_it_names() {
    let source = r#"
        use /std/{Nat, List, Str};
        use /std/Bool/{Holds, True};

        let first_or(xs : List(Nat), i : Nat) -> Str =
            match i < List/len(xs)
            | true =>
                match i
                | 0 =>
                    let _ : Holds(i < List/len(xs)) = True/qed();
                    let _ : Holds(0 < List/len(xs)) = True/qed();
                    "first"
                | _ => "later"
                end
            | false => "past"
            end;

        /std/print(first_or([7, 8], 0))
        "#;

    assert_eq!(run(source), b"first");
}

// An arm's goal takes the case wherever the scrutinee occurs in it, an occurrence an operator spells through a witness of its own among them. `f`'s result matches on `a <= b` and so does `f`, so in each arm the result's match is on that arm's case, which the goal of the match inside the arm shows: a match on a literal.
#[test]
fn an_arms_goal_takes_the_case_where_an_operator_spells_the_scrutinee() {
    let elaborated = core_elab(
        r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};
        let g(a: Nat, b: Nat, @ok: Holds(a <= b)) -> Nat = 1;
        let f(a: Nat, b: Nat, k: Nat) -> Holds((match a <= b | true => g(a, b, @True/qed()) | false => 0 end) <= 1) =
            match a <= b
            | true => match k | 0 => True/qed() | _ + 1 => True/qed() end
            | false => match k | 0 => True/qed() | _ + 1 => True/qed() end
            end;
        /std/print("held")
        "#,
    );

    assert!(
        elaborated.contains("Bool.match false ~ Nat")
            && elaborated.contains("Bool.match true ~ Nat"),
        "{elaborated}"
    );
}
