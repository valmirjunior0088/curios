//! What an arm is checked at: its own case, and a hypothesis at the instance it was given.

use {super::test_support::*, crate::test_support::*, curios_elab::Error};

#[test]
fn an_induction_hypothesis_is_typed_at_the_predecessor() {
    refused(INDUCTION_HYPOTHESIS_AT_THE_SCRUTINEE).by_elaborator_in("/bogus", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

#[test]
fn a_dispatch_default_is_checked_at_its_scrutinee() {
    refused(DISPATCH_DEFAULT_AT_A_CASE).by_elaborator_in("/bogus", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

#[test]
fn a_boolean_arm_is_checked_at_its_own_case() {
    refused(BOOL_ARM_AT_THE_WRONG_CASE).by_elaborator_in("/bogus", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

#[test]
fn a_dispatch_literal_arm_is_checked_at_that_literal() {
    refused(DISPATCH_LITERAL_AT_THE_WRONG_VALUE).by_elaborator_in("/bogus", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

// A guard's arm learns its own comparison and nothing stronger: `x <= y` at `true` is no `x < y`, which fails at `x = y`.
#[test]
fn a_guard_at_most_does_not_answer_the_strict_comparison() {
    refused(
        r#"
        use /std/{Nat};
        use /std/Bool/{True, Holds};

        let strict(x: Nat, y: Nat, h: Holds(x <= y)) -> Holds(x < y) =
            match x <= y | true => True/qed() | false => match h end end;

        /std/print(Nat/to_str(0))
        "#,
    )
    .by_elaborator_in("/strict", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

// What a classification learned under an arm's equation lives as long as the arm: inside the `0` arm `f(h(x)) - f(0)` is zero, and after the match nothing says `h(x)` is.
#[test]
fn a_classing_made_under_an_arms_equation_answers_nothing_after_the_arm() {
    refused(r#"
        use /std/{Nat, Eq};
        use /std/Bool/{True, Holds};

        let leak(f: (Nat) -> Nat, h: (Nat) -> Nat, x: Nat) -> Holds(f(h(x)) - f(0) == 0) =
            let warm: Nat = match h(x) | 0 => (let seen: Eq()(f(h(x)) - f(0), 0) = Eq/refl(); 0) | k + 1 => 0 end;
            True/qed();

        /std/print(Nat/to_str(0))
        "#).by_elaborator_in("/leak", |error| matches!(error, Error::TypeMismatch { .. }));
}

// What a guard's spelling settled under a split's re-typing answers is the split's: at `c = true` the operands are units and the guard is `k(0)`, inside a lambda there as anywhere in the arm, and after the split nothing says so. `curios-cert`'s `scope_tests` hold the kernel to the same terms.
#[test]
fn a_guard_settled_in_a_lambda_inside_a_split_answers_nothing_after_the_split() {
    refused(r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};

        let F(c: Bool) -> Type = match c | true => {} | false => Nat end;
        let leak(c: Bool, h: (F(c)) -> Nat, k: (Nat) -> Bool, p: F(c), q: F(c), guard: Holds(match h(p) == h(q) | true => k(0) | false => k(1) end)) -> Holds(k(0)) =
            match (match h(p) == h(q) | true => k(0) | false => k(1) end)
            | false => match guard end
            | true =>
                let split: Nat = match c | false => 0 | true => ((settled: Holds(k(0))) => 0)(True/qed()) end;
                True/qed()
            end;

        /std/print(Nat/to_str(0))
        "#).by_elaborator_in("/leak", |error| matches!(error, Error::TypeMismatch { .. }));
}

// The same guard settled in the arm of a split the operands are units in, and asked in the arm where they are numbers.
#[test]
fn a_guard_settled_in_one_arm_of_a_split_answers_nothing_in_the_other() {
    refused(r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};

        let F(c: Bool) -> Type = match c | false => {} | true => Nat end;
        let leak(c: Bool, h: (F(c)) -> Nat, k: (Nat) -> Bool, p: F(c), q: F(c), guard: Holds(match h(p) == h(q) | true => k(0) | false => k(1) end)) -> Holds(k(0)) =
            match (match h(p) == h(q) | true => k(0) | false => k(1) end)
            | false => match guard end
            | true =>
                match c
                | false => (let settled: Holds(k(0)) = True/qed(); True/qed())
                | true => True/qed()
                end
            end;

        /std/print(Nat/to_str(0))
        "#).by_elaborator_in("/leak", |error| matches!(error, Error::TypeMismatch { .. }));
}

// IEEE's `==` calls the two zeros equal where they are two terms, so a float equality's arm learns the comparison and no equation of its operands. Read as one, `eq_of_eql` would prove `+0.0` and `-0.0` one value, whose bytes differ.
#[test]
fn a_float_equality_arm_learns_no_equation_of_its_operands() {
    refused(
        r#"
        use /std/{Flt, Eq};
        use /std/Bool/{True, Holds};

        let eq_of_eql(a: Flt, b: Flt, h: Holds(a == b)) -> Eq()(a, b) =
            match a == b | true => Eq/refl() | false => match h end end;

        let _forged: Eq()(+0.0, -0.0) = eq_of_eql(+0.0, -0.0, True/qed());

        /std/print("unreachable")
        "#,
    )
    .by_elaborator_in(
        "/eq_of_eql",
        |error| matches!(error, Error::TypeMismatch { expected, .. } if pinned(expected) == "/std/Eq/Eq(Flt)(a, b)"),
    );
}

// No order relates a NaN, so a float comparison that came back false states no mirror: under the false arm of `a < b`, `b <= a` is false wherever `a` is a NaN. Read as its dual, as a total order's is, the arm would hand `mirror` a proof of `1.0 <= +nan.0`, which is `False`.
#[test]
fn a_false_float_comparison_arm_learns_no_mirror() {
    refused(
        r#"
        use /std/{Flt, Bool};
        use /std/Bool/{True, Holds};

        let mirror(a: Flt, b: Flt, h: Holds(Bool/not(a < b))) -> Holds(b <= a) =
            match a < b | true => match h end | false => True/qed() end;

        let _forged: Holds(1.0 <= +nan.0) = mirror(+nan.0, 1.0, True/qed());

        /std/print("unreachable")
        "#,
    )
    .by_elaborator_in(
        "/mirror",
        |error| matches!(error, Error::TypeMismatch { expected, .. } if pinned(expected) == "/sys/Bool/Holds(b <= a)"),
    );
}

// A literal dispatch is `Nat`'s alone. Over a float it would read IEEE's `==`, which takes `-0.0` at the arm `0` while that arm's case states the scrutinee is the literal, `+0.0`: an arm proving `Eq()(f, +0.0)` would prove it of `-0.0`.
#[test]
fn a_literal_dispatch_reads_no_float() {
    refused(
        r#"
        use /std/{Nat, Flt};

        let tag(f: Flt) -> Nat = match f | 0 => 1 | _ => 0 end;

        /std/print(Nat/to_str(tag(-0.0)))
        "#,
    )
    .by_elaborator_in(
        "/tag",
        |error| matches!(error, Error::NotNatType { head_type } if pinned(head_type) == "Flt"),
    );
}

// `A` and `B` are one group whose parameters share the name `n`, a `Nat` in one and a `Bool` in the other, and `mkA`'s payload states `n == 1` of its own family's. An `A(0)` holds a `Holds(0 == 1)`, so `forged`'s `qed` would fit only where the case were read at its sibling's parameter, or at another instance's, and `f` would hand it on at `False`. The kernel puts the same proposition as Core text under this name, in `curios-cert`'s `recheck::case_tests`.
#[test]
fn a_case_reads_its_own_familys_parameter_in_a_group() {
    refused(
        r#"
        use /std/{Nat, Bool};
        use /std/Bool/{False, True, Holds};

        induct A(n: Nat): Type
        | mkA(h: Holds(n == 1), other: B(true))
        and B(n: Bool): Type
        | mkB(h: Holds(n), other: A(0))
        | base()
        end

        let f(x: A(0)) -> False = match x | mkA(h, _) => h end;

        let forged: False = f(A/mkA(True/qed(), B/base()));

        /std/print("FORGED")
        "#,
    )
    .by_elaborator_in("/forged", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

// The control: an `A(1)` holds a `Holds(1 == 1)`, which `qed` inhabits, and its arm runs.
#[test]
fn a_case_of_a_family_in_a_group_is_built_at_its_own_parameter() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Bool};
        use /std/Bool/{False, True, Holds};

        induct A(n: Nat): Type
        | mkA(h: Holds(n == 1), other: B(true))
        and B(n: Bool): Type
        | mkB(h: Holds(n), other: A(0))
        | base()
        end

        let f(x: A(1)) -> Nat = match x | mkA(h, _) => 1 end;

        /std/print(Nat/to_str(f(A/mkA(True/qed(), B/base()))))
        "#),
        b"1"
    );
}
