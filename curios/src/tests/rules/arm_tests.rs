//! What an arm is checked at: its own case, and a hypothesis at the instance it was given.

use {super::test_support::*, curios_elab::Error};

// The induction hypothesis of a `Nat` fold is the motive at the predecessor, and the goal the motive at the successor: handed back directly, `ih` would prove `Eq()(k + 1, 0)` from `Eq()(k, 0)`, so a rule that typed it at the scrutinee rather than at the peeled index would make every predicate provable by induction. The control builds a vector at each length from the one below, which only the hypothesis at the predecessor types.
#[test]
fn an_induction_hypothesis_is_typed_at_the_predecessor() {
    control(
        r#"
        use /std/{Nat, Vec};
        let bogus(n : Nat) -> Vec(Nat, n) =
            match n : (m) => Vec(Nat, m)
            | 0 => Vec/nil()
            | k + 1; ih => Vec/cons(k, ih)
            end;
        /std/print(Nat/to_str(Vec/first(bogus(3))))
        "#,
        b"2",
    )
    .refused(&[
        ("{Nat, Vec}", "{Nat, Eq}"),
        ("-> Vec(Nat, n) =", "-> Eq()(n, 0) ="),
        ("(m) => Vec(Nat, m)", "(m) => Eq()(m, 0)"),
        ("| 0 => Vec/nil()", "| 0 => Eq/refl()"),
        ("Vec/cons(k, ih)", "ih"),
        ("Nat/to_str(Vec/first(bogus(3)))", "\"FORGED\""),
    ])
    .by_elaborator_in("/bogus", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

// A natural-number dispatch's default binds nothing and refines no index, so it is checked at the scrutinee's own value: its goal is `Eq()(n, 0)` for an arbitrary `n`, which `Eq/refl()` cannot inhabit. Checked at the `0` arm's instance — the shape a refinement leak would produce — reflexivity would discharge it and every natural would equal zero. The control states what reflexivity proves at every value.
#[test]
fn a_dispatch_default_is_checked_at_its_scrutinee() {
    control(
        r#"
        use /std/{Nat, Eq};
        let bogus(n : Nat) -> Eq()(n, n) =
            match n : (m) => Eq()(m, m)
            | 0 => Eq/refl()
            | _ => Eq/refl()
            end;
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("-> Eq()(n, n) =", "-> Eq()(n, 0) ="),
        ("(m) => Eq()(m, m)", "(m) => Eq()(m, 0)"),
    ])
    .by_elaborator_in("/bogus", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

// A `Bool` arm is checked at its own case value: the `false` arm's goal is the motive at `false`, so `Eq/refl()` would have to inhabit `Eq()(false, true)`, and an arm rule that refined the scrutinee to the wrong case, or to none, would let every boolean equal `true`. The control's motive reduces at each arm's own case.
#[test]
fn a_boolean_arm_is_checked_at_its_own_case() {
    control(
        r#"
        use /std/{Bool, Eq};
        let bogus(b : Bool) -> Eq()(Bool/not(Bool/not(b)), b) =
            match b : (c) => Eq()(Bool/not(Bool/not(c)), c)
            | true => Eq/refl()
            | false => Eq/refl()
            end;
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("-> Eq()(Bool/not(Bool/not(b)), b) =", "-> Eq()(b, true) ="),
        (
            "(c) => Eq()(Bool/not(Bool/not(c)), c)",
            "(c) => Eq()(c, true)",
        ),
    ])
    .by_elaborator_in("/bogus", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

// A dispatch's literal arm is checked at that literal: the `1` arm's goal is the motive at `1`, which `Eq/refl()` cannot inhabit for `Eq()(1, 0)` — the default arm's companion, a literal arm refined to the wrong literal. The control states what reflexivity proves at every value.
#[test]
fn a_dispatch_literal_arm_is_checked_at_that_literal() {
    control(
        r#"
        use /std/{Nat, Eq};
        let bogus(n : Nat) -> Eq()(n, n) =
            match n : (m) => Eq()(m, m)
            | 0 => Eq/refl()
            | 1 => Eq/refl()
            | _ => Eq/refl()
            end;
        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("-> Eq()(n, n) =", "-> Eq()(n, 0) ="),
        ("(m) => Eq()(m, m)", "(m) => Eq()(m, 0)"),
    ])
    .by_elaborator_in("/bogus", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

// A guard's arm learns its own comparison and nothing stronger: `x <= y` at `true` is no `x < y`, which fails at `x = y`. The control states the comparison the arm learns.
#[test]
fn a_guard_at_most_does_not_answer_the_strict_comparison() {
    control(
        r#"
        use /std/{Nat};
        use /std/Bool/{True, Holds};

        let strict(x: Nat, y: Nat, h: Holds(x <= y)) -> Holds(x <= y) =
            match x <= y | true => True/qed() | false => match h end end;

        /std/print(Nat/to_str(0))
        "#,
        b"0",
    )
    .refused(&[("-> Holds(x <= y) =", "-> Holds(x < y) =")])
    .by_elaborator_in("/strict", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

// What a classification learned under an arm's equation lives as long as the arm: inside the `0` arm `f(h(x)) - f(0)` is zero, and after the match nothing says `h(x)` is. The control claims nothing past the arm.
#[test]
fn a_classing_made_under_an_arms_equation_answers_nothing_after_the_arm() {
    control(r#"
        use /std/{Nat, Eq};
        use /std/Bool/{True, Holds};

        let leak(f: (Nat) -> Nat, h: (Nat) -> Nat, x: Nat) -> True =
            let warm: Nat = match h(x) | 0 => (let seen: Eq()(f(h(x)) - f(0), 0) = Eq/refl(); 0) | k + 1 => 0 end;
            True/qed();

        /std/print(Nat/to_str(0))
        "#, b"0")
        .refused(&[("-> True =", "-> Holds(f(h(x)) - f(0) == 0) =")])
        .by_elaborator_in("/leak", |error| matches!(error, Error::TypeMismatch { .. }));
}

// What a guard's spelling settled under a split's re-typing answers is the split's: at `c = true` the operands are units and the guard is `k(0)`, inside a lambda there as anywhere in the arm, and after the split nothing says so. `curios-cert`'s `scope_tests` hold the kernel to the same terms. The control claims nothing past the split.
#[test]
fn a_guard_settled_in_a_lambda_inside_a_split_answers_nothing_after_the_split() {
    control(r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};

        let F(c: Bool) -> Type = match c | true => {} | false => Nat end;
        let leak(c: Bool, h: (F(c)) -> Nat, k: (Nat) -> Bool, p: F(c), q: F(c), guard: Holds(match h(p) == h(q) | true => k(0) | false => k(1) end)) -> True =
            match (match h(p) == h(q) | true => k(0) | false => k(1) end)
            | false => match guard end
            | true =>
                let split: Nat = match c | false => 0 | true => ((settled: Holds(k(0))) => 0)(True/qed()) end;
                True/qed()
            end;

        /std/print(Nat/to_str(0))
        "#, b"0")
        .refused(&[(") -> True =\n", ") -> Holds(k(0)) =\n")])
        .by_elaborator_in("/leak", |error| matches!(error, Error::TypeMismatch { .. }));
}

// The same guard settled in the arm of a split the operands are units in, and asked in the arm where they are numbers. The control claims nothing in the other arm.
#[test]
fn a_guard_settled_in_one_arm_of_a_split_answers_nothing_in_the_other() {
    control(r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};

        let F(c: Bool) -> Type = match c | false => {} | true => Nat end;
        let leak(c: Bool, h: (F(c)) -> Nat, k: (Nat) -> Bool, p: F(c), q: F(c), guard: Holds(match h(p) == h(q) | true => k(0) | false => k(1) end)) -> True =
            match (match h(p) == h(q) | true => k(0) | false => k(1) end)
            | false => match guard end
            | true =>
                match c
                | false => (let settled: Holds(k(0)) = True/qed(); True/qed())
                | true => True/qed()
                end
            end;

        /std/print(Nat/to_str(0))
        "#, b"0")
        .refused(&[(") -> True =\n", ") -> Holds(k(0)) =\n")])
        .by_elaborator_in("/leak", |error| matches!(error, Error::TypeMismatch { .. }));
}

// IEEE's `==` calls the two zeros equal where they are two terms, so a float equality's arm learns the comparison and no equation of its operands. Read as one, `eq_of_eql` would prove `+0.0` and `-0.0` one value, whose bytes differ. The control states the comparison the arm learns, which holds of `+0.0` and `-0.0`.
#[test]
fn a_float_equality_arm_learns_no_equation_of_its_operands() {
    control(
        r#"
        use /std/{Flt, Eq};
        use /std/Bool/{True, Holds};

        let eq_of_eql(a: Flt, b: Flt, h: Holds(a == b)) -> Holds(a == b) =
            match a == b | true => True/qed() | false => match h end end;

        let _same: Holds(+0.0 == -0.0) = eq_of_eql(+0.0, -0.0, True/qed());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("-> Holds(a == b) =", "-> Eq()(a, b) ="),
        ("| true => True/qed()", "| true => Eq/refl()"),
        ("let _same: Holds(+0.0 == -0.0)", "let _forged: Eq()(+0.0, -0.0)"),
    ])
    .by_elaborator_in(
        "/eq_of_eql",
        |error| matches!(error, Error::TypeMismatch { expected, .. } if pinned(expected) == "/std/Eq/Eq(Flt)(a, b)"),
    );
}

// No order relates a NaN, so a float comparison that came back false states no mirror: under the false arm of `a < b`, `b <= a` is false wherever `a` is a NaN. Read as its dual, as a total order's is, the arm would hand `mirror` a proof of `1.0 <= +nan.0`, which is `False`. The control compares naturals, a total order, whose false arm states the mirror.
#[test]
fn a_false_float_comparison_arm_learns_no_mirror() {
    control(
        r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};

        let mirror(a: Nat, b: Nat, h: Holds(Bool/not(a < b))) -> Holds(b <= a) =
            match a < b | true => match h end | false => True/qed() end;

        let _mirrored: Holds(1 <= 2) = mirror(2, 1, True/qed());

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("{Nat, Bool}", "{Flt, Bool}"),
        ("(a: Nat, b: Nat,", "(a: Flt, b: Flt,"),
        (
            "let _mirrored: Holds(1 <= 2) = mirror(2, 1, True/qed());",
            "let _forged: Holds(1.0 <= +nan.0) = mirror(+nan.0, 1.0, True/qed());",
        ),
    ])
    .by_elaborator_in(
        "/mirror",
        |error| matches!(error, Error::TypeMismatch { expected, .. } if pinned(expected) == "/sys/Bool/Holds(b <= a)"),
    );
}

// A literal dispatch is `Nat`'s alone. Over a float it would read IEEE's `==`, which takes `-0.0` at the arm `0` while that arm's case states the scrutinee is the literal, `+0.0`: an arm proving `Eq()(f, +0.0)` would prove it of `-0.0`. The control dispatches a `Nat`.
#[test]
fn a_literal_dispatch_reads_no_float() {
    control(
        r#"
        use /std/{Nat};

        let tag(f: Nat) -> Nat = match f | 0 => 1 | _ => 0 end;

        /std/print(Nat/to_str(tag(0)))
        "#,
        b"1",
    )
    .refused(&[
        ("{Nat}", "{Nat, Flt}"),
        ("tag(f: Nat)", "tag(f: Flt)"),
        ("tag(0)))", "tag(-0.0)))"),
    ])
    .by_elaborator_in(
        "/tag",
        |error| matches!(error, Error::NotNatType { head_type } if pinned(head_type) == "Flt"),
    );
}

// `A` and `B` are one group whose parameters share the name `n`, a `Nat` in one and a `Bool` in the other, and `mkA`'s payload states `n == 1` of its own family's. An `A(0)` holds a `Holds(0 == 1)`, so `forged`'s `qed` would fit only where the case were read at its sibling's parameter, or at another instance's, and `f` would hand it on at `False`. The kernel puts the same proposition as Core text under this name, in `curios-cert`'s `recheck::case_tests`. The control is an `A(1)`, which holds a `Holds(1 == 1)` that `qed` inhabits, and its arm runs.
#[test]
fn a_case_reads_its_own_familys_parameter_in_a_group() {
    control(
        r#"
        use /std/{Nat, Bool};
        use /std/Bool/{False, True, Holds};

        induct A(n: Nat): Type
        | mkA(h: Holds(n == 1), other: B(true))
        and B(n: Bool): Type
        | mkB(h: Holds(n), other: A(0))
        | base()
        end

        let f(x: A(1)) -> Nat = match x | mkA(h, _) => 1 end;

        let forged: Nat = f(A/mkA(True/qed(), B/base()));

        /std/print(Nat/to_str(forged))
        "#,
        b"1",
    )
    .refused(&[
        ("f(x: A(1)) -> Nat", "f(x: A(0)) -> False"),
        ("mkA(h, _) => 1", "mkA(h, _) => h"),
        ("let forged: Nat", "let forged: False"),
        ("Nat/to_str(forged)", "\"FORGED\""),
    ])
    .by_elaborator_in("/forged", |error| {
        matches!(error, Error::TypeMismatch { .. })
    });
}

// A guard's spelling settled inside an arm of a split on an expression reads that arm's equation nowhere: under `g(c) = true` the operands are units, and `one` asks for the guard's spelling there, which is the stuck match whatever the arm knows, so after the split nothing says `k(0)`. Nothing before the split names `k`, so the guard is first settled inside it. `curios-cert`'s `recheck::equation_tests` hold the kernel to the same term. The control claims nothing past the split.
#[test]
fn a_guard_settled_under_an_inner_arms_equation_answers_nothing_after_that_arm() {
    control(r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};

        let F(c: Bool) -> Type = match c | true => {} | false => Nat end;
        let leak(g: (Bool) -> Bool, c: Bool, h: (F(g(c))) -> Nat, k: (Nat) -> Bool, p: F(g(c)), q: F(g(c))) -> True =
            match (match h(p) == h(q) | true => k(0) | false => k(1) end)
            | true =>
                let split: {} = match g(c) | false => () | true => (let one: Holds(h(p) + h(q) == h(p) + h(p)) = True/qed(); ()) end;
                let after: True = True/qed();
                True/qed()
            | false => True/qed()
            end;

        /std/print(Nat/to_str(0))
        "#, b"0")
        .refused(&[("let after: True =", "let after: Holds(k(0)) =")])
        .by_elaborator_in("/leak", |error| matches!(error, Error::TypeMismatch { .. }));
}

// A refused arm is not taken for one no reduction takes on the strength of its sibling's equation: under the `false` arm's `g(e) = false` the guard's proof is `False` and `g(e)` is forced to `false`, and the `true` arm, which hands `True/qed()` on at `False`, is refused. `curios-cert`'s `recheck::equation_tests` hold the kernel to the same program, where a refused arm can be withdrawn. The control states what the `true` arm proves.
#[test]
fn a_refused_arm_is_not_withdrawn_by_its_siblings_equation() {
    control(
        r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, False, Holds};

        let w(g: (Bool) -> Bool, e: Bool, h: Holds(g(e))) -> Holds(g(e)) =
            match g(e)
            | false => (let k: Holds(g(e) == false) = True/qed(); match h end)
            | true => True/qed()
            end;

        /std/print(Nat/to_str(0))
        "#,
        b"0",
    )
    .refused(&[("-> Holds(g(e)) =", "-> False =")])
    .by_elaborator_in("/w", |error| matches!(error, Error::TypeMismatch { .. }));
}

// A guard an inner arm restated is settled in that arm and goes with it: at `c = true` the split re-types `p` and `q` at `{}`, where `h(p) == h(q)` and so the guard is `k(0)`, and settling `inside` asks for the guard's spelling there. The recorded guard steps aside while the split's arm stands, settled nowhere in it, so after the split `after` reads the guard with `c` open, where it is the stuck match and not `k(0)`. Settled inside the arm, the recorded guard's spelling outlived it, and `forged`, closed at `P := (k) => Holds(k(0))`, `c := false` and `k := (n) => n == 1`, stood at `False`. The soundness board's fixed ticket `elimination/a_guards_reduced_spelling_is_settled_under_one_case_of_a_split_inside_its_arm_and_answers_outside_that_case` holds the same program. The control: inside the split's arm the guard's restatement settles under the arm's re-typing, where `p` and `q` stand at `{}` and the guard is `k(0)`, so `inside` proves `k(0)` there, and after the split `after` claims only what holds anyway.
#[test]
fn a_guard_restated_in_an_inner_arm_is_settled_nowhere_in_it() {
    control(r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, False, Holds};

        let F(c: Bool) -> Type = match c | true => {} | false => Nat end;
        let f(P: ((Nat) -> Bool) -> Prop, c: Bool, h: (F(c)) -> Nat, k: (Nat) -> Bool, p: F(c), q: F(c), into: (Holds(k(0) || true)) -> P(k)) -> (Holds(match h(p) == h(q) | true => k(0) | false => k(1) end)) -> P(k) =
            match (match h(p) == h(q) | true => k(0) | false => k(1) end): (b) => (Holds(b)) -> P(k)
            | true => (_: Holds(true)) =>
                let split: {} = match c | false => () | true => (let inside: Holds(k(0)) = True/qed(); ()) end;
                let after: Holds(k(0) || true) = True/qed();
                into(after)
            | false => (g: Holds(false)) => match g end
            end;

        let forged = f;

        /std/print(Nat/to_str(0))
        "#, b"0")
        .refused(&[
            ("into: (Holds(k(0) || true)) -> P(k)", "into: (Holds(k(0))) -> P(k)"),
            ("let inside: Holds(k(0)) =", "let inside: Holds(k(0) || true) ="),
            ("let after: Holds(k(0) || true) =", "let after: Holds(k(0)) ="),
            ("let forged = f;", "let forged: False = f((k: (Nat) -> Bool) => Holds(k(0)), false, (n: Nat) => n, (n: Nat) => n == 1, 0, 1, (x: Holds(0 == 1)) => x)(True/qed());"),
        ])
        .by_elaborator_in("/f", |error| matches!(error, Error::TypeMismatch { .. }));
}

// A guard restated in the arm of one split is settled in no arm of a second split nested inside it, and the recorded guard stands aside through both: after the splits `after` reads the guard with `c` open, where it is the stuck match and not `k(0)`. The control states `after` at `Holds(k(0) || true)`, which holds at every value.
#[test]
fn a_guard_restated_in_a_nested_split_is_settled_nowhere_in_either_arm() {
    control(
        r#"
        use /std/{Nat, Bool};
        use /std/Bool/{True, Holds};

        let F(c: Bool) -> Type = match c | true => {} | false => Nat end;
        let f(P: ((Nat) -> Bool) -> Prop, c: Bool, d: Bool, h: (F(c)) -> Nat, k: (Nat) -> Bool, p: F(c), q: F(c), into: (Holds(k(0) || true)) -> P(k)) -> (Holds(match h(p) == h(q) | true => k(0) | false => k(1) end)) -> P(k) =
            match (match h(p) == h(q) | true => k(0) | false => k(1) end): (b) => (Holds(b)) -> P(k)
            | true => (_: Holds(true)) =>
                let split: {} = match c | false => () | true => (match d | true => (let inside: Holds(k(0) || true) = True/qed(); ()) | false => () end) end;
                let after: Holds(k(0) || true) = True/qed();
                into(after)
            | false => (g: Holds(false)) => match g end
            end;

        /std/print("ok")
        "#,
        b"ok",
    )
    .refused(&[
        ("use /std/Bool/{True, Holds};", "use /std/Bool/{True, False, Holds};"),
        ("into: (Holds(k(0) || true)) -> P(k)", "into: (Holds(k(0))) -> P(k)"),
        ("let after: Holds(k(0) || true)", "let after: Holds(k(0))"),
        (
            "/std/print(\"ok\")",
            "let forged: False = f((k: (Nat) -> Bool) => Holds(k(0)), false, true, (n: Nat) => n, (n: Nat) => n == 1, 0, 1, (x: Holds(0 == 1)) => x)(True/qed());\n\n        /std/print(\"ok\")",
        ),
    ])
    .by_elaborator_in("/f", |error| matches!(error, Error::TypeMismatch { .. }));
}
