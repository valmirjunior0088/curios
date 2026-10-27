//! What an arm learns of a let-bound scrutinee, through its definition and a chain of them, and a guard meeting itself through a definition.

use {crate::tests::run, curios_utilities::fill};

// === A scrutinee bound by `let` ===============================================
//
// The kernel reads a `let`-bound scrutinee by value, so what it matches on is the binding's definition, and it refines that. The elaborator binds the name to its definition and refines the name, and a metavariable solved against the name is stored as the definition — so refining a local binding refines its definition too, in both arms, whatever the scrutinee's type, or `refl`'s implicit below, `x`, would stay apart from `xp + 1`.

#[test]
fn a_let_bound_scrutinee_is_refined_through_its_definition() {
    let source = r#"
        use /std/{Nat, Eq};
        let f(x: Nat) -> Nat =
            let y: Nat = x;
            match y
            | 0 => let _zero: Eq()(y, 0) = Eq/refl(); 0
            | xp + 1 =>
                let _same: Eq()(y, y) = Eq/refl();
                let _successor: Eq()(y, xp + 1) = Eq/refl();
                let _definition: Eq()(x, xp + 1) = Eq/refl(@Nat, @(xp + 1));
                xp
            end;
        /std/print(Nat/to_str(f(5)))
        "#;
    assert_eq!(run(source), b"4");
}

#[test]
fn a_let_bound_boolean_scrutinee_is_refined_through_its_definition() {
    let source = r#"
        use /std/{Bool, Eq, Str};
        let f(b: Bool) -> Str =
            let c: Bool = b;
            match c
            | true => let _: Eq()(c, true) = Eq/refl(); "yes"
            | false => let _: Eq()(c, false) = Eq/refl(); "no"
            end;
        /std/print(f(true))
        "#;
    assert_eq!(run(source), b"yes");
}

// The ambient path, where the arm is checked against the expected type with the scrutinee standing for its case.
#[test]
fn a_let_bound_inductive_scrutinee_is_refined_through_its_definition() {
    let source = r#"
        use /std/{Nat, Eq, Option};
        let f(o: Option(Nat)) -> Nat =
            let p: Option(Nat) = o;
            match p
            | some(v) => let _: Eq()(p, Option/some(v)) = Eq/refl(); v
            | none() => 0
            end;
        /std/print(Nat/to_str(f(Option/some(7))))
        "#;
    assert_eq!(run(source), b"7");
}

#[test]
fn a_chain_of_lets_is_refined_to_its_end() {
    let source = r#"
        use /std/{Nat, Eq};
        let f(x: Nat) -> Nat =
            let y: Nat = x;
            let z: Nat = y;
            match z
            | 0 => 0
            | zp + 1 =>
                let _: Eq()(z, z) = Eq/refl();
                let _: Eq()(x, zp + 1) = Eq/refl(@Nat, @(zp + 1));
                zp
            end;
        /std/print(Nat/to_str(f(3)))
        "#;
    assert_eq!(run(source), b"2");
}

// A definition that is itself a stuck expression is refined under its own spelling, as the kernel's case equation for the substituted scrutinee is.
#[test]
fn a_let_bound_expression_scrutinee_is_refined_through_its_definition() {
    let source = r#"
        use /std/{Nat, Eq};
        let count(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => count(k) + 1 end;
        let f(x: Nat) -> Nat =
            let y: Nat = count(x);
            match y
            | 0 => 0
            | yp + 1 => let _: Eq()(y, y) = Eq/refl(); yp
            end;
        /std/print(Nat/to_str(f(3)))
        "#;
    assert_eq!(run(source), b"2");
}

// A guard meets itself inside a definition that spells it over a `let`. `outside` binds `n` to `Byte/to_nat(c)` and guards on `Bool/not(Nat/in_range(n, lo, hi))`; the arm refines the same guard spelled over `Byte/to_nat(c)`. The reducer substitutes a `let` as the kernel does, so the unfolded guard is the arm's own spelling rather than one over a fresh name the arm's key never mentions.
#[test]
fn a_guard_meets_itself_through_a_definition_that_binds_its_operand() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Byte, Bool, Eq};
        let outside(c: Byte, lo: Nat, hi: Nat) -> Bool =
            let n = Byte/to_nat(c);
            choose | Bool/not(Nat/in_range(n, lo, hi)) => false | _ => true end;
        let _inside(c: Byte, lo: Nat, hi: Nat) -> Nat =
            match Bool/not(Nat/in_range(Byte/to_nat(c), lo, hi))
            | true => 0
            | false => let _: Eq()(outside(c, lo, hi), true) = Eq/refl(); 1
            end;
        /std/print("ok")
        "#),
        b"ok"
    );
}

// A guard written over the arm's own `let` meets the same guard reached through a definition that spells the value. The arm binds `n` to `Byte/to_nat(c)` and guards on `Bool/not(Nat/in_range(n, lo, hi))`; `outside` spells that guard over `Byte/to_nat(c)` itself. The kernel substitutes `n` before it records the arm's equation, and the elaborator records the equation under the kernel's spelling too: recorded over the name alone, its lookup filter would never let the probe, which names `c`, reach the key, and a settlement of the key would unfold `n` only where reduction touched it, leaving it named inside the branch `Bool/not`'s match keeps.
#[test]
fn a_guard_over_a_let_meets_itself_spelled_over_the_value() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Byte, Bool, Eq};
        let outside(c: Byte, lo: Nat, hi: Nat) -> Bool =
            choose | Bool/not(Nat/in_range(Byte/to_nat(c), lo, hi)) => false | _ => true end;
        let _inside(c: Byte, lo: Nat, hi: Nat) -> Nat =
            let n = Byte/to_nat(c);
            match Bool/not(Nat/in_range(n, lo, hi))
            | true => 0
            | false => let _: Eq()(outside(c, lo, hi), true) = Eq/refl(); 1
            end;
        /std/print("ok")
        "#),
        b"ok"
    );
}

// Both sides of the guard spelled over a `let`: the arm's own, and the one inside `outside`, which is how `/std/Str/step` binds the byte it reads and how a proof walking its guards names it. Each side's `let` is met the way the kernel meets it — the definition's by substitution, the arm's by the equation's recorded kernel spelling — so they meet.
#[test]
fn a_guard_over_a_let_meets_a_definition_that_binds_its_own() {
    assert_eq!(
        run(r#"
        use /std/{Nat, Byte, Bool, Eq};
        let outside(c: Byte, lo: Nat, hi: Nat) -> Bool =
            let n = Byte/to_nat(c);
            choose | Bool/not(Nat/in_range(n, lo, hi)) => false | _ => true end;
        let _inside(c: Byte, lo: Nat, hi: Nat) -> Nat =
            let m = Byte/to_nat(c);
            match Bool/not(Nat/in_range(m, lo, hi))
            | true => 0
            | false => let _: Eq()(outside(c, lo, hi), true) = Eq/refl(); 1
            end;
        /std/print("ok")
        "#),
        b"ok"
    );
}

// A guard over a local definition answers a term written over the definition's value. The kernel reads the guard by value, so `Nat/in_range(m, 240, 244)` is its scrutinee as the kernel holds it; the elaborator's key names `n`, and it meets the term at the guard's reduced spelling, where the filter in front of the equation reads the guard as the kernel spells it. The control is the guard written over `m` itself. `curios-elab`'s `reduce::question_tests` holds the filter under this name.
//
// Mutation-checked: with the elaborator's filter reading the guard as written, the first program is refused at `True/qed()`, whose expected `Holds(Nat/in_range(m, 240, 244))` is no `True` there.
#[test]
fn a_guard_over_a_local_definition_answers_a_term_over_its_value() {
    for guard in [
        "let n = m + 0; match Nat/in_range(n, 240, 244)",
        "match Nat/in_range(m, 240, 244)",
    ] {
        let source = fill!(
            r#"
        use /std/{Nat, Str};
        use /std/Bool/{Holds, True};

        let ranged(m : Nat) -> Str =
            ${guard}
            | true =>
                let _ : Holds(240 <= m) = Nat/le/of_in_range(m, 240, 244, True/qed()).low;
                "inside"
            | false => "outside"
            end;

        /std/print(ranged(242))
        "#,
            guard
        );

        assert_eq!(run(&source), b"inside", "{guard}");
    }
}
