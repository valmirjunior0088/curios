//! An arm's case equation, held to the arm that records it: no equation answers a term it was not recorded for — not one an arm's restatement left naming no local, not one an inner arm's equation reduces it to, not one read past the arm that restated it or the arm that assumed it, and not the scrutinee of a refused sibling — each a module the elaborator would not build, written as Core text and put to the kernel with nothing in scope, declaring the propositions it uses.

use {super::test_support::*, crate::*, curios_common::fill};

/// `G(x) <= 2` names `x`, and the `3` arm of `match x` restates it as `G(3) <= 2`, which names no local and is not recorded: the arm, where `x` is `3` and the guard says `G(3) <= 2` is `true`, is one no reduction takes, since `G(3) <= 2` is `false`. Inside it `Holds(G(3) <= 2)` is asked of `qed`.
const A_RESTATEMENT_NAMING_NO_LOCAL: &str = r#"
let /G : (n: #NatType) -> #NatType =
    (n: #NatType) =>
        n;
let /w : {
    f: (x: #NatType) -> #BoolType,
    p: /std/Bool/Holds(#NatLe(/G(3), 2))
} =
    (
        (x: #NatType) =>
            match #NatLe(/G(x), 2): (b) => #BoolType
            | false =>
                false
            | true =>
                match x: (y) => #BoolType
                | 3 =>
                    ((k: /std/Bool/Holds(#NatLe(/G(3), 2))) =>
                        true)(/std/Bool/True/qed())
                | _ =>
                    true
                end
            end,
        /std/Bool/True/qed()
    );
/w.1
"#;

// No arm's restatement answers a closed comparison, so the dead arm leaves nothing behind it: it is refused and withdrawn as one reduction never takes, and `/w`'s second component, checked after it, finds `Holds(G(3) <= 2)` uninhabited. Remembered for the declaration as the restatement would answer it inside the arm, `G(3) <= 2` would be `true` there, and `/w.1`, which the module closes with and which stands at `False`, would be a proof of it: the closing term is admitted, and `/w`'s refusal is all that stands between the module and `False`. `closed_computation_tests` holds the same of a dispatch's resolved spelling.
#[test]
fn an_arms_restatement_naming_no_local_leaves_nothing_the_declaration_reads_after_it() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_RESTATEMENT_NAMING_NO_LOCAL}",
        PROPOSITIONS,
        A_RESTATEMENT_NAMING_NO_LOCAL
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(w), error: Error::Mismatch { .. } }] if *w == global("w")
        ),
        "{verdicts:?}"
    );
}

/// The board's guard settled by the unit type, with the split on `g(c)`, an expression, where the ticket split on `c`: the inner arm records `g(c) = true`, solves nothing and re-types nothing, so the guard is not restated there. Inside it `F(g(c))` reduces to `{}`, `h(p)` and `h(q)` are one atom, and `u` reduces a stuck form naming the guard's locals, which asks for the guard's reduced spelling.
const A_GUARD_SETTLED_UNDER_AN_INNER_ARMS_EQUATION: &str = r#"
((g: (b: #BoolType) -> #BoolType, c: #BoolType, h: (x: match g(c): (x2) => Type
        | false =>
            #NatType
        | true =>
            {}
        end) -> #NatType, k: (n: #NatType) -> #BoolType, p: match g(c): (x3) => Type
    | false =>
        #NatType
    | true =>
        {}
    end, q: match g(c): (x4) => Type
    | false =>
        #NatType
    | true =>
        {}
    end, guard: /std/Bool/Holds(match #NatEql(h(p), h(q)): (x5) => #BoolType
        | false =>
            k(1)
        | true =>
            k(0)
        end)) =>
    match match #NatEql(h(p), h(q)): (x9) => #BoolType
    | false =>
        k(1)
    | true =>
        k(0)
    end: (x6) => /std/Bool/Holds(k(0))
    | false =>
        match guard: (x7) => /std/Bool/Holds(k(0))
        end
    | true =>
        let w:
            #{node = InductType}
            /std/Bool/True() =
            match g(c): (x8) =>
                #{node = InductType}
                /std/Bool/True()
            | false =>
                #{node = Variant}
                /std/Bool/True/qed()
            | true =>
                let u: /std/Bool/Holds(#NatEql(#NatAdd(h(p), h(q)), #NatAdd(h(p), h(p)))) =
                    #{node = Variant}
                    /std/Bool/True/qed();
                #{node = Variant}
                /std/Bool/True/qed()
            end;
        #{node = Variant}
        /std/Bool/True/qed()
    end)(
    (b2: #BoolType) => b2,
    false,
    (n2: #NatType) => n2,
    (n3: #NatType) => #NatEql(n3, 1),
    0,
    1,
    #{node = Variant}
    /std/Bool/True/qed()
)
"#;

// Settled with the inner arm's equation in force, the guard's spelling would be `k(0)`, kept with the guard past the arm, and the `qed` after the split would hold at `Holds(k(0))`, which at `g` the identity and `c := false` is `Holds(false)`: the closing term would be admitted at `False`. A settlement withholds the equation it settles and every one inside it, so the spelling is the stuck match and that `qed` is refused. `curios`'s `tests::rules::arm_tests` holds the elaborator to the same term.
//
// Mutation-checked at `Scope::in_force_innermost_first`: with a settlement withholding the equation it settles alone, and not those inside it, the module is admitted.
#[test]
fn a_guard_settled_under_an_inner_arms_equation_answers_nothing_after_that_arm() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_GUARD_SETTLED_UNDER_AN_INNER_ARMS_EQUATION}",
        PROPOSITIONS,
        A_GUARD_SETTLED_UNDER_AN_INNER_ARMS_EQUATION
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict {
                name: None,
                error: Error::Mismatch { .. }
            }]
        ),
        "{verdicts:?}"
    );
}

/// `Two(n)` has two cases, both at `0`. The scrutinee `w(())` is no variable, so the `a` arm records `w(()) = a`; its index inversion solves `n := 0`, which `w`'s type names, so the equation just assumed is restated at the solution in the same arm, and `inside` reads it. After the match `after` asks the same proposition, `n` open and `w(())` no case.
const AN_ARMS_OWN_EQUATION_RESTATED: &str = r#"
#{absent = [/Two/a, /Two/b]}
induct /Two: (n: #NatType) -> pub Type
| a(): (0)
| b(): (0)
end
((n: #NatType, w: (u: {}) ->
        #{node = InductType}
        /Two()(n)) =>
    let split: #BoolType =
        match w(()): (n2, x) => #BoolType
        | a() =>
            let inside: /std/Bool/Holds(match w(()): (n3, y) => #BoolType
                | a() =>
                    true
                | b() =>
                    false
                end) =
                #{node = Variant}
                /std/Bool/True/qed();
            true
        | b() =>
            false
        end;
    let after: /std/Bool/Holds(match w(()): (n3, y) => #BoolType
        | a() =>
            true
        | b() =>
            false
        end) =
        #{node = Variant}
        /std/Bool/True/qed();
    after)(
    0,
    (u2: {}) =>
        #{node = Variant}
        /Two/b()
)
"#;

// Neither the arm's equation nor its restatement outlives the arm, nor what reduction remembered of a term under them: `after` is refused. Kept past the arm, either would type `after`, and at `n := 0`, `w := (_) => b`, that is `Holds(false)`, so the closing term would be admitted at `False`.
#[test]
fn an_arms_own_equation_restated_at_its_index_solution_answers_nothing_after_the_arm() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${AN_ARMS_OWN_EQUATION_RESTATED}",
        PROPOSITIONS,
        AN_ARMS_OWN_EQUATION_RESTATED
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict {
                name: None,
                error: Error::Mismatch { .. }
            }]
        ),
        "{verdicts:?}"
    );
}

/// The guard is the match on `h(p) == h(q)`, `a < b` at `true` and `b < a` at `false`, and its `false` arm assumes it `false`. The `true` arm of the split on `c` re-types `p` and `q` at `{}`, where `h(p)` and `h(q)` are one atom, so the guard, restated there, settles to `a < b`; the readers then hold `b <= a`, that spelling's dual, `true`, and `inside` reads it. After the split `after` asks the same.
const A_READERS_ANSWER_UNDER_A_SPLIT: &str = r#"
((c: #BoolType, h: (x: match c: (x2) => Type
        | false =>
            #NatType
        | true =>
            {}
        end) -> #NatType, a: #NatType, b: #NatType, p: match c: (x3) => Type
    | false =>
        #NatType
    | true =>
        {}
    end, q: match c: (x4) => Type
    | false =>
        #NatType
    | true =>
        {}
    end, guard: /std/Bool/Holds(#BoolXor(match #NatEql(h(p), h(q)): (x5) => #BoolType
        | false =>
            #NatLt(b, a)
        | true =>
            #NatLt(a, b)
        end, true))) =>
    match match #NatEql(h(p), h(q)): (x9) => #BoolType
    | false =>
        #NatLt(b, a)
    | true =>
        #NatLt(a, b)
    end: (x6) => /std/Bool/Holds(#NatLe(b, a))
    | false =>
        let w:
            #{node = InductType}
            /std/Bool/True() =
            match c: (x8) =>
                #{node = InductType}
                /std/Bool/True()
            | false =>
                #{node = Variant}
                /std/Bool/True/qed()
            | true =>
                let inside: /std/Bool/Holds(#NatLe(b, a)) =
                    #{node = Variant}
                    /std/Bool/True/qed();
                #{node = Variant}
                /std/Bool/True/qed()
            end;
        let after: /std/Bool/Holds(#NatLe(b, a)) =
            #{node = Variant}
            /std/Bool/True/qed();
        after
    | true =>
        match guard: (x7) => /std/Bool/Holds(#NatLe(b, a))
        end
    end)(
    false,
    (n2: #NatType) => n2,
    0,
    1,
    0,
    1,
    #{node = Variant}
    /std/Bool/True/qed()
)
"#;

// What the readers answered of a restated guard inside the split goes with the restatement: after the split `c` is open, the guard's spelling is the stuck match, `b <= a` is nothing the equation answers, and `after` is refused. Kept past the split, the readers' answer would type `after`, and at `c := false`, `a := 0`, `b := 1`, where the guard is `1 < 0`, `false`, `Holds(1 <= 0)` is `False`, so the closing term would be admitted at `False`.
//
// Mutation-checked at `Scope::restate`: with no equation restated, the module is admitted, the recorded guard settling inside the split.
#[test]
fn a_readers_answer_through_a_restated_guard_answers_nothing_after_the_split() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_READERS_ANSWER_UNDER_A_SPLIT}",
        PROPOSITIONS,
        A_READERS_ANSWER_UNDER_A_SPLIT
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict {
                name: None,
                error: Error::Mismatch { .. }
            }]
        ),
        "{verdicts:?}"
    );
}

/// The guard is the match on `h(p) == h(q)`, `k(m(r))` at `true` and `k(m2)` at `false`, and its `true` arm assumes it `true`. The `true` arm of the split on `c` re-types `p` and `q` at `{}`, so the guard, restated there, settles to `k(m(r))`; `inside` asks `k(m(s))`, which no reader takes apart and which the kernel's conversion holds one with `k(m(r))`, `r` and `s` being proofs of one proposition, so the restatement answers it. After the split `after` asks the same.
const A_CONVERSIONS_ANSWER_UNDER_A_SPLIT: &str = r#"
((c: #BoolType, h: (x: match c: (x2) => Type
        | false =>
            #NatType
        | true =>
            {}
        end) -> #NatType, k: (n: #BoolType) -> #BoolType, m: (t:
        #{node = InductType}
        /std/Bool/True()) -> #BoolType, m2: #BoolType, r:
        #{node = InductType}
        /std/Bool/True(), s:
        #{node = InductType}
        /std/Bool/True(), p: match c: (x3) => Type
    | false =>
        #NatType
    | true =>
        {}
    end, q: match c: (x4) => Type
    | false =>
        #NatType
    | true =>
        {}
    end, guard: /std/Bool/Holds(match #NatEql(h(p), h(q)): (x5) => #BoolType
        | false =>
            k(m2)
        | true =>
            k(m(r))
        end)) =>
    match match #NatEql(h(p), h(q)): (x9) => #BoolType
    | false =>
        k(m2)
    | true =>
        k(m(r))
    end: (x6) => /std/Bool/Holds(k(m(s)))
    | false =>
        match guard: (x7) => /std/Bool/Holds(k(m(s)))
        end
    | true =>
        let w:
            #{node = InductType}
            /std/Bool/True() =
            match c: (x8) =>
                #{node = InductType}
                /std/Bool/True()
            | false =>
                #{node = Variant}
                /std/Bool/True/qed()
            | true =>
                let inside: /std/Bool/Holds(k(m(s))) =
                    #{node = Variant}
                    /std/Bool/True/qed();
                #{node = Variant}
                /std/Bool/True/qed()
            end;
        let after: /std/Bool/Holds(k(m(s))) =
            #{node = Variant}
            /std/Bool/True/qed();
        after
    end)(
    false,
    (n2: #NatType) => n2,
    (b2: #BoolType) => b2,
    (t2:
        #{node = InductType}
        /std/Bool/True()) => false,
    true,
    #{node = Variant}
    /std/Bool/True/qed(),
    #{node = Variant}
    /std/Bool/True/qed(),
    0,
    1,
    #{node = Variant}
    /std/Bool/True/qed()
)
"#;

// What the kernel's conversion answered of a restated guard inside the split, asked from inside reduction, goes with the restatement: `after` is refused. Kept past the split, the answer would type `after`, and at `c := false`, `p := 0`, `q := 1`, `k` the identity, `m(_) := false`, `m2 := true`, where the guard is `true`, `Holds(k(m(s)))` is `Holds(false)`, so the closing term would be admitted at `False`.
//
// Mutation-checked at `Scope::restate`: with no equation restated, the module is admitted, the recorded guard settling inside the split.
#[test]
fn a_conversions_answer_through_a_restated_guard_answers_nothing_after_the_split() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_CONVERSIONS_ANSWER_UNDER_A_SPLIT}",
        PROPOSITIONS,
        A_CONVERSIONS_ANSWER_UNDER_A_SPLIT
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict {
                name: None,
                error: Error::Mismatch { .. }
            }]
        ),
        "{verdicts:?}"
    );
}

/// The `false` arm of `match g(e)` records `g(e) = false`, under which `h : Holds(g(e))` is `False` and is taken apart by an empty match, and `k` forces `g(e)` as a comparison's operand, the reduction a refused arm is withdrawn by, so the arm reduces `g(e)` to `false` through its equation at both demands. The `true` arm hands `True/qed()` on at `False`.
const A_REFUSED_ARM_BESIDE_ITS_SIBLINGS_EQUATION: &str = r#"
let /w : (g: (b: #BoolType) -> #BoolType, e: #BoolType, h: /std/Bool/Holds(g(e))) -> /std/Bool/False =
    (g: (b: #BoolType) -> #BoolType, e: #BoolType, h: /std/Bool/Holds(g(e))) =>
        match g(e): (b) => /std/Bool/False
        | false =>
            let k: /std/Bool/Holds(#BoolEql(g(e), false)) =
                /std/Bool/True/qed();
            match h: (z) => /std/Bool/False
            end
        | true =>
            /std/Bool/True/qed()
        end;
/w((b: #BoolType) => b, true, /std/Bool/True/qed())
"#;

// A refused arm is withdrawn where its scrutinee computes to another case, and `g(e)` is computed with the `true` arm's bracket and its sibling's closed, so it stands stuck and the `true` arm's refusal stands. Read where the `false` arm's equation still answered it, or off a reduct remembered under that equation, `g(e)` would be `false`, the `true` arm would be withdrawn untyped, and the closing term, `/w` at the identity, would be admitted at `False`: `/w`'s refusal is all that stands between the module and `False`. `curios`'s `tests::rules::arm_tests` holds the elaborator to the same program.
#[test]
fn a_refused_arm_is_not_withdrawn_by_its_siblings_equation() {
    let verdicts = core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${A_REFUSED_ARM_BESIDE_ITS_SIBLINGS_EQUATION}",
        PROPOSITIONS,
        A_REFUSED_ARM_BESIDE_ITS_SIBLINGS_EQUATION
    ));

    assert!(
        matches!(
            &verdicts[..],
            [Verdict { name: Some(w), error: Error::Mismatch { .. } }] if *w == global("w")
        ),
        "{verdicts:?}"
    );
}
