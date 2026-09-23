//! What an arm learns from its scrutinee, and the equations it may not assume.

use {
    crate::tests::{error, run, run_text},
    curios_runtime::MockHost,
};

// A refinement key is stored at the arm and probed wherever the scrutinee is mentioned again, so the two spellings have to compare equal. They did not when the scrutinee carried an *inferred* metavariable: `Pred/test(t, b)` elaborates to `(?w).0(t, b)`, and a second occurrence mints its own `?w'`, so two terms solved to the same witness keyed differently and the arm silently refined nothing. Solved metavariables are now materialized into the key, which is what makes the two spellings one.
//
// Three constraints shape the scrutinee, and they pull against each other. It must carry a metavariable, or there is nothing to materialize. It must not reduce away, or the store is never reached — hence a method whose body eliminates the *symbolic* `b`. And its head must be one the kernel's refinement store reads, which a function parameter stopped being once `an_effect_behind_a_function_parameter_does_not_refine` landed. A concept dispatch is all three at once, and it is what `/std/Str/Valid/cont_len` refines on in production rather than a shape invented here.
//
// Mutation-checked: dropping `zonk_solved_term_metas` from `canonical_scrutinee` refuses this program, with `p`'s expected type still reading the unrefined method body.
#[test]
fn an_inferred_implicit_does_not_break_a_refinement_key() {
    let source = r#"
        use /std/{Eq, Bool, Nat, Str};

        concept Pred(A : Type) : pub Type {
            test(A, Bool) -> Bool,
        }

        satisfy Pred(Nat) {
            test(n, b) = match b | true => true | false => false end,
        }

        let refined(t : Nat, b : Bool) -> Str =
            match Pred/test(t, b)
            | true =>
                let p : Eq(true, Pred/test(t, b)) = Eq/refl();
                "refined"
            | false => "unrefined"
            end;

        /std/print(refined(7, true))
        "#;

    assert_eq!(run(source), b"refined");
}

// A refinement on a boolean connective reaches every spelling of the scrutinee that reduces to it. `x && g(7)` is the scrutinee; the occurrence is spelled `x && h(7)`, with `h` a different function folding to the same `true`, so the written key misses and the escalation has to match the two through their canonical forms — every operand reduced, on both sides. That is the form the elaborator's `refined_after_fold` and the kernel's `refined_reduct` both bring a probed value to, which is what keeps them reaching the same occurrences now that `&&` leaves its right operand as written behind a stuck left. Before the connectives were tagged in `Term::head_key`, an operator-spelled scrutinee registered a key nothing could look up, and not even `x && g(7)` itself refined — a gap that comment recorded.
//
// Two occurrences, two routes to the same intrinsic. `x && h(7)` arrives at each reducer as the witness projection the scrutinee was written as; `Bool/and(x, h(7))` arrives under the wrapper's own head, which no key is gated on, and becomes the intrinsic only once the wrapper unfolds. In the elaborator both are decided at the probe *before* decomposition, which re-runs on every continued term and canonicalizes on a miss; in the kernel both are decided at the stuck reduct, brought to operand-canonical form by `refined_reduct`.
//
// Both checkers run this. Mutation-checked: dropping `BoolAnd` from `head_key` refuses it at `p`, and comparing the kernel's probed value uncanonicalized refuses it at `p` too. The elaborator's `refined_after_fold` canonicalization is not what either occurrence rests on — the probe before decomposition reaches them first — and is kept for the fold that changes a spelling.
#[test]
fn a_boolean_refinement_reaches_an_occurrence_spelled_differently_on_its_right() {
    let source = r#"
        use /std/{Eq, Bool, Nat, Str};

        let g(n : Nat) -> Bool = n == 7;
        let h(n : Nat) -> Bool = n == 7;

        let refined(x : Bool) -> Str =
            match x && g(7)
            | true =>
                let p : Eq(x && h(7), true) = Eq/refl();
                let q : Eq(Bool/and(x, h(7)), true) = Eq/refl();
                "refined"
            | false => "unrefined"
            end;

        /std/print(refined(true))
        "#;

    assert_eq!(run(source), b"refined");
}

// An arm is opened at the forced constructor's own payload, so a matched payload reduces to the value that constructor carried. Opening it at projections of the scrutinee instead reduces to the same value but leaves a residual Core cannot type — `Proj` has no rule for an inductive — and the difference is invisible until such a residual reaches conversion as a metavariable solution candidate, where re-validation refuses it as `NotATuple`: a hard verdict that fails the goal outright rather than parking it. A parameterized family is what keeps the projection from reducing away first, and a phantom parameter suffices, so this reached `/std/Option/map` and every container whose operation returns what a match arm bound.
#[test]
fn a_matched_payload_converts_against_the_value_it_carried() {
    let source = r#"
        use /std/{Nat, Option, Eq};

        induct L(V : Type) : Type
        | nil()
        | cons(V, L(V))
        end

        let head_opt(@V : Type, l : L(V)) -> Option(V) =
            match l | nil() => Option/none() | cons(h, t) => Option/some(h) end;

        let carried : Eq(head_opt(L/cons(7, L/nil())), Option/some(7)) = Eq/refl();

        /std/print("carried")
        "#;

    assert_eq!(run(source), b"carried");
}

// The companion to the arm binding above: opening an arm at the constructor's payload must not make conversion accept an equation that is merely false.
#[test]
fn a_matched_payload_still_refuses_a_false_equation() {
    let source = r#"
        use /std/{Nat, Option, Eq};

        induct L(V : Type) : Type
        | nil()
        | cons(V, L(V))
        end

        let head_opt(@V : Type, l : L(V)) -> Option(V) =
            match l | nil() => Option/none() | cons(h, t) => Option/some(h) end;

        let carried : Eq(head_opt(L/cons(7, L/nil())), Option/some(8)) = Eq/refl();

        /std/print("carried")
        "#;

    let error = error(source);
    assert!(error.contains("type mismatch"), "unexpected error: {error}");
}

/// An immediate-encoded arm binds its payload through a read of its own rather than aliasing the scrutinee.
///
/// `stop(Nat)` beside `cons(Nat, L)` takes the `Immediate` family encoding, so `stop`'s payload rides bare and its arm's binder used to *be* the scrutinee. `total`'s `acc + z` then demanded a raw carrier of the scrutinee itself; the representation analysis admitted it because a continuation parameter has no producer to contradict it, carried the demand back along the loop edge to `build`'s accumulator, and the emitter coerced a freshly built `struct.new $tuple/3` with `ref.cast (ref i31)` — a trap for every input above zero, where zero alone answered correctly because the list is then just the bare `stop`.
///
/// The depth is host-tainted deliberately: a closed program folds at compile time and never reaches the emitter, so the fixture would pass while the bug stood.
#[test]
fn an_immediate_arm_payload_survives_arithmetic_in_a_loop() {
    let (system, io) = MockHost::builder().stdin_lines(["A"]).build();
    run_text(
        r#"
        use /std/{Byte, Bytes, Nat, Option, Str, Io};

        induct L : Type
        | stop(Nat)
        | cons(Nat, L)
        end

        let build(n : Nat, acc : L) -> L =
            match n : (_) => L
            | 0 => acc
            | m + 1; ih => build(m, L/cons(m, acc))
            end;

        let total(c : L, acc : Nat) -> Nat =
            match c : (_) => Nat
            | stop(z) => acc + z
            | cons(v, tail) => total(tail, acc + v)
            end;

        let bytes = match Io/read(Io/stdin, 16)! : (_) => Bytes
            | chunk(b) => b
            | eof() => x[]
            | error(_) => x[]
            end;
        let n = Nat/sub(Byte/to_nat(Option/unwrap_or(Bytes/try_get(bytes, 0), 0)), 60);
        /std/print(Nat/to_str(total(build(n, L/stop(n)), 0)))
        "#,
        system,
    )
    .expect("expected result");

    // `A` is 65, so the depth is 5: the cons cells carry 0..4 and `stop` carries 5.
    assert_eq!(io.output(), b"15");
}

// The false arm of `n < m` is the fact `m <= n`, read the other way. The arm records its equation on the guard as written, and the goal spells the dual, so the reducer asks the dual spelling with the literal negated — in both checkers, since the program certifies.
#[test]
fn the_false_arm_of_a_comparison_proves_its_dual() {
    let source = r#"
        use /std/{Nat, Option, Bool};

        let at_least(n : Nat, m : Nat) -> Option(Nat/Le(m, n)) =
            match n < m
            | true => Option/none()
            | false => Option/some(Bool/True/qed())
            end;

        let shown(n : Nat, m : Nat) -> Nat =
            match at_least(n, m) | some(_) => n | none() => m end;

        /std/print(Nat/to_str(shown(7, 3)))
        "#;
    assert_eq!(run(source), b"7");
}

// A guard decides a bound spelled across the `<`/`<=` seam. `List/slice`'s precondition is `s + l <= len`, so slicing one element at `i` asks for `i + 1 <= len(l)`, while the guard a program writes to establish it is `i < len(l)` — one proposition, two spellings, and the arm records only the one the author wrote. Both reducers retry a miss on the successor spelling, so a bound discharges without the author having to spell the comparison the way the standard library's signature happens to.
//
// It certifies, which is the half that matters: the elaborator discharged this first while the kernel still refused it, and the seam is a rule only when both checkers look in the same two places.
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

// **The seam is a second spelling, not a second fact.** Two guards that between them imply a bound only through arithmetic — `i < n` and `len(l) == n * k` give `(i * k) + k <= len(l)` by monotonicity of `*` — leave it stuck, because nothing here reasons about the operands. A retry that answered this would be deciding a proposition rather than looking up another spelling of one.
#[test]
fn a_bound_a_hypothesis_only_implies_is_still_stuck() {
    let source = r#"
        use /std/{Nat, List};

        let chunk(@T : Type, n : Nat, k : Nat, l : List(T), i : Nat) -> List(T) =
            match i < n
            | false => []
            | true =>
                match List/len(l) == n * k
                | false => []
                | true => List/slice(l, i * k, k)
                end
            end;

        /std/print("unreached")
        "#;
    let message = error(source);
    assert!(
        message.contains("nothing discharged"),
        "the seam decided a bound that needs arithmetic:\n{message}"
    );
}

// === A scrutinee bound by `let` ===============================================
//
// The kernel substitutes a `let` rather than binding it, so what it matches on is the binding's definition, and it refines that. The elaborator binds the name to its definition and refines the name — and a metavariable solved against the name is stored as the definition, which the arm never refined: `refl`'s implicit below became `x`, and `x` stayed apart from `xp + 1`. Refining a local binding refines its definition too, in both arms, whatever the scrutinee's type.

#[test]
fn a_let_bound_scrutinee_is_refined_through_its_definition() {
    let source = r#"
        use /std/{Nat, Eq};
        let f(x: Nat) -> Nat =
            let y: Nat = x;
            match y
            | 0 => let _zero: Eq(y, 0) = Eq/refl(); 0
            | xp + 1 =>
                let _same: Eq(y, y) = Eq/refl();
                let _successor: Eq(y, xp + 1) = Eq/refl();
                let _definition: Eq(x, xp + 1) = Eq/refl(@Nat, @(xp + 1));
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
            | true => let _: Eq(c, true) = Eq/refl(); "yes"
            | false => let _: Eq(c, false) = Eq/refl(); "no"
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
            | some(v) => let _: Eq(p, Option/some(v)) = Eq/refl(); v
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
                let _: Eq(z, z) = Eq/refl();
                let _: Eq(x, zp + 1) = Eq/refl(@Nat, @(zp + 1));
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
            | yp + 1 => let _: Eq(y, y) = Eq/refl(); yp
            end;
        /std/print(Nat/to_str(f(3)))
        "#;
    assert_eq!(run(source), b"2");
}
