//! What an arm learns from its scrutinee, and the equations it may not assume.

use {
    super::super::{error, run, run_text},
    curios_runtime::MockHost,
};

// A refinement key is stored at the arm and probed wherever the scrutinee is mentioned again, so the two spellings have to compare equal. They did not when the scrutinee carried an *inferred* metavariable: `Pred/test(t, b)` elaborates to `(?w).0(t, b)`, and a second occurrence mints its own `?w'`, so two terms solved to the same witness keyed differently and the arm silently refined nothing. Solved metavariables are now materialized into the key, which is what makes the two spellings one.
//
// Three constraints shape the scrutinee, and they pull against each other. It must carry a metavariable, or there is nothing to materialize. It must not reduce away, or the store is never reached — hence a method whose body eliminates the *symbolic* `b`. And its head must be one `curios_cert::fixes_no_value` reads, which a function parameter stopped being once `an_effect_behind_a_function_parameter_does_not_refine` landed. A concept dispatch is all three at once, and it is what `/std/Str/utf8/cont_len` refines on in production rather than a shape invented here.
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
        use /std/{Handle, Byte, Bytes, Nat, Option, Str};

        induct L : Type
        | stop(Nat)
        | cons(Nat, L)
        end

        rec build(n : Nat, acc : L) -> L =
            match n : (_) => L
            | 0 => acc
            | m + 1; ih => build(m, L/cons(m, acc))
            end;

        rec total(c : L, acc : Nat) -> Nat =
            match c : (_) => Nat
            | stop(z) => acc + z
            | cons(v, tail) => total(tail, acc + v)
            end;

        let bytes = match Handle/read(Handle/stdin, 16)! : (_) => Bytes
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
