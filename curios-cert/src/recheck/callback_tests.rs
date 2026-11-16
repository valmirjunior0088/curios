//! A call in a callback, graded where its callee applies it: modules built by hand and put to the certifier alone, each claim beside the program `curios`'s `tests::rules::totality_callback_tests` or `totality_callback_binder_tests` puts to both checkers.

use {super::test_support::*, crate::*, curios_common::fill};

/// `h`, which applies `f` below `n` and hands it on in place, and `spin`, which recurs through the lambda it hands `h`: `${zero}` is `h`'s `0` arm.
const THROUGH_A_CALLBACK: &str = r#"
induct /T: pub Prop
| t()
end

#{node = Rec}
let /h: (n: #NatType, f: (#NatType) -> #{node = InductType} /T()) -> #{node = InductType} /T() =
    (n: #NatType, f: (#NatType) -> #{node = InductType} /T()) =>
        #{goal = #{node = InductType} /T()}
        match n
        | 0 => ${zero}
        | k + 1 =>
            #{goal = #{node = InductType} /T()}
            match k | 0 => /h(k, f) | j + 1 => f(j) end
        end;

#{node = Rec}
let /spin: (n: #NatType) -> #{node = InductType} /T() =
    (n: #NatType) => /h(n, (c: #NatType) => /spin(c));
"#;

/// The certifier summarizes how a definition applies its parameters from the module it judges, with no elaborator's record beside it, and a use its walk does not follow leaves the summary empty: with `h`'s `0` arm a constructor, `h` applies `f` below `n` alone and `spin` descends through it; with the arm applying `f` at `n + 1` through a `let` alias, `h` publishes nothing for `f` and `spin`, whose `spin(1)` is `h(0, …)` applying the lambda at `1`, does not descend. Read as unseen, the alias would leave `f(j)` the summary's one application, and the certifier would admit a proof that recurs for ever. `curios`'s `tests::rules::totality_callback_tests` puts the alias, among the other routes, to both checkers as a program.
#[test]
fn a_callback_applied_through_an_alias_at_a_value_not_below_is_not_graded_below() {
    let zero = "#{node = Variant} /T/t()";
    let control = text_verdicts(&fill!(THROUGH_A_CALLBACK, zero));
    assert_eq!(
        control,
        Vec::new(),
        "a recursion through a callback its callee applies below its argument descends"
    );

    let zero = "let g: (#NatType) -> #{node = InductType} /T() = f; g(#NatAdd(n, 1))";
    let verdicts = text_verdicts(&fill!(THROUGH_A_CALLBACK, zero));
    assert!(
        refuses(&verdicts, Some("spin"), |error| matches!(
            error,
            Error::NotDescending { .. }
        )),
        "a callback applied through an alias at a value not below was graded below: {verdicts:?}",
    );
}

/// `/hide` and `/relay` are one group, `/relay`'s type taking a callback at the place `/hide`'s `f` stands while its body is `${relay}`, a definition outside the group named or applied in part, which binds no parameter. `/hide` hands `/relay` `${callback}` where `n` is `0` and applies `f` to `k`, below `n`, otherwise, and `/spin` recurses through the callback it hands `/hide`.
const A_CALLBACK_HANDED_TO_A_MEMBER_WHOSE_BODY_BINDS_NONE: &str = r#"
let /apply: (m: #NatType, g: (#NatType) -> #{node = InductType} /std/Bool/True()) ->
    #{node = InductType}
    /std/Bool/True() =
    (m: #NatType, g: (#NatType) -> #{node = InductType} /std/Bool/True()) => g(0);
let /pick: (z: #NatType) ->
    (m: #NatType, g: (#NatType) -> #{node = InductType} /std/Bool/True()) ->
        #{node = InductType}
        /std/Bool/True() =
    (z: #NatType) => (m: #NatType, g: (#NatType) -> #{node = InductType} /std/Bool/True()) => g(z);
let /hide: (n: #NatType, f: (#NatType) -> #{node = InductType} /std/Bool/True()) ->
    #{node = InductType}
    /std/Bool/True() =
    (n: #NatType, f: (#NatType) -> #{node = InductType} /std/Bool/True()) =>
        #{goal = #{node = InductType} /std/Bool/True()}
        match n | 0 => /relay(n, ${callback}) | k + 1 => f(k) end
and /relay: (m: #NatType, g: (#NatType) -> #{node = InductType} /std/Bool/True()) ->
    #{node = InductType}
    /std/Bool/True() =
    ${relay};
#{node = Rec}
let /spin: (n: #NatType) -> #{node = InductType} /std/Bool/True() =
    (n: #NatType) => /hide(n, (c: #NatType) => /spin(c));
"#;

// A member's parameters are the ones its leading lambdas bind, whatever its type takes: `/relay` named as `/apply` or as `/pick(0)` binds none, so `/hide` handing `f` to it at its own place hands `f` to what `/relay` is, which applies it to `0`, and `/hide` publishes nothing for `f`. Counted by its type, `f` would be `/relay`'s second parameter, which no body of the group applies: `/hide`'s summary would hold `f(k)` alone, below `n`, and `/spin`, which at `0` is `/apply(0, …)`, `/spin(0)` again, would descend. No program reaches either module, since the elaborator refuses a group whose members do not all need one another. The control hands `/relay` a callback of its own, so `/hide` applies `f` below `n` alone and `/spin` descends. `curios`'s `tests::rules::totality_descent_tests` puts a member whose body binds fewer parameters than its type takes to both checkers as `a_callback_handed_past_a_members_parameters_is_graded_as_unrelated`.
//
// Mutation-checked: with `*position < arity` removed from `handed_on` in `curios-analysis/src/totality.rs`, both refusals certify and this test fails.
#[test]
fn a_callback_handed_to_a_member_whose_body_binds_no_parameter_is_graded_as_unrelated() {
    let verdicts = |relay: &str, callback: &str| {
        let module = fill!(
            A_CALLBACK_HANDED_TO_A_MEMBER_WHOSE_BODY_BINDS_NONE,
            relay,
            callback
        );
        text_verdicts(&fill!("${PROPOSITIONS}\n${module}", PROPOSITIONS, module))
    };
    for relay in ["/apply", "/pick(0)"] {
        let refused = verdicts(relay, "f");
        assert!(
            refuses(&refused, Some("spin"), |error| matches!(
                error,
                Error::NotDescending { .. }
            )),
            "`/relay` as `{relay}`: a callback handed to a member that binds no parameter was graded below `n`: {refused:?}",
        );

        let control = verdicts(
            relay,
            "(x: #NatType) => #{node = Variant} /std/Bool/True/qed()",
        );
        assert_eq!(
            control,
            Vec::new(),
            "`/relay` as `{relay}`: the control stopped certifying, a callback `/hide` applies below `n` alone",
        );
    }
}

/// `/loop`'s type opens one list and answers `/Next`, a definition whose value is a second list, while its body binds both by leading lambdas, and calls `${call}` where `n` is a successor.
const A_SECOND_LIST_THAT_IS_A_DEFINITIONS_VALUE: &str = r#"
let /Next: Prop =
    (f: (#NatType) -> #{node = InductType} /std/Bool/True()) ->
        #{node = InductType}
        /std/Bool/True();
#{node = Rec}
let /loop: (n: #NatType) -> /Next =
    (n: #NatType) => (f: (#NatType) -> #{node = InductType} /std/Bool/True()) =>
        #{goal = #{node = InductType} /std/Bool/True()}
        match n | 0 => f(0) | k + 1 => ${call} end;
"#;

// A member's parameters as the certifier records its calls against them are the ones the walk counts as the columns of a call to it: both are its leading lambdas, the second here checked against `/Next` reduced, so `/loop(n)(f)` grades on a square matrix and its diagonal, `n` handed back at `n`, carries no decrease. Bound by the first lambda alone, `/loop` would grade the call on one row against two columns, which composes with nothing and is no idempotent, so the group would descend on a call that hands back everything it was handed. The control descends on `k`.
//
// Mutation-checked: with `continue_parameters` dropped from `check_lambda`'s `Step::Bodies` arm in `curios-cert/src/certifier/infer.rs`, the refusal certifies and this test fails.
#[test]
fn a_member_whose_second_list_is_a_definitions_value_is_graded_over_both_its_lambdas() {
    let verdicts = |call: &str| {
        let module = fill!(A_SECOND_LIST_THAT_IS_A_DEFINITIONS_VALUE, call);
        text_verdicts(&fill!("${PROPOSITIONS}\n${module}", PROPOSITIONS, module))
    };

    let refused = verdicts("/loop(n)(f)");
    assert!(
        refuses(&refused, Some("loop"), |error| matches!(
            error,
            Error::NotDescending { .. }
        )),
        "a call handing back both of a member's lists descended: {refused:?}",
    );
    assert_eq!(
        verdicts("/loop(k)(f)"),
        Vec::new(),
        "the control stopped certifying, a call on the predecessor",
    );
}

/// `each` applies `p` to the `Never` a `more` holds and the function that answers to `r` unchanged, and `split` applies `p` to that `Never` and `q` to `r` unchanged. `Never` has no closed value, so a `spin` whose calls descend is a total function out of it; `${spin}` is what `spin` hands its callee.
const CALLBACKS_OUT_OF_AN_EMPTY_TYPE: &str = r#"
induct /Never: pub Type
| more(x: #{node = InductType} /Never())
end

#{node = Rec}
let /each: (
    n: #NatType,
    t: #{node = InductType} /Never(),
    r: #{node = InductType} /Never(),
    p: (#{node = InductType} /Never()) -> (#{node = InductType} /Never()) -> /std/Bool/False
) -> /std/Bool/False =
    (
        n: #NatType,
        t: #{node = InductType} /Never(),
        r: #{node = InductType} /Never(),
        p: (#{node = InductType} /Never()) -> (#{node = InductType} /Never()) -> /std/Bool/False
    ) =>
        #{goal = /std/Bool/False}
        match n
        | 0 => #{goal = /std/Bool/False} match t | more(x) => p(x)(r) end
        | k + 1 => /each(k, t, r, p)
        end;

#{node = Rec}
let /split: (
    n: #NatType,
    t: #{node = InductType} /Never(),
    r: #{node = InductType} /Never(),
    p: (#{node = InductType} /Never()) -> /std/Bool/False,
    q: (#{node = InductType} /Never()) -> /std/Bool/False
) -> /std/Bool/False =
    (
        n: #NatType,
        t: #{node = InductType} /Never(),
        r: #{node = InductType} /Never(),
        p: (#{node = InductType} /Never()) -> /std/Bool/False,
        q: (#{node = InductType} /Never()) -> /std/Bool/False
    ) =>
        #{goal = /std/Bool/False}
        match n
        | 0 => #{goal = /std/Bool/False} match t | more(x) => p(x) end
        | k + 1 => q(r)
        end;

#{node = Rec}
let /spin: (t: #{node = InductType} /Never()) -> /std/Bool/False =
    (t: #{node = InductType} /Never()) => ${spin};
/std/Bool/True/qed()
"#;

/// The verdicts on [`CALLBACKS_OUT_OF_AN_EMPTY_TYPE`] with `spin` handing its callee `spin`.
fn spin_verdicts(spin: &str) -> Vec<Verdict> {
    core_text_verdicts(&fill!(
        "${PROPOSITIONS}\n${CALLBACKS}",
        PROPOSITIONS,
        CALLBACKS = fill!(CALLBACKS_OUT_OF_AN_EMPTY_TYPE, spin)
    ))
}

/// Whether `verdicts` refuse `spin` for a group that does not descend.
fn spin_does_not_descend(verdicts: &[Verdict]) -> bool {
    refuses(verdicts, Some("spin"), |error| {
        matches!(error, Error::NotDescending { .. })
    })
}

// A bound a callback is handed stays with its binders: `each` stands `c` below `t`, and the lambda the callback answers binds `e` to `r`, which `spin` hands `t` itself, so `spin(e)` is `spin(t)` again. `curios`'s `tests::rules::totality_callback_binder_tests` puts the program to both checkers under this name. The control recurses on `c`.
#[test]
fn a_lambda_a_callback_answers_takes_none_of_the_callbacks_bounds() {
    let attack = spin_verdicts(
        "/each(0, t, t, (c: #{node = InductType} /Never()) => (e: #{node = InductType} /Never()) => /spin(e))",
    );
    assert!(spin_does_not_descend(&attack), "{attack:?}");

    let control = spin_verdicts(
        "/each(0, t, t, (c: #{node = InductType} /Never()) => (e: #{node = InductType} /Never()) => /spin(c))",
    );
    assert!(
        !refuses(&control, Some("spin"), |error| matches!(
            error,
            Error::NotDescending { .. } | Error::NotTotal { .. }
        )),
        "{control:?}"
    );
}

// Each argument of a call is bounded by the applications of its own parameter: `split` stands `p`'s binder below `t` and `q`'s at `r`, which `spin` hands `t` itself, so `spin(e)` is `spin(t)` again. `curios`'s `tests::rules::totality_callback_binder_tests` puts the program to both checkers under this name. The control's `q` callback recurses on what a `more` of `t` holds.
#[test]
fn a_callbacks_sibling_is_bounded_by_its_own_applications() {
    let attack = spin_verdicts(
        "/split(1, t, t, (c: #{node = InductType} /Never()) => /spin(c), (e: #{node = InductType} /Never()) => /spin(e))",
    );
    assert!(spin_does_not_descend(&attack), "{attack:?}");

    let control = spin_verdicts(
        "/split(1, t, t, (c: #{node = InductType} /Never()) => /spin(c), (e: #{node = InductType} /Never()) => #{goal = /std/Bool/False} match t | more(y) => /spin(y) end)",
    );
    assert!(
        !refuses(&control, Some("spin"), |error| matches!(
            error,
            Error::NotDescending { .. } | Error::NotTotal { .. }
        )),
        "{control:?}"
    );
}
