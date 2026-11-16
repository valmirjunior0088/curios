//! A call in a callback, graded where its callee applies it: modules built by hand and put to the certifier alone, each claim beside the program `curios`'s `tests::rules::totality_callback_tests` puts to both checkers.

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
