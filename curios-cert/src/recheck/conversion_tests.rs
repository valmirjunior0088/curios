//! Conversion as a walk takes it: where a remembered verdict answers.

use {super::test_support::*, crate::*, curios_analysis::test_support::*, curios_common::fill};

/// `g(x, y, z)`, which steps to `g(y, 0, z - 1)` until `z` is zero.
const STEPPING: &str = r#"
#{node = Rec}
let /g : (_x: #NatType, y: #NatType, z: #NatType) -> #NatType =
    (_x: #NatType, y: #NatType, z: #NatType) =>
        #{result = Ambient(#NatType)}
        match #NatEql(z, 0)
        | false =>
            /g(y, 0, #NatSub(z, 1))
        | true =>
            0
        end;
"#;

/// `g(p, a, m)` and `g(q, b, m)` unfold, as a pair, to `g(a, 0, m - 1)` and `g(b, 0, m - 1)`: the same two heads, apart inside their own unfolding, where the outer pair is refused. Compared from no pair, that inner pair unfolds once to two equal calls and holds. Asked first, its verdict would answer inside the outer pair's unfolding, and the outer pair would hold where asked alone it does not: acceptance would follow the order goals were met in. Whichever is asked first, the outer pair is refused, with the memos on as off.
///
/// Mutation-checked: with a remembered verdict answering whatever is being unfolded where it is asked, the outer pair asked after the inner one holds with the memos on.
#[test]
fn a_verdict_reached_by_unfolding_a_pair_does_not_answer_inside_that_pairs_unfolding() {
    let outer = "#NatEql(/g(p, a, m), /g(q, b, m))";
    let inner = "#NatEql(/g(a, 0, #NatSub(m, 1)), /g(b, 0, #NatSub(m, 1)))";
    let module = |body: String| {
        core_text_module(&fill!(
            r#"${PROPOSITIONS}${STEPPING}
#{proof = true}
let /second : (p: #NatType, q: #NatType, a: #NatType, b: #NatType, m: #NatType) -> /std/Bool/Holds(${outer}) =
    (p: #NatType, q: #NatType, a: #NatType, b: #NatType, m: #NatType) =>
        ${body};
"#,
            PROPOSITIONS,
            STEPPING,
            outer = outer,
            body = body,
        ))
    };
    let alone = module("/std/Bool/True/qed()".to_string());
    let after = module(fill!(
        r#"let first : /std/Bool/Holds(${inner}) = /std/Bool/True/qed();
        /std/Bool/True/qed()"#,
        inner = inner,
    ));
    let refused = |verdicts: &[Verdict]| {
        verdicts.iter().any(|verdict| {
            verdict.name == Some(global("second"))
                && matches!(verdict.error, Error::Mismatch { .. })
        })
    };

    for uncached in [false, true] {
        for (asked, definitions) in [("alone", &alone), ("after the inner pair", &after)] {
            let mut certifier = match uncached {
                false => Certifier::new(BUDGET, SYNTAX),
                true => Certifier::uncached(BUDGET, SYNTAX),
            };
            let verdicts =
                super::verdicts_from(&mut certifier, definitions, None, &Globals::default(), None)
                    .verdicts;
            assert!(
                refused(&verdicts),
                "the outer pair asked {asked}, with the memos off: {uncached}, holds: {verdicts:?}",
            );
        }
    }
}
