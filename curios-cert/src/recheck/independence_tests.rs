//! A verdict filed for one declaration leaves every other declaration's verdict as a walk that refused nothing would have left it.

use {super::test_support::*, crate::*};

/// A declaration admitted on a refused declaration's stated definition leaves the module refused.
///
/// `check_definition` returns before its define step and the walk defines the item anyway, at its declared type with its real body, so every later declaration is judged against exactly what a fully passing walk would have shown it. `lie : Absurd = ()` is refused and still stands at `Absurd`, so `truth : Absurd = lie` is admitted and the program closes with a closed inhabitant of an empty proposition whose verdict rests on what the refused declaration's judgment put in the environment. What stands between that and a proof of `False` is the walk's own list, which `curios-pipeline` fails the build on at its first entry: the walk reports the lying declaration and nothing else, so a declaration admitted on a refused one's definition admits no module.
///
/// The list is asserted whole, so the test says both halves at once: the lie is reported, and nothing that rests on it is — a walk that dropped the lie's verdict would admit this module at an empty proposition.
#[test]
fn a_declaration_admitted_on_a_refused_declarations_definition_leaves_the_module_refused() {
    let verdicts = text_verdicts(
        r#"
induct /Absurd: pub Prop
end

let /lie: #{node = InductType} /Absurd() =
    ();

let /truth: #{node = InductType} /Absurd() =
    /lie;

/truth
: #{node = InductType} /Absurd()
"#,
    );

    assert_eq!(
        verdicts
            .iter()
            .map(|verdict| verdict.name)
            .collect::<Vec<_>>(),
        vec![Some(global("lie"))],
        "the walk must refuse the lying declaration, and nothing of what rests on it: {verdicts:?}",
    );
}

/// A family admitted by composing through the polarity vector a refused declaration's judgment filed leaves the module refused.
///
/// `accept` files each entry's polarity vector the moment `positivity_vectors` hands one back, and `check_induct_decl` runs after that, so a vector stands in the environment for an entry the walk goes on to refuse — `Shape(A : Type)` below declares `mk` twice, which `check_distinct_tags` refuses as `RepeatedTag`, while its parameter is strictly positive in both of them and the vector filed for it says so. `Knot`, declared after it, puts itself inside that parameter, and its own strict positivity composes through the filed vector and accepts; with no vector on record the parameter would answer `Mixed` and `Knot` would be refused. So the vector a refused declaration's judgment filed is what a later declaration's verdict rests on, and what stands between that and a family nothing established the positivity of is the walk's own list: the walk reports the family with the repeated tag and nothing else.
///
/// Mutation-checked against `Knot`'s acceptance resting on anything else: with `Shape`'s payload at `(A) -> Nat`, so that the filed vector reads the parameter `Neg`, the same module is refused twice over, `Knot` by `NotPositive { polarity: Neg }` at its `wrap` payload.
#[test]
fn a_family_admitted_through_a_refused_declarations_vector_leaves_the_module_refused() {
    // `Shape` declares one tag twice, and `Knot` wraps itself in a `Shape`.
    let verdicts = text_verdicts(
        r#"
#{absent = [/Shape/mk, /Shape/mk]}
induct /Shape(A: Type): pub Type
| mk(a: A)
| mk(a2: A)
end

#{absent = [/Knot/wrap]}
induct /Knot: pub Type
| wrap(x: #{node = InductType} /Shape(#{node = InductType} /Knot()))
end
"#,
    );

    assert_eq!(
        verdicts
            .iter()
            .map(|verdict| (verdict.name, matches!(verdict.error, Error::RepeatedTag(_))))
            .collect::<Vec<_>>(),
        vec![(Some(global("Shape")), true)],
        "the walk must refuse the family with the repeated tag, and nothing that composed through its vector: {verdicts:?}",
    );
}
