//! A nominal node's levels compared by its family's variance: what the arms compare, what they still compare whatever the levels, and what reaches them.

use {
    crate::certifier::convert::test_support::*,
    crate::{test_support::*, *},
    curios_analysis::held_levels,
    curios_common::{Qualifier, fill},
    curios_core::{Global, LevelHead, Reducer, UniverseParam, Variance},
};

/// `induct Wrap.{u}(A: Type u): Type u | wrap(a: A) end`, filed at `${variance}`: a level only its parameter's type mentions.
const WRAP: &str = r#"
#{universe_context = {parameter_count = 1}, variances = [${variance}]}
induct /Wrap(A: #{level = u} Type): pub #{level = u} Type
| wrap(a: A)
end
"#;

/// `induct Box.{u}: Type (u + 1) | box(T: Type u) end`, filed at `${variance}`: a level its payload's `Type` mentions.
const BOX: &str = r#"
#{universe_context = {parameter_count = 1}, variances = [${variance}]}
induct /Box: pub #{level = u + 1} Type
| box(T: #{level = u} Type)
end
"#;

/// `induct Leaf.{u}: Type | leaf() end`, filed at `${variance}`: a family with no parameter, which its bare name applies in full.
const LEAF: &str = r#"
#{universe_context = {parameter_count = 1}, variances = [${variance}]}
induct /Leaf: pub Type
| leaf()
end
"#;

/// The family `text` writes at `variance`, declared in `certifier` with the vector filed for it, and its name: what a comparison reads is the vector the walk that accepted the entry filed, which a fixture declaring one by hand files for it.
fn filed(certifier: &mut Certifier, text: &str, path: &str, variance: Variance) -> Global {
    let name = Global::Authored(Qualifier::from([path]));
    declared(certifier, &fill!(text, variance = format!("{variance:?}")));
    certifier.file_variances(&name, vec![variance]);

    name
}

/// Whether `name` at two instances apart in its one level is one term at `type_`: as written, and reduced first.
fn two_instances_convert(certifier: &mut Certifier, path: &str, type_: &str) -> [bool; 2] {
    let at = |level: u32| {
        term(&fill!(
            "#{universes = {levels = [${level}]}} /${path}",
            level,
            path
        ))
    };
    let reduced = [0, 1].map(|level| certifier.reduce(at(level)).expect("an instance unfolds"));
    let type_ = term(type_);

    [
        convert(certifier, &type_, &at(0), &at(1)),
        convert(certifier, &type_, &reduced[0], &reduced[1]),
    ]
    .map(|verdict| verdict.expect("a verdict within the budget"))
}

// Two values compared with no type to direct them, as a stuck elimination's arm is: `wrap` at two instances is one value, `box` is two, and a payload is compared whatever the levels.
#[test]
fn a_value_compares_at_its_familys_variance() {
    let mut certifier = certifier();
    filed(&mut certifier, WRAP, "Wrap", Variance::Irrelevant);
    filed(&mut certifier, BOX, "Box", Variance::Invariant);
    let wrapped = |level: u32, held: usize| {
        term(&fill!(
            "#{node = Variant, universes = {levels = [${level}]}} /Wrap/wrap(@#NatType, ${held})",
            level,
            held
        ))
    };
    let boxing = |level: u32| {
        term(&fill!(
            "#{node = Variant, universes = {levels = [${level}]}} /Box/box(#NatType)",
            level
        ))
    };
    let untyped = term("Type");

    assert_eq!(
        convert(&mut certifier, &untyped, &wrapped(0, 3), &wrapped(1, 3)),
        Ok(true)
    );
    assert_eq!(
        convert(&mut certifier, &untyped, &boxing(0), &boxing(1)),
        Ok(false)
    );
    assert_eq!(
        convert(&mut certifier, &untyped, &wrapped(0, 3), &wrapped(1, 4)),
        Ok(false)
    );
}

/// `Wrap` at the instance `level` over `param`, a type.
fn wrap_at(level: &str, param: &str) -> String {
    fill!(
        "#{node = InductType, universes = {levels = [${level}]}} /Wrap(${param})",
        level,
        param
    )
}

// Only the level goes uncompared. Either side leads, the arguments being opened at the left side's instance.
#[test]
fn an_irrelevant_levels_arguments_are_compared_at_either_instance() {
    let mut certifier = certifier();
    filed(&mut certifier, WRAP, "Wrap", Variance::Irrelevant);
    let small = term(&wrap_at("0", "#NatType"));
    let large = term(&wrap_at("1", "#BoolType"));
    let untyped = term("Type");

    assert_eq!(
        convert(
            &mut certifier,
            &untyped,
            &small,
            &term(&wrap_at("1", "#NatType"))
        ),
        Ok(true)
    );
    assert_eq!(convert(&mut certifier, &untyped, &small, &large), Ok(false));
    assert_eq!(convert(&mut certifier, &untyped, &large, &small), Ok(false));
}

// `Alias.{u} = (A: Type u) => A` at two instances: the spines of one definition decide nothing once its levels differ, and both sides unfold to `Nat`. It is why a field that mentions a level only on such an instance is one type at every instance of it.
#[test]
fn a_definition_applied_at_two_instances_converts_by_unfolding() {
    let mut certifier = certifier();
    declared(
        &mut certifier,
        "#{universe_context = {parameter_count = 1}}\nlet /Alias: (A: #{level = u} Type) -> #{level = u} Type = (A: #{level = u} Type) => A;",
    );
    let at = |level: u32| {
        term(&fill!(
            "(#{universes = {levels = [${level}]}} /Alias)(#NatType)",
            level
        ))
    };

    assert_eq!(
        convert(&mut certifier, &term("Type"), &at(0), &at(1)),
        Ok(true)
    );
}

// A former not yet applied is a function, and eta applies both sides to one binder before either head is read, so the nodes they build are what is compared: by variance, as any two nodes are. Carried invariant, the same pair is two functions, as written and reduced first.
#[test]
fn two_bare_formers_apart_in_an_irrelevant_level_convert_at_their_type() {
    for (variance, converts) in [(Variance::Irrelevant, true), (Variance::Invariant, false)] {
        let mut certifier = certifier();
        filed(&mut certifier, WRAP, "Wrap", variance);

        assert_eq!(
            two_instances_convert(&mut certifier, "Wrap", "(A: Type) -> Type"),
            [converts; 2],
            "{variance:?}"
        );
    }
}

// A family with no parameter is applied in full by its bare name, so two instances of the name apart in an irrelevant level are one type, whether or not the pair was reduced first.
#[test]
fn a_family_with_no_parameter_converts_by_its_name_at_two_instances() {
    for (variance, converts) in [(Variance::Irrelevant, true), (Variance::Invariant, false)] {
        let mut certifier = certifier();
        filed(&mut certifier, LEAF, "Leaf", variance);

        assert_eq!(
            two_instances_convert(&mut certifier, "Leaf", "Type"),
            [converts; 2],
            "{variance:?}"
        );
    }
}

// An arm's equation answers at another instance of a level its scrutinee's family is irrelevant in, and at no other instance of one it is invariant in: what reduction asks conversion of a stuck form and an equation's scrutinee is compared by variance, as every pair is. `curios-elab`'s `reduce::question_tests` holds this proposition under this name.
#[test]
fn a_case_equation_answers_at_another_instance_of_an_irrelevant_level() {
    let mut certifier = certifier();
    filed(&mut certifier, WRAP, "Wrap", Variance::Irrelevant);
    filed(&mut certifier, BOX, "Box", Variance::Invariant);
    let boxing = |level: &str| {
        fill!(
            "#{node = InductType, universes = {levels = [${level}]}} /Box()",
            level
        )
    };
    let [wrapped_at_0, wrapped_at_1, boxing_at_0, boxing_at_1] = under(
        &mut certifier,
        "over: (T: #{level = 2} Type) -> #NatType",
        &fill!(
            "over(${w0}), over(${w1}), over(${b0}), over(${b1})",
            w0 = wrap_at("0", "#NatType"),
            w1 = wrap_at("1", "#NatType"),
            b0 = boxing("0"),
            b1 = boxing("1")
        ),
    );

    certifier.scoped(|certifier| {
        certifier
            .refine(wrapped_at_0, term("1"))
            .expect("the equation records");
        assert_eq!(whnf(certifier, wrapped_at_1), Ok(term("1")));
    });
    certifier.scoped(|certifier| {
        certifier
            .refine(boxing_at_0, term("1"))
            .expect("the equation records");
        assert_eq!(whnf(certifier, boxing_at_1.clone()), Ok(boxing_at_1));
    });
}

// A lambda's body is read by the vector the certifier filed for its family: `(A: Type u) => Wrap.{v}(A)` holds `u`, its binder's, and not `v`, which `Wrap` is irrelevant in, where over `Box`, invariant in its level, both are held.
#[test]
fn a_lambdas_body_is_read_by_the_vector_filed_for_its_family() {
    let mut certifier = certifier();
    filed(&mut certifier, WRAP, "Wrap", Variance::Irrelevant);
    filed(&mut certifier, BOX, "Box", Variance::Invariant);
    let mut held = |body: &str| {
        let found = held_levels(
            &mut certifier,
            &[term(&fill!("(A: #{level = u} Type) => ${body}", body))],
        );

        [0, 1].map(|index| found.holds(LevelHead::Param(UniverseParam(index))))
    };

    assert_eq!(held(&wrap_at("v", "A")), [true, false]);
    assert_eq!(
        held("#{node = InductType, universes = {levels = [v]}} /Box()"),
        [true, true]
    );
}

/// `pair.{u}: {Type u, Nat} = (Nat, 3)` and `held.{u}: {Type (u + 1)} = (Type u,)`.
const FIELDS: &str = r#"
#{universe_context = {parameter_count = 1}}
let /pair: {first: #{level = u} Type, second: #NatType} = (#NatType, 3);
#{universe_context = {parameter_count = 1}}
let /held: {first: #{level = u + 1} Type} = (#{level = u} Type,);
"#;

// `pair.{u}: {Type u, Nat} = (Nat, 3)` and `held.{u}: {Type (u + 1)} = (Type u,)`, each projected at two instances: the certifier forces a projection to its field before it compares heads, so a field whose value drops the level is one term at both, and one whose value holds it is two. The elaborator's twin of this proposition shares the name.
#[test]
fn a_field_that_drops_a_level_is_one_term_at_two_instances() {
    let mut certifier = certifier();
    declared(&mut certifier, FIELDS);
    let first = |path: &str, level: u32| {
        term(&fill!(
            "(#{universes = {levels = [${level}]}} /${path}).0",
            path,
            level
        ))
    };
    let ground = term("Type");

    assert_eq!(
        convert(
            &mut certifier,
            &ground,
            &first("pair", 0),
            &first("pair", 1)
        ),
        Ok(true)
    );
    assert_eq!(
        convert(
            &mut certifier,
            &ground,
            &first("held", 0),
            &first("held", 1)
        ),
        Ok(false)
    );
}
