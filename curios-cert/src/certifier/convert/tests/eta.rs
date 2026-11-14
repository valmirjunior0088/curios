//! Eta at a function, a pair and a type with no field or one inhabitant — by the goal's type, and by a literal where no type directs it.

use {
    crate::certifier::convert::test_support::*,
    crate::{test_support::*, *},
    curios_common::fill,
    curios_core::Term,
};

/// Eta at a function type: `f` and `(x) => f(x)` are the same function, and neither side has to be written in the other's shape for conversion to see it.
#[test]
fn eta_makes_a_function_converge_with_its_expansion() {
    let mut certifier = certifier();
    let [f, expanded] = terms("(f, (x: #NatType) => f(x))");

    assert_eq!(
        convert(
            &mut certifier,
            &term("(x: #NatType) -> #NatType"),
            &f,
            &expanded
        ),
        Ok(true),
    );
}

/// Eta at a Σ type: `p` and `(p.0, p.1)` are the same pair.
#[test]
fn eta_makes_a_pair_converge_with_its_projections() {
    let mut certifier = certifier();
    let [p, expanded] = terms("(p, (p.0, p.1))");

    assert_eq!(
        convert(
            &mut certifier,
            &term("{a: #NatType, b: #NatType}"),
            &p,
            &expanded
        ),
        Ok(true),
    );
}

/// Eta by the lambda, where no type directs it. At `Type` — what a stuck elimination's arm and a projection's head are compared at — a lambda converges with the neutral it expands, the neutral applied to the lambda's own binder. The lambda is no forwarder, `(x) => f(x + 0)`, so `whnf` does not contract it and the rule is what decides. The near miss drops the binder and is refused: the rule compares the body.
///
/// Mutation-checked: without `function_eta`'s arms the first two are refused, and with its body comparison answered `true` the third is accepted.
#[test]
fn eta_by_the_lambda_converges_a_function_with_a_neutral_at_no_type() {
    let mut certifier = certifier();
    let [neutral, past_a_fold, dropped] = under(
        &mut certifier,
        "f: (x: #NatType) -> #NatType",
        "f, (x: #NatType) => f(#NatAdd(x, 0)), (x: #NatType) => f(0)",
    );
    let ground = term("Type");

    assert_eq!(
        convert(&mut certifier, &ground, &past_a_fold, &neutral),
        Ok(true)
    );
    assert_eq!(
        convert(&mut certifier, &ground, &neutral, &past_a_fold),
        Ok(true)
    );
    assert_eq!(
        convert(&mut certifier, &ground, &dropped, &neutral),
        Ok(false)
    );
}

/// Eta by the literal at a record, where no type directs it: a tuple literal converges with the neutral it projects, field by field, and one with its components swapped does not.
///
/// Mutation-checked: without `tuple_eta`'s arms the first two are refused, and with its field comparison answered `true` the third is accepted.
#[test]
fn eta_by_the_literal_converges_a_pair_with_a_neutral_at_no_type() {
    let mut certifier = certifier();
    let [neutral, expansion, swapped] = terms("(p, (p.0, p.1), (p.1, p.0))");
    let ground = term("Type");

    assert_eq!(
        convert(&mut certifier, &ground, &expansion, &neutral),
        Ok(true)
    );
    assert_eq!(
        convert(&mut certifier, &ground, &neutral, &expansion),
        Ok(true)
    );
    assert_eq!(
        convert(&mut certifier, &ground, &swapped, &neutral),
        Ok(false)
    );
}

/// The unit literal has no field to compare, so against a neutral its walk is empty and answers `true`: `{}` has one inhabitant, and a neutral of the literal's type is it.
#[test]
fn the_unit_literal_converges_with_a_neutral() {
    let mut certifier = certifier();
    let [neutral, unit] = terms("(u, ())");

    assert_eq!(
        convert(&mut certifier, &term("{}"), &unit, &neutral),
        Ok(true)
    );
    assert_eq!(
        convert(&mut certifier, &term("Type"), &neutral, &unit),
        Ok(true)
    );
}

/// `U`, a structure that declares no field, and `S`, one of a number.
const FIELD_LESS_AND_ONE_FIELD: &str = r#"
struct /U: pub Type {}
struct /S: pub Type {
    a: #NatType,
}
"#;

/// Unit eta, by the goal's type: at the empty Σ and at a nominal struct that declares no field, any two terms converge without being read — two variables, and two stuck applications of two heads, which no structural rule equates. It composes with the eta that reaches it: two variables at a record of units converge by their projections, and two at a function into a unit by their applications.
///
/// The rule is the type's. At `Type`, what a child with no typed context is compared at, two variables stay apart, and so do two at a record or a struct that has a relevant field.
///
/// Mutation-checked: with the empty Σ left to the structural rules the first goal is refused, without the struct's arm the field-less struct's is, and with that arm taken whatever the declaration's fields the last goal is accepted.
#[test]
fn any_two_terms_converge_at_a_type_with_no_field() {
    let mut certifier = certifier();
    declared(&mut certifier, FIELD_LESS_AND_ONE_FIELD);
    let [u, v, this, that] = terms("(u, v, f(0), g(1))");
    let mut at =
        |type_: &str, this: &Term, that: &Term| convert(&mut certifier, &term(type_), this, that);

    assert_eq!(at("{}", &u, &v), Ok(true));
    assert_eq!(at("{}", &this, &that), Ok(true));
    assert_eq!(at("#{node = StructType} /U()", &u, &v), Ok(true));
    assert_eq!(at("{a: {}, b: {}}", &u, &v), Ok(true));
    assert_eq!(at("(x: #NatType) -> {}", &u, &v), Ok(true));

    assert_eq!(at("Type", &u, &v), Ok(false));
    assert_eq!(at("{a: #NatType}", &u, &v), Ok(false));
    assert_eq!(at("#{node = StructType} /S()", &u, &v), Ok(false));
}

/// The structures [`any_two_terms_converge_at_a_struct_with_one_inhabitant`] compares at: `W` of a unit, `Proved` of a proof of the proposition `P`, `N` nesting such fields, `V` and `M` with a number somewhere, `R` reaching itself, `Left` and `Right` reaching themselves through each other, and `Pair`, which names no struct.
const ONE_INHABITANT: &str = r#"
induct /P: pub Prop
end
struct /W: pub Type {
    u: {},
}
struct /Proved: pub Type {
    p: #{node = InductType} /P(),
}
struct /N: pub Type {
    w: #{node = StructType} /W(),
    f: (x: #NatType) -> {},
    r: {a: {}, b: #{node = StructType} /Proved()},
}
struct /V: pub Type {
    n: #NatType,
    u: {},
}
struct /M: pub Type {
    w: #{node = StructType} /W(),
    v: #{node = StructType} /V(),
}
struct /R(n: #NatType): pub Type {
    next: (m: #NatType) -> #{node = StructType} /R(m),
}
struct /Left(n: #NatType): pub Type {
    right: (m: #NatType) -> #{node = StructType} /Right(m),
}
struct /Right(n: #NatType): pub Type {
    left: (m: #NatType) -> #{node = StructType} /Left(m),
}
struct /Pair(n: Type): pub Type {
    a: n,
    b: n,
}
"#;

/// A nominal struct has no eta by its type, so what eta would decide between two terms at one is read off the type: any two converge at a struct every field of which has one inhabitant — a unit, a proof, a function into a unit, a record of such, another such struct — whatever their shapes. The elaborator's twin of this proposition shares the name.
///
/// The refusals are the rule's bounds. One relevant field keeps two variables apart, at whatever depth it sits; and a struct that reaches itself, by its own fields or through another's, answers no where it is met again, which is what ends the walk. A struct nested in its own parameter is met again too, and is judged again: its declaration names no struct, and the parameter is what bounds the walk.
///
/// Mutation-checked: with the struct left to the structural rules the five held goals are refused; with every struct counted the four refused goals are accepted; with every struct met again refused the goal at the struct nested in its own parameter is; with none refused the last two goals spend the budget; and with a declaration read for its own name alone the last one does.
#[test]
fn any_two_terms_converge_at_a_struct_with_one_inhabitant() {
    let mut certifier = certifier();
    declared(&mut certifier, ONE_INHABITANT);
    let [u, v, this, that] = terms("(u, v, f(0), g(1))");
    let mut at = |structure: &str, this: &Term, that: &Term| {
        convert(
            &mut certifier,
            &term(&fill!("#{node = StructType} ${structure}", structure)),
            this,
            that,
        )
    };

    assert_eq!(
        [
            at("/W()", &u, &v),
            at("/W()", &this, &that),
            at("/Proved()", &u, &v),
            at("/N()", &u, &v),
            at("/Pair(#{node = StructType} /Pair({}))", &u, &v),
        ],
        [Ok(true), Ok(true), Ok(true), Ok(true), Ok(true)]
    );
    assert_eq!(
        [
            at("/V()", &u, &v),
            at("/M()", &u, &v),
            at("/Pair(#{node = StructType} /Pair(#NatType))", &u, &v),
            at("/R(0)", &u, &v),
            at("/Left(0)", &u, &v),
        ],
        [Ok(false), Ok(false), Ok(false), Ok(false), Ok(false)]
    );
}

/// `W` of a unit, `V` of a number, and `Tagged` of a unit over a parameter.
const READ_FORCED: &str = r#"
struct /W: pub Type {
    a: {},
}
struct /V: pub Type {
    a: #NatType,
}
struct /Tagged(n: #NatType): pub Type {
    a: {},
}
"#;

/// A goal's type is read forced: a member of a declaration group is folded in weak-head form, and every rule a type directs is keyed on the type's own shape. Two variables converge at a group's member that is a struct of a unit, closed or over a variable, the unit, a record of units or a function into a unit, and stay apart at one that is a struct or a record with a relevant field. The elaborator's twin of this proposition shares the name.
///
/// Mutation-checked: with the type read in weak-head form, the five held goals are refused.
#[test]
fn a_goals_type_is_read_forced() {
    let mut certifier = certifier();
    declared(&mut certifier, READ_FORCED);
    let [u, v] = terms("(u, v)");
    // `rec First : Type = <body> and Second : Type = {}; First`, the spelling a declaration in a group is referred to by.
    let mut at = |body: &str| {
        let member = term(&fill!(
            "(let First: Type = ${body} and Second: Type = {}; First)",
            body
        ));

        convert(&mut certifier, &member, &u, &v)
    };

    assert_eq!(
        [
            at("#{node = StructType} /W()"),
            at("#{node = StructType} /Tagged(k)"),
            at("{}"),
            at("{a: {}, b: {}}"),
            at("(x: #NatType) -> {}"),
        ],
        [Ok(true), Ok(true), Ok(true), Ok(true), Ok(true)]
    );
    assert_eq!(
        [at("#{node = StructType} /V()"), at("{a: #NatType, b: {}}"),],
        [Ok(false), Ok(false)]
    );
}

/// Eta by the goal's type is fired ahead of every structural rule, whatever the two sides' shapes: two stuck applications of two heads converge at a record of units, by their projections, and at a function into a unit, by their applications, where a structural comparison would set head against head and refuse. At a record with a relevant field the projections are compared, and the two stay apart. The elaborator's twin of this proposition shares the name.
#[test]
fn eta_by_the_goals_type_is_fired_whatever_the_two_sides_shapes() {
    let mut certifier = certifier();
    let [this, that] = terms("(s(0), t(1))");
    let mut at = |type_: &str| convert(&mut certifier, &term(type_), &this, &that);

    assert_eq!(
        [
            at("{a: {}, b: {}}"),
            at("(a: #NatType) -> {}"),
            at("{a: #NatType, b: {}}"),
        ],
        [Ok(true), Ok(true), Ok(false)]
    );
}

/// A literal's eta is the goal type's to refuse. At a type former that is not the literal's own the two sides are not of one type, and the neutral restriction alone would let a literal with no field — whose walk compares nothing — convert with any neutral: a lambda, the unit literal and a field-less struct's literal against a neutral at `Nat`, and that struct's literal at another struct, are refused. Each is still taken at the literal's own type, and at `Type`, which says nothing. The elaborator's twin of this proposition shares the name.
///
/// Mutation-checked: without the refusal for a lambda and a tuple the first two goals are accepted, without the struct's the next two are, and with a sort counted a former the goals at `Type` are refused.
#[test]
fn eta_by_a_literal_is_refused_at_a_type_former_that_is_not_its_own() {
    let mut certifier = certifier();
    declared(&mut certifier, FIELD_LESS_AND_ONE_FIELD);
    let [function, expansion, unit, empty, neutral] = under(
        &mut certifier,
        "f: (x: #NatType) -> #NatType",
        "f, (x: #NatType) => f(#NatAdd(x, 0)), (), /U {}, n",
    );
    let (ground, number) = (term("Type"), term("#NatType"));
    let (unit_type, field_less, one_field) = (
        term("{}"),
        term("#{node = StructType} /U()"),
        term("#{node = StructType} /S()"),
    );
    let mut at =
        |type_: &Term, this: &Term, that: &Term| convert(&mut certifier, type_, this, that);

    assert_eq!(
        [
            at(&number, &expansion, &function),
            at(&number, &unit, &neutral),
            at(&number, &empty, &neutral),
            at(&one_field, &neutral, &empty),
        ],
        [Ok(false), Ok(false), Ok(false), Ok(false)]
    );
    assert_eq!(
        [
            at(&unit_type, &unit, &neutral),
            at(&field_less, &empty, &neutral),
            at(&ground, &expansion, &function),
            at(&ground, &unit, &neutral),
            at(&ground, &neutral, &empty),
        ],
        [Ok(true), Ok(true), Ok(true), Ok(true), Ok(true)]
    );
}

/// `E`, a family with no constructor at `Type`, `S`, a structure of two numbers, and `D`, another family at `Type`.
const NEUTRALS: &str = r#"
induct /E: pub Type
end
struct /S: pub Type {
    a: #NatType,
    b: #NatType,
}
induct /D: pub Type
end
"#;

/// `match c end` at an ambient goal `${goal}`: an elimination with no arm, stuck on a variable.
const STUCK: &str = "#{result = Ambient(${goal})} match c end";

/// A literal's eta is taken against every neutral inhabitant, a term whose head does not show its type, and against no other. A stuck elimination is one as a variable is: a lambda, a tuple literal and a struct literal each converge with the stuck elimination they expand, where no type directs the rule and at the struct's own type — the lambda past a fold, since one that only forwards reduces to what it forwards — and each near miss beside them — a binder dropped, two components swapped, two fields swapped — is refused. Against a head that shows another type — a number, a lambda, a constructor's value — the rule is not taken, and the unit literal's empty walk equates it with nothing. The elaborator's twin of this proposition shares the name.
///
/// Mutation-checked: with the set narrowed to a variable, a projection and an application the four held goals are refused, and with every head counted a neutral the unit literal converges with a number.
#[test]
fn eta_by_a_literal_is_taken_against_a_neutral_alone() {
    let mut certifier = certifier();
    declared(&mut certifier, NEUTRALS);
    let stuck = |goal: &str| fill!(STUCK, goal);
    let (function, pair, structure) = (
        stuck("(x: #NatType) -> #NatType"),
        stuck("{a: #NatType, b: #NatType}"),
        stuck("#{node = StructType} /S()"),
    );
    let literal = |first: usize, second: usize| {
        fill!(
            "/S { #{field = Struct(/S, ${first})} (${structure}).${first}, #{field = Struct(/S, ${second})} (${structure}).${second} }",
            first,
            second,
            structure
        )
    };
    let terms = [
        fill!("(x: #NatType) => (${function})(#NatAdd(x, 0))", function),
        function.clone(),
        fill!("((${pair}).0, (${pair}).1)", pair),
        pair.clone(),
        literal(0, 1),
        structure.clone(),
        fill!("(x: #NatType) => (${function})(0)", function),
        fill!("((${pair}).1, (${pair}).0)", pair),
        literal(1, 0),
        "(x: #NatType) => x".to_string(),
        "()".to_string(),
        "#{node = Variant} /D/mk()".to_string(),
    ];
    let [
        applied,
        function,
        projected,
        pair,
        literal,
        structure,
        dropped,
        swapped,
        crossed,
        identity,
        unit,
        constructed,
    ] = under(
        &mut certifier,
        "c: #{node = InductType} /E()",
        &terms.join(", "),
    );
    let (ground, record, one) = (term("Type"), term("#{node = StructType} /S()"), term("1"));
    let mut at =
        |type_: &Term, this: &Term, that: &Term| convert(&mut certifier, type_, this, that);

    assert_eq!(
        [
            at(&ground, &applied, &function),
            at(&ground, &projected, &pair),
            at(&ground, &literal, &structure),
            at(&record, &literal, &structure),
        ],
        [Ok(true), Ok(true), Ok(true), Ok(true)]
    );
    assert_eq!(
        [
            at(&ground, &dropped, &function),
            at(&ground, &swapped, &pair),
            at(&ground, &crossed, &structure),
        ],
        [Ok(false), Ok(false), Ok(false)]
    );
    assert_eq!(
        [
            at(&ground, &identity, &one),
            at(&ground, &unit, &one),
            at(&ground, &unit, &identity),
            at(&ground, &literal, &constructed),
        ],
        [Ok(false), Ok(false), Ok(false), Ok(false)]
    );
}

/// A struct literal with fewer fields than its declaration must not convert with a neutral inhabitant.
///
/// The eta walk is driven by the literal's fields, so a short literal runs out before the declaration's telescope does, and a vacuous remainder that passed would equate a malformed literal with *any* neutral at the type, in the accepting direction. The walk answers with whether it consumed the whole telescope.
#[test]
fn a_short_struct_literal_does_not_convert_with_a_neutral() {
    let mut certifier = certifier();
    declared(
        &mut certifier,
        "struct /S: pub Type {\n    a: #NatType,\n    b: #NatType,\n}",
    );
    let [literal, neutral] = terms("(/S {}, s)");

    assert_eq!(
        convert(
            &mut certifier,
            &term("#{node = StructType} /S()"),
            &literal,
            &neutral
        ),
        Ok(false)
    );
}
