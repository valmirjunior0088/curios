//! Variance and cumulativity of a function type, the bare-sort head rules, the relation through a definition and under an arm's case, and the sigma and pi rules at an argument position.

use {
    crate::certifier::infer::{subsumes, test_support::*},
    crate::{test_support::*, *},
    curios_common::fill,
};

/// Whether `this` stands where `that` is wanted, each read as text.
fn below(certifier: &mut Certifier, this: &str, that: &str) -> Result<bool, Error> {
    subsumes(certifier, &term(this), &term(that))
}

/// `(x: Nat) -> ${codomain}`: a function type over a number.
fn over_a_number(codomain: &str) -> String {
    fill!("(x: #NatType) -> ${codomain}", codomain)
}

/// The subsumption fork, which the soundness board records as having no fixture in either direction.
///
/// `subsumes` compares a Π's **domains by conversion** and its **codomains cumulatively**. Only one of those two choices can be got wrong in the admitting direction, and it is the domain: reading it *covariantly* would accept `(x : Type 0) -> B` where `(x : Type 1) -> B'` is wanted, so a function that only handles small arguments would be applied to a large one, which is the shape the hierarchy exists to forbid. Contravariance would be sound and strictly more permissive; invariance is what ships, and it is the freely-revisable side precisely because widening later breaks nothing already accepted.
///
/// Both directions are asserted, because invariance is the conjunction of two refusals and a rule that had drifted to covariance would still refuse the other one. The head cases are the control: without them a `subsumes` that refused every function type would satisfy the two assertions above, and cumulativity would be dead while the test stayed green.
#[test]
fn a_function_types_domain_is_invariant() {
    let mut certifier = certifier();
    let (small, large) = ("(x: Type) -> {}", "(x: #{level = 1} Type) -> {}");

    // Neither direction: the domains are compared by conversion, and `Type 0` is not convertible with `Type 1`.
    assert_eq!(below(&mut certifier, small, large), Ok(false));
    assert_eq!(below(&mut certifier, large, small), Ok(false));
}

/// The other half of the same fork, and the direction the language needs: under a binder the codomains are still compared cumulatively, each sort in its own levels, and a proposition stands for no type there either.
#[test]
fn a_function_types_codomain_is_cumulative() {
    let mut certifier = certifier();
    let pi = over_a_number;

    assert_eq!(
        below(&mut certifier, &pi("Type"), &pi("#{level = 1} Type")),
        Ok(true)
    );
    assert_eq!(
        below(&mut certifier, &pi("#{level = 1} Type"), &pi("Type")),
        Ok(false)
    );
    assert_eq!(
        below(&mut certifier, &pi("Prop"), &pi("#{level = 1} Prop")),
        Ok(true)
    );
    // No proposition stands where a type is wanted, under a binder as at the head.
    assert_eq!(below(&mut certifier, &pi("Prop"), &pi("Type")), Ok(false));
}

/// The control for both fixtures above: the same verdicts at the head, where no telescope is walked at all. A `subsumes` that had stopped descending into function types would pass those two and fail these.
#[test]
fn the_head_rules_still_decide_a_bare_sort() {
    let mut certifier = certifier();

    assert_eq!(below(&mut certifier, "Type", "#{level = 1} Type"), Ok(true));
    assert_eq!(
        below(&mut certifier, "#{level = 1} Type", "Type"),
        Ok(false)
    );
    assert_eq!(below(&mut certifier, "Prop", "#{level = 1} Prop"), Ok(true));
    assert_eq!(below(&mut certifier, "Prop", "Type"), Ok(false));
    assert_eq!(below(&mut certifier, "Type", "Prop"), Ok(false));
}

/// `Family`, an alias of `(x: Nat) -> Type 0`, `Predicate`, of `(x: Nat) -> Prop`, and `Small`, of `Prop`.
const ALIASES: &str = r#"
let /Family: #{level = 1} Type = (x: #NatType) -> Type;
let /Predicate: #{level = 1} Type = (x: #NatType) -> Prop;
let /Small: #{level = 1} Type = Prop;
"#;

/// A definition that names a sort or a function type is subsumed as what it names: `subsumes` reads both sides forced, so an alias of `(x: Nat) -> Type 0` stands where `(x: Nat) -> Type 1` is wanted and an alias of `Prop` where `Prop 1` is, while an alias of a predicate, or of `Prop`, stands for no type on either side. Read as written, the aliases' cumulative halves would go on as the equations they are not, which conversion refuses once it has unfolded them.
///
/// Mutation-checked: with `subsumes` matching on its two sides unreduced, the alias of a function type is refused where `(x: Nat) -> Type 1` is wanted, and no other fixture of this module moves.
#[test]
fn a_definition_naming_a_sort_or_a_function_type_is_subsumed_as_what_it_names() {
    let mut certifier = certifier();
    declared(&mut certifier, ALIASES);
    let large = over_a_number("#{level = 1} Type");

    assert_eq!(below(&mut certifier, "/Family", &large), Ok(true));
    assert_eq!(below(&mut certifier, "/Predicate", &large), Ok(false));
    assert_eq!(
        below(&mut certifier, "/Small", "#{level = 1} Prop"),
        Ok(true)
    );
    assert_eq!(
        below(&mut certifier, "/Small", "#{level = 1} Type"),
        Ok(false)
    );
    assert_eq!(
        below(&mut certifier, &over_a_number("Prop"), "/Family"),
        Ok(false)
    );
}

/// Over the locals a type is read under where a split has put `k(0)` at `true` — `k: (Nat) -> Bool`, a proposition `P`, a family of small types `f: (Nat) -> Type 0` and a predicate `g: (Nat) -> Prop` — `match k(0) | false => 0 | true => let u: decided = ${held}; 0 end`, where `decided` is `match k(0) | false => Nat | true => ${upper} end` at `Type 2`: `upper` where the split's equation is in force, and neither a sort nor a function type while `k(0)` is open.
const HOLDING: &str = "(k: (n: #NatType) -> #BoolType, P: Prop, f: (n: #NatType) -> Type, g: (n: #NatType) -> Prop) => match k(0): (_) => #NatType | false => 0 | true => (let u: match k(0): (_) => #{level = 2} Type | false => #NatType | true => ${upper} end = ${held}; 0) end";

/// A type that is a sort or a function type only where an arm's case equation is in force is subsumed there as the one the case makes it, cumulatively and holding no proposition: under `k(0) = true`, `decided(Type 1)` takes `Nat` and refuses `P`, and `decided((n: Nat) -> Type 1)` takes the family of small types `f` and refuses the predicate `g`. `curios`'s `tests::rules::subsumption_tests` puts the same propositions to the elaborator under this name.
///
/// Mutation-checked: with `subsumes` matching on its two sides unreduced, `Nat` is refused at the sort the case reaches.
#[test]
fn what_an_arms_case_makes_a_sort_or_a_function_type_is_subsumed_as_one_there() {
    let judged =
        |upper: &str, held: &str| infer(&mut certifier(), &term(&fill!(HOLDING, upper, held)));
    let refused = |verdict: Result<_, Error>| matches!(verdict, Err(Error::Mismatch { .. }));
    let (one, small_family) = ("#{level = 1} Type", "(n: #NatType) -> #{level = 1} Type");

    assert!(judged(one, "#NatType").is_ok());
    assert!(refused(judged(one, "P")));
    assert!(judged(small_family, "f").is_ok());
    assert!(refused(judged(small_family, "g")));
}

/// An argument reaches the *checked* rules, not merely inference and subsumption.
///
/// `check` dispatches let-descent, Π-introduction and Σ-introduction before falling through to infer-then-subsume, and an argument position must reach them. A typing walk driving its child obligations through an explicit worklist that calls the node rule and `subsumes` directly loses all three rules at every argument, constructor payload and record field, and nothing in the prelude or the corpus reaches a shape that shows it — which is why this fixture is written against the *rule* rather than against a program.
///
/// Mutation-checked: replacing the `check` call at the application arm of `infer_within` with an inference and a `subsumes` refuses this term with a mismatch between `(Type, Nat)` and the dependent pair.
#[test]
fn a_dependent_tuple_in_argument_position_reaches_the_sigma_rule() {
    let mut certifier = certifier();
    // `f((7))`: well-typed only by opening the telescope at the first component, which is what the Σ rule does and inference cannot.
    let [call] = under(
        &mut certifier,
        &fill!("f: (p: ${pair}) -> {}", pair = DEPENDENT_PAIR),
        "f((#NatType, 7)),",
    );

    assert_eq!(infer(&mut certifier, &call), Ok(term("{}")));
}

/// The Π half of the rule above: a lambda in argument position is checked against its expected function type, so its *body* keeps the expectation.
///
/// Routing the body through `check` is what lets it reach the Σ rule with that expectation intact; inferring the lambda instead manufactures the non-dependent codomain `(Type, Nat)` first, and the mismatch surfaces at the codomain rather than at the body. The two rules compose here exactly as they do in `check`.
///
/// Mutation-checked with the fixture above, and by the same edit.
#[test]
fn a_lambda_in_argument_position_reaches_the_pi_rule() {
    let mut certifier = certifier();
    // `g((n) => (n))`: the body is a dependent pair, and only the checked route hands it the expectation that makes it one.
    let [call] = under(
        &mut certifier,
        &fill!(
            "g: (k: (n: #NatType) -> ${pair}) -> {}",
            pair = DEPENDENT_PAIR
        ),
        "g((n: #NatType) => (#NatType, n)),",
    );

    assert_eq!(infer(&mut certifier, &call), Ok(term("{}")));
}

/// The codomain rule reaches no function type across a mark: subsumption is cumulative under a binder only where the two telescopes state one calling convention, and a pair apart in a mark goes on as the equation it is, which refuses it. Read past the mark, `(@x: Nat) -> Type 0` would stand where `(x: Nat) -> Type 1` is wanted. The control is the same pair under one mark, which the codomain rule accepts.
#[test]
fn a_function_type_is_below_none_of_other_marks() {
    let mut certifier = certifier();
    let hidden = "(@x: #NatType) -> Type";

    assert_eq!(
        below(
            &mut certifier,
            hidden,
            "(@x: #NatType) -> #{level = 1} Type"
        ),
        Ok(true)
    );
    assert_eq!(
        below(&mut certifier, hidden, "(x: #NatType) -> #{level = 1} Type"),
        Ok(false)
    );
}
