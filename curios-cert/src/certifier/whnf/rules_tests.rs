//! Beta, delta, zeta, iota, projection, eta and the switch arms — one reduction rule each.

use {
    super::{test_support::*, *},
    crate::test_support::*,
    curios_common::fill,
    curios_core::{Apply, Reducer, Subterm, Term},
};

/// `f`, a definition of `3` generalized over one universe parameter.
const POLYMORPHIC: &str = "#{universe_context = {parameter_count = 1}}\nlet /f: #NatType = 3;";

#[test]
fn beta_opens_a_function_over_its_arguments() {
    let mut certifier = certifier();

    assert_eq!(
        whnf(&mut certifier, term("((x: #NatType) => x)(7)")),
        Ok(term("7"))
    );
}

#[test]
fn delta_unfolds_a_monomorphic_definition() {
    let mut certifier = certifier();
    declared(&mut certifier, "let /f: #NatType = 3;");

    assert_eq!(whnf(&mut certifier, term("/f")), Ok(term("3")));
}

/// A definition generalized over universe parameters denotes no particular instance, so a reference to it stating none is a normal form. Unfolding it here would silently pick an instance nobody stated.
#[test]
fn delta_withholds_a_universe_polymorphic_definition() {
    let mut certifier = certifier();
    declared(&mut certifier, POLYMORPHIC);

    let occurrence = term("/f");
    assert_eq!(whnf(&mut certifier, occurrence.clone()), Ok(occurrence));
}

/// The same definition *does* unfold through a stated instance, which is the one position that names which one it is.
#[test]
fn a_universe_instance_unfolds_what_a_bare_occurrence_withholds() {
    let mut certifier = certifier();
    declared(&mut certifier, POLYMORPHIC);

    assert_eq!(
        whnf(&mut certifier, term("#{universes = {levels = [0]}} /f")),
        Ok(term("3"))
    );
}

#[test]
fn an_undefined_variable_is_its_own_normal_form() {
    let mut certifier = certifier();

    let occurrence = term("x");
    assert_eq!(whnf(&mut certifier, occurrence.clone()), Ok(occurrence));
}

/// Zeta. The second binding refers to the first, so this also pins the left-to-right order: `y` must see `x`'s value, not `x` itself.
#[test]
fn zeta_substitutes_let_bindings_left_to_right() {
    let mut certifier = certifier();

    let block = term("(let x: #NatType = 2; let y: #NatType = #NatAdd(x, 3); y)");

    assert_eq!(whnf(&mut certifier, block), Ok(term("5")));
}

/// The intrinsic folds are shared with the elaborator through `Reducer`; this is the certifier reaching them with its own strategy underneath.
#[test]
fn intrinsics_fold_through_the_reducer_seam() {
    let mut certifier = certifier();
    declared(&mut certifier, "let /x: #NatType = 2;");

    assert_eq!(whnf(&mut certifier, term("#NatAdd(/x, 2)")), Ok(term("4")));
}

#[test]
fn iota_selects_an_inductive_arm_and_binds_its_payload() {
    let mut certifier = certifier();

    let matched = term(
        "match #{node = Variant} /E/some(42): (m) => #NatType | none() => 0 | some(a) => a end",
    );

    assert_eq!(whnf(&mut certifier, matched), Ok(term("42")));
}

#[test]
fn iota_peels_one_successor_off_a_nat() {
    let mut certifier = certifier();

    // `match 3 { 0 => 0 | succ(pred, _) => pred }` peels exactly one layer.
    let matched = term("match 3: (m) => #NatType | 0 => 0 | pred + 1 => pred end");

    assert_eq!(whnf(&mut certifier, matched), Ok(term("2")));
}

/// The switch of `scrutinee` over the literals `1` and `2`, falling through to `99`.
fn switch(scrutinee: &str) -> Term {
    term(&fill!(
        "match ${scrutinee}: (m) => #NatType | 1 => 10 | 2 => 20 | _ => 99 end",
        scrutinee
    ))
}

#[test]
fn a_switch_takes_the_case_a_literal_names() {
    let mut certifier = certifier();

    assert_eq!(whnf(&mut certifier, switch("2")), Ok(term("20")));
}

#[test]
fn a_switch_falls_through_to_its_default() {
    let mut certifier = certifier();

    assert_eq!(whnf(&mut certifier, switch("7")), Ok(term("99")));
}

/// A symbolic scrutinee decides nothing, so the switch rebuilds as the neutral term it is rather than guessing an arm.
#[test]
fn a_switch_on_a_symbolic_scrutinee_stays_stuck() {
    let mut certifier = certifier();

    let reduced = whnf(&mut certifier, switch("n")).expect("a stuck term still reduces");
    assert!(matches!(&*reduced, Subterm::Match(_)));
}

#[test]
fn a_bool_match_dispatches_on_a_literal() {
    let mut certifier = certifier();

    let matched = term("match true: (m) => #NatType | false => 0 | true => 1 end");

    assert_eq!(whnf(&mut certifier, matched), Ok(term("1")));
}

#[test]
fn projection_selects_a_tuple_field() {
    let mut certifier = certifier();

    assert_eq!(whnf(&mut certifier, term("(10, 20, 30).1")), Ok(term("20")));
}

/// A constructor is projected through the flat runtime view `(tag, payload...)`, so field 1 is payload component 0 — unlike a struct, which has no tag to skip.
#[test]
fn projection_skips_a_variants_tag_but_not_a_structs() {
    let mut certifier = certifier();

    let variant = term("(#{node = Variant} /E/some(42)).1");
    assert_eq!(whnf(&mut certifier, variant), Ok(term("42")));

    let struct_ = term("#{field = Struct(/E, 0)} /E { 42 }.0");
    assert_eq!(whnf(&mut certifier, struct_), Ok(term("42")));
}

/// Under the binder's own mark a forwarding contracts, and across one it stays as written: `(@x) => f(x)` is a function at `(@x: Nat) -> …`, and `f` one at another type. `f` is assumed, so no binder a probe mints is `f` again.
#[test]
fn eta_contracts_a_function_that_only_forwards() {
    let mut certifier = certifier();
    let [f, forwarding, hidden, across] = under(
        &mut certifier,
        "f: (x: #NatType) -> #NatType",
        "f, (x: #NatType) => f(x), (@x: #NatType) => f(@x), (@x: #NatType) => f(x)",
    );

    assert_eq!(whnf(&mut certifier, forwarding), Ok(f.clone()));
    assert_eq!(whnf(&mut certifier, hidden), Ok(f));
    assert_eq!(whnf(&mut certifier, across.clone()), Ok(across));
}

/// The side condition is load-bearing: contracting `(x) => x(x)` would move an occurrence of `x` out from under the binder that gives it meaning.
#[test]
fn eta_declines_when_the_head_mentions_the_binder() {
    let mut certifier = certifier();

    let function = term("(x: #NatType) => x(x)");

    assert_eq!(whnf(&mut certifier, function.clone()), Ok(function));
}

/// A forwarding's argument may be an index no binder of the term binds — one the boundary refuses, which reduction takes all the same: the lambda's own binder is no identity it can be compared with, so the lambda is its own normal form rather than an abort.
#[test]
fn an_eta_probes_argument_may_be_a_bound_index() {
    let mut certifier = certifier();

    let function = term("(x: #NatType) => (#{binding = Bound(3)} _)(#{binding = Bound(2)} _)");

    assert_eq!(whnf(&mut certifier, function.clone()), Ok(function));
}

/// Eta hands back the head as the occurrence spells it, and a reference to a declaration is its instance: `(x) => f@{3}(x)` contracts to `f@{3}`, which unfolds at level 3, where the bare `f` the contraction must not invent stays stuck.
///
/// Asked of an `Instance` head because that is the shape every reference to a declaration takes ([`curios_core::Instance`]): the node carries the levels and has no term child, so the probe's side condition that the head mentions none of the binders is vacuous on it, and the per-argument identity check is the whole of what decides. A contraction that handed back the declaration without its levels would hand every later reader a reference to no instance of the scheme — the term `delta_withholds_a_universe_polymorphic_definition` holds stuck — and one that read the levels off the wrong occurrence would be a false equation between two instances.
///
/// Across a mark it declines as it does for a local head: `(@x) => f@{3}(x)` is a function at `(@x : Nat) -> …` and `f@{3}` one at a type plicity holds apart.
///
/// Asked of both statements of the rule, which must agree: the forwarding whose domain names an assumed local is one the closed machine's gate declines, so `step_func` decides it, and the local-free one is the machine's own `curios_core::eta_head`.
///
/// Mutation-checked, in the open case: with `step_func` handing back `Term::instance(name, Universes::default())` for an `Instance` head, the open forwarding reduces to the stuck bare occurrence instead of to `Type 3`.
#[test]
fn eta_contracts_to_the_instance_the_occurrence_states() {
    let mut certifier = certifier();
    declared(
        &mut certifier,
        "#{universe_context = {parameter_count = 1}}\nlet /f: #NatType = #{level = u} Type;",
    );
    let at = |level: u32| fill!("(#{universes = {levels = [${level}]}} /f)", level);

    // The domain carries the assumed local in the open case, which is what the machine's gate reads, and plays no part in the probe.
    for (label, domain) in [("open", "#NatAdd(y, 1)"), ("local-free", "#NatType")] {
        let [three, one, bare, across] = under(
            &mut certifier,
            "y: #NatType",
            &fill!(
                "(x: ${domain}) => ${three}(x), (x: ${domain}) => ${one}(x), (x: ${domain}) => /f(x), (@x: ${domain}) => ${three}(x)",
                domain,
                three = at(3),
                one = at(1)
            ),
        );

        assert_eq!(
            whnf(&mut certifier, three),
            Ok(term("#{level = 3} Type")),
            "{label}: the contraction did not keep the instance's levels",
        );
        assert_eq!(
            whnf(&mut certifier, one),
            Ok(term("#{level = 1} Type")),
            "{label}: the contraction read another occurrence's levels",
        );
        assert_eq!(
            whnf(&mut certifier, bare),
            Ok(term("/f")),
            "{label}: the contraction invented an instance the occurrence does not state",
        );
        assert_eq!(
            whnf(&mut certifier, across.clone()),
            Ok(across),
            "{label}: a forwarding across a mark contracted",
        );
    }
}

/// A recursive call keeps its folded spelling until an eliminator demands the value, which is what stops an occurrence from unfolding forever.
#[test]
fn a_recursive_application_stays_folded_until_forced() {
    let mut certifier = certifier();
    let countdown = |argument: &str| {
        term(&fill!(
            "#{node = Rec} (let countdown: (n: #NatType) -> #NatType = (n: #NatType) => match n: (m) => #NatType | 0 => 0 | pred + 1 => countdown(pred) end; countdown(${argument}))",
            argument
        ))
    };

    // Applied to a symbolic argument, it reduces to a folded recursive call.
    let Subterm::Rec(rec) = Term::unwrap_or_clone(countdown("x")) else {
        unreachable!("written as a rec")
    };
    let reduced = whnf(&mut certifier, unfold_rec(rec)).expect("ordinary reduction terminates");
    assert!(matches!(
        &*reduced,
        Subterm::Apply(Apply { head, .. }) if head.as_rec_proj().is_some()
    ));

    // Applied to a literal, forcing runs it to the end.
    assert_eq!(certifier.reduce_forced(countdown("3")), Ok(term("0")));
}

/// A member whose result is a function, forced where it is applied past its own parameters: `f(2)(y)` is the call `f(2)` applied to `y`, so the force reaches through the outer application to the call, and `y`, symbolic, rides along to the answer. Read one level deep, the outer application would be a neutral the force hands back folded.
#[test]
fn a_recursive_call_applied_past_its_parameters_unfolds_when_forced() {
    let mut certifier = certifier();
    let [y, applied] = under(
        &mut certifier,
        "y: #NatType",
        "y, #{node = Rec} (let f: (n: #NatType) -> (x: #NatType) -> #NatType = (n: #NatType) => match n: (m) => (x: #NatType) -> #NatType | 0 => (x: #NatType) => x | pred + 1 => (x: #NatType) => f(pred)(x) end; f(2)(y))",
    );

    assert_eq!(certifier.reduce_forced(applied), Ok(y));
}

/// A boolean operation reduces its right operand only once its left is a literal. The left here is a local, so the right — a fold that would answer `true` — is handed back as written; with the left `true`, the same right folds and so does the whole. This is the rule that keeps weak-head reduction of a `&&`/`||` tree from being its full normalization, which on a web of predicate definitions naming each other twice would be `2^n` under every demand — see `reduce_bool_binary`.
#[test]
fn a_stuck_left_operand_leaves_the_right_as_written() {
    let mut certifier = certifier();
    let [stuck, open] = under(
        &mut certifier,
        "x: #BoolType",
        "#BoolAnd(x, #BoolAnd(true, true)), #BoolAnd(true, #BoolAnd(true, true))",
    );

    assert_eq!(
        whnf(&mut certifier, stuck.clone()),
        Ok(stuck),
        "a stuck left settles the fold, and the right is not reduced for an answer it cannot change"
    );
    assert_eq!(
        whnf(&mut certifier, open),
        Ok(term("true")),
        "a literal left reads the right and folds"
    );
}

/// The line the rule above draws: an equality reads its right operand under a stuck left, because its laws read it — `x == true` is `x` — where a connective's laws would only meet a leaf the tree flattening forces later. No predicate web is built out of equalities, so the cliff the connectives step back from is not here.
#[test]
fn an_equality_reads_its_right_operand() {
    let mut certifier = certifier();
    let [x, equality] = under(
        &mut certifier,
        "x: #BoolType",
        "x, #BoolEql(x, #BoolAnd(true, true))",
    );

    assert_eq!(
        whnf(&mut certifier, equality),
        Ok(x),
        "the right folds to `true`, and `x == true` is `x`"
    );
}

#[test]
fn an_application_whose_group_dissolved_to_its_member_still_unfolds() {
    let mut certifier = certifier();
    // The elaborator's twin, put to the certifier: a group whose member never mentions itself has no fixed point to keep, so opening its tail reduces past the projection and leaves the member's own `Func`. Declining every head that is not still a projection would keep the folded spelling with that `Func` in hand, and the two checkers would read one declaration two ways.
    let [value, applied] = under(
        &mut certifier,
        "value: #NatType",
        "value, (#{node = Rec} (let unused: (n: #NatType) -> #NatType = (n: #NatType) => n; (n: #NatType) => n))(value)",
    );
    let Subterm::Apply(apply) = Term::unwrap_or_clone(applied) else {
        unreachable!("written as an application")
    };

    assert_eq!(unfold_rec_apply(&mut certifier, apply), Ok(Some(value)));
}
