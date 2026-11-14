//! Recursive heads: alpha-variant groups, two groups compared as binders, a folded call compared against its unfolding, and the pair unfolded against itself once.

use {
    crate::certifier::convert::{Pairs, convert_intrinsic, test_support::*},
    crate::{test_support::*, *},
    curios_analysis::test_support::*,
    curios_common::fill,
    curios_core::{
        Free, Intrinsic, Level, Subterm, Term, UniverseConstraint, UniverseContext, UniverseParam,
    },
};

/// `lower ≤ upper` between two of the item's own universe parameters, as `assume_universes` takes it.
fn between(lower: usize, upper: usize) -> UniverseConstraint {
    UniverseConstraint {
        lower: Level::param(UniverseParam(lower)),
        upper: Level::param(UniverseParam(upper)),
    }
}

/// The intrinsic node `term` is, for the rule that compares two of them.
fn intrinsic(term: &Term) -> Intrinsic {
    match &**term {
        Subterm::Intrinsic(node) => node.clone(),
        _ => panic!("an intrinsic: {term:?}"),
    }
}

/// `countdown`, a group descending from its argument to `0`, at a word `${n}`, its motive's `${m}`, its predecessor's `${pred}` and hypothesis's `${ih}`, called at `x`: one recursion under any words.
const COUNTDOWN: &str = "#{node = Rec} (let ${member}: (${n}: #NatType) -> #NatType = (${n}: #NatType) => match ${n}: (${m}) => #NatType | 0 => 0 | ${pred} + 1; ${ih} => ${member}(${pred}) end; ${member}(${argument}))";

/// Binder *identity* must not leak into conversion. Two `rec` groups written with different minted names are the same group: binder names are display hints, and the bodies are de Bruijn-indexed under their scopes.
///
/// This is the property that lets a folded recursive call be compared structurally at all — see the projection arm, which requires the groups to be equal.
#[test]
fn two_alpha_variant_recursive_groups_convert() {
    let mut certifier = certifier();
    let [left, right] = terms(&fill!(
        "(${left}, ${right})",
        left = fill!(
            COUNTDOWN,
            member = "countdown",
            n = "n",
            m = "m",
            pred = "pred",
            ih = "ih",
            argument = "x"
        ),
        right = fill!(
            COUNTDOWN,
            member = "loop",
            n = "k",
            m = "motive",
            pred = "p",
            ih = "rest",
            argument = "x"
        )
    ));

    assert_eq!(
        convert(&mut certifier, &term("#NatType"), &left, &right),
        Ok(true)
    );
}

/// A recursive call applied to a symbolic argument stays folded, and comparing it with itself terminates rather than unfolding in lockstep forever.
#[test]
fn a_folded_recursive_call_converts_without_unfolding_forever() {
    let mut certifier = Certifier::new(10_000, SYNTAX);
    let called = |argument: &str| {
        fill!(
            COUNTDOWN,
            member = "countdown",
            n = "n",
            m = "m",
            pred = "pred",
            ih = "ih",
            argument
        )
    };
    let [folded, same_shape_other_argument] =
        terms(&fill!("(${x}, ${y})", x = called("x"), y = called("y")));
    let nat = term("#NatType");

    assert_eq!(
        convert(&mut certifier, &nat, &folded, &folded.clone()),
        Ok(true),
    );
    assert_eq!(
        convert(&mut certifier, &nat, &folded, &same_shape_other_argument),
        Ok(false),
    );
}

/// `rec f : (n: Nat) -> Nat = (n) => match n | 0 => base | pred + 1 => f(pred) + by; f`, its step's sum written `f(pred) + by` or, commuted, `by + f(pred)`: one recursion in two spellings no rename relates.
fn stepping(base: usize, by: usize, commuted: bool) -> String {
    let step = match commuted {
        false => fill!("#NatAdd(f(pred), ${by})", by),
        true => fill!("#NatAdd(${by}, f(pred))", by),
    };

    fill!(
        "#{node = Rec} (let f: (n: #NatType) -> #NatType = (n: #NatType) => match n: (m) => #NatType | 0 => ${base} | pred + 1; ih => ${step} end; f)",
        base,
        step
    )
}

/// The two groups `left` and `right` written, bare and called at an assumed `x`, opened in `certifier`.
fn bare_and_called(certifier: &mut Certifier, left: &str, right: &str) -> [Term; 4] {
    under(
        certifier,
        "x: #NatType",
        &fill!(
            "${left}, ${right}, (${left})(x), (${right})(x)",
            left,
            right
        ),
    )
}

/// Two recursive groups are compared as the binders they are: the same member of two groups is one where, member for member, their types and their bodies convert under one shared set of binders. The two here are one recursion whose step is `f(pred) + 2` in one and `2 + f(pred)` in the other, so no spelling relates them, and an unfolding of both poses the same two calls under fresh binders without end. They are compared bare, at their function type, and as two calls, whose heads are compared before their spines. The elaborator's twin shares the name.
///
/// Mutation-checked: with the comparison removed the budget, small here, runs out on both; with it holding any two groups of one shape, `two_groups_whose_bodies_part_are_apart` fails.
#[test]
fn two_groups_whose_bodies_convert_are_one() {
    let mut certifier = Certifier::new(10_000, SYNTAX);
    let [left, right, left_called, right_called] = bare_and_called(
        &mut certifier,
        &stepping(0, 2, false),
        &stepping(0, 2, true),
    );
    let function = term("(n: #NatType) -> #NatType");

    assert_eq!(convert(&mut certifier, &function, &left, &right), Ok(true));
    assert_eq!(
        convert(
            &mut certifier,
            &term("#NatType"),
            &left_called,
            &right_called
        ),
        Ok(true),
    );
}

/// The control: two groups of one shape whose base cases part are two functions, bare and at the head of two calls. The comparison refuses them at the base case, and so does the unfolding it leaves them to, a fold's base case being compared before its step.
#[test]
fn two_groups_whose_bodies_part_are_apart() {
    let mut certifier = Certifier::new(10_000, SYNTAX);
    let [left, right, left_called, right_called] = bare_and_called(
        &mut certifier,
        &stepping(0, 2, false),
        &stepping(1, 2, true),
    );
    let function = term("(n: #NatType) -> #NatType");

    assert_eq!(convert(&mut certifier, &function, &left, &right), Ok(false));
    assert_eq!(
        convert(
            &mut certifier,
            &term("#NatType"),
            &left_called,
            &right_called
        ),
        Ok(false),
    );
}

/// `rec f : (n: Nat, x: Nat) -> Nat = (n, x) => match n | 0 => x | pred + 1 => f(pred, x); f`: a recursion on `n` that hands `x` back.
const HANDING_BACK: &str = "#{node = Rec} (let f: (n: #NatType, x: #NatType) -> #NatType = (n: #NatType, x: #NatType) => match n: (m) => #NatType | 0 => x | pred + 1; ih => f(pred, x) end; f)";

/// [`HANDING_BACK`] with its two parameters in the other order: one recursion over two orders of its arguments.
const HANDING_BACK_FLIPPED: &str = "#{node = Rec} (let f: (x: #NatType, n: #NatType) -> #NatType = (x: #NatType, n: #NatType) => match n: (m) => #NatType | 0 => x | pred + 1; ih => f(x, pred) end; f)";

/// `rec g : (x: Nat, y: Nat, z: Nat) -> Nat = (x, y, z) => match z | 0 => 0 | pred + 1 => g(y, 0, pred); g`: a recursion that discards `x` at its first step and `y` at its second, so two calls apart in `x` are one after one unfolding, and two apart in `y` after two.
const SHIFTING: &str = "#{node = Rec} (let g: (x: #NatType, y: #NatType, z: #NatType) -> #NatType = (x: #NatType, y: #NatType, z: #NatType) => match z: (m) => #NatType | 0 => 0 | pred + 1; ih => g(y, 0, pred) end; g)";

/// A pair of calls is unfolded against itself once, and met again inside that unfolding it is apart. The first two are one recursion over two orders of its arguments, which no comparison as binders holds, one binder standing for both members where their calls take their arguments in two orders. Unfolded, they pose their two heads again one binder further in, and would again at every round, in a scope no round has met. The second two are two calls of one member that a second unfolding would reconcile: the first poses the same member against itself at two spines, and that is where it stops. What holds either pair is an induction, which a lemma states, and the calls one unfolding reconciles are still one. The elaborator's twin shares the name.
///
/// Mutation-checked: with a pair unfolded however often it is met, the budget, small here, runs out on the first two and the second two convert.
#[test]
fn a_pair_met_again_inside_its_own_unfolding_is_apart() {
    let mut certifier = Certifier::new(10_000, SYNTAX);
    let [handed, flipped, at_a_c, at_b_d, again_at_a_c, at_b_c] = under(
        &mut certifier,
        "a: #NatType, b: #NatType, c: #NatType, d: #NatType, n: #NatType",
        &fill!(
            "(${handing})(n, a), (${flipped})(a, n), (${shifting})(a, c, n), (${shifting})(b, d, n), (${shifting})(a, c, n), (${shifting})(b, c, n)",
            handing = HANDING_BACK,
            flipped = HANDING_BACK_FLIPPED,
            shifting = SHIFTING
        ),
    );
    let nat = term("#NatType");

    assert_eq!(convert(&mut certifier, &nat, &handed, &flipped), Ok(false));
    assert_eq!(convert(&mut certifier, &nat, &at_a_c, &at_b_d), Ok(false));
    assert_eq!(
        convert(&mut certifier, &nat, &again_at_a_c, &at_b_c),
        Ok(true)
    );
}

/// A verdict that rested on a pair left apart is not remembered. Two calls are apart inside their own unfolding and nowhere else, so a refusal that met one is true of the path it was reached on, and it is filed nowhere — where a comparison that met none is filed, and forgotten with the budget.
///
/// Mutation-checked: with a verdict filed whatever was left apart while deciding it, the two calls are remembered apart; and with the declaration's verdicts kept across a restore, the control still answers after it.
#[test]
fn a_verdict_that_rested_on_a_pair_left_apart_is_not_remembered() {
    let mut certifier = Certifier::new(10_000, SYNTAX);
    let [left, right] = under(
        &mut certifier,
        "n: #NatType, a: #NatType",
        &fill!(
            "(${handing})(n, a), (${flipped})(a, n)",
            handing = HANDING_BACK,
            flipped = HANDING_BACK_FLIPPED
        ),
    );
    let nat = term("#NatType");

    assert_eq!(convert(&mut certifier, &nat, &left, &right), Ok(false));
    assert_eq!(
        certifier
            .convert_hit(&nat, &left, &right)
            .map(|remembered| remembered.verdict),
        None
    );

    let ground = term("Type");
    let redex = term("((x: Type) => x)(#NatType)");
    assert_eq!(convert(&mut certifier, &ground, &redex, &nat), Ok(true));
    assert_eq!(
        certifier
            .convert_hit(&ground, &redex, &nat)
            .map(|remembered| remembered.verdict),
        Some(true)
    );

    certifier.restore_budget();
    assert_eq!(
        certifier
            .convert_hit(&ground, &redex, &nat)
            .map(|remembered| remembered.verdict),
        None
    );
}

/// Conversion keeps the constant function apart from the identity whatever index the local it mentions was minted at.
///
/// A positive control for capture-avoidance. Eta at a function type opens a binder that would alias a free local the certifier could mint again — `(x) => y` and `(x) => x` become the same term the moment the opened binder *is* `y` — so `y` is assumed at each of the first identities a fresh certifier hands out, one of which is the binder the comparison opens, the constant naming it by the identity the text states. `Certifier::assume` raises the counter past it, so that binder is another. Mutation-checked: an `assume` that leaves the counter alone makes the two convertible.
#[test]
fn conversion_separates_a_constant_from_the_identity_at_every_colliding_index() {
    let nat = term("#NatType");
    let identity = term("(x: #NatType) => x");
    let function = term("(x: #NatType) -> #NatType");

    for index in 0..16 {
        let mut certifier = Certifier::new(100_000, SYNTAX);
        certifier.assume(&Free::local(index, Some("y")), &nat);
        let constant = term(&fill!(
            "(x: #NatType) => #{binding = Free(${index})} y",
            index
        ));

        assert!(
            !convert(&mut certifier, &function, &constant, &identity)
                .expect("the comparison completes"),
            "the constant function over local {index} and the identity are not convertible",
        );
    }
}

/// Two recursive types of two shapes that unfold alike are apart: no goal is held for having been met before. Both sides are the type `X = (X) -> Nat`, spelled as groups of two lengths — the right carries a second, unused member — so no position pairs them as binders, and forced against each other each is a function type whose domain poses the two again. Inside their own unfolding the pair is apart. What the two spellings share is a coinduction, which no rule of conversion checks. The elaborator's twin shares the name.
///
/// Mutation-checked: with a pair unfolded however often it is met, the budget, small here, runs out.
#[test]
fn two_groups_of_two_shapes_that_unfold_alike_are_apart() {
    let mut certifier = Certifier::new(10_000, SYNTAX);

    assert_eq!(
        convert(
            &mut certifier,
            &term("Type"),
            &term("#{node = Rec} (let a: Type = (x: a) -> #NatType; a)"),
            &term("(let b: Type = (y: b) -> #NatType and unused: Type = #NatType; b)"),
        ),
        Ok(false),
    );
}

/// `rec f : (t: Type⟨${level}⟩, x: Nat, y: Nat) -> Nat = (t, x, y) => match y | 0 => 0 | p + 1 => f(t, 0, p); f` — the projection of a one-member group whose only universe data is its level, so two of them differ in nothing but their instance. The recursive call keeps the body restuck, so forcing returns the folded spelling, and it discards `x`, so two calls differing only there converge after one unfolding.
const POLYMORPHIC_FOLD: &str = "#{node = Rec} (let f: (t: #{level = ${level}} Type, x: #NatType, y: #NatType) -> #NatType = (t: #{level = ${level}} Type, x: #NatType, y: #NatType) => match y: (m) => #NatType | 0 => 0 | pred + 1; ih => f(t, 0, pred) end; f)";

/// [`POLYMORPHIC_FOLD`] at `level` applied to `Nat`, a discarded argument and a symbolic count, each a word over the hypotheses the caller opens.
fn fold_call(level: &str, discarded: &str) -> String {
    fill!(
        "(${fold})(#NatType, ${discarded}, n)",
        fold = fill!(POLYMORPHIC_FOLD, level),
        discarded
    )
}

/// Two instances of one recursive group are one term when the item's hypotheses force their levels equal: the group is decided by `rec_instances` — as an applied head, and bare at `Type` — with the equation every other level-bearing head already gets, rather than unfolded. The elaborator's twin of this proposition shares the name.
#[test]
fn two_instances_of_one_recursive_group_convert_when_their_levels_are_equal_under_the_hypotheses() {
    let mut certifier = certifier();
    certifier.assume_universes(&UniverseContext {
        parameter_count: 2,
        quality_count: 0,
        constraints: vec![between(0, 1), between(1, 0)],
    });
    let [at_u, at_v] = under(
        &mut certifier,
        "a: #NatType, n: #NatType",
        &fill!(
            "${u}, ${v}",
            u = fold_call("u", "a"),
            v = fold_call("v", "a")
        ),
    );

    assert_eq!(
        convert(&mut certifier, &term("#NatType"), &at_u, &at_v),
        Ok(true),
        "the same fold at two levels the hypotheses equate did not converge",
    );
    assert_eq!(
        convert(
            &mut certifier,
            &term("Type"),
            &term(&fill!(POLYMORPHIC_FOLD, level = "u")),
            &term(&fill!(POLYMORPHIC_FOLD, level = "v")),
        ),
        Ok(true),
        "the bare projections were not decided before reduction opened them",
    );
}

/// Two instances of one recursive group whose levels the hypotheses do not force equal are refused at the head, without an unfolding: each unfolding reproduces the same two instances on the recursive call, so a retry could only pose them again. The elaborator's twin shares the name.
#[test]
fn two_instances_of_one_recursive_group_at_unequal_levels_are_refused_without_unfolding() {
    let mut certifier = Certifier::new(10_000, SYNTAX);
    certifier.assume_universes(&UniverseContext {
        parameter_count: 2,
        quality_count: 0,
        constraints: vec![between(0, 1)],
    });
    let [at_u, at_v] = under(
        &mut certifier,
        "a: #NatType, n: #NatType",
        &fill!(
            "${u}, ${v}",
            u = fold_call("u", "a"),
            v = fold_call("v", "a")
        ),
    );

    assert_eq!(
        convert(&mut certifier, &term("#NatType"), &at_u, &at_v),
        Ok(false),
        "two instances related only by cumulativity were not refused outright",
    );
}

/// The retry the head rule must not take away: two calls of one group at one level whose spines differ only in an argument the fold discards still converge, through the one unfolding that discards it.
#[test]
fn two_calls_of_one_recursive_group_with_different_spines_still_retry_through_an_unfolding() {
    let mut certifier = certifier();
    certifier.assume_universes(&UniverseContext {
        parameter_count: 1,
        quality_count: 0,
        constraints: Vec::new(),
    });
    let [at_a, at_b] = under(
        &mut certifier,
        "a: #NatType, n: #NatType, b: #NatType",
        &fill!(
            "${a}, ${b}",
            a = fold_call("u", "a"),
            b = fold_call("u", "b")
        ),
    );

    assert_eq!(
        convert(&mut certifier, &term("#NatType"), &at_a, &at_b),
        Ok(true),
        "a spine difference the fold discards stopped being reconciled by unfolding",
    );
}

/// **Two calls of one recursive group differing only in a proof convert by their spines, the proof compared at the member's own type.** A `rec` projection carries its member type, so `spine_telescope` reads the telescope there as it does off a variable's declaration. `tests::rules`'s `a_recursive_function_carrying_a_proof_converts_without_unfolding` is the program, reaching the group through a global it is the value of; `two_calls_of_one_recursive_group_with_different_spines_still_retry_through_an_unfolding`, whose spines differ in an argument the fold discards, is the control that a mismatch still unfolds.
///
/// Mutation-checked against the two halves of the rule, neither of which this goal holds alone: with no telescope read off a `rec` projection it still passes, the two proofs compared at `Type` and one by the proposition a lookup types both at (`by_their_own_type`), and removing the spine attempt before forcing leaves it passing — that half is `irrelevance`'s `a_definition_applied_to_two_proofs_converts_before_unfolding`, which fails under it.
#[test]
fn two_calls_of_one_recursive_group_at_two_proofs_convert_without_unfolding() {
    let mut certifier = Certifier::new(10_000, SYNTAX);
    declared(&mut certifier, "induct /P: pub Prop\nend");
    let proposition = "#{node = InductType} /P()";
    let group = fill!(
        "#{node = Rec} (let countdown: (n: #NatType, p: ${proposition}) -> #NatType = (n: #NatType, p: ${proposition}) => match n: (m) => #NatType | 0 => 0 | pred + 1; ih => countdown(pred, p) end; countdown)",
        proposition
    );
    let [left, right] = under(
        &mut certifier,
        &fill!(
            "x: #NatType, p: ${proposition}, q: ${proposition}",
            proposition
        ),
        &fill!("(${group})(x, p), (${group})(x, q)", group),
    );

    assert_eq!(
        convert(&mut certifier, &term("#NatType"), &left, &right),
        Ok(true),
    );
}

/// `x + g(a, c, n)` and `x + g(b, c, n)`, two sums over two calls of [`SHIFTING`], opened in `certifier`.
fn sums_over_two_calls(certifier: &mut Certifier) -> [Intrinsic; 2] {
    under(
        certifier,
        "x: #NatType, a: #NatType, b: #NatType, c: #NatType, n: #NatType",
        &fill!(
            "#NatAdd(x, (${shifting})(a, c, n)), #NatAdd(x, (${shifting})(b, c, n))",
            shifting = SHIFTING
        ),
    )
    .map(|sum| intrinsic(&sum))
}

/// What the intrinsic rule hands on is compared inside the pairs of the goal that produced it, not from none. With `g` against itself already being unfolded, `x + g(a, c, n)` against `x + g(b, c, n)` comes down to the two calls, which one unfolding reconciles and which are apart inside the unfolding they stand in. Compared from no pair, the same two sums are one.
#[test]
fn what_the_intrinsic_rule_hands_on_is_compared_inside_the_pairs_of_its_goal() {
    let mut certifier = certifier();
    let [left, right] = sums_over_two_calls(&mut certifier);

    let mut pairs = Pairs::default();
    assert!(pairs.unfold(&term(SHIFTING), &term(SHIFTING)));
    assert_eq!(
        convert_intrinsic(&mut certifier, &mut pairs, &left, &right),
        Ok(false),
        "the two calls were unfolded inside their own unfolding",
    );
    assert_eq!(
        convert_intrinsic(&mut certifier, &mut Pairs::default(), &left, &right),
        Ok(true),
        "the control: from no pair, the two calls are one after one unfolding",
    );
}

/// A verdict reached across a pair left apart is no answer from no pair. `x + g(a, c, n)` against `x + g(b, c, n)`, asked first inside `g`'s unfolding against itself, where the two calls are apart, and then from no pair, where one unfolding reconciles them, is one there, with the memos on as with them off.
///
/// Mutation-checked: with `compare` filing a verdict whether or not `Pairs::apart` moved across it, the comparison from no pair answers the `false` filed inside the unfolding.
#[test]
fn a_verdict_reached_across_a_pair_left_apart_is_no_answer_from_no_pair() {
    for mut certifier in [certifier(), Certifier::uncached(100_000, SYNTAX)] {
        let [left, right] = sums_over_two_calls(&mut certifier);

        let mut pairs = Pairs::default();
        assert!(pairs.unfold(&term(SHIFTING), &term(SHIFTING)));
        assert_eq!(
            convert_intrinsic(&mut certifier, &mut pairs, &left, &right),
            Ok(false),
            "inside the pair's own unfolding the two calls are apart",
        );
        assert_eq!(
            convert_intrinsic(&mut certifier, &mut Pairs::default(), &left, &right),
            Ok(true),
            "from no pair, one unfolding reconciles them, whatever was asked before",
        );
    }
}
