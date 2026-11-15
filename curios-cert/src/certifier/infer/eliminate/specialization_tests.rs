//! Context specialization under an arm: the index equations it forces, the clashes that excuse it, and a match over indices or instances that stands.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_common::fill,
    curios_core::{Reducer, Term},
};

/// The arm rule specializes the *context*, not just the goal. Matching `s(p) : Ix(p + 1)` against a scrutinee typed `Ix(b)` forces `b ≡ p + 1`, so a hypothesis at `P(b)` inhabits the arm's expectation `P(p + 1)`.
///
/// This is the shape transitivity over an inductive order needs, where matching the first proof pins the index the second proof's type names: the equation refines an *outer* binder, which no goal at the case reaches and no arm binder owns.
#[test]
fn a_forced_index_equation_refines_an_outer_hypothesis() {
    let mut certifier = certifier();
    opaque(&mut certifier, "P", "(n: #NatType) -> Type");
    declare(&mut certifier, "Ix", "i: #NatType", "Type", &[SUCCESSOR]);
    let [matched, at_b] = under(
        &mut certifier,
        &fill!(
            "b: #NatType, w: /P(b), subject: ${ix}",
            ix = family_at("Ix", "b")
        ),
        &fill!(
            "${matched}, /P(b)",
            matched = eliminating("/P(b)", &["s(p) => w"])
        ),
    );

    assert_eq!(infer(&mut certifier, &matched), Ok(at_b));
}

/// A forced equation whose solution mentions a refinable variable is refused rather than substituted: `Cyc(b)` matched against a case aimed at `b + 1` would solve `b := b + 1`, and the occurs check must leave the arm unspecialized instead. The hypothesis therefore stays at `P(b)` and fails the expectation `P(b + 1)` — a refusal, never a silent cycle.
///
/// The case's target names the outer `b` by its identity, so the family is declared once `b` is opened.
#[test]
fn a_cyclic_index_equation_refines_nothing() {
    let mut certifier = certifier();
    opaque(&mut certifier, "P", "(n: #NatType) -> Type");
    let (locals, [matched]) = hypotheses(
        &mut certifier,
        &fill!(
            "b: #NatType, w: /P(b), subject: ${cyc}",
            cyc = family_at("Cyc", "b")
        ),
        &fill!(
            "${matched},",
            matched = eliminating("/P(b)", &["mk() => w"])
        ),
    );
    declare(
        &mut certifier,
        "Cyc",
        "i: #NatType",
        "Type",
        &[&fill!(
            "mk(): (#Nat(Succ(1, #{binding = Free(${b})} b)))",
            b = locals[0].index()
        )],
    );

    assert!(matches!(
        infer(&mut certifier, &matched),
        Err(Error::Mismatch { .. }),
    ));
}

/// A variable `Bool` scrutinee stands refined to the literal in each arm — the zero-index instance of the same specialization.
#[test]
fn a_bool_arm_sees_its_scrutinee_at_the_literal() {
    let mut certifier = certifier();
    opaque(&mut certifier, "P", "(x: #BoolType) -> Type");
    let [matched, at_h] = under(
        &mut certifier,
        "h: #BoolType, w: /P(h)",
        "#{goal = /P(h)} match h | false => w | true => w end, /P(h)",
    );

    assert_eq!(infer(&mut certifier, &matched), Ok(at_h));
}

/// The carrier sorts the two clash fixtures below are put at, each with whether an omitted arm stands there.
const CARRIERS: [(&str, &str, bool); 2] = [
    (
        "a relevant carrier, whose two constructors a program tells apart",
        "Type",
        true,
    ),
    (
        "a proposition, whose two inhabitants irrelevance identifies",
        "Prop",
        false,
    ),
];

/// `Bit : ${sort} | a() | b()`, declared in `certifier`, and the text of its type and its two inhabitants.
fn bit(certifier: &mut Certifier, sort: &str) -> [String; 3] {
    declare(certifier, "Bit", "", sort, &["a()", "b()"]);

    [
        family_at("Bit", ""),
        "#{node = Variant} /Bit/a()".to_string(),
        "#{node = Variant} /Bit/b()".to_string(),
    ]
}

/// Whether the elimination with no arm of `subject` stood, as the clash fixtures read it.
fn omitted(certifier: &mut Certifier, label: &str, stands: bool, matched: &Term) {
    let verdict = infer(certifier, matched);
    match stands {
        true => assert!(verdict.is_ok(), "{label}: the omitted arm was demanded"),
        false => assert!(
            matches!(verdict, Err(Error::MissingArm { .. })),
            "{label}: the omitted arm was excused"
        ),
    }
}

/// Two forcings of one binder that clash make the case impossible, so the arm may be omitted — and at a proposition they do not, so it may not.
///
/// This is the clash rule reaching the certifier through the walk that certifies a module, rather than through the shared unifier alone. The target is *non-linear*: `same(@z) : (z, z)` is `Eq`'s shape, so neither index position clashes on its own — each solves `z`, here to `a()` and to `b()` — and what decides the case is whether those two forcings can be one value.
///
/// The pair differs in nothing but the sort of the *carrier the forcings inhabit* — not of the family being eliminated, which stays a proposition as `Eq` is. That is the distinction the rule turns on and the one a fixture most easily gets wrong: what licenses telling two constructors apart is their own family's sort. At a relevant carrier they are values a program distinguishes, so `Same(a(), b())` is empty and the omission stands. At a `Prop` carrier irrelevance identifies them, the deletion rule reconciles the pair before a clash is ever asked for, and the omitted arm is one the elimination cannot prove absent — the vacuous-elimination route, which is why that half is the half worth holding.
#[test]
fn a_clash_between_two_forcings_of_one_binder_excuses_the_arm() {
    for (label, carrier_sort, omission_stands) in CARRIERS {
        let mut certifier = certifier();
        let [bit, a, b] = bit(&mut certifier, carrier_sort);
        declare(
            &mut certifier,
            "Same",
            &fill!("i: ${bit}, j: ${bit}", bit),
            "Prop",
            &[&fill!("same(z: ${bit}): (z, z)", bit)],
        );

        // No arm for `same`, at two indices it cannot both equal.
        let [matched] = under(
            &mut certifier,
            &fill!(
                "subject: ${same}",
                same = family_at("Same", &fill!("${a}, ${b}", a, b))
            ),
            &fill!("${matched},", matched = eliminating("#NatType", &[])),
        );

        omitted(&mut certifier, label, omission_stands, &matched);
    }
}

/// One variable of the scrutinee's indices forced to two values that clash makes the case impossible as two forcings of one binder do, so the arm may be omitted — and at a proposition they do not, so it may not.
///
/// The mirror of the fixture above. The targets are closed, `apart() : (a(), b())`, and it is the scrutinee's indices that name one variable twice, `(x, x)`. The unifier's first direction decides nothing of it, a variable being what may yet be either target; the second solves `x` to each target and asks whether the two can be one value. At a relevant carrier they cannot, so no value of `Apart(x, x)` is `apart()` and the omission stands. At a `Prop` carrier they convert, `apart()` inhabits `Apart(x, x)` for every `x`, and the omitted arm is one the elimination cannot prove absent.
///
/// Mutation-checked: with absence asked of the first direction alone, the relevant half demands the arm.
#[test]
fn an_outer_variable_forced_two_ways_excuses_the_arm() {
    for (label, carrier_sort, omission_stands) in CARRIERS {
        let mut certifier = certifier();
        let [bit, a, b] = bit(&mut certifier, carrier_sort);
        declare(
            &mut certifier,
            "Apart",
            &fill!("i: ${bit}, j: ${bit}", bit),
            "Type",
            &[&fill!("apart(): (${a}, ${b})", a, b)],
        );

        // No arm for `apart`, at two indices that are one variable.
        let [matched] = under(
            &mut certifier,
            &fill!(
                "x: ${bit}, subject: ${apart}",
                bit,
                apart = family_at("Apart", "x, x")
            ),
            &fill!("${matched},", matched = eliminating("#NatType", &[])),
        );

        omitted(&mut certifier, label, omission_stands, &matched);
    }
}

/// `Same : (Nat, Nat) -> Prop | same(z : Nat) : (z, z)`, the shape of an equality: one constructor, its payload both of its indices, declared in `certifier`.
fn same(certifier: &mut Certifier) {
    declare(
        certifier,
        "Same",
        "i: #NatType, j: #NatType",
        "Prop",
        &["same(z: #NatType): (z, z)"],
    );
}

/// A match over a scrutinee assumed at `Same(${indices})` whose one arm is `same(z) => ${body}`, stating the instance `Same(${stated})` where it is given one, opened in `certifier` beside `n` and the hypotheses `binders`: `n`, and the match.
fn over_same(
    certifier: &mut Certifier,
    binders: &str,
    indices: &str,
    stated: Option<&str>,
    body: &str,
) -> [Term; 2] {
    let arm = fill!("same(z) => ${body}", body);
    let matched = match stated {
        Some(stated) => fill!(
            "#{goal = #NatType, cases = {stated = ${instance}}} match subject | ${arm} end",
            instance = family_at("Same", stated),
            arm
        ),
        None => eliminating("#NatType", &[&arm]),
    };

    under(
        certifier,
        &fill!(
            "${binders}, subject: ${same}",
            binders,
            same = family_at("Same", indices)
        ),
        &fill!("n, ${matched}", matched),
    )
}

/// A match on a proposition whose indices pin its payload fires with its scrutinee unreduced, binding the payload to the index that states it: the scrutinee is a variable, which no reduction takes to a constructor, and its type's two indices are one, which is everything `same` would say.
#[test]
fn an_arm_that_names_a_pinned_payload_is_bound_to_the_index() {
    let mut certifier = certifier();
    same(&mut certifier);
    let [n, matched] = over_same(&mut certifier, "n: #NatType", "n, n", Some("n, n"), "z");

    assert_eq!(infer(&mut certifier, &matched), Ok(term("#NatType")));
    assert_eq!(certifier.reduce(matched), Ok(n));
}

/// An arm that names no payload fires the same way, with nothing to bind.
#[test]
fn an_arm_written_over_the_index_fires_with_its_proof_unreduced() {
    let mut certifier = certifier();
    same(&mut certifier);
    let [n, matched] = over_same(&mut certifier, "n: #NatType", "n, n", Some("n, n"), "n");

    assert_eq!(infer(&mut certifier, &matched), Ok(term("#NatType")));
    assert_eq!(certifier.reduce(matched), Ok(n));
}

/// The control: at two indices nothing holds one, the match stands. The scrutinee is the same variable, so a rule that fired on the shape of the match alone would answer here too.
#[test]
fn a_match_over_indices_that_differ_stands() {
    let mut certifier = certifier();
    same(&mut certifier);
    let [_, matched] = over_same(
        &mut certifier,
        "n: #NatType, m: #NatType",
        "n, m",
        Some("n, m"),
        "n",
    );

    assert_eq!(certifier.reduce(matched.clone()), Ok(matched));
}

/// What a match fires on is the instance it states, and a match that states none stands: the scrutinee is assumed at `Same(n, n)`, whose indices pin the payload, and no rule looks that up.
#[test]
fn a_match_that_states_no_instance_stands() {
    let mut certifier = certifier();
    same(&mut certifier);
    let [_, matched] = over_same(&mut certifier, "n: #NatType", "n, n", None, "z");

    assert_eq!(infer(&mut certifier, &matched), Ok(term("#NatType")));
    assert_eq!(certifier.reduce(matched.clone()), Ok(matched));
}

/// The instance a match states is held to the type its scrutinee is inferred at: a match over a scrutinee at `Same(n, m)` that states `Same(n, n)` is refused where it is typed. Reduced where nothing typed it, it is its arm at the payload it states, the step every reader of the term takes alike, with the scrutinee's own type read by none.
///
/// Mutation-checked: with the stated instance left unheld, the match is typed at `Nat`.
#[test]
fn a_stated_instance_is_held_to_the_scrutinees_type() {
    let mut certifier = certifier();
    same(&mut certifier);
    let [n, matched] = over_same(
        &mut certifier,
        "n: #NatType, m: #NatType",
        "n, m",
        Some("n, n"),
        "z",
    );

    assert!(matches!(
        infer(&mut certifier, &matched),
        Err(Error::Mismatch { .. })
    ));
    assert_eq!(certifier.reduce(matched), Ok(n));
}
