//! What an arm puts in force and where it ends: what its case teaches, the locals its solution re-types, and the spellings settled under them, asked of a term inside a lambda, under a nested arm, beside a `let`, in the arm's sibling and after the arm.

use {
    super::test_support::*,
    crate::{test_support::*, *},
    curios_common::fill,
    curios_core::Term,
};

/// `match v | true => Nat | false => Bool`: a type `3` inhabits exactly where `v` reduces to `true`.
fn family(v: &str) -> String {
    fill!(
        "match ${v}: (_) => Type | false => #BoolType | true => #NatType end",
        v
    )
}

/// A case value is bound at its own type, which its case's targets give it. `s(p) : Ix(p + 1)` matched against `subject : Ix(a + b)` leaves `a + b ~ p + 1` standing, an index the unifier does not take apart, so `subject` stands for `s(p) : Ix(p + 1)` where its own type is `Ix(a + b)`, and the arm, expected at the targets, reads it at the type the value has.
///
/// Mutation-checked: with a solution bound at the type its variable stands at in scope, `subject` is at `Ix(a + b)` where the arm expects `Ix(p + 1)`.
#[test]
fn a_case_value_is_bound_at_its_own_type() {
    let mut certifier = certifier();
    declare(&mut certifier, "Ix", "i: #NatType", "Type", &[SUCCESSOR]);
    let [matched, at_the_sum] = under(
        &mut certifier,
        &fill!(
            "a: #NatType, b: #NatType, subject: ${ix}",
            ix = family_at("Ix", "#NatAdd(a, b)")
        ),
        &fill!(
            "${matched}, ${at_the_sum}",
            matched = eliminating("i, s", &family_at("Ix", "i"), &["s(p) => subject"]),
            at_the_sum = family_at("Ix", "#NatAdd(a, b)")
        ),
    );

    assert_eq!(infer(&mut certifier, &matched), Ok(at_the_sum));
}

/// `match c : (_) => Type | false => Nat | true => {}`, the unit type at `unit` and `Nat` at the other case: where a split re-types `p, q` at it, any two of them are one by the type's shape.
fn carried(unit: bool) -> String {
    let (at_false, at_true) = match unit {
        true => ("#NatType", "{}"),
        false => ("{}", "#NatType"),
    };

    fill!(
        "match c: (_) => Type | false => ${at_false} | true => ${at_true} end",
        at_false,
        at_true
    )
}

/// `family(k(0))`, which the guard below answers only where its spelling was settled as `k(0)`.
fn claimed() -> String {
    family("k(0)")
}

/// `let u : family(k(0)) = 3; 0`, typed where the split has made the guard `k(0)`.
fn settles() -> String {
    fill!("(let u: ${claimed} = 3; 0)", claimed = claimed())
}

/// `let z : family(k(0)) = 3; 0`, typed where the split no longer stands.
fn asks() -> String {
    fill!("(let z: ${claimed} = 3; 0)", claimed = claimed())
}

/// `let w : Nat = split; asks`.
fn after(split: &str) -> String {
    fill!(
        "(let w: #NatType = ${split}; ${asks})",
        split,
        asks = asks()
    )
}

/// `match left == right : (_) => Bool | false => k(1) | true => k(0) end`: the guard, `k(0)` exactly where its comparison folds to `true`.
fn read(left: &str, right: &str) -> String {
    fill!(
        "match #NatEql(${left}, ${right}): (_) => #BoolType | false => k(1) | true => k(0) end",
        left,
        right
    )
}

/// `(c, d, h, k, p, q) => match read : (_) => Nat | false => 0 | true => arm end`, over the split variable `c`, a second variable `d`, `h : (carried) -> Nat`, `k : (Nat) -> Bool` and `p, q : carried`.
fn under_the_guard(unit: bool, read: &str, arm: &str) -> Term {
    term(&fill!(
        "(c: #BoolType, d: #BoolType, h: (x: ${carried}) -> #NatType, k: (n: #NatType) -> #BoolType, p: ${carried}, q: ${carried}) => match ${read}: (_) => #NatType | false => 0 | true => ${arm} end",
        carried = carried(unit),
        read,
        arm
    ))
}

/// The board's unit guard, `h(p) == h(q)`, over `arm`.
fn guarding(unit: bool, arm: &str) -> Term {
    under_the_guard(unit, &read("h(p)", "h(q)"), arm)
}

/// `match c : (_) => Nat | false => 0 | true => arm end`.
fn split(arm: &str) -> String {
    fill!(
        "match c: (_) => #NatType | false => 0 | true => ${arm} end",
        arm
    )
}

/// A refusal of the forged term by the mismatch the guard's question leaves unanswered after the split.
fn refused_after_the_split(verdict: Result<Term, Error>) -> bool {
    matches!(verdict, Err(Error::Mismatch { .. }))
}

/// A guard settled where a split re-types its operands at the unit type answers a term inside a lambda there, and nothing after the split: `((u : family(k(0))) => 0)(3)` in the split's arm, and `family(k(0))` asked once `c` is open again. The control is the lambda alone, which the split's spelling answers.
///
/// Mutation-checked: with `Scope::restate` restating only the equations whose spellings name a solved variable, reading no local's type or value, the certifier admits the term asked after the split.
#[test]
fn a_guard_settled_in_a_lambda_inside_a_split_answers_nothing_after_the_split() {
    let lambda = split(&fill!("((u: ${claimed}) => 0)(3)", claimed = claimed()));

    assert!(infer(&mut certifier(), &guarding(true, &lambda)).is_ok());
    assert!(refused_after_the_split(infer(
        &mut certifier(),
        &guarding(true, &after(&lambda))
    )));
}

/// The same guard settled under an arm nested in the split, on a second variable the guard does not read: the nested arm restates nothing, and what the split re-typed is read there as in the split itself.
///
/// Mutation-checked as [`a_guard_settled_in_a_lambda_inside_a_split_answers_nothing_after_the_split`] is.
#[test]
fn a_guard_settled_under_an_arm_nested_in_a_split_answers_nothing_after_the_split() {
    let nested = split(&fill!(
        "match d: (_) => #NatType | false => 0 | true => ${settles} end",
        settles = settles()
    ));

    assert!(infer(&mut certifier(), &guarding(true, &nested)).is_ok());
    assert!(refused_after_the_split(infer(
        &mut certifier(),
        &guarding(true, &after(&nested))
    )));
}

/// The same guard settled beside a `let` bound under the split, `let r : carried = p`, which the split's solution binds at the unit type.
///
/// Mutation-checked as [`a_guard_settled_in_a_lambda_inside_a_split_answers_nothing_after_the_split`] is.
#[test]
fn a_guard_settled_beside_a_let_bound_under_a_split_answers_nothing_after_the_split() {
    let bound = split(&fill!(
        "(let r: ${carried} = p; ${settles})",
        carried = carried(true),
        settles = settles()
    ));

    assert!(infer(&mut certifier(), &guarding(true, &bound)).is_ok());
    assert!(refused_after_the_split(infer(
        &mut certifier(),
        &guarding(true, &after(&bound))
    )));
}

/// A guard settled in one arm of a split answers nothing in its sibling: the operands are units at `c = false`, the arm the certifier types first, and numbers at `c = true`, where `family(k(0))` is asked.
///
/// Mutation-checked as [`a_guard_settled_in_a_lambda_inside_a_split_answers_nothing_after_the_split`] is.
#[test]
fn a_guard_settled_in_one_arm_of_a_split_answers_nothing_in_the_other() {
    let both = |sibling: &str| {
        fill!(
            "match c: (_) => #NatType | false => ${settles} | true => ${sibling} end",
            settles = settles(),
            sibling
        )
    };

    assert!(infer(&mut certifier(), &guarding(false, &both("0"))).is_ok());
    assert!(refused_after_the_split(infer(
        &mut certifier(),
        &guarding(false, &both(&asks()))
    )));
}

/// A guard whose operands meet the re-typed binders only as the arms of a stuck elimination, `h(match d : (_) => carried | true => p | false => p end)` against the same over `q`, where conversion reads the binders' type off the binders themselves: what the split decided of them is the split's alone. The elimination's motive spells the binders' type, so the guard names the split's variable and is restated by the variable it names, before any type is read.
///
/// Mutation-checked: with `Scope::restate` restating no equation, the certifier admits the term asked after the split; restating only the equations whose spellings name a solved variable still refuses it.
#[test]
fn a_guard_over_eliminations_of_re_typed_binders_answers_nothing_after_the_split() {
    let measured = |x: &str| {
        fill!(
            "h(match d: (_) => ${carried} | false => ${x} | true => ${x} end)",
            carried = carried(true),
            x
        )
    };
    let forged = |arm: &str| under_the_guard(true, &read(&measured("p"), &measured("q")), arm);

    assert!(infer(&mut certifier(), &forged(&split(&settles()))).is_ok());
    assert!(refused_after_the_split(infer(
        &mut certifier(),
        &forged(&after(&split(&settles())))
    )));
}

/// `(A, B : Type, p : SameType(former(A), former(B))) => match p : (_, _, _) => SameType(A, B) | same(z) => same(A) end`, the former applied at `${at_a}` and `${at_b}`.
const LEARNED_THROUGH: &str = "(A: Type, B: Type, p: #{node = InductType} /SameType()(${at_a}, ${at_b})) => match p: (_, _, _) => #{node = InductType} /SameType()(A, B) | same(z) => #{node = Variant} /SameType/same(A) end";

/// An equation between two types one former builds teaches an arm nothing of what the former was applied to: inversion decomposes literals, a relevant family's values and tuples, and refuses a type former, so an injective type constructor, the half of Hur's route to `False` that excluded middle or impredicativity completes, is nowhere a case's lesson. Asked of an intrinsic former, `List`, of a declared family, of an opaque global and of a function type; the control is the equation between the two variables themselves, which the arm solves.
#[test]
fn an_equation_between_two_types_one_former_builds_teaches_an_arm_nothing_of_their_arguments() {
    let mut certifier = certifier();
    // `SameType : (Type, Type) -> Prop | same(z : Type) : (z, z)`.
    declare(
        &mut certifier,
        "SameType",
        "i: Type, j: Type",
        "Prop",
        &["same(z: Type): (z, z)"],
    );
    declare(&mut certifier, "Former", "i: Type", "Type", &[]);
    opaque(&mut certifier, "F", "(x: Type) -> Type");
    let learned_through = |former: &str| {
        let at = |t: &str| fill!(former, t);
        term(&fill!(LEARNED_THROUGH, at_a = at("A"), at_b = at("B")))
    };

    for (label, former) in [
        ("List", "#ListType(${t})"),
        ("a declared family", "#{node = InductType} /Former()(${t})"),
        ("an opaque global", "/F(${t})"),
        ("a function type", "(x: ${t}) -> #NatType"),
    ] {
        assert!(
            matches!(
                infer(&mut certifier, &learned_through(former)),
                Err(Error::Mismatch { .. })
            ),
            "{label}: the arm learned what the former was applied to"
        );
    }
    assert!(infer(&mut certifier, &learned_through("${t}")).is_ok());
}

/// A type that is a proposition only under an arm is a type there and nowhere else: over `S(c) = match c : (_) => Type 1 | false => Type | true => Prop` and `F(c) = match c : (d) => S(d) | false => Nat | true => Unit`, a binder at `F(c)` with `c` open is refused, its type's sort a stuck elimination, so nothing irrelevance grants a value of it inside the arm reaches a binder, a motive, a field or a result outside it. The control binds one at `c = true`, inside the split.
#[test]
fn a_binder_at_a_type_whose_sort_an_arm_decides_is_refused_outside_the_arm() {
    let mut certifier = certifier();
    declare(&mut certifier, "Unit", "", "Prop", &["unit()"]);
    let proposition_at = fill!(
        "match c: (d) => match d: (_) => #{level = 1} Type | false => Type | true => Prop end | false => #NatType | true => ${unit} end",
        unit = family_at("Unit", "")
    );

    let outside = term(&fill!(
        "(c: #BoolType, x: ${proposition_at}) => 0",
        proposition_at
    ));
    assert!(matches!(
        infer(&mut certifier, &outside),
        Err(Error::NotASort(_))
    ));

    let inside = term(&fill!(
        "(c: #BoolType) => match c: (_) => #NatType | false => 1 | true => ((x: ${proposition_at}) => 0)(#{node = Variant} /Unit/unit()) end",
        proposition_at
    ));
    assert!(infer(&mut certifier, &inside).is_ok());
}

/// The board's unit guard settled under an arm whose case solves the split variable by index inversion rather than by a split on the variable itself: `Is : (Bool) -> Type | yes : (true) | no : (false)`, and `e : Is(c)` matched, its `yes` arm refining `c` to `true` and re-typing `p, q` at the unit type there.
#[test]
fn a_guard_settled_where_inversion_solves_its_operands_type_answers_nothing_after_the_match() {
    let mut certifier = certifier();
    declare(
        &mut certifier,
        "Is",
        "i: #BoolType",
        "Type",
        &["yes(): (true)", "no(): (false)"],
    );
    let inverted = |arm: &str| {
        fill!(
            "match e: (_, _) => #NatType | yes() => ${arm} | no() => 0 end",
            arm
        )
    };
    let forged = |arm: &str| {
        term(&fill!(
            "(c: #BoolType, h: (x: ${carried}) -> #NatType, k: (n: #NatType) -> #BoolType, p: ${carried}, q: ${carried}, e: ${is}) => match ${read}: (_) => #NatType | false => 0 | true => ${arm} end",
            carried = carried(true),
            is = family_at("Is", "c"),
            read = read("h(p)", "h(q)"),
            arm
        ))
    };

    assert!(infer(&mut certifier, &forged(&inverted(&settles()))).is_ok());
    assert!(refused_after_the_split(infer(
        &mut certifier,
        &forged(&after(&inverted(&settles())))
    )));
}
