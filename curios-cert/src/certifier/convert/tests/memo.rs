//! Remembered verdicts: a pair of nodes compared once, and a verdict that does not outlive the equations, case or binder it was reached under.

use {
    crate::certifier::convert::test_support::*,
    crate::{test_support::*, *},
    curios_analysis::test_support::*,
    curios_common::fill,
    curios_core::Free,
};

/// `depth` levels of `f(x, x)` over `base`, under the share keys from `keys` on.
fn tower(base: &str, depth: usize, keys: usize) -> String {
    doubled("f(${below}, ${again})", base, depth, keys)
}

/// Two terms that are equal graphs built apart are compared once per pair of their nodes. Sixty levels of `f(x, x)` over a local function — one tower over `n`, one over a redex that reduces to it — convert in their size, and a tower over another local is refused in its size. At a depth the uncached certifier affords, both give the same verdicts.
///
/// Mutation-checked: with no remembered verdict answered, the sixty-level comparison runs the budget out.
#[test]
fn two_towers_built_apart_are_compared_once_per_pair_of_nodes() {
    let verdicts = |mut certifier: Certifier, depth: usize| {
        let [over_n, over_redex, again_over_n, over_m] = under(
            &mut certifier,
            "f: (a: #NatType, b: #NatType) -> #NatType, n: #NatType, m: #NatType",
            &[
                tower("n", depth, 0),
                tower("((y: #NatType) => y)(n)", depth, depth),
                tower("n", depth, 2 * depth),
                tower("m", depth, 3 * depth),
            ]
            .join(", "),
        );
        let nat = term("#NatType");

        [
            convert(&mut certifier, &nat, &over_n, &over_redex),
            convert(&mut certifier, &nat, &again_over_n, &over_m),
        ]
    };

    assert_eq!(verdicts(certifier(), 60), [Ok(true), Ok(false)]);
    assert_eq!(
        verdicts(certifier(), 8),
        verdicts(Certifier::uncached(100_000, SYNTAX), 8),
    );
}

/// A remembered verdict lives as long as the equations it was reached under: `n` and `0` are apart before an arm that assumes `n = 0`, one inside it, and apart after it — and the uncached certifier agrees on all three.
///
/// Mutation-checked: with the scoped verdicts left standing where an equation moves, the comparison inside the arm answers what it answered before it.
#[test]
fn a_remembered_verdict_does_not_outlive_the_equations_it_was_reached_under() {
    let sequence = |mut certifier: Certifier| {
        let [n] = under(&mut certifier, "n: #NatType", "n,");
        let compared =
            |certifier: &mut Certifier| convert(certifier, &term("#NatType"), &n, &term("0"));

        let before = compared(&mut certifier);
        let inside = certifier.scoped(|certifier| {
            certifier
                .refine(n.clone(), term("0"))
                .expect("the equation records");
            compared(certifier)
        });
        let after = compared(&mut certifier);

        [before, inside, after]
    };

    let cached = sequence(certifier());

    assert_eq!(cached, [Ok(false), Ok(true), Ok(false)]);
    assert_eq!(cached, sequence(Certifier::uncached(100_000, SYNTAX)));
}

/// A verdict read off a type's *shape* lives no longer than the case that gave the type that shape. `x` and `y` at `match c | true => W | false => Nat end`, a struct of a unit under `c = true` and `Nat` under `c = false`, are apart before an arm that assumes `c = true`, one inside it where the type has one inhabitant, and apart after it. It is the shape-reading counterpart of `a_remembered_verdict_does_not_outlive_the_equations_it_was_reached_under`, which reaches `true` by the equation making its two terms one rather than by the type's shape, so neither covers the other's path. The uncached certifier agrees on all three.
///
/// Mutation-checked: with the scoped verdicts left standing where an equation moves, the comparison inside the arm answers what it answered before it.
#[test]
fn a_one_inhabitant_verdict_does_not_outlive_the_case_that_shaped_its_type() {
    let sequence = |mut certifier: Certifier| {
        declared(&mut certifier, "struct /W: pub Type {\n    u: {},\n}");
        let [c, x, y, shaped] = under(
            &mut certifier,
            &fill!("c: #BoolType, x: ${shaped}, y: ${shaped}", shaped = SHAPED),
            &fill!("c, x, y, ${shaped}", shaped = SHAPED),
        );
        let compared = |certifier: &mut Certifier| convert(certifier, &shaped, &x, &y);

        let before = compared(&mut certifier);
        let inside = certifier.scoped(|certifier| {
            certifier
                .refine(c.clone(), term("true"))
                .expect("the equation records");
            compared(certifier)
        });
        let after = compared(&mut certifier);

        [before, inside, after]
    };

    let cached = sequence(certifier());

    assert_eq!(cached, [Ok(false), Ok(true), Ok(false)]);
    assert_eq!(cached, sequence(Certifier::uncached(100_000, SYNTAX)));
}

/// `match c | false => Nat | true => W end`, over a `Bool` `c`: a type whose shape the case on `c` decides.
const SHAPED: &str =
    "#{goal = Type} match c | false => #NatType | true => #{node = StructType} /W() end";

/// Nor its binder. `p(a)` and `p(b)` are one where `p` takes a proof — its arguments compare at a proposition — and apart where it takes data, and a name assumed again at another type, once its first binder is closed, is compared at the second.
///
/// Mutation-checked: answering a scoped verdict without asking whether its binders stand calls the two applications under the second `h` one.
#[test]
fn a_remembered_verdict_does_not_outlive_its_binder() {
    let mut certifier = certifier();
    let nat = term("#NatType");
    // The first binders opened at a proposition, with the types the second assumes them at written over them.
    let ((locals, [left, right, function, carrier]), at_a_proof) = certifier.scoped(|certifier| {
        let opened = hypotheses(
            certifier,
            "h: Prop, p: (x: h) -> #NatType, a: h, b: h",
            "p(a), p(b), (x: h) -> #NatType, h",
        );
        let verdict = convert(certifier, &nat, &opened.1[0], &opened.1[1]);

        (opened, verdict)
    });
    let at_data = certifier.scoped(|certifier| {
        let [h, p, a, b] = <[Free; 4]>::try_from(locals).expect("four binders");
        certifier.assume(&h, &term("Type"));
        certifier.assume(&p, &function);
        certifier.assume(&a, &carrier);
        certifier.assume(&b, &carrier);

        convert(certifier, &nat, &left, &right)
    });

    assert_eq!(at_a_proof, Ok(true));
    assert_eq!(at_data, Ok(false));
}
