//! The proof a certificate stands for, written as the term an author would write, so that elaboration and both checkers judge it as they judge written code.
//!
//! **A certificate is a sum of facts.** Each fact enters as a `Le` at its carrier — a strict fact as its successor, a guard's false arm through its dual, an equation as its two bounds — scaled by its multiplier through `Le/mul_mono_r`, and `Le/add` adds two. Conversion's cancellation does the rest: a sum whose sides differ by a constant reduces to `Bool/True` or `Bool/False`, which is clause 1 of what `curios-core`'s `linear` module says conversion decides.
//!
//! **The direct form.** Where the negated goal's multiplier is one, the facts' sum is the goal's view plus a constant `s >= 0` — `a <= b` and `b <= c` sum to `a <= c`, a strict fact to its successor's — and the sum with the literal fact `0 <= s` is the proof, one proposition with the goal by clause 5.
//!
//! **The refuting form.** Otherwise the proof splits on the goal's own decision, spelled as the bound's reduct spells it, so the arm's refinement is keyed where the bound reads it. The true arm is `Bool/True/qed()`. The false arm eliminates, with a zero-arm match, the sum of the facts with the negated goal — proved there by `Le/of_not_lt` or `Lt/of_not_le` at `qed` — whose type reduces to `Bool/False`: `a <= b` from `2 * a <= 2 * b` scales the negated goal `b + 1 <= a` by two and leaves `2 <= 0`.
//!
//! **The absurd form.** Where the goal is itself an empty proposition, the sum of the facts is the proof, its type reducing to that proposition; where the facts refute each other without the goal, the sum is eliminated by a zero-arm match in the goal's place.
//!
//! **The carrier of a sum** is `Int` where the goal or any fact it sums is at `Int`, and `Nat` otherwise. A `Nat` fact enters an `Int` sum as its sides widened, the proposition conversion aligns it with. `Nat` addition never truncates, so a `Nat` sum is exact.

use {
    super::{Fact, Target, eliminate, global, in_scope, literal, order, qed, split},
    crate::Context,
    curios_algebra::Carrier,
    curios_core::{Intrinsic, Term},
    curios_num::{Integer, Natural},
    curios_utilities::Plicity,
};

/// What writing a certificate came to.
pub(super) enum Written {
    /// The proof, not yet checked.
    Proof(Term),
    /// A name the proof applies is not in scope: inside `/std`, an item compiled before the vocabulary.
    Unwritten,
}

/// The proof `multipliers` stands for over `facts`, the goal where there is one, and its negation where it could be written — the negation's multiplier last.
pub(super) fn write(
    context: &mut Context,
    facts: &[Fact],
    goal: Option<&Target>,
    negated: Option<&Fact>,
    multipliers: &[Natural],
) -> Written {
    let (weights, goal_weight) = multipliers.split_at(facts.len());
    let goal_weight = goal_weight
        .first()
        .cloned()
        .unwrap_or_else(|| Natural::from(0u32));
    let used = facts
        .iter()
        .zip(weights)
        .filter(|(_, weight)| !weight.is_zero())
        .collect::<Vec<_>>();
    let integer = goal.is_some_and(|goal| goal.carrier == Carrier::Integer)
        || used
            .iter()
            .any(|(fact, _)| fact.carrier == Carrier::Integer);
    let carrier = match integer {
        true => Carrier::Integer,
        false => Carrier::Natural,
    };
    let none = Integer::from(0);

    let written = match (goal, negated) {
        // The goal is empty, and the facts' sum is a proof of it.
        (None, _) => sum(context, carrier, &used, none),
        // The facts refute each other without the goal.
        (Some(_), _) if goal_weight.is_zero() => {
            sum(context, carrier, &used, none).map(|refuted| eliminate(context, refuted))
        }
        (Some(goal), _) if goal_weight == Natural::from(1u32) => {
            sum(context, carrier, &used, slack(&used, goal))
        }
        (Some(goal), Some(negated)) => {
            let mut with_goal = used.clone();
            with_goal.push((negated, &goal_weight));
            sum(context, carrier, &with_goal, none).map(|refuted| {
                let false_arm = eliminate(context, refuted);
                // Computed first: `split` borrows the context mutably, and so would an argument written in the same call.
                let true_arm = qed(context);
                split(context, &goal.decision, false_arm, true_arm)
            })
        }
        // The negation is the only row that carries the goal's weight, so a weighted goal without one cannot be.
        (Some(_), None) => None,
    };
    match written {
        Some(proof) => Written::Proof(proof),
        None => Written::Unwritten,
    }
}

/// The constant `s` by which the facts' weighted sum exceeds the goal's view, `Σ wᵢ·fᵢ = g + s`: read off the constants alone, since the monomials cancel exactly where the negated goal's multiplier is one. It is the certificate's constant less one, so it is never negative.
fn slack(used: &[(&Fact, &Natural)], goal: &Target) -> Integer {
    let total = used.iter().fold(Integer::from(0), |total, (fact, weight)| {
        total + fact.form.constant.clone() * Integer::from((*weight).clone())
    });
    total - goal.form.constant.clone()
}

/// The weighted sum of `used` at `carrier`, closed by the literal fact `0 <= slack` where the slack is positive: a proof of `Le(L, R)` whose `L - R` is the weighted sum of the facts' forms, less the slack.
fn sum(
    context: &Context,
    carrier: Carrier,
    used: &[(&Fact, &Natural)],
    slack: Integer,
) -> Option<Term> {
    let order = order(context, carrier);
    let scaling = used
        .iter()
        .any(|(_, weight)| **weight != Natural::from(1u32));
    let names = match scaling {
        true => vec![order.add, order.scale],
        false => vec![order.add],
    };
    if !in_scope(context, &names) {
        return None;
    }

    let mut terms = used
        .iter()
        .map(|(fact, weight)| scaled(context, carrier, fact, weight))
        .collect::<Vec<_>>();
    if slack > Integer::from(0) {
        terms.push((
            literal(carrier, 0u32),
            literal(carrier, slack.magnitude()),
            qed(context),
        ));
    }

    let (_, _, proof) = terms.into_iter().reduce(
        |(left, right, proof), (next_left, next_right, next_proof)| {
            let summed = Term::apply_marked(
                global(order.add),
                [
                    (Plicity::Implicit, left.clone()),
                    (Plicity::Implicit, right.clone()),
                    (Plicity::Implicit, next_left.clone()),
                    (Plicity::Implicit, next_right.clone()),
                    (Plicity::Explicit, proof),
                    (Plicity::Explicit, next_proof),
                ],
            );
            (
                add(carrier, left, next_left),
                add(carrier, right, next_right),
                summed,
            )
        },
    )?;
    Some(proof)
}

/// One fact at `carrier`, scaled by `weight`: its sides — widened where the fact is at `Nat` and the sum at `Int` — and its proof.
fn scaled(
    context: &Context,
    carrier: Carrier,
    fact: &Fact,
    weight: &Natural,
) -> (Term, Term, Term) {
    let widen = |side: &Term| match (fact.carrier, carrier) {
        (Carrier::Natural, Carrier::Integer) => Term::intrinsic(Intrinsic::NatToInt(side.clone())),
        _ => side.clone(),
    };
    let (left, right) = (widen(&fact.left), widen(&fact.right));
    if *weight == Natural::from(1u32) {
        return (left, right, fact.proof.clone());
    }

    let k = literal(carrier, weight.clone());
    let mut arguments = vec![
        (Plicity::Implicit, left.clone()),
        (Plicity::Implicit, right.clone()),
        (Plicity::Explicit, k.clone()),
        (Plicity::Explicit, fact.proof.clone()),
    ];
    // `Int`'s scaling asks that the factor is not negative, which conversion decides of a literal.
    if carrier == Carrier::Integer {
        arguments.push((Plicity::Explicit, qed(context)));
    }
    let proof = Term::apply_marked(global(order(context, carrier).scale), arguments);
    (
        multiply(carrier, left, k.clone()),
        multiply(carrier, right, k),
        proof,
    )
}

fn add(carrier: Carrier, left: Term, right: Term) -> Term {
    Term::intrinsic(match carrier {
        Carrier::Natural => Intrinsic::NatAdd(left, right),
        _ => Intrinsic::IntAdd(left, right),
    })
}

fn multiply(carrier: Carrier, left: Term, right: Term) -> Term {
    Term::intrinsic(match carrier {
        Carrier::Natural => Intrinsic::NatMul(left, right),
        _ => Intrinsic::IntMul(left, right),
    })
}
