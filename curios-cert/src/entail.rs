//! The level entailment oracle: whether an assumed constraint set forces `lower <= upper`. Sound and deliberately incomplete — left maxima decompose exactly, atoms chain through hypothesis uppers with offset shifts, and a cycle guard plus a fuel bound refuse rather than diverge. It is a rule that admits programs (levels the hypotheses force equal are equal in every instance satisfying them), which is why it lives in the certifier rather than beside the representation.

use curios_core::{Level, LevelHead, UniverseConstraint};

/// Whether `assumed` proves `lower ≤ upper` — the entailment a generic definition is checked under, where `assumed` is its own declared constraint set with the parameters held abstract.
///
/// Sound and deliberately incomplete, in the kernel's stated direction: a refusal is a visible disagreement, an over-eager acceptance is silent. The left side decomposes exactly — `max(c, a₁, …) ≤ u` holds iff each part is bounded — and each atom is bounded either structurally or through an assumed constraint whose lower side mentions its head: from `h + j ≤ U`, raising both sides gives `h + k ≤ U + (k ∸ j)`, and the shifted upper bound recurses. A goal already on the path refuses (cyclic hypotheses such as `u ≤ v, v ≤ u` are legal), and a fuel bound refuses hypothesis chains that grow offsets without repeating — both incomplete, neither unsound.
pub(crate) fn entails(assumed: &[UniverseConstraint], lower: &Level, upper: &Level) -> bool {
    let fuel = 4 * assumed.len() + 4;

    level_entailed(assumed, lower, upper, &mut Vec::new(), fuel)
}

/// The value `level` cannot go below, whatever its parameters are assigned: a parameter ranges over the naturals, so an atom at offset `k` already carries `k`, and the level is at least the largest of those and its own constant.
///
/// This is the one fact both constant rules below rest on, named once rather than spelled twice. [`Level::structurally_leq`] reads it on the *upper* side — a constant is dominated when the upper level's floor reaches it — and [`constant_entailed`] reads it on a premise's *lower* side, where it is what lets a premise carrying atoms bound a bare constant at all.
fn floor(level: &Level) -> u32 {
    level
        .atoms
        .values()
        .copied()
        .chain([level.constant])
        .max()
        .unwrap_or(level.constant)
}

/// `lower ≤ upper`: the constant part and every atom must each be bounded.
///
/// The constant rule is [`Level::structurally_leq`]'s: a parameter ranges over the naturals, so `head + k` already dominates any constant `n ≤ k`. That is the rule with *no* hypotheses, and it is not the whole of the relation — an assumed constraint narrows what a parameter ranges over, so the constant falls through to [`constant_entailed`] rather than deciding the pair.
fn level_entailed(
    assumed: &[UniverseConstraint],
    lower: &Level,
    upper: &Level,
    visiting: &mut Vec<(LevelHead, u32)>,
    fuel: usize,
) -> bool {
    let constant_bounded = lower.constant <= floor(upper)
        || constant_entailed(assumed, lower.constant, upper, visiting, fuel);

    constant_bounded
        && lower
            .atoms
            .iter()
            .all(|(head, offset)| atom_entailed(assumed, *head, *offset, upper, visiting, fuel))
}

/// `constant ≤ upper` through an assumed constraint, where `upper`'s own floor does not reach it.
///
/// **A hypothesis is what puts a floor under a parameter.** `Type u` with `1 ≤ u` assumed is the shape a group's own monomorphic recursion states — `pick(@Type, …)` calling itself at `Type 0` needs the group's `u` strictly above zero, and the elaborator records exactly that constraint in the scheme. Without this the kernel reads `u` as ranging over every natural, finds no `k` in `upper` dominating the constant, and refuses a program whose own declaration says it may not: the premise is in `assumed` and was never consulted, because the constant was decided before the hypotheses were reached.
///
/// **A premise is read at its own floor, so one carrying atoms bounds a constant too.** `u + 1 ≤ v` says `v` is at least one, because `u` is a natural and the offset is carried whatever it is assigned — so the premise's floor is what transits, not its constant. Soundness is one step of transitivity at that floor: `n ≤ floor(L) ≤ L ≤ U ≤ upper`, with the recursion carrying the last leg and [`floor`] justifying the first.
fn constant_entailed(
    assumed: &[UniverseConstraint],
    constant: u32,
    upper: &Level,
    visiting: &mut Vec<(LevelHead, u32)>,
    fuel: usize,
) -> bool {
    let Some(fuel) = fuel.checked_sub(1) else {
        return false;
    };

    assumed.iter().any(|constraint| {
        floor(&constraint.lower) >= constant
            && level_entailed(assumed, &constraint.upper, upper, visiting, fuel)
    })
}

/// `head + offset ≤ upper`, structurally or through an assumed constraint.
fn atom_entailed(
    assumed: &[UniverseConstraint],
    head: LevelHead,
    offset: u32,
    upper: &Level,
    visiting: &mut Vec<(LevelHead, u32)>,
    fuel: usize,
) -> bool {
    if upper.atoms.get(&head).is_some_and(|bound| offset <= *bound) {
        return true;
    }

    let Some(fuel) = fuel.checked_sub(1) else {
        return false;
    };
    if visiting.contains(&(head, offset)) {
        return false;
    }

    visiting.push((head, offset));
    let outcome = assumed.iter().any(|constraint| {
        constraint.lower.atoms.get(&head).is_some_and(|premise| {
            let surplus = offset.saturating_sub(*premise);
            match constraint.upper.checked_add(surplus) {
                Ok(shifted) => level_entailed(assumed, &shifted, upper, visiting, fuel),
                Err(_) => false,
            }
        })
    });
    visiting.pop();

    outcome
}

#[cfg(test)]
mod tests;
