//! The level entailment oracle: whether an assumed constraint set forces `lower <= upper`. Sound and deliberately incomplete — left maxima decompose exactly, atoms chain through hypothesis uppers with offset shifts, and a cycle guard plus a fuel bound refuse rather than diverge. It is a rule that admits programs (levels the hypotheses force equal are equal in every instance satisfying them), which is why it lives in the certifier rather than beside the representation.

use curios_core::{Level, LevelHead, UniverseConstraint};

/// Whether `assumed` proves `lower ≤ upper` — the entailment a generic definition is checked under, where `assumed` is its own declared constraint set with the parameters held abstract.
///
/// Sound and deliberately incomplete, in the kernel's stated direction: a refusal is a visible disagreement, an over-eager acceptance is silent. The left side decomposes exactly — `max(c, a₁, …) ≤ u` holds iff each part is bounded — and each atom is bounded either structurally or through an assumed constraint whose lower side mentions its head: from `h + j ≤ U`, raising both sides gives `h + k ≤ U + (k ∸ j)`, and the shifted upper bound recurses. A goal already on the path refuses (cyclic hypotheses such as `u ≤ v, v ≤ u` are legal), and a fuel bound refuses hypothesis chains that grow offsets without repeating — both incomplete, neither unsound.
pub(crate) fn entails(assumed: &[UniverseConstraint], lower: &Level, upper: &Level) -> bool {
    let fuel = 4 * assumed.len() + 4;

    level_entailed(assumed, lower, upper, &mut Vec::new(), fuel)
}

/// `lower ≤ upper`: the constant part and every atom must each be bounded.
///
/// The constant rule is [`Level::structurally_leq`]'s: a parameter ranges over the naturals, so `head + k` already dominates any constant `n ≤ k`.
fn level_entailed(
    assumed: &[UniverseConstraint],
    lower: &Level,
    upper: &Level,
    visiting: &mut Vec<(LevelHead, u32)>,
    fuel: usize,
) -> bool {
    let constant_bounded = lower.constant <= upper.constant
        || upper.atoms.values().any(|offset| *offset >= lower.constant);

    constant_bounded
        && lower
            .atoms
            .iter()
            .all(|(head, offset)| atom_entailed(assumed, *head, *offset, upper, visiting, fuel))
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
