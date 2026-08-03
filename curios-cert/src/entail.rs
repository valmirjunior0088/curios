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
mod tests {
    use {
        super::*,
        curios_core::{UniverseConstraintKind, UniverseConstraintOrigin, UniverseParam},
    };

    fn param(index: usize) -> Level {
        Level::param(UniverseParam(index))
    }

    fn leq(lower: &Level, upper: &Level) -> UniverseConstraint {
        UniverseConstraint {
            lower: lower.clone(),
            upper: upper.clone(),
            origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
        }
    }

    #[test]
    fn a_recorded_constraint_entails_itself() {
        let (u, w) = (param(0), param(1));

        assert!(entails(&[leq(&u, &w)], &u, &w));
        assert!(!entails(&[], &u, &w));
    }
    #[test]
    fn entailment_chains_through_the_hypotheses() {
        let (u, v, w) = (param(0), param(1), param(2));
        let assumed = [leq(&u, &v), leq(&v, &w)];

        assert!(entails(&assumed, &u, &w));
        assert!(!entails(&assumed, &w, &u));
    }
    #[test]
    fn entailment_shifts_offsets_with_the_hypothesis() {
        let (u, v) = (param(0), param(1));
        let raised = u.succ().expect("level has a successor");
        let bound = v.succ().expect("level has a successor");

        assert!(entails(&[leq(&u, &v)], &raised, &bound));
        assert!(!entails(&[leq(&u, &v)], &bound, &raised));
    }
    #[test]
    fn a_left_maximum_needs_every_part_bounded() {
        let (u, v, w) = (param(0), param(1), param(2));
        let joined = Level::max([u.clone(), v.clone()]);

        assert!(entails(&[leq(&u, &w), leq(&v, &w)], &joined, &w));
        assert!(!entails(&[leq(&u, &w)], &joined, &w));
    }
    /// A hypothesis may be shifted, never *tightened*. `u ≤ v` says nothing about `u + 1 ≤ v`, and reading it as if it did would let an instance claim a bound one level below the one it holds — which is a level gained for free, and the direction the hierarchy exists to forbid.
    #[test]
    fn a_hypothesis_does_not_gain_an_offset() {
        let (u, v) = (param(0), param(1));
        let raised = u.succ().expect("level has a successor");

        assert!(!entails(&[leq(&u, &v)], &raised, &v));
    }

    /// A parameter ranges over every natural, so nothing bounds it from below. `3 ≤ u` must fail at `u`'s own offset and hold only once the upper side carries the constant itself.
    #[test]
    fn a_constant_is_not_bounded_by_a_bare_parameter() {
        let u = param(0);
        let three = Level::constant(3);
        let raised = u.checked_add(3).expect("level admits the offset");

        assert!(!entails(&[], &three, &u));
        assert!(entails(&[], &three, &raised));
    }

    #[test]
    fn cyclic_hypotheses_terminate_and_refuse_the_unrelated() {
        let (u, v, w) = (param(0), param(1), param(2));
        let assumed = [leq(&u, &v), leq(&v, &u)];

        assert!(entails(&assumed, &u, &v));
        assert!(!entails(&assumed, &u, &w));
    }
}
