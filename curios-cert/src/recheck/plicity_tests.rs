//! The registry's plicity vector, which no kernel rule reads.

//! What the walk derives for itself rather than reading off the module.
//!
//! It also holds the hand-built adversarial modules. A refusal the elaborator reaches first leaves no module behind, so a rule where `curios-elab` is the stricter of the two cannot be put to this crate by any surface program — `Expect::NotAsked` in `curios/src/tests/perimeter.rs` records exactly that gap. Reaching it means constructing the finished module here and asking `recheck_module_verdicts` directly.

use crate::Globals;

use super::test_support::*;

/// `plicities` is the one field on a registry entry that no clause of `check_induct_decl` establishes, and the stated reason it needs none is that this kernel never reads it — its only consumer is `curios-elab`'s `payload_plicities`, which slices at the parameter count, so a short vector is a panic in the elaborator rather than a judgment here.
///
/// That reason is an *inventory*: every consumer of a registry entry, read against the clauses. An inventory is exactly the kind of claim that goes stale as code moves, and the polarity vector beside it on the same entry already has `positivity::tests::a_carried_polarity_vector_is_recomputed_rather_than_believed` holding its own version of this. This holds the plicity half executably instead: the same declaration, once with the honest vector and once with a lie, must produce the same verdicts.
///
/// The lie is unusable rather than merely wrong — an empty vector where the constructor carries one payload binder, which is the shape `payload_plicities` would slice out of range on. What must not happen is this kernel quietly deciding something *differently* because of it, which is what a future nominal rule reading the field would introduce without any clause noticing.
///
/// The control is [`a_wrong_payload_count_is_still_refused_under_a_lying_plicity_vector`]: the same lie beside a genuine error. Without it, "the kernel ignores plicities" and "the kernel ignores this module" read alike.
#[test]
fn a_registry_plicity_vector_is_read_by_no_kernel_rule() {
    assert_eq!(
        fixture_verdicts(
            &plicity_module(true, 1),
            1_000_000,
            &Globals::default(),
            crate::SYNTAX
        ),
        fixture_verdicts(
            &plicity_module(false, 1),
            1_000_000,
            &Globals::default(),
            crate::SYNTAX
        ),
        "a plicity vector no kernel rule reads changed a verdict",
    );
    assert_eq!(
        fixture_verdicts(
            &plicity_module(false, 1),
            1_000_000,
            &Globals::default(),
            crate::SYNTAX
        ),
        Vec::new(),
        "both sides must be accepted, or the equality above is two refusals agreeing",
    );
}

/// The control for the fixture above: under the same lying plicity vector, an ordinary error is still caught.
#[test]
fn a_wrong_payload_count_is_still_refused_under_a_lying_plicity_vector() {
    assert!(
        !fixture_verdicts(
            &plicity_module(false, 0),
            1_000_000,
            &Globals::default(),
            crate::SYNTAX
        )
        .is_empty(),
        "the kernel accepted a constructor application at the wrong payload count",
    );
}
