//! The registry's plicity vector, which no kernel rule reads.

use crate::Globals;

use super::test_support::*;
use curios_analysis::fixture::SYNTAX;

/// `plicities` is the one field on a registry entry that no clause of `check_induct_decl` establishes, and the reason it needs none is that this kernel never reads it — its consumer is `InductDecl::payload_plicities`, which the elaborator reads. Its *length* is no one's to check: `InductParam::new` pairs the vector with its telescope at the one door that builds one, so a short vector is unrepresentable.
///
/// That reason is an *inventory*: every consumer of a registry entry, read against the clauses. An inventory is exactly the kind of claim that goes stale as code moves, and the polarity vector beside it on the same entry has `curios-analysis`'s `a_carried_polarity_vector_is_recomputed_rather_than_believed` holding its own version of this. This holds the plicity half executably instead: the same declaration, once with the honest vector and once with a lie, must produce the same verdicts.
///
/// The lie is therefore merely wrong — an `Implicit` mark where the declaration says `Explicit`, the one lie the sealed constructor admits. What must not happen is this kernel quietly deciding something *differently* because of it, which is what a future nominal rule reading the field would introduce without any clause noticing.
///
/// The control is [`a_wrong_payload_count_is_still_refused_under_a_lying_plicity_vector`]: the same lie beside a genuine error. Without it, "the kernel ignores plicities" and "the kernel ignores this module" read alike.
#[test]
fn a_registry_plicity_vector_is_read_by_no_kernel_rule() {
    assert_eq!(
        fixture_verdicts(
            &plicity_module(true, 1),
            1_000_000,
            &Globals::default(),
            SYNTAX
        ),
        fixture_verdicts(
            &plicity_module(false, 1),
            1_000_000,
            &Globals::default(),
            SYNTAX
        ),
        "a plicity vector no kernel rule reads changed a verdict",
    );
    assert_eq!(
        fixture_verdicts(
            &plicity_module(false, 1),
            1_000_000,
            &Globals::default(),
            SYNTAX
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
            SYNTAX
        )
        .is_empty(),
        "the kernel accepted a constructor application at the wrong payload count",
    );
}
