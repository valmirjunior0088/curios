//! The registry's marks, which no kernel rule reads.

use {super::test_support::*, crate::Globals, curios_analysis::test_support::SYNTAX};

/// A constructor's marks are the part of a registry entry that no clause of `check_induct_decl` establishes, and the reason they need none is that this kernel never reads them — their consumer is `InductDecl::payload_plicities`, which the elaborator reads. That each binder has one is no one's to check: a mark is its telescope entry's own, so a signature a mark short of its binders cannot be written, and the same holds of the marks a declaration's parameters stand under in its arity.
///
/// That reason is an *inventory*: every consumer of a registry entry, read against the clauses. An inventory is exactly the kind of claim that goes stale as code moves, and the polarity vector beside it on the same entry has `curios-analysis`'s `a_carried_polarity_vector_is_recomputed_rather_than_believed` holding its own version of this. This holds the plicity half executably instead: the same declaration, once with the honest mark and once with a lie, must produce the same verdicts.
///
/// The lie is therefore merely wrong — an `Implicit` mark where the declaration says `Explicit`. What must not happen is this kernel quietly deciding something *differently* because of it, which is what a future nominal rule reading the marks would introduce without any clause noticing.
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
