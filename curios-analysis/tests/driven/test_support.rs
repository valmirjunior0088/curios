//! What the driven suites share: a certifier, a family declared over a single payload, and the declarations a fixture states.

use {
    curios_analysis::test_support::*,
    curios_cert::Certifier,
    curios_common::Qualifier,
    curios_core::{
        Atom, Constructors, Free, Global, InductDecl, InductParam, Telescope, Term, UniverseContext,
    },
};

pub fn certifier() -> Certifier<'static> {
    Certifier::new(100_000, SYNTAX)
}

/// A nullary family at `result_sort` with two nullary constructors, `a` and `b`.
///
/// Two constructors is the point: it gives the family two closed inhabitants that are syntactically distinct, so whether they are *interchangeable* is decided by the family's sort and by nothing else.
pub fn declare(certifier: &mut Certifier, path: &str, result_sort: Term) -> Global {
    let family = Global::Authored(Qualifier::from([path]));

    certifier
        .declare_induct(
            &family,
            &InductDecl::new(
                UniverseContext::default(),
                Telescope::done(Telescope::done(())),
                Constructors::try_from(
                    ["a", "b"]
                        .into_iter()
                        .map(|tag| {
                            (
                                Atom::from(tag),
                                InductParam::over(&[], Telescope::done(Vec::new())),
                            )
                        })
                        .collect::<Vec<_>>(),
                )
                .unwrap(),
                result_sort,
                Qualifier::from([path]),
                true,
            ),
        )
        .expect("a name is declared once");

    family
}

/// A single-constructor family whose one payload is `payload_type`.
pub fn single_payload(payload_type: Term, result_sort: Term) -> InductDecl {
    let binder = Free::local(0, Some("f"));

    InductDecl::new(
        UniverseContext::default(),
        Telescope::done(Telescope::done(())),
        Constructors::try_from(vec![(
            Atom::from("c"),
            InductParam::over(&[], Telescope::build([(binder, payload_type)], Vec::new())),
        )])
        .unwrap(),
        result_sort,
        Qualifier::from(["T"]),
        true,
    )
}
