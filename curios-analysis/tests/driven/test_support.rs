//! What the driven suites share: a kernel, a family declared over a single payload, and the declarations a fixture states.

use {
    curios_analysis::test_support::SYNTAX,
    curios_cert::Kernel,
    curios_core::{Atom, Free, Global, InductDecl, InductParam, Telescope, Term, UniverseContext},
    curios_utilities::{Plicity, Qualifier},
};

pub fn kernel() -> Kernel<'static> {
    Kernel::new(100_000, SYNTAX)
}

/// A nullary family at `result_sort` with two nullary constructors, `a` and `b`.
///
/// Two constructors is the point: it gives the family two closed inhabitants that are syntactically distinct, so whether they are *interchangeable* is decided by the family's sort and by nothing else.
pub fn declare(kernel: &mut Kernel, path: &str, result_sort: Term) -> Global {
    let family = Global::Authored(Qualifier::from([path]));

    kernel
        .declare_induct(
            &family,
            &InductDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::done(Telescope::done(())),
                constructors: ["a", "b"]
                    .into_iter()
                    .map(|tag| {
                        (
                            Atom::from(tag),
                            InductParam::new(Telescope::done(Vec::new()), Vec::new()),
                        )
                    })
                    .collect(),
                result_sort,
                module: Qualifier::from([path]),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )
        .expect("a name is declared once");

    family
}

/// A single-constructor family whose one payload is `payload_type`.
pub fn single_payload(payload_type: Term, result_sort: Term) -> InductDecl {
    let binder = Free::local(0, Some("f"));

    InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("c"),
            InductParam::new(
                Telescope::build([(binder, payload_type)], Vec::new()),
                vec![Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort,
        module: Qualifier::from(["T"]),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}
