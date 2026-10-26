//! Fixtures the module suites share: the kernel they start from, and the families, structures and names they declare against it.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use {
    crate::Kernel,
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam, Intrinsic, StructDecl, Telescope, Term,
        UniverseContext,
    },
    curios_utilities::{Plicity, Qualifier},
};

pub(super) fn kernel() -> Kernel<'static> {
    Kernel::new(100_000, SYNTAX)
}

/// The name [`family`] registers its declaration under.
pub(super) fn fam() -> Global {
    Global::Authored(Qualifier::from(["Fam"]))
}

/// One single-constructor family whose payload is `payload_type`, declared at `result_sort` with no parameters, registered and returned for checking.
pub(super) fn family(kernel: &mut Kernel, result_sort: Term, payload_type: Term) -> InductDecl {
    let payload = Free::local(0, Some("x"));

    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam::new(
                Telescope::build([(payload, payload_type)], Vec::new()),
                vec![Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort,
        module: Qualifier::from(["Fam"]),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };
    kernel
        .declare_induct(&fam(), &declaration)
        .expect("a name is declared once");

    declaration
}

/// `(A : Prop) -> A`: a proposition at level one, since it quantifies over the ground propositions.
pub(super) fn large_proposition() -> Term {
    let a = Free::local(1, Some("A"));

    Term::func_type([(a, Term::prop())], Term::free_var(&a))
}

/// Declare `Fam : outer | mk(A : inner, f : (A) -> Fam) end`, with `outer` a level above `inner`, and hand back the family-type term and its declaration. A family holding a value of any type of `inner` — Russell's `set` at `Type`, the paradox of trees' `node` at `Prop` — takes this shape, and the two sorts a level apart are what make the *declaration* stand: the payload fits under the family. The paradox is the self-application a caller builds from it, not the declaration.
pub(super) fn family_holding_a_type(
    kernel: &mut Kernel,
    outer: Term,
    inner: Term,
) -> (Term, InductDecl) {
    let carrier = Free::local(0, Some("A"));
    let forward = Free::local(1, Some("f"));
    let fam_type = Term::induct_type(fam(), Vec::<Term>::new(), Vec::<Term>::new());

    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam::new(
                Telescope::build(
                    [
                        (carrier, inner),
                        (
                            forward,
                            Term::func_type(
                                [(Free::local(2, Some("x")), Term::free_var(&carrier))],
                                fam_type.clone(),
                            ),
                        ),
                    ],
                    Vec::new(),
                ),
                vec![Plicity::Explicit, Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: outer,
        module: Qualifier::from(["Fam"]),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };
    kernel
        .declare_induct(&fam(), &declaration)
        .expect("a name is declared once");

    (fam_type, declaration)
}

/// `mk(Fam, (x) => x)` — the family's own type handed to the field that must sit below it. Russell's and the paradox of trees' self-application both take this shape, and the identity closes the forward field whatever the first argument's fate.
pub(super) fn holding_itself(fam_type: &Term) -> Term {
    let bound = Free::local(3, Some("x"));
    let identity = Term::func([(bound, fam_type.clone())], Term::free_var(&bound));

    Term::variant(
        fam(),
        Vec::<Term>::new(),
        "mk",
        [fam_type.clone(), identity],
    )
}

/// `Fam(T : Type) | mk(_ : Nat)`, with the constructor telescope's *parameter slot* declared at `prefix` rather than at the family's `Type`.
pub(super) fn prefixed(kernel: &mut Kernel, prefix: Term) -> InductDecl {
    let t = Free::local(0, Some("T"));
    let payload = Free::local(1, Some("n"));

    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::build([(t, Term::type_ground())], Telescope::done(())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam::new(
                Telescope::build(
                    [(t, prefix), (payload, Term::intrinsic(Intrinsic::NatType))],
                    Vec::new(),
                ),
                vec![Plicity::Implicit, Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: Term::type_ground(),
        module: Qualifier::from(["Fam"]),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };
    kernel
        .declare_induct(&fam(), &declaration)
        .expect("a name is declared once");

    declaration
}

/// A `Prop`-sorted family with no constructors, so that a field typed by it is a genuine proof rather than a proposition held as data.
pub(super) fn proposition(kernel: &mut Kernel, path: &str) -> Term {
    let name = Global::Authored(Qualifier::from([path]));
    kernel
        .declare_induct(
            &name,
            &InductDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::done(Telescope::done(())),
                constructors: Vec::new().into(),
                result_sort: Term::prop(),
                module: Qualifier::from([path]),
                rep_public: true,
                polarities: Vec::new(),
                variances: Vec::new(),
            },
        )
        .expect("a name is declared once");

    Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new())
}

/// A `Prop`-sorted structure with one field of `field_type` and no parameters, registered and returned for checking.
pub(super) fn proposition_with_field(kernel: &mut Kernel, field_type: Term) -> StructDecl {
    struct_with_field(kernel, Term::prop(), field_type)
}

/// A structure at `result_sort` with one field of `field_type` and no parameters, registered and returned for checking.
pub(super) fn struct_with_field(
    kernel: &mut Kernel,
    result_sort: Term,
    field_type: Term,
) -> StructDecl {
    let name = Global::Authored(Qualifier::from(["Bad"]));
    let declaration = StructDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build(
            [(Free::local(0, Some("value")), field_type)],
            (),
        )),
        result_sort,
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };
    kernel
        .declare_struct(&name, &declaration)
        .expect("a name is declared once");

    declaration
}

/// The name the definitions below are checked under.
pub(super) fn lemma() -> Free {
    Free::from(&Global::Authored(Qualifier::from(["lemma"])))
}
