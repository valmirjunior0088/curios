//! What a private representation refuses outside its module, and the machinery and oracle that may rederive it.

use {
    super::test_support::*,
    crate::*,
    curios_core::{Atom, Free, Term},
    curios_utilities::Qualifier,
};

// Only the witness lowering mints a `Derive`, always in checked position against a concept application; met in inference, or checked against anything else, the transient has nothing to derive from and refuses rather than passing through.
#[test]
fn a_derive_transient_is_refused_outside_a_checked_witness_body() {
    let mut context = context();
    let term = Term::derive();

    assert!(matches!(
        elaborate(&mut context, &term, Mode::Infer),
        Err(Error::DeriveOutsideWitness)
    ));
    assert!(matches!(
        elaborate(&mut context, &term, Mode::Check(nat())),
        Err(Error::DeriveOutsideWitness)
    ));
}

fn opt_type() -> Term {
    Term::induct_type(nominal("Opt"), Vec::<Term>::new(), Vec::<Term>::new())
}

fn make_opt_opaque(context: &mut Context) {
    let mut induct_decl = context.induct_decl(&nominal("Opt")).unwrap().clone();
    induct_decl.module = Qualifier::from(["Owner"]);
    induct_decl.rep_public = false;
    context.update_induct(&nominal("Opt"), induct_decl);
}

#[test]
fn opaque_inductive_construction_is_rejected_outside_declaring_module() {
    let mut context = context();
    register_opt(&mut context);
    make_opt_opaque(&mut context);

    let term = Term::variant(
        nominal("Opt"),
        Vec::<Term>::new(),
        "none",
        Vec::<Term>::new(),
    );
    assert!(matches!(
        elaborate(&mut context, &term, Mode::Infer),
        Err(Error::PrivateRepresentation { name }) if name == nominal("Opt")
    ));

    context.set_island(Qualifier::from(["Owner"]));
    assert!(elaborate(&mut context, &term, Mode::Infer).is_ok());
}

#[test]
fn opaque_inductive_eliminators_are_rejected_before_shape_analysis() {
    let mut context = context();
    register_opt(&mut context);
    make_opt_opaque(&mut context);
    let scrutinee = context.fresh(Some("x"));
    let payload = context.fresh(Some("n"));
    context.assume(&scrutinee, &opt_type());

    let empty = Term::induct_match(
        Term::free_var(&scrutinee),
        Some(&scrutinee),
        nat(),
        Vec::<(Atom, Vec<Free>, Term)>::new(),
    );
    let named = Term::induct_match(
        Term::free_var(&scrutinee),
        Some(&scrutinee),
        nat(),
        [
            ("none", Vec::<Free>::new(), nat_lit(0)),
            ("some", vec![payload], nat_lit(1)),
        ],
    );
    let defaulted = Term::induct_match_default(
        Term::free_var(&scrutinee),
        Some(&scrutinee),
        nat(),
        [("none", Vec::<Free>::new(), nat_lit(0))],
        nat_lit(1),
    );

    for term in [empty, named, defaulted] {
        assert!(matches!(
            elaborate(&mut context, &term, Mode::Infer),
            Err(Error::PrivateRepresentation { name }) if name == nominal("Opt")
        ));
    }
}

// Privacy is a property of surface elaboration: machinery re-deriving types from already-elaborated terms runs under `with_suppressed_privacy` (no island — no use site to judge from), which admits what enforcement rejects and restores the island on exit.
#[test]
fn privacy_suppression_admits_machinery_rederivation() {
    let mut context = context();
    register_opt(&mut context);
    make_opt_opaque(&mut context);

    let term = Term::variant(
        nominal("Opt"),
        Vec::<Term>::new(),
        "none",
        Vec::<Term>::new(),
    );
    assert!(matches!(
        elaborate(&mut context, &term, Mode::Infer),
        Err(Error::PrivateRepresentation { name }) if name == nominal("Opt")
    ));

    let admitted =
        context.with_suppressed_privacy(|context| elaborate(context, &term, Mode::Infer).is_ok());
    assert!(admitted);

    assert!(matches!(
        elaborate(&mut context, &term, Mode::Infer),
        Err(Error::PrivateRepresentation { name }) if name == nominal("Opt")
    ));
}

// The oracle's suppressions are a package: a re-validated candidate can embed machinery-built projections of private representations, and a swallowed privacy error would silently flip the verdict.
#[test]
fn oracle_suppresses_privacy_as_part_of_its_package() {
    let mut context = context();
    register_opt(&mut context);
    make_opt_opaque(&mut context);

    let term = Term::variant(
        nominal("Opt"),
        Vec::<Term>::new(),
        "none",
        Vec::<Term>::new(),
    );
    let admitted = context.with_oracle(|context| elaborate(context, &term, Mode::Infer).is_ok());
    assert!(admitted);
}
