//! An inductive match: its coverage and default, its goal over an indexed family, and the hidden payloads an arm leaves out or names under their mark.

use {
    crate::elaborate::test_support::*,
    crate::*,
    curios_common::{Plicity, Qualifier},
    curios_core::{
        Atom, Cases, Free, InductArm, InductDecl, Many, Match, Scope, Subterm, Telescope, Term,
        UniverseContext,
    },
};

#[test]
fn inductive_match_default_relaxes_coverage() {
    let mut context = context();
    register_opt(&mut context);

    // `#{goal = Nat} match some(5) | none() => 0 | _ => 99 end` — only `none` is enumerated; the un-written `some` constructor is covered by the catch-all, so this otherwise-incomplete match elaborates, at its goal.
    let term = Term::induct_match_default(
        Term::variant(nominal("Opt"), Vec::<Term>::new(), "some", [nat_lit(5)]),
        nat(),
        [("none", Vec::<Free>::new(), nat_lit(0))],
        nat_lit(99),
    );

    let (_, type_) = elaborate(&mut context, &term, Mode::Infer).unwrap();
    assert_eq!(type_, nat());
}

#[test]
fn inductive_match_missing_arm_without_default_is_rejected() {
    let mut context = context();
    register_opt(&mut context);

    // The same match without the catch-all: `some` is genuinely missing from an unindexed inductive, so coverage fails.
    let term = Term::induct_match(
        Term::variant(nominal("Opt"), Vec::<Term>::new(), "some", [nat_lit(5)]),
        nat(),
        [("none", Vec::<Free>::new(), nat_lit(0))],
    );

    assert!(elaborate(&mut context, &term, Mode::Infer).is_err());
}

// induct Tagged : Type | tag(@n : Nat, x : Nat) end — a hidden payload ahead of a plain one, the least telescope an arm's binders are aligned against.
fn register_tagged(context: &mut Context) {
    let hidden = context.fresh(Some("n"));
    let plain = context.fresh(Some("x"));
    context
        .register_induct(
            &nominal("Tagged"),
            InductDecl::new(
                UniverseContext::empty(),
                Telescope::done(Telescope::done(())),
                Vec::from([(
                    Atom::from("tag"),
                    curios_core::test_support::case(
                        0,
                        Telescope::build([(hidden, nat()), (plain, nat())], Vec::new()),
                        vec![Plicity::Implicit, Plicity::Explicit],
                    ),
                )])
                .try_into()
                .unwrap(),
                Term::type_ground(),
                Qualifier::empty(),
                true,
            ),
        )
        .unwrap();
}

// `#{goal = Nat} match tag(3, 5) | tag(<binders>) => <body> end`, the arm's binders under the marks written.
fn match_tagged(binders: &[(Plicity, &Free)], body: Term) -> Term {
    let names = binders.iter().map(|(_, name)| *name).collect::<Vec<_>>();
    Subterm::Match(Match {
        head: Term::variant(
            nominal("Tagged"),
            Vec::<Term>::new(),
            "tag",
            [nat_lit(3), nat_lit(5)],
        ),
        goal: nat(),
        cases: Cases::Induct {
            cases: Vec::from([(
                Atom::from("tag"),
                InductArm::new(
                    Scope::close(Many(names.len()), &names, body),
                    binders.iter().map(|(mark, _)| *mark).collect(),
                ),
            )]),
            default: None,
            stated: None,
        },
    })
    .into()
}

// The marks of the one arm an elaborated match over `Tagged` holds.
fn arm_marks(term: &Term) -> Vec<Plicity> {
    let Subterm::Match(Match {
        cases: Cases::Induct { cases, .. },
        ..
    }) = &**term
    else {
        panic!("expected an inductive match, got {term:?}");
    };
    cases[0].1.plicities().to_vec()
}

// An arm writes the plain payloads and leaves the hidden one out, as the call that builds the value does. The elaborated arm binds every payload under the constructor's marks, so it is elaborated again unchanged.
#[test]
fn an_arm_leaves_a_hidden_payload_out() {
    let mut context = context();
    register_tagged(&mut context);
    let x = context.fresh(Some("x"));
    let term = match_tagged(&[(Plicity::Explicit, &x)], Term::free_var(&x));

    let (elaborated, type_) = elaborate(&mut context, &term, Mode::Infer).unwrap();
    assert_eq!(type_, nat());
    assert_eq!(
        arm_marks(&elaborated),
        [Plicity::Implicit, Plicity::Explicit]
    );

    let (again, _) = elaborate(&mut context, &elaborated, Mode::Infer).unwrap();
    assert_eq!(again, elaborated);
}

// A hidden payload is written under its mark, ahead of the plain payload it precedes, where the arm names it.
#[test]
fn an_arm_names_a_hidden_payload_under_its_mark() {
    let mut context = context();
    register_tagged(&mut context);
    let n = context.fresh(Some("n"));
    let x = context.fresh(Some("x"));
    let term = match_tagged(
        &[(Plicity::Implicit, &n), (Plicity::Explicit, &x)],
        Term::free_var(&n),
    );

    let (elaborated, type_) = elaborate(&mut context, &term, Mode::Infer).unwrap();
    assert_eq!(type_, nat());
    assert_eq!(
        arm_marks(&elaborated),
        [Plicity::Implicit, Plicity::Explicit]
    );
}

// An arm's refusals are a lambda's: plain binders are counted against plain payloads, a mark of the other kind is out of order, and a marked binder with no hidden payload left stands at a plain payload or at nothing.
#[test]
fn an_arm_that_misaligns_is_refused_as_a_lambda_is() {
    let mut context = context();
    register_tagged(&mut context);
    let n = context.fresh(Some("n"));
    let x = context.fresh(Some("x"));
    let mut refusal = |binders: &[(Plicity, &Free)]| {
        let term = match_tagged(binders, nat_lit(0));
        elaborate(&mut context, &term, Mode::Infer).unwrap_err()
    };

    assert!(matches!(
        refusal(&[(Plicity::Explicit, &n), (Plicity::Explicit, &x)]),
        Error::CtorArityMismatch {
            expected: 1,
            got: 2,
            ..
        }
    ));
    assert!(matches!(
        refusal(&[]),
        Error::CtorArityMismatch {
            expected: 1,
            got: 0,
            ..
        }
    ));
    assert!(matches!(
        refusal(&[(Plicity::Witness, &n), (Plicity::Explicit, &x)]),
        Error::HiddenMemberOutOfOrder {
            written: Plicity::Witness,
            slot: Plicity::Implicit,
            ..
        }
    ));
    assert!(matches!(
        refusal(&[(Plicity::Implicit, &n), (Plicity::Implicit, &x)]),
        Error::BinderPlicityMismatch {
            site: BinderSite::Payload { .. },
            position: 2,
            expected: Plicity::Explicit,
            written: Plicity::Implicit,
            ..
        }
    ));
    assert!(matches!(
        refusal(&[(Plicity::Explicit, &x), (Plicity::Implicit, &n)]),
        Error::HiddenMemberWithoutSlot {
            written: Plicity::Implicit
        }
    ));
}

// induct Flag : (b : Nat) -> Type | off() : (0) | on() : (1) end — the minimal indexed family, for the indices a catch-all is checked at.
fn register_flag(context: &mut Context) {
    let index = context.fresh(Some("b"));
    context
        .register_induct(
            &nominal("Flag"),
            InductDecl::new(
                UniverseContext::empty(),
                Telescope::done(Telescope::build([(index, nat())], ())),
                Vec::from([
                    (
                        Atom::from("off"),
                        curios_core::test_support::case(
                            0,
                            Telescope::done(vec![nat_lit(0)]),
                            vec![],
                        ),
                    ),
                    (
                        Atom::from("on"),
                        curios_core::test_support::case(
                            0,
                            Telescope::done(vec![nat_lit(1)]),
                            vec![],
                        ),
                    ),
                ])
                .try_into()
                .unwrap(),
                Term::type_ground(),
                Qualifier::empty(),
                true,
            ),
        )
        .unwrap();
}

#[test]
fn inductive_match_default_is_allowed_on_an_indexed_family() {
    let mut context = context();
    register_flag(&mut context);

    // A `| _ =>` catch-all over an indexed family: the enumerated arm is checked at its own case target index and the default at the scrutinee's actual one.
    let term: Term = Subterm::Match(Match {
        head: Term::variant(
            nominal("Flag"),
            Vec::<Term>::new(),
            "on",
            Vec::<Term>::new(),
        ),
        goal: nat(),
        cases: Cases::Induct {
            cases: Vec::from([(
                Atom::from("on"),
                InductArm::new(Scope::close(Many(0), &[], nat_lit(0)), vec![]),
            )]),
            default: Some(nat_lit(1)),
            stated: None,
        },
    })
    .into();

    let (_, type_) = elaborate(&mut context, &term, Mode::Infer).unwrap();
    assert_eq!(type_, nat());
}
