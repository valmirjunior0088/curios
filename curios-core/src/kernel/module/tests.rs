use {
    crate::{
        Atom, Free, Global, InductDecl, InductParam, Kernel, KernelError, Level, Telescope, Term,
        UniverseContext, kernel::module::check_induct_decl,
    },
    curios_base::{Plicity, Qualifier, RootId},
};

fn kernel() -> Kernel {
    let mut kernel = Kernel::new(100_000);
    kernel.set_local_floor(1_000);
    kernel
}

/// One single-constructor family whose payload is `payload_type`, declared at
/// `result_sort` with no parameters, registered and returned for checking.
fn family(kernel: &mut Kernel, result_sort: Term, payload_type: Term) -> InductDecl {
    let name = Global::Authored(Qualifier::from(["Fam"]));
    let payload = Free::local(0, Some("x"));
    let constructed = Term::induct_type(name.clone(), Vec::<Term>::new(), Vec::<Term>::new());

    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        params: Telescope::done(()),
        indices: Telescope::done(()),
        constructors: vec![(
            Atom::from("mk"),
            InductParam {
                telescope: Telescope::build([(payload, payload_type)], constructed),
                plicities: vec![Plicity::Explicit],
            },
        )],
        result_sort,
        module: Qualifier::from(["Fam"]),
        root: RootId::Entry,
        rep_public: true,
        polarities: Vec::new(),
    };
    kernel.declare_induct(&name, &declaration);

    declaration
}

/// `induct Bad : Type 0 | mk(x : Type 0)` contains the universe it lives in.
/// This is the fixture no surface program can spell — levels have no syntax —
/// and the clause the item walk cannot check, because it computes each
/// signature's sort and compares it to nothing.
#[test]
fn a_payload_at_the_familys_own_level_is_refused() {
    let mut kernel = kernel();
    let declaration = family(&mut kernel, Term::type_ground(), Term::type_ground());

    assert!(matches!(
        check_induct_decl(&mut kernel, &declaration),
        Err(KernelError::Oversized { .. }),
    ));
}

/// `Box : Type 1 | mk(x : Type 0)` is the stratification working: the payload
/// sorts one level below the family.
#[test]
fn a_payload_below_the_familys_level_is_admitted() {
    let mut kernel = kernel();
    let one = Level::zero().succ().expect("level zero has a successor");
    let declaration = family(&mut kernel, Term::type_at(one), Term::type_ground());

    assert_eq!(check_induct_decl(&mut kernel, &declaration), Ok(()));
}

/// A `Prop`-sorted family carries no size condition: `Prop` is impredicative,
/// and the large-elimination guard is what keeps that sound.
#[test]
fn a_proposition_family_has_no_size_condition() {
    let mut kernel = kernel();
    let declaration = family(&mut kernel, Term::prop(), Term::type_ground());

    assert_eq!(check_induct_decl(&mut kernel, &declaration), Ok(()));
}

/// A uniform parameter gets one rung of slack: a family at `Type 0` may take a
/// `T : Type` parameter — the parameter's domain sorts at `1 ≤ 0 + 1` — while
/// the same domain as a *payload* is refused above.
#[test]
fn a_uniform_parameter_has_one_rung_of_slack() {
    let mut kernel = kernel();
    let name = Global::Authored(Qualifier::from(["Vec"]));
    let t = Free::local(0, Some("T"));
    let x = Free::local(1, Some("x"));
    let constructed = Term::induct_type(name.clone(), [Term::free_var(&t)], Vec::<Term>::new());

    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        params: Telescope::build([(t.clone(), Term::type_ground())], ()),
        indices: Telescope::done(()),
        constructors: vec![(
            Atom::from("mk"),
            InductParam {
                telescope: Telescope::build(
                    [(t.clone(), Term::type_ground()), (x, Term::free_var(&t))],
                    constructed,
                ),
                plicities: vec![Plicity::Implicit, Plicity::Explicit],
            },
        )],
        result_sort: Term::type_ground(),
        module: Qualifier::from(["Vec"]),
        root: RootId::Entry,
        rep_public: true,
        polarities: Vec::new(),
    };
    kernel.declare_induct(&name, &declaration);

    assert_eq!(check_induct_decl(&mut kernel, &declaration), Ok(()));
}
