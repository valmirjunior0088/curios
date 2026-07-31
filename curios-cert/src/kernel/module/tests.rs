use {
    crate::{Kernel, KernelError, check_induct_decl, check_struct_decl},
    curios_base::{Plicity, Qualifier, RootId},
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam, Level, Polarity, StructDecl, Telescope, Term,
        UniverseContext,
    },
};

fn kernel() -> Kernel {
    let mut kernel = Kernel::new(100_000);
    kernel.set_local_floor(1_000);
    kernel
}

/// One single-constructor family whose payload is `payload_type`, declared at `result_sort` with no parameters, registered and returned for checking.
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

/// `induct Bad : Type 0 | mk(x : Type 0)` contains the universe it lives in. This is the fixture no surface program can spell — levels have no syntax — and the clause the item walk cannot check, because it computes each signature's sort and compares it to nothing.
#[test]
fn a_payload_at_the_familys_own_level_is_refused() {
    let mut kernel = kernel();
    let declaration = family(&mut kernel, Term::type_ground(), Term::type_ground());

    assert!(matches!(
        check_induct_decl(&mut kernel, &declaration),
        Err(KernelError::Oversized { .. }),
    ));
}

/// `Box : Type 1 | mk(x : Type 0)` is the stratification working: the payload sorts one level below the family.
#[test]
fn a_payload_below_the_familys_level_is_admitted() {
    let mut kernel = kernel();
    let one = Level::zero().succ().expect("level zero has a successor");
    let declaration = family(&mut kernel, Term::type_at(one), Term::type_ground());

    assert_eq!(check_induct_decl(&mut kernel, &declaration), Ok(()));
}

/// A `Prop`-sorted family carries no size condition: `Prop` is impredicative, and the large-elimination guard is what keeps that sound.
#[test]
fn a_proposition_family_has_no_size_condition() {
    let mut kernel = kernel();
    let declaration = family(&mut kernel, Term::prop(), Term::type_ground());

    assert_eq!(check_induct_decl(&mut kernel, &declaration), Ok(()));
}

/// A uniform parameter gets one rung of slack: a family at `Type 0` may take a `T : Type` parameter — the parameter's domain sorts at `1 ≤ 0 + 1` — while the same domain as a *payload* is refused above.
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

/// A `Prop`-sorted structure carrying a `Nat`, which is the shape `Prop` non-informativeness exists to forbid.
///
/// Irrelevance identifies every inhabitant of a proposition, and a structure's payload is read back by *projection*, which is not an elimination and so meets no large-elimination guard: an informative field hands two convertible values to the same projection, and `Eq` plus congruence turns that into `False`.
///
/// This crate used to leave the rule entirely to `curios-elab` — `check_struct_decl` ran only the size condition, which returns immediately for a result sort that is not `Type`. The two-checker matrix records the consequence: `informative_prop_field` reaches the elaborator and the kernel is never asked, so nothing here backed the rule up. `invert.rs`'s irrelevance guard cites this very property as its reason for leaving structures undecomposed, which is what made the omission load-bearing rather than academic.
#[test]
fn a_proposition_may_not_carry_an_informative_field() {
    let mut kernel = kernel();
    let declaration = proposition_with_field(&mut kernel, Term::prim(curios_core::Prim::NatType));

    assert!(matches!(
        check_struct_decl(&mut kernel, &declaration),
        Err(KernelError::Informative { .. })
    ));
}

/// The other end of the discrimination: a proposition whose field is *itself* a proposition carries nothing a program can read, and must stay legal — a rule that refused every `Prop` structure would be indistinguishable from one that refused this.
#[test]
fn a_proposition_may_carry_a_proof() {
    let mut kernel = kernel();
    let declaration = proposition_with_field(&mut kernel, Term::prop());

    assert!(check_struct_decl(&mut kernel, &declaration).is_ok());
}

/// A `Prop`-sorted structure with one field of `field_type` and no parameters, registered and returned for checking.
fn proposition_with_field(kernel: &mut Kernel, field_type: Term) -> StructDecl {
    let name = Global::Authored(Qualifier::from(["Bad"]));
    let declaration = StructDecl {
        universe_context: UniverseContext::empty(),
        params: Telescope::Done(Box::new(())),
        fields: Telescope::Cons(
            field_type,
            curios_core::Scope::close(
                curios_core::One,
                &[&Free::local(0, Some("value"))],
                Telescope::Done(Box::new(())),
            ),
        ),
        result_sort: Term::prop(),
        module: Qualifier::default(),
        root: RootId::Entry,
        rep_public: true,
        polarities: Vec::<Polarity>::new(),
    };
    kernel.declare_struct(&name, &declaration);

    declaration
}
