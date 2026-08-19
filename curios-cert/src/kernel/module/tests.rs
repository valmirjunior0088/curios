use {
    crate::{Kernel, KernelError, check_induct_decl, check_struct_decl},
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam, Intrinsic, Level, StructDecl, Telescope, Term,
        UniverseContext,
    },
    curios_utilities::{Plicity, Qualifier},
};

fn kernel() -> Kernel {
    let mut kernel = Kernel::new(100_000, crate::fixture::SYNTAX);
    kernel.set_local_floor(1_000);
    kernel
}

/// The name [`family`] registers its declaration under.
fn fam() -> Global {
    Global::Authored(Qualifier::from(["Fam"]))
}

/// One single-constructor family whose payload is `payload_type`, declared at `result_sort` with no parameters, registered and returned for checking.
fn family(kernel: &mut Kernel, result_sort: Term, payload_type: Term) -> InductDecl {
    let payload = Free::local(0, Some("x"));

    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam {
                telescope: Telescope::build([(payload, payload_type)], Vec::new()),
                plicities: vec![Plicity::Explicit],
            },
        )],
        result_sort,
        module: Qualifier::from(["Fam"]),
        rep_public: true,
        polarities: Vec::new(),
    };
    kernel.declare_induct(&fam(), &declaration);

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

    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::build([(t.clone(), Term::type_ground())], Telescope::done(())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam {
                telescope: Telescope::build(
                    [(t.clone(), Term::type_ground()), (x, Term::free_var(&t))],
                    Vec::new(),
                ),
                plicities: vec![Plicity::Implicit, Plicity::Explicit],
            },
        )],
        result_sort: Term::type_ground(),
        module: Qualifier::from(["Vec"]),
        rep_public: true,
        polarities: Vec::new(),
    };
    kernel.declare_induct(&name, &declaration);

    assert_eq!(check_induct_decl(&mut kernel, &declaration), Ok(()));
}

/// A constructor's parameter prefix must be the declaration's own parameters, not merely as many binders.
///
/// A `Telescope` is closed — it carries its own binders — and a constructor's payload types mention the family's parameters, so the parameters are repeated as the constructor telescope's prefix. That repetition is the price of self-contained telescopes and is kept deliberately; what is *not* optional is that the two agree, because [`InductDecl::instantiate`] substitutes the family's actual parameters into those slots. A prefix declaring one at a different domain types the payload against a family this declaration does not describe.
///
/// Nothing checked it. `check_signature` asked `Sort::of` on each domain for the size condition alone, and the terminal clause compared the constructed type's parameters against the *minted binder identities* rather than against the arity's domains — so the family below, whose `mk` declares its parameter slot at `Nat` while the family declares it at `Type`, passed both. Its payload is deliberately `Nat` rather than `T`, so that the refusal comes from the prefix clause and not from `Sort::of` tripping over a non-sort.
///
/// Its control is [`a_matching_parameter_prefix_is_admitted`], the same declaration with the prefix at `Type`: the clause must refuse a disagreement, not every constructor.
#[test]
fn a_parameter_prefix_that_disagrees_with_the_family_is_refused() {
    let mut kernel = kernel();
    let declaration = prefixed(&mut kernel, Term::intrinsic(Intrinsic::NatType));

    assert!(matches!(
        check_induct_decl(&mut kernel, &declaration),
        Err(KernelError::Mismatch { .. }),
    ));
}

/// A constructor telescope shorter than the family's parameter prefix is refused, not crashed on.
///
/// The prefix walk takes `param_count` entries, so a shorter telescope used to end the loop early without complaint and hand the short parameter list to `indices_at`, which panics on `Telescope::open`'s arity assert. A malformed registry entry is the program's fault, and the kernel answers for the program's faults with a refusal.
#[test]
fn a_constructor_telescope_shorter_than_the_parameter_prefix_is_refused() {
    let mut kernel = kernel();
    let t = Free::local(0, Some("T"));

    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::build([(t, Term::type_ground())], Telescope::done(())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam {
                telescope: Telescope::done(Vec::new()),
                plicities: Vec::new(),
            },
        )],
        result_sort: Term::type_ground(),
        module: Qualifier::from(["Fam"]),
        rep_public: true,
        polarities: Vec::new(),
    };
    kernel.declare_induct(&fam(), &declaration);

    assert!(matches!(
        check_induct_decl(&mut kernel, &declaration),
        Err(KernelError::Arity { .. }),
    ));
}

/// The control for the fixture above: a prefix that does agree stays admitted.
#[test]
fn a_matching_parameter_prefix_is_admitted() {
    let mut kernel = kernel();
    let declaration = prefixed(&mut kernel, Term::type_ground());

    assert_eq!(check_induct_decl(&mut kernel, &declaration), Ok(()));
}

/// `Fam(T : Type) | mk(_ : Nat)`, with the constructor telescope's *parameter slot* declared at `prefix` rather than at the family's `Type`.
fn prefixed(kernel: &mut Kernel, prefix: Term) -> InductDecl {
    let t = Free::local(0, Some("T"));
    let payload = Free::local(1, Some("n"));

    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::build([(t.clone(), Term::type_ground())], Telescope::done(())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam {
                telescope: Telescope::build(
                    [(t, prefix), (payload, Term::intrinsic(Intrinsic::NatType))],
                    Vec::new(),
                ),
                plicities: vec![Plicity::Implicit, Plicity::Explicit],
            },
        )],
        result_sort: Term::type_ground(),
        module: Qualifier::from(["Fam"]),
        rep_public: true,
        polarities: Vec::new(),
    };
    kernel.declare_induct(&fam(), &declaration);

    declaration
}

/// A `Prop`-sorted structure carrying a `Nat`, which is the shape `Prop` non-informativeness exists to forbid.
///
/// Irrelevance identifies every inhabitant of a proposition, and a structure's payload is read back by *projection*, which is not an elimination and so meets no large-elimination guard: an informative field hands two convertible values to the same projection, and `Eq` plus congruence turns that into `False`.
///
/// This crate used to leave the rule entirely to `curios-elab` — `check_struct_decl` ran only the size condition, which returns immediately for a result sort that is not `Type`. The two-checker matrix records the consequence: `informative_prop_field` reaches the elaborator and the kernel is never asked, so nothing here backed the rule up. `invert.rs`'s irrelevance guard cites this very property as its reason for leaving structures undecomposed, which is what made the omission load-bearing rather than academic.
#[test]
fn a_proposition_may_not_carry_an_informative_field() {
    let mut kernel = kernel();
    let declaration = proposition_with_field(&mut kernel, Term::intrinsic(Intrinsic::NatType));

    assert!(matches!(
        check_struct_decl(&mut kernel, &declaration),
        Err(KernelError::Informative { .. })
    ));
}

/// A `Prop`-sorted structure carrying a *type*, which the same rule forbids for the same reason.
///
/// `carried : Type 0` makes the field a type held as data. Irrelevance identifies `Bad{{}}` with `Bad{False}` at `Bad`, and `.0` reads the carried type straight back out — a projection meets no elimination guard at all — so `{}` and `False` become convertible, and `subst` turns `()` into a closed inhabitant of `False`.
///
/// This is the field-side face of the elimination-side hole in `super::super::infer::eliminate::tests`: both gates asked `carries_information`, which reported a universe-typed field as carrying nothing on the grounds that erasure deletes a type. No `.crs` file reached it — `curios-elab` refuses the declaration by its own `is_prop` test, which is why the two-checker matrix records `informative_prop_field` as never reaching the kernel — but `check_struct_decl` returned `Ok(())` while the hole was open.
#[test]
fn a_proposition_may_not_carry_a_type() {
    let mut kernel = kernel();
    let declaration = proposition_with_field(&mut kernel, Term::type_ground());

    assert!(matches!(
        check_struct_decl(&mut kernel, &declaration),
        Err(KernelError::Informative { .. })
    ));
}

/// The other end of the discrimination: a proposition whose field is *itself* a proposition carries nothing a program can read, and must stay legal — a rule that refused every `Prop` structure would be indistinguishable from one that refused this.
///
/// The field type is a `Prop`-sorted family, not the universe `Prop`. It was written `Term::prop()` until the universe-typed field above was refused, which typed the field as a proposition held *as data* rather than as a proof of one — precisely the shape now forbidden. So this control passed for the wrong reason, and the discrimination it names went untested for as long as the hole was open.
#[test]
fn a_proposition_may_carry_a_proof() {
    let mut kernel = kernel();
    let held = proposition(&mut kernel, "Held");
    let declaration = proposition_with_field(&mut kernel, held);

    assert!(check_struct_decl(&mut kernel, &declaration).is_ok());
}

/// A `Prop`-sorted family with no constructors, so that a field typed by it is a genuine proof rather than a proposition held as data.
fn proposition(kernel: &mut Kernel, path: &str) -> Term {
    let name = Global::Authored(Qualifier::from([path]));
    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::done(())),
            constructors: Vec::new(),
            result_sort: Term::prop(),
            module: Qualifier::from([path]),
            rep_public: true,
            polarities: Vec::new(),
        },
    );

    Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new())
}

/// A `Prop`-sorted structure with one field of `field_type` and no parameters, registered and returned for checking.
fn proposition_with_field(kernel: &mut Kernel, field_type: Term) -> StructDecl {
    let name = Global::Authored(Qualifier::from(["Bad"]));
    let declaration = StructDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build(
            [(Free::local(0, Some("value")), field_type)],
            (),
        )),
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
    };
    kernel.declare_struct(&name, &declaration);

    declaration
}
