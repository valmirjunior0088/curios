use {
    crate::{Error, Kernel, check_definition, check_induct_decl, check_struct_decl},
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam, Intrinsic, Level, Nat, StructDecl, Telescope,
        Term, UniverseContext,
    },
    curios_utilities::{Plicity, Qualifier},
};

fn kernel() -> Kernel<'static> {
    Kernel::new(100_000, SYNTAX)
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
            InductParam::new(
                Telescope::build([(payload, payload_type)], Vec::new()),
                vec![Plicity::Explicit],
            ),
        )],
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

/// `induct Bad : Type 0 | mk(x : Type 0)` contains the universe it lives in. This is the fixture no surface program can spell — levels have no syntax — and the clause the item walk cannot check, because it computes each signature's sort and compares it to nothing.
#[test]
fn a_payload_at_the_familys_own_level_is_refused() {
    let mut kernel = kernel();
    let declaration = family(&mut kernel, Term::type_ground(), Term::type_ground());

    assert!(matches!(
        check_induct_decl(&mut kernel, &declaration),
        Err(Error::Oversized { .. }),
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

/// A `Prop`-sorted family is sized as a `Type`-sorted one is: `induct Box : Prop | mk(x : Type 0)` holds a payload a level above it. Left unsized, a payload quantifying over the sort its family sits in is applied at the family, and the application reads as a descent.
#[test]
fn a_proposition_family_is_sized_as_a_type_family_is() {
    let mut kernel = kernel();
    let declaration = family(&mut kernel, Term::prop(), Term::type_ground());

    assert!(matches!(
        check_induct_decl(&mut kernel, &declaration),
        Err(Error::Oversized { .. }),
    ));
}

/// `Box : Prop 1 | mk(x : Type 0)`: the payload fits under a proposition at its own level.
#[test]
fn a_payload_below_a_propositions_level_is_admitted() {
    let mut kernel = kernel();
    let one = Level::zero().succ().expect("level zero has a successor");
    let declaration = family(&mut kernel, Term::prop_at(one), Term::type_ground());

    assert_eq!(check_induct_decl(&mut kernel, &declaration), Ok(()));
}

/// A proof counts at its proposition's level, in a data family as in a proposition: `induct Box : Type 0 | mk(x : (A : Prop) -> A)` holds a proof of a proposition one level up. Exempt, a data family squashes a large proposition into the sort it ranges over.
#[test]
fn a_proof_payload_counts_at_its_propositions_level() {
    let mut kernel = kernel();
    let declaration = family(&mut kernel, Term::type_ground(), large_proposition());

    assert!(matches!(
        check_induct_decl(&mut kernel, &declaration),
        Err(Error::Oversized { .. }),
    ));
}

/// The control: the same proof fits under a family one level up.
#[test]
fn a_proof_payload_below_the_familys_level_is_admitted() {
    let mut kernel = kernel();
    let one = Level::zero().succ().expect("level zero has a successor");
    let declaration = family(&mut kernel, Term::type_at(one), large_proposition());

    assert_eq!(check_induct_decl(&mut kernel, &declaration), Ok(()));
}

/// `(A : Prop) -> A`: a proposition at level one, since it quantifies over the ground propositions.
fn large_proposition() -> Term {
    let a = Free::local(1, Some("A"));

    Term::func_type([(a, Term::prop())], Term::free_var(&a))
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
        arity: Telescope::build([(t, Term::type_ground())], Telescope::done(())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam::new(
                Telescope::build(
                    [(t, Term::type_ground()), (x, Term::free_var(&t))],
                    Vec::new(),
                ),
                vec![Plicity::Implicit, Plicity::Explicit],
            ),
        )],
        result_sort: Term::type_ground(),
        module: Qualifier::from(["Vec"]),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };
    kernel
        .declare_induct(&name, &declaration)
        .expect("a name is declared once");

    assert_eq!(check_induct_decl(&mut kernel, &declaration), Ok(()));
}

/// A constructor's parameter prefix must be the declaration's own parameters, not merely as many binders.
///
/// A `Telescope` is closed — it carries its own binders — and a constructor's payload types mention the family's parameters, so the parameters are repeated as the constructor telescope's prefix. That repetition is the price of self-contained telescopes and is kept deliberately; what is *not* optional is that the two agree, because [`InductDecl::instantiate`] substitutes the family's actual parameters into those slots. A prefix declaring one at a different domain types the payload against a family this declaration does not describe.
///
/// The family below declares `mk`'s parameter slot at `Nat` while the family declares it at `Type`: a size condition that asks `Sort::of` of each domain alone, or a terminal clause comparing the constructed type's parameters against the *minted binder identities* rather than the arity's domains, would pass it. Its payload is deliberately `Nat` rather than `T`, so that the refusal comes from the prefix clause and not from `Sort::of` tripping over a non-sort.
///
/// Its control is [`a_matching_parameter_prefix_is_admitted`], the same declaration with the prefix at `Type`: the clause must refuse a disagreement, not every constructor.
#[test]
fn a_parameter_prefix_that_disagrees_with_the_family_is_refused() {
    let mut kernel = kernel();
    let declaration = prefixed(&mut kernel, Term::intrinsic(Intrinsic::NatType));

    assert!(matches!(
        check_induct_decl(&mut kernel, &declaration),
        Err(Error::Mismatch { .. }),
    ));
}

/// A constructor telescope shorter than the family's parameter prefix is refused, not crashed on.
///
/// The prefix walk takes `param_count` entries, so a shorter telescope would end the loop early without complaint and hand the short parameter list to `indices_at`, which panics on `Telescope::open`'s arity assert. A malformed registry entry is the program's fault, and the kernel answers for the program's faults with a refusal.
#[test]
fn a_constructor_telescope_shorter_than_the_parameter_prefix_is_refused() {
    let mut kernel = kernel();
    let t = Free::local(0, Some("T"));

    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::build([(t, Term::type_ground())], Telescope::done(())),
        constructors: vec![(
            Atom::from("mk"),
            InductParam::new(Telescope::done(Vec::new()), Vec::new()),
        )],
        result_sort: Term::type_ground(),
        module: Qualifier::from(["Fam"]),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };
    kernel
        .declare_induct(&fam(), &declaration)
        .expect("a name is declared once");

    assert!(matches!(
        check_induct_decl(&mut kernel, &declaration),
        Err(Error::Arity { .. }),
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
        )],
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

/// A `Prop`-sorted structure carrying a `Nat`, which is the shape `Prop` non-informativeness exists to forbid.
///
/// Irrelevance identifies every inhabitant of a proposition, and a structure's payload is read back by *projection*, which is not an elimination and so meets no large-elimination guard: an informative field hands two convertible values to the same projection, and `Eq` plus congruence turns that into `False`.
///
/// The kernel holds the rule itself rather than leaving it to `curios-elab`: the elaborator refuses such a declaration before the kernel is asked, so no program backs the kernel's copy up, and `invert.rs`'s irrelevance guard cites this very property as its reason for leaving structures undecomposed.
#[test]
fn a_proposition_may_not_carry_an_informative_field() {
    let mut kernel = kernel();
    let declaration = proposition_with_field(&mut kernel, Term::intrinsic(Intrinsic::NatType));

    assert!(matches!(
        check_struct_decl(&mut kernel, &declaration),
        Err(Error::Informative { .. })
    ));
}

/// A `Prop`-sorted structure carrying a *type*, which the same rule forbids for the same reason.
///
/// `carried : Type 0` makes the field a type held as data. Irrelevance identifies `Bad{{}}` with `Bad{False}` at `Bad`, and `.0` reads the carried type straight back out — a projection meets no elimination guard at all — so `{}` and `False` become convertible, and `subst` turns `()` into a closed inhabitant of `False`.
///
/// This is the field-side face of the elimination-side hole in `super::super::infer::eliminate::tests`: both gates ask `carries_information`, which must count a universe-typed field as carrying information however completely erasure deletes it. No `.crs` file reaches it — `curios-elab` refuses the declaration by its own `is_prop` test, so the kernel is never asked from source, and only this fixture holds `check_struct_decl` to it.
#[test]
fn a_proposition_may_not_carry_a_type() {
    let mut kernel = kernel();
    let declaration = proposition_with_field(&mut kernel, Term::type_ground());

    assert!(matches!(
        check_struct_decl(&mut kernel, &declaration),
        Err(Error::Informative { .. })
    ));
}

/// The other end of the discrimination: a proposition whose field is *itself* a proposition carries nothing a program can read, and must stay legal — a rule that refused every `Prop` structure would be indistinguishable from one that refused this.
///
/// The field type is a `Prop`-sorted family, not the universe `Prop`. Typed at `Term::prop()`, the field would be a proposition held *as data* rather than a proof of one — precisely the shape the fixture above forbids — and this control would pass for the wrong reason.
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
    kernel
        .declare_induct(
            &name,
            &InductDecl {
                universe_context: UniverseContext::default(),
                arity: Telescope::done(Telescope::done(())),
                constructors: Vec::new(),
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
        variances: Vec::new(),
    };
    kernel
        .declare_struct(&name, &declaration)
        .expect("a name is declared once");

    declaration
}

/// The name the definitions below are checked under.
fn lemma() -> Free {
    Free::from(&Global::Authored(Qualifier::from(["lemma"])))
}

/// A definition marked a proof is checked as any other and brought into scope with nothing to unfold to: its body is its own check's to read, and no judgment's after it. Left unmarked, the same definition unfolds.
#[test]
fn a_definition_marked_a_proof_unfolds_to_nothing() {
    for proof in [true, false] {
        let mut kernel = kernel();
        family(&mut kernel, Term::prop(), Term::tuple_type_unit());
        let statement = Term::induct_type(fam(), Vec::<Term>::new(), Vec::<Term>::new());
        let body = Term::variant(
            fam(),
            Vec::<Term>::new(),
            "mk",
            [Term::tuple(Vec::<Term>::new())],
        );

        assert_eq!(
            check_definition(
                &mut kernel,
                &lemma(),
                &statement,
                &body,
                proof,
                &UniverseContext::empty(),
            ),
            Ok(()),
        );
        assert_eq!(kernel.value(&lemma()), (!proof).then_some(&body));
    }
}

/// The mark is held to the sort of the type it is stated at. A number that unfolded to nothing would be one no judgment could compute with, so a definition marked a proof at `Nat` is refused.
#[test]
fn a_definition_marked_a_proof_at_no_proposition_is_refused() {
    let mut kernel = kernel();

    assert!(matches!(
        check_definition(
            &mut kernel,
            &lemma(),
            &Term::intrinsic(Intrinsic::NatType),
            &Term::intrinsic(Intrinsic::Nat(Nat::new(2usize))),
            true,
            &UniverseContext::empty(),
        ),
        Err(Error::NoProof(_)),
    ));
}

/// A statement that reduces to a proposition is typed at `Prop` as written, and its proof is one: the mark is read off the sort the statement is typed at, with the statement unreduced. `((T: Prop) => T)(Fam)` is typed at `Prop` and is `Fam`. The same redex over `Type` is no statement of a proposition at all: no proposition stands where a type is wanted.
#[test]
fn a_definition_marked_a_proof_at_a_statement_that_reduces_to_a_proposition_is_admitted() {
    let mut kernel = kernel();
    family(&mut kernel, Term::prop(), Term::tuple_type_unit());
    let t = Free::local(1, Some("T"));
    let over = |sort: Term| {
        Term::apply(
            Term::func([(t, sort)], Term::free_var(&t)),
            [Term::induct_type(
                fam(),
                Vec::<Term>::new(),
                Vec::<Term>::new(),
            )],
        )
    };
    assert!(matches!(
        check_definition(
            &mut kernel,
            &Free::local(2, Some("at_a_type")),
            &over(Term::type_ground()),
            &Term::variant(
                fam(),
                Vec::<Term>::new(),
                "mk",
                [Term::tuple(Vec::<Term>::new())],
            ),
            true,
            &UniverseContext::empty(),
        ),
        Err(Error::Mismatch { .. }),
    ));
    let statement = Term::apply(
        Term::func([(t, Term::prop())], Term::free_var(&t)),
        [Term::induct_type(
            fam(),
            Vec::<Term>::new(),
            Vec::<Term>::new(),
        )],
    );
    let body = Term::variant(
        fam(),
        Vec::<Term>::new(),
        "mk",
        [Term::tuple(Vec::<Term>::new())],
    );

    assert_eq!(
        check_definition(
            &mut kernel,
            &lemma(),
            &statement,
            &body,
            true,
            &UniverseContext::empty(),
        ),
        Ok(()),
    );
    assert_eq!(kernel.value(&lemma()), None);
}
