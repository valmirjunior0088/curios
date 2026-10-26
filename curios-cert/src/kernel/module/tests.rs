use {
    crate::{Error, Kernel, Sort, check_definition, check_induct_decl, check_struct_decl, infer},
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam, Intrinsic, Level, Nat, StructDecl, StructType,
        Subterm, Telescope, Term, UniverseContext, Universes,
    },
    curios_utilities::{Plicity, Qualifier},
};

fn kernel() -> Kernel<'static> {
    Kernel::new(100_000, SYNTAX)
}

/// A declaration's binders are opened at identities of its own: the counter the kernel mints them from starts over where the budget is restored, so how many binders the walk opened before a declaration decides nothing of it. The carriers' algebra ranks an atom by a hash that takes a local's identity in, and under a counter that ran on the kernel answered one term two ways by what it had judged before. A binder still open keeps its identity, and the counter starts over past it.
#[test]
fn a_restored_budget_starts_the_identities_over() {
    let mut kernel = kernel();
    kernel.restore_budget();
    let first = kernel.fresh(None);

    // What an earlier declaration opened.
    kernel.fresh(None);
    kernel.fresh(None);
    kernel.restore_budget();
    assert_eq!(kernel.fresh(None), first);

    let held = Free::local(
        first.local_index().expect("a binder is a local") + 7,
        Some("held"),
    );
    kernel.assume(&held, &Term::intrinsic(Intrinsic::NatType));
    kernel.restore_budget();
    assert!(kernel.fresh(None).local_index() > held.local_index());
}

/// What a typing hit recalls lives as long as the typing it was filed with: a term typed under one arm, where its typing closed a group that does not descend, and typed again under another, where it closed none, recalls nothing once the first arm's entry is gone.
#[test]
fn a_typing_hit_recalls_what_its_own_typing_closed() {
    let mut kernel = kernel();
    let local = kernel.fresh(Some("x"));
    let nat = Term::intrinsic(Intrinsic::NatType);
    kernel.assume(&local, &nat);
    let term = Term::free_var(&local);

    kernel.infer_store(term.clone(), nat.clone(), true);
    kernel.memos.begin_equations();
    kernel.infer_store(term.clone(), nat, false);

    assert_eq!(
        kernel.infer_hit(&term).map(|(_, closed)| closed),
        Some(false)
    );
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

/// Declare `Fam : outer | mk(A : inner, f : (A) -> Fam) end`, with `outer` a level above `inner`, and hand back the family-type term and its declaration. A family holding a value of any type of `inner` — Russell's `set` at `Type`, the paradox of trees' `node` at `Prop` — takes this shape, and the two sorts a level apart are what make the *declaration* stand: the payload fits under the family. The paradox is the self-application a caller builds from it, not the declaration.
fn family_holding_a_type(kernel: &mut Kernel, outer: Term, inner: Term) -> (Term, InductDecl) {
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
fn holding_itself(fam_type: &Term) -> Term {
    let bound = Free::local(3, Some("x"));
    let identity = Term::func([(bound, fam_type.clone())], Term::free_var(&bound));

    Term::variant(fam(), Vec::<Term>::new(), "mk", [fam_type.clone(), identity])
}

/// Russell's paradox through a large inductive, the kernel's reading of the clash the elaborator refuses at `R: V = V/set(Regular, …)`. `V : Type 1 | set(A : Type 0, f : (A) -> V)` is a set of the values of any `Type 0`, a valid declaration since the payload fits below it; and `V/set(V, …)`, the set whose members are `V`s, hands `V` itself — a `Type 1` — to the `A : Type 0` field, which asks a type to sit strictly below itself. The declaration stands and the self-application is refused, where `curios`'s `tests::universes::hierarchy_tests` puts the paradox's own `R` to the elaborator.
#[test]
fn a_large_inductive_holding_its_own_type_is_refused_at_the_self_application() {
    let mut kernel = kernel();
    let one = Level::zero().succ().expect("level zero has a successor");
    let (fam_type, declaration) =
        family_holding_a_type(&mut kernel, Term::type_at(one), Term::type_ground());

    assert_eq!(check_induct_decl(&mut kernel, &declaration), Ok(()));
    assert!(matches!(
        infer(&mut kernel, &holding_itself(&fam_type)),
        Err(Error::Mismatch { .. }),
    ));
}

/// Coquand's paradox of trees, the same clash one sort down, which the elaborator refuses at `t: T = T/node(T, …)`. `T : Prop 1 | node(A : Prop 0, f : (A) -> T)` is a tree branching over any proposition, a valid declaration; and `T/node(T, …)` hands `T` itself — a `Prop 1` — to the `A : Prop 0` field, asking a proposition to sit strictly below itself. The declaration stands and the self-application is refused, as its `Type` twin above is.
#[test]
fn a_tree_over_every_proposition_is_refused_at_the_self_application() {
    let mut kernel = kernel();
    let one = Level::zero().succ().expect("level zero has a successor");
    let (fam_type, declaration) =
        family_holding_a_type(&mut kernel, Term::prop_at(one), Term::prop());

    assert_eq!(check_induct_decl(&mut kernel, &declaration), Ok(()));
    assert!(matches!(
        infer(&mut kernel, &holding_itself(&fam_type)),
        Err(Error::Mismatch { .. }),
    ));
}

/// Hurkens' impredicative pair, the kernel's reading of the clash the elaborator refuses at `Packed: Prop = {P: Prop, p: P}`. A strong sum of a proposition and its proof holds a `Prop`, so it sits a level above the propositions it ranges over: the kernel sizes `{P: Prop, p: P}` as a `Type`, never a `Prop`, so the carrier Girard's paradox needs one sort down is none of the propositions it would pack. `carries_information` says so, which is what refuses the definition where its sort is stated `Prop`.
#[test]
fn an_impredicative_pair_of_a_proposition_and_its_proof_is_a_type() {
    let mut kernel = kernel();
    let proposition = Free::local(0, Some("P"));
    let proof = Free::local(1, Some("p"));
    let pair = Term::tuple_type([
        (proposition, Term::prop()),
        (proof, Term::free_var(&proposition)),
    ]);

    assert!(matches!(Sort::of(&mut kernel, &pair), Ok(Sort::Type(_))));
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
        )]
        .into(),
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

/// A field's mark changes nothing of what it holds: a hidden field at `Nat`, and a concept's superclass edge at a relevant record, are each refused as a written field is. The elaborator refuses both sources before the kernel is asked (`curios`'s `tests::board::proposition_tests`), so this is the kernel's copy of the rule under each mark a generated record entry carries.
#[test]
fn a_proposition_may_not_carry_an_informative_field_under_a_mark() {
    let dictionary = Global::Authored(Qualifier::from(["Dict"]));
    let marked = |kernel: &mut Kernel, mark: Plicity, field_type: Term| {
        let name = Global::Authored(Qualifier::from(["Bad"]));
        let declaration = StructDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::build_marked(
                [(mark, Free::local(0, Some("value")), field_type)],
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

        check_struct_decl(kernel, &declaration)
    };

    let mut hidden = kernel();
    assert!(matches!(
        marked(
            &mut hidden,
            Plicity::Implicit,
            Term::intrinsic(Intrinsic::NatType),
        ),
        Err(Error::Informative { .. })
    ));

    // A relevant record standing for a data concept's dictionary.
    let mut superclass = kernel();
    let counted = StructDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build(
            [(
                Free::local(1, Some("count")),
                Term::intrinsic(Intrinsic::NatType),
            )],
            (),
        )),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };
    superclass
        .declare_struct(&dictionary, &counted)
        .expect("a name is declared once");
    let dictionary_type: Term = Subterm::StructType(StructType {
        name: dictionary,
        universes: Universes::default(),
        params: Vec::new(),
    })
    .into();
    assert!(matches!(
        marked(&mut superclass, Plicity::Witness, dictionary_type),
        Err(Error::Informative { .. })
    ));
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
fn proposition_with_field(kernel: &mut Kernel, field_type: Term) -> StructDecl {
    struct_with_field(kernel, Term::prop(), field_type)
}

/// A structure at `result_sort` with one field of `field_type` and no parameters, registered and returned for checking.
fn struct_with_field(kernel: &mut Kernel, result_sort: Term, field_type: Term) -> StructDecl {
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

/// A `struct` field is sized against the structure's level exactly as an inductive payload is: `struct Bad : Type 0 { value : Type 0 }` holds a field at its own level and is refused, the struct face of [`a_payload_at_the_familys_own_level_is_refused`]. The existing `Prop`-structure fixtures reach `check_non_informative` first and never exercise the size clause, so without this the field-sizing arm of `check_signature` could stop firing and only the `Prop` path would notice.
#[test]
fn a_struct_field_at_the_structs_own_level_is_refused() {
    let mut kernel = kernel();
    let declaration = struct_with_field(&mut kernel, Term::type_ground(), Term::type_ground());

    assert!(matches!(
        check_struct_decl(&mut kernel, &declaration),
        Err(Error::Oversized { .. }),
    ));
}

/// The control: the same field one level below the structure is admitted, so the clause refuses a field at or above the level and not every field.
#[test]
fn a_struct_field_below_the_structs_level_is_admitted() {
    let mut kernel = kernel();
    let one = Level::zero().succ().expect("level zero has a successor");
    let declaration = struct_with_field(&mut kernel, Term::type_at(one), Term::type_ground());

    assert_eq!(check_struct_decl(&mut kernel, &declaration), Ok(()));
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
                None,
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
            None,
            &UniverseContext::empty(),
        ),
        Err(Error::NoProof(_)),
    ));
}

/// The sort a definition states its value inhabits is held to the sort the value has: `Nat` stated a proposition is refused, and stated at `Type` admitted, so no judgment reads a sort off the name that its reduct denies.
#[test]
fn a_definition_stating_its_value_at_a_sort_it_does_not_inhabit_is_refused() {
    let at = |stated: Term| {
        check_definition(
            &mut kernel(),
            &lemma(),
            &Term::type_ground(),
            &Term::intrinsic(Intrinsic::NatType),
            false,
            Some(&stated),
            &UniverseContext::empty(),
        )
    };

    assert!(matches!(at(Term::prop()), Err(Error::ValueSort { .. })));
    assert_eq!(at(Term::type_ground()), Ok(()));
}

/// The sort a definition states its value inhabits is held to its level as well as to its quality: the value `Type 0` inhabits `Type 1`, and stated at `Type 0` it is refused. The level is read with nothing run: a list's sort is its element's as a lookup classifies it, the lookup reads the stated sort, and stated a level low `List(lemma)` would sit at `Type 0` over elements that are the small types themselves, a universe holding a code for itself. Stated at `Type 1`, the list sits there.
///
/// Mutation-checked: with `hold_value_sort` comparing the two sorts' qualities alone, `Type 0` stated is admitted.
#[test]
fn a_definition_stating_its_value_a_level_below_the_one_it_inhabits_is_refused() {
    let one = Level::zero().succ().expect("level zero has a successor");
    let at = |kernel: &mut Kernel, stated: Term| {
        check_definition(
            kernel,
            &lemma(),
            &Term::type_at(one.clone()),
            &Term::type_ground(),
            false,
            Some(&stated),
            &UniverseContext::empty(),
        )
    };

    assert!(matches!(
        at(&mut kernel(), Term::type_ground()),
        Err(Error::ValueSort { .. })
    ));

    let mut kernel = kernel();
    assert_eq!(at(&mut kernel, Term::type_at(one.clone())), Ok(()));
    assert_eq!(
        infer(
            &mut kernel,
            &Term::intrinsic(Intrinsic::list_type(Term::free_var(&lemma())))
        ),
        Ok(Term::type_at(one)),
    );
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
            None,
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
            None,
            &UniverseContext::empty(),
        ),
        Ok(()),
    );
    assert_eq!(kernel.value(&lemma()), None);
}
