//! An inductive declaration: its payloads sized against the family's level, the self-applications the known paradoxes need, a uniform parameter's slack, and a constructor's parameter prefix.

use {
    super::test_support::*,
    crate::{Error, Sort, check_induct_decl, infer},
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam, Intrinsic, Level, Telescope, Term,
        UniverseContext,
    },
    curios_utilities::{Plicity, Qualifier},
};

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

/// An intrinsic carrier of propositions is data at their level, and a list of the ground propositions themselves sits where they do, a level above them: `induct Box : Prop | mk(x : List(Prop))` holds a code for every proposition of its own level, the carrier Coquand's paradox of trees needs, and is refused as `mk(x : Prop)` is. `Box : Prop 1` holds it, which is the control.
///
/// Mutation-checked at `sort_of_intrinsic`: a carrier pinned at level zero admits the small proposition.
#[test]
fn a_proposition_holding_a_list_of_its_own_propositions_is_refused() {
    let codes = || Term::intrinsic(Intrinsic::ListType(Term::prop()));

    let mut kernel = kernel();
    let declaration = family(&mut kernel, Term::prop(), codes());
    assert!(matches!(
        check_induct_decl(&mut kernel, &declaration),
        Err(Error::Oversized { .. }),
    ));

    let mut kernel = self::kernel();
    let one = Level::zero().succ().expect("level zero has a successor");
    let declaration = family(&mut kernel, Term::prop_at(one), codes());
    assert_eq!(check_induct_decl(&mut kernel, &declaration), Ok(()));
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
