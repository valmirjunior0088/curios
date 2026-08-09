use {
    crate::{Kernel, KernelError, Sort},
    curios_base::{Qualifier, RootId},
    curios_core::{
        Free, Global, InductDecl, Intrinsic, Level, Many, RecGroup, RecMemberScopes, Scope,
        Telescope, Term, UniverseContext,
    },
};

fn kernel() -> Kernel {
    let mut kernel = Kernel::new(100_000);
    kernel.set_local_floor(1_000);
    kernel
}

fn nominal(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

/// A nominal family with no constructors, declared at `result_sort`. Enough to give the tests below a base type at a known sort — the registry is what says whether a nominal type is a proposition, so there is no way to build one without it.
fn declare(kernel: &mut Kernel, path: &str, result_sort: Term) -> Term {
    let name = nominal(path);

    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::done(())),
            constructors: Vec::new(),
            result_sort,
            module: Qualifier::from([path]),
            root: RootId::Entry,
            rep_public: true,
            polarities: Vec::new(),
        },
    );

    Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new())
}

#[test]
fn an_intrinsic_type_sits_at_level_zero() {
    let mut kernel = kernel();

    assert_eq!(
        Sort::of(&mut kernel, &Term::intrinsic(Intrinsic::NatType)),
        Ok(Sort::Type(Level::zero())),
    );
}

/// `Type u : Type (u + 1)`, and `Prop : Type 0`. Both are the sort of a universe, not the universe itself.
#[test]
fn a_universe_is_one_level_above_itself() {
    let mut kernel = kernel();

    assert_eq!(
        Sort::of(&mut kernel, &Term::type_ground()),
        Ok(Sort::Type(
            Level::zero().succ().expect("level zero succeeds")
        )),
    );
    assert_eq!(
        Sort::of(&mut kernel, &Term::prop()),
        Ok(Sort::Type(Level::zero())),
    );
}

#[test]
fn a_nominal_types_sort_is_the_one_its_declaration_states() {
    let mut kernel = kernel();
    let proposition = declare(&mut kernel, "P", Term::prop());
    let data = declare(&mut kernel, "D", Term::type_ground());

    assert_eq!(Sort::of(&mut kernel, &proposition), Ok(Sort::Prop));
    assert_eq!(Sort::of(&mut kernel, &data), Ok(Sort::Type(Level::zero())),);
}

/// Π into a proposition is a proposition however large its domain, which is what makes `(n : Nat) -> P(n)` erasable.
#[test]
fn a_function_into_a_proposition_is_a_proposition() {
    let mut kernel = kernel();
    let proposition = declare(&mut kernel, "P", Term::prop());
    let binder = Free::local(0, Some("n"));

    let pi = Term::func_type([(binder, Term::intrinsic(Intrinsic::NatType))], proposition);

    assert_eq!(Sort::of(&mut kernel, &pi), Ok(Sort::Prop));
}

#[test]
fn a_function_into_data_takes_the_join_of_its_parts() {
    let mut kernel = kernel();
    let binder = Free::local(0, Some("n"));

    let pi = Term::func_type(
        [(binder, Term::intrinsic(Intrinsic::NatType))],
        Term::intrinsic(Intrinsic::NatType),
    );

    assert_eq!(Sort::of(&mut kernel, &pi), Ok(Sort::Type(Level::zero())));
}

/// A record of nothing but propositions is a proposition — but the *empty* record is unit, not a proposition. It is what an effect returns, so calling it a proposition would erase a value the program still needs.
#[test]
fn a_record_of_propositions_is_a_proposition_but_the_empty_one_is_unit() {
    let mut kernel = kernel();
    let proposition = declare(&mut kernel, "P", Term::prop());

    let all_props = Term::tuple_type([
        (binder(0, "a"), proposition.clone()),
        (binder(1, "b"), proposition),
    ]);
    assert_eq!(Sort::of(&mut kernel, &all_props), Ok(Sort::Prop));

    let unit = Term::tuple_type(Vec::<(Free, Term)>::new());
    assert_eq!(Sort::of(&mut kernel, &unit), Ok(Sort::Type(Level::zero())));
}

/// One relevant field is enough to make the whole record relevant: its inhabitants are distinguishable, so irrelevance must not apply.
#[test]
fn one_relevant_field_makes_a_record_relevant() {
    let mut kernel = kernel();
    let proposition = declare(&mut kernel, "P", Term::prop());

    let mixed = Term::tuple_type([
        (binder(0, "a"), proposition),
        (binder(1, "b"), Term::intrinsic(Intrinsic::NatType)),
    ]);

    assert_eq!(Sort::of(&mut kernel, &mixed), Ok(Sort::Type(Level::zero())));
}

/// A list *of* proofs is not a proposition: it has a length, so two lists are distinguishable even when their elements are not.
#[test]
fn a_list_of_proofs_is_not_a_proposition() {
    let mut kernel = kernel();
    let proposition = declare(&mut kernel, "P", Term::prop());

    let list = Term::intrinsic(Intrinsic::ListType(proposition));

    assert_eq!(Sort::of(&mut kernel, &list), Ok(Sort::Type(Level::zero())));
}

/// The type of a hypothesis is read off the binder it was opened at, which is how a `Prop`-typed variable is recognized as a proof.
/// A recursive group's claim about its own member is not what decides that member's sort.
///
/// [`Sort::of`] reduces before it classifies, and forcing unfolds a projection to the member's body — so the sort reported is the body's, read honestly, rather than the `member_type` the group asserts. That matters because the projection arm answers through [`synth_neutral`](super::synth_neutral), which is a *lookup*: it reads the group's claim and checks nothing, and it has to stay that way, since certifying there would re-enter the very group whose member types are being sorted.
///
/// So the group below claims its member is a proposition while its body is a `Type`-sorted family, and the honest answer is the body's. Reduction is the whole defense at this arm, which is why it is pinned here rather than left to be re-derived from the reduction rules.
#[test]
fn a_groups_claim_about_its_member_does_not_decide_that_members_sort() {
    let mut kernel = kernel();
    let data = declare(&mut kernel, "Data", Term::type_ground());
    let member = Free::local(900, Some("T"));

    let group = RecGroup::new(vec![RecMemberScopes {
        // The claim: this member is a proposition.
        type_: Scope::close(Many(1), &[&member], Term::prop()),
        // The body: a family the registry puts at `Type 0`.
        body: Scope::close(Many(1), &[&member], data.clone()),
    }]);

    assert_eq!(
        Sort::of(&mut kernel, &Term::rec_proj(group, 0)),
        Ok(Sort::Type(Level::zero())),
        "the group's `Prop` claim was trusted over the sort its member actually reduces to",
    );
}

#[test]
fn a_hypothesis_takes_the_sort_of_the_type_it_was_opened_at() {
    let mut kernel = kernel();
    let hypothesis = Free::local(0, Some("h"));

    // `h : Prop`: the hypothesis names a proposition, so its sort is the universe it was opened at.
    kernel.assume(&hypothesis, &Term::prop());
    let sort = Sort::of(&mut kernel, &Term::free_var(&hypothesis));

    assert_eq!(sort, Ok(Sort::Prop));
}

/// Refusing beats guessing: an unregistered nominal type has no sort the kernel can determine, and inventing one is the unsound direction.
#[test]
fn an_unregistered_nominal_type_is_refused_rather_than_guessed() {
    let mut kernel = kernel();
    let name = nominal("Missing");
    let type_ = Term::induct_type(name.clone(), Vec::<Term>::new(), Vec::<Term>::new());

    assert_eq!(
        Sort::of(&mut kernel, &type_),
        Err(KernelError::Undeclared(name)),
    );
}

/// An intrinsic *value* is not a type, so nothing classifies it — again a refusal rather than a default.
#[test]
fn a_value_in_type_position_is_refused() {
    let mut kernel = kernel();

    assert!(matches!(
        Sort::of(&mut kernel, &Term::intrinsic(Intrinsic::Bool(true))),
        Err(KernelError::Unclassified(_)),
    ));
}

fn binder(index: u32, hint: &str) -> Free {
    Free::local(index, Some(hint))
}
