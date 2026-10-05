use {
    crate::{Error, Kernel, Sort},
    curios_analysis::{Erased, test_support::SYNTAX},
    curios_core::{
        Free, Global, InductDecl, Intrinsic, Level, Many, Nat, RecGroup, RecMemberScopes, Scope,
        Telescope, Term, UniverseContext, UniverseParam,
    },
    curios_utilities::Qualifier,
};

fn kernel() -> Kernel {
    Kernel::new(100_000, SYNTAX)
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
            rep_public: true,
            polarities: Vec::new(),
            variances: Vec::new(),
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

/// The type of a hypothesis is read off the binder it was opened at, which is how a `Prop`-typed variable is recognized as a proof.
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
    let type_ = Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new());

    assert_eq!(Sort::of(&mut kernel, &type_), Err(Error::Undeclared(name)),);
}

/// An intrinsic *value* is not a type, so nothing classifies it — again a refusal rather than a default.
#[test]
fn a_value_in_type_position_is_refused() {
    let mut kernel = kernel();

    assert!(matches!(
        Sort::of(&mut kernel, &Term::intrinsic(Intrinsic::Bool(true))),
        Err(Error::Unclassified(_)),
    ));
}

fn binder(index: u32, hint: &str) -> Free {
    Free::local(index, Some(hint))
}

// === The `Instance` arm, which reads its levels ==========================
//
// `Sort::of` classifies a universe instance as the neutral it is, at the levels the occurrence states — the clause `documentation/design/soundness/formation/universe-instances-and-constraints.md` holds over each head. `step_instance` leaves an `Instance` stuck only over a `Var` whose `value_at` is `None` — a local, or a global declared without a body — while every defined scheme instantiates its body at the instance before the arm can see it, and a rec-projection head steps to an instantiated projection. The three fixtures below hold one leg each.

/// The one production-reachable head: a local. A local is monomorphic — it was opened at one type, so there is no scheme to instantiate — and its sort is its binder's, whatever levels the wrapper states and however many. The width-2 vector is deliberate: even an instance no typing rule would admit cannot move the lookup, because a local head is answered from its binder before any instance is checked.
#[test]
fn a_universe_instance_over_a_local_head_takes_the_locals_sort() {
    let mut kernel = kernel();
    let one = Level::zero().succ().expect("level zero succeeds");

    let hypothesis = Free::local(0, Some("h"));
    kernel.assume(&hypothesis, &Term::prop());
    let data = Free::local(1, Some("d"));
    kernel.assume(&data, &Term::type_ground());

    for levels in [vec![Level::zero()], vec![one.clone(), Level::zero()]] {
        assert_eq!(
            Sort::of(&mut kernel, &Term::instance_of(&hypothesis, levels.clone()),),
            Ok(Sort::Prop),
        );
        assert_eq!(
            Sort::of(&mut kernel, &Term::instance_of(&data, levels),),
            Ok(Sort::Type(Level::zero())),
        );
    }
}

/// A defined scheme never reaches the arm: `step_instance` instantiates its body at the stated instance first, so the sort lands at the *instance's* level — the second clause of the entry's Assumes, probed at the sort. `A<u> : Type (u + 1) = Type u` at `u := 1` classifies as `Type 2`, with no scheme parameter surviving to be captured.
#[test]
fn a_universe_instance_over_a_defined_scheme_classifies_at_the_instances_level() {
    let mut kernel = kernel();
    let scheme = Free::from(&nominal("A"));
    let parameter = Level::param(UniverseParam(0));

    kernel.define(
        &scheme,
        &Term::type_at(parameter.checked_add(1).expect("level admits the offset")),
        &Term::type_at(parameter),
        &UniverseContext {
            parameter_count: 1,
            constraints: Vec::new(),
        },
    );

    let one = Level::zero().succ().expect("level zero succeeds");
    let two = one.clone().succ().expect("level one succeeds");

    assert_eq!(
        Sort::of(&mut kernel, &Term::instance_of(&scheme, vec![one]),),
        Ok(Sort::Type(two)),
    );
}

/// The one route that brings a scheme head to the arm — a polymorphic global declared without a body, a permanent neutral — classifies at the instance's level, as [`synth_neutral`](super::synth_neutral) reads the same head inside a spine: `A<u> : Type (u + 1)` is `Type 1` at `u := 0` and `Type 2` at `u := 1`. A classification that dropped the levels would answer the scheme's own sort with its parameter still in it, one answer for both instances. Nothing on the compile path declares a bodiless global — the module walk `define`s every item with its real body, and `Globals::of` records bodies for every `Let` and `Rec` — so only [`Kernel::declare`] builds this state.
#[test]
fn a_universe_instance_over_a_bodiless_scheme_classifies_at_the_instances_level() {
    let mut kernel = kernel();
    let scheme = Free::from(&nominal("A"));
    let parameter = Level::param(UniverseParam(0));

    kernel.declare(
        &scheme,
        &Term::type_at(parameter.checked_add(1).expect("level admits the offset")),
        &UniverseContext {
            parameter_count: 1,
            constraints: Vec::new(),
        },
    );

    let one = Level::zero().succ().expect("level zero succeeds");
    let two = one.clone().succ().expect("level one succeeds");

    assert_eq!(
        Sort::of(
            &mut kernel,
            &Term::instance_of(&scheme, vec![Level::zero()]),
        ),
        Ok(Sort::Type(one.clone())),
    );
    assert_eq!(
        Sort::of(&mut kernel, &Term::instance_of(&scheme, vec![one])),
        Ok(Sort::Type(two)),
    );
}

/// A record of two fields at one type, `depth` deep: a graph of `depth + 1` nodes whose tree has `2^depth` fields.
fn doubled(base: Term, depth: usize) -> Term {
    (0..depth).fold(base, |type_, level| {
        let first = Free::local(2 * level as u32 + 100, None);
        let second = Free::local(2 * level as u32 + 101, None);

        Term::tuple_type([(first, type_.clone()), (second, type_)])
    })
}

/// A sort is remembered per type, so a record sixty levels deep whose two fields are one type each — a tree no walk per path finishes — is classified in the size of its graph, over a closed base and over a local alike. At a depth the uncached kernel affords, both give the same sort.
///
/// Mutation-checked: with no remembered sort answered, neither sixty-level row finishes.
#[test]
fn a_shared_type_is_classified_once_per_node() {
    let nat = Term::intrinsic(Intrinsic::NatType);
    assert_eq!(
        Sort::of(&mut kernel(), &doubled(nat.clone(), 60)),
        Ok(Sort::Type(Level::zero())),
    );

    let hypothesis = binder(0, "A");
    let mut over_a_local = kernel();
    over_a_local.assume(&hypothesis, &Term::type_ground());
    assert_eq!(
        Sort::of(&mut over_a_local, &doubled(Term::free_var(&hypothesis), 60)),
        Ok(Sort::Type(Level::zero())),
    );

    let mut uncached = Kernel::uncached(100_000, SYNTAX);
    assert_eq!(
        Sort::of(&mut kernel(), &doubled(nat.clone(), 8)),
        Sort::of(&mut uncached, &doubled(nat, 8)),
    );
}

/// A remembered sort lives as long as the equations it was taken under: a type whose universe is a scrutinee is a proposition in the arm that makes it `Prop`, data in the arm that makes it `Type`, and unclassified outside both — and the uncached kernel agrees on all three.
///
/// Mutation-checked: leaving the scoped sorts standing where an equation moves answers the second arm `Prop`.
#[test]
fn a_remembered_sort_does_not_outlive_the_equations_it_was_taken_under() {
    let sequence = |kernel: &mut Kernel| {
        let universe = binder(0, "u");
        let hypothesis = binder(1, "A");
        kernel.assume(
            &universe,
            &Term::type_at(Level::zero().succ().expect("one")),
        );
        kernel.assume(&hypothesis, &Term::free_var(&universe));
        let type_ = Term::free_var(&hypothesis);

        let under = |kernel: &mut Kernel, value: Term| {
            kernel.scoped(|kernel| {
                kernel
                    .refine(Term::free_var(&universe), value)
                    .expect("the equation records");
                Sort::of(kernel, &type_)
            })
        };

        [
            under(kernel, Term::prop()),
            under(kernel, Term::type_ground()),
            Sort::of(kernel, &type_),
        ]
    };

    let cached = sequence(&mut kernel());

    assert_eq!(cached[0], Ok(Sort::Prop));
    assert_eq!(cached[1], Ok(Sort::Type(Level::zero())));
    assert!(matches!(cached[2], Err(Error::NotASort(_))));
    assert_eq!(cached, sequence(&mut Kernel::uncached(100_000, SYNTAX)));
}

/// What a position is recorded as lives as its type's sort does: a term at a type one arm's equation makes a proposition is a proof in that arm and nothing the obligations constrain in the arm that makes its type data, whichever is checked first.
///
/// Mutation-checked: leaving the remembered halves standing where an equation moves records the second arm's position as a proof too.
#[test]
fn a_position_is_classified_under_its_own_arm() {
    let mut kernel = kernel();
    let universe = binder(0, "u");
    let hypothesis = binder(1, "A");
    let inhabitant = binder(2, "a");
    kernel.assume(
        &universe,
        &Term::type_at(Level::zero().succ().expect("one")),
    );
    kernel.assume(&hypothesis, &Term::free_var(&universe));
    kernel.assume(&inhabitant, &Term::free_var(&hypothesis));

    let recorded = [Term::prop(), Term::type_ground()].map(|value| {
        kernel.scoped(|kernel| {
            kernel
                .refine(Term::free_var(&universe), value)
                .expect("the equation records");
            kernel.record_checked(&Term::free_var(&inhabitant), &Term::free_var(&hypothesis))
        })
    });
    let (positions, failure) = kernel.take_checked();

    assert_eq!(recorded, [Some(0), None]);
    assert_eq!(
        positions
            .iter()
            .map(|position| position.erased)
            .collect::<Vec<_>>(),
        [Erased::Proof],
    );
    assert!(failure.is_none());
}

/// Nor does it outlive the type a local stands at: an arm re-assumes a local at its specialized type, and a neutral's sort is read off its binder, so the sort remembered before the arm does not answer inside it, nor the arm's after it.
///
/// Mutation-checked: with `Kernel::assume` leaving the tables alone where it re-types a binder, the shadowed read answers `Type`.
#[test]
fn a_remembered_sort_does_not_outlive_the_type_its_local_stands_at() {
    let mut kernel = kernel();
    let hypothesis = binder(0, "h");
    let type_ = Term::free_var(&hypothesis);

    kernel.assume(&hypothesis, &Term::type_ground());
    assert_eq!(Sort::of(&mut kernel, &type_), Ok(Sort::Type(Level::zero())));

    let shadowed = kernel.scoped(|kernel| {
        kernel.assume(&hypothesis, &Term::prop());
        Sort::of(kernel, &type_)
    });

    assert_eq!(shadowed, Ok(Sort::Prop));
    assert_eq!(Sort::of(&mut kernel, &type_), Ok(Sort::Type(Level::zero())));
}

/// Nor its binder. A name handed in from outside can be assumed again once its first binder is closed, at another type, and a sort read under the first does not answer for the second: the table files each answer with the binders it was read under, and asks whether they stand.
///
/// Mutation-checked: answering a scoped sort without asking whether its binders stand classifies the second `h` at `Type`.
#[test]
fn a_remembered_sort_does_not_outlive_its_binder() {
    let mut kernel = kernel();
    let hypothesis = binder(0, "h");
    let type_ = Term::free_var(&hypothesis);

    let first = kernel.scoped(|kernel| {
        kernel.assume(&hypothesis, &Term::type_ground());
        Sort::of(kernel, &type_)
    });
    let second = kernel.scoped(|kernel| {
        kernel.assume(&hypothesis, &Term::prop());
        Sort::of(kernel, &type_)
    });

    assert_eq!(first, Ok(Sort::Type(Level::zero())));
    assert_eq!(second, Ok(Sort::Prop));
}

/// A local-free type an arm's equation makes a proposition, beside that equation's scrutinee. `g: (P) -> Nat` has no body, the type is `(x: P) -> match g(x) < 10 | true => Q | false => Nat`, which names no local, and the scrutinee is `g(p) < 10` over a proof `p` in scope. Under its equation `g(x) < 10` is that scrutinee to the kernel's conversion, the two calls differing in a proof, so the type is a proposition inside the arm and data outside it.
fn family_over_a_proof(kernel: &mut Kernel) -> (Term, Term) {
    let proposition = declare(kernel, "P", Term::prop());
    let other = declare(kernel, "Q", Term::prop());
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let g = Free::from(&nominal("g"));
    kernel.declare(
        &g,
        &Term::func_type([(binder(90, "x"), proposition.clone())], nat_type.clone()),
        &UniverseContext::default(),
    );
    let p = binder(1, "p");
    kernel.assume(&p, &proposition);
    let guard = |proof: &Free| {
        Term::intrinsic(Intrinsic::nat_lt(
            Term::apply(Term::free_var(&g), [Term::free_var(proof)]),
            Term::intrinsic(Intrinsic::Nat(Nat::new(10usize))),
        ))
    };
    let x = binder(91, "x");
    let family = Term::func_type(
        [(x, proposition)],
        Term::bool_match(guard(&x), None, Term::type_ground(), nat_type, other),
    );
    assert!(!family.has_local_free());

    (family, guard(&p))
}

/// Nor does a local-free type's: its sort is the declaration's only where no case equation is in force, since an equation answers what the kernel's conversion holds equal to its scrutinee, and a term naming only a proof the classification opened can be that ([`family_over_a_proof`]). Both orders are run, and the uncached kernel agrees.
///
/// Mutation-checked both ways: with such a sort read as the declaration's under an equation, the answer from before the arm is handed back inside it, and with it filed there, the arm's answer is handed back after the arm.
#[test]
fn a_local_free_types_sort_does_not_outlive_the_equations_it_was_read_under() {
    let orders = |inside_first: bool, mut kernel: Kernel| {
        let (family, scrutinee) = family_over_a_proof(&mut kernel);
        let is_prop = |kernel: &mut Kernel| Sort::of(kernel, &family).map(|sort| sort.is_prop());

        let before = (!inside_first).then(|| is_prop(&mut kernel));
        let inside = kernel.scoped(|kernel| {
            kernel
                .refine(scrutinee, Term::intrinsic(Intrinsic::Bool(true)))
                .expect("the equation records");
            is_prop(kernel)
        });
        let after = is_prop(&mut kernel);

        (before, inside, after)
    };

    for inside_first in [false, true] {
        let cached = orders(inside_first, kernel());
        assert_eq!(
            cached,
            ((!inside_first).then_some(Ok(false)), Ok(true), Ok(false))
        );
        assert_eq!(
            cached,
            orders(inside_first, Kernel::uncached(100_000, SYNTAX))
        );
    }
}

/// A position at such a type is classified under its own arm as well: a term at it is a proof inside the arm and nothing the obligations constrain outside it, whichever is recorded first.
///
/// Mutation-checked both ways: with the half read as the declaration's under an equation, the position inside the arm goes unrecorded in the first order, and with it filed there, the term is recorded as a proof after the arm in the second.
#[test]
fn a_position_at_a_local_free_type_is_classified_under_its_own_arm() {
    for inside_first in [false, true] {
        let mut kernel = kernel();
        let (family, scrutinee) = family_over_a_proof(&mut kernel);
        let inhabitant = binder(2, "f");
        kernel.assume(&inhabitant, &family);
        let record =
            |kernel: &mut Kernel| kernel.record_checked(&Term::free_var(&inhabitant), &family);

        let before = (!inside_first).then(|| record(&mut kernel));
        let inside = kernel.scoped(|kernel| {
            kernel
                .refine(scrutinee, Term::intrinsic(Intrinsic::Bool(true)))
                .expect("the equation records");
            record(kernel)
        });
        let after = record(&mut kernel);
        let (positions, failure) = kernel.take_checked();

        assert_eq!(
            (before, inside, after),
            ((!inside_first).then_some(None), Some(0), None)
        );
        assert_eq!(
            positions
                .iter()
                .map(|position| position.erased)
                .collect::<Vec<_>>(),
            [Erased::Proof],
        );
        assert!(failure.is_none());
    }
}

/// A sort is remembered by the reduction that read it: what plain reduction classified is not answered where a judgment asks, nor a judgment's where plain reduction does. A sort is read through reducts, and a judgment's reduction may ask the kernel's conversion where plain reduction asks nothing, so each files its own.
///
/// Mutation-checked both ways: with the table read as a judgment's whichever reduction asks, plain reduction finds nothing of what it filed, and with the answer filed there whichever reduction read it, a judgment is handed plain reduction's.
#[test]
fn a_sort_is_remembered_by_the_reduction_that_read_it() {
    let mut kernel = kernel();
    let read = declare(&mut kernel, "Read", Term::type_ground());
    let judged = declare(&mut kernel, "Judged", Term::prop());

    kernel
        .plainly(|kernel| Sort::of(kernel, &read))
        .expect("classifies");
    assert!(kernel.sort_hit(&read).is_none());
    assert!(kernel.plainly(|kernel| kernel.sort_hit(&read)).is_some());

    Sort::of(&mut kernel, &judged).expect("classifies");
    assert!(kernel.plainly(|kernel| kernel.sort_hit(&judged)).is_none());
    assert!(kernel.sort_hit(&judged).is_some());
}
