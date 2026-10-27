//! Flex-rigid solving: a candidate committed as its check rebuilt it, the occurs and scope checks, and a solve rolled back to its mark.

use {
    super::test_support::*,
    crate::*,
    curios_core::{
        CalleeId, DefinitionKind, Free, Intrinsic, Level, MetavarId, MetavarOrigin, Subterm, Term,
        WitnessOrigin,
    },
};

// === Metavariables / unification ===========================================

#[test]
fn flex_rigid_commits_solution() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());

    // ?0 ≟ Nat  (at type Type)
    let nat = Term::intrinsic(Intrinsic::NatType);
    assert_eq!(conv(&mut context, &Term::hole(0), &nat), Ok(true));
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&nat));
}

#[test]
fn solve_is_symmetric() {
    let mut context = context();
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());

    let nat = Term::intrinsic(Intrinsic::NatType);
    // rigid on the left, flex on the right
    assert_eq!(conv(&mut context, &nat, &Term::hole(0)), Ok(true));
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&nat));
}

/// Abstracting a subject out of a candidate costs the candidate's graph, not its tree: each level sums the one below with itself, sixty over, and the subject at the base is abstracted once. The result stays a graph — both operands of its root are one node.
#[test]
fn abstraction_walks_a_shared_candidate_once_per_node() {
    let mut context = context();
    let f = context.fresh(Some("f"));
    let birth = context.fresh(Some("b"));
    let subject = Term::apply(Term::free_var(&f), [nat(1)]);

    let mut candidate = subject.clone();
    for _ in 0..60 {
        candidate = Term::intrinsic(Intrinsic::nat_add(candidate.clone(), candidate));
    }
    let abstracted = super::abstract_occurrences(&candidate, &[(subject, birth)]);

    assert!(!abstracted.mentions_free(&f));
    assert!(abstracted.mentions_free(&birth));
    let Subterm::Intrinsic(Intrinsic::NatAdd(left, right)) = &*abstracted else {
        panic!("the abstraction changed the root: {abstracted}");
    };
    assert!(std::ptr::eq::<Subterm>(&**left, &**right));
}

#[test]
fn occurs_check_rejects_cyclic_solution() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());

    // ?0 ≟ (x : ?0) -> Nat  — the candidate mentions ?0 itself.
    let cyclic = Term::func_type([(x, Term::hole(0))], Term::intrinsic(Intrinsic::NatType));
    assert_eq!(conv(&mut context, &Term::hole(0), &cyclic), Ok(false));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

#[test]
fn scope_check_rejects_out_of_context_variable() {
    let mut context = context();
    let x_binder = context.fresh(Some("x"));
    // Birth with empty Γ: no variable is in scope for ?0.
    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());

    // ?0 ≟ x  — `x` is not available to ?0.
    let x = Term::free_var(&x_binder);
    assert_eq!(conv(&mut context, &Term::hole(0), &x), Ok(false));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);
}

// A `let`'s local is no variable out of a metavariable's context: it stands for its value, so a candidate that names it is that value's. `?0` is born outside the frame that binds `n`, and `f(n)` commits as `f(2)`; refused for naming `n`, a call would be admitted by which of its arguments is read first.
#[test]
fn a_candidate_names_a_lets_local_by_its_value() {
    let mut context = context();
    let f = context.fresh(Some("f"));
    let x = context.fresh(Some("x"));
    let n = context.fresh(Some("n"));
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    context.assume(
        &f,
        &Term::func_type([(x, nat_type.clone())], nat_type.clone()),
    );
    context.birth_metavar(MetavarId(0), Vec::new(), nat_type.clone());

    context.with_frame(|context| {
        context.define_assuming(&n, &nat_type, &nat(2), None);
        let candidate = Term::apply(Term::free_var(&f), [Term::free_var(&n)]);
        assert_eq!(conv(context, &Term::hole(0), &candidate), Ok(true));
    });

    assert_eq!(
        context.metavar_solution(MetavarId(0)),
        Some(&Term::apply(Term::free_var(&f), [nat(2)]))
    );
}

#[test]
fn scope_check_allows_in_context_variable() {
    let mut context = context();
    let x_binder = context.fresh(Some("x"));
    // Γ = (x : Type); result is Type, and the candidate `x` is in scope.
    context.assume(&x_binder, &Term::type_ground());
    context.birth_metavar(
        MetavarId(0),
        vec![(x_binder, Term::type_ground())],
        Term::type_ground(),
    );

    let x = Term::free_var(&x_binder);
    let occurrence = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![x.clone()]);
    assert_eq!(conv(&mut context, &occurrence, &x), Ok(true));
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&x));
}

// What is committed is the term the check rebuilt. A lambda annotated below the domain it is solved at checks, its annotation against that domain by subsumption, and is committed at the domain, which is the annotation the kernel requires of it; committed as it came, it is a solution the elaborator accepts and the kernel refuses. The same holds of a lambda beneath a tuple, which the check reaches through the tuple's field type.
#[test]
fn a_candidate_is_committed_as_its_check_rebuilt_it() {
    let mut context = context();
    let carrier = context.fresh(Some("T"));
    let field = context.fresh(Some("f"));
    let sort = |level: u32| Term::type_at(Level::constant(level));
    let identity = |level: u32| Term::func([(carrier, sort(level))], Term::free_var(&carrier));
    let family = Term::func_type([(carrier, sort(1))], sort(1));
    let annotation = |term: &Term| {
        let Subterm::Func(func) = &**term else {
            panic!("not a lambda: {term}");
        };
        func.telescope
            .cursor()
            .entry()
            .expect("a lambda binds a parameter")
            .1
    };

    // ?0 : (Type 1) -> Type 1, against the identity annotated at Type 0.
    context.birth_metavar(MetavarId(0), Vec::new(), family.clone());
    assert_eq!(conv(&mut context, &Term::hole(0), &identity(0)), Ok(true));
    let solution = context
        .metavar_solution(MetavarId(0))
        .cloned()
        .expect("the candidate checks");
    assert_eq!(annotation(&solution), sort(1));

    // ?1 : {f: (Type 1) -> Type 1}, against a tuple holding that identity.
    context.birth_metavar(
        MetavarId(1),
        Vec::new(),
        Term::tuple_type([(field, family)]),
    );
    assert_eq!(
        conv(&mut context, &Term::hole(1), &Term::tuple([identity(0)])),
        Ok(true)
    );
    let solution = context
        .metavar_solution(MetavarId(1))
        .cloned()
        .expect("the candidate checks");
    let Subterm::Tuple(tuple) = &*solution else {
        panic!("not a tuple: {solution}");
    };
    assert_eq!(annotation(&tuple.fields[0]), sort(1));
}

/// A candidate re-validation cannot judge yet is postponed, not refused. `?1 : ?0` against a lambda has no function type to be checked at while `?0` is unsolved — the check would park, and inside the oracle may not — and the candidate is committed once `?0` is solved. The same holds one level down: the lambda inside a pair at `{?0, Nat}`, where the frozen type is a record and only its field waits. The control is a candidate at a type that contradicts it, which nothing leaves undecided and which is refused outright.
///
/// Mutation-checked: with a declined park not recorded the two waiting goals are mismatches, and with every failed check read as undecided the control is blocked.
#[test]
fn a_candidate_waits_for_the_type_it_is_checked_at() {
    let mut context = context();
    let (x, a, b) = (
        context.fresh(Some("x")),
        context.fresh(Some("a")),
        context.fresh(Some("b")),
    );
    let lambda = Term::func([(x, nat_type())], Term::free_var(&x));
    let pair = Term::tuple([lambda.clone(), nat(1)]);
    let pair_type = Term::tuple_type([(a, Term::hole(0)), (b, nat_type())]);

    context.birth_metavar(MetavarId(0), Vec::new(), Term::type_ground());
    context.birth_metavar(MetavarId(1), Vec::new(), Term::hole(0));
    context.birth_metavar(MetavarId(2), Vec::new(), pair_type.clone());
    context.birth_metavar(MetavarId(3), Vec::new(), nat_type());

    let goals = |context: &mut Context| {
        [
            (Term::hole(0), Term::hole(1), lambda.clone()),
            (pair_type.clone(), Term::hole(2), pair.clone()),
            (nat_type(), Term::hole(3), lambda.clone()),
        ]
        .map(|(type_, slot, candidate)| {
            match convert_outcome(context, &type_, &slot, &candidate) {
                Ok(Outcome::Converts) => "converts",
                Ok(Outcome::Blocked(_)) => "blocked",
                Ok(Outcome::Mismatch(_)) => "mismatch",
                Err(_) => "exhausted",
            }
        })
    };

    assert_eq!(goals(&mut context), ["blocked", "blocked", "mismatch"]);
    assert_eq!(context.metavar_solution(MetavarId(1)), None);
    assert_eq!(context.metavar_solution(MetavarId(2)), None);

    context.solve_metavar(MetavarId(0), Term::func_type([(x, nat_type())], nat_type()));

    assert_eq!(goals(&mut context), ["converts", "converts", "mismatch"]);
    assert_eq!(context.metavar_solution(MetavarId(1)), Some(&lambda));
    assert_eq!(context.metavar_solution(MetavarId(2)), Some(&pair));
}

#[test]
fn conversion_cannot_solve_a_protected_recursive_slot() {
    let mut context = context();
    let member = context.fresh(Some("member"));
    let (id, slot) = context.fresh_rec_slot(&member, Term::type_ground());
    let nat_type = Term::intrinsic(Intrinsic::NatType);

    assert!(matches!(
        convert_outcome(&mut context, &Term::type_ground(), &slot, &nat_type),
        Ok(Outcome::Blocked(_))
    ));
    assert!(context.metavar_solution(id).is_none());

    context.fill_rec_slot(id, nat_type.clone());
    assert_eq!(reduce(&mut context, slot), Ok(nat_type));
}

#[test]
fn rollback_solutions_unwinds_to_the_mark() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    context.birth_metavar(MetavarId(0), vec![(a, nat_type())], nat_type());
    context.birth_metavar(MetavarId(1), vec![(a, nat_type())], nat_type());

    context.solve_metavar(MetavarId(0), nat(1));
    let mark = context.solution_mark();
    context.solve_metavar(MetavarId(1), nat(2));

    context.rollback_solutions(mark);

    // The solution past the mark is unwound; the one before it survives. This is the bracket `solve` wraps around re-validation, so a rejected candidate's nested solves leave no fingerprints.
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&nat(1)));
    assert_eq!(context.metavar_solution(MetavarId(1)), None);
}

#[test]
fn stuck_intrinsic_on_a_metavar_parks_instead_of_mismatching() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    context.birth_metavar(MetavarId(0), vec![(a, nat_type())], nat_type());
    let m = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&a)]);
    let stuck: Term = Subterm::Intrinsic(Intrinsic::NatSub(m.clone(), nat(1))).into();

    // `?0 - 1 ≈ 0` is undecided, not unequal: solving `?0` may fold the subtraction. (`NatAdd` escapes via successor peeling; the other operators rely on this parking.)
    let outcome = convert_outcome(&mut context, &Term::type_ground(), &stuck, &nat(0));
    assert!(matches!(outcome, Ok(Outcome::Blocked(_))));
    assert_eq!(context.metavar_solution(MetavarId(0)), None);

    // Within one run, a sibling goal pins `?0 := 1`; the parked subtraction is retried, folds to `0`, and converts.
    let this = Term::tuple([stuck, m]);
    let that = Term::tuple([nat(0), nat(1)]);
    assert_eq!(conv(&mut context, &this, &that), Ok(true));
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&nat(1)));
}

#[test]
fn rigid_head_mismatch_with_a_metavar_inside_still_fails_fast() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    context.birth_metavar(MetavarId(0), vec![(a, nat_type())], nat_type());
    let m = Term::metavar_birthed(0, MetavarOrigin::Hole, vec![Term::free_var(&a)]);

    // An inductive type against `Nat` is provably unequal whatever `?0` becomes — the heads are rigid — so the mismatch stays hard (and is reported at the use site, not deferred to the drain).
    let induct_decl = Term::induct_type(nominal("Vec"), [m], Vec::<Term>::new());
    let outcome = convert_outcome(
        &mut context,
        &Term::type_ground(),
        &induct_decl,
        &nat_type(),
    );
    assert!(matches!(outcome, Ok(Outcome::Mismatch(_))));
}

// A slot met against a dictionary's name is solved to the name, whatever minted it: the rigid side as written passes every check, and committed reduced the slot would spell the structure of methods out wherever it stands.
#[test]
fn a_slot_met_against_a_dictionarys_name_is_solved_to_the_name() {
    let dictionary = Free::from(&nominal("dictionary"));
    let methods = || Term::tuple([nat(1), nat(2)]);
    let solved = |origin: MetavarOrigin| {
        let mut context = context();
        let type_ = Term::tuple_type([
            (context.fresh(Some("first")), nat_type()),
            (context.fresh(Some("second")), nat_type()),
        ]);
        context.define_assuming(
            &dictionary,
            &type_,
            &methods(),
            Some(&DefinitionKind::Authored),
        );
        context.birth_metavar(MetavarId(0), Vec::new(), type_.clone());
        let slot = Term::metavar_birthed(0, origin, Vec::new());
        assert_eq!(
            convert(&mut context, &type_, &slot, &Term::free_var(&dictionary)),
            Ok(true)
        );

        context.metavar_solution(MetavarId(0)).cloned()
    };
    let premise = MetavarOrigin::Witness(WitnessOrigin {
        func: CalleeId::Function(Free::from(&nominal("holds"))),
        role: Some("key".into()),
    });

    assert_eq!(solved(premise), Some(Term::free_var(&dictionary)));
    assert_eq!(
        solved(MetavarOrigin::Hole),
        Some(Term::free_var(&dictionary))
    );
}

// A metavariable met against another's solution is solved to what that one spells: `?0` holds the name `flat`, and `?1`, met against `?0`, holds the name too, where the reduct of either is the definition's lambda.
#[test]
fn a_solution_met_through_a_solved_metavariable_keeps_the_name_it_spells() {
    let mut context = context();
    let flat = Free::from(&nominal("flat"));
    let argument = context.fresh(Some("n"));
    let type_ = Term::func_type([(argument, nat_type())], nat_type());
    context.define_assuming(
        &flat,
        &type_,
        &Term::func([(argument, nat_type())], nat(0)),
        Some(&DefinitionKind::Authored),
    );
    context.birth_metavar(MetavarId(0), Vec::new(), type_.clone());
    context.birth_metavar(MetavarId(1), Vec::new(), type_.clone());

    let name = Term::free_var(&flat);
    assert_eq!(
        convert(&mut context, &type_, &Term::hole(0), &name),
        Ok(true)
    );
    assert_eq!(
        convert(&mut context, &type_, &Term::hole(1), &Term::hole(0)),
        Ok(true)
    );
    assert_eq!(context.metavar_solution(MetavarId(0)), Some(&name));
    assert_eq!(context.metavar_solution(MetavarId(1)), Some(&name));
}
