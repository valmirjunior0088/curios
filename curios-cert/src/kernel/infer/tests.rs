use {
    crate::{Kernel, KernelError, check, infer},
    curios_base::{Plicity, Qualifier, RootId},
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam, Level, Prim, Telescope, Term, UniverseContext,
    },
};

fn kernel() -> Kernel {
    let mut kernel = Kernel::new(100_000);
    kernel.set_local_floor(1_000);
    kernel
}

fn binder(index: u32, hint: &str) -> Free {
    Free::local(index, Some(hint))
}

fn nat(n: usize) -> Term {
    Term::prim(Prim::Nat(curios_core::Nat::new(n)))
}

fn nat_type() -> Term {
    Term::prim(Prim::NatType)
}

fn bool_type() -> Term {
    Term::prim(Prim::BoolType)
}

fn one() -> Level {
    Level::zero().succ().expect("level zero has a successor")
}

#[test]
fn a_universe_is_one_level_above_itself() {
    let mut kernel = kernel();

    assert_eq!(
        infer(&mut kernel, &Term::type_ground()),
        Ok(Term::type_at(one())),
    );
    assert_eq!(infer(&mut kernel, &Term::prop()), Ok(Term::type_ground()));
}

#[test]
fn a_literal_has_its_carriers_type() {
    let mut kernel = kernel();

    assert_eq!(infer(&mut kernel, &nat(7)), Ok(nat_type()));
    assert_eq!(
        infer(&mut kernel, &Term::prim(Prim::Bool(true))),
        Ok(bool_type()),
    );
}

#[test]
fn a_variable_has_the_type_it_was_bound_at() {
    let mut kernel = kernel();
    let x = binder(0, "x");
    kernel.assume(&x, &nat_type());

    assert_eq!(infer(&mut kernel, &Term::free_var(&x)), Ok(nat_type()));
}

/// A finished term has no free names. Refusing rather than treating one as a
/// neutral is what makes that a checked statement.
#[test]
fn an_unbound_variable_is_refused() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    assert_eq!(
        infer(&mut kernel, &Term::free_var(&x)),
        Err(KernelError::Unbound(x)),
    );
}

#[test]
fn a_lambda_has_the_function_type_over_its_telescope() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let identity = Term::func([(x.clone(), nat_type())], Term::free_var(&x));
    let arrow = Term::func_type([(x, nat_type())], nat_type());

    assert_eq!(infer(&mut kernel, &identity), Ok(arrow));
}

#[test]
fn an_application_substitutes_its_arguments_into_the_result() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let identity = Term::func([(x.clone(), nat_type())], Term::free_var(&x));

    assert_eq!(
        infer(&mut kernel, &Term::apply(identity, [nat(4)])),
        Ok(nat_type()),
    );
}

/// Dependency: applying a family to an argument puts *that argument* into the
/// result type, which is the whole point of a dependent function.
#[test]
fn a_dependent_result_mentions_the_argument_supplied() {
    let mut kernel = kernel();
    let a = binder(0, "A");
    let x = binder(1, "x");

    // `(A : Type, x : A) -> A` applied at `(Nat, 3)` results in `Nat`.
    let f = Term::func(
        [
            (a.clone(), Term::type_ground()),
            (x.clone(), Term::free_var(&a)),
        ],
        Term::free_var(&x),
    );

    assert_eq!(
        infer(&mut kernel, &Term::apply(f, [nat_type(), nat(3)])),
        Ok(nat_type()),
    );
}

#[test]
fn an_argument_of_the_wrong_type_is_refused() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let f = Term::func([(x.clone(), nat_type())], Term::free_var(&x));
    let applied = Term::apply(f, [Term::prim(Prim::Bool(true))]);

    assert!(matches!(
        infer(&mut kernel, &applied),
        Err(KernelError::Mismatch { .. }),
    ));
}

#[test]
fn an_application_of_the_wrong_arity_is_refused() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let f = Term::func([(x.clone(), nat_type())], Term::free_var(&x));
    let applied = Term::apply(f, [nat(1), nat(2)]);

    assert_eq!(
        infer(&mut kernel, &applied),
        Err(KernelError::Arity {
            expected: 1,
            actual: 2
        }),
    );
}

#[test]
fn applying_a_non_function_is_refused() {
    let mut kernel = kernel();

    assert!(matches!(
        infer(&mut kernel, &Term::apply(nat(1), [nat(2)])),
        Err(KernelError::NotAFunction(_)),
    ));
}

#[test]
fn a_tuple_has_the_product_of_its_components_types() {
    let mut kernel = kernel();

    let pair = Term::tuple([nat(1), Term::prim(Prim::Bool(false))]);
    let type_ = infer(&mut kernel, &pair).expect("a closed pair has a type");

    assert_eq!(
        infer(&mut kernel, &Term::proj(pair.clone(), 0)),
        Ok(nat_type()),
    );
    assert_eq!(infer(&mut kernel, &Term::proj(pair, 1)), Ok(bool_type()));
    assert!(matches!(&*type_, curios_core::Subterm::TupleType(_)));
}

#[test]
fn projecting_from_a_non_tuple_is_refused() {
    let mut kernel = kernel();

    assert!(matches!(
        infer(&mut kernel, &Term::proj(nat(1), 0)),
        Err(KernelError::NotATuple(_)),
    ));
}

/// `let` is checked binding by binding and then substituted, so the tail's type
/// is computed with the values in place rather than with opaque names.
#[test]
fn a_let_checks_its_binding_and_substitutes_it() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let term = Term::let_(&x, nat_type(), nat(2), Term::free_var(&x));
    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));

    let wrong = Term::let_(&x, bool_type(), nat(2), Term::free_var(&x));
    assert!(matches!(
        infer(&mut kernel, &wrong),
        Err(KernelError::Mismatch { .. }),
    ));
}

/// A recursive group's members are assumed at their declared types while their
/// bodies are checked, which is what lets a member call itself.
#[test]
fn a_recursive_group_checks_its_bodies_against_its_declared_types() {
    let mut kernel = kernel();
    let countdown = binder(0, "countdown");
    let n = binder(1, "n");
    let motive = binder(2, "m");
    let pred = binder(3, "pred");
    let hypothesis = binder(4, "ih");

    let signature = Term::func_type([(n.clone(), nat_type())], nat_type());
    let body = Term::func(
        [(n.clone(), nat_type())],
        Term::nat_match(
            Term::free_var(&n),
            Some(&motive),
            nat_type(),
            nat(0),
            &pred,
            &hypothesis,
            Term::apply(Term::free_var(&countdown), [Term::free_var(&pred)]),
        ),
    );

    let term = Term::rec(
        [(countdown.clone(), signature.clone(), body)],
        Term::apply(Term::free_var(&countdown), [nat(3)]),
    );

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
}

/// A group whose body does not have the type it declares is refused — the
/// assumption is what the body must live up to, not a licence.
#[test]
fn a_recursive_body_that_misses_its_declared_type_is_refused() {
    let mut kernel = kernel();
    let f = binder(0, "f");
    let n = binder(1, "n");

    let signature = Term::func_type([(n.clone(), nat_type())], nat_type());
    let body = Term::func([(n, nat_type())], Term::prim(Prim::Bool(true)));

    let term = Term::rec(
        [(f.clone(), signature, body)],
        Term::apply(Term::free_var(&f), [nat(1)]),
    );

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(KernelError::Mismatch { .. }),
    ));
}

/// A constructor's type is its signature's terminal — the family at the
/// parameters and index targets this case aims at.
#[test]
fn a_constructor_has_the_type_its_signature_ends_in() {
    let mut kernel = kernel();
    let name = Global::Authored(Qualifier::from(["Wrapped"]));
    let payload = binder(0, "value");

    // `induct Wrapped | mk(value : Nat) end`
    let constructed = Term::induct_type(name.clone(), Vec::<Term>::new(), Vec::<Term>::new());
    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: UniverseContext::default(),
            params: Telescope::done(()),
            indices: Telescope::done(()),
            constructors: vec![(
                Atom::from("mk"),
                InductParam {
                    telescope: Telescope::build([(payload, nat_type())], constructed.clone()),
                    plicities: vec![Plicity::Explicit],
                },
            )],
            result_sort: Term::type_ground(),
            module: Qualifier::from(["Wrapped"]),
            root: RootId::Entry,
            rep_public: true,
            polarities: Vec::new(),
        },
    );

    let value = Term::variant(name, Vec::<Term>::new(), "mk", [nat(5)]);
    assert_eq!(infer(&mut kernel, &value), Ok(constructed));
}

/// A constructor's payload is checked against its declared field types.
#[test]
fn a_constructor_payload_of_the_wrong_type_is_refused() {
    let mut kernel = kernel();
    let name = Global::Authored(Qualifier::from(["Wrapped"]));
    let payload = binder(0, "value");
    let constructed = Term::induct_type(name.clone(), Vec::<Term>::new(), Vec::<Term>::new());

    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: UniverseContext::default(),
            params: Telescope::done(()),
            indices: Telescope::done(()),
            constructors: vec![(
                Atom::from("mk"),
                InductParam {
                    telescope: Telescope::build([(payload, nat_type())], constructed),
                    plicities: vec![Plicity::Explicit],
                },
            )],
            result_sort: Term::type_ground(),
            module: Qualifier::from(["Wrapped"]),
            root: RootId::Entry,
            rep_public: true,
            polarities: Vec::new(),
        },
    );

    let value = Term::variant(
        name,
        Vec::<Term>::new(),
        "mk",
        [Term::prim(Prim::Bool(true))],
    );
    assert!(matches!(
        infer(&mut kernel, &value),
        Err(KernelError::Mismatch { .. }),
    ));
}

/// An elimination's type is its motive at the scrutinee.
#[test]
fn an_elimination_has_the_type_its_motive_states() {
    let mut kernel = kernel();
    let motive = binder(0, "m");
    let pred = binder(1, "pred");
    let hypothesis = binder(2, "ih");

    let term = Term::nat_match(
        nat(3),
        Some(&motive),
        nat_type(),
        nat(0),
        &pred,
        &hypothesis,
        Term::free_var(&pred),
    );

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
}

/// Cumulativity: a small type stands where a larger universe is wanted, and
/// `Prop` stands wherever a `Type` is. The reverse does not hold.
#[test]
fn a_small_universe_is_admitted_where_a_larger_one_is_wanted() {
    let mut kernel = kernel();

    assert_eq!(
        check(&mut kernel, &Term::type_ground(), &Term::type_at(one())),
        Ok(()),
    );
    assert_eq!(
        check(&mut kernel, &nat_type(), &Term::type_ground()),
        Ok(())
    );
    assert!(matches!(
        check(&mut kernel, &Term::type_at(one()), &Term::type_ground()),
        Err(KernelError::Mismatch { .. }),
    ));
}

/// A primitive's operands are checked against the types its rule demands.
#[test]
fn a_primitive_operand_of_the_wrong_type_is_refused() {
    let mut kernel = kernel();

    let mixed = Term::prim(Prim::nat_add(Term::prim(Prim::Bool(true)), nat(1)));

    assert!(matches!(
        infer(&mut kernel, &mixed),
        Err(KernelError::Mismatch { .. }),
    ));
}

#[test]
fn a_primitive_operation_has_the_result_type_its_rule_states() {
    let mut kernel = kernel();

    assert_eq!(
        infer(&mut kernel, &Term::prim(Prim::nat_add(nat(1), nat(2)))),
        Ok(nat_type()),
    );
    assert_eq!(
        infer(&mut kernel, &Term::prim(Prim::nat_lt(nat(1), nat(2)))),
        Ok(bool_type()),
    );
}

/// A list literal carries its element type, every element checks against it —
/// and `[]` types at exactly that carried element, the case that used to be
/// refused for having no element to read a type from.
#[test]
fn a_list_literal_checks_its_elements_against_its_carried_type() {
    let mut kernel = kernel();

    assert_eq!(
        infer(
            &mut kernel,
            &Term::prim(Prim::Lst(nat_type(), vec![nat(1), nat(2)])),
        ),
        Ok(Term::prim(Prim::LstType(nat_type()))),
    );

    assert!(matches!(
        infer(
            &mut kernel,
            &Term::prim(Prim::Lst(
                nat_type(),
                vec![nat(1), Term::prim(Prim::Bool(true))],
            )),
        ),
        Err(KernelError::Mismatch { .. }),
    ));

    assert_eq!(
        infer(&mut kernel, &Term::prim(Prim::Lst(nat_type(), Vec::new()))),
        Ok(Term::prim(Prim::LstType(nat_type()))),
    );
}

/// A generic definition is checked *under* its own constraint set. `(x :
/// Type.{u}) => x` inhabits `(x : Type.{u}) -> Type.{w}` exactly when `u ≤ w`
/// is among the hypotheses — discarding them was the route by which a correct
/// polymorphic definition was refused.
#[test]
fn a_definition_checks_under_its_own_constraints() {
    use {
        crate::check_definition,
        curios_core::{
            UniverseConstraint, UniverseConstraintKind, UniverseConstraintOrigin, UniverseParam,
        },
    };

    let (u, w) = (
        Level::param(UniverseParam(0)),
        Level::param(UniverseParam(1)),
    );
    let x = binder(0, "x");
    let name = binder(1, "poly");

    let type_ = Term::func_type(
        [(x.clone(), Term::type_at(u.clone()))],
        Term::type_at(w.clone()),
    );
    let body = Term::func([(x.clone(), Term::type_at(u.clone()))], Term::free_var(&x));

    let constrained = UniverseContext {
        parameter_count: 2,
        constraints: vec![UniverseConstraint {
            lower: u,
            upper: w,
            origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
        }],
    };
    let mut kernel = kernel();
    assert_eq!(
        check_definition(&mut kernel, &name, &type_, &body, &constrained),
        Ok(()),
    );

    let unconstrained = UniverseContext {
        parameter_count: 2,
        constraints: Vec::new(),
    };
    let mut kernel = self::kernel();
    assert!(matches!(
        check_definition(&mut kernel, &name, &type_, &body, &unconstrained),
        Err(KernelError::Mismatch { .. }),
    ));
}

/// The other direction: an occurrence must *satisfy* the scheme it
/// instantiates. A scheme declaring `u + 1 ≤ w` refuses the instance `{0, 0}`
/// and admits `{0, 1}`.
#[test]
fn an_instance_must_satisfy_its_schemes_constraints() {
    use curios_core::{
        UniverseConstraint, UniverseConstraintKind, UniverseConstraintOrigin, UniverseParam,
    };

    let u = Level::param(UniverseParam(0));
    let w = Level::param(UniverseParam(1));
    let name = binder(1, "bounded");

    let mut kernel = kernel();
    kernel.declare(
        &name,
        &Term::type_at(u.clone()),
        &UniverseContext {
            parameter_count: 2,
            constraints: vec![UniverseConstraint {
                lower: u.succ().expect("level has a successor"),
                upper: w,
                origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
            }],
        },
    );

    let at = |levels: Vec<Level>| Term::universe_inst(Term::free_var(&name), levels);

    assert_eq!(
        infer(&mut kernel, &at(vec![Level::zero(), one()])),
        Ok(Term::type_at(Level::zero())),
    );
    assert!(matches!(
        infer(&mut kernel, &at(vec![Level::zero(), Level::zero()])),
        Err(KernelError::UniverseInstance { .. }),
    ));
}

/// The two-line route to `False`: a recursive proof assumed at its own type.
/// Erasure deletes proofs, so a proof-typed `rec` member must descend — and
/// `f = f` has a self-call that decreases on nothing.
#[test]
fn a_recursive_proof_that_does_not_descend_is_refused() {
    use curios_base::{Qualifier, RootId};
    use curios_core::{Global, InductDecl, UniverseContext};

    let mut kernel = kernel();
    let name = Global::Authored(Qualifier::from(["False"]));
    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: UniverseContext::default(),
            params: Telescope::done(()),
            indices: Telescope::done(()),
            constructors: Vec::new(),
            result_sort: Term::prop(),
            module: Qualifier::from(["False"]),
            root: RootId::Entry,
            rep_public: true,
            polarities: Vec::new(),
        },
    );
    let false_ = Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new());

    let f = binder(0, "f");
    let term = Term::rec(
        [(f.clone(), false_, Term::free_var(&f))],
        Term::free_var(&f),
    );

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(KernelError::NotDescending { .. }),
    ));
}

/// The type-level twin — `rec Bad : Type = Bad` — is the equi-recursive route:
/// a type-yielding member must descend for the same reason.
#[test]
fn a_recursive_type_that_does_not_descend_is_refused() {
    let mut kernel = kernel();
    let bad = binder(0, "Bad");
    let term = Term::rec(
        [(bad.clone(), Term::type_ground(), Term::free_var(&bad))],
        Term::free_var(&bad),
    );

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(KernelError::NotDescending { .. }),
    ));
}

/// A *value* recursion that does not descend is not an error: `rec` is general
/// recursion by design, and a program that loops is only a program that loops.
#[test]
fn a_recursive_value_needs_no_descent() {
    let mut kernel = kernel();
    let f = binder(0, "f");
    let n = binder(1, "n");

    let signature = Term::func_type([(n.clone(), nat_type())], nat_type());
    let body = Term::func(
        [(n.clone(), nat_type())],
        Term::apply(Term::free_var(&f), [Term::free_var(&n)]),
    );
    let term = Term::rec(
        [(f.clone(), signature, body)],
        Term::apply(Term::free_var(&f), [nat(1)]),
    );

    assert_eq!(infer(&mut kernel, &term), Ok(nat_type()));
}

/// A free-monoid cons arm is checked under its binders — the peeled generator,
/// the tail, and the induction hypothesis at that tail — against the motive at
/// one generator over the tail. The hypothesis really is usable at the tail's
/// instance, and a body of the wrong type at the case's instance is refused.
#[test]
fn a_free_monoid_arm_must_inhabit_the_motive_at_its_case() {
    let mut kernel = kernel();
    let n = binder(0, "n");
    let motive = binder(1, "m");
    let pred = binder(2, "pred");
    let ih = binder(3, "ih");
    kernel.assume(&n, &nat_type());

    // Motive `(m) => Nat`: the zero arm at `Nat`, the succ arm's ih at `Nat`,
    // and using the ih is exactly inhabiting the motive at the tail.
    let counts = Term::nat_match(
        Term::free_var(&n),
        Some(&motive),
        nat_type(),
        nat(0),
        &pred,
        &ih,
        Term::free_var(&ih),
    );
    assert_eq!(infer(&mut kernel, &counts), Ok(nat_type()));

    // A succ arm that produces a Bool where the motive demands a Nat.
    let wrong = Term::nat_match(
        Term::free_var(&n),
        Some(&motive),
        nat_type(),
        nat(0),
        &pred,
        &ih,
        Term::prim(Prim::Bool(true)),
    );
    assert!(matches!(
        infer(&mut kernel, &wrong),
        Err(KernelError::Mismatch { .. }),
    ));

    // A zero arm of the wrong type is refused too — the identity case is a
    // case like any other.
    let wrong_zero = Term::nat_match(
        Term::free_var(&n),
        Some(&motive),
        nat_type(),
        Term::prim(Prim::Bool(true)),
        &pred,
        &ih,
        Term::free_var(&ih),
    );
    assert!(matches!(
        infer(&mut kernel, &wrong_zero),
        Err(KernelError::Mismatch { .. }),
    ));
}

/// The carrier's element type must agree with the scrutinee's: the arms are
/// typed against the carrier's copy, and the values flowing through the match
/// carry the scrutinee's.
#[test]
fn a_free_monoid_carrier_must_match_its_scrutinee() {
    let mut kernel = kernel();
    let xs = binder(0, "xs");
    let motive = binder(1, "m");
    let head = binder(2, "head");
    let tail = binder(3, "tail");
    let ih = binder(4, "ih");
    kernel.assume(&xs, &Term::prim(Prim::LstType(nat_type())));

    // Carrier claims Bool elements over a Nat-list scrutinee.
    let mismatched = Term::lst_match(
        Term::free_var(&xs),
        bool_type(),
        Some(&motive),
        nat_type(),
        nat(0),
        &head,
        &tail,
        &ih,
        Term::free_var(&ih),
    );
    assert!(matches!(
        infer(&mut kernel, &mismatched),
        Err(KernelError::Mismatch { .. }),
    ));
}

/// Elaboration-only syntax reaching the kernel means a term was handed over
/// before elaboration finished with it.
#[test]
fn elaboration_only_syntax_is_refused() {
    let mut kernel = kernel();

    let metavar = Term::metavar(curios_core::MetaId::from(0usize));
    assert!(matches!(
        infer(&mut kernel, &metavar),
        Err(KernelError::NotCore(_)),
    ));
}
