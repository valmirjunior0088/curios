use {
    crate::{Kernel, KernelError, check, check_definition, infer},
    curios_base::{Plicity, Qualifier, RootId},
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam, Intrinsic, Level, MetaId, Nat, Polarity,
        StructDecl, StructType, Subterm, Telescope, Term, UniverseConstraint,
        UniverseConstraintKind, UniverseConstraintOrigin, UniverseContext, UniverseParam,
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
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}

fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

fn bool_type() -> Term {
    Term::intrinsic(Intrinsic::BoolType)
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
        infer(&mut kernel, &Term::intrinsic(Intrinsic::Bool(true))),
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

/// A finished term has no free names. Refusing rather than treating one as a neutral is what makes that a checked statement.
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

/// Dependency: applying a family to an argument puts *that argument* into the result type, which is the whole point of a dependent function.
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
    let applied = Term::apply(f, [Term::intrinsic(Intrinsic::Bool(true))]);

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

    let pair = Term::tuple([nat(1), Term::intrinsic(Intrinsic::Bool(false))]);
    let type_ = infer(&mut kernel, &pair).expect("a closed pair has a type");

    assert_eq!(
        infer(&mut kernel, &Term::proj(pair.clone(), 0)),
        Ok(nat_type()),
    );
    assert_eq!(infer(&mut kernel, &Term::proj(pair, 1)), Ok(bool_type()));
    assert!(matches!(&*type_, Subterm::TupleType(_)));
}

#[test]
fn projecting_from_a_non_tuple_is_refused() {
    let mut kernel = kernel();

    assert!(matches!(
        infer(&mut kernel, &Term::proj(nat(1), 0)),
        Err(KernelError::NotATuple(_)),
    ));
}

/// `let` is checked binding by binding and then substituted, so the tail's type is computed with the values in place rather than with opaque names.
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

/// A recursive group's members are assumed at their declared types while their bodies are checked, which is what lets a member call itself.
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

/// A group whose body does not have the type it declares is refused — the assumption is what the body must live up to, not a licence.
#[test]
fn a_recursive_body_that_misses_its_declared_type_is_refused() {
    let mut kernel = kernel();
    let f = binder(0, "f");
    let n = binder(1, "n");

    let signature = Term::func_type([(n.clone(), nat_type())], nat_type());
    let body = Term::func([(n, nat_type())], Term::intrinsic(Intrinsic::Bool(true)));

    let term = Term::rec(
        [(f.clone(), signature, body)],
        Term::apply(Term::free_var(&f), [nat(1)]),
    );

    assert!(matches!(
        infer(&mut kernel, &term),
        Err(KernelError::Mismatch { .. }),
    ));
}

/// A constructor's type is its signature's terminal — the family at the parameters and index targets this case aims at.
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
            arity: Telescope::done(Telescope::done(())),
            constructors: vec![(
                Atom::from("mk"),
                InductParam {
                    telescope: Telescope::build([(payload, nat_type())], Vec::new()),
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

    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::done(())),
            constructors: vec![(
                Atom::from("mk"),
                InductParam {
                    telescope: Telescope::build([(payload, nat_type())], Vec::new()),
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
        [Term::intrinsic(Intrinsic::Bool(true))],
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

/// Cumulativity: a small type stands where a larger universe is wanted, and `Prop` stands wherever a `Type` is. The reverse does not hold.
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

/// An intrinsic's operands are checked against the types its rule demands.
#[test]
fn an_intrinsic_operand_of_the_wrong_type_is_refused() {
    let mut kernel = kernel();

    let mixed = Term::intrinsic(Intrinsic::nat_add(
        Term::intrinsic(Intrinsic::Bool(true)),
        nat(1),
    ));

    assert!(matches!(
        infer(&mut kernel, &mixed),
        Err(KernelError::Mismatch { .. }),
    ));
}

#[test]
fn an_intrinsic_operation_has_the_result_type_its_rule_states() {
    let mut kernel = kernel();

    assert_eq!(
        infer(
            &mut kernel,
            &Term::intrinsic(Intrinsic::nat_add(nat(1), nat(2)))
        ),
        Ok(nat_type()),
    );
    assert_eq!(
        infer(
            &mut kernel,
            &Term::intrinsic(Intrinsic::nat_lt(nat(1), nat(2)))
        ),
        Ok(bool_type()),
    );
}

/// A list literal carries its element type, every element checks against it — and `[]` types at exactly that carried element, the case that used to be refused for having no element to read a type from.
#[test]
fn a_list_literal_checks_its_elements_against_its_carried_type() {
    let mut kernel = kernel();

    assert_eq!(
        infer(
            &mut kernel,
            &Term::intrinsic(Intrinsic::Lst(nat_type(), vec![nat(1), nat(2)])),
        ),
        Ok(Term::intrinsic(Intrinsic::LstType(nat_type()))),
    );

    assert!(matches!(
        infer(
            &mut kernel,
            &Term::intrinsic(Intrinsic::Lst(
                nat_type(),
                vec![nat(1), Term::intrinsic(Intrinsic::Bool(true))],
            )),
        ),
        Err(KernelError::Mismatch { .. }),
    ));

    assert_eq!(
        infer(
            &mut kernel,
            &Term::intrinsic(Intrinsic::Lst(nat_type(), Vec::new()))
        ),
        Ok(Term::intrinsic(Intrinsic::LstType(nat_type()))),
    );
}

/// A generic definition is checked *under* its own constraint set. `(x : Type.{u}) => x` inhabits `(x : Type.{u}) -> Type.{w}` exactly when `u ≤ w` is among the hypotheses — discarding them was the route by which a correct polymorphic definition was refused.
#[test]
fn a_definition_checks_under_its_own_constraints() {
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

/// The other direction: an occurrence must *satisfy* the scheme it instantiates. A scheme declaring `u + 1 ≤ w` refuses the instance `{0, 0}` and admits `{0, 1}`.
#[test]
fn an_instance_must_satisfy_its_schemes_constraints() {
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

/// The two-line route to `False`: a recursive proof assumed at its own type. Erasure deletes proofs, so a proof-typed `rec` member must descend — and `f = f` has a self-call that decreases on nothing.
#[test]
fn a_recursive_proof_that_does_not_descend_is_refused() {
    let mut kernel = kernel();
    let name = Global::Authored(Qualifier::from(["False"]));
    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::done(())),
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

/// The type-level twin — `rec Bad : Type = Bad` — is the equi-recursive route: a type-yielding member must descend for the same reason.
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

/// A *value* recursion that does not descend is not an error: `rec` is general recursion by design, and a program that loops is only a program that loops.
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

/// A free-monoid cons arm is checked under its binders — the peeled generator, the tail, and the induction hypothesis at that tail — against the motive at one generator over the tail. The hypothesis really is usable at the tail's instance, and a body of the wrong type at the case's instance is refused.
#[test]
fn a_free_monoid_arm_must_inhabit_the_motive_at_its_case() {
    let mut kernel = kernel();
    let n = binder(0, "n");
    let motive = binder(1, "m");
    let pred = binder(2, "pred");
    let ih = binder(3, "ih");
    kernel.assume(&n, &nat_type());

    // Motive `(m) => Nat`: the zero arm at `Nat`, the succ arm's ih at `Nat`, and using the ih is exactly inhabiting the motive at the tail.
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
        Term::intrinsic(Intrinsic::Bool(true)),
    );
    assert!(matches!(
        infer(&mut kernel, &wrong),
        Err(KernelError::Mismatch { .. }),
    ));

    // A zero arm of the wrong type is refused too — the identity case is a case like any other.
    let wrong_zero = Term::nat_match(
        Term::free_var(&n),
        Some(&motive),
        nat_type(),
        Term::intrinsic(Intrinsic::Bool(true)),
        &pred,
        &ih,
        Term::free_var(&ih),
    );
    assert!(matches!(
        infer(&mut kernel, &wrong_zero),
        Err(KernelError::Mismatch { .. }),
    ));
}

/// The carrier's element type must agree with the scrutinee's: the arms are typed against the carrier's copy, and the values flowing through the match carry the scrutinee's.
#[test]
fn a_free_monoid_carrier_must_match_its_scrutinee() {
    let mut kernel = kernel();
    let xs = binder(0, "xs");
    let motive = binder(1, "m");
    let head = binder(2, "head");
    let tail = binder(3, "tail");
    let ih = binder(4, "ih");
    kernel.assume(&xs, &Term::intrinsic(Intrinsic::LstType(nat_type())));

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

/// A list or a cell *of* proofs is not a proposition, and the typing rule has to say so — `Sort::of` already does.
///
/// The two answers came from two implementations of one rule. `Sort::of` routes a parameterized former through `sort_of_intrinsic`, which lands a `Prop`-sorted element at `Type 0` on the reasoning `sort/tests.rs` states: a list has a length and a cell has an identity, so their inhabitants are distinguishable however indistinguishable the elements are. The typing rule computed the *element's* sort instead and reported that as the former's, so `Lst(P)` inferred at `Prop` while `Sort::of(Lst(P))` said `Type 0`.
///
/// Only one of those can be right, and the disagreement is a closed inhabitant of `False`. `Prop` is the type of propositions, so a former admitted there stands wherever one is wanted: at `(X : Prop, x : X, y : X) -> Eq(@X, x, y)` — reflexivity discharges it, since irrelevance identifies any two inhabitants of `X` — instantiating `X` at `Lst(P)` yields `Eq(Lst(P), [p], [])` for a one-element list against the empty one. Congruence through `Lst/len` carries that to `Eq(1, 0)`, and transport turns `()` into a proof of `False`.
///
/// Verified while the hole was open: `check(Lst(P), Prop)` returned `Ok(())`, `check_definition` accepted that lemma, and `infer` on the instantiated application returned `Eq(Lst P, [p], [])`. The surface route was live too — `curios/src/tests/perimeter::a_list_of_proofs_is_not_a_proposition` is the program, which elaborated and which the compile-path recheck certified. It stopped short of a runtime only in erasure, which refuses any call whose every argument erases; that is a separate defect of the erase boundary, and the same lemma at a genuine proposition trips it identically.
///
/// The controls are the other half. A list at a relevant element still reports that element's level, `Lst(Type 0)` included, so the fix cannot have pinned every former at zero; and a genuine proposition still stands where a `Prop` is wanted, so it cannot have closed the hole by refusing the position outright.
#[test]
fn a_list_or_cell_of_proofs_is_not_a_proposition() {
    let mut kernel = kernel();
    let name = Global::Authored(Qualifier::from(["P"]));

    // `induct P : Prop end` — an empty proposition, so `Sort::of` reads `Prop` off its declaration.
    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::done(())),
            constructors: Vec::new(),
            result_sort: Term::prop(),
            module: Qualifier::from(["P"]),
            root: RootId::Entry,
            rep_public: true,
            polarities: Vec::new(),
        },
    );
    let proposition = Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new());

    let list = Term::intrinsic(Intrinsic::LstType(proposition.clone()));
    let cell = Term::intrinsic(Intrinsic::CellType(proposition.clone()));

    assert_eq!(infer(&mut kernel, &list), Ok(Term::type_ground()));
    assert_eq!(infer(&mut kernel, &cell), Ok(Term::type_ground()));

    assert!(matches!(
        check(&mut kernel, &list, &Term::prop()),
        Err(KernelError::Mismatch { .. }),
    ));
    assert!(matches!(
        check(&mut kernel, &cell, &Term::prop()),
        Err(KernelError::Mismatch { .. }),
    ));

    // A former still carries its element's level rather than being pinned at zero, and a proposition still stands where one is wanted.
    assert_eq!(
        infer(
            &mut kernel,
            &Term::intrinsic(Intrinsic::LstType(nat_type()))
        ),
        Ok(Term::type_ground()),
    );
    assert_eq!(
        infer(
            &mut kernel,
            &Term::intrinsic(Intrinsic::LstType(Term::type_ground())),
        ),
        Ok(Term::type_at(one())),
    );
    assert_eq!(check(&mut kernel, &proposition, &Term::prop()), Ok(()));
}

/// Elaboration-only syntax reaching the kernel means a term was handed over before elaboration finished with it.
#[test]
fn elaboration_only_syntax_is_refused() {
    let mut kernel = kernel();

    let metavar = Term::metavar(MetaId::from(0usize));
    assert!(matches!(
        infer(&mut kernel, &metavar),
        Err(KernelError::NotCore(_)),
    ));
}

/// A structure with one parameter and one field at that parameter's type — the shape an occurrence can state the wrong parameter count for.
fn parameterized_struct(kernel: &mut Kernel) -> Global {
    let name = Global::Authored(Qualifier::from(["Boxed"]));
    let param = binder(0, "A");
    let field = binder(1, "value");

    let fields = Telescope::build([(field, Term::free_var(&param))], ());
    let declaration = StructDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::build([(param, Term::type_ground())], fields),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        root: RootId::Entry,
        rep_public: true,
        polarities: vec![Polarity::Strict],
    };
    kernel.declare_struct(&name, &declaration);

    name
}

/// A nominal occurrence states as many parameters as its declaration declares, and the two rules that open a `struct` arity at an occurrence's parameters must check that before opening it.
///
/// `Telescope::open` asserts on a count mismatch, so an unguarded rule does not refuse the item — it **aborts the walk**, losing every other verdict, which is what makes `recheck_module_verdicts`' count a count. `Sort::of` and `synth_neutral` both guarded this; `infer`'s projection rule and `check`'s record rule reached `open` behind `check_instance` alone, which decides the *universe* width and says nothing about the parameters.
///
/// Neither is reachable from a surface program — the elaborator does not emit an occurrence at the wrong count — so this is the shape that needs a term built by hand, and it is a fail-open-to-abort rather than an unsoundness. Both rules are reached here at a one-parameter family occurring with none: the projection through a variable assumed at that type, and the record through a literal checked against it.
#[test]
fn a_structure_occurrence_at_the_wrong_parameter_count_is_refused() {
    let mut kernel = kernel();
    let name = parameterized_struct(&mut kernel);

    let short: Term = Subterm::StructType(StructType {
        name,
        universes: Vec::new(),
        params: Vec::new(),
    })
    .into();

    let value = binder(2, "v");
    kernel.assume(&value, &short);

    assert!(
        matches!(
            infer(&mut kernel, &Term::proj(Term::free_var(&value), 0)),
            Err(KernelError::Arity { .. }),
        ),
        "a projection opened the arity at a parameter count the declaration does not have",
    );

    assert!(
        matches!(
            check(&mut kernel, &Term::tuple([nat(0)]), &short),
            Err(KernelError::Arity { .. }),
        ),
        "a record literal opened the arity at a parameter count the declaration does not have",
    );
}

/// The control: at the parameter count it declares, the same family still projects and still accepts a record.
///
/// Without it the guard above would pass just as well if the rules refused every `struct` occurrence.
#[test]
fn a_structure_occurrence_at_its_declared_parameter_count_still_works() {
    let mut kernel = kernel();
    let name = parameterized_struct(&mut kernel);

    let exact: Term = Subterm::StructType(StructType {
        name,
        universes: Vec::new(),
        params: vec![nat_type()],
    })
    .into();

    let value = binder(2, "v");
    kernel.assume(&value, &exact);

    assert_eq!(
        infer(&mut kernel, &Term::proj(Term::free_var(&value), 0)),
        Ok(nat_type()),
    );
    assert_eq!(check(&mut kernel, &Term::tuple([nat(7)]), &exact), Ok(()));
}

/// A case form names the carrier it eliminates, and that claim needs establishing like any other.
///
/// `check_free_monoid` establishes it — `Carrier::Nat` matches the scrutinee's type against `NatType`, `Carrier::Bin` against its own grain, and `Carrier::Lst` converts its carried element type against the scrutinee's — and `Cases::Induct` gets it from needing an `InductType` to read a declaration off at all. The other two forms read the claim and checked nothing, which is the same shape as every count the boundary now checks: no typing rule looks at a case form, so no ordering discipline would ever have caught it.
///
/// What it costs is the discipline the free-monoid rule states for itself: the arms are typed at the *case values* — `false` and `true`, or the enumerated literals — while the result is typed at `motive(scrutinee)` and a value flowing through the match carries the scrutinee's type, so a disagreement types the arms at one carrier and runs them at another. `curios-elab` refuses both spellings at `check_intrinsic_head`, which is why no surface program reaches them and why the certifier's copy of the rule went unwritten.
#[test]
fn a_boolean_elimination_requires_a_boolean_scrutinee() {
    let mut kernel = kernel();

    // `match (0 : Nat) : (_) => Nat | false => 1 | true => 2 end`
    let eliminated = Term::bool_match(nat(0), None, nat_type(), nat(1), nat(2));

    assert!(
        infer(&mut kernel, &eliminated).is_err(),
        "the kernel typed a boolean elimination of a `Nat`",
    );
}

/// The same for the dispatch form, whose default is checked at the scrutinee's own instance while its enumerated arms are checked at `Nat` literals.
#[test]
fn a_dispatch_requires_a_natural_scrutinee() {
    let mut kernel = kernel();

    // `match (true : Bool) : (_) => Nat | 0 => 1 | _ => 2 end`
    let eliminated = Term::switch(
        Term::intrinsic(Intrinsic::Bool(true)),
        None,
        nat_type(),
        [(0u32, nat(1))],
        nat(2),
    );

    assert!(
        infer(&mut kernel, &eliminated).is_err(),
        "the kernel typed a `Nat` dispatch on a `Bool`",
    );
}

/// The control for both, and it is what a fix by brick would fail: each form at its own carrier is ordinary code and must keep typing.
#[test]
fn each_intrinsic_elimination_at_its_own_carrier_is_still_accepted() {
    let mut kernel = kernel();

    let boolean = Term::bool_match(
        Term::intrinsic(Intrinsic::Bool(true)),
        None,
        nat_type(),
        nat(1),
        nat(2),
    );
    assert_eq!(infer(&mut kernel, &boolean), Ok(nat_type()));

    let dispatch = Term::switch(nat(0), None, nat_type(), [(0u32, nat(1))], nat(2));
    assert_eq!(infer(&mut kernel, &dispatch), Ok(nat_type()));
}
