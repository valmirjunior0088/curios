//! Variance and cumulativity of a function type, the bare-sort head rules, the relation through a definition and under an arm's case, and the sigma and pi rules at an argument position.

use {
    super::{subsumes, test_support::*},
    crate::{Error, infer},
    curios_core::{Free, Term, UniverseContext},
    curios_utilities::Plicity,
};

/// The subsumption fork, which the soundness board records as having no fixture in either direction.
///
/// `subsumes` compares a Π's **domains by conversion** and its **codomains cumulatively**. Only one of those two choices can be got wrong in the admitting direction, and it is the domain: reading it *covariantly* would accept `(x : Type 0) -> B` where `(x : Type 1) -> B'` is wanted, so a function that only handles small arguments would be applied to a large one, which is the shape the hierarchy exists to forbid. Contravariance would be sound and strictly more permissive; invariance is what ships, and it is the freely-revisable side precisely because widening later breaks nothing already accepted.
///
/// Both directions are asserted, because invariance is the conjunction of two refusals and a rule that had drifted to covariance would still refuse the other one. The head cases are the control: without them a `subsumes` that refused every function type would satisfy the two assertions above, and cumulativity would be dead while the test stayed green.
#[test]
fn a_function_types_domain_is_invariant() {
    let mut kernel = kernel();
    let x = binder(900, "x");
    let pi = |domain: Term| Term::func_type([(x, domain)], Term::tuple_type_unit());

    // Neither direction: the domains are compared by conversion, and `Type 0` is not convertible with `Type 1`.
    assert_eq!(
        subsumes(
            &mut kernel,
            &pi(Term::type_ground()),
            &pi(Term::type_at(one()))
        ),
        Ok(false),
    );
    assert_eq!(
        subsumes(
            &mut kernel,
            &pi(Term::type_at(one())),
            &pi(Term::type_ground())
        ),
        Ok(false),
    );
}

/// The other half of the same fork, and the direction the language needs: under a binder the codomains are still compared cumulatively, each sort in its own levels, and a proposition stands for no type there either.
#[test]
fn a_function_types_codomain_is_cumulative() {
    let mut kernel = kernel();
    let x = binder(901, "x");
    let pi = |codomain: Term| Term::func_type([(x, nat_type())], codomain);

    assert_eq!(
        subsumes(
            &mut kernel,
            &pi(Term::type_ground()),
            &pi(Term::type_at(one()))
        ),
        Ok(true),
    );
    assert_eq!(
        subsumes(
            &mut kernel,
            &pi(Term::type_at(one())),
            &pi(Term::type_ground())
        ),
        Ok(false),
    );
    assert_eq!(
        subsumes(&mut kernel, &pi(Term::prop()), &pi(Term::prop_at(one()))),
        Ok(true),
    );
    // No proposition stands where a type is wanted, under a binder as at the head.
    assert_eq!(
        subsumes(&mut kernel, &pi(Term::prop()), &pi(Term::type_ground())),
        Ok(false),
    );
}

/// The control for both fixtures above: the same verdicts at the head, where no telescope is walked at all. A `subsumes` that had stopped descending into function types would pass those two and fail these.
#[test]
fn the_head_rules_still_decide_a_bare_sort() {
    let mut kernel = kernel();

    assert_eq!(
        subsumes(&mut kernel, &Term::type_ground(), &Term::type_at(one())),
        Ok(true),
    );
    assert_eq!(
        subsumes(&mut kernel, &Term::type_at(one()), &Term::type_ground()),
        Ok(false),
    );
    assert_eq!(
        subsumes(&mut kernel, &Term::prop(), &Term::prop_at(one())),
        Ok(true),
    );
    assert_eq!(
        subsumes(&mut kernel, &Term::prop(), &Term::type_ground()),
        Ok(false),
    );
    assert_eq!(
        subsumes(&mut kernel, &Term::type_ground(), &Term::prop()),
        Ok(false),
    );
}

/// A definition that names a sort or a function type is subsumed as what it names: `subsumes` reads both sides forced, so an alias of `(x: Nat) -> Type 0` stands where `(x: Nat) -> Type 1` is wanted and an alias of `Prop` where `Prop 1` is, while an alias of a predicate, or of `Prop`, stands for no type on either side. Read as written, the aliases' cumulative halves would go on as the equations they are not, which conversion refuses once it has unfolded them.
///
/// Mutation-checked: with `subsumes` matching on its two sides unreduced, the alias of a function type is refused where `(x: Nat) -> Type 1` is wanted, and no other fixture of this module moves.
#[test]
fn a_definition_naming_a_sort_or_a_function_type_is_subsumed_as_what_it_names() {
    let mut kernel = kernel();
    let x = binder(903, "x");
    let pi = |codomain: Term| Term::func_type([(x, nat_type())], codomain);
    let family = binder(904, "Family");
    let predicate = binder(905, "Predicate");
    let small = binder(906, "Small");
    for (name, value) in [
        (family, pi(Term::type_ground())),
        (predicate, pi(Term::prop())),
        (small, Term::prop()),
    ] {
        kernel
            .define(&name, &Term::type_at(one()), &value, &UniverseContext::empty())
            .expect("a name is declared once");
    }
    let named = |name: &Free| Term::free_var(name);

    assert_eq!(
        subsumes(&mut kernel, &named(&family), &pi(Term::type_at(one()))),
        Ok(true),
    );
    assert_eq!(
        subsumes(&mut kernel, &named(&predicate), &pi(Term::type_at(one()))),
        Ok(false),
    );
    assert_eq!(
        subsumes(&mut kernel, &named(&small), &Term::prop_at(one())),
        Ok(true),
    );
    assert_eq!(
        subsumes(&mut kernel, &named(&small), &Term::type_at(one())),
        Ok(false),
    );
    assert_eq!(
        subsumes(&mut kernel, &pi(Term::prop()), &named(&family)),
        Ok(false),
    );
}

/// The locals a type is read under where a split has put `k(0)` at `true`: `k: (Nat) -> Bool`, a proposition `P`, a family of small types `f: (Nat) -> Type 0` and a predicate `g: (Nat) -> Prop`.
struct UnderTheCase {
    k: Free,
    p: Free,
    f: Free,
    g: Free,
}

impl UnderTheCase {
    fn new() -> Self {
        Self {
            k: binder(910, "k"),
            p: binder(911, "P"),
            f: binder(912, "f"),
            g: binder(913, "g"),
        }
    }

    fn decided_at(&self) -> Term {
        Term::apply(Term::free_var(&self.k), [nat(0)])
    }

    /// `match k(0) : (_) => Type 2 | false => Nat | true => upper end`: `upper` where the split's equation is in force, and neither a sort nor a function type while `k(0)` is open.
    fn decided(&self, upper: Term) -> Term {
        let two = one().succ().expect("level one has a successor");
        Term::bool_match(
            self.decided_at(),
            None,
            Term::type_at(two),
            nat_type(),
            upper,
        )
    }

    /// `(k, P, f, g) => match k(0) : (_) => Nat | false => 0 | true => let u: decided(upper) = held; 0 end`.
    fn holding(&self, upper: Term, held: Term) -> Term {
        let over_nat = |hint: u32, codomain: Term| {
            Term::func_type([(binder(hint, "n"), nat_type())], codomain)
        };
        Term::func(
            [
                (self.k, over_nat(915, bool_type())),
                (self.p, Term::prop()),
                (self.f, over_nat(916, Term::type_ground())),
                (self.g, over_nat(917, Term::prop())),
            ],
            Term::bool_match(
                self.decided_at(),
                None,
                nat_type(),
                nat(0),
                Term::let_(&binder(914, "u"), self.decided(upper), held, nat(0)),
            ),
        )
    }
}

/// A type that is a sort or a function type only where an arm's case equation is in force is subsumed there as the one the case makes it, cumulatively and holding no proposition: under `k(0) = true`, `decided(Type 1)` takes `Nat` and refuses `P`, and `decided((n: Nat) -> Type 1)` takes the family of small types `f` and refuses the predicate `g`. `curios`'s `tests::board::subsumption_tests` puts the same propositions to the elaborator under this name.
///
/// Mutation-checked: with `subsumes` matching on its two sides unreduced, `Nat` is refused at the sort the case reaches.
#[test]
fn what_an_arms_case_makes_a_sort_or_a_function_type_is_subsumed_as_one_there() {
    let case = UnderTheCase::new();
    let small_family = || {
        Term::func_type(
            [(binder(918, "n"), nat_type())],
            Term::type_at(one()),
        )
    };
    let refused = |verdict: Result<Term, Error>| matches!(verdict, Err(Error::Mismatch { .. }));

    assert!(
        infer(
            &mut kernel(),
            &case.holding(Term::type_at(one()), nat_type())
        )
        .is_ok()
    );
    assert!(refused(infer(
        &mut kernel(),
        &case.holding(Term::type_at(one()), Term::free_var(&case.p)),
    )));
    assert!(
        infer(
            &mut kernel(),
            &case.holding(small_family(), Term::free_var(&case.f))
        )
        .is_ok()
    );
    assert!(refused(infer(
        &mut kernel(),
        &case.holding(small_family(), Term::free_var(&case.g)),
    )));
}

/// An argument reaches the *checked* rules, not merely inference and subsumption.
///
/// `check` dispatches let-descent, Π-introduction and Σ-introduction before falling through to infer-then-subsume, and an argument position must reach them. A typing walk driving its child obligations through an explicit worklist that calls the node rule and `subsumes` directly loses all three rules at every argument, constructor payload and record field, and nothing in the prelude or the corpus reaches a shape that shows it — which is why this fixture is written against the *rule* rather than against a program.
///
/// Mutation-checked: replacing the `check` call at the application arm of `infer_within` with an inference and a `subsumes` refuses this term with a mismatch between `(Type, Nat)` and the dependent pair.
#[test]
fn a_dependent_tuple_in_argument_position_reaches_the_sigma_rule() {
    let mut kernel = kernel();

    let f = binder(42, "f");
    kernel.assume(
        &f,
        &Term::func_type(
            [(binder(43, "p"), dependent_pair_type())],
            Term::tuple_type_unit(),
        ),
    );

    // `f((7))`: well-typed only by opening the telescope at the first component, which is what the Σ rule does and inference cannot.
    let call = Term::apply(Term::free_var(&f), [Term::tuple([nat_type(), nat(7)])]);

    assert_eq!(infer(&mut kernel, &call), Ok(Term::tuple_type_unit()));
}

/// The Π half of the rule above: a lambda in argument position is checked against its expected function type, so its *body* keeps the expectation.
///
/// Routing the body through `check` is what lets it reach the Σ rule with that expectation intact; inferring the lambda instead manufactures the non-dependent codomain `(Type, Nat)` first, and the mismatch surfaces at the codomain rather than at the body. The two rules compose here exactly as they do in `check`.
///
/// Mutation-checked with the fixture above, and by the same edit.
#[test]
fn a_lambda_in_argument_position_reaches_the_pi_rule() {
    let mut kernel = kernel();

    let g = binder(44, "g");
    kernel.assume(
        &g,
        &Term::func_type(
            [(
                binder(45, "k"),
                Term::func_type([(binder(46, "n"), nat_type())], dependent_pair_type()),
            )],
            Term::tuple_type_unit(),
        ),
    );

    // `g((n) => (n))`: the body is a dependent pair, and only the checked route hands it the expectation that makes it one.
    let n = binder(47, "n");
    let call = Term::apply(
        Term::free_var(&g),
        [Term::func(
            [(n, nat_type())],
            Term::tuple([nat_type(), Term::free_var(&n)]),
        )],
    );

    assert_eq!(infer(&mut kernel, &call), Ok(Term::tuple_type_unit()));
}

/// The codomain rule reaches no function type across a mark: subsumption is cumulative under a binder only where the two telescopes state one calling convention, and a pair apart in a mark goes on as the equation it is, which refuses it. Read past the mark, `(@x: Nat) -> Type 0` would stand where `(x: Nat) -> Type 1` is wanted. The control is the same pair under one mark, which the codomain rule accepts.
#[test]
fn a_function_type_is_below_none_of_other_marks() {
    let mut kernel = kernel();
    let x = binder(902, "x");
    let pi =
        |mark: Plicity, codomain: Term| Term::func_type_marked([(mark, x, nat_type())], codomain);

    assert_eq!(
        subsumes(
            &mut kernel,
            &pi(Plicity::Implicit, Term::type_ground()),
            &pi(Plicity::Implicit, Term::type_at(one()))
        ),
        Ok(true),
    );
    assert_eq!(
        subsumes(
            &mut kernel,
            &pi(Plicity::Implicit, Term::type_ground()),
            &pi(Plicity::Explicit, Term::type_at(one()))
        ),
        Ok(false),
    );
}
