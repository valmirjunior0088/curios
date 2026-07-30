use {
    crate::{Kernel, KernelError, convert},
    curios_base::{Plicity, Qualifier, RootId},
    curios_core::{Free, Global, InductDecl, MetaId, Prim, Telescope, Term, UniverseContext},
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

/// A nominal family at a stated sort — the only way to obtain a base proposition, since the registry is what says a nominal type is one.
fn declare(kernel: &mut Kernel, path: &str, result_sort: Term) -> Term {
    let name = Global::Authored(Qualifier::from([path]));

    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: UniverseContext::default(),
            params: Telescope::done(()),
            indices: Telescope::done(()),
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
fn a_term_converts_with_itself() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    assert_eq!(
        convert(
            &mut kernel,
            &nat_type(),
            &Term::free_var(&x),
            &Term::free_var(&x)
        ),
        Ok(true),
    );
}

#[test]
fn distinct_literals_do_not_convert() {
    let mut kernel = kernel();

    assert_eq!(
        convert(&mut kernel, &nat_type(), &nat(1), &nat(2)),
        Ok(false)
    );
}

/// Conversion is up to computation, so a redex converts with its value.
#[test]
fn beta_equal_terms_convert() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let redex = Term::apply(
        Term::func([(x.clone(), nat_type())], Term::free_var(&x)),
        [nat(4)],
    );

    assert_eq!(convert(&mut kernel, &nat_type(), &redex, &nat(4)), Ok(true));
}

#[test]
fn a_definition_converts_with_its_value() {
    let mut kernel = kernel();
    let f = binder(0, "f");
    kernel.define(&f, &nat_type(), &nat(3), &UniverseContext::default());

    assert_eq!(
        convert(&mut kernel, &nat_type(), &Term::free_var(&f), &nat(3)),
        Ok(true),
    );
}

/// Eta at a function type: `f` and `(x) => f(x)` are the same function, and neither side has to be written in the other's shape for conversion to see it.
#[test]
fn eta_makes_a_function_converge_with_its_expansion() {
    let mut kernel = kernel();
    let f = binder(0, "f");
    let x = binder(1, "x");
    let arrow = Term::func_type([(x.clone(), nat_type())], nat_type());

    let expanded = Term::func(
        [(x.clone(), nat_type())],
        Term::apply(Term::free_var(&f), [Term::free_var(&x)]),
    );

    assert_eq!(
        convert(&mut kernel, &arrow, &Term::free_var(&f), &expanded),
        Ok(true),
    );
}

/// Eta at a Σ type: `p` and `(p.0, p.1)` are the same pair.
#[test]
fn eta_makes_a_pair_converge_with_its_projections() {
    let mut kernel = kernel();
    let p = binder(0, "p");
    let pair_type = Term::tuple_type([(binder(8, "a"), nat_type()), (binder(9, "b"), nat_type())]);

    let expanded = Term::tuple([
        Term::proj(Term::free_var(&p), 0),
        Term::proj(Term::free_var(&p), 1),
    ]);

    assert_eq!(
        convert(&mut kernel, &pair_type, &Term::free_var(&p), &expanded),
        Ok(true),
    );
}

/// Proof irrelevance: at a `Prop`-sorted type any two terms convert, *without* either being examined. This is what licenses erasure to drop proofs wholesale.
#[test]
fn any_two_inhabitants_of_a_proposition_convert() {
    let mut kernel = kernel();
    let proposition = declare(&mut kernel, "P", Term::prop());
    let (left, right) = (binder(0, "p"), binder(1, "q"));

    assert_eq!(
        convert(
            &mut kernel,
            &proposition,
            &Term::free_var(&left),
            &Term::free_var(&right),
        ),
        Ok(true),
    );
}

/// The same two terms at a *relevant* type are not interchangeable. Irrelevance is a property of the type, and this is the direction that would be unsound to get wrong.
#[test]
fn irrelevance_does_not_leak_into_a_relevant_type() {
    let mut kernel = kernel();
    let data = declare(&mut kernel, "D", Term::type_ground());
    let (left, right) = (binder(0, "p"), binder(1, "q"));

    assert_eq!(
        convert(
            &mut kernel,
            &data,
            &Term::free_var(&left),
            &Term::free_var(&right),
        ),
        Ok(false),
    );
}

/// A primitive is congruent when it is the same operation on convertible operands — decided generically, so no operation can be omitted from the rule.
#[test]
fn a_primitive_is_congruent_in_its_operands() {
    let mut kernel = kernel();
    let n = binder(0, "n");
    let m = binder(1, "m");
    kernel.define(
        &m,
        &nat_type(),
        &Term::free_var(&n),
        &UniverseContext::default(),
    );

    let left = Term::prim(Prim::nat_mul(Term::free_var(&n), nat(3)));
    let right = Term::prim(Prim::nat_mul(Term::free_var(&m), nat(3)));

    assert_eq!(convert(&mut kernel, &nat_type(), &left, &right), Ok(true));
}

#[test]
fn different_operations_do_not_convert() {
    let mut kernel = kernel();
    let n = binder(0, "n");

    let add = Term::prim(Prim::nat_add(Term::free_var(&n), nat(3)));
    let mul = Term::prim(Prim::nat_mul(Term::free_var(&n), nat(3)));

    assert_eq!(convert(&mut kernel, &nat_type(), &add, &mul), Ok(false));
}

/// The free-monoid peel is what decides `n + 2 ≡ m + 2` by comparing `n` with `m` rather than comparing two opaque symbolic sums.
#[test]
fn a_shared_successor_floor_is_peeled_before_comparing() {
    let mut kernel = kernel();
    let n = binder(0, "n");
    let m = binder(1, "m");

    let left = Term::prim(Prim::nat_add(Term::free_var(&n), nat(2)));
    let right = Term::prim(Prim::nat_add(Term::free_var(&m), nat(2)));

    // Distinct symbolic bases: the peel exposes the real disagreement.
    assert_eq!(convert(&mut kernel, &nat_type(), &left, &right), Ok(false));

    // The same base: equal after the shared floor comes off.
    let same = Term::prim(Prim::nat_add(Term::free_var(&n), nat(2)));
    assert_eq!(convert(&mut kernel, &nat_type(), &left, &same), Ok(true));
}

/// Plicity is part of a function type's identity: `(A) -> A` and `(@A) -> A` have different calling conventions, and conflating them would let a value be applied through the wrong one.
#[test]
fn plicity_distinguishes_two_function_types() {
    let mut kernel = kernel();
    let a = binder(0, "a");

    let explicit = Term::func_type([(a.clone(), nat_type())], nat_type());
    let implicit = Term::from(curios_core::Subterm::FuncType(curios_core::FuncType {
        telescope: match &*explicit {
            curios_core::Subterm::FuncType(func) => func.telescope.clone(),
            _ => unreachable!("built as a function type"),
        },
        plicities: vec![Plicity::Implicit],
    }));

    assert_eq!(
        convert(&mut kernel, &Term::type_ground(), &explicit, &implicit),
        Ok(false),
    );
}

/// Two universes convert only at the same level. Cumulativity is a *subtyping* rule and belongs to checking, not here: conversion is symmetric and levels are not.
#[test]
fn universes_convert_only_at_the_same_level() {
    let mut kernel = kernel();
    let zero = Term::type_ground();
    let one = Term::type_at(
        curios_core::Level::zero()
            .succ()
            .expect("level zero succeeds"),
    );

    assert_eq!(convert(&mut kernel, &zero, &zero, &zero), Ok(true));
    assert_eq!(convert(&mut kernel, &zero, &zero, &one), Ok(false));
}

/// Binder *identity* must not leak into conversion. Two `rec` groups written with different minted names are the same group: binder names are display hints, and the bodies are de Bruijn-indexed under their scopes.
///
/// This is the property that lets a folded recursive call be compared structurally at all — see the `RecMember` arm, which requires the groups to be equal.
#[test]
fn two_alpha_variant_recursive_groups_convert() {
    let mut kernel = kernel();
    let x = binder(90, "x");

    let countdown = |group_binder: Free, param: Free, motive: Free, pred: Free, ih: Free| {
        let body = Term::func(
            [(param.clone(), nat_type())],
            Term::nat_match(
                Term::free_var(&param),
                Some(&motive),
                nat_type(),
                nat(0),
                &pred,
                &ih,
                Term::apply(Term::free_var(&group_binder), [Term::free_var(&pred)]),
            ),
        );

        Term::rec(
            [(
                group_binder.clone(),
                Term::func_type([(param, nat_type())], nat_type()),
                body,
            )],
            Term::apply(Term::free_var(&group_binder), [Term::free_var(&x)]),
        )
    };

    let left = countdown(
        binder(0, "countdown"),
        binder(1, "n"),
        binder(2, "m"),
        binder(3, "pred"),
        binder(4, "ih"),
    );
    let right = countdown(
        binder(10, "loop"),
        binder(11, "k"),
        binder(12, "motive"),
        binder(13, "p"),
        binder(14, "rest"),
    );

    assert_ne!(
        format!("{left}"),
        String::new(),
        "the fixture should render, so a failure names real terms",
    );
    assert_eq!(convert(&mut kernel, &nat_type(), &left, &right), Ok(true));
}

/// A recursive call applied to a symbolic argument stays folded, and comparing it with itself terminates rather than unfolding in lockstep forever.
#[test]
fn a_folded_recursive_call_converts_without_unfolding_forever() {
    let mut kernel = Kernel::new(10_000);
    kernel.set_local_floor(1_000);

    let n = binder(0, "n");
    let motive = binder(1, "m");
    let pred = binder(2, "pred");
    let hypothesis = binder(3, "ih");
    let countdown = binder(4, "countdown");
    let x = binder(5, "x");

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

    let group = [(
        countdown.clone(),
        Term::func_type([(n.clone(), nat_type())], nat_type()),
        body,
    )];

    let folded = Term::rec(
        group.clone(),
        Term::apply(Term::free_var(&countdown), [Term::free_var(&x)]),
    );
    let same_shape_other_argument = Term::rec(
        group,
        Term::apply(
            Term::free_var(&countdown),
            [Term::free_var(&binder(6, "y"))],
        ),
    );

    assert_eq!(
        convert(&mut kernel, &nat_type(), &folded, &folded.clone()),
        Ok(true),
    );
    assert_eq!(
        convert(
            &mut kernel,
            &nat_type(),
            &folded,
            &same_shape_other_argument,
        ),
        Ok(false),
    );
}

/// A metavariable is elaboration-only syntax, and conversion refuses it rather than comparing ids — the exclusion is the kernel's own, not an inherited guarantee of the zonk traversal. Reflexivity is the one admitted case (the syntactic fast path, sound because it decides nothing about the unknown); any comparison that would have to *look* at a metavariable refuses.
#[test]
fn a_metavariable_does_not_convert_with_anything_else() {
    let mut kernel = kernel();
    let left = Term::metavar(MetaId::from(0usize));
    let right = Term::metavar(MetaId::from(1usize));

    assert!(matches!(
        convert(&mut kernel, &Term::type_ground(), &left, &right),
        Err(KernelError::NotCore(_)),
    ));
    assert!(matches!(
        convert(&mut kernel, &Term::type_ground(), &left, &nat(0)),
        Err(KernelError::NotCore(_)),
    ));
}
