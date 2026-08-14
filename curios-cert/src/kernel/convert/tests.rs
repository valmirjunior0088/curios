use {
    crate::{Kernel, KernelError, convert},
    curios_core::{
        Free, FuncType, Global, InductDecl, Intrinsic, Level, MetaId, Nat, StructDecl, StructType,
        Subterm, Telescope, Term, UniverseContext,
    },
    curios_utilities::{Plicity, Qualifier},
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

/// A nominal family at a stated sort — the only way to obtain a base proposition, since the registry is what says a nominal type is one.
fn declare(kernel: &mut Kernel, path: &str, result_sort: Term) -> Term {
    let name = Global::Authored(Qualifier::from([path]));

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

/// A struct literal with fewer fields than its declaration must not convert with a neutral inhabitant.
///
/// The eta walk is driven by the literal's fields, so a short literal used to run out before the declaration's telescope did and the vacuous remainder passed — equating a malformed literal with *any* neutral at the type, in the accepting direction. The walk now answers with whether it consumed the whole telescope.
#[test]
fn a_short_struct_literal_does_not_convert_with_a_neutral() {
    let mut kernel = kernel();
    let name = Global::Authored(Qualifier::from(["S"]));
    kernel.declare_struct(
        &name,
        &StructDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::build(
                [(binder(8, "a"), nat_type()), (binder(9, "b"), nat_type())],
                (),
            )),
            result_sort: Term::type_ground(),
            module: Qualifier::from(["S"]),
            rep_public: true,
            polarities: Vec::new(),
        },
    );

    let type_ = Term::from(Subterm::StructType(StructType {
        name: name.clone(),
        universes: Vec::new(),
        params: Vec::new(),
    }));
    let literal = Term::struct_(name, Vec::<Term>::new(), Vec::<Term>::new());
    let neutral = Term::free_var(&binder(0, "s"));

    assert_eq!(convert(&mut kernel, &type_, &literal, &neutral), Ok(false));
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

/// An intrinsic is congruent when it is the same operation on convertible operands — decided generically, so no operation can be omitted from the rule.
#[test]
fn an_intrinsic_is_congruent_in_its_operands() {
    let mut kernel = kernel();
    let n = binder(0, "n");
    let m = binder(1, "m");
    kernel.define(
        &m,
        &nat_type(),
        &Term::free_var(&n),
        &UniverseContext::default(),
    );

    let left = Term::intrinsic(Intrinsic::nat_mul(Term::free_var(&n), nat(3)));
    let right = Term::intrinsic(Intrinsic::nat_mul(Term::free_var(&m), nat(3)));

    assert_eq!(convert(&mut kernel, &nat_type(), &left, &right), Ok(true));
}

#[test]
fn different_operations_do_not_convert() {
    let mut kernel = kernel();
    let n = binder(0, "n");

    let add = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(3)));
    let mul = Term::intrinsic(Intrinsic::nat_mul(Term::free_var(&n), nat(3)));

    assert_eq!(convert(&mut kernel, &nat_type(), &add, &mul), Ok(false));
}

/// The free-monoid peel is what decides `n + 2 ≡ m + 2` by comparing `n` with `m` rather than comparing two opaque symbolic sums.
#[test]
fn a_shared_successor_floor_is_peeled_before_comparing() {
    let mut kernel = kernel();
    let n = binder(0, "n");
    let m = binder(1, "m");

    let left = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(2)));
    let right = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&m), nat(2)));

    // Distinct symbolic bases: the peel exposes the real disagreement.
    assert_eq!(convert(&mut kernel, &nat_type(), &left, &right), Ok(false));

    // The same base: equal after the shared floor comes off.
    let same = Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat(2)));
    assert_eq!(convert(&mut kernel, &nat_type(), &left, &same), Ok(true));
}

/// Plicity is part of a function type's identity: `(A) -> A` and `(@A) -> A` have different calling conventions, and conflating them would let a value be applied through the wrong one.
#[test]
fn plicity_distinguishes_two_function_types() {
    let mut kernel = kernel();
    let a = binder(0, "a");

    let explicit = Term::func_type([(a.clone(), nat_type())], nat_type());
    let implicit = Term::from(Subterm::FuncType(FuncType {
        telescope: match &*explicit {
            Subterm::FuncType(func) => func.telescope.clone(),
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
    let one = Term::type_at(Level::zero().succ().expect("level zero succeeds"));

    assert_eq!(convert(&mut kernel, &zero, &zero, &zero), Ok(true));
    assert_eq!(convert(&mut kernel, &zero, &zero, &one), Ok(false));
}

/// Binder *identity* must not leak into conversion. Two `rec` groups written with different minted names are the same group: binder names are display hints, and the bodies are de Bruijn-indexed under their scopes.
///
/// This is the property that lets a folded recursive call be compared structurally at all — see the projection arm, which requires the groups to be equal.
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

/// Conversion keeps the constant function apart from the identity even when the binder floor claims every name is available.
///
/// A positive control for capture-avoidance. Eta at a function type opens a binder that would alias a free local if the floor were wrong — `(x) => y` and `(x) => x` become the same term the moment the opened binder *is* `y` — so seeding at zero and colliding deliberately with what the kernel mints next is the sharpest form of the question. It does not produce a capture, and the route that would have made it reachable is now closed at the source: `recheck` derives the floor from the module's own terms rather than reading `Module::binder_floor`, which nothing checks.
#[test]
fn conversion_separates_a_constant_from_the_identity_at_a_zero_floor() {
    let colliding = {
        let mut scout = Kernel::new(100_000);
        scout.set_local_floor(0);
        scout.fresh(Some("y"))
    };

    let mut kernel = Kernel::new(100_000);
    kernel.set_local_floor(0);

    let nat = Term::intrinsic(Intrinsic::NatType);
    kernel.assume(&colliding, &nat);

    let parameter = Free::local(9_000, Some("x"));
    let constant = Term::func(
        [(parameter.clone(), nat.clone())],
        Term::free_var(&colliding),
    );
    let identity = Term::func(
        [(parameter.clone(), nat.clone())],
        Term::free_var(&parameter),
    );
    let function = Term::func_type([(parameter, nat.clone())], nat);

    assert!(
        !convert(&mut kernel, &function, &constant, &identity).expect("the comparison completes"),
        "the constant function and the identity are not convertible",
    );
}

/// The coinductive recurrence rule, which nothing in the corpus reaches.
///
/// Two folded recursive spellings can unfold forever without ever disagreeing — that is what an equirecursive type is — so `compare` keeps the goals it is already inside and *assumes* one that recurs. It is the only rule in this crate that discharges a goal without looking at either term, which is why `convert`'s module documentation calls it the place a conversion checker is most likely to be unsound.
///
/// Instrumenting `History::enter` and running the whole corpus — the fixed prelude through the kernel's own walk, plus every test program — counts 83,945 goals entered and **zero** recurrences. The two recursive-group fixtures above do not reach it either: `two_alpha_variant_recursive_groups_convert` converges because alpha-variant groups are structurally equal, and `a_folded_recursive_call_converts_without_unfolding_forever` terminates on `force` discarding an unfolding that restuck. So the rule was live code that no program could exercise, which is the condition under which a rule's mistakes stay invisible — the same shape as (V)'s argument rule sitting inert at 6010 of 6041 sites while the corpus passed throughout.
///
/// This reaches it. Both sides are the equirecursive type `X = (X) -> Nat`, spelled as *different* groups — the right carries a second, unused member — so the projection arm's syntactic comparison fails and both sides take a delta step. Unfolding poses `(left) -> Nat ≡ (right) -> Nat`, whose domain goal is the goal already in progress, and the recurrence discharges it. The two really are the same type, so accepting is correct; what is being pinned is that the rule fires here at all.
///
/// The control is [`a_recurrence_does_not_excuse_a_finite_disagreement`], the same construction with the codomains differing: the cycle closes on the domain exactly as here, and the comparison must still fail on the sibling goal. That is the whole argument the rule rests on — a genuine cycle leaves nothing but itself to check, and any finite disagreement surfaces elsewhere first — so a blanket accept would pass the witness and fail the control.
#[test]
fn a_recurring_goal_is_assumed_rather_than_unfolded_forever() {
    let mut kernel = kernel();

    assert_eq!(
        convert(
            &mut kernel,
            &Term::type_ground(),
            &equirecursive(binder(20, "a"), binder(21, "x"), nat_type(), false),
            &equirecursive(binder(30, "b"), binder(31, "y"), nat_type(), true),
        ),
        Ok(true),
        "two spellings of the equirecursive type `X = (X) -> Nat` did not converge",
    );
}

/// The control for the fixture above: the cycle closes on the domain, and the codomains still have to agree.
#[test]
fn a_recurrence_does_not_excuse_a_finite_disagreement() {
    let mut kernel = kernel();
    let other = declare(&mut kernel, "Other", Term::type_ground());

    assert_eq!(
        convert(
            &mut kernel,
            &Term::type_ground(),
            &equirecursive(binder(40, "a"), binder(41, "x"), nat_type(), false),
            &equirecursive(binder(50, "b"), binder(51, "y"), other, true),
        ),
        Ok(false),
        "the recurrence on the domain was read as settling the whole comparison",
    );
}

/// `rec m : Type = (m) -> codomain; m`, optionally carrying a second unused member so that two such groups are not structurally equal and must take a delta step to be compared.
fn equirecursive(member: Free, param: Free, codomain: Term, padded: bool) -> Term {
    let body = Term::func_type([(param, Term::free_var(&member))], codomain);
    let mut items = vec![(member.clone(), Term::type_ground(), body)];

    if padded {
        items.push((binder(99, "unused"), Term::type_ground(), nat_type()));
    }

    Term::rec(items, Term::free_var(&member))
}

/// Definitional proof irrelevance at a *computed* proposition — the shape every real firing takes, and the one this crate's copy had never been tested at.
///
/// `any_two_inhabitants_of_a_proposition_convert` above uses a nominal `Prop`-sorted family, which is the easy rung: the registry says outright that the type is a proposition. Every one of the 37 firings measured in `curios-elab` across the whole prelude is at a type that only *computes* to one — a stuck `match` at motive `Prop`, which is how `/std/BigNat` states a validity predicate over a `Bool`. Here the rule is inert, 0 firings in 86,547 goals, because a proof reaching conversion in this crate does so in an untyped child position compared at `Type`; so nothing in any program brings the rule and the shape together, and only a fixture can.
///
/// The shape is worth its own fixture because of what irrelevance trusts. It accepts *without inspecting either term*, and `Sort::of` classifies a stuck `match` by its **motive** rather than by its arms — a claim the term makes about itself, which is exactly why `check_motive` exists to type a motive under its real binders before any rule reads it. At a computed proposition the motive is therefore the whole of what this rule rests on.
///
/// The control is the identical construction with the motive at `Type` and relevant arms in both, which must *not* converge. It separates "reads the motive" from "accepts every stuck match", and without it a rule that skipped the sort test entirely would satisfy the witness.
#[test]
fn irrelevance_fires_at_a_computed_proposition() {
    let mut kernel = kernel();
    let held = declare(&mut kernel, "Held", Term::prop());
    let empty = declare(&mut kernel, "Empty", Term::prop());

    let scrutinee = binder(40, "b");
    kernel.assume(&scrutinee, &Term::intrinsic(Intrinsic::BoolType));

    let computed = Term::bool_match(Term::free_var(&scrutinee), None, Term::prop(), empty, held);

    let (left, right) = (binder(41, "p"), binder(42, "q"));
    kernel.assume(&left, &computed);
    kernel.assume(&right, &computed);

    assert_eq!(
        convert(
            &mut kernel,
            &computed,
            &Term::free_var(&left),
            &Term::free_var(&right),
        ),
        Ok(true),
        "two inhabitants of a proposition the motive computes did not convert",
    );
}

/// The control for the fixture above: the same stuck `match`, with the motive at `Type` and relevant arms, must still distinguish its inhabitants.
#[test]
fn irrelevance_does_not_fire_at_a_computed_relevant_type() {
    let mut kernel = kernel();
    let one = declare(&mut kernel, "One", Term::type_ground());
    let two = declare(&mut kernel, "Two", Term::type_ground());

    let scrutinee = binder(50, "b");
    kernel.assume(&scrutinee, &Term::intrinsic(Intrinsic::BoolType));

    let computed = Term::bool_match(
        Term::free_var(&scrutinee),
        None,
        Term::type_ground(),
        one,
        two,
    );

    let (left, right) = (binder(51, "x"), binder(52, "y"));
    kernel.assume(&left, &computed);
    kernel.assume(&right, &computed);

    assert_eq!(
        convert(
            &mut kernel,
            &computed,
            &Term::free_var(&left),
            &Term::free_var(&right),
        ),
        Ok(false),
        "irrelevance leaked into a relevant type the motive computes",
    );
}
