//! Beta, delta, zeta, iota, projection, eta and the switch arms — one reduction rule each.

use {
    super::{test_support::*, *},
    curios_core::{Apply, Free, Global, Intrinsic, Level, Reducer, Subterm, Term, UniverseParam},
    curios_utilities::{Plicity, Qualifier},
};

#[test]
fn beta_opens_a_function_over_its_arguments() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let term = Term::apply(Term::func([(x, nat_type())], Term::free_var(&x)), [nat(7)]);

    assert_eq!(whnf(&mut kernel, term), Ok(nat(7)));
}

#[test]
fn delta_unfolds_a_monomorphic_definition() {
    let mut kernel = kernel();
    let f = Global::Authored(Qualifier::from(["f"]));
    kernel
        .define(&f, &nat_type(), &nat(3), &monomorphic())
        .expect("a name is declared once");

    assert_eq!(
        whnf(&mut kernel, Term::instance(f, Universes::default())),
        Ok(nat(3))
    );
}

/// A definition generalized over universe parameters denotes no particular instance, so a reference to it stating none is a normal form. Unfolding it here would silently pick an instance nobody stated.
#[test]
fn delta_withholds_a_universe_polymorphic_definition() {
    let mut kernel = kernel();
    let f = Global::Authored(Qualifier::from(["f"]));
    kernel
        .define(&f, &nat_type(), &nat(3), &polymorphic())
        .expect("a name is declared once");

    let occurrence = Term::instance(f, Universes::default());
    assert_eq!(whnf(&mut kernel, occurrence.clone()), Ok(occurrence));
}

/// The same definition *does* unfold through a stated instance, which is the one position that names which one it is.
#[test]
fn a_universe_instance_unfolds_what_a_bare_occurrence_withholds() {
    let mut kernel = kernel();
    let f = Global::Authored(Qualifier::from(["f"]));
    kernel
        .define(&f, &nat_type(), &nat(3), &polymorphic())
        .expect("a name is declared once");

    let instance = Term::instance(f, vec![Level::zero()]);
    assert_eq!(whnf(&mut kernel, instance), Ok(nat(3)));
}

#[test]
fn an_undefined_variable_is_its_own_normal_form() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let occurrence = Term::free_var(&x);
    assert_eq!(whnf(&mut kernel, occurrence.clone()), Ok(occurrence));
}

/// Zeta. The second binding refers to the first, so this also pins the left-to-right order: `y` must see `x`'s value, not `x` itself.
#[test]
fn zeta_substitutes_let_bindings_left_to_right() {
    let mut kernel = kernel();
    let x = binder(0, "x");
    let y = binder(1, "y");

    let term = Term::let_(
        &x,
        nat_type(),
        nat(2),
        Term::let_(
            &y,
            nat_type(),
            Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), nat(3))),
            Term::free_var(&y),
        ),
    );

    assert_eq!(whnf(&mut kernel, term), Ok(nat(5)));
}

/// The intrinsic folds are shared with the elaborator through `Reducer`; this is the kernel reaching them with its own strategy underneath.
#[test]
fn intrinsics_fold_through_the_reducer_seam() {
    let mut kernel = kernel();
    let x = Global::Authored(Qualifier::from(["x"]));
    kernel
        .define(&x, &nat_type(), &nat(2), &monomorphic())
        .expect("a name is declared once");

    let term = Term::intrinsic(Intrinsic::nat_add(
        Term::instance(x, Universes::default()),
        nat(2),
    ));

    assert_eq!(whnf(&mut kernel, term), Ok(nat(4)));
}

#[test]
fn iota_selects_an_inductive_arm_and_binds_its_payload() {
    let mut kernel = kernel();
    let motive = binder(0, "m");
    let payload = binder(1, "a");

    let term = Term::induct_match(
        Term::variant(
            Global::Authored(Qualifier::from(["E"])),
            Vec::<Term>::new(),
            "some",
            [nat(42)],
        ),
        Some(&motive),
        nat_type(),
        [
            ("none", Vec::<Free>::new(), nat(0)),
            ("some", vec![payload], Term::free_var(&payload)),
        ],
    );

    assert_eq!(whnf(&mut kernel, term), Ok(nat(42)));
}

#[test]
fn iota_peels_one_successor_off_a_nat() {
    let mut kernel = kernel();
    let motive = binder(0, "m");
    let pred = binder(1, "pred");
    let hypothesis = binder(2, "ih");

    // `match 3 { 0 => 0 | succ(pred, _) => pred }` peels exactly one layer.
    let term = Term::nat_match(
        nat(3),
        Some(&motive),
        nat_type(),
        nat(0),
        &pred,
        &hypothesis,
        Term::free_var(&pred),
    );

    assert_eq!(whnf(&mut kernel, term), Ok(nat(2)));
}

#[test]
fn a_switch_takes_the_case_a_literal_names() {
    let mut kernel = kernel();
    let motive = binder(0, "m");

    let term = Term::switch(
        nat(2),
        Some(&motive),
        nat_type(),
        [(1u32, nat(10)), (2, nat(20))],
        nat(99),
    );

    assert_eq!(whnf(&mut kernel, term), Ok(nat(20)));
}

#[test]
fn a_switch_falls_through_to_its_default() {
    let mut kernel = kernel();
    let motive = binder(0, "m");

    let term = Term::switch(
        nat(7),
        Some(&motive),
        nat_type(),
        [(1u32, nat(10)), (2, nat(20))],
        nat(99),
    );

    assert_eq!(whnf(&mut kernel, term), Ok(nat(99)));
}

/// A symbolic scrutinee decides nothing, so the switch rebuilds as the neutral term it is rather than guessing an arm.
#[test]
fn a_switch_on_a_symbolic_scrutinee_stays_stuck() {
    let mut kernel = kernel();
    let motive = binder(0, "m");
    let n = binder(1, "n");

    let term = Term::switch(
        Term::free_var(&n),
        Some(&motive),
        nat_type(),
        [(1u32, nat(10))],
        nat(99),
    );

    let reduced = whnf(&mut kernel, term).expect("a stuck term still reduces");
    assert!(matches!(&*reduced, Subterm::Match(_)));
}

#[test]
fn a_bool_match_dispatches_on_a_literal() {
    let mut kernel = kernel();
    let motive = binder(0, "m");

    let term = Term::bool_match(
        Term::intrinsic(Intrinsic::Bool(true)),
        Some(&motive),
        nat_type(),
        nat(0),
        nat(1),
    );

    assert_eq!(whnf(&mut kernel, term), Ok(nat(1)));
}

#[test]
fn projection_selects_a_tuple_field() {
    let mut kernel = kernel();

    let term = Term::proj(Term::tuple([nat(10), nat(20), nat(30)]), 1);

    assert_eq!(whnf(&mut kernel, term), Ok(nat(20)));
}

/// A constructor is projected through the flat runtime view `(tag, payload...)`, so field 1 is payload component 0 — unlike a struct, which has no tag to skip.
#[test]
fn projection_skips_a_variants_tag_but_not_a_structs() {
    let mut kernel = kernel();
    let name = Global::Authored(Qualifier::from(["E"]));

    let variant = Term::proj(
        Term::variant(name, Vec::<Term>::new(), "some", [nat(42)]),
        1,
    );
    assert_eq!(whnf(&mut kernel, variant), Ok(nat(42)));

    let struct_ = Term::proj_struct(Term::struct_(name, Vec::<Term>::new(), [nat(42)]), name, 0);
    assert_eq!(whnf(&mut kernel, struct_), Ok(nat(42)));
}

#[test]
fn eta_contracts_a_function_that_only_forwards() {
    let mut kernel = kernel();
    let x = binder(0, "x");
    let f = binder(1, "f");

    let term = Term::func(
        [(x, nat_type())],
        Term::apply(Term::free_var(&f), [Term::free_var(&x)]),
    );

    assert_eq!(whnf(&mut kernel, term), Ok(Term::free_var(&f)));

    // Under the binder's own mark a forwarding contracts, and across one it stays as written: `(@x) => f(x)` is a function at `(@x: Nat) -> …`, and `f` one at another type. `f` is assumed, so no binder a later probe mints is `f` again.
    kernel.assume(&f, &Term::func_type([(x, nat_type())], nat_type()));
    let hidden = Term::func_marked(
        [(Plicity::Implicit, x, nat_type())],
        Term::apply_marked(
            Term::free_var(&f),
            [(Plicity::Implicit, Term::free_var(&x))],
        ),
    );
    assert_eq!(whnf(&mut kernel, hidden), Ok(Term::free_var(&f)));

    let across = Term::func_marked(
        [(Plicity::Implicit, x, nat_type())],
        Term::apply(Term::free_var(&f), [Term::free_var(&x)]),
    );
    assert_eq!(whnf(&mut kernel, across.clone()), Ok(across));
}

/// The side condition is load-bearing: contracting `(x) => x(x)` would move an occurrence of `x` out from under the binder that gives it meaning.
#[test]
fn eta_declines_when_the_head_mentions_the_binder() {
    let mut kernel = kernel();
    let x = binder(0, "x");

    let term = Term::func(
        [(x, nat_type())],
        Term::apply(Term::free_var(&x), [Term::free_var(&x)]),
    );

    assert_eq!(whnf(&mut kernel, term.clone()), Ok(term));
}

/// Eta hands back the head as the occurrence spells it, and a reference to a declaration is its instance: `(x) => f@{3}(x)` contracts to `f@{3}`, which unfolds at level 3, where the bare `f` the contraction must not invent stays stuck.
///
/// Asked of an `Instance` head because that is the shape every reference to a declaration takes ([`curios_core::Instance`]): the node carries the levels and has no term child, so the probe's side condition that the head mentions none of the binders is vacuous on it, and the per-argument identity check is the whole of what decides. A contraction that handed back the declaration without its levels would hand every later reader a reference to no instance of the scheme — the term `delta_withholds_a_universe_polymorphic_definition` holds stuck — and one that read the levels off the wrong occurrence would be a false equation between two instances.
///
/// Across a mark it declines as it does for a local head: `(@x) => f@{3}(x)` is a function at `(@x : Nat) -> …` and `f@{3}` one at a type plicity holds apart.
///
/// Asked of both statements of the rule, which must agree: the forwarding whose domain names an assumed local is one the closed machine's gate declines, so `step_func` decides it, and the local-free one is the machine's own `curios_core::eta_head`.
///
/// Mutation-checked, in the open case: with `step_func` handing back `Term::instance(name, Universes::default())` for an `Instance` head, the open forwarding reduces to the stuck bare occurrence instead of to `Type 3`.
#[test]
fn eta_contracts_to_the_instance_the_occurrence_states() {
    let mut kernel = kernel();
    let (x, y) = (binder(0, "x"), binder(1, "y"));
    kernel.assume(&y, &nat_type());
    let f = Global::Authored(Qualifier::from(["f"]));
    kernel
        .define(
            &f,
            &nat_type(),
            &Term::type_at(Level::param(UniverseParam(0))),
            &polymorphic(),
        )
        .expect("a name is declared once");
    let at = |level: u32| Term::instance(f, vec![Level::constant(level)]);
    // The domain carries the assumed local in the open case, which is what the machine's gate reads, and plays no part in the probe.
    let forwarding = |head: Term, domain: Term| {
        Term::func([(x, domain)], Term::apply(head, [Term::free_var(&x)]))
    };
    let open = || Term::intrinsic(Intrinsic::nat_add(Term::free_var(&y), nat(1)));

    for (label, domain) in [("open", open()), ("local-free", nat_type())] {
        assert_eq!(
            whnf(&mut kernel, forwarding(at(3), domain.clone())),
            Ok(Term::type_at(Level::constant(3))),
            "{label}: the contraction did not keep the instance's levels",
        );
        assert_eq!(
            whnf(&mut kernel, forwarding(at(1), domain.clone())),
            Ok(Term::type_at(Level::constant(1))),
            "{label}: the contraction read another occurrence's levels",
        );

        let bare = Term::instance(f, Universes::default());
        assert_eq!(
            whnf(&mut kernel, forwarding(bare.clone(), domain.clone())),
            Ok(bare),
            "{label}: the contraction invented an instance the occurrence does not state",
        );

        let across = Term::func_marked(
            [(Plicity::Implicit, x, domain)],
            Term::apply(at(3), [Term::free_var(&x)]),
        );
        assert_eq!(
            whnf(&mut kernel, across.clone()),
            Ok(across),
            "{label}: a forwarding across a mark contracted",
        );
    }
}

/// A recursive call keeps its folded spelling until an eliminator demands the value, which is what stops an occurrence from unfolding forever.
#[test]
fn a_recursive_application_stays_folded_until_forced() {
    let mut kernel = kernel();
    let n = binder(0, "n");
    let motive = binder(1, "m");
    let pred = binder(2, "pred");
    let hypothesis = binder(3, "ih");
    let countdown = binder(4, "countdown");
    let x = binder(5, "x");

    let body = Term::func(
        [(n, nat_type())],
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
        countdown,
        Term::func_type([(n, nat_type())], nat_type()),
        body,
    )];

    // Applied to a symbolic argument, it reduces to a folded recursive call.
    let symbolic = Term::rec(
        group.clone(),
        Term::apply(Term::free_var(&countdown), [Term::free_var(&x)]),
    );
    let Subterm::Rec(rec) = Term::unwrap_or_clone(symbolic) else {
        unreachable!("built as a rec")
    };
    let reduced = whnf(&mut kernel, unfold_rec(rec)).expect("ordinary reduction terminates");
    assert!(matches!(
        &*reduced,
        Subterm::Apply(Apply { head, .. }) if head.as_rec_proj().is_some()
    ));

    // Applied to a literal, forcing runs it to the end.
    let concrete = Term::rec(group, Term::apply(Term::free_var(&countdown), [nat(3)]));
    assert_eq!(kernel.reduce_forced(concrete), Ok(nat(0)));
}

/// A member whose result is a function, forced where it is applied past its own parameters: `f(2)(y)` is the call `f(2)` applied to `y`, so the force reaches through the outer application to the call, and `y`, symbolic, rides along to the answer. Read one level deep, the outer application would be a neutral the force hands back folded.
#[test]
fn a_recursive_call_applied_past_its_parameters_unfolds_when_forced() {
    let mut kernel = kernel();
    let n = binder(0, "n");
    let motive = binder(1, "m");
    let pred = binder(2, "pred");
    let hypothesis = binder(3, "ih");
    let f = binder(4, "f");
    let x = binder(5, "x");
    let y = binder(6, "y");
    let arrow = Term::func_type([(x, nat_type())], nat_type());

    let body = Term::func(
        [(n, nat_type())],
        Term::nat_match(
            Term::free_var(&n),
            Some(&motive),
            arrow.clone(),
            Term::func([(x, nat_type())], Term::free_var(&x)),
            &pred,
            &hypothesis,
            Term::func(
                [(x, nat_type())],
                Term::apply(
                    Term::apply(Term::free_var(&f), [Term::free_var(&pred)]),
                    [Term::free_var(&x)],
                ),
            ),
        ),
    );

    let term = Term::rec(
        [(f, Term::func_type([(n, nat_type())], arrow), body)],
        Term::apply(
            Term::apply(Term::free_var(&f), [nat(2)]),
            [Term::free_var(&y)],
        ),
    );

    assert_eq!(kernel.reduce_forced(term), Ok(Term::free_var(&y)));
}

/// A boolean operation reduces its right operand only once its left is a literal. The left here is a local, so the right — a fold that would answer `true` — is handed back as written; with the left `true`, the same right folds and so does the whole. This is the rule that keeps weak-head reduction of a `&&`/`||` tree from being its full normalization, which on a web of predicate definitions naming each other twice would be `2^n` under every demand — see `reduce_bool_binary`.
#[test]
fn a_stuck_left_operand_leaves_the_right_as_written() {
    let mut kernel = kernel();
    let x = binder(0, "x");
    kernel.assume(&x, &Term::intrinsic(Intrinsic::BoolType));
    let literal = |value: bool| Term::intrinsic(Intrinsic::Bool(value));
    let folds = Term::intrinsic(Intrinsic::BoolAnd(literal(true), literal(true)));

    let stuck = Term::intrinsic(Intrinsic::BoolAnd(Term::free_var(&x), folds.clone()));
    assert_eq!(
        whnf(&mut kernel, stuck.clone()),
        Ok(stuck),
        "a stuck left settles the fold, and the right is not reduced for an answer it cannot change"
    );

    let open = Term::intrinsic(Intrinsic::BoolAnd(literal(true), folds));
    assert_eq!(
        whnf(&mut kernel, open),
        Ok(literal(true)),
        "a literal left reads the right and folds"
    );
}

/// The line the rule above draws: an equality reads its right operand under a stuck left, because its laws read it — `x == true` is `x` — where a connective's laws would only meet a leaf the tree flattening forces later. No predicate web is built out of equalities, so the cliff the connectives step back from is not here.
#[test]
fn an_equality_reads_its_right_operand() {
    let mut kernel = kernel();
    let x = binder(0, "x");
    kernel.assume(&x, &Term::intrinsic(Intrinsic::BoolType));
    let literal = |value: bool| Term::intrinsic(Intrinsic::Bool(value));
    let folds = Term::intrinsic(Intrinsic::BoolAnd(literal(true), literal(true)));

    let equality = Term::intrinsic(Intrinsic::BoolEql(Term::free_var(&x), folds));
    assert_eq!(
        whnf(&mut kernel, equality),
        Ok(Term::free_var(&x)),
        "the right folds to `true`, and `x == true` is `x`"
    );
}

#[test]
fn an_application_whose_group_dissolved_to_its_member_still_unfolds() {
    let mut kernel = kernel();
    let n = binder(0, "n");
    let unused = binder(1, "unused");
    let value = binder(2, "value");
    let identity = Term::func([(n, nat_type())], Term::free_var(&n));

    // The elaborator's twin, put to the kernel: a group whose member never mentions itself has no fixed point to keep, so opening its tail reduces past the projection and leaves the member's own `Func`. Declining every head that is not still a projection would keep the folded spelling with that `Func` in hand, and the two checkers would read one declaration two ways.
    let term: Term = Term::apply(
        Term::rec(
            [(
                unused,
                Term::func_type([(n, nat_type())], nat_type()),
                identity.clone(),
            )],
            identity,
        ),
        [Term::free_var(&value)],
    );
    let Subterm::Apply(apply) = Term::unwrap_or_clone(term) else {
        unreachable!()
    };

    assert_eq!(
        unfold_rec_apply(&mut kernel, apply),
        Ok(Some(Term::free_var(&value)))
    );
}
