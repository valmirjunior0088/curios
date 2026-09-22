//! Collecting and reaching over a term: free variables, metavariables, and the sharing a walk must visit once.

use {
    crate::*,
    curios_utilities::{Plicity, Qualifier},
    std::{collections::BTreeSet, rc::Rc},
};

use super::test_support::*;

#[test]
fn collect_ignores_index_names() {
    let x = Free::local(0, Some("x"));
    let y = Free::local(1, Some("y"));
    let z = Free::local(2, Some("z"));
    let w = Free::local(3, Some("w"));
    let term = Term::func(
        [(x.clone(), Term::type_ground())],
        Term::tuple([
            Term::free_var(&x),
            Term::rec(
                vec![(y.clone(), Term::type_ground(), Term::free_var(&z))],
                Term::tuple([Term::free_var(&y), Term::free_var(&w)]),
            ),
        ]),
    );

    assert_eq!(term.free_vars(), BTreeSet::from([w.clone(), z.clone()]));
}

/// A chain-shaped term shares its single carrier child's memoized set upward instead of copying it once per link.
#[test]
fn free_vars_share_the_single_carrier_child_allocation() {
    let x = Free::local(0, Some("x"));
    let chain = Term::proj(Term::proj(Term::free_var(&x), 0), 0);
    chain.free_vars();

    let Subterm::Proj(outer) = chain.as_ref() else {
        panic!("chain changed shape");
    };
    let Subterm::Proj(middle) = outer.head.as_ref() else {
        panic!("chain changed shape");
    };
    assert!(Rc::ptr_eq(
        chain.inner.frees.get().unwrap(),
        middle.head.inner.frees.get().unwrap(),
    ));
}

#[test]
fn metavar_is_a_closed_global_head() {
    let m = Term::hole(7);
    assert_eq!(m.reach(), 0);
    assert!(m.closed());
    assert_eq!(format!("{m}"), "?7");
}

#[test]
fn metavars_collects_ids_across_structure() {
    let x = Free::local(0, Some("x"));
    // (λx. ?1)(?2, Nat.add ?3 ?1)
    let term = Term::apply(
        Term::func([(x.clone(), Term::type_ground())], Term::hole(1)),
        [
            Term::hole(2),
            Term::intrinsic(Intrinsic::nat_add(Term::hole(3), Term::hole(1))),
        ],
    );
    assert_eq!(term.metavars(), BTreeSet::from([1, 2, 3].map(MetavarId)));
}

#[test]
fn any_metavar_short_circuits_and_agrees_with_collection() {
    let x = Free::local(0, Some("x"));
    // (λx. ?1)(?2, Nat.add ?3 ?1)
    let term = Term::apply(
        Term::func([(x.clone(), Term::type_ground())], Term::hole(1)),
        [
            Term::hole(2),
            Term::intrinsic(Intrinsic::nat_add(Term::hole(3), Term::hole(1))),
        ],
    );

    // A predicate over the ids agrees with the collecting walk: a present id is found, an absent one is not.
    assert!(term.any_metavar(&mut |id| id == MetavarId(3)));
    assert!(!term.any_metavar(&mut |id| id == MetavarId(99)));
    assert_eq!(term.any_metavar(&mut |_| true), !term.metavars().is_empty());

    // Bails on the first metavariable: the head's `?1` is reached first, so an accept-anything predicate runs exactly once instead of visiting all four.
    let mut visits = 0;
    assert!(term.any_metavar(&mut |_| {
        visits += 1;
        true
    }));
    assert_eq!(visits, 1);

    // A metavariable-free term never fires the predicate.
    let plain = Term::func([(x.clone(), Term::type_ground())], Term::free_var(&x));
    let mut fired = false;
    assert!(!plain.any_metavar(&mut |_| {
        fired = true;
        true
    }));
    assert!(!fired);
}

// The elaboration-runaway pin: a metavariable below a shared node defeats the `has_metavar` prune on every ancestor, so without the walk's visited set this term — 64 levels of self-application over a metavar, a 2^64-node tree expansion of a 65-node DAG — cannot be walked at all. Terminating is the assertion; the exact id set and the single predicate firing pin that dedup skips revisits, not nodes.
#[test]
fn any_metavar_visits_a_shared_subterm_once() {
    let mut term = Term::hole(1);
    for _ in 0..64 {
        term = Term::apply(term.clone(), [term]);
    }

    assert_eq!(term.metavars(), BTreeSet::from([MetavarId(1)]));

    let mut visits = 0;
    assert!(!term.any_metavar(&mut |_| {
        visits += 1;
        false
    }));
    assert_eq!(visits, 1);
}

#[test]
fn deep_terms_are_searched_without_native_recursion() {
    // The shape neither prune stops: `has_metavar` is set on every ancestor of a hole, so a spine this tall is descended in full. Both walks used to do that natively at five debug frames per link and die as a bare `SIGSEGV` — *below* the depth at which the reduction budget refuses, so the same program crashed at eight thousand links and reported cleanly at sixty thousand. A regression here is a stack overflow rather than a failure.
    let argument = Term::free_var(&Free::local(0, None));
    let mut term = Term::hole(1);
    for _ in 0..DEEP {
        term = Term::apply(term, [argument.clone()]);
    }

    // The hole at the base is found, and an absent id walks the whole spine to say so.
    assert!(term.any_metavar(&mut |id| id == MetavarId(1)));
    assert!(!term.any_metavar(&mut |id| id == MetavarId(99)));

    // Its sibling, which the kernel's motive check reaches: the needle is found under the spine, and one that never occurs is refused after walking it.
    assert!(term.mentions_term(&argument));
    assert!(!term.mentions_term(&Term::free_var(&Free::local(1, None))));
}

#[test]
fn has_local_free_flags_locals_not_globals() {
    let binder_0 = Free::local(0, Some("/std/Str/step"));
    let binder_1 = Free::local(1, Some("c#1"));
    let binder_2 = Free::local(2, Some("x#9"));
    fn global(path: [&str; 2]) -> Term {
        Term::free_var(&Free::global(Qualifier::from(path)))
    }

    // A local is a discriminant, not a spelling: what makes a free variable context-dependent — the only kind that can invalidate an elaboration-cache entry across frames — is that a scope opened it.
    assert!(Term::free_var(&Free::local(3, Some("x"))).has_local_free());
    assert!(!global(["std", "Nat"]).has_local_free());

    // A compiler-generated *global* is not context dependent and must not set the bit. This used to be a search for a marker character, so a witness that spelled itself `witness#N` misfired on every term mentioning one, silently disabling three elaboration caches. No spelling can do that now.
    assert!(
        !Term::free_var(&Free::Global(Global::Witness(WitnessId::new(
            Qualifier::from(["std"]),
            0,
        ))))
        .has_local_free()
    );
    assert!(!global(["std", "Nat"]).has_local_free());

    // The bit is structural: a minted name anywhere in the tree sets it.
    let inner = Term::apply(Term::free_var(&binder_0), [Term::free_var(&binder_1)]);
    assert!(inner.has_local_free());

    // A binder whose label hint carries `#` stays clean: the hint is not an occurrence, and the captured variable is bound, not free.
    let binder = Term::func(
        [(binder_2.clone(), Term::type_ground())],
        Term::free_var(&binder_2),
    );
    assert!(!binder.has_local_free());
}

#[test]
fn has_metavar_flags_any_metavariable_node() {
    let x = Free::local(0, Some("x"));
    assert!(Term::hole(1).has_metavar());
    assert!(
        Term::apply(
            Term::func([(x.clone(), Term::type_ground())], Term::free_var(&x)),
            [Term::hole(2)],
        )
        .has_metavar()
    );
    assert!(!Term::func([(x.clone(), Term::type_ground())], Term::free_var(&x)).has_metavar());
}

#[test]
fn metavar_is_inert_under_traversal() {
    let x = Free::local(0, Some("x"));
    let y = Free::local(1, Some("y"));
    // shifting/capture must not disturb a metavariable node
    let m = Term::hole(4);
    assert_eq!(m.shift(3), m);
    let scope = Scope::close(One, &[&x], Term::hole(4));
    assert_eq!(scope.open(&[&Term::free_var(&y)]), Term::hole(4));
}

#[test]
fn variant_collects_metavars_and_prints_as_function_call() {
    let ctor = Term::variant(
        nominal("Result"),
        [Term::hole(1)],
        "success",
        [Term::hole(2)],
    );
    assert_eq!(ctor.metavars(), BTreeSet::from([1, 2].map(MetavarId)));
    assert_eq!(format!("{ctor}"), "/Result/success(?2)");

    let type_ = Term::induct_type(
        nominal("Result"),
        [Term::intrinsic(Intrinsic::NatType), Term::hole(3)],
        Vec::<Term>::new(),
    );
    assert_eq!(type_.metavars(), BTreeSet::from([3].map(MetavarId)));
    assert_eq!(format!("{type_}"), "/Result(Nat, ?3)");
}

#[test]
fn inductive_variants_reach_spans_components() {
    assert_eq!(
        Term::induct_type(
            nominal("Result"),
            [Term::var(Var::bound(2))],
            Vec::<Term>::new()
        )
        .reach(),
        3
    );
    assert_eq!(
        Term::variant(
            nominal("Result"),
            [Term::var(Var::bound(0))],
            "success",
            [Term::var(Var::bound(4))],
        )
        .reach(),
        5
    );
}

#[test]
fn reach_basic_values() {
    let x = Free::local(0, Some("x"));
    assert_eq!(Term::type_ground().reach(), 0);
    assert_eq!(Term::free_var(&x).reach(), 0);
    assert_eq!(Term::var(Var::bound(0)).reach(), 1);
    assert_eq!(Term::var(Var::bound(3)).reach(), 4);
    // closed identity function λx.x
    assert_eq!(
        Term::func([(x.clone(), Term::type_ground())], Term::free_var(&x)).reach(),
        0
    );
}

#[test]
fn reach_telescope_absorbs_arity() {
    // body references bound index 2 (reach 3); each telescope binder absorbs one. `Scope::constant` places the body without capturing, so the bound index is preserved exactly (unlike `Telescope::build`, which captures by label).
    let f1 = Term::from(Subterm::Func(Func {
        telescope: Telescope::Cons(
            Term::type_ground(),
            Scope::constant(One, Telescope::done(Term::var(Var::bound(2)))),
        ),
        plicities: vec![Plicity::Explicit],
    }));
    assert_eq!(f1.reach(), 2); // one binder: (2 + 1) - 1

    let f2 = Term::from(Subterm::Func(Func {
        telescope: Telescope::Cons(
            Term::type_ground(),
            Scope::constant(
                One,
                Telescope::Cons(
                    Term::type_ground(),
                    Scope::constant(One, Telescope::done(Term::var(Var::bound(2)))),
                ),
            ),
        ),
        plicities: vec![Plicity::Explicit, Plicity::Explicit],
    }));
    assert_eq!(f2.reach(), 1); // two binders: (2 + 1) - 2
}

/// Every `Type` is a position the stripped skeleton records, a ground `Type 0` included: `(Type 0, Type u)` and `(Type u, Type 0)` strip to one skeleton over two two-entry vectors that pair zero with `u` and `u` with zero, where a universes-only walk would have skipped the `Type 0` and paired `u` with itself. Two terms differing only in levels strip to one skeleton with their vectors aligned.
#[test]
fn stripping_levels_records_a_ground_sort_as_a_position() {
    let u = Term::type_at(Level::param(UniverseParam(0)));
    let v = Term::type_at(Level::param(UniverseParam(1)));
    let ground = Term::type_at(Level::zero());

    let (ground_first, ground_first_levels) =
        strip_universe_levels(&Term::tuple([ground.clone(), u.clone()]));
    let (ground_second, ground_second_levels) =
        strip_universe_levels(&Term::tuple([u.clone(), ground]));
    assert_eq!(ground_first, ground_second);
    assert_eq!(
        ground_first_levels,
        vec![(0, Level::zero()), (0, Level::param(UniverseParam(0)))]
    );
    assert_eq!(
        ground_second_levels,
        vec![(0, Level::param(UniverseParam(0))), (0, Level::zero())]
    );

    let (at_u, u_levels) =
        strip_universe_levels(&Term::tuple([u, Term::intrinsic(Intrinsic::NatType)]));
    let (at_v, v_levels) =
        strip_universe_levels(&Term::tuple([v, Term::intrinsic(Intrinsic::NatType)]));
    assert_eq!(at_u, at_v);
    assert_eq!(u_levels, vec![(0, Level::param(UniverseParam(0)))]);
    assert_eq!(v_levels, vec![(0, Level::param(UniverseParam(1)))]);
}

/// Equality up to a wildcard ignores exactly the wildcard positions: a hole against anything is a match, and everything else still has to agree.
#[test]
fn equality_up_to_a_wildcard_ignores_only_the_wildcard_positions() {
    let nat = Term::intrinsic(Intrinsic::NatType);
    let bool_ = Term::intrinsic(Intrinsic::BoolType);
    let is_hole = |term: &Term| matches!(&**term, Subterm::Metavar(_));

    assert!(
        Term::tuple([Term::hole(0), nat.clone()])
            .equal_up_to(&Term::tuple([nat.clone(), nat.clone()]), is_hole),
        "a hole did not stand for the term in its position",
    );
    assert!(
        !Term::tuple([Term::hole(0), nat.clone()])
            .equal_up_to(&Term::tuple([nat.clone(), bool_.clone()]), is_hole),
        "a difference outside the wildcard was ignored",
    );
    assert!(
        !Term::tuple([nat.clone(), nat.clone()]).equal_up_to(&Term::tuple([nat, bool_]), |_| false),
        "with no wildcards the comparison stopped being structural equality",
    );
}
