use {
    crate::*,
    curios_utilities::{Plicity, Qualifier},
    std::{
        collections::{BTreeSet, HashSet, hash_map::RandomState},
        hash::BuildHasher,
        rc::Rc,
    },
};

/// A declaration's name, from the path a test writes. Fixture-only.
fn nominal(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

#[cfg(feature = "archive")]
#[test]
fn archive_resets_caches_and_preserves_rc_sharing() {
    let shared_binder = Free::local(0, Some("shared"));
    let shared = Term::free_var(&shared_binder);
    let term = Term::tuple([shared.clone(), shared]);
    term.get_or_init_hash();
    term.reach();
    term.free_vars();
    term.has_local_free();
    term.has_metavar();
    assert!(term.inner.scalars.is_filled());
    assert!(term.inner.frees.is_filled());

    let bytes =
        curios_archive::rkyv::to_bytes::<curios_archive::rkyv::rancor::Error>(&term).unwrap();
    let restored =
        curios_archive::rkyv::from_bytes::<Term, curios_archive::rkyv::rancor::Error>(&bytes)
            .unwrap();
    assert!(!restored.inner.scalars.is_filled());
    assert!(!restored.inner.frees.is_filled());

    let Subterm::Tuple(tuple) = restored.as_ref() else {
        panic!("restored term changed shape");
    };
    assert!(Rc::ptr_eq(&tuple.fields[0].inner, &tuple.fields[1].inner));
}

#[test]
fn close_open_substitutes_label_name() {
    let x = Free::local(0, Some("x"));
    let y = Free::local(1, Some("y"));
    let term = Scope::close(One, &[&x], Term::free_var(&x)).open(&[&Term::free_var(&y)]);

    let Subterm::Var(var) = &*term else {
        panic!("unexpected `{term:?}`")
    };

    assert_eq!(var, &Var::free(y.clone()));
}

#[test]
fn close_open_preserves_nested_bind() {
    let x = Free::local(0, Some("x"));
    let y = Free::local(1, Some("y"));
    let z = Free::local(2, Some("z"));
    let w = Free::local(3, Some("w"));
    let term = Scope::close(
        One,
        &[&x],
        Term::func([(y.clone(), Term::type_ground())], Term::free_var(&x)),
    )
    .open(&[&Term::free_var(&z)]);

    let Subterm::Func(body) = &*term else {
        panic!("unexpected `{term:?}`")
    };

    let opened = body.telescope.open(&[&Term::free_var(&w)]);
    let Subterm::Var(var) = &*opened else {
        panic!("unexpected term")
    };

    assert_eq!(var, &Var::free(z.clone()));
}

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
    let m = Term::metavar(7);
    assert_eq!(m.reach(), 0);
    assert!(m.closed());
    assert_eq!(format!("{m}"), "?7");
}

#[test]
fn metavars_collects_ids_across_structure() {
    let x = Free::local(0, Some("x"));
    // (λx. ?1)(?2, Nat.add ?3 ?1)
    let term = Term::apply(
        Term::func([(x.clone(), Term::type_ground())], Term::metavar(1)),
        [
            Term::metavar(2),
            Term::intrinsic(Intrinsic::nat_add(Term::metavar(3), Term::metavar(1))),
        ],
    );
    assert_eq!(term.metavars(), BTreeSet::from([1, 2, 3].map(MetaId)));
}

#[test]
fn any_metavar_short_circuits_and_agrees_with_collection() {
    let x = Free::local(0, Some("x"));
    // (λx. ?1)(?2, Nat.add ?3 ?1)
    let term = Term::apply(
        Term::func([(x.clone(), Term::type_ground())], Term::metavar(1)),
        [
            Term::metavar(2),
            Term::intrinsic(Intrinsic::nat_add(Term::metavar(3), Term::metavar(1))),
        ],
    );

    // A predicate over the ids agrees with the collecting walk: a present id is found, an absent one is not.
    assert!(term.any_metavar(&mut |id| id == MetaId(3)));
    assert!(!term.any_metavar(&mut |id| id == MetaId(99)));
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

#[test]
fn has_local_free_flags_locals_not_globals() {
    let binder_0 = Free::local(0, Some("/syn/Str/step"));
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
    assert!(Term::metavar(1).has_metavar());
    assert!(
        Term::apply(
            Term::func([(x.clone(), Term::type_ground())], Term::free_var(&x)),
            [Term::metavar(2)],
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
    let m = Term::metavar(4);
    assert_eq!(m.shift(3), m);
    let scope = Scope::close(One, &[&x], Term::metavar(4));
    assert_eq!(scope.open(&[&Term::free_var(&y)]), Term::metavar(4));
}

#[test]
fn variant_collects_metavars_and_prints_as_function_call() {
    let ctor = Term::variant(
        nominal("Result"),
        [Term::metavar(1)],
        "success",
        [Term::metavar(2)],
    );
    assert_eq!(ctor.metavars(), BTreeSet::from([1, 2].map(MetaId)));
    assert_eq!(format!("{ctor}"), "/Result/success(?2)");

    let type_ = Term::induct_type(
        nominal("Result"),
        [Term::intrinsic(Intrinsic::NatType), Term::metavar(3)],
        Vec::<Term>::new(),
    );
    assert_eq!(type_.metavars(), BTreeSet::from([3].map(MetaId)));
    assert_eq!(format!("{type_}"), "/Result(Nat, ?3)");
}

#[test]
fn implicit_marks_print_and_default_to_explicit() {
    let binder_0 = Free::local(0, Some("T"));
    let x = Free::local(1, Some("x"));
    let foo = Free::local(2, Some("foo"));
    let binder_3 = Free::local(3, Some("Nat"));
    let ft = Term::func_type_marked(
        [
            (Plicity::Implicit, binder_0.clone(), Term::type_ground()),
            (Plicity::Explicit, x.clone(), Term::free_var(&binder_0)),
        ],
        Term::free_var(&binder_0),
    );
    assert_eq!(format!("{ft}"), "(@T: Type, x: T) -> T");

    // The unmarked builders default every slot to `Explicit`.
    let plain = Term::func_type(
        [(binder_0.clone(), Term::type_ground())],
        Term::type_ground(),
    );
    match &*plain {
        Subterm::FuncType(FuncType { plicities, .. }) => {
            assert_eq!(plicities, &[Plicity::Explicit]);
        }
        _ => unreachable!(),
    }

    let call = Term::apply_marked(
        Term::free_var(&foo),
        [
            (Plicity::Implicit, Term::free_var(&binder_3)),
            (Plicity::Explicit, Term::free_var(&x)),
        ],
    );
    assert_eq!(format!("{call}"), "foo(@Nat, x)");
}

#[test]
fn inductive_match_case_binders_are_captured() {
    let r = Free::local(0, Some("r"));
    let value = Free::local(1, Some("value"));
    // match r : #m => Type; | success(value) => value;
    let term = Term::induct_match(
        Term::free_var(&r),
        None,
        Term::type_ground(),
        [("success", vec![value.clone()], Term::free_var(&value))],
    );

    let free = term.free_vars();
    assert!(free.contains(&r));
    assert!(!free.contains(&value));
}

#[test]
fn inductive_match_default_prints_a_catch_all_arm() {
    let r = Free::local(0, Some("r"));
    let a = Free::local(1, Some("a"));
    let b = Free::local(2, Some("b"));
    // The catch-all renders as a trailing `| _ =>` arm, after the enumerated constructors — mirroring `Cases::Switch`'s default.
    let term = Term::induct_match_default(
        Term::free_var(&r),
        None,
        Term::type_ground(),
        [("none", Vec::<Free>::new(), Term::free_var(&a))],
        Term::free_var(&b),
    );

    let printed = term.to_string();
    assert!(
        printed.contains("| 'none =>") && printed.contains("| _ =>"),
        "expected an enumerated arm and a catch-all, got:\n{printed}"
    );
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
    // body references bound index 2 (reach 3); each telescope binder absorbs one. `Scope::constant` places the body without capturing, so the bound index is preserved exactly (unlike `Telescope::cons`, which captures by label).
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

#[test]
fn open_shares_closed_body_without_rebuild() {
    let x = Free::local(0, Some("x"));
    let y = Free::local(1, Some("y"));
    // body does not mention the bound variable -> open returns the stored Rc unchanged
    let scope = Scope::close(One, &[&x], Term::type_ground());
    let opened = scope.open(&[&Term::free_var(&y)]);
    assert!(Rc::ptr_eq(&opened.inner, &scope.body().inner));
}

#[test]
fn open_shares_closed_subterm_inside_substituted_body() {
    let a = Free::local(0, Some("a"));
    let x = Free::local(1, Some("x"));
    let y = Free::local(2, Some("y"));
    let closed = Term::func([(a.clone(), Term::type_ground())], Term::free_var(&a)); // λa.a, closed
    let scope = Scope::close(One, &[&x], Term::tuple([Term::free_var(&x), closed]));

    let stored_field = match &**scope.body() {
        Subterm::Tuple(Tuple { fields, .. }) => fields[1].clone(),
        _ => panic!("expected tuple body"),
    };

    let opened = scope.open(&[&Term::free_var(&y)]);

    let opened_field = match &*opened {
        Subterm::Tuple(Tuple { fields, .. }) => fields[1].clone(),
        _ => panic!("expected tuple result"),
    };

    // the substituted field changed; the closed field is shared, not rebuilt
    assert_eq!(opened_field, stored_field);
    assert!(Rc::ptr_eq(&opened_field.inner, &stored_field.inner));
}

#[test]
fn binder_name_hints_are_identity_irrelevant() {
    let x = Free::local(0, Some("x"));
    let y = Free::local(1, Some("y"));

    let this = Term::func([(x.clone(), Term::type_ground())], Term::free_var(&x));
    let that = Term::func([(y.clone(), Term::type_ground())], Term::free_var(&y));

    assert_eq!(this, that);

    let state = RandomState::new();
    assert_eq!(state.hash_one(&this), state.hash_one(&that));
}

#[test]
fn tuple_type_field_labels_are_identity() {
    let cp = Free::local(0, Some("cp"));
    let v = Free::local(1, Some("v"));
    let r = Free::local(2, Some("r"));
    // Field labels are the target of `.label` resolution, so unlike binder hints they split identity: an α-equal twin with different labels must not be substituted for this type by any Eq-keyed cache.
    let this = Term::tuple_type([
        (cp.clone(), Term::type_ground()),
        (v.clone(), Term::free_var(&cp)),
    ]);
    let that = Term::tuple_type([
        (r.clone(), Term::type_ground()),
        (v.clone(), Term::free_var(&r)),
    ]);

    assert_ne!(this, that);
    assert_eq!(
        this,
        Term::tuple_type([
            (cp.clone(), Term::type_ground()),
            (v.clone(), Term::free_var(&cp))
        ])
    );
}

/// The number of distinct `Node`s reachable from `term`, counting a shared node once. Inlined here because only these tests ask the question.
fn distinct_nodes(term: &Term) -> usize {
    let mut seen = HashSet::new();
    let mut stack = Vec::from([term.clone()]);
    while let Some(node) = stack.pop() {
        if !seen.insert(Rc::as_ptr(&node.inner)) {
            continue;
        }
        node.as_ref().any_child_term(&mut |child| {
            stack.push(child.clone());
            false
        });
    }
    seen.len()
}

#[test]
fn a_memoized_rewrite_keeps_a_shared_subterm_shared() {
    let f = Free::local(0, Some("f"));
    let x = Free::local(1, Some("x"));
    let shared = Term::apply(Term::free_var(&f), [Term::free_var(&x)]);
    let term = Term::tuple([shared.clone(), shared]);

    let rewritten: Term = term.traverse(&mut Visit::rewriting_shared(
        |_, _| None,
        Box::new(|_, _| None),
    ));

    let Subterm::Tuple(tuple) = rewritten.as_ref() else {
        panic!("the rewrite changed the term's shape");
    };
    assert!(
        Rc::ptr_eq(&tuple.fields[0].inner, &tuple.fields[1].inner),
        "a memoized rewrite split one shared subterm into two nodes"
    );
}

/// A rewrite that rebuilds a shared node once per *occurrence* rather than once per node turns a DAG into its expansion. This is the shape that made it matter: a string literal lowers to a chain threading a scan state, where every link mentions the previous state, so the term is linear in distinct nodes but triangular expanded. Losing the memo here cost O(n^2) nodes for an n-byte literal, and every later pass over the term inherited it.
#[test]
fn a_memoized_rewrite_keeps_a_shared_chain_linear() {
    let lead = Free::local(0, Some("lead"));
    let stop = Free::local(1, Some("stop"));
    let step = Free::local(2, Some("step"));
    let depth = 200;
    let mut state = Term::free_var(&lead);
    let mut chain = Term::free_var(&stop);
    for _ in 0..depth {
        chain = Term::tuple([state.clone(), chain]);
        state = Term::apply(Term::free_var(&step), [state]);
    }

    assert!(
        distinct_nodes(&chain) < 4 * depth,
        "the fixture itself is not shared, so the test proves nothing"
    );

    let rewritten: Term = chain.traverse(&mut Visit::rewriting_shared(
        |_, _| None,
        Box::new(|_, _| None),
    ));

    assert_eq!(
        distinct_nodes(&rewritten),
        distinct_nodes(&chain),
        "a memoized rewrite expanded the shared chain"
    );
}

/// Deeper than the ~50,000 steps at which a growing conversion used to abort the process, so a regression is a stack overflow rather than a slow test.
const DEEP: u32 = 100_000;

/// A left-nested application spine `((x a) a) …`, `DEEP` links tall.
fn deep_spine(seed: u32) -> Term {
    let argument = Term::free_var(&Free::local(seed, None));
    let mut term = Term::free_var(&Free::local(seed, None));
    for _ in 0..DEEP {
        term = Term::apply(term, [argument.clone()]);
    }
    term
}

#[test]
fn deep_terms_compare_without_native_recursion() {
    // Equality used to recurse once per link, so a term this tall answered by aborting the process. Two independently built spines are structurally equal but share no node, which is exactly the case that has to walk.
    assert_eq!(deep_spine(0), deep_spine(0));
    assert_ne!(deep_spine(0), deep_spine(1));
}

#[test]
fn deep_terms_are_released_without_native_recursion() {
    // The other half: releasing a spine this tall used to recurse once per link through the derived drop of the `Rc` chain. Every term built here goes out of scope at the end of the test, which is the whole point.
    let shared = deep_spine(0);
    let sharing = Term::tuple([shared.clone(), shared.clone()]);

    assert_eq!(sharing, Term::tuple([shared.clone(), shared]));
}
