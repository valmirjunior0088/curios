use {
    crate::*,
    curios_base::Plicity,
    std::{collections::BTreeSet, rc::Rc},
};

#[cfg(feature = "archive")]
#[test]
fn archive_resets_caches_and_preserves_rc_sharing() {
    let shared = Term::free_var(&crate::fixture_binder("shared"));
    let term = Term::tuple([shared.clone(), shared]);
    term.get_or_init_hash();
    term.reach();
    term.free_vars();
    term.has_local_free();
    term.has_metavar();
    assert!(term.inner.hash.get().is_some());
    assert!(term.inner.reach.get().is_some());
    assert!(term.inner.free_vars.get().is_some());
    assert!(term.inner.has_local_free.get().is_some());
    assert!(term.inner.has_metavar.get().is_some());

    let bytes = rkyv::to_bytes::<rkyv::rancor::Error>(&term).unwrap();
    let restored = rkyv::from_bytes::<Term, rkyv::rancor::Error>(&bytes).unwrap();
    assert!(restored.inner.hash.get().is_none());
    assert!(restored.inner.reach.get().is_none());
    assert!(restored.inner.free_vars.get().is_none());
    assert!(restored.inner.has_local_free.get().is_none());
    assert!(restored.inner.has_metavar.get().is_none());

    let Subterm::Tuple(tuple) = restored.as_ref() else {
        panic!("restored term changed shape");
    };
    assert!(Rc::ptr_eq(&tuple.fields[0].inner, &tuple.fields[1].inner));
}

#[test]
fn close_open_substitutes_label_name() {
    let term = Scope::close(
        One,
        &[&crate::fixture_binder("x")],
        Term::free_var(&crate::fixture_binder("x")),
    )
    .open(&[&Term::free_var(&crate::fixture_binder("y"))]);

    let Subterm::Var(var) = &*term else {
        panic!("unexpected `{term:?}`")
    };

    assert_eq!(var, &Var::free(crate::fixture_binder("y")));
}

#[test]
fn close_open_preserves_nested_bind() {
    let term = Scope::close(
        One,
        &[&crate::fixture_binder("x")],
        Term::func(
            [(crate::fixture_binder("y"), Term::type_ground())],
            Term::free_var(&crate::fixture_binder("x")),
        ),
    )
    .open(&[&Term::free_var(&crate::fixture_binder("z"))]);

    let Subterm::Func(body) = &*term else {
        panic!("unexpected `{term:?}`")
    };

    let opened = body
        .telescope
        .open(&[&Term::free_var(&crate::fixture_binder("w"))]);
    let Subterm::Var(var) = &*opened else {
        panic!("unexpected term")
    };

    assert_eq!(var, &Var::free(crate::fixture_binder("z")));
}

#[test]
fn collect_ignores_index_names() {
    let term = Term::func(
        [(crate::fixture_binder("x"), Term::type_ground())],
        Term::tuple([
            Term::free_var(&crate::fixture_binder("x")),
            Term::rec(
                vec![(
                    crate::fixture_binder("y"),
                    Term::type_ground(),
                    Term::free_var(&crate::fixture_binder("z")),
                )],
                Term::tuple([
                    Term::free_var(&crate::fixture_binder("y")),
                    Term::free_var(&crate::fixture_binder("w")),
                ]),
            ),
        ]),
    );

    assert_eq!(
        term.free_vars(),
        BTreeSet::from([crate::fixture_binder("w"), crate::fixture_binder("z")])
    );
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
    // (λx. ?1)(?2, Nat.add ?3 ?1)
    let term = Term::apply(
        Term::func(
            [(crate::fixture_binder("x"), Term::type_ground())],
            Term::metavar(1),
        ),
        [
            Term::metavar(2),
            Term::prim(Prim::nat_add(Term::metavar(3), Term::metavar(1))),
        ],
    );
    assert_eq!(term.metavars(), BTreeSet::from([1, 2, 3].map(MetaId)));
}

#[test]
fn any_metavar_short_circuits_and_agrees_with_collection() {
    // (λx. ?1)(?2, Nat.add ?3 ?1)
    let term = Term::apply(
        Term::func(
            [(crate::fixture_binder("x"), Term::type_ground())],
            Term::metavar(1),
        ),
        [
            Term::metavar(2),
            Term::prim(Prim::nat_add(Term::metavar(3), Term::metavar(1))),
        ],
    );

    // A predicate over the ids agrees with the collecting walk: a present id is
    // found, an absent one is not.
    assert!(term.any_metavar(&mut |id| id == MetaId(3)));
    assert!(!term.any_metavar(&mut |id| id == MetaId(99)));
    assert_eq!(term.any_metavar(&mut |_| true), !term.metavars().is_empty());

    // Bails on the first metavariable: the head's `?1` is reached first, so an
    // accept-anything predicate runs exactly once instead of visiting all four.
    let mut visits = 0;
    assert!(term.any_metavar(&mut |_| {
        visits += 1;
        true
    }));
    assert_eq!(visits, 1);

    // A metavariable-free term never fires the predicate.
    let plain = Term::func(
        [(crate::fixture_binder("x"), Term::type_ground())],
        Term::free_var(&crate::fixture_binder("x")),
    );
    let mut fired = false;
    assert!(!plain.any_metavar(&mut |_| {
        fired = true;
        true
    }));
    assert!(!fired);
}

#[test]
fn has_local_free_flags_locals_not_globals() {
    fn global(path: [&str; 2]) -> Term {
        Term::free_var(&Free::global(curios_base::Qualifier::from(path)))
    }

    // A local is a discriminant, not a spelling: what makes a free variable
    // context-dependent — the only kind that can invalidate an elaboration-cache
    // entry across frames — is that a scope opened it.
    assert!(Term::free_var(&Free::local(3, Some("x"))).has_local_free());
    assert!(!global(["std", "Nat"]).has_local_free());

    // A compiler-generated *global* is not context dependent and must not set
    // the bit. This used to be a search for a marker character, so a witness
    // that spelled itself `witness#N` misfired on every term mentioning one,
    // silently disabling three elaboration caches. No spelling can do that now.
    assert!(!Term::free_var(&Free::Global(Global::Witness(WitnessId(0)))).has_local_free());
    assert!(!global(["std", "Nat"]).has_local_free());

    // The bit is structural: a minted name anywhere in the tree sets it.
    let inner = Term::apply(
        Term::free_var(&crate::fixture_binder("/syn/Str/step")),
        [Term::free_var(&crate::fixture_binder("c#1"))],
    );
    assert!(inner.has_local_free());

    // A binder whose label hint carries `#` stays clean: the hint is not an
    // occurrence, and the captured variable is bound, not free.
    let binder = Term::func(
        [(crate::fixture_binder("x#9"), Term::type_ground())],
        Term::free_var(&crate::fixture_binder("x#9")),
    );
    assert!(!binder.has_local_free());
}

#[test]
fn has_metavar_flags_any_metavariable_node() {
    assert!(Term::metavar(1).has_metavar());
    assert!(
        Term::apply(
            Term::func(
                [(crate::fixture_binder("x"), Term::type_ground())],
                Term::free_var(&crate::fixture_binder("x"))
            ),
            [Term::metavar(2)],
        )
        .has_metavar()
    );
    assert!(
        !Term::func(
            [(crate::fixture_binder("x"), Term::type_ground())],
            Term::free_var(&crate::fixture_binder("x"))
        )
        .has_metavar()
    );
}

#[test]
fn metavar_is_inert_under_traversal() {
    // shifting/capture must not disturb a metavariable node
    let m = Term::metavar(4);
    assert_eq!(m.shift(3), m);
    let scope = Scope::close(One, &[&crate::fixture_binder("x")], Term::metavar(4));
    assert_eq!(
        scope.open(&[&Term::free_var(&crate::fixture_binder("y"))]),
        Term::metavar(4)
    );
}

#[test]
fn variant_collects_metavars_and_prints_as_function_call() {
    let ctor = Term::variant("Result", [Term::metavar(1)], "success", [Term::metavar(2)]);
    assert_eq!(ctor.metavars(), BTreeSet::from([1, 2].map(MetaId)));
    assert_eq!(format!("{ctor}"), "Result/success(?2)");

    let type_ = Term::induct_type(
        "Result",
        [Term::prim(Prim::NatType), Term::metavar(3)],
        Vec::<Term>::new(),
    );
    assert_eq!(type_.metavars(), BTreeSet::from([3].map(MetaId)));
    assert_eq!(format!("{type_}"), "Result(Nat, ?3)");
}

#[test]
fn implicit_marks_print_and_default_to_explicit() {
    let ft = Term::func_type_marked(
        [
            (
                Plicity::Implicit,
                crate::fixture_binder("T"),
                Term::type_ground(),
            ),
            (
                Plicity::Explicit,
                crate::fixture_binder("x"),
                Term::free_var(&crate::fixture_binder("T")),
            ),
        ],
        Term::free_var(&crate::fixture_binder("T")),
    );
    assert_eq!(format!("{ft}"), "(@T : Type, x : T) -> T");

    // The unmarked builders default every slot to `Explicit`.
    let plain = Term::func_type(
        [(crate::fixture_binder("T"), Term::type_ground())],
        Term::type_ground(),
    );
    match &*plain {
        Subterm::FuncType(FuncType { plicities, .. }) => {
            assert_eq!(plicities, &[Plicity::Explicit]);
        }
        _ => unreachable!(),
    }

    let call = Term::apply_marked(
        Term::free_var(&crate::fixture_binder("foo")),
        [
            (
                Plicity::Implicit,
                Term::free_var(&crate::fixture_binder("Nat")),
            ),
            (
                Plicity::Explicit,
                Term::free_var(&crate::fixture_binder("x")),
            ),
        ],
    );
    assert_eq!(format!("{call}"), "foo(@Nat, x)");
}

#[test]
fn inductive_match_case_binders_are_captured() {
    // match r : #m => Type; | success(value) => value;
    let term = Term::induct_match(
        Term::free_var(&crate::fixture_binder("r")),
        None,
        Term::type_ground(),
        [(
            "success",
            vec![crate::fixture_binder("value")],
            Term::free_var(&crate::fixture_binder("value")),
        )],
    );

    let free = term.free_vars();
    assert!(free.contains(&crate::fixture_binder("r")));
    assert!(!free.contains(&crate::fixture_binder("value")));
}

#[test]
fn inductive_match_default_prints_a_catch_all_arm() {
    // The catch-all renders as a trailing `| _ =>` arm, after the enumerated
    // constructors — mirroring `Cases::Switch`'s default.
    let term = Term::induct_match_default(
        Term::free_var(&crate::fixture_binder("r")),
        None,
        Term::type_ground(),
        [(
            "none",
            Vec::<crate::Free>::new(),
            Term::free_var(&crate::fixture_binder("a")),
        )],
        Term::free_var(&crate::fixture_binder("b")),
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
        Term::induct_type("Result", [Term::var(Var::bound(2))], Vec::<Term>::new()).reach(),
        3
    );
    assert_eq!(
        Term::variant(
            "Result",
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
    assert_eq!(Term::type_ground().reach(), 0);
    assert_eq!(Term::free_var(&crate::fixture_binder("x")).reach(), 0);
    assert_eq!(Term::var(Var::bound(0)).reach(), 1);
    assert_eq!(Term::var(Var::bound(3)).reach(), 4);
    // closed identity function λx.x
    assert_eq!(
        Term::func(
            [(crate::fixture_binder("x"), Term::type_ground())],
            Term::free_var(&crate::fixture_binder("x"))
        )
        .reach(),
        0
    );
}

#[test]
fn reach_telescope_absorbs_arity() {
    // body references bound index 2 (reach 3); each telescope binder absorbs one.
    // `Scope::constant` places the body without capturing, so the bound index is
    // preserved exactly (unlike `Telescope::cons`, which captures by label).
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
    // body does not mention the bound variable -> open returns the stored Rc unchanged
    let scope = Scope::close(One, &[&crate::fixture_binder("x")], Term::type_ground());
    let opened = scope.open(&[&Term::free_var(&crate::fixture_binder("y"))]);
    assert!(Rc::ptr_eq(&opened.inner, &scope.body().inner));
}

#[test]
fn open_shares_closed_subterm_inside_substituted_body() {
    let closed = Term::func(
        [(crate::fixture_binder("a"), Term::type_ground())],
        Term::free_var(&crate::fixture_binder("a")),
    ); // λa.a, closed
    let scope = Scope::close(
        One,
        &[&crate::fixture_binder("x")],
        Term::tuple([Term::free_var(&crate::fixture_binder("x")), closed]),
    );

    let stored_field = match &**scope.body() {
        Subterm::Tuple(Tuple { fields, .. }) => fields[1].clone(),
        _ => panic!("expected tuple body"),
    };

    let opened = scope.open(&[&Term::free_var(&crate::fixture_binder("y"))]);

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
    use std::hash::BuildHasher;

    let this = Term::func(
        [(crate::fixture_binder("x"), Term::type_ground())],
        Term::free_var(&crate::fixture_binder("x")),
    );
    let that = Term::func(
        [(crate::fixture_binder("y"), Term::type_ground())],
        Term::free_var(&crate::fixture_binder("y")),
    );

    assert_eq!(this, that);

    let state = std::collections::hash_map::RandomState::new();
    assert_eq!(state.hash_one(&this), state.hash_one(&that));
}

#[test]
fn tuple_type_field_labels_are_identity() {
    // Field labels are the target of `.label` resolution, so unlike binder
    // hints they split identity: an α-equal twin with different labels must
    // not be substituted for this type by any Eq-keyed cache.
    let this = Term::tuple_type([
        (crate::fixture_binder("cp"), Term::type_ground()),
        (
            crate::fixture_binder("v"),
            Term::free_var(&crate::fixture_binder("cp")),
        ),
    ]);
    let that = Term::tuple_type([
        (crate::fixture_binder("r"), Term::type_ground()),
        (
            crate::fixture_binder("v"),
            Term::free_var(&crate::fixture_binder("r")),
        ),
    ]);

    assert_ne!(this, that);
    assert_eq!(
        this,
        Term::tuple_type([
            (crate::fixture_binder("cp"), Term::type_ground()),
            (
                crate::fixture_binder("v"),
                Term::free_var(&crate::fixture_binder("cp"))
            )
        ])
    );
}

/// The number of distinct `Node`s reachable from `term`, counting a shared node
/// once. Inlined here because only these tests ask the question.
fn distinct_nodes(term: &Term) -> usize {
    let mut seen = std::collections::HashSet::new();
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
    let shared = Term::apply(
        Term::free_var(&crate::fixture_binder("f")),
        [Term::free_var(&crate::fixture_binder("x"))],
    );
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

/// A rewrite that rebuilds a shared node once per *occurrence* rather than once
/// per node turns a DAG into its expansion. This is the shape that made it
/// matter: a string literal lowers to a chain threading a scan state, where
/// every link mentions the previous state, so the term is linear in distinct
/// nodes but triangular expanded. Losing the memo here cost O(n^2) nodes for an
/// n-byte literal, and every later pass over the term inherited it.
#[test]
fn a_memoized_rewrite_keeps_a_shared_chain_linear() {
    let depth = 200;
    let mut state = Term::free_var(&crate::fixture_binder("lead"));
    let mut chain = Term::free_var(&crate::fixture_binder("stop"));
    for _ in 0..depth {
        chain = Term::tuple([state.clone(), chain]);
        state = Term::apply(Term::free_var(&crate::fixture_binder("step")), [state]);
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
