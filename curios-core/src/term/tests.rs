use {
    crate::*,
    curios_base::Plicity,
    std::{collections::BTreeSet, rc::Rc},
};

#[cfg(feature = "archive")]
#[test]
fn archive_resets_caches_and_preserves_rc_sharing() {
    let shared = Term::free_var("shared");
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
    let term = Scope::close(One, &["x"], Term::free_var("x")).open(&[&Term::free_var("y")]);

    let Subterm::Var(var) = &*term else {
        panic!("unexpected `{term:?}`")
    };

    assert_eq!(var, &Var::free("y"));
}

#[test]
fn close_open_preserves_nested_bind() {
    let term = Scope::close(
        One,
        &["x"],
        Term::func([("y", Term::type_())], Term::free_var("x")),
    )
    .open(&[&Term::free_var("z")]);

    let Subterm::Func(body) = &*term else {
        panic!("unexpected `{term:?}`")
    };

    let opened = body.telescope.open(&[&Term::free_var("w")]);
    let Subterm::Var(var) = &*opened else {
        panic!("unexpected term")
    };

    assert_eq!(var, &Var::free("z"));
}

#[test]
fn collect_ignores_index_names() {
    let term = Term::func(
        [("x", Term::type_())],
        Term::tuple([
            Term::free_var("x"),
            Term::rec(
                vec![("y", Term::type_(), Term::free_var("z"))],
                Term::tuple([Term::free_var("y"), Term::free_var("w")]),
            ),
        ]),
    );

    assert_eq!(
        term.free_vars(),
        BTreeSet::from([String::from("w"), String::from("z")])
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
        Term::func([("x", Term::type_())], Term::metavar(1)),
        [
            Term::metavar(2),
            Term::prim(Prim::nat_add(Term::metavar(3), Term::metavar(1))),
        ],
    );
    assert_eq!(term.metavars(), BTreeSet::from([1, 2, 3].map(MetavarId)));
}

#[test]
fn any_metavar_short_circuits_and_agrees_with_collection() {
    // (λx. ?1)(?2, Nat.add ?3 ?1)
    let term = Term::apply(
        Term::func([("x", Term::type_())], Term::metavar(1)),
        [
            Term::metavar(2),
            Term::prim(Prim::nat_add(Term::metavar(3), Term::metavar(1))),
        ],
    );

    // A predicate over the ids agrees with the collecting walk: a present id is
    // found, an absent one is not.
    assert!(term.any_metavar(&mut |id| id == MetavarId(3)));
    assert!(!term.any_metavar(&mut |id| id == MetavarId(99)));
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
    let plain = Term::func([("x", Term::type_())], Term::free_var("x"));
    let mut fired = false;
    assert!(!plain.any_metavar(&mut |_| {
        fired = true;
        true
    }));
    assert!(!fired);
}

#[test]
fn has_local_free_flags_minted_names_not_binder_hints() {
    // `#` is the elaborator's minting marker (`Context::fresh`) and cannot
    // occur in a written identifier, so a free var carrying it is the mark of
    // a context-dependent local — and the only kind that can invalidate an
    // elaboration-cache entry across frames.
    assert!(Term::free_var("x#3").has_local_free());
    assert!(!Term::free_var("/std/Nat").has_local_free());

    // The bit is structural: a minted name anywhere in the tree sets it.
    let inner = Term::apply(Term::free_var("/syn/Str/step"), [Term::free_var("c#1")]);
    assert!(inner.has_local_free());

    // A binder whose label hint carries `#` stays clean: the hint is not an
    // occurrence, and the captured variable is bound, not free.
    let binder = Term::func([("x#9", Term::type_())], Term::free_var("x#9"));
    assert!(!binder.has_local_free());
}

#[test]
fn has_metavar_flags_any_metavariable_node() {
    assert!(Term::metavar(1).has_metavar());
    assert!(
        Term::apply(
            Term::func([("x", Term::type_())], Term::free_var("x")),
            [Term::metavar(2)],
        )
        .has_metavar()
    );
    assert!(!Term::func([("x", Term::type_())], Term::free_var("x")).has_metavar());
}

#[test]
fn metavar_is_inert_under_traversal() {
    // shifting/capture must not disturb a metavariable node
    let m = Term::metavar(4);
    assert_eq!(m.shift(3), m);
    let scope = Scope::close(One, &["x"], Term::metavar(4));
    assert_eq!(scope.open(&[&Term::free_var("y")]), Term::metavar(4));
}

#[test]
fn variant_collects_metavars_and_prints_as_function_call() {
    let ctor = Term::variant("Result", [Term::metavar(1)], "success", [Term::metavar(2)]);
    assert_eq!(ctor.metavars(), BTreeSet::from([1, 2].map(MetavarId)));
    assert_eq!(format!("{ctor}"), "Result/success(?2)");

    let type_ = Term::inductive_type(
        "Result",
        [Term::prim(Prim::NatType), Term::metavar(3)],
        Vec::<Term>::new(),
    );
    assert_eq!(type_.metavars(), BTreeSet::from([3].map(MetavarId)));
    assert_eq!(format!("{type_}"), "Result(Nat, ?3)");
}

#[test]
fn implicit_marks_print_and_default_to_explicit() {
    let ft = Term::func_type_marked(
        [
            (Plicity::Implicit, "T", Term::type_()),
            (Plicity::Explicit, "x", Term::free_var("T")),
        ],
        Term::free_var("T"),
    );
    assert_eq!(format!("{ft}"), "(@T : Type, x : T) -> T");

    // The unmarked builders default every slot to `Explicit`.
    let plain = Term::func_type([("T", Term::type_())], Term::type_());
    match &*plain {
        Subterm::FuncType(FuncType { plicities, .. }) => {
            assert_eq!(plicities, &[Plicity::Explicit]);
        }
        _ => unreachable!(),
    }

    let call = Term::apply_marked(
        Term::free_var("foo"),
        [
            (Plicity::Implicit, Term::free_var("Nat")),
            (Plicity::Explicit, Term::free_var("x")),
        ],
    );
    assert_eq!(format!("{call}"), "foo(@Nat, x)");
}

#[test]
fn inductive_match_case_binders_are_captured() {
    // match r : #m => Type; | success(value) => value;
    let term = Term::inductive_match(
        Term::free_var("r"),
        None,
        Term::type_(),
        [("success", vec!["value"], Term::free_var("value"))],
    );

    let free = term.free_vars();
    assert!(free.contains("r"));
    assert!(!free.contains("value"));
}

#[test]
fn inductive_match_default_prints_a_catch_all_arm() {
    // The catch-all renders as a trailing `| _ =>` arm, after the enumerated
    // constructors — mirroring `Cases::Switch`'s default.
    let term = Term::inductive_match_default(
        Term::free_var("r"),
        None,
        Term::type_(),
        [("none", Vec::<&str>::new(), Term::free_var("a"))],
        Term::free_var("b"),
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
        Term::inductive_type("Result", [Term::var(Var::bound(2))], Vec::<Term>::new()).reach(),
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
    assert_eq!(Term::type_().reach(), 0);
    assert_eq!(Term::free_var("x").reach(), 0);
    assert_eq!(Term::var(Var::bound(0)).reach(), 1);
    assert_eq!(Term::var(Var::bound(3)).reach(), 4);
    // closed identity function λx.x
    assert_eq!(
        Term::func([("x", Term::type_())], Term::free_var("x")).reach(),
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
            Term::type_(),
            Scope::constant(One, Telescope::done(Term::var(Var::bound(2)))),
        ),
    }));
    assert_eq!(f1.reach(), 2); // one binder: (2 + 1) - 1

    let f2 = Term::from(Subterm::Func(Func {
        telescope: Telescope::Cons(
            Term::type_(),
            Scope::constant(
                One,
                Telescope::Cons(
                    Term::type_(),
                    Scope::constant(One, Telescope::done(Term::var(Var::bound(2)))),
                ),
            ),
        ),
    }));
    assert_eq!(f2.reach(), 1); // two binders: (2 + 1) - 2
}

#[test]
fn open_shares_closed_body_without_rebuild() {
    // body does not mention the bound variable -> open returns the stored Rc unchanged
    let scope = Scope::close(One, &["x"], Term::type_());
    let opened = scope.open(&[&Term::free_var("y")]);
    assert!(Rc::ptr_eq(&opened.inner, &scope.body().inner));
}

#[test]
fn open_shares_closed_subterm_inside_substituted_body() {
    let closed = Term::func([("a", Term::type_())], Term::free_var("a")); // λa.a, closed
    let scope = Scope::close(One, &["x"], Term::tuple([Term::free_var("x"), closed]));

    let stored_field = match &**scope.body() {
        Subterm::Tuple(Tuple { fields, .. }) => fields[1].clone(),
        _ => panic!("expected tuple body"),
    };

    let opened = scope.open(&[&Term::free_var("y")]);

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

    let this = Term::func([("x", Term::type_())], Term::free_var("x"));
    let that = Term::func([("y", Term::type_())], Term::free_var("y"));

    assert_eq!(this, that);

    let state = std::collections::hash_map::RandomState::new();
    assert_eq!(state.hash_one(&this), state.hash_one(&that));
}

#[test]
fn tuple_type_field_labels_are_identity() {
    // Field labels are the target of `.label` resolution, so unlike binder
    // hints they split identity: an α-equal twin with different labels must
    // not be substituted for this type by any Eq-keyed cache.
    let this = Term::tuple_type([("cp", Term::type_()), ("v", Term::free_var("cp"))]);
    let that = Term::tuple_type([("r", Term::type_()), ("v", Term::free_var("r"))]);

    assert_ne!(this, that);
    assert_eq!(
        this,
        Term::tuple_type([("cp", Term::type_()), ("v", Term::free_var("cp"))])
    );
}
