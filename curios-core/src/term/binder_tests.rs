//! Closing and opening binders, and the hints and labels that are not part of a term's identity.

use {
    crate::*,
    curios_utilities::Plicity,
    std::{collections::hash_map::RandomState, hash::BuildHasher, rc::Rc},
};

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

    let bytes = curios_archive::to_bytes(&term).unwrap();
    let restored = curios_archive::from_bytes::<Term>(&bytes).unwrap();
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
fn name_hints_are_identity_irrelevant() {
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

#[test]
fn closing_and_opening_a_variable_headed_instance_round_trips() {
    let f = Free::local(41, Some("f"));
    let instance = Term::instance_of(&f, vec![Level::zero()]);
    let scope = Scope::close(Many(1), &[&f], instance.clone());

    assert_eq!(scope.open(&[&Term::free_var(&f)]), instance);
}

#[test]
fn opening_substitutes_a_projection_into_a_member_headed_instance() {
    let f = Free::local(40, Some("f"));
    let instance = Term::instance_of(&f, vec![Level::zero()]);
    let scope = Scope::close(Many(1), &[&f], instance);

    let rec = Term::rec(
        vec![(f.clone(), Term::type_ground(), Term::type_ground())],
        Term::free_var(&f),
    );
    let Subterm::Rec(Rec { group, .. }) = &*rec else {
        panic!("the fixture changed shape");
    };

    let opened = scope.open(&[&Term::rec_proj(group.clone(), 0)]);
    let Subterm::Instance(Instance {
        head: InstanceHead::RecProj(opened_group, 0),
        ..
    }) = &*opened
    else {
        panic!("the projection did not re-enter the typed head: {opened}");
    };
    assert_eq!(opened_group, group);
}

/// The seam's total answer for the one replacement no elaborated term produces: a binder no scheme governs substituted by an arbitrary value. The instance dissolves to the replacement — the levels-inert reading the kernel's sort fixtures pin for local heads — because a reducer that promises totality on arbitrary terms may not abort here.
#[test]
fn a_replacement_that_is_not_a_head_shape_dissolves_the_instance() {
    let f = Free::local(42, Some("f"));
    let instance = Term::instance_of(&f, vec![Level::zero()]);
    let scope = Scope::close(Many(1), &[&f], instance);
    let value = Term::intrinsic(Intrinsic::NatType);

    assert_eq!(scope.open(&[&value]), value);
}
