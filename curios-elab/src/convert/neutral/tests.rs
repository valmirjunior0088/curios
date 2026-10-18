//! The binders a walk opens beside the types it reads, each found by its name.

use {
    crate::Opened,
    curios_core::{Free, Term},
};

/// A name opened again is read at its innermost type while that stands, and at the one before once it is closed.
///
/// Mutation-checked: a close that forgot the position it shadowed leaves the name unbound.
#[test]
fn a_name_opened_again_is_read_innermost_until_it_is_closed() {
    let name = Free::local(0, Some("x"));
    let mut opened = Opened::default();
    opened.push(name, Term::type_ground());
    opened.push(name, Term::prop());

    assert_eq!(opened.type_of(&name), Some(&Term::prop()));
    opened.truncate(1);
    assert_eq!(opened.type_of(&name), Some(&Term::type_ground()));
    opened.pop();
    assert!(!opened.binds(&name));
}

/// A type names an opened binder where one of its free names is open, whether it holds fewer names than are open or more.
#[test]
fn a_type_names_an_opened_binder_through_its_free_names() {
    let [x, y, z] = [0, 1, 2].map(|identity| Free::local(identity, None));
    let mut opened = Opened::default();
    opened.push(x, Term::type_ground());
    opened.push(y, Term::type_ground());

    assert!(opened.names_one_in(&Term::free_var(&y)));
    assert!(!opened.names_one_in(&Term::free_var(&z)));
    assert!(opened.names_one_in(&Term::tuple([
        Term::free_var(&z),
        Term::free_var(&Free::local(5, None)),
        Term::free_var(&x),
    ])));
    assert!(!opened.names_one_in(&Term::tuple([
        Term::free_var(&z),
        Term::free_var(&Free::local(3, None)),
        Term::free_var(&Free::local(4, None)),
    ])));
}
