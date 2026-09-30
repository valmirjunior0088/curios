//! The kernel's view of a term held under local definitions.

use {
    super::*,
    crate::test_support::Scope,
    curios_core::{Intrinsic, Nat},
};

fn nat(value: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(value)))
}

fn sum(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::nat_add(left, right))
}

fn var(name: &Free) -> Term {
    Term::free_var(name)
}

/// Unfolding toward solved variables substitutes a definition only where it reaches one, so a term is respelled no further than the substitution needs; unfolding everything is the kernel's spelling, every definition substituted.
#[test]
fn an_unfolding_toward_solved_variables_leaves_other_definitions_named() {
    let s = Free::local(1, Some("s"));
    let t = Free::local(2, Some("t"));
    let u = Free::local(3, Some("u"));
    let v = Free::local(4, Some("v"));
    let mut scope = Scope::default();
    scope.assume(&s);
    scope.assume(&v);
    scope.define(&t, var(&s));
    scope.define(&u, var(&v));

    let term = sum(var(&t), var(&u));
    let solutions = [(s, nat(7))];

    assert_eq!(
        Unfolding::toward(&scope, &solutions).term(&term),
        sum(var(&s), var(&u))
    );
    assert_eq!(
        Unfolding::everything(&scope).term(&term),
        sum(var(&s), var(&v))
    );
}

/// A definition that mentions itself is read once: a term naming it is unfolded a single level, and the walk ends.
#[test]
fn a_definition_that_mentions_itself_is_read_once() {
    let s = Free::local(1, Some("s"));
    let f = Free::local(2, Some("f"));
    let mut scope = Scope::default();
    scope.assume(&s);
    scope.define(&f, sum(var(&f), var(&s)));

    assert_eq!(
        Unfolding::everything(&scope).term(&sum(var(&f), nat(1))),
        sum(sum(var(&f), var(&s)), nat(1))
    );
}

/// The locals the kernel's spelling names: a local as itself, a local definition as the locals its definition names in turn, and a top-level name not at all.
#[test]
fn the_locals_beneath_a_definition_are_the_ones_it_names() {
    let s = Free::local(1, Some("s"));
    let x = Free::local(2, Some("x"));
    let y = Free::local(3, Some("y"));
    let t = Free::local(4, Some("t"));
    let u = Free::local(5, Some("u"));
    let top = Free::local(6, Some("top"));
    let mut scope = Scope::default();
    for name in [&s, &x, &y] {
        scope.assume(name);
    }
    scope.define(&t, var(&s));
    scope.define(&u, sum(var(&x), var(&y)));

    let locals = locals_beneath(&scope, &[sum(var(&t), var(&u)), var(&top)])
        .into_iter()
        .collect::<BTreeSet<_>>();

    assert_eq!(locals, BTreeSet::from([s, x, y]));
}

/// The variable a term is, beneath its definitions: the last link of a chain of variables. An expression beneath them is no variable, and neither is a chain that returns to itself.
#[test]
fn the_variable_beneath_a_chain_of_definitions_is_its_last_link() {
    let s = Free::local(1, Some("s"));
    let t = Free::local(2, Some("t"));
    let u = Free::local(3, Some("u"));
    let e = Free::local(4, Some("e"));
    let loop_ = Free::local(5, Some("loop"));
    let mut scope = Scope::default();
    scope.assume(&s);
    scope.define(&u, var(&s));
    scope.define(&t, var(&u));
    scope.define(&e, sum(var(&s), nat(1)));
    scope.define(&loop_, var(&loop_));

    assert_eq!(beneath(&scope, &var(&t)), Some(&s));
    assert_eq!(beneath(&scope, &var(&e)), None);
    assert_eq!(beneath(&scope, &var(&loop_)), None);
}
