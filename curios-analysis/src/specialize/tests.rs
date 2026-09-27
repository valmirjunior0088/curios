//! What a case's solution re-types, read through local definitions.

use {
    super::*,
    crate::test_support::Scope,
    curios_core::{Intrinsic, Nat},
};

fn nat(value: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(value)))
}

/// A term mentioning exactly `name`, standing for a type that does.
fn over(name: &Free) -> Term {
    Term::intrinsic(Intrinsic::nat_add(Term::free_var(name), nat(1)))
}

/// `let t = s; match t | …` solves `s`, the variable the kernel's arm meets once the `let` is substituted. Controls: a top-level name is no local and solves nothing, and neither does an expression.
#[test]
fn a_let_of_a_variable_solves_the_variable_beneath_it() {
    let s = Free::local(1, Some("s"));
    let t = Free::local(2, Some("t"));
    let top = Free::local(3, Some("top"));
    let mut scope = Scope::default();
    scope.assume(&s);
    scope.define(&t, Term::free_var(&s));

    let value = nat(7);
    for (scrutinee, expected) in [
        (Term::free_var(&s), Some((s.clone(), value.clone()))),
        (Term::free_var(&t), Some((s.clone(), value.clone()))),
        (Term::free_var(&top), None),
        (over(&s), None),
    ] {
        assert_eq!(
            scrutinee_solution(&scope, &scrutinee, &value),
            expected,
            "{scrutinee:?}"
        );
    }
}

/// With `s` solved, a local typed over `s` is re-typed, and so is one typed over `let t = s`, with `t` inlined — both are typed over `s` once the kernel has substituted the `let`. A local mentioning neither keeps its entry, and so does `s`, whose occurrences the arm substitutes.
#[test]
fn a_local_typed_through_a_let_is_retyped_with_the_let_inlined() {
    let s = Free::local(1, Some("s"));
    let t = Free::local(2, Some("t"));
    let z = Free::local(3, Some("z"));
    let w = Free::local(4, Some("w"));
    let u = Free::local(5, Some("u"));
    let mut scope = Scope::default();
    scope.assume(&s);
    scope.define(&t, Term::free_var(&s));

    let nat_type = Term::intrinsic(Intrinsic::NatType);
    let locals = [
        (s.clone(), nat_type.clone()),
        (z.clone(), over(&s)),
        (w.clone(), over(&t)),
        (u.clone(), nat_type),
    ];
    let solutions = [(s.clone(), nat(7))];

    let over_seven = Term::intrinsic(Intrinsic::nat_add(nat(7), nat(1)));
    assert_eq!(
        retyped(&scope, &locals, &solutions),
        vec![(z, over_seven.clone()), (w, over_seven)]
    );
}
