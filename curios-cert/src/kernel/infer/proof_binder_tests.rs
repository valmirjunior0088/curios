//! A binder's mark in the kernel: held to a proof, and held one way.

use {
    super::test_support::*,
    crate::{Error, infer},
    curios_core::{Func, Let, Subterm, Term},
};

/// `function`, a function literal, with each binder stating what `proofs` says of whether it binds a proof.
fn stating(function: Term, proofs: &[bool]) -> Term {
    let Subterm::Func(Func { telescope }) = Term::unwrap_or_clone(function) else {
        panic!("a function literal");
    };

    Term::from(Subterm::Func(Func {
        telescope: telescope.with_proofs(proofs),
    }))
}

/// A binder that states it binds a proof is refused at a type that is no proposition: a function literal's, and a `let`'s. The closed machine binds what such a binder binds unreduced, and the kernel holds the machine it runs to what no judgment reads the value of.
#[test]
fn a_binder_marked_at_a_type_that_is_no_proposition_is_refused() {
    let mut kernel = kernel();
    let n = binder(0, "n");

    let function = stating(Term::func([(n, nat_type())], Term::free_var(&n)), &[true]);
    assert_eq!(
        infer(&mut kernel, &function),
        Err(Error::NoProof(nat_type()))
    );

    let Subterm::Let(Let { bindings, tail }) =
        Term::unwrap_or_clone(Term::let_(&n, nat_type(), nat(3), Term::free_var(&n)))
    else {
        panic!("a `let`");
    };
    let block = Term::from(Subterm::Let(Let {
        bindings,
        tail: tail.with_proofs(&[true]),
    }));
    assert_eq!(infer(&mut kernel, &block), Err(Error::NoProof(nat_type())));
}

/// A type is no proof, and neither is a family: a binder at a sort and one at a function into a sort are refused a mark. Conversion reads the reduct of a type, so the machine evaluates one before it binds it, however completely erasure deletes it.
#[test]
fn a_binder_marked_at_a_sort_or_a_family_is_refused() {
    let (p, family, n) = (binder(0, "P"), binder(1, "F"), binder(2, "n"));
    let family_type = Term::func_type([(binder(3, "k"), nat_type())], Term::type_ground());

    let over_a_sort = stating(
        Term::func([(p, Term::prop()), (n, nat_type())], Term::free_var(&n)),
        &[true, false],
    );
    assert_eq!(
        infer(&mut kernel(), &over_a_sort),
        Err(Error::NoProof(Term::prop()))
    );

    let over_a_family = stating(
        Term::func(
            [(family, family_type.clone()), (n, nat_type())],
            Term::free_var(&n),
        ),
        &[true, false],
    );
    assert_eq!(
        infer(&mut kernel(), &over_a_family),
        Err(Error::NoProof(family_type))
    );
}

/// A binder marked at a proof and at a function into one is accepted, and so is the same function with no binder marked: a mark that could stand and does not costs a reduction and is refused nowhere.
#[test]
fn a_binder_marked_at_a_proof_is_accepted_and_one_left_unmarked_too() {
    let (p, h, lemma, n) = (
        binder(0, "P"),
        binder(1, "h"),
        binder(2, "lemma"),
        binder(3, "n"),
    );
    let function = Term::func(
        [
            (p, Term::prop()),
            (h, Term::free_var(&p)),
            (
                lemma,
                Term::func_type([(binder(4, "k"), nat_type())], Term::free_var(&p)),
            ),
            (n, nat_type()),
        ],
        Term::free_var(&n),
    );

    let unmarked = infer(&mut kernel(), &function);
    assert!(unmarked.is_ok(), "{unmarked:?}");
    assert_eq!(
        infer(
            &mut kernel(),
            &stating(function, &[false, true, true, false])
        ),
        unmarked,
    );
}
