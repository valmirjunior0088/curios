//! Forcing what the carriers' readers read: an atom's arguments brought to one spelling, and nothing a reader does not read moved.

use {
    super::{force_atoms, test_support::*},
    crate::{Free, Intrinsic, Nat, Term, int_same},
};

/// The pair as the forcing hands it back, or as it was where nothing moved. Through the reducer that reduces nothing, so what is left to bring two spellings together is the ordering alone.
fn forced(this: &Term, that: &Term) -> (Term, Term) {
    force_atoms(&mut Inert, this, that)
        .expect("the inert reducer spends nothing")
        .unwrap_or_else(|| (this.clone(), that.clone()))
}

fn applied(head: u32, hint: &'static str, argument: Term) -> Term {
    Term::apply(Term::free_var(&Free::local(head, Some(hint))), [argument])
}

fn int_add(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::IntAdd(left, right))
}

fn int_mul(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::IntMul(left, right))
}

/// Two spellings that differ only by commuting sums inside the atoms' arguments, which no reader pairs as they stand and the readers pair once they are forced. `same` is the carrier's identity as its readers read it — the atoms by identity, the order of the summands and factors around them free — since forcing orders only what stands inside an atom's arguments.
fn meet(same: fn(&Term, &Term) -> bool, this: Term, that: Term) {
    assert!(
        !same(&this, &that),
        "the readers pair the fixture's spellings already"
    );
    let (this, that) = forced(&this, &that);
    assert!(same(&this, &that), "the forced spellings are still two");
}

#[test]
fn commuted_sums_inside_the_summands_of_a_sum_meet() {
    let (a, b, c) = (sym(1, "a"), sym(2, "b"), sym(3, "c"));
    meet(
        Nat::same,
        add(
            applied(10, "f", add(a.clone(), b.clone())),
            applied(11, "g", c.clone()),
        ),
        add(applied(11, "g", c), applied(10, "f", add(b, a))),
    );
}

#[test]
fn commuted_sums_inside_the_factors_of_an_int_product_meet() {
    let (i, j, k) = (sym(1, "i"), sym(2, "j"), sym(3, "k"));
    meet(
        int_same,
        int_mul(
            applied(10, "h", int_add(i.clone(), j.clone())),
            applied(11, "e", k.clone()),
        ),
        int_mul(applied(11, "e", k), applied(10, "h", int_add(j, i))),
    );
}

#[test]
fn a_nested_atom_is_forced_under_the_head_it_is_stuck_on() {
    let (a, b) = (sym(1, "a"), sym(2, "b"));
    meet(
        Nat::same,
        add(
            applied(10, "f", applied(11, "g", add(a.clone(), b.clone()))),
            lit(1),
        ),
        add(applied(10, "f", applied(11, "g", add(b, a))), lit(1)),
    );
}

#[test]
fn a_sum_under_a_binder_in_an_argument_is_ordered() {
    let a = sym(1, "a");
    let x = Free::local(4, Some("x"));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let lambda = |body: Term| Term::func([(x, nat.clone())], body);
    meet(
        Nat::same,
        add(
            applied(10, "h", lambda(add(a.clone(), Term::free_var(&x)))),
            sym(2, "b"),
        ),
        add(
            applied(10, "h", lambda(add(Term::free_var(&x), a))),
            sym(2, "b"),
        ),
    );
}

#[test]
fn a_pair_whose_atoms_have_nothing_to_force_is_left_alone() {
    let this = add(sym(1, "a"), applied(10, "f", sym(2, "b")));
    let that = add(applied(10, "f", sym(2, "b")), sym(1, "a"));

    assert_eq!(force_atoms(&mut Inert, &this, &that), Ok(None));
}

#[test]
fn a_pair_no_reader_reads_through_is_left_to_the_congruence() {
    let difference =
        |summed: Term| Term::intrinsic(Intrinsic::NatSub(applied(10, "f", summed), sym(3, "c")));
    let this = difference(add(sym(1, "a"), sym(2, "b")));
    let that = difference(add(sym(2, "b"), sym(1, "a")));

    assert_eq!(force_atoms(&mut Inert, &this, &that), Ok(None));
}
