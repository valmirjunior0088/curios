//! Classing what the carriers' readers read: which terms are handed out as atoms, which pairs a judge is asked about, and a pair read back one spelling to a class.

use {
    super::{
        Classes, atoms_of, atoms_within, classable, classed, reduce_intrinsic, refold,
        test_support::*,
    },
    crate::{Free, Intrinsic, Nat, Subterm, Term, int_same},
    std::convert::Infallible,
};

fn applied(head: u32, hint: &'static str, argument: Term) -> Term {
    Term::apply(Term::free_var(&Free::local(head, Some(hint))), [argument])
}

fn int_add(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::IntAdd(left, right))
}

fn int_mul(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::IntMul(left, right))
}

/// The atoms of a pair, through the reducer that reduces nothing, so an atom is the operand as it was written.
fn atoms(this: &Term, that: &Term) -> Vec<Term> {
    atoms_of(&mut Inert, this, that).expect("the inert reducer spends nothing")
}

/// The judge a checker's conversion is here: two applications are one where their arguments are one sum up to the order of its summands.
fn commuted(this: &Term, that: &Term) -> Result<bool, Infallible> {
    let argument = |term: &Term| match &**term {
        Subterm::Apply(apply) => Some(apply.arguments[0].term.clone()),
        _ => None,
    };
    Ok(match (argument(this), argument(that)) {
        (Some(this), Some(that)) => {
            let mut this = Nat::summands(&this);
            let mut that = Nat::summands(&that);
            this.sort_by_key(Term::structural_hash);
            that.sort_by_key(Term::structural_hash);
            this == that
        }
        _ => false,
    })
}

fn classes(atoms: &[Term]) -> Classes {
    let Ok(classes) = Classes::of(atoms, commuted);
    classes
}

#[test]
fn an_atom_is_an_operand_the_readers_do_not_read_through() {
    let (a, b, c) = (sym(1, "a"), sym(2, "b"), sym(3, "c"));
    let call = applied(10, "f", add(a.clone(), b.clone()));
    // The sum is read through, and the call is an atom whole: its argument's own sum is no atom of the pair.
    assert_eq!(
        atoms(&add(call.clone(), c.clone()), &add(c.clone(), call.clone())),
        vec![call, c]
    );
}

#[test]
fn a_pair_neither_side_of_which_is_read_through_has_no_atoms() {
    let (a, b) = (sym(1, "a"), sym(2, "b"));
    // Two calls of one head are each the whole of their side: the only pair to class would be the pair being decided.
    let this = applied(10, "f", add(a.clone(), b.clone()));
    let that = applied(10, "f", add(b, a));
    assert!(atoms(&this, &that).is_empty());
}

#[test]
fn two_calls_of_one_head_may_be_one_and_two_heads_may_not() {
    let (a, b) = (sym(1, "a"), sym(2, "b"));
    let this = applied(10, "f", add(a.clone(), b.clone()));
    let that = applied(10, "f", add(b.clone(), a.clone()));
    let other = applied(11, "g", add(b, a.clone()));
    assert!(classable(&[this.clone(), that]));
    assert!(!classable(&[this.clone(), other]));
    // A variable converts with nothing but itself, so no judge is asked about one.
    assert!(!classable(&[this, a]));
}

#[test]
fn a_judge_is_asked_only_about_pairs_that_may_be_one() {
    let (a, b) = (sym(1, "a"), sym(2, "b"));
    let atoms = [
        applied(10, "f", add(a.clone(), b.clone())),
        applied(11, "g", add(a.clone(), b.clone())),
        applied(10, "f", add(b.clone(), a.clone())),
        a,
    ];
    let mut asked = Vec::new();
    let Ok(classes) = Classes::of(&atoms, |this, that| {
        asked.push((this.clone(), that.clone()));
        commuted(this, that)
    });
    assert_eq!(asked, vec![(atoms[0].clone(), atoms[2].clone())]);
    assert!(!classes.is_empty());
}

#[test]
fn commuted_sums_inside_the_summands_of_a_sum_meet_once_classed() {
    let (a, b, c) = (sym(1, "a"), sym(2, "b"), sym(3, "c"));
    let this = add(
        applied(10, "f", add(a.clone(), b.clone())),
        applied(11, "g", c.clone()),
    );
    let that = add(applied(11, "g", c), applied(10, "f", add(b, a)));
    assert!(!Nat::same(&this, &that));

    let classes = classes(&atoms(&this, &that));
    let (this, that) = classed(&mut Inert, &this, &that, &classes)
        .expect("the inert reducer spends nothing")
        .expect("one atom joined another's class");
    assert!(Nat::same(&this, &that));
}

#[test]
fn commuted_sums_inside_the_factors_of_an_int_product_meet_once_classed() {
    let (i, j, k) = (sym(1, "i"), sym(2, "j"), sym(3, "k"));
    let call = |left: &Term, right: &Term| applied(10, "h", int_add(left.clone(), right.clone()));
    let this = int_mul(call(&i, &j), applied(11, "e", k.clone()));
    let that = int_mul(applied(11, "e", k), call(&j, &i));
    assert!(!int_same(&this, &that));

    let Ok(classes) = Classes::of(&atoms(&this, &that), |_, _| Ok::<_, Infallible>(true));
    let (this, that) = classed(&mut Inert, &this, &that, &classes)
        .expect("the inert reducer spends nothing")
        .expect("one atom joined another's class");
    assert!(int_same(&this, &that));
}

#[test]
fn a_partition_that_joins_nothing_hands_nothing_back() {
    let (a, b) = (sym(1, "a"), sym(2, "b"));
    let this = add(applied(10, "f", a.clone()), b.clone());
    let that = add(b, applied(11, "g", a));
    let classes = classes(&atoms(&this, &that));
    assert!(classes.is_empty());
    assert_eq!(classed(&mut Inert, &this, &that, &classes), Ok(None));
}

/// `operation` as its fold leaves it stuck, folded once more over its atoms as `commuted` classes them: what a checker's reducer does at a stuck fold, with the judge standing for its conversion.
fn taken_again(operation: Intrinsic) -> Option<Term> {
    let stuck: Term = reduce_intrinsic(&mut Folding, &operation)
        .expect("the folding reducer spends nothing")
        .into();
    let Subterm::Intrinsic(stuck) = &*stuck else {
        panic!("the fold left no operation to take again: {stuck}");
    };
    let atoms = atoms_within(&mut Inert, stuck).expect("the inert reducer spends nothing");
    refold(&mut Folding, stuck, &classes(&atoms)).expect("the folding reducer spends nothing")
}

/// An equality, an ordering and a truncated difference over two calls a judge holds one are each stuck to a fold that pairs atoms by identity, and each is decided once the fold is taken again over one spelling to the class. The difference is the one operation the readers do not read through whose fold still pairs atoms, so it is here by name.
///
/// Mutation-checked with the test below: a `refold` that keeps whatever the classed fold built passes this one.
#[test]
fn a_fold_over_atoms_classed_as_one_is_taken_again() {
    let (a, b) = (sym(1, "a"), sym(2, "b"));
    let this = applied(10, "f", add(a.clone(), b.clone()));
    let that = applied(10, "f", add(b, a));

    let equal = taken_again(Intrinsic::nat_eql(this.clone(), that.clone()));
    assert_eq!(equal.and_then(|term| term.as_bool()), Some(true));
    let at_most = taken_again(Intrinsic::nat_lte(this.clone(), that.clone()));
    assert_eq!(at_most.and_then(|term| term.as_bool()), Some(true));
    assert_eq!(taken_again(Intrinsic::nat_sub(this, that)), Some(lit(0)));
}

/// A fold that decides no more keeps the spelling it had: `f(a + b) < f(b + a) * c` classed is still a comparison, and the representative it would be spelled with was picked by the order a walk met the atoms in, which is no property of the term. So nothing is handed back, and the operation stands as it was written.
///
/// Mutation-checked: a `refold` that keeps whatever the classed fold built hands back a comparison over one spelling of the call.
#[test]
fn a_fold_that_decides_no_more_keeps_its_spelling() {
    let (a, b, c) = (sym(1, "a"), sym(2, "b"), sym(3, "c"));
    let this = applied(10, "f", add(a.clone(), b.clone()));
    let that = applied(10, "f", add(b, a));

    let product = Term::intrinsic(Intrinsic::NatMul(that, c.clone()));
    assert_eq!(taken_again(Intrinsic::nat_lt(this.clone(), product)), None);
    // Nothing classed is nothing taken again: two heads are two atoms to every judge.
    let other = applied(11, "g", c);
    assert_eq!(taken_again(Intrinsic::nat_eql(this, other)), None);
}
