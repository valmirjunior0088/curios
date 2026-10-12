//! Classing what the carriers' readers read: which terms are handed out as atoms, which pairs a judge is asked about, a pair read back one spelling to a class, and an operation held to another by its own reader.

use {
    super::{
        Classes, atoms_in, atoms_of, atoms_within, classable, classed, classed_in, refold,
        test_support::*,
    },
    curios_algebra::Operation,
    curios_core::{
        Free, Intrinsic, Level, Nat, Reducer, Subterm, Term, int_same, reduce_intrinsic,
    },
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
            let (this, mut that) = (summands(&this), summands(&that));
            this.len() == that.len()
                && this.iter().all(|summand| {
                    that.iter()
                        .position(|other| other == summand)
                        .map(|at| that.swap_remove(at))
                        .is_some()
                })
        }
        _ => false,
    })
}

/// The summands of a sum, in the order they are written.
fn summands(term: &Term) -> Vec<Term> {
    match &**term {
        Subterm::Intrinsic(Intrinsic::NatAdd(left, right)) => {
            [summands(left), summands(right)].concat()
        }
        _ => vec![term.clone()],
    }
}

fn classes(atoms: &[Term]) -> Classes {
    let Ok(classes) = Classes::of(atoms, None, commuted);
    classes
}

/// Whether some atom joined another's class.
fn joined(classes: &Classes) -> bool {
    !classes.representatives.is_empty()
}

#[test]
fn an_atom_is_an_operand_that_is_no_operation_of_the_algebra() {
    let (a, b, c) = (sym(1, "a"), sym(2, "b"), sym(3, "c"));
    let call = applied(10, "f", add(a.clone(), b.clone()));
    // The sum is a layer of the walk, and the call is an atom whole: its argument's own sum is no atom of the pair.
    assert_eq!(
        atoms(&add(call.clone(), c.clone()), &add(c.clone(), call.clone())),
        vec![call, c]
    );
}

#[test]
fn a_pair_neither_side_of_which_is_read_into_has_no_atoms() {
    let (a, b) = (sym(1, "a"), sym(2, "b"));
    // Two calls of one head are each the whole of their side: the only pair to class would be the pair being decided.
    let this = applied(10, "f", add(a.clone(), b.clone()));
    let that = applied(10, "f", add(b.clone(), a.clone()));
    assert!(atoms(&this, &that).is_empty());

    // So are two quotients: each is handed out whole, which would hand the pair back as its own two atoms.
    let (p, q) = (sym(5, "p"), sym(6, "q"));
    let this = quotient(add(a.clone(), b.clone()), sym(3, "c"), p);
    let that = quotient(add(b, a), sym(3, "c"), q);
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
    let Ok(classes) = Classes::of(&atoms, None, |this, that| {
        asked.push((this.clone(), that.clone()));
        commuted(this, that)
    });
    assert_eq!(asked, vec![(atoms[0].clone(), atoms[2].clone())]);
    assert!(joined(&classes));
}

/// A classing made while another is asking is held inside the two atoms that one asked about: a call standing inside one of them is classed, and one that stands inside neither is left an atom of its own whatever the judge would say of it, which is where the two calls of a recursive function land one unfolding on.
///
/// Mutation-checked: a `Classes::of` that reads no `asked` joins the two calls that stand inside nothing asked.
#[test]
fn a_classing_inside_another_asks_only_about_what_stands_inside_the_pair_asked() {
    let (a, b, c) = (sym(1, "a"), sym(2, "b"), sym(3, "c"));
    let call = |left: &Term, right: &Term| applied(10, "f", add(left.clone(), right.clone()));
    let asked = (
        applied(11, "g", call(&a, &b)),
        applied(11, "g", call(&b, &a)),
    );
    let every = |_: &Term, _: &Term| Ok::<_, Infallible>(true);

    let Ok(inside) = Classes::of(&[call(&a, &b), call(&b, &a)], Some(&asked), every);
    assert!(joined(&inside));

    let Ok(outside) = Classes::of(&[call(&a, &c), call(&c, &a)], Some(&asked), every);
    assert!(!joined(&outside));

    // The two asked about are not classed inside their own comparison.
    let Ok(themselves) = Classes::of(&[asked.0.clone(), asked.1.clone()], Some(&asked), every);
    assert!(!joined(&themselves));
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

    let mut classes = classes(&atoms(&this, &that));
    let (this, that) = classed(&mut Inert, &this, &that, &mut classes)
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

    let Ok(mut classes) = Classes::of(&atoms(&this, &that), None, |_, _| Ok::<_, Infallible>(true));
    let (this, that) = classed(&mut Inert, &this, &that, &mut classes)
        .expect("the inert reducer spends nothing")
        .expect("one atom joined another's class");
    assert!(int_same(&this, &that));
}

#[test]
fn a_partition_that_joins_nothing_hands_nothing_back() {
    let (a, b) = (sym(1, "a"), sym(2, "b"));
    let this = add(applied(10, "f", a.clone()), b.clone());
    let that = add(b, applied(11, "g", a));
    let mut classes = classes(&atoms(&this, &that));
    assert!(!joined(&classes));
    assert_eq!(classed(&mut Inert, &this, &that, &mut classes), Ok(None));
}

/// A term read with nothing beside it is its own atom where it is no operation of the algebra, which is the pair `atoms_of` hands nothing back for.
#[test]
fn a_term_read_alone_is_its_own_atom_where_it_is_no_operation() {
    let (a, b, c) = (sym(1, "a"), sym(2, "b"), sym(3, "c"));
    let call = applied(10, "f", add(a, b));
    let alone = |term: &Term| atoms_in(&mut Inert, term).expect("the inert reducer spends nothing");
    assert_eq!(alone(&call), vec![call.clone()]);
    assert_eq!(alone(&add(call.clone(), c.clone())), vec![call, c]);
}

/// A reader that keeps its classes meets an atom a second time in another term: it joins the class the first reading opened, and a term read after is spelled with that class's representative whether it is an operation over the atom or is the atom itself.
///
/// Mutation-checked: a `join` that opens its classes afresh at each call leaves the second call an atom of its own.
#[test]
fn an_atom_read_later_joins_the_class_an_earlier_reading_opened() {
    let (a, b, c) = (sym(1, "a"), sym(2, "b"), sym(3, "c"));
    let first = applied(10, "f", add(a.clone(), b.clone()));
    let second = applied(10, "f", add(b, a));
    let alone = |term: &Term| atoms_in(&mut Inert, term).expect("the inert reducer spends nothing");
    let read = |term: &Term, classes: &mut Classes| {
        classed_in(&mut Inert, term, classes).expect("the inert reducer spends nothing")
    };

    let mut classes = Classes::default();
    let Ok(()) = classes.join(&alone(&add(first.clone(), c.clone())), None, commuted);
    assert!(!joined(&classes));
    assert_eq!(
        read(&add(first.clone(), c.clone()), &mut classes),
        add(first.clone(), c.clone())
    );

    let Ok(()) = classes.join(&alone(&second), None, commuted);
    assert!(joined(&classes));
    assert_eq!(read(&second, &mut classes), first);
    // The sum over it is the sum the first reading met, commuted, which its own reader holds one with it.
    assert_eq!(
        read(&add(c.clone(), second), &mut classes),
        add(first.clone(), c)
    );
    // The representative reads as itself.
    assert_eq!(read(&first, &mut classes), first);
}

/// An atom the classes already hold is asked about no more, neither a representative nor an atom that joined one, so reading a term again costs no question.
///
/// Mutation-checked: a `join` that skips nothing asks about the second call against the first once more.
#[test]
fn an_atom_already_classed_is_asked_about_no_more() {
    let (a, b) = (sym(1, "a"), sym(2, "b"));
    let first = applied(10, "f", add(a.clone(), b.clone()));
    let second = applied(10, "f", add(b, a));

    let mut asked = 0;
    let mut counted = |this: &Term, that: &Term| {
        asked += 1;
        commuted(this, that)
    };
    let mut classes = Classes::default();
    let Ok(()) = classes.join(&[first.clone(), second.clone()], None, &mut counted);
    let Ok(()) = classes.join(&[second, first], None, &mut counted);
    assert_eq!(asked, 1);
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
    refold(&mut Folding, stuck, &mut classes(&atoms)).expect("the folding reducer spends nothing")
}

/// An equality, an ordering and a truncated difference over two calls a judge holds one are each stuck to a fold that pairs atoms by identity, and each is decided once the fold is taken again over one spelling to the class. A difference's fold cancels what its two operands share, which no reader of a pair reads, so it is here by name.
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

/// A sign copied from a value the judge holds one with the magnitude is the magnitude's own: `copysign(f(a + b), f(b + a))` is stuck to a fold that compares the two by identity, and is `f(a + b)` once the fold is taken again over one spelling, as `copysign(f(a + b), neg(f(b + a)))` is its negation. The negation is a layer of the walk, so the atom under it is the call and not the negation of the call, which no judge is asked to hold one with a call.
#[test]
fn a_sign_copied_from_a_value_classed_with_its_magnitude_is_its_own() {
    let (a, b) = (sym(1, "a"), sym(2, "b"));
    let this = applied(10, "f", add(a.clone(), b.clone()));
    let that = applied(10, "f", add(b, a));

    assert_eq!(
        taken_again(Intrinsic::FltCopysign(this.clone(), that.clone())),
        Some(this.clone())
    );
    let negated = Term::intrinsic(Intrinsic::FltNeg(that));
    assert_eq!(
        taken_again(Intrinsic::FltCopysign(this.clone(), negated)),
        Some(Term::intrinsic(Intrinsic::FltNeg(this)))
    );
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

fn nat_and(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::NatAnd(left, right))
}

fn quotient(dividend: Term, divisor: Term, non_zero: Term) -> Term {
    Term::intrinsic(Intrinsic::NatDiv {
        dividend,
        divisor,
        non_zero,
    })
}

/// A pair as the readers class it with no checker asked: what a consumer reads where no two of the pair's atoms may be one.
fn layered(reducer: &mut impl Reducer, this: &Term, that: &Term) -> Option<(Term, Term)> {
    classed(reducer, this, that, &mut Classes::default()).expect("the reducer spends nothing")
}

/// An operation one reader reads is one atom of the reader above it wherever its own reader holds it one: the sum under `Nat/and` is commuted, no two of the pair's atoms may be one, and the two sides are spelled as one term with no checker asked.
///
/// Mutation-checked: a walk that holds no node to another hands nothing back.
#[test]
fn an_operation_under_another_readers_is_spelled_as_the_first_its_own_reader_holds_it_one_with() {
    let (a, b, c) = (sym(1, "a"), sym(2, "b"), sym(3, "c"));
    let this = nat_and(add(a.clone(), b.clone()), c.clone());
    let that = nat_and(add(b.clone(), a.clone()), c.clone());
    assert!(!classable(&atoms(&this, &that)));

    let (classed_this, classed_that) =
        layered(&mut Inert, &this, &that).expect("the commuted sum is spelled as the first");
    assert_eq!(classed_this, this);
    assert_eq!(classed_that, this);

    // Two sums the linear reader holds apart stay two atoms of the reader above.
    let other = nat_and(add(b, c.clone()), c);
    assert_eq!(layered(&mut Inert, &this, &other), None);
}

/// Two operations of two kinds are never held one, whatever their operands: a node is held only to the earlier nodes of its own operation. So two sums over them are two sums, though they stand over one set of atoms.
///
/// Mutation-checked: with any two nodes of a bucket held one, the two sums are.
#[test]
fn an_operation_is_held_only_to_operations_of_its_kind() {
    let (a, b, c) = (sym(1, "a"), sym(2, "b"), sym(3, "c"));
    let this = add(nat_and(a.clone(), b.clone()), c.clone());
    let that = add(
        Term::intrinsic(Intrinsic::NatOr(a.clone(), b.clone())),
        c.clone(),
    );
    assert_eq!(layered(&mut Inert, &this, &that), None);
}

/// A walk spells an operation over its operands and folds none: `Nat/and(f(a + b) - f(b + a), c)` is read as `Nat/and(f(a + b) - f(a + b), c)`, the difference standing. Its fold is a reducer's to take again, where it takes one (`a_fold_over_atoms_classed_as_one_is_taken_again`), and a question, answered by plain reduction, takes none, which both checkers' `a_fold_inside_a_question_is_not_taken_again` hold.
#[test]
fn a_walk_spells_an_operation_and_folds_none() {
    let (a, b, c) = (sym(1, "a"), sym(2, "b"), sym(3, "c"));
    let first = applied(10, "f", add(a.clone(), b.clone()));
    let second = applied(10, "f", add(b, a));
    let difference = |left: &Term, right: &Term| {
        Term::intrinsic(Intrinsic::nat_sub(left.clone(), right.clone()))
    };
    let this = nat_and(difference(&first, &second), c.clone());
    let that = nat_and(lit(0), c.clone());

    let mut classes = classes(&atoms(&this, &that));
    assert!(joined(&classes));
    let (classed_this, _) = classed(&mut Inert, &this, &that, &mut classes)
        .expect("the inert reducer spends nothing")
        .expect("the second call is spelled as the first");
    assert_eq!(classed_this, nat_and(difference(&first, &first), c));
}

/// An operation the table leaves an operand out of is its checker's: a quotient is handed out as an atom after its dividend and its divisor, and two quotients a judge holds one are spelled as the first is, over the first's operands.
#[test]
fn an_operation_holding_a_proof_is_handed_out_once_its_operands_are_walked() {
    let (a, b, c, d) = (sym(1, "a"), sym(2, "b"), sym(3, "c"), sym(4, "d"));
    let (p, q) = (sym(5, "p"), sym(6, "q"));
    let first = applied(10, "f", add(a.clone(), b.clone()));
    let second = applied(10, "f", add(b, a));
    let this = quotient(first.clone(), c.clone(), p);
    let that = quotient(second.clone(), c.clone(), q);
    let pair = (add(this.clone(), d.clone()), add(d.clone(), that.clone()));

    let handed = atoms(&pair.0, &pair.1);
    assert_eq!(handed, vec![first, c, this.clone(), d, second, that]);

    let Ok(mut classes) = Classes::of(&handed, None, |_, _| Ok::<_, Infallible>(true));
    let (classed_this, classed_that) = classed(&mut Inert, &pair.0, &pair.1, &mut classes)
        .expect("the inert reducer spends nothing")
        .expect("the second quotient joined the first's class");
    assert_eq!(classed_this, pair.0);
    assert!(Nat::same(&classed_this, &classed_that));
    assert!(classed_that.mentions_term(&this));
}

/// Operations over different atoms are told apart with nothing read: each of four `Nat/and`s over two variables of its own is kept in a bucket of its own, so the same four written backwards with their operands swapped are each read against their twin alone.
///
/// Mutation-checked: with every operation kept by its operation alone, the four stand in one bucket.
#[test]
fn an_operation_is_read_only_against_operations_over_its_atoms() {
    let pairs = [(1, 2), (3, 4), (5, 6), (7, 8)].map(|(x, y)| (sym(x, "x"), sym(y, "y")));
    let summed = |operations: Vec<Term>| operations.into_iter().reduce(add).expect("a summand");
    let this = summed(
        pairs
            .iter()
            .map(|(x, y)| nat_and(x.clone(), y.clone()))
            .collect(),
    );
    let that = summed(
        pairs
            .iter()
            .rev()
            .map(|(x, y)| nat_and(y.clone(), x.clone()))
            .collect(),
    );

    let mut classes = Classes::default();
    let (classed_this, classed_that) = classed(&mut Inert, &this, &that, &mut classes)
        .expect("the inert reducer spends nothing")
        .expect("each operation is spelled as its twin");
    assert!(Nat::same(&classed_this, &classed_that));

    let buckets = classes
        .nodes
        .iter()
        .filter(|((kind, _), _)| kind.1 == Operation::And)
        .collect::<Vec<_>>();
    assert_eq!(buckets.len(), 4);
    assert!(buckets.iter().all(|(_, nodes)| nodes.len() == 1));
}

/// The truth table holds two formulas one over two sets of atoms, so an operation over truth values is read against every earlier one of its operation: `p && (p || q)` is `p && (p || r)`, and an equality of the two is `true` once its fold is taken again.
///
/// Mutation-checked: with a formula kept by its atoms, as every other operation is, the two are never read against each other and the equality stays stuck.
#[test]
fn a_formula_is_read_against_every_formula_of_its_operation() {
    let (p, q, r) = (sym(1, "p"), sym(2, "q"), sym(3, "r"));
    let absorbing = |other: &Term| {
        Term::intrinsic(Intrinsic::BoolAnd(
            p.clone(),
            Term::intrinsic(Intrinsic::BoolOr(p.clone(), other.clone())),
        ))
    };
    let equal = taken_again(Intrinsic::BoolEql(absorbing(&q), absorbing(&r)));
    assert_eq!(equal.and_then(|term| term.as_bool()), Some(true));
}

/// A node under a node of its own operation is held where the law table does not declare that operation associative, and not where it does: the inner sum of `(a + b) + c` is no atom of the outer sum's reader, which flattens it, where the inner `Nat/and` of `Nat/and(Nat/and(a, b), c)` is one of the outer's.
///
/// Mutation-checked: with every node held the inner sum is held too, and with no node held under its own operation the inner `Nat/and` is not.
#[test]
fn an_operation_under_its_own_is_held_where_its_reader_does_not_flatten_it() {
    let (a, b, c) = (sym(1, "a"), sym(2, "b"), sym(3, "c"));
    let held = |term: &Term, operation: Operation| {
        let mut classes = Classes::default();
        classed_in(&mut Inert, term, &mut classes).expect("the inert reducer spends nothing");
        classes
            .nodes
            .iter()
            .filter(|((kind, _), _)| kind.1 == operation)
            .map(|(_, nodes)| nodes.len())
            .sum::<usize>()
    };
    assert_eq!(
        held(&add(add(a.clone(), b.clone()), c.clone()), Operation::Sum),
        1
    );
    assert_eq!(held(&nat_and(nat_and(a, b), c), Operation::And), 2);
}

/// A call of one universe-polymorphic name at a level of its own, over one argument.
fn applied_at(level: u32, argument: Term) -> Term {
    Term::apply(
        Term::instance_of(&Free::local(10, Some("f")), vec![Level::constant(level)]),
        [argument],
    )
}

/// Two applications apart in their heads' universe instances are asked about by their arguments, and are one where every pair is: `f.{0}(a + b)` and `f.{1}(b + a)` are put to the judge as `a + b` beside `b + a`, and never whole, where the judge would be handed two heads to compare first. The same call at an argument the judge holds apart stays an atom of its own.
///
/// Mutation-checked: with two applications asked about only where their heads are one term, the judge is asked nothing and the three are three atoms.
#[test]
fn two_calls_apart_in_a_universe_instance_are_asked_about_by_their_arguments() {
    let (a, b) = (sym(1, "a"), sym(2, "b"));
    let this = applied_at(0, add(a.clone(), b.clone()));
    let that = applied_at(1, add(b.clone(), a.clone()));
    let other = applied_at(2, a.clone());
    assert!(classable(&[this.clone(), that.clone()]));

    let mut asked = Vec::new();
    let Ok(mut classes) = Classes::of(
        &[this.clone(), that.clone(), other.clone()],
        None,
        |left, right| {
            asked.push((left.clone(), right.clone()));
            let (left, mut right) = (summands(left), summands(right));
            Ok::<_, Infallible>(
                left.len() == right.len()
                    && left.iter().all(|summand| {
                        right
                            .iter()
                            .position(|other| other == summand)
                            .map(|at| right.swap_remove(at))
                            .is_some()
                    }),
            )
        },
    );
    assert_eq!(
        asked,
        vec![
            (add(a.clone(), b.clone()), add(b.clone(), a.clone())),
            (add(a.clone(), b), a)
        ]
    );
    let read = |term: &Term, classes: &mut Classes| {
        classed_in(&mut Inert, term, classes).expect("the inert reducer spends nothing")
    };
    assert_eq!(read(&this, &mut classes), this);
    assert_eq!(read(&that, &mut classes), this);
    assert_eq!(read(&other, &mut classes), other);
}
