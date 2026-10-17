//! A stuck operation classified from its operands' classifications against one walk of the whole: the same atoms classed, the same representatives opened in the same order, every node held and spelled alike and the fold taken again alike, over the shapes a seed, a prepending and their fallbacks meet.

use {
    super::{test_support::*, *},
    curios_core::Nat,
    std::convert::Infallible,
};

fn applied(head: u32, hint: &'static str, argument: Term) -> Term {
    Term::apply(
        Term::free_var(&Free::local(u64::from(head), Some(hint))),
        [argument],
    )
}

fn nat_and(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::NatAnd(left, right))
}

fn sub(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::nat_sub(left, right))
}

fn mul(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::nat_mul(left, right))
}

fn min(left: Term, right: Term) -> Term {
    Term::intrinsic(Intrinsic::NatMin(left, right))
}

fn quotient(dividend: Term, divisor: Term, non_zero: Term) -> Term {
    Term::intrinsic(Intrinsic::NatDiv {
        dividend,
        divisor,
        non_zero,
    })
}

/// The judge a checker's conversion is here: two applications of one head are one where their arguments are one number.
fn judge(this: &Term, that: &Term) -> Result<bool, Infallible> {
    let argument = |term: &Term| match &**term {
        Subterm::Apply(apply) => Some(apply.arguments[0].term.clone()),
        _ => None,
    };
    Ok(match (argument(this), argument(that)) {
        (Some(this), Some(that)) => Nat::same(&this, &that),
        _ => false,
    })
}

/// `term`'s operation classified with `kept` beside its operands, as a reducer does at a stuck fold: the fold taken again, and what the classification found.
fn classified(
    term: &Term,
    kept: &[Option<Rc<Classification>>],
) -> (Option<Term>, Classification, Seed) {
    let Subterm::Intrinsic(operation) = &**term else {
        panic!("a stuck operation: {term}");
    };
    let mut within =
        atoms_within(&mut Inert, operation, kept).expect("the inert reducer spends nothing");
    let seed = within.seed;
    match within.classable() {
        true => {
            let Ok(()) = within.class(judge);
        }
        false => within.open(),
    }
    let (folded, classification) =
        refold(&mut Inert, operation, within, kept).expect("the inert reducer spends nothing");
    (
        folded,
        classification.expect("an operation is classified"),
        seed,
    )
}

/// The classification a reducer files for `term` once its operands' are filed: `None` where `term` is no operation or its fold, taken again, moved.
fn filed(term: &Term) -> Option<Rc<Classification>> {
    let Subterm::Intrinsic(operation) = &**term else {
        return None;
    };
    layer(operation)?;
    let kept = beside(term);
    let (folded, classification, _) = classified(term, &kept);
    folded.is_none().then(|| Rc::new(classification))
}

/// The classifications filed for each operand `term`'s walk reads.
fn beside(term: &Term) -> Vec<Option<Rc<Classification>>> {
    let Subterm::Intrinsic(operation) = &**term else {
        return Vec::new();
    };
    read_operands(operation).iter().map(filed).collect()
}

/// The representatives `classes` opened, in the order it opened them.
fn opened(classes: &Classes) -> Vec<Term> {
    let mut opened = classes
        .opened
        .iter()
        .map(|(atom, place)| (*place, atom.clone()))
        .collect::<Vec<_>>();
    opened.sort_by_key(|(place, _)| *place);
    opened.into_iter().map(|(_, atom)| atom).collect()
}

/// The nodes of each bucket, in the order held.
fn buckets(classes: &Classes) -> Vec<(Bucket, Vec<Term>)> {
    let mut buckets = classes
        .nodes
        .iter()
        .map(|(bucket, nodes)| {
            (
                *bucket,
                nodes.iter().map(|(_, node)| node.clone()).collect(),
            )
        })
        .collect::<Vec<_>>();
    buckets.sort_by_key(|(bucket, _)| hashed(bucket));
    buckets
}

/// The entries of `map`, in an order of their keys' hashes.
fn entries<K: Hash + Eq + Clone, V: Clone>(map: &Persistent<K, V>) -> Vec<(K, V)> {
    let mut entries = map
        .iter()
        .map(|(key, value)| (key.clone(), value.clone()))
        .collect::<Vec<_>>();
    entries.sort_by_key(|(key, _)| hashed(key));
    entries
}

/// Whether `term` classified from its operands' classifications is classified as one walk of it is, and where that walk started.
#[track_caller]
fn extends_alike(term: &Term) -> Seed {
    let (folded, whole, _) = classified(term, &[]);
    let (extended_fold, extended, seed) = classified(term, &beside(term));
    assert_eq!(extended_fold, folded, "the fold taken again, over {term}");
    assert_eq!(
        extended.spelled, whole.spelled,
        "the operation's spelling, over {term}"
    );
    assert_eq!(
        opened(&extended.classes),
        opened(&whole.classes),
        "the classes opened, over {term}"
    );
    assert_eq!(
        entries(&extended.classes.representatives),
        entries(&whole.classes.representatives),
        "the atoms joined, over {term}"
    );
    assert_eq!(
        buckets(&extended.classes),
        buckets(&whole.classes),
        "the nodes held, over {term}"
    );
    assert_eq!(
        entries(&extended.classes.held),
        entries(&whole.classes.held),
        "every node's class, over {term}"
    );
    assert_eq!(
        entries(&extended.classes.unread),
        entries(&whole.classes.unread),
        "the quotients, over {term}"
    );
    assert_eq!(
        entries(&extended.classes.spelled),
        entries(&whole.classes.spelled),
        "the atoms spelled, over {term}"
    );
    assert_eq!(
        entries(&extended.handed),
        entries(&whole.handed),
        "the atoms handed out, over {term}"
    );
    seed
}

/// A sum of `Nat/and(xk, yk)` nested to the left, as an infix chain parses.
fn left_sum(summands: u32) -> Term {
    (1..=summands)
        .map(|at| nat_and(sym(2 * at, "x"), sym(2 * at + 1, "y")))
        .reduce(add)
        .expect("a summand")
}

#[test]
fn a_sum_nested_to_the_left_is_classified_from_the_sum_it_extends() {
    assert_eq!(extends_alike(&left_sum(8)), Seed::First);
}

#[test]
fn a_sum_nested_to_the_right_is_classified_from_the_sum_it_holds() {
    let sum = (1..=8)
        .rev()
        .map(|at| nat_and(sym(2 * at, "x"), sym(2 * at + 1, "y")))
        .reduce(|right, left| add(left, right))
        .expect("a summand");
    assert_eq!(extends_alike(&sum), Seed::Prepended(1));
}

/// A difference whose subtrahend is a long sum, and a product whose first factor is a literal: a small operand placed before the classification of a large one.
///
/// Mutation-checked: with an atom both hold left where the seed opened it, the classes are opened in another order.
#[test]
fn a_small_operand_is_placed_before_a_large_one() {
    let n = sym(1, "n");
    assert_eq!(
        extends_alike(&sub(n.clone(), left_sum(8))),
        Seed::Prepended(1)
    );
    let chain = (0..6).fold(n.clone(), |below, at| {
        min(quotient(below, lit(7), sym(100 + at, "p")), lit(3))
    });
    assert_eq!(extends_alike(&mul(lit(7), chain)), Seed::Prepended(1));
}

/// A tower's lines as written, each naming the line before three times: every operation over the one before, quotients and literals among them.
#[test]
fn a_tower_of_lines_is_classified_from_the_line_before() {
    let mut line = sym(1, "n");
    for at in 0..6 {
        let c = min(quotient(line.clone(), lit(7), sym(100 + at, "p")), lit(3));
        let d = min(quotient(line.clone(), lit(5), sym(200 + at, "q")), lit(2));
        line = sub(sub(line.clone(), mul(c, lit(7))), mul(d, lit(5)));
        extends_alike(&line);
    }
}

/// Operations a reader holds one across the operands: the later one is spelled as the earlier, by the walk from the first operand where it is small and first.
///
/// Mutation-checked: with the seed's nodes never read against the prefix's, the sum's node keeps a spelling of its own.
#[test]
fn an_operation_held_to_one_across_operands_is_held_alike() {
    let (x, y) = (sym(50, "x"), sym(51, "y"));
    let commuted = nat_and(y.clone(), x.clone());
    let long = add(left_sum(6), nat_and(x, y));
    assert_eq!(
        extends_alike(&add(long.clone(), commuted.clone())),
        Seed::First
    );
    // Small and first beside a long sum whose node its reader holds one with it: its atoms are placed before the sum's, and its node would move the sum's spelling, so the walk of the nodes starts from the first operand.
    assert_eq!(extends_alike(&add(commuted, long)), Seed::Prepended(1));
}

/// Two quotients of one dividend by one divisor in two operands are one, the later spelled as the earlier.
#[test]
fn quotients_over_one_dividend_are_spelled_alike_across_operands() {
    let (a, b, c) = (sym(1, "a"), sym(2, "b"), sym(3, "c"));
    let first = quotient(add(a.clone(), b.clone()), c.clone(), sym(4, "p"));
    let second = quotient(add(a, b), c, sym(5, "q"));
    let long = add(left_sum(6), second);
    extends_alike(&add(long.clone(), first.clone()));
    extends_alike(&add(first, long));
}

/// A sum of calls of one head, some of them one to the judge: the checker is asked of the atoms past what a classification kept, and the classes come out as one classing of every atom.
///
/// Mutation-checked: with a prefix's atom placed before the seed's with no check that it may be one with them, a later call keeps the class the seed opened for it.
#[test]
fn calls_of_one_head_are_classed_as_one_classing_of_them_all() {
    let calls = |order: &[(u32, u32)]| {
        order
            .iter()
            .map(|(left, right)| applied(10, "g", add(sym(*left, "a"), sym(*right, "b"))))
            .reduce(add)
            .expect("a call")
    };
    let sum = calls(&[(1, 2), (3, 4), (2, 1), (5, 6), (4, 3), (7, 8)]);
    assert_eq!(extends_alike(&sum), Seed::First);
    // A call first beside a long sum of calls it may be one with: the checker would be asked, so the walk starts from the first operand.
    let small = applied(10, "g", add(sym(8, "a"), sym(7, "b")));
    assert_eq!(extends_alike(&add(small, sum)), Seed::Nothing);
}
