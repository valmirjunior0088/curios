use {
    super::*,
    crate::{Atom, Conclusion},
};

fn atom(index: u32) -> Atom {
    Atom::new(index)
}

fn nat(coefficient: u32, atoms: &[u32]) -> Summand<Natural, u32> {
    Summand {
        coefficient: Natural::from(coefficient),
        monomial: Monomial::new(atoms.iter().copied().map(atom).collect()),
        origin: atoms.first().copied().unwrap_or(0),
    }
}

fn int(coefficient: i32, atoms: &[u32]) -> Summand<Integer, u32> {
    Summand {
        coefficient: Integer::from(coefficient),
        monomial: Monomial::new(atoms.iter().copied().map(atom).collect()),
        origin: atoms.first().copied().unwrap_or(0),
    }
}

/// Every assignment of the values `0..=3` to atoms `0..count`, which is where a cancellation's claims are checked: each is a statement about every value, so each must hold at each of these.
fn valuations(count: u32) -> Vec<Vec<i64>> {
    (0..4i64.pow(count))
        .map(|mut index| {
            (0..count)
                .map(|_| {
                    let value = index % 4;
                    index /= 4;
                    value
                })
                .collect()
        })
        .collect()
}

fn natural_value(combination: &Combination<Natural, u32>, values: &[i64]) -> i64 {
    let constant = i64::try_from(u64::try_from(&combination.constant).unwrap()).unwrap();
    combination
        .summands
        .iter()
        .map(|summand| {
            let coefficient = i64::try_from(u64::try_from(&summand.coefficient).unwrap()).unwrap();
            let product: i64 = summand
                .monomial
                .atoms()
                .iter()
                .map(|atom| values[atom.index() as usize])
                .product();
            coefficient * product
        })
        .sum::<i64>()
        + constant
}

fn integer_value(combination: &Combination<Integer, u32>, values: &[i64]) -> i64 {
    let as_i64 = |value: &Integer| i64::from(i32::try_from(value).unwrap());
    combination
        .summands
        .iter()
        .map(|summand| {
            let product: i64 = summand
                .monomial
                .atoms()
                .iter()
                .map(|atom| values[atom.index() as usize])
                .product();
            as_i64(&summand.coefficient) * product
        })
        .sum::<i64>()
        + as_i64(&combination.constant)
}

// Like monomials merge by coefficient, and a merged monomial keeps its first position and its first origin — which is what makes collecting a collected combination the identity.
#[test]
fn collection_merges_like_monomials_where_they_first_appeared() {
    let collected = Combination::collect(
        Natural::from(1u32),
        [nat(2, &[1]), nat(1, &[0]), nat(3, &[1]), nat(1, &[0, 1])],
    );

    let shape = collected
        .summands
        .iter()
        .map(|summand| {
            (
                u64::try_from(&summand.coefficient).unwrap(),
                summand.monomial.atoms().to_vec(),
            )
        })
        .collect::<Vec<_>>();
    assert_eq!(
        shape,
        vec![
            (5, vec![atom(1)]),
            (1, vec![atom(0)]),
            (1, vec![atom(0), atom(1)])
        ]
    );

    let again = Combination::collect(collected.constant.clone(), collected.summands.clone());
    assert_eq!(again.summands.len(), collected.summands.len());
}

// `Nat`'s cancellation is a multiset one, and it preserves every order relation: at every valuation, the residuals compare exactly as the originals did. `2 · a + b + 3` against `a + c + 1` leaves `a + b + 2` against `c`.
#[test]
fn natural_cancellation_preserves_every_comparison() {
    let left = Combination::collect(Natural::from(3u32), [nat(2, &[0]), nat(1, &[1])]);
    let right = Combination::collect(Natural::from(1u32), [nat(1, &[0]), nat(1, &[2])]);

    let cancelled = left.clone().cancel_common(right.clone());
    assert_eq!(cancelled.progress, Progress::Summands);
    assert_eq!(
        cancelled.left.summands.len(),
        2,
        "one `a` survives on the left"
    );
    assert_eq!(
        cancelled.right.summands.len(),
        1,
        "only `c` is left on the right"
    );

    for values in valuations(3) {
        let before = natural_value(&left, &values).cmp(&natural_value(&right, &values));
        let after =
            natural_value(&cancelled.left, &values).cmp(&natural_value(&cancelled.right, &values));
        assert_eq!(before, after, "at {values:?}");
    }
}

// A pair sharing nothing is left as it was, and one sharing only a constant has its summands untouched — what the caller's reconstruction reads to keep the terms it was handed.
#[test]
fn natural_cancellation_reports_how_much_it_took() {
    let a = Combination::collect(Natural::zero(), [nat(1, &[0])]);
    let b = Combination::collect(Natural::zero(), [nat(1, &[1])]);
    assert_eq!(
        a.clone().cancel_common(b.clone()).progress,
        Progress::Nothing
    );

    let a_floored = Combination::collect(Natural::from(2u32), [nat(1, &[0])]);
    let b_floored = Combination::collect(Natural::from(1u32), [nat(1, &[1])]);
    let cancelled = a_floored.cancel_common(b_floored);
    assert_eq!(cancelled.progress, Progress::Constant);
    assert_eq!(u64::try_from(&cancelled.left.constant).unwrap(), 1);
    assert!(cancelled.right.constant.is_zero());
}

// The three verdicts over `Nat`: both sides emptied is equality, a positive constant against an emptied side is impossible whatever the other side's summands take, and a symbolic side against an emptied one is only an equivalent residual — `x` may be zero.
#[test]
fn natural_cancellation_concludes_only_what_every_value_forces() {
    let deduce = |left: Combination<Natural, u32>, right: Combination<Natural, u32>| {
        left.cancel_common(right).deduction()
    };

    let commuted = deduce(
        Combination::collect(Natural::zero(), [nat(1, &[0]), nat(1, &[1])]),
        Combination::collect(Natural::zero(), [nat(1, &[1]), nat(1, &[0])]),
    );
    assert!(matches!(commuted, Deduction::Equal));

    let floored = deduce(
        Combination::collect(Natural::from(2u32), [nat(1, &[0])]),
        Combination::collect(Natural::from(1u32), [nat(1, &[0])]),
    );
    assert!(matches!(floored, Deduction::Impossible));

    let open = deduce(
        Combination::collect(Natural::from(1u32), [nat(1, &[0])]),
        Combination::<Natural, u32>::collect(Natural::from(1u32), []),
    );
    assert!(matches!(open, Deduction::Equivalent(_)));

    let stuck = deduce(
        Combination::collect(Natural::zero(), [nat(1, &[0])]),
        Combination::collect(Natural::zero(), [nat(1, &[1])]),
    );
    assert!(matches!(stuck, Deduction::Undecided));
}

// `Int`'s split preserves the difference, and so every comparison, at every valuation; each monomial lands on the side that keeps its coefficient positive.
#[test]
fn integer_split_preserves_every_comparison() {
    let left = Combination::collect(Integer::from(2), [int(1, &[0]), int(2, &[1])]);
    let right = Combination::collect(Integer::from(5), [int(3, &[1]), int(-1, &[2])]);

    let (split_left, split_right) = left.clone().split_by_sign(right.clone());
    assert!(
        split_left
            .summands
            .iter()
            .chain(&split_right.summands)
            .all(|summand| summand.coefficient > Integer::from(0))
    );

    for values in valuations(3) {
        let before = integer_value(&left, &values).cmp(&integer_value(&right, &values));
        let after = integer_value(&split_left, &values).cmp(&integer_value(&split_right, &values));
        assert_eq!(before, after, "at {values:?}");
    }
}

// The gated split leaves a pair sharing nothing as it was, and two constants decide by value alone — no sign bound is read, since an `Int` summand may be negative.
#[test]
fn integer_cancellation_splits_only_what_is_shared() {
    let i = Combination::collect(Integer::zero(), [int(1, &[0])]);
    let j = Combination::collect(Integer::from(3), [int(1, &[1])]);
    let untouched = i.clone().cancel_common(j.clone());
    assert_eq!(untouched.progress, Progress::Nothing);
    assert!(matches!(untouched.deduction(), Deduction::Undecided));

    let three = Combination::<Integer, u32>::collect(Integer::from(3), []);
    let four = Combination::collect(Integer::from(4), []);
    assert!(matches!(
        three.clone().cancel_common(four).deduction(),
        Deduction::Impossible
    ));
    assert!(matches!(
        three.clone().cancel_common(three).deduction(),
        Deduction::Equal
    ));
}

// Everything inversion may read, conversion may too, at the same strength.
#[test]
fn a_deduction_is_a_conclusion_at_the_same_strength() {
    assert_eq!(
        Conclusion::from(Deduction::Equivalent(1)),
        Conclusion::Equivalent(1)
    );
    assert_eq!(
        Conclusion::<u32>::from(Deduction::Impossible),
        Conclusion::Impossible
    );
    assert!(matches!(
        Deduction::Equivalent(1).map(|residual| residual + 1),
        Deduction::Equivalent(2)
    ));
}
