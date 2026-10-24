use super::*;

/// The form `a·x + b·y + c` over two atoms, in canonical order, with the atoms it stands on recorded non-negative where `naturals` says so.
fn form(a: i32, b: i32, c: i32, naturals: bool) -> LinearForm {
    let (x, y) = (Atom::new(0, 0), Atom::new(1, 1));
    let terms = [(a, x), (b, y)]
        .into_iter()
        .filter(|(coefficient, _)| *coefficient != 0)
        .map(|(coefficient, atom)| (Integer::from(coefficient), Monomial::new(vec![atom])))
        .collect::<Vec<_>>();
    let nonnegative = match naturals {
        true => terms
            .iter()
            .flat_map(|(_, monomial)| monomial.atoms().to_vec())
            .collect(),
        false => Vec::new(),
    };
    LinearForm {
        constant: Integer::from(c),
        terms,
        nonnegative,
    }
}

/// What `relation` states of `form` against zero where its atoms take `values`, in the order they were handed out.
fn holds(relation: Operation, form: &LinearForm, values: [i32; 2]) -> bool {
    let value = form
        .terms
        .iter()
        .fold(form.constant.clone(), |sum, (coefficient, monomial)| {
            let product = monomial
                .atoms()
                .iter()
                .fold(Integer::from(1), |product, atom| {
                    product * Integer::from(values[atom.index() as usize])
                });
            sum + coefficient.clone() * product
        });
    let zero = Integer::from(0);
    match relation {
        Operation::Less => value < zero,
        Operation::AtMost => value <= zero,
        Operation::Equal => value == zero,
        Operation::Unequal => value != zero,
        _ => unreachable!("a comparison"),
    }
}

// The reading at its polarity states what the comparison states, at every value its atoms take: over ℤ, and over ℕ where the atoms are naturals, which is where the alignment reads a sum against zero as an order comparison.
#[test]
fn a_polarized_reading_states_the_comparison_at_every_value() {
    let relations = [
        Operation::Less,
        Operation::AtMost,
        Operation::Equal,
        Operation::Unequal,
    ];
    for naturals in [false, true] {
        let values = match naturals {
            true => 0..=4,
            false => -3..=3,
        };
        for (a, b, c) in
            (-2..=2).flat_map(|a| (-2..=2).flat_map(move |b| (-3..=3).map(move |c| (a, b, c))))
        {
            for relation in relations {
                let stated = form(a, b, c, naturals);
                let (read, polarized, negated) = stated.clone().polarized(relation);
                for x in values.clone() {
                    for y in values.clone() {
                        assert_eq!(
                            holds(read, &polarized, [x, y]) != negated,
                            holds(relation, &stated, [x, y]),
                            "{relation:?} of {stated:?} read as {read:?} of {polarized:?}, negated {negated}, at ({x}, {y})"
                        );
                    }
                }
            }
        }
    }
}

// One proposition however it is spelled, at one polarity, and its negation at the other: `x < y` and `x + 1 <= y` read alike, and `y <= x` and `y < x + 1` read as their negation.
#[test]
fn a_comparison_and_its_negation_have_one_reading_at_two_polarities() {
    let less = form(1, -1, 0, false).polarized(Operation::Less);
    let successor = form(1, -1, 1, false).polarized(Operation::AtMost);
    let dual = form(-1, 1, 0, false).polarized(Operation::AtMost);
    let dual_successor = form(-1, 1, -1, false).polarized(Operation::Less);
    assert_eq!(less, successor);
    assert!(!less.2);
    for negation in [dual, dual_successor] {
        assert_eq!((negation.0, &negation.1), (less.0, &less.1));
        assert!(negation.2);
    }

    let equal = form(1, -1, 0, false).polarized(Operation::Equal);
    let unequal = form(-1, 1, 0, false).polarized(Operation::Unequal);
    assert_eq!(
        (equal.0, &equal.1, equal.2),
        (unequal.0, &unequal.1, !unequal.2)
    );
}
