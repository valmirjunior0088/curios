use {super::*, std::cmp::Ordering};

const GRID: [i32; 5] = [-2, -1, 0, 1, 3];

/// Every verdict a comparison could soundly reach of two values: the exact one, and each weaker one it implies.
fn verdicts(left: i32, right: i32) -> Vec<Comparison> {
    match left.cmp(&right) {
        Ordering::Less => vec![
            Comparison::Lt,
            Comparison::Le,
            Comparison::Ne,
            Comparison::Stuck,
        ],
        Ordering::Equal => vec![
            Comparison::Eq,
            Comparison::Le,
            Comparison::Ge,
            Comparison::Stuck,
        ],
        Ordering::Greater => vec![
            Comparison::Gt,
            Comparison::Ge,
            Comparison::Ne,
            Comparison::Stuck,
        ],
    }
}

fn computed(lattice: Lattice, left: i32, right: i32) -> i32 {
    match lattice {
        Lattice::Meet => left.min(right),
        Lattice::Join => left.max(right),
    }
}

/// Whether `verdict` is true of two values, read as the at-most it states.
fn holds(verdict: Comparison, lower: i32, upper: i32) -> bool {
    match verdict {
        Comparison::Lt => lower < upper,
        Comparison::Le => lower <= upper,
        _ => false,
    }
}

// The leaf a verdict keeps is the operation's value: at every pair of values and every verdict a comparison could soundly reach of them, a meet that keeps one leaf is that leaf, and a join likewise, so dropping the other changes nothing.
//
// Mutation-checked: a meet that keeps the first leaf where it is at least the second fails at `min(-2, -1)`.
#[test]
fn the_leaf_a_verdict_keeps_is_the_value() {
    for lattice in [Lattice::Meet, Lattice::Join] {
        let mut dropped = 0;
        for left in GRID {
            for right in GRID {
                for verdict in verdicts(left, right) {
                    let kept = match lattice.keeps(verdict) {
                        Kept::First => left,
                        Kept::Second => right,
                        Kept::Both => continue,
                    };
                    dropped += 1;
                    assert_eq!(
                        kept,
                        computed(lattice, left, right),
                        "{lattice:?} of {left} and {right} under {verdict:?}"
                    );
                }
            }
        }
        assert!(dropped > 0, "{lattice:?} drops a leaf the grid reaches");
    }
}

// What the operands' verdicts force of the operation is true of it: at every two operands and every term, whatever `below` and `above` conclude from verdicts a comparison could soundly reach is the order the values are in, strictly where they say so. And they conclude at all: a grid that forced nothing would hold vacuously.
#[test]
fn what_the_operands_force_of_a_lattice_operation_is_its_order() {
    for lattice in [Lattice::Meet, Lattice::Join] {
        let (mut below, mut above) = (0, 0);
        for left in GRID {
            for right in GRID {
                let value = computed(lattice, left, right);
                for term in GRID {
                    for first in verdicts(left, term) {
                        for second in verdicts(right, term) {
                            if let Some(forced) = lattice.below(&[first, second]) {
                                below += 1;
                                assert!(
                                    holds(forced, value, term),
                                    "{lattice:?}({left}, {right}) against {term}: {forced:?}"
                                );
                            }
                        }
                    }
                    for first in verdicts(term, left) {
                        for second in verdicts(term, right) {
                            if let Some(forced) = lattice.above(&[first, second]) {
                                above += 1;
                                assert!(
                                    holds(forced, term, value),
                                    "{term} against {lattice:?}({left}, {right}): {forced:?}"
                                );
                            }
                        }
                    }
                }
            }
        }
        assert!(below > 0 && above > 0, "{lattice:?} forces an order");
    }
}

// `min` is the meet and `max` the join, and nothing else is a lattice operation.
#[test]
fn min_is_the_meet_and_max_the_join() {
    assert_eq!(Operation::Min.lattice(), Some(Lattice::Meet));
    assert_eq!(Operation::Max.lattice(), Some(Lattice::Join));
    assert_eq!(Operation::And.lattice(), None);
}
