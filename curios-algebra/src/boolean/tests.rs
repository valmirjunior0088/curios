use super::*;

/// The value `operation` gives two Boolean operands.
fn apply(operation: Operation, left: bool, right: bool) -> bool {
    match operation {
        Operation::And => left && right,
        Operation::Or => left || right,
        Operation::Xor | Operation::Unequal => left != right,
        Operation::Equal => left == right,
        _ => unreachable!("a connective"),
    }
}

// Every identity holds at every value of its operands: for each connective and each shape of observation — a literal on either side, one term twice, an operand beside its negation — the reduct the identity names has the value the connective computes, whatever the symbolic operand is.
#[test]
fn every_boolean_identity_holds_at_every_value() {
    let connectives = [
        Operation::And,
        Operation::Or,
        Operation::Xor,
        Operation::Equal,
        Operation::Unequal,
    ];
    for operation in connectives {
        for symbol in [false, true] {
            for literal in [false, true] {
                let shapes = [
                    (
                        literal,
                        symbol,
                        BooleanPair {
                            left: Some(literal),
                            ..BooleanPair::default()
                        },
                    ),
                    (
                        symbol,
                        literal,
                        BooleanPair {
                            right: Some(literal),
                            ..BooleanPair::default()
                        },
                    ),
                    (
                        symbol,
                        symbol,
                        BooleanPair {
                            same: true,
                            ..BooleanPair::default()
                        },
                    ),
                    (
                        symbol,
                        !symbol,
                        BooleanPair {
                            complementary: true,
                            ..BooleanPair::default()
                        },
                    ),
                ];
                for (left, right, pair) in shapes {
                    let Some(connected) = operation.boolean_identity(pair) else {
                        continue;
                    };
                    let value = match connected {
                        Connected::Left => left,
                        Connected::Right => right,
                        Connected::Literal(value) => value,
                        Connected::NegatedLeft => !left,
                        Connected::NegatedRight => !right,
                        Connected::Nested(_) => unreachable!("no nesting observed"),
                    };
                    assert_eq!(
                        value,
                        apply(operation, left, right),
                        "{operation:?} over {left} and {right}, observed as {pair:?}"
                    );
                }
            }
        }
    }
}

// `(a ⊕ c) ⊕ c = a` at every value, whichever side the nested `xor` stands on and whichever of its operands the other side cancels.
#[test]
fn a_nested_xor_cancels_at_every_value() {
    for a in [false, true] {
        for c in [false, true] {
            let cases = [
                (Nested::LeftFirst, (a != c, c), a),
                (Nested::LeftSecond, (c != a, c), a),
                (Nested::RightFirst, (c, a != c), a),
                (Nested::RightSecond, (c, c != a), a),
            ];
            for (nested, (left, right), kept) in cases {
                let pair = BooleanPair {
                    nested: Some(nested),
                    ..BooleanPair::default()
                };
                assert_eq!(
                    Operation::Xor.boolean_identity(pair),
                    Some(Connected::Nested(nested))
                );
                assert_eq!(left != right, kept, "{nested:?} at a = {a}, c = {c}");
            }
        }
    }
}

// Agreement at every assignment is equality — De Morgan over two atoms — and a disagreement is only undecided: `x && y` against `x` differs where `y` is false, which no value may reach, so the table answers nothing rather than a clash.
#[test]
fn agreement_everywhere_is_equality_and_disagreement_is_nothing() {
    let mut formula = Formula::default();
    let (x, y) = (formula.new_atom().unwrap(), formula.new_atom().unwrap());
    let atom = |formula: &mut Formula, index, negated| formula.push(Node::Atom { index, negated });

    let left_x = atom(&mut formula, x, false);
    let left_y = atom(&mut formula, y, false);
    let both = formula.push(Node::And(left_x, left_y));
    let truth = formula_true(&mut formula);
    let not_both = formula.push(Node::Xor(both, truth));
    let not_x = atom(&mut formula, x, true);
    let not_y = atom(&mut formula, y, true);
    let either_not = formula.push(Node::Or(not_x, not_y));
    assert!(formula.agree(not_both, either_not));

    let only_x = atom(&mut formula, x, false);
    assert!(!formula.agree(both, only_x));
}

fn formula_true(formula: &mut Formula) -> usize {
    formula.push(Node::Literal(true))
}

// The table holds eight atoms and no more, and what it costs is stated before it runs: one visit per node per assignment.
#[test]
fn the_table_declines_past_its_cap_and_states_its_cost() {
    let mut formula = Formula::default();
    for _ in 0..BOOL_ATOM_CAP {
        assert!(formula.new_atom().is_some());
    }
    assert_eq!(formula.new_atom(), None);

    let node = formula.push(Node::Literal(true));
    assert_eq!(formula.evaluation_size(), (1 << BOOL_ATOM_CAP, 1));
    assert!(formula.agree(node, node));
}

// Two leaf sets are one set however many times, and in whatever order, a leaf repeats; one leaf more on either side is undecided.
#[test]
fn leaf_sets_agree_as_sets() {
    let (a, b, c) = (Atom::new(0, 0), Atom::new(1, 1), Atom::new(2, 2));
    assert!(same_leaves(&[a, b, a], &[b, a]));
    assert!(!same_leaves(&[a, b], &[a, b, c]));
}
