use super::{test_support::*, *};

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

/// A formula over `atoms` atoms and no node yet.
fn over(atoms: usize) -> Formula {
    let mut formula = Formula::default();
    for _ in 0..atoms {
        formula.new_atom();
    }
    formula
}

// Agreement at every assignment is equality — De Morgan over two atoms — and a disagreement is only undecided: `x && y` against `x` differs where `y` is false, which no value may reach, so the diagram answers nothing rather than a clash.
#[test]
fn agreement_everywhere_is_equality_and_disagreement_is_nothing() {
    let mut formula = Formula::default();
    let (x, y) = (formula.new_atom(), formula.new_atom());
    let atom = |formula: &mut Formula, index, negated| formula.push(Node::Atom { index, negated });

    let left_x = atom(&mut formula, x, false);
    let left_y = atom(&mut formula, y, false);
    let both = formula.push(Node::And(left_x, left_y));
    let truth = formula.push(Node::Literal(true));
    let not_both = formula.push(Node::Xor(both, truth));
    let not_x = atom(&mut formula, x, true);
    let not_y = atom(&mut formula, y, true);
    let either_not = formula.push(Node::Or(not_x, not_y));
    assert!(diagrammed(&formula, not_both, either_not));

    let only_x = atom(&mut formula, x, false);
    assert!(!diagrammed(&formula, both, only_x));
}

// The diagram decides what the table decides: over pairs of formulas drawn at every size up to eight atoms, the two verdicts are one, and the draws reach agreement as well as its absence.
#[test]
fn the_diagram_decides_what_the_table_decides() {
    let mut draws = Draws::new();
    let mut agreements = 0;
    for _ in 0..4000 {
        let atoms = 1 + draws.below(8);
        let mut formula = over(atoms);
        let (left, right) = draws.pair(&mut formula, atoms);
        let tabled = tabled(&formula, left, right);
        assert_eq!(
            diagrammed(&formula, left, right),
            tabled,
            "{:?} at {left} against {right}",
            formula.nodes
        );
        agreements += usize::from(tabled);
    }
    assert!(agreements >= 100, "only {agreements} pairs agree");
}

// A pair is decided alike whatever numbering its atoms carry: the numbering is the diagram's order, which moves what a comparison costs and never what it answers.
#[test]
fn a_pair_is_decided_alike_under_every_numbering_of_its_atoms() {
    let mut draws = Draws::new();
    for _ in 0..1000 {
        let atoms = 2 + draws.below(7);
        let mut formula = over(atoms);
        let (left, right) = draws.pair(&mut formula, atoms);
        let mut numbering = (0..atoms).collect::<Vec<_>>();
        for at in (1..atoms).rev() {
            numbering.swap(at, draws.below(at + 1));
        }
        let mut renumbered = over(atoms);
        for node in &formula.nodes {
            renumbered.push(match *node {
                Node::Atom { index, negated } => Node::Atom {
                    index: numbering[index],
                    negated,
                },
                node => node,
            });
        }
        assert_eq!(
            diagrammed(&renumbered, left, right),
            diagrammed(&formula, left, right),
            "{:?} renumbered by {numbering:?}",
            formula.nodes
        );
    }
}

/// `(a0 || … || ak) && ¬a0 && … && ¬ak` over the `count` atoms from `first`, a contradiction: appended, its root's position.
fn contradiction(formula: &mut Formula, first: usize, count: usize) -> usize {
    let atom = |formula: &mut Formula, index, negated| formula.push(Node::Atom { index, negated });
    let mut disjunction = atom(formula, first, false);
    for index in first + 1..first + count {
        let next = atom(formula, index, false);
        disjunction = formula.push(Node::Or(disjunction, next));
    }
    let mut conjunction = disjunction;
    for index in first..first + count {
        let negation = atom(formula, index, true);
        conjunction = formula.push(Node::And(conjunction, negation));
    }
    conjunction
}

// Two contradictions over disjoint atoms each agree with `false` and with each other at every size, past any a table reaches, which is transitivity where a cap on the pair broke it; and what the comparison is charged grows as the square of the atoms, the size of the diagrams the chain of conjunctions builds, a doubling of the atoms charging under five times as much.
#[test]
fn two_contradictions_agree_at_every_size_and_cost_the_square_of_their_atoms() {
    let mut last: Option<u64> = None;
    for side in [8, 16, 32, 64] {
        let mut formula = over(2 * side);
        let left = contradiction(&mut formula, 0, side);
        let right = contradiction(&mut formula, side, side);
        let falsity = formula.push(Node::Literal(false));
        assert!(diagrammed(&formula, left, falsity), "{side} atoms a side");
        assert!(diagrammed(&formula, falsity, right), "{side} atoms a side");
        assert!(diagrammed(&formula, left, right), "{side} atoms a side");

        let spent = charged(&formula, left, right);
        if let Some(last) = last {
            assert!(
                spent < 5 * last,
                "{side} atoms a side: {spent} after {last}"
            );
        }
        last = Some(spent);
    }
}

// A refused charge stops the comparison where it stands, and the refusal is handed back as it was given.
#[test]
fn a_refused_charge_is_handed_back() {
    let mut formula = over(1);
    let atom = formula.push(Node::Atom {
        index: 0,
        negated: false,
    });
    let mut agreement = formula.agreement(atom, atom);
    assert_eq!(agreement.step(|_| Err("spent")), Err("spent"));
}

// Two leaf sets are one set however many times, and in whatever order, a leaf repeats; one leaf more on either side is undecided.
#[test]
fn leaf_sets_agree_as_sets() {
    let (a, b, c) = (Atom::new(0, 0), Atom::new(1, 1), Atom::new(2, 2));
    assert!(same_leaves(
        &[a.clone(), b.clone(), a.clone()],
        &[b.clone(), a.clone()]
    ));
    assert!(!same_leaves(&[a.clone(), b.clone()], &[a, b, c]));
}
