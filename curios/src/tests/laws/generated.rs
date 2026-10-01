//! The rows `curios-algebra`'s law table states: every family at every carrier whose operation declares it, spelled as Curios source and put to both checkers, and held against each carrier's semantics at closed values.
//!
//! A law declared at two carriers is stated at both by construction, and a family a carrier's procedure does not decide fails at that carrier rather than going unstated.

use {
    super::{closes, misplaced, semantics::holds},
    curios_algebra::{Carrier, Constant, Expr, Family, Law, Operation, TABLE},
    curios_num::Grain,
};

/// Every family the table declares, at the operation and carrier declaring it.
pub(super) fn declared() -> Vec<(Carrier, Operation, Family)> {
    TABLE
        .iter()
        .flat_map(|declared| {
            declared
                .families
                .iter()
                .map(|family| (declared.carrier, declared.operation, *family))
        })
        .collect()
}

/// One generated row: where it comes from, the law, and its spelling — binders, then claim.
pub(super) struct Row {
    pub(super) carrier: Carrier,
    pub(super) operation: Operation,
    pub(super) law: Law,
    pub(super) source: (String, String),
}

impl Row {
    pub(super) fn name(&self) -> String {
        format!(
            "{:?} at {:?}, {:?}",
            self.operation, self.carrier, self.law.family
        )
    }
}

/// The rows the families in `declared` state, in its order.
pub(super) fn rows(declared: &[(Carrier, Operation, Family)]) -> Vec<Row> {
    declared
        .iter()
        .flat_map(|(carrier, operation, family)| {
            family
                .laws(*carrier, *operation)
                .into_iter()
                .map(|law| Row {
                    carrier: *carrier,
                    operation: *operation,
                    source: spell(&law),
                    law,
                })
        })
        .collect()
}

/// The groups one program each states: every row declared at one carrier.
fn by_carrier(rows: &[Row]) -> Vec<(String, Vec<(String, String)>)> {
    let mut groups: Vec<(Carrier, Vec<(String, String)>)> = Vec::new();
    for row in rows {
        match groups
            .iter_mut()
            .find(|(carrier, _)| *carrier == row.carrier)
        {
            Some((_, group)) => group.push(row.source.clone()),
            None => groups.push((row.carrier, vec![row.source.clone()])),
        }
    }
    groups
        .into_iter()
        .map(|(carrier, group)| (format!("generated at {carrier:?}"), group))
        .collect()
}

/// A law as Curios source: its binders — the variables, then a proof for every narrowing it takes — and `Eq()(left, right)`.
pub(super) fn spell(law: &Law) -> (String, String) {
    let mut spelling = Spelling::default();
    let claim = format!(
        "Eq()({}, {})",
        spelling.expr(&law.left),
        spelling.expr(&law.right)
    );
    let mut variables = Vec::new();
    collect(&law.left, &mut variables);
    collect(&law.right, &mut variables);
    let binders = variables
        .iter()
        .map(|(index, carrier)| format!("{}: {}", name(*index, *carrier), type_name(*carrier)))
        .chain(spelling.proofs)
        .collect::<Vec<_>>()
        .join(", ");
    (binders, claim)
}

fn collect(expr: &Expr, into: &mut Vec<(usize, Carrier)>) {
    match expr {
        Expr::Var { index, carrier } => {
            if !into.contains(&(*index, *carrier)) {
                into.push((*index, *carrier));
            }
        }
        Expr::Constant { .. } => {}
        Expr::Apply { operands, .. } => {
            for operand in operands {
                collect(operand, into);
            }
        }
    }
}

/// A variable's name: its carrier's letters in order, the fourth kept for a word's element.
pub(super) fn name(index: usize, carrier: Carrier) -> &'static str {
    let names: [&str; 5] = match carrier {
        Carrier::Natural => ["x", "y", "z", "a", "w"],
        Carrier::Integer => ["i", "j", "k", "l", "q"],
        Carrier::Boolean => ["b", "c", "d", "v", "u"],
        Carrier::Byte => ["m", "n", "o", "k", "r"],
        Carrier::Float => ["f", "g", "h", "e", "s"],
        Carrier::Packed(Grain::X) => ["bs", "cs", "ds", "es", "fs"],
        Carrier::Packed(Grain::B) => ["ts", "us", "ws", "vs", "rs"],
        Carrier::List => ["xs", "ys", "zs", "ws", "vs"],
    };
    names[index]
}

pub(super) fn type_name(carrier: Carrier) -> &'static str {
    match carrier {
        Carrier::Natural => "Nat",
        Carrier::Integer => "Int",
        Carrier::Boolean => "Bool",
        Carrier::Byte => "Byte",
        Carrier::Float => "Flt",
        Carrier::Packed(Grain::X) => "Bytes",
        Carrier::Packed(Grain::B) => "Bits",
        Carrier::List => "List(Nat)",
    }
}

/// The spelling of one law's terms, and the proof binders its narrowings asked for.
#[derive(Default)]
struct Spelling {
    proofs: Vec<String>,
}

impl Spelling {
    fn expr(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Var { index, carrier } => name(*index, *carrier).to_owned(),
            Expr::Constant { value, carrier } => constant(*value, *carrier).to_owned(),
            Expr::Apply {
                carrier,
                operation,
                operands,
            } => {
                // The one operand a spelling cannot type on its own: an empty list's length names its element.
                if let (Carrier::List, Operation::Length, [Expr::Constant { .. }]) =
                    (carrier, operation, operands.as_slice())
                {
                    return "List/len(@Nat, [])".to_owned();
                }
                let operands = operands
                    .iter()
                    .map(|operand| self.expr(operand))
                    .collect::<Vec<_>>();
                self.apply(*carrier, *operation, &operands)
            }
        }
    }

    /// `operation` at `carrier` over spelled operands: its surface form, and for a narrowing a proof binder stating the precondition, passed by name.
    fn apply(&mut self, carrier: Carrier, operation: Operation, operands: &[String]) -> String {
        let infix = |symbol: &str| format!("({} {symbol} {})", operands[0], operands[1]);
        let call = |function: &str| format!("{function}({})", operands.join(", "));
        let concat = |open: &str| format!("{open}..{}, ..{}]", operands[0], operands[1]);
        let append = |open: &str| format!("{open}..{}, {}]", operands[0], operands[1]);
        match (carrier, operation) {
            (_, Operation::Sum) => infix("+"),
            (_, Operation::Difference) => infix("-"),
            (_, Operation::Product) => infix("*"),
            (_, Operation::Quotient) => infix("/"),
            (_, Operation::Remainder) => infix("%"),
            (_, Operation::Equal) => infix("=="),
            (_, Operation::Unequal) => infix("!="),
            (_, Operation::Less) => infix("<"),
            (_, Operation::AtMost) => infix("<="),
            (Carrier::Boolean, Operation::And) => infix("&&"),
            (Carrier::Boolean, Operation::Or) => infix("||"),
            (Carrier::Boolean, Operation::Xor) => call("Bool/xor"),
            (Carrier::Natural, Operation::And) => call("Nat/and"),
            (Carrier::Natural, Operation::Or) => call("Nat/or"),
            (Carrier::Natural, Operation::Xor) => call("Nat/xor"),
            (Carrier::Natural, Operation::ShiftLeft) => call("Nat/shl"),
            (Carrier::Natural, Operation::ShiftRight) => call("Nat/shr"),
            (Carrier::Packed(Grain::X), Operation::Concat) => concat("x["),
            (Carrier::Packed(Grain::B), Operation::Concat) => concat("b["),
            (Carrier::List, Operation::Concat) => concat("["),
            (Carrier::Packed(Grain::X), Operation::Append) => append("x["),
            (Carrier::Packed(Grain::B), Operation::Append) => append("b["),
            (Carrier::List, Operation::Append) => append("["),
            (Carrier::Packed(Grain::X), Operation::Length) => call("Bytes/len"),
            (Carrier::Packed(Grain::B), Operation::Length) => call("Bits/len"),
            (Carrier::List, Operation::Length) => call("List/len"),
            (
                Carrier::Natural,
                Operation::Conversion {
                    from: Carrier::Byte,
                },
            ) => call("Byte/to_nat"),
            (
                Carrier::Integer,
                Operation::Conversion {
                    from: Carrier::Natural,
                },
            ) => call("Nat/to_int"),
            (
                Carrier::Packed(Grain::X),
                Operation::Conversion {
                    from: Carrier::Float,
                },
            ) => call("Flt/to_le_bytes"),
            (
                Carrier::Packed(Grain::B),
                Operation::Conversion {
                    from: Carrier::Packed(Grain::X),
                },
            ) => call("Bytes/to_bits"),
            (
                Carrier::Natural,
                Operation::Conversion {
                    from: Carrier::Integer,
                },
            ) => self.narrowing(
                "Int/to_nat",
                &operands[0],
                format!("Int/ge({}, +0)", operands[0]),
            ),
            (
                Carrier::Byte,
                Operation::Conversion {
                    from: Carrier::Natural,
                },
            ) => self.narrowing(
                "Nat/to_byte",
                &operands[0],
                format!("Nat/lt({}, 256)", operands[0]),
            ),
            (
                Carrier::Float,
                Operation::Conversion {
                    from: Carrier::Packed(Grain::X),
                },
            ) => self.narrowing(
                "Flt/of_le_bytes",
                &operands[0],
                format!("Nat/eql(Bytes/len({}), 8)", operands[0]),
            ),
            (
                Carrier::Packed(Grain::X),
                Operation::Conversion {
                    from: Carrier::Packed(Grain::B),
                },
            ) => self.narrowing(
                "Bits/to_bytes",
                &operands[0],
                format!("Nat/eql(Nat/rem(Bits/len({}), 8), 0)", operands[0]),
            ),
            _ => panic!("no spelling for {operation:?} at {carrier:?}"),
        }
    }

    /// A narrowing's call, with a proof binder stating what it asks of its operand.
    fn narrowing(&mut self, function: &str, operand: &str, holds: String) -> String {
        let proof = format!("ok{}", self.proofs.len());
        self.proofs.push(format!("{proof}: Holds({holds})"));
        format!("{function}({operand}, @{proof})")
    }
}

fn constant(value: Constant, carrier: Carrier) -> &'static str {
    match (value, carrier) {
        (Constant::Zero, Carrier::Natural) => "0",
        (Constant::One, Carrier::Natural) => "1",
        (Constant::Zero, Carrier::Integer) => "+0",
        (Constant::One, Carrier::Integer) => "+1",
        (Constant::True, Carrier::Boolean) => "true",
        (Constant::False, Carrier::Boolean) => "false",
        (Constant::Empty, Carrier::Packed(Grain::X)) => "x[]",
        (Constant::Empty, Carrier::Packed(Grain::B)) => "b[]",
        (Constant::Empty, Carrier::List) => "[]",
        _ => panic!("no spelling for {value:?} at {carrier:?}"),
    }
}

#[test]
fn every_generated_law_closes_by_refl() {
    let mut failed = Vec::new();
    for (name, group) in by_carrier(&rows(&declared())) {
        if let Err(failures) = closes(&group) {
            failed.push(format!("{name}:\n{}", failures.join("\n")));
        }
    }
    assert!(failed.is_empty(), "{}", failed.join("\n"));
}

#[test]
fn every_generated_law_is_a_fit_for_refl() {
    let found = by_carrier(&rows(&declared()))
        .into_iter()
        .flat_map(|(name, group)| misplaced(&name, &group, group.len()))
        .collect::<Vec<_>>();
    assert!(found.is_empty(), "{}", found.join("\n"));
}

#[test]
fn every_generated_law_holds_at_closed_values() {
    let failed = rows(&declared())
        .iter()
        .filter_map(|row| {
            holds(&row.law)
                .err()
                .map(|error| format!("{}: `{}`: {error}", row.name(), row.source.1))
        })
        .collect::<Vec<_>>();
    assert!(failed.is_empty(), "{}", failed.join("\n"));
}

#[test]
fn a_family_the_table_withdraws_takes_its_rows_with_it() {
    let every = declared();
    let (carrier, operation, family) = every[0];
    let rest = every[1..].to_vec();

    let withdrawn = family.laws(carrier, operation);
    let claims = |declared: &[(Carrier, Operation, Family)]| {
        rows(declared)
            .into_iter()
            .map(|row| row.law)
            .collect::<Vec<_>>()
    };
    let (all, remaining) = (claims(&every), claims(&rest));
    assert_eq!(all.len(), remaining.len() + withdrawn.len());
    assert!(withdrawn.iter().all(|law| !remaining.contains(law)));
}

#[test]
fn a_family_declared_where_nothing_decides_it_fails_at_that_carrier() {
    // `and` at ℕ is associative, which the closed values confirm, and conversion does not decide it: declared there, the family fails through the checkers and not in its semantics.
    let undecided = rows(&[(Carrier::Natural, Operation::And, Family::Associativity)]);
    assert!(undecided.iter().all(|row| holds(&row.law).is_ok()));
    let group = undecided
        .iter()
        .map(|row| row.source.clone())
        .collect::<Vec<_>>();
    assert!(closes(&group).is_err());
}
