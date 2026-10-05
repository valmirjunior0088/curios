//! The respelling grid: whether a verdict follows how a term is spelled, stated so that both answers are checked.
//!
//! A row is two spellings conversion equates at the top of an item, and a column is a position one of them is put in: the arm of a guard written as the other, a fold's operand, the arm of a second match on a variable the guard names. Where conversion is a congruence, stable under an arm's equation and under the substitution an arm performs, every cell holds, since a term stands wherever a term it converts with stands. A cell that does not is stated here as refused, or as the kernel's refusal of what the elaborator accepted, so the refused set is a record, and deciding one is a cell moving rather than a test appearing.
//!
//! **Each row's control is its first column.** A pair conversion does not equate outside every arm is no respelling, and a cell over it would be refused for that and say nothing of the position; so each row states the equation at the top of an item, and it is held.

use super::{
    laws::{Binder, orders},
    typecheck,
};

const IMPORTS: &str = "use /std/{Nat, Int, Bool, List, Eq, Io}; use /std/Bool/{Holds, True};";

/// The binders every cell is stated under. Two proofs of one bound, two functions and a higher-order one give the atoms that convert without being identical.
const BINDERS: &[Binder] = &[
    Binder {
        name: "a",
        type_: "Nat",
        needs: &[],
    },
    Binder {
        name: "b",
        type_: "Nat",
        needs: &[],
    },
    Binder {
        name: "c",
        type_: "Nat",
        needs: &[],
    },
    Binder {
        name: "f",
        type_: "(Nat) -> Nat",
        needs: &[],
    },
    Binder {
        name: "w",
        type_: "(n: Nat, at: Holds(n < 10)) -> Nat",
        needs: &[],
    },
    Binder {
        name: "p1",
        type_: "Holds(a < 10)",
        needs: &["a"],
    },
    Binder {
        name: "p2",
        type_: "Holds(a < 10)",
        needs: &["a"],
    },
    Binder {
        name: "u",
        type_: "((Nat) -> Nat) -> Nat",
        needs: &[],
    },
    Binder {
        name: "i",
        type_: "Int",
        needs: &[],
    },
    Binder {
        name: "j",
        type_: "Int",
        needs: &[],
    },
    Binder {
        name: "v",
        type_: "(Int) -> Int",
        needs: &[],
    },
    Binder {
        name: "xs",
        type_: "List(Nat)",
        needs: &[],
    },
    Binder {
        name: "flag",
        type_: "Bool",
        needs: &[],
    },
    Binder {
        name: "g",
        type_: "(Bool) -> Nat",
        needs: &[],
    },
];

/// How many orders of [`BINDERS`] the held cells are stated at. A refused cell is stated at the first two, the declared order and its reverse.
const ORDERS: usize = 8;

/// What the compiler answers of one cell.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Answer {
    Held,
    /// Refused by the elaborator, so the kernel is not asked.
    Refused,
    /// Accepted by the elaborator and refused by the kernel.
    Kernel,
}

/// One cell: the row and column it stands at, the item's result and body, and the side it is stated on.
struct Cell {
    at: String,
    result: String,
    body: String,
    stated: Answer,
}

/// A guard and a respelling of it, with the respelling's dual where the guard is an ordering.
struct Guard {
    guard: &'static str,
    respelled: &'static str,
    dual: Option<&'static str>,
    refused: &'static [Arm],
}

/// The positions a guard's respelling is put in.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Arm {
    /// The control: the two spellings convert at the top of an item.
    Top,
    /// `Holds` of the respelling, in the guard's `true` arm, by `True/qed()`.
    Qed,
    /// The same, by `True/proved()`.
    Proved,
    /// A `match` on the respelling in a term, in the guard's `true` arm.
    Term,
    /// A `match` on the respelling in a type, in the guard's `true` arm.
    Type,
    /// `Holds` of the respelling's dual, in the guard's `false` arm.
    Dual,
}

const ARMS: &[Arm] = &[
    Arm::Top,
    Arm::Qed,
    Arm::Proved,
    Arm::Term,
    Arm::Type,
    Arm::Dual,
];

const GUARDS: &[Guard] = &[
    Guard {
        guard: "a + b < 10",
        respelled: "b + a < 10",
        dual: Some("10 <= b + a"),
        refused: &[],
    },
    Guard {
        guard: "a + b + c < 10",
        respelled: "c + a + b < 10",
        dual: Some("10 <= c + a + b"),
        refused: &[],
    },
    Guard {
        guard: "a + b < c + 10",
        respelled: "b + a < 10 + c",
        dual: Some("10 + c <= b + a"),
        refused: &[],
    },
    Guard {
        guard: "a == b",
        respelled: "b == a",
        dual: None,
        refused: &[],
    },
    Guard {
        guard: "f(a + b) < 10",
        respelled: "f(b + a) < 10",
        dual: Some("10 <= f(b + a)"),
        refused: &[Arm::Qed, Arm::Proved, Arm::Term, Arm::Type, Arm::Dual],
    },
    Guard {
        guard: "w(a, p1) < 5",
        respelled: "w(a, p2) < 5",
        dual: Some("5 <= w(a, p2)"),
        refused: &[Arm::Qed, Arm::Proved, Arm::Term, Arm::Type, Arm::Dual],
    },
    Guard {
        guard: "u((x) => x + a) < 5",
        respelled: "u((x) => a + x) < 5",
        dual: Some("5 <= u((x) => a + x)"),
        refused: &[Arm::Qed, Arm::Proved, Arm::Term, Arm::Type, Arm::Dual],
    },
    Guard {
        guard: "Nat/in_range(a + b, 1, 5)",
        respelled: "Nat/in_range(b + a, 1, 5)",
        dual: None,
        refused: &[Arm::Qed, Arm::Proved, Arm::Term, Arm::Type],
    },
];

impl Guard {
    fn cells(&self) -> Vec<Cell> {
        let Guard {
            guard, respelled, ..
        } = self;
        ARMS.iter()
            .filter_map(|arm| {
                let in_true = |fact: String| {
                    (
                        "{}".to_owned(),
                        format!("match {guard} | true => {fact} () | false => () end"),
                    )
                };
                let (result, body) = match arm {
                    Arm::Top => (
                        format!("Eq()({guard}, {respelled})"),
                        "Eq/refl()".to_owned(),
                    ),
                    Arm::Qed => in_true(format!("let _: Holds({respelled}) = True/qed();")),
                    Arm::Proved => in_true(format!("let _: Holds({respelled}) = True/proved();")),
                    Arm::Term => in_true(format!(
                        "let _: Eq()(match {respelled} | true => 0 | false => 1 end, 0) = Eq/refl();"
                    )),
                    Arm::Type => in_true(format!(
                        "let _: (match {respelled} | true => {{}} | false => Nat end) = ();"
                    )),
                    Arm::Dual => {
                        let dual = self.dual?;
                        (
                            "{}".to_owned(),
                            format!(
                                "match {guard} | true => () | false => let _: Holds({dual}) = True/qed(); () end"
                            ),
                        )
                    }
                };
                Some(Cell {
                    at: format!("`{guard}` respelled `{respelled}`, {arm:?}"),
                    result,
                    body,
                    stated: match self.refused.contains(arm) {
                        true => Answer::Refused,
                        false => Answer::Held,
                    },
                })
            })
            .collect()
    }
}

/// Two terms of one carrier that convert without being identical.
struct Atoms {
    left: &'static str,
    right: &'static str,
    /// The carrier's zero, and whether the carrier is `Nat`, whose `match` takes a literal arm.
    zero: &'static str,
    nat: bool,
    refused: &'static [Fold],
}

/// The positions two atoms that convert are put in.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Fold {
    /// The control: the two atoms convert at the top of an item.
    Top,
    /// Their `==` against `true`.
    Equal,
    /// Their `<=` against `true`.
    AtMost,
    /// Their difference against zero.
    Difference,
    /// A `match` on their `==` against its `true` arm.
    Match,
    /// The `0` arm of a `match` on one, holding the other equal to `0`.
    Switch,
}

const FOLDS: &[Fold] = &[
    Fold::Top,
    Fold::Equal,
    Fold::AtMost,
    Fold::Difference,
    Fold::Match,
    Fold::Switch,
];

const ATOMS: &[Atoms] = &[
    Atoms {
        left: "f(a + b)",
        right: "f(b + a)",
        zero: "0",
        nat: true,
        refused: &[
            Fold::Equal,
            Fold::AtMost,
            Fold::Difference,
            Fold::Match,
            Fold::Switch,
        ],
    },
    Atoms {
        left: "w(a, p1)",
        right: "w(a, p2)",
        zero: "0",
        nat: true,
        refused: &[
            Fold::Equal,
            Fold::AtMost,
            Fold::Difference,
            Fold::Match,
            Fold::Switch,
        ],
    },
    Atoms {
        left: "u((x) => x + a)",
        right: "u((x) => a + x)",
        zero: "0",
        nat: true,
        refused: &[
            Fold::Equal,
            Fold::AtMost,
            Fold::Difference,
            Fold::Match,
            Fold::Switch,
        ],
    },
    Atoms {
        left: "v(i + j)",
        right: "v(j + i)",
        zero: "+0",
        nat: false,
        refused: &[Fold::Equal, Fold::AtMost, Fold::Match],
    },
];

impl Atoms {
    fn cells(&self) -> Vec<Cell> {
        let Atoms {
            left, right, zero, ..
        } = self;
        FOLDS
            .iter()
            .filter_map(|fold| {
                let claim = |claim: String| (claim, "Eq/refl()".to_owned());
                let (result, body) = match fold {
                    Fold::Top => claim(format!("Eq()({left}, {right})")),
                    Fold::Equal => claim(format!("Eq()({left} == {right}, true)")),
                    Fold::AtMost => claim(format!("Eq()({left} <= {right}, true)")),
                    Fold::Difference => claim(format!("Eq()({left} - {right}, {zero})")),
                    Fold::Match => claim(format!(
                        "Eq()(match {left} == {right} | true => 0 | false => 1 end, 0)"
                    )),
                    Fold::Switch => {
                        if !self.nat {
                            return None;
                        }
                        (
                            "{}".to_owned(),
                            format!(
                                "match {left} | 0 => let _: Eq()({right}, 0) = Eq/refl(); () | _ => () end"
                            ),
                        )
                    }
                };
                Some(Cell {
                    at: format!("`{left}` and `{right}`, {fold:?}"),
                    result,
                    body,
                    stated: match self.refused.contains(fold) {
                        true => Answer::Refused,
                        false => Answer::Held,
                    },
                })
            })
            .collect()
    }
}

/// A guard and a second match, inside the guard's `true` arm, on a variable the guard names.
struct Substitution {
    guard: &'static str,
    variable: &'static str,
    /// The arm of the second match the fact is stated in, and the arm beside it.
    case: &'static str,
    other: &'static str,
    /// The guard with the case's value written for the variable.
    at_case: &'static str,
    refused: &'static [Under],
    kernel: &'static [Under],
}

/// The positions a guard's fact is put in under a second match.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Under {
    /// The control: the two matches nested the other way, the fact as written.
    Outside,
    /// The fact as written, by `True/qed()`.
    Qed,
    /// The fact as written, by `True/proved()`.
    Proved,
    /// The fact with the case's value written, by `True/qed()`.
    AtCase,
}

const UNDERS: &[Under] = &[Under::Outside, Under::Qed, Under::Proved, Under::AtCase];

const SUBSTITUTIONS: &[Substitution] = &[
    Substitution {
        guard: "a < List/len(xs)",
        variable: "a",
        case: "0",
        other: "_",
        at_case: "0 < List/len(xs)",
        refused: &[],
        kernel: &[],
    },
    Substitution {
        guard: "a + b < 10",
        variable: "b",
        case: "0",
        other: "_",
        at_case: "a + 0 < 10",
        refused: &[],
        kernel: &[],
    },
    Substitution {
        guard: "g(flag) < 5",
        variable: "flag",
        case: "true",
        other: "false",
        at_case: "g(true) < 5",
        refused: &[Under::AtCase],
        kernel: &[],
    },
];

impl Substitution {
    fn cells(&self) -> Vec<Cell> {
        let Substitution {
            guard,
            variable,
            case,
            other,
            at_case,
            ..
        } = self;
        UNDERS
            .iter()
            .map(|under| {
                let inside = |fact: &str, proof: &str| {
                    format!(
                        "match {guard} | true => (match {variable} | {case} => let _: Holds({fact}) = {proof}; () | {other} => () end) | false => () end"
                    )
                };
                let body = match under {
                    Under::Outside => format!(
                        "match {variable} | {case} => (match {guard} | true => let _: Holds({guard}) = True/qed(); () | false => () end) | {other} => () end"
                    ),
                    Under::Qed => inside(guard, "True/qed()"),
                    Under::Proved => inside(guard, "True/proved()"),
                    Under::AtCase => inside(at_case, "True/qed()"),
                };
                Cell {
                    at: format!("`{guard}` under a match on `{variable}`, {under:?}"),
                    result: "{}".to_owned(),
                    body,
                    stated: match (self.kernel.contains(under), self.refused.contains(under)) {
                        (true, _) => Answer::Kernel,
                        (false, true) => Answer::Refused,
                        (false, false) => Answer::Held,
                    },
                }
            })
            .collect()
    }
}

/// One program stating each cell as an item under `binders`.
fn program(binders: &str, cells: &[&Cell]) -> String {
    let items = cells
        .iter()
        .enumerate()
        .map(|(index, cell)| {
            format!(
                "let cell{index}({binders}) -> {} = {};",
                cell.result, cell.body
            )
        })
        .collect::<Vec<_>>()
        .join("\n");
    format!("{IMPORTS}\n{items}\nIo/pure(())")
}

/// What the compiler answers of `cell` alone under `binders`.
fn answer(binders: &str, cell: &Cell) -> Answer {
    match typecheck(&program(binders, &[cell])) {
        Ok(()) => Answer::Held,
        Err(error) if error.contains("the kernel refused") => Answer::Kernel,
        Err(_) => Answer::Refused,
    }
}

/// The cells the compiler puts on another side than stated, each asked alone at the declared order of the binders and at its reverse.
fn misplaced(cells: &[Cell]) -> Vec<String> {
    let mut misplaced = Vec::new();
    for binders in orders(BINDERS, 2) {
        for cell in cells {
            let answered = answer(&binders, cell);
            if answered != cell.stated {
                misplaced.push(format!(
                    "{}: stated {:?}, answered {answered:?}",
                    cell.at, cell.stated
                ));
            }
        }
    }
    misplaced
}

#[test]
fn every_respelled_guard_is_on_the_side_stated() {
    let cells = GUARDS.iter().flat_map(Guard::cells).collect::<Vec<_>>();
    let misplaced = misplaced(&cells);
    assert!(misplaced.is_empty(), "{}", misplaced.join("\n"));
}

#[test]
fn every_fold_over_two_atoms_that_convert_is_on_the_side_stated() {
    let cells = ATOMS.iter().flat_map(Atoms::cells).collect::<Vec<_>>();
    let misplaced = misplaced(&cells);
    assert!(misplaced.is_empty(), "{}", misplaced.join("\n"));
}

#[test]
fn every_guard_under_a_match_on_its_variable_is_on_the_side_stated() {
    let cells = SUBSTITUTIONS
        .iter()
        .flat_map(Substitution::cells)
        .collect::<Vec<_>>();
    let misplaced = misplaced(&cells);
    assert!(misplaced.is_empty(), "{}", misplaced.join("\n"));
}

#[test]
fn every_held_cell_holds_at_every_order_of_its_binders() {
    // The admitting direction at the whole sweep: one program per order holding every cell stated held, and on its failure one per cell, so the report names the cells an order moved.
    let cells = GUARDS
        .iter()
        .flat_map(Guard::cells)
        .chain(ATOMS.iter().flat_map(Atoms::cells))
        .chain(SUBSTITUTIONS.iter().flat_map(Substitution::cells))
        .filter(|cell| cell.stated == Answer::Held)
        .collect::<Vec<_>>();
    let held = cells.iter().collect::<Vec<_>>();
    for (order, binders) in orders(BINDERS, ORDERS).into_iter().enumerate() {
        if typecheck(&program(&binders, &held)).is_ok() {
            continue;
        }
        let moved = cells
            .iter()
            .filter(|cell| answer(&binders, cell) != Answer::Held)
            .map(|cell| cell.at.clone())
            .collect::<Vec<_>>();
        panic!(
            "at order {order} of the binders these cells do not hold:\n{}",
            moved.join("\n")
        );
    }
}
