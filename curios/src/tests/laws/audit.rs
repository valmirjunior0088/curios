//! The audit of the theory conversion decides, generated from the law table's rows: the conditions a theory kept in conversion must meet — Coq Modulo Theory's, whose metatheory with strong elimination is Jouannaud and Strub's (2017) — put to both checkers at every declared law.
//!
//! Conversion stays symmetric (each law reversed), transitive (two laws chained through a side they share), and closed under substitution (each law at compound terms that change its atoms, and through a solved metavariable); and constructors stay free modulo the theory, which is what inversion reads when it concludes a clash (a case split on an equation between distinct constructors needs no arm). A finite grid is evidence about the implemented fragment, not a metatheorem, and it grows with the law table it is generated from.

use {
    super::{
        Binder, IMPORTS, Row, applied, closes, declared, name, orders, rows, spell, top_level,
        type_name,
    },
    crate::tests::typecheck,
    curios_algebra::{Carrier, Constant, Expr, Family, Law, Operation},
};

/// Every generated law with its sides swapped.
#[test]
fn every_law_holds_reversed() {
    let reversed = rows(&declared())
        .iter()
        .map(|row| {
            spell(&Law {
                left: row.law.right.clone(),
                right: row.law.left.clone(),
                ..row.law.clone()
            })
        })
        .collect::<Vec<_>>();
    if let Err(failures) = closes(&reversed) {
        panic!("{}", failures.join("\n"));
    }
}

/// Every two laws at one carrier that share a side, chained through it: `a = b` and `c = b` give `a = c`.
#[test]
fn every_two_laws_sharing_a_side_chain() {
    let rows = rows(&declared());
    let mut chains = Vec::new();
    for (at, row) in rows.iter().enumerate() {
        for other in &rows[at + 1..] {
            if row.carrier == other.carrier && row.law.right == other.law.right {
                chains.push(spell(&Law {
                    left: row.law.left.clone(),
                    right: other.law.left.clone(),
                    ..row.law.clone()
                }));
            }
        }
    }
    assert!(!chains.is_empty());
    if let Err(failures) = closes(&chains) {
        panic!("{}", failures.join("\n"));
    }
}

/// Every law at a numeric or Boolean carrier with its first variable replaced by a compound term, which changes the atoms the law's procedure reads: `x := a + 1`, `i := l + 1`, and `b := x < y`.
#[test]
fn every_law_holds_at_compound_terms() {
    let instances = rows(&declared())
        .iter()
        .filter_map(|row| {
            let (var, compound) = compound(&row.law)?;
            Some(spell(&Law {
                left: substitute(&row.law.left, &var, &compound),
                right: substitute(&row.law.right, &var, &compound),
                ..row.law.clone()
            }))
        })
        .collect::<Vec<_>>();
    assert!(!instances.is_empty());
    if let Err(failures) = closes(&instances) {
        panic!("{}", failures.join("\n"));
    }
}

/// Every operation the table declares commutative, over two atoms a side that convert without being identical — one call under two proofs of its bound — and swapped: `f(a, p1) ⋆ f(b, q1)` against `f(b, q2) ⋆ f(a, p2)`. No reader pairs such atoms by their spelling, so the row holds only where each checker classes them, and it is stated at several orders of its binders because the order an operation holds its operands in is, for some, a hash.
#[test]
fn every_commutative_law_holds_over_atoms_that_differ_in_a_proof() {
    const ORDERS: usize = 8;
    let commutative = declared()
        .into_iter()
        .filter(|(_, _, family)| *family == Family::Commutativity)
        .collect::<Vec<_>>();
    assert!(!commutative.is_empty());

    let mut carriers: Vec<Carrier> = Vec::new();
    for (carrier, _, _) in &commutative {
        if !carriers.contains(carrier) {
            carriers.push(*carrier);
        }
    }
    let mut failures = Vec::new();
    for carrier in carriers {
        let call = format!("(n: Nat, at: Holds(n < 10)) -> {}", type_name(carrier));
        let binders = [
            ("a", "Nat".to_owned(), &[][..]),
            ("b", "Nat".to_owned(), &[][..]),
            ("f", call, &[][..]),
            ("p1", "Holds(a < 10)".to_owned(), &["a"][..]),
            ("p2", "Holds(a < 10)".to_owned(), &["a"][..]),
            ("q1", "Holds(b < 10)".to_owned(), &["b"][..]),
            ("q2", "Holds(b < 10)".to_owned(), &["b"][..]),
        ];
        let binders = binders
            .iter()
            .map(|(name, type_, needs)| Binder { name, type_, needs })
            .collect::<Vec<_>>();
        let claims = commutative
            .iter()
            .filter(|(at, _, _)| *at == carrier)
            .map(|(_, operation, _)| {
                let side = |left: &str, right: &str| {
                    applied(carrier, *operation, &[left.to_owned(), right.to_owned()])
                };
                format!(
                    "Eq()({}, {})",
                    side("f(a, p1)", "f(b, q1)"),
                    side("f(b, q2)", "f(a, p2)")
                )
            })
            .collect::<Vec<_>>();
        for binders in orders(&binders, ORDERS) {
            let rows = claims
                .iter()
                .map(|claim| (binders.clone(), claim.clone()))
                .collect::<Vec<_>>();
            if let Err(found) = closes(&rows) {
                failures.push(format!(
                    "{carrier:?} under `{binders}`:\n{}",
                    found.join("\n")
                ));
            }
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n\n"));
}

/// The compound term a law's first variable is replaced by, where its carrier has one.
fn compound(law: &Law) -> Option<(Expr, Expr)> {
    let var = first_var(&law.left)?;
    let Expr::Var { carrier, .. } = var else {
        unreachable!("a variable")
    };
    let successor = |carrier, fresh| Expr::Apply {
        carrier,
        operation: Operation::Sum,
        operands: vec![
            Expr::Var {
                index: fresh,
                carrier,
            },
            Expr::Constant {
                value: Constant::One,
                carrier,
            },
        ],
    };
    let compound = match carrier {
        Carrier::Natural => successor(Carrier::Natural, 4),
        Carrier::Integer => successor(Carrier::Integer, 4),
        Carrier::Boolean => Expr::Apply {
            carrier: Carrier::Natural,
            operation: Operation::Less,
            operands: vec![
                Expr::Var {
                    index: 0,
                    carrier: Carrier::Natural,
                },
                Expr::Var {
                    index: 1,
                    carrier: Carrier::Natural,
                },
            ],
        },
        _ => return None,
    };
    Some((var, compound))
}

/// Every law whose first variable stands on both sides, with that variable on the left read through a metavariable the comparison must solve: `h(Eq/refl())` against `h(@w, e: Eq()(left[x := w], right))`. A law [`solves`] names as one that cannot must still refuse, so a change that moves one is seen, as a refused row moving is.
#[test]
fn a_metavariable_is_solved_through_every_law_that_can_solve_it() {
    let items = rows(&declared())
        .iter()
        .enumerate()
        .filter_map(|(index, row)| Some((solves(row), solving(index, row)?)))
        .collect::<Vec<_>>();
    assert!(items.iter().any(|(expected, _)| *expected));
    let check = |items: &[&String]| {
        let items = items
            .iter()
            .map(|item| item.as_str())
            .collect::<Vec<_>>()
            .join("\n");
        typecheck(&format!("{IMPORTS}\n{items}\nIo/pure(())"))
    };
    let (solving, refusing): (Vec<_>, Vec<_>) = items.iter().partition(|(expected, _)| *expected);
    let solving = solving.iter().map(|(_, item)| item).collect::<Vec<_>>();
    // The laws that solve are put to one program, and only on its failure to one each, to name them.
    let mut misplaced = Vec::new();
    if check(&solving).is_err() {
        misplaced.extend(
            solving
                .iter()
                .filter(|item| check(&[item]).is_err())
                .map(|item| format!("{item}\ndoes not solve its metavariable")),
        );
    }
    misplaced.extend(
        refusing
            .iter()
            .filter(|(_, item)| check(&[item]).is_ok())
            .map(|(_, item)| {
                format!("{item}\nsolves its metavariable, recorded as one that cannot")
            }),
    );
    assert!(misplaced.is_empty(), "{}", misplaced.join("\n\n"));
}

/// Whether a metavariable standing for one of `row`'s atoms is solved through the law. A commutativity solves at every carrier: one operand of each side pairs by identity, in either position, and the two left over are compared, which is where the metavariable meets its partner (`peel_commutative`, and `peel_comparison` and the cancellations before it). A distribution at `Nat` or `Int` solves though no operand pairs, the metavariable standing in both of two summands: the equation is linear in it, and the elaborator solves it to the quotient. A Boolean distribution does not: the truth table reads a metavariable as one more atom, so it decides an equation between known atoms and proposes no solution for an unknown one, which is incompleteness in the refusing direction.
fn solves(row: &Row) -> bool {
    !matches!(
        (row.carrier, row.law.family),
        (Carrier::Boolean, Family::Distribution(_))
    )
}

/// The item that reads `row`'s law through a metavariable, where the law's first variable stands on both of its sides.
fn solving(index: usize, row: &Row) -> Option<String> {
    let var = first_var(&row.law.left)?;
    if !mentions(&row.law.right, &var) {
        return None;
    }
    let Expr::Var { carrier, .. } = var else {
        unreachable!("a variable")
    };
    let solved = Expr::Var { index: 4, carrier };
    let (binders, claim) = spell(&Law {
        left: substitute(&row.law.left, &var, &solved),
        ..row.law.clone()
    });
    let (outer, _) = &row.source;
    let hole = name(4, carrier);
    let type_ = type_name(carrier);
    // The helper's own binders are the law's, re-stated after the solved variable, which is implicit and first because a proof binder's proposition may mention it; the outer item supplies the law's binders to it by name, in the helper's order.
    let helper = top_level(&binders)
        .into_iter()
        .filter(|binder| !binder.starts_with(&format!("{hole}:")))
        .collect::<Vec<_>>();
    let arguments = helper
        .iter()
        .map(|binder| binder.split(':').next().unwrap_or_default().trim())
        .collect::<Vec<_>>();
    let helper_binders = helper.join(", ");
    Some(format!(
        "let solve{index}({outer}) -> Bool = let h(@{hole}: {type_}, {helper_binders}, e: {claim}) -> Bool = true; h({}, Eq/refl());",
        arguments.join(", ")
    ))
}

/// A case split on an equation between two distinct constructors needs no arm, at every intrinsic case inversion distinguishes: two literals, a successor against zero, a cons against the empty word, and two packed heads. The control is an equation inversion cannot refute, which still demands its arm.
#[test]
fn distinct_constructors_stay_distinct() {
    let refuted = [
        "x: Nat",
        "Eq()(0, 1)",
        "Eq()(x + 1, 0)",
        "Eq()(+0, +1)",
        "Eq()(true, false)",
    ]
    .to_vec();
    let (binders, equations) = refuted.split_first().unwrap();
    let words = [
        ("a: Nat, xs: List(Nat)", "Eq()([a, ..xs], [])"),
        ("k: Byte, bs: Bytes", "Eq()(x[k, ..bs], x[])"),
        ("v: Bool, ts: Bits", "Eq()(b[v, ..ts], b[])"),
        ("bs: Bytes, cs: Bytes", "Eq()(x[0, ..bs], x[1, ..cs])"),
        ("ts: Bits, us: Bits", "Eq()(b[0, ..ts], b[1, ..us])"),
    ];
    let items = equations
        .iter()
        .map(|equation| (binders.to_string(), equation.to_string()))
        .chain(
            words
                .iter()
                .map(|(binders, equation)| (binders.to_string(), equation.to_string())),
        )
        .enumerate()
        .map(|(index, (binders, equation))| {
            format!("let absurd{index}({binders}, p: {equation}) -> Nat = match p end;")
        })
        .collect::<Vec<_>>();
    let source = format!("{IMPORTS}\n{}\nIo/pure(())", items.join("\n"));
    if let Err(error) = typecheck(&source) {
        let failures = items
            .iter()
            .filter(|item| typecheck(&format!("{IMPORTS}\n{item}\nIo/pure(())")).is_err())
            .cloned()
            .collect::<Vec<_>>();
        panic!("{}\n{error}", failures.join("\n"));
    }

    let control = format!(
        "{IMPORTS}\nlet unrefuted(x: Nat, p: Eq()(x, 1)) -> Nat = match p end;\nIo/pure(())"
    );
    assert!(
        typecheck(&control).is_err(),
        "an equation that may hold still demands its arm"
    );
}

fn first_var(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Var { .. } => Some(expr.clone()),
        Expr::Constant { .. } => None,
        Expr::Apply { operands, .. } => operands.iter().find_map(first_var),
    }
}

fn mentions(expr: &Expr, var: &Expr) -> bool {
    match expr {
        Expr::Var { .. } => expr == var,
        Expr::Constant { .. } => false,
        Expr::Apply { operands, .. } => operands.iter().any(|operand| mentions(operand, var)),
    }
}

fn substitute(expr: &Expr, var: &Expr, replacement: &Expr) -> Expr {
    match expr {
        Expr::Var { .. } if expr == var => replacement.clone(),
        Expr::Var { .. } | Expr::Constant { .. } => expr.clone(),
        Expr::Apply {
            carrier,
            operation,
            operands,
        } => Expr::Apply {
            carrier: *carrier,
            operation: *operation,
            operands: operands
                .iter()
                .map(|operand| substitute(operand, var, replacement))
                .collect(),
        },
    }
}
