//! The order a match's arms are stored in: the order the family declares its constructors, however the arms are written.

use crate::tests::typecheck;

/// Two matches whose arms are written in two orders are one term, so each checker converts the one to the other: with a catch-all, which reads only the constructors the arms name; with an arm for every constructor; and through the matrix form, a tuple's rows naming a constructor each.
#[test]
fn two_matches_whose_arms_are_written_in_two_orders_are_one_term() {
    let source = r#"use /std/{Nat, Eq};

pub induct Digit: pub Type
| d0()
| d1()
| d2()
| d3()
end

pub let f(d: Digit) -> Nat = match d | d2() => 2 | d0() => 0 | _ => 9 end;
pub let g(d: Digit) -> Nat = match d | d0() => 0 | d2() => 2 | _ => 9 end;
pub let defaulted(d: Digit) -> Eq()(f(d), g(d)) = Eq/refl();

pub let h(d: Digit) -> Nat = match d | d3() => 3 | d1() => 1 | d2() => 2 | d0() => 0 end;
pub let k(d: Digit) -> Nat = match d | d0() => 0 | d1() => 1 | d2() => 2 | d3() => 3 end;
pub let covered(d: Digit) -> Eq()(h(d), k(d)) = Eq/refl();

pub let deep(a: Digit, b: Digit) -> Nat = match (a, b) | (d3(), d0()) => 30 | (d1(), _) => 1 | _ => 9 end;
pub let shallow(a: Digit, b: Digit) -> Nat = match (a, b) | (d1(), _) => 1 | (d3(), d0()) => 30 | _ => 9 end;
pub let rows(a: Digit, b: Digit) -> Eq()(deep(a, b), shallow(a, b)) = Eq/refl();

/std/print("held")"#;

    assert_eq!(typecheck(source), Ok(()));
}
