//! A term classed by its layers, nested or summed, which the nested-operation suites check and the bench's measurement of them sizes.

use {curios_common::fill, std::fmt::Write};

/// `L(0) = x0` and `L(k) = Nat/and(L(k - 1), Nat/or(xk, yk))`, against its mirror with the operands of every operation commuted, closed by `Eq/refl()`.
pub fn nested(depth: usize) -> String {
    let mut binders = String::from("x0: Nat");
    let (mut left, mut right) = (String::from("x0"), String::from("x0"));
    for level in 1..=depth {
        write!(binders, ", x{level}: Nat, y{level}: Nat").expect("a string takes every write");
        left = format!("Nat/and({left}, Nat/or(x{level}, y{level}))");
        right = format!("Nat/and(Nat/or(y{level}, x{level}), {right})");
    }
    fill!(
        "use /std/{Nat, Eq, Io};\nlet nested(${binders}) -> Eq()(${left}, ${right}) => Eq/refl();\nIo/pure(())",
        binders,
        left,
        right
    )
}

/// A sum of `summands` operations `Nat/and(xk, yk)`, against the sum written in the other order with each operation commuted, closed by `Eq/refl()`: every summand is an atom of the sum and a node of its own reader's.
pub fn summed(summands: usize) -> String {
    let binders = (1..=summands)
        .map(|at| format!("x{at}: Nat, y{at}: Nat"))
        .collect::<Vec<_>>()
        .join(", ");
    let left = (1..=summands)
        .map(|at| format!("Nat/and(x{at}, y{at})"))
        .collect::<Vec<_>>()
        .join(" + ");
    let right = (1..=summands)
        .rev()
        .map(|at| format!("Nat/and(y{at}, x{at})"))
        .collect::<Vec<_>>()
        .join(" + ");
    fill!(
        "use /std/{Nat, Eq, Io};\nlet summed(${binders}) -> Eq()(${left}, ${right}) => Eq/refl();\nIo/pure(())",
        binders,
        left,
        right
    )
}
