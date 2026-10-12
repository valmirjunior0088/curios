//! What a term costs to class by its layers: an operation whose operands hold operations in turn, each commuted against its mirror, and a sum of many operations of one kind.
//!
//! A node is held to another by its own reader over operands spelled already, so each layer of a term is read once, from its leaves, and only against the nodes that stand over its atoms. Two nodes put to a checker as two atoms are would have their operands classed again inside each comparison, and the cost would double with every level; a node read against every earlier node of its operation would be read pair by pair at every fold above it. The fixtures hold the two shapes at sizes neither answers at; `nested_operation_measurements` reports what each size costs.

use {
    crate::tests::typecheck,
    std::{fmt::Write, time::Instant},
};

/// `L(0) = x0` and `L(k) = Nat/and(L(k - 1), Nat/or(xk, yk))`, against its mirror with the operands of every operation commuted, closed by `Eq/refl()`.
fn nested(depth: usize) -> String {
    let mut binders = String::from("x0: Nat");
    let (mut left, mut right) = (String::from("x0"), String::from("x0"));
    for level in 1..=depth {
        write!(binders, ", x{level}: Nat, y{level}: Nat").expect("a string takes every write");
        left = format!("Nat/and({left}, Nat/or(x{level}, y{level}))");
        right = format!("Nat/and(Nat/or(y{level}, x{level}), {right})");
    }
    format!(
        "use /std/{{Nat, Eq, Io}};\nlet nested({binders}) -> Eq()({left}, {right}) = Eq/refl();\nIo/pure(())"
    )
}

/// A sum of `summands` operations `Nat/and(xk, yk)`, against the sum written in the other order with each operation commuted, closed by `Eq/refl()`: every summand is an atom of the sum and a node of its own reader's.
fn summed(summands: usize) -> String {
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
    format!(
        "use /std/{{Nat, Eq, Io}};\nlet summed({binders}) -> Eq()({left}, {right}) = Eq/refl();\nIo/pure(())"
    )
}

#[test]
fn an_operation_nested_and_commuted_at_every_level_is_one_with_its_mirror() {
    if let Err(error) = typecheck(&nested(32)) {
        panic!("{error}");
    }
}

#[test]
fn a_sum_of_operations_each_commuted_is_one_with_the_sum_written_backwards() {
    if let Err(error) = typecheck(&summed(64)) {
        panic!("{error}");
    }
}

/// One shape the measurement reads: its name, its program at a size, and the sizes it is read at.
struct Shape {
    name: &'static str,
    program: fn(usize) -> String,
    sizes: &'static [usize],
}

const SHAPES: [Shape; 2] = [
    Shape {
        name: "nested",
        program: nested,
        sizes: &[8, 16, 32, 64],
    },
    Shape {
        name: "summed",
        program: summed,
        sizes: &[16, 64, 256],
    },
];

/// What classing a term by its layers costs as the term grows.
///
/// # How to take it
///
/// ```sh
/// cargo test --package curios --lib -- --ignored --nocapture nested_operation_measurements
/// ```
///
/// Timed, in the test profile: five rounds, each over every shape and size so no row is read on a warmer machine than another, each reading one compilation through both checkers. Median of the five, with their span.
///
/// # What it last printed
///
/// Taken on top of `0db77a9a9` on `x86_64-unknown-linux-gnu`, on a machine under load. Every row is accepted.
///
/// | shape | size | compile |
/// | --- | --- | --- |
/// | nested | 8 | 0.30 s [0.10–0.43] |
/// | nested | 16 | 0.24 s [0.11–0.43] |
/// | nested | 32 | 0.35 s [0.16–0.58] |
/// | nested | 64 | 0.70 s [0.64–0.84] |
/// | summed | 16 | 0.27 s [0.20–0.36] |
/// | summed | 64 | 0.94 s [0.65–1.40] |
/// | summed | 256 | 14.80 s [12.99–17.06] |
///
/// **A nested term costs its depth and no doubling of it.** **A long sum's cost is mostly the reducers' own**: every stuck sum walks the sum beneath it, whether or not a summand is respelled, which `documentation/roadmap/arithmetic/000-findings.md` holds with the readings of the same sum unrespelled.
#[test]
#[ignore = "measurement, timed: reports what a size costs rather than asserting"]
fn nested_operation_measurements() {
    const ROUNDS: usize = 5;
    let rows = SHAPES
        .iter()
        .flat_map(|shape| {
            shape
                .sizes
                .iter()
                .map(move |size| (shape.name, *size, (shape.program)(*size)))
        })
        .collect::<Vec<_>>();
    let mut readings = vec![Vec::with_capacity(ROUNDS); rows.len()];
    let mut accepted = vec![true; rows.len()];
    for _ in 0..ROUNDS {
        for (at, (_, _, source)) in rows.iter().enumerate() {
            let start = Instant::now();
            accepted[at] &= typecheck(source).is_ok();
            readings[at].push(start.elapsed().as_secs_f64());
        }
    }
    println!("shape    size  accepted  compile, median [span]");
    for (at, (shape, size, _)) in rows.iter().enumerate() {
        readings[at].sort_by(f64::total_cmp);
        let (least, median, most) = (
            readings[at][0],
            readings[at][ROUNDS / 2],
            readings[at][ROUNDS - 1],
        );
        println!(
            "{shape:<8} {size:<5} {:<9} {median:.2} s [{least:.2}–{most:.2}]",
            accepted[at]
        );
    }
}
