//! What a term costs to class by its layers: an operation whose operands hold operations in turn, each commuted against its mirror, a sum of many operations of one kind, and a sum of many calls of one head.
//!
//! A node is held to another by its own reader over operands spelled already, so each layer of a term is read once, from its leaves, and only against the nodes that stand over its atoms. Two nodes put to a checker as two atoms are would have their operands classed again inside each comparison, and the cost would double with every level; a node read against every earlier node of its operation would be read pair by pair at every fold above it. A stuck sum is classified from the sum it extends, so a sum of calls of one head puts each pair of calls to its checker once, where classifying every level from nothing asks the classes below again. The fixtures hold the three shapes at sizes none answers at otherwise; `nested_operation_measurements` reports what each size of the first two costs.

use {
    crate::tests::typecheck,
    curios_utilities::fill,
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
    fill!(
        "use /std/{Nat, Eq, Io};\nlet nested(${binders}) -> Eq()(${left}, ${right}) = Eq/refl();\nIo/pure(())",
        binders,
        left,
        right
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
    fill!(
        "use /std/{Nat, Eq, Io};\nlet summed(${binders}) -> Eq()(${left}, ${right}) = Eq/refl();\nIo/pure(())",
        binders,
        left,
        right
    )
}

/// A sum of `summands` calls `g(xk)` of one head, against the sum written in the other order, closed by `Eq/refl()`: any two calls may be one, so every call is put to its checker against the classes opened before it.
fn called(summands: usize) -> String {
    let binders = (1..=summands)
        .map(|at| format!("x{at}: Nat"))
        .collect::<Vec<_>>()
        .join(", ");
    let left = (1..=summands)
        .map(|at| format!("g(x{at})"))
        .collect::<Vec<_>>()
        .join(" + ");
    let right = (1..=summands)
        .rev()
        .map(|at| format!("g(x{at})"))
        .collect::<Vec<_>>()
        .join(" + ");
    fill!(
        "use /std/{Nat, Eq, Io};\nlet called(g: (Nat) -> Nat, ${binders}) -> Eq()(${left}, ${right}) = Eq/refl();\nIo/pure(())",
        binders,
        left,
        right
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

/// Each stuck sum of calls is classified from the sum it extends, so each pair of calls is put to its checker once; classified from nothing at every level, the classes of every sum below are asked again, and the compile takes twenty times as long.
#[test]
fn a_sum_of_calls_of_one_head_is_one_with_the_sum_written_backwards() {
    if let Err(error) = typecheck(&called(256)) {
        panic!("{error}");
    }
}

/// `L(0) = x0` and `L(k)` built from `L(k - 1)` by `step` at `yk`, beside its twin `R` over `u0` and `vk`, under a `match` on `L(depth) <= R(depth)` whose arms are both `0`, claimed `0` by `Eq/refl()`, which the stuck `match` refuses: every operation of the two towers is stuck beside every other of its kind.
fn towers(depth: usize, step: fn(&str, &str) -> String) -> String {
    let mut binders = String::from("x0: Nat, u0: Nat");
    let (mut left, mut right) = (String::from("x0"), String::from("u0"));
    for level in 1..=depth {
        write!(binders, ", y{level}: Nat, v{level}: Nat").expect("a string takes every write");
        left = step(&left, &format!("y{level}"));
        right = step(&right, &format!("v{level}"));
    }
    fill!(
        "use /std/{Nat, Eq, Io};\nlet towers(${binders}) -> Eq()(match ${left} <= ${right} | true => 0 | false => 0 end, 0) = Eq/refl();\nIo/pure(())",
        binders,
        left,
        right
    )
}

/// Two towers of remainders: two remainders of one dividend by one divisor are one with no question put, so no two of them are put to conversion.
fn remainders(depth: usize) -> String {
    towers(depth, |inner, at| format!("{inner} % ({at} + 1)"))
}

/// The control: two towers of conjunctions, nodes their readers hold, at the same depths.
fn conjunctions(depth: usize) -> String {
    towers(depth, |inner, at| format!("Nat/and({inner}, {at})"))
}

/// One shape the measurement reads: its name, its program at a size, and the sizes it is read at.
struct Shape {
    name: &'static str,
    program: fn(usize) -> String,
    sizes: &'static [usize],
}

const SHAPES: [Shape; 4] = [
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
    Shape {
        name: "remainders",
        program: remainders,
        sizes: &[12, 24],
    },
    Shape {
        name: "conjunctions",
        program: conjunctions,
        sizes: &[12, 24],
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
/// Taken at `9c543476a` on `x86_64-unknown-linux-gnu`, at a load average under two. The nested and summed rows are accepted, and both towers are refused, as their claim is.
///
/// | shape | size | compile |
/// | --- | --- | --- |
/// | nested | 8 | 0.01 s [0.01–0.05] |
/// | nested | 16 | 0.02 s [0.02–0.02] |
/// | nested | 32 | 0.03 s [0.03–0.03] |
/// | nested | 64 | 0.05 s [0.05–0.05] |
/// | summed | 16 | 0.02 s [0.02–0.02] |
/// | summed | 64 | 0.06 s [0.06–0.07] |
/// | summed | 256 | 0.46 s [0.45–0.55] |
/// | remainders | 12 | 0.09 s [0.09–0.11] |
/// | remainders | 24 | 0.45 s [0.45–0.45] |
/// | conjunctions | 12 | 0.03 s [0.03–0.03] |
/// | conjunctions | 24 | 0.16 s [0.16–0.16] |
///
/// **A nested term costs its depth and no doubling of it.** **A long sum grows by 7.7 times from 64 summands to 256**, each stuck fold extending the classification the fold beneath it filed (`curios_analysis::Classification`). **Two towers of remainders grow as two of conjunctions do**, by five times from twelve to twenty-four against five and a third: no remainder is put to a checker beside another.
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
