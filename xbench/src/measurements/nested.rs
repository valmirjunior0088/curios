//! What a term costs to class by its layers: an operation whose operands hold operations in turn, each commuted against its mirror, a sum of many operations of one kind, and a sum of many calls of one head.

use {
    crate::{Measure, phase, read, turn},
    curios::{TALLIES, test_support::*},
    curios_common::fill,
    std::fmt::Write,
};

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
        "use /std/{Nat, Eq, Io};\nlet towers(${binders}) -> Eq()(match ${left} <= ${right} | true => 0 | false => 0 end, 0) => Eq/refl();\nIo/pure(())",
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
/// `cargo xbench measure nested_operation_measurements` takes it: `<shape> at <size>`, one compilation through both checkers, every shape at every size turned round-robin five times after an untimed round so no row is read on a warmer machine than another, and `<shape> at <size>/accepted`, one where both checkers accepted it and nought where one refused.
///
/// # Its records
///
/// `cargo xbench report nested_operation_measurements` reads them: each figure's readings by group, and what each sitting was taken under.
pub fn nested_operation_measurements() {
    let rows = SHAPES
        .iter()
        .flat_map(|shape| {
            shape
                .sizes
                .iter()
                .map(move |size| (shape.name, *size, (shape.program)(*size)))
        })
        .collect::<Vec<_>>();

    let mut compiles = rows
        .iter()
        .map(|(shape, size, source)| {
            move || {
                let name = format!("{shape} at {size}");
                let checked = phase(TALLIES, || typecheck(source).is_ok());
                checked.read(&name, Some(*size as u64));
                let accepted = checked.answer;
                read(
                    &format!("{name}/accepted"),
                    Measure::Count,
                    Some(*size as u64),
                    u64::from(accepted),
                );
            }
        })
        .collect::<Vec<_>>();
    let mut turned = compiles
        .iter_mut()
        .map(|compile| compile as &mut dyn FnMut())
        .collect::<Vec<_>>();
    turn(5, &mut turned);
}
