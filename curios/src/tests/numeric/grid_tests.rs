//! The differential grid: every `Nat` and `Int` operation, run by the compiled program across the i31, limb and sign boundaries, against `curios-num`'s carrier arithmetic.

use {
    crate::tests::run_text,
    curios_num::{Floating, Integer, Natural},
    curios_runtime::MockHost,
};

/// The program under test: it reads `x y` pairs separated by `;` from its one line of input, so no operand is a literal a folder could reach, and prints one line of results per pair, in the order [`expected`] lists them.
const PROGRAM: &str = r#"
use /std/{Str, Nat, Int, Flt, Byte, Bytes, List, Option, Bool, Io, print};

let int(s: Str) -> Int =
    Option/unwrap_or(Int/of_str(s), +0);

let flag(b: Bool) -> Str =
    match b | true => "1" | false => "0" end;

let bits(f: Flt) -> Str =
    Bytes/fold(Flt/to_le_bytes(f), "", (b, acc) => Str/concat(Str/concat(acc, ":"), Nat/to_str(Byte/to_nat(b))));

let row(pair: Str) -> Str =
    let (xs, ys) = Option/unwrap_or(Str/split_once(pair, " "), (pair, "+1"));
    let x = int(xs);
    let y = int(ys);
    let n = Int/abs(x);
    let m = Int/abs(y);
    let k = Nat/and(m, 63);
    Str/join(" ", [
        Int/to_str(x + y),
        Int/to_str(x - y),
        Int/to_str(x * y),
        match y == +0 | true => "-" | false => Int/to_str(x / y) end,
        match y == +0 | true => "-" | false => Int/to_str(x % y) end,
        Int/to_str(Int/and(x, y)),
        Int/to_str(Int/or(x, y)),
        Int/to_str(Int/xor(x, y)),
        Int/to_str(Int/shl(x, k)),
        Int/to_str(Int/shr(x, k)),
        flag(x < y),
        flag(x <= y),
        flag(x == y),
        Nat/to_str(n - m),
        match m | 0 => "-" | mp + 1 => Nat/to_str(n / (mp + 1)) end,
        match m | 0 => "-" | mp + 1 => Nat/to_str(n % (mp + 1)) end,
        Nat/to_str(Nat/shl(n, k)),
        Nat/to_str(Nat/shr(n, k)),
        bits(Int/to_flt(x)),
        Int/to_str(Option/unwrap_or(Flt/try_to_int(Int/to_flt(x)), +0)),
    ]);

let input = /std/read()!;
match input: (_) => Io({})
| some(bytes) =>
    match Str/of_bytes(bytes): (_) => Io({})
    | some(s) => print(Str/join("\n", List/map(Str/split(Str/trim(s), ";"), row)))
    | none() => print("invalid utf-8")
    end
| none() => print("no input")
end
"#;

/// Every value the grid pairs: each side of the i31's two edges and of a limb, two and three limbs, a value past 128 bits with low bits set, at both signs.
fn values() -> Vec<Integer> {
    let two = |k: u32| {
        Integer::from(1u32)
            .shl_within(&Natural::from(k), u64::MAX)
            .expect("a power of two")
    };
    let magnitudes = [
        Integer::from(0u32),
        Integer::from(1u32),
        Integer::from(7u32),
        two(30) - Integer::from(1u32),
        two(30),
        two(30) + Integer::from(1u32),
        two(32) - Integer::from(1u32),
        two(32) + Integer::from(1u32),
        two(64) + Integer::from(1u32),
        Integer::from(3u64.pow(40)) * Integer::from(7u32),
        two(128) - Integer::from(1u32),
        two(130) + Integer::from(12345u32),
    ];
    magnitudes
        .iter()
        .flat_map(|magnitude| match magnitude.is_zero() {
            true => vec![magnitude.clone()],
            false => vec![magnitude.clone(), -magnitude.clone()],
        })
        .collect()
}

/// An `Int` as the program prints it: its sign always written.
fn signed(value: &Integer) -> String {
    format!("{value:+}")
}

/// What the program should print for one pair, by `curios-num`'s carrier methods — the arithmetic every folder shares.
fn expected(x: &Integer, y: &Integer) -> String {
    let n = x.magnitude();
    let m = y.magnitude();
    let k = m.rem(&Natural::from(64u32)).expect("a nonzero divisor");
    let flag = |b: bool| String::from(if b { "1" } else { "0" });
    let quotient = |result: Result<Integer, _>| result.map_or("-".into(), |q| signed(&q));
    let natural = |result: Result<Natural, _>| result.map_or("-".into(), |q| format!("{q:?}"));
    let float = Floating::of_integer(x);
    let bytes = float
        .to_bits()
        .to_le_bytes()
        .iter()
        .map(|byte| format!(":{byte}"))
        .collect::<String>();

    [
        signed(&(x.clone() + y.clone())),
        signed(&(x.clone() - y.clone())),
        signed(&(x.clone() * y.clone())),
        quotient(x.div(y)),
        quotient(x.rem(y)),
        signed(&(x.clone() & y.clone())),
        signed(&(x.clone() | y.clone())),
        signed(&(x.clone() ^ y.clone())),
        signed(
            &x.shl_within(&k, u64::MAX)
                .expect("an unlimited shift answers"),
        ),
        signed(&(x >> &k)),
        flag(x < y),
        flag(x <= y),
        flag(x == y),
        format!("{:?}", n.monus(&m)),
        natural(n.div(&m)),
        natural(n.rem(&m)),
        format!(
            "{:?}",
            n.shl_within(&k, u64::MAX)
                .expect("an unlimited shift answers")
        ),
        format!("{:?}", &n >> &k),
        bytes,
        signed(&float.to_integer().expect("a finite float truncates")),
    ]
    .join(" ")
}

/// Every operation agrees with `curios-num` on every pair of the grid's values.
///
/// The running program crosses from an i31 to a boxed magnitude inside these pairs, so each operation's fast path, its range check and its big-number helper are all reached, and Knuth's division meets divisors of one, two and three limbs at both signs. A disagreement names the pair and the column.
#[test]
fn every_operation_agrees_with_curios_num_across_the_boundaries() {
    let values = values();
    let pairs: Vec<(Integer, Integer)> = values
        .iter()
        .flat_map(|x| values.iter().map(move |y| (x.clone(), y.clone())))
        .collect();
    let line = pairs
        .iter()
        .map(|(x, y)| format!("{} {}", signed(x), signed(y)))
        .collect::<Vec<_>>()
        .join(";");

    let (system, io) = MockHost::builder().stdin_lines([line]).build();
    run_text(PROGRAM, system).expect("the grid program runs");
    let output = String::from_utf8(io.output()).expect("the grid prints text");

    let rows: Vec<&str> = output.split('\n').collect();
    assert_eq!(rows.len(), pairs.len(), "one row per pair");
    for ((x, y), row) in pairs.iter().zip(rows) {
        let expected = expected(x, y);
        for (column, (got, want)) in row.split(' ').zip(expected.split(' ')).enumerate() {
            assert_eq!(got, want, "column {column} of {x} and {y}");
        }
    }
}
