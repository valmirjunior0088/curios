//! What a rope's run holds once it is read: every element in its place, whatever shape built it.

use {
    crate::tests::test_support::*,
    curios_native::{MockHost, test_support::*},
};

/// Every element at its position, weighed so an element out of place moves the sum, as the program weighs its runs.
fn weighed(run: &[u64]) -> u64 {
    run.iter().enumerate().fold(0, |sum, (at, element)| {
        (sum + (at as u64 + 1) * element) % 1_000_003
    })
}

/// A flattening writes every element where it stands, whatever shape built the run: a lean of appends deeper than any stack holds frames for, a build by halves whose every node has two children never flattened, a run joined to itself, a window ahead of a half never flattened, and a run grown past a prefix whose room it claims. Each is read first where a flattening is what answers — a read falling in a child never flattened — and the reads and the weighed runs are printed, at a size the program reads from its arguments so nothing folds before it runs.
#[test]
fn a_flattening_writes_every_element_where_it_stands() {
    let n = 100_000;
    let (system, io) = MockHost::builder()
        .args(["shapes".to_string(), n.to_string()])
        .build();
    run_text(
        r#"
        use /std/{Str, Nat, List, Io};

        let leaning(k: Nat, acc: List(Nat)) -> List(Nat) =
            match k: (_) => List(Nat) | 0 => acc | m + 1 => leaning(m, [..acc, m]) end;

        let halves(lo: Nat, k: Nat) -> List(Nat) =
            match k: (_) => List(Nat)
            | 0 => []
            | 1 => [lo]
            | _ =>
                let half = k / 2;
                [..halves(lo, half), ..halves(lo + half, k - half)]
            end;

        let weighed(xs: List(Nat)) -> Nat =
            List/fold(xs, (0, 0), (x, acc) => ((acc.0 + (acc.1 + 1) * x) % 1000003, acc.1 + 1)).0;

        let read(xs: List(Nat), at: Nat) -> Nat =
            match List/try_get(xs, at) | some(x) => x | none() => 0 end;

        let shapes(n: Nat) -> List(Nat) =
            let lean = leaning(n, []);
            let first = read(lean, n / 2);
            let halved = halves(0, n);
            let second = read(halved, n / 4);
            let shared = halves(0, n);
            let joined = [..shared, ..shared];
            let third = read(joined, n / 2);
            let cut = leaning(n, []);
            let rest = match cut | [_, ..r] => r | [] => cut end;
            let windowed = [..rest, ..halves(0, n)];
            let fourth = read(windowed, n);
            let prefix = leaning(n, []);
            let fifth = read(prefix, n / 2);
            let grown = [..[..prefix, 1], 2];
            let sixth = read(grown, 0);
            let regrown = [..[..grown, 3], 4];
            let seventh = read(regrown, 0);
            [
                weighed(lean) + first,
                weighed(halved) + second,
                weighed(joined) + third,
                weighed(windowed) + fourth,
                weighed(regrown) + fifth + sixth + seventh,
            ];

        let entry() -> /std/Io({}) =
            let arguments = /std/Proc/args()!;
            let n =
                match List/try_get(arguments, 1)
                | some(bytes) =>
                    match Str/of_bytes(bytes)
                    | success(s) => match Nat/of_str(s) | some(v) => v | none() => 0 end
                    | failure(_) => 0
                    end
                | none() => 0
                end;
            /std/print(Str/join(" ", List/map(shapes(n), Nat/to_str)));

        entry()
        "#,
        system,
    )
    .expect("the shapes run");

    let lean = (0..n).rev().collect::<Vec<_>>();
    let halved = (0..n).collect::<Vec<_>>();
    let joined = [halved.clone(), halved.clone()].concat();
    let windowed = [lean[1..].to_vec(), halved.clone()].concat();
    let regrown = [lean.clone(), vec![1, 2, 3, 4]].concat();
    let reads = [
        lean[n as usize / 2],
        halved[n as usize / 4],
        joined[n as usize / 2],
        windowed[n as usize],
        lean[n as usize / 2] + regrown[0] + regrown[0],
    ];
    let expected = [&lean, &halved, &joined, &windowed, &regrown]
        .iter()
        .zip(reads)
        .map(|(run, read)| (weighed(run) + read).to_string())
        .collect::<Vec<_>>()
        .join(" ");

    assert_eq!(String::from_utf8(io.output().to_vec()).unwrap(), expected);
}

/// One read in a loop answers whatever shape each turn hands it: a walk over a leaf, over a concatenation it flattens partway, and over each of a text's lines, which are views; one site reading the first byte of each of those in turn; and a walk over a list of appended ranges. A site keeps the run it last resolved, so each turn is either that run or one resolved afresh, and every element is weighed where it stands.
#[test]
fn a_read_in_a_loop_answers_whatever_shape_each_turn_hands_it() {
    let (system, io) = MockHost::builder()
        .args(["shapes".to_string(), "abcdefghij".to_string()])
        .build();
    run_text(
        r#"
        use /std/{Str, Nat, List, Bytes, Byte, Io, Proc, Option};

        let weigh(b: Bytes, acc: Nat) -> Nat =
            match b | x[] => acc | x[h, ..t] => weigh(t, (acc * 31 + Byte/to_nat(h)) % 1000003) end;

        let head(b: Bytes) -> Nat =
            match b | x[] => 0 | x[h, .._] => Byte/to_nat(h) end;

        let heads(ps: List(Bytes), acc: Nat) -> Nat =
            match ps | [] => acc | [p, ..rest] => heads(rest, (acc * 31 + head(p)) % 1000003) end;

        let weigh_all(ps: List(Bytes), acc: Nat) -> Nat =
            match ps | [] => acc | [p, ..rest] => weigh_all(rest, weigh(p, acc)) end;

        let sum(xs: List(Nat), acc: Nat) -> Nat =
            match xs | [] => acc | [x, ..rest] => sum(rest, (acc * 31 + x) % 1000003) end;

        let entry() -> /std/Io({}) =
            let arguments = Proc/args()!;
            let leaf = match List/try_get(arguments, 1) | some(b) => b | none() => x[] end;
            let joined = x[..leaf, ..Str/to_bytes("-mid-"), ..leaf];
            let text = Str/concat(Nat/to_str(List/len(arguments)), "\nalpha\nbeta gamma\ndelta");
            let viewed = List/map(Str/lines(text), Str/to_bytes);
            let parts = [leaf, joined, ..viewed, joined, leaf];
            let n = Bytes/len(leaf);
            let grown = [..List/range(0, n), ..List/range(n, 2 * n), ..List/range(0, 3)];
            /std/print(Str/join(" ", List/map([heads(parts, 0), weigh_all(parts, 0), sum(grown, 0)], Nat/to_str)));

        entry()
        "#,
        system,
    )
    .expect("the shapes run");

    let leaf = b"abcdefghij".to_vec();
    let joined = [leaf.clone(), b"-mid-".to_vec(), leaf.clone()].concat();
    let parts = [
        leaf.clone(),
        joined.clone(),
        b"2".to_vec(),
        b"alpha".to_vec(),
        b"beta gamma".to_vec(),
        b"delta".to_vec(),
        joined,
        leaf,
    ];
    let step = |acc: u64, x: u64| (acc * 31 + x) % 1_000_003;
    let heads = parts
        .iter()
        .fold(0, |acc, part| step(acc, u64::from(part[0])));
    let weights = parts
        .iter()
        .flatten()
        .fold(0, |acc, byte| step(acc, u64::from(*byte)));
    let sum = (0..10).chain(10..20).chain(0..3).fold(0, step);

    assert_eq!(
        String::from_utf8(io.output().to_vec()).unwrap(),
        format!("{heads} {weights} {sum}"),
    );
}

/// A run built once from a window and read once holds no room: room is given past a flattened node, the run itself read and grown since, and a window first piece shows nothing of the kind. The run here is a window of twelve mebibytes of a base it keeps alive, read whole once by a cut; under a heap of sixty-four mebibytes the base and an exact payload fit, and the base and a payload twice the run do not.
#[test]
fn a_run_built_from_a_window_and_read_once_holds_no_room() {
    let compiled = compile(
        r#"
        use /std/{Str, Nat, Bytes, List, Io};
        let entry() -> /std/Io({}) =
            let arguments = /std/Proc/args()!;
            let mib =
                match List/try_get(arguments, 1)
                | some(bytes) =>
                    match Str/of_bytes(bytes)
                    | success(s) => match Nat/of_str(s) | some(v) => v | none() => 0 end
                    | failure(_) => 0
                    end
                | none() => 0
                end;
            let base = Bytes/replicate(mib * 1048576 + 1, 0x41);
            let window = Bytes/slice(base, 1, mib * 1048576);
            let built = x[..window, ..Bytes/replicate(16, 0x42)];
            let cut = Bytes/drop(built, 1);
            /std/print(Nat/to_str(Bytes/len(cut)));
        entry()
        "#,
    )
    .expect("the build compiles");
    let (system, io) = MockHost::builder().args(["window", "12"]).build();
    run_within_heap(&compiled, system, 64 << 20).expect("the base and an exact payload fit");

    assert_eq!(io.output(), (12 * 1_048_576 + 15).to_string().as_bytes());
}
