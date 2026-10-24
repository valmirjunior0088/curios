//! What a rope's run holds once it is read: every element in its place, whatever shape built it.

use {crate::tests::run_text, curios_native::MockHost};

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
            let arguments = /std/Process/args()!;
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
