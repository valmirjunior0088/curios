//! What each tower costs each checker, counted.

use {
    super::{Shape, Stands, tower},
    curios_core::{Zonked, take_looks},
    curios_pipeline::{DEFAULT_STEP_BUDGET, recheck_with_prelude_measured, typecheck_with_prelude},
    curios_text::{Entrypoint, RootSource},
};

/// What one checker made of a tower: its verdict, what its heaviest declaration spent where it reached one, and the nodes its walks looked at.
struct Cost {
    verdict: &'static str,
    units: Option<u64>,
    looks: u64,
}

impl Cost {
    fn row(&self) -> String {
        let units = match self.units {
            Some(units) => units.to_string(),
            None => "-".to_string(),
        };

        format!("{:<9} {units:>10} {:>12}", self.verdict, self.looks)
    }
}

/// What each checker makes of `source`: the elaborator's side is the lowering, elaboration and erasure, the kernel's its walk over what the elaborator built — absent where the elaborator built nothing.
fn costs(source: &str) -> (Cost, Option<Cost>) {
    let entrypoint = source.parse::<Entrypoint>().expect("a tower parses");

    take_looks();
    let checked = typecheck_with_prelude(DEFAULT_STEP_BUDGET, &entrypoint, &RootSource::none());
    let looks = take_looks();
    let Ok(checked) = checked else {
        let refused = Cost {
            verdict: "refused",
            units: None,
            looks,
        };
        return (refused, None);
    };
    let elaborator = Cost {
        verdict: match checked.obligations.is_empty() {
            true => "accepted",
            false => "refused",
        },
        units: Some(checked.consumption.units()),
        looks,
    };

    let module = Zonked::project(&checked.program).expect("a checked program is zonked");
    let (verdicts, heaviest) =
        recheck_with_prelude_measured(&module, DEFAULT_STEP_BUDGET, |kernel| {
            kernel.heaviest_declaration()
        });
    let kernel = Cost {
        verdict: match verdicts.is_empty() {
            true => "accepted",
            false => "refused",
        },
        units: Some(heaviest.units()),
        looks: take_looks(),
    };

    (elaborator, Some(kernel))
}

/// What every tower costs each checker at two heights.
///
/// # How to take it
///
/// ```sh
/// cargo test --all-features --package curios --lib -- --ignored --nocapture tower_measurements
/// ```
///
/// Counted, so the figures hold on any machine and in either profile. Units are what the heaviest declaration spent of its budget; looks are how many times a term handed out its node while the checker ran (`curios_core::take_looks`) — the lowering, elaboration and erasure on one side, the kernel's walk on the other. A checker that walks a tower's graph adds four lines' worth across four lines, as the chain's rows do; one that walks it once per path multiplies by sixteen.
///
/// Looks are the column a walk cannot hide from. A hit on a remembered reduct is free, so a walk that only reads — the sort of a type — spends no unit however many paths it takes, and shows in looks alone.
///
/// # What it last printed
///
/// Over the checkers of `8d5ab81fd`, every row accepted by both.
///
/// | tower | lines | elaborator units | elaborator looks | kernel units | kernel looks |
/// | --- | --- | --- | --- | --- | --- |
/// | chain | 12 | 3 102 | 7 993 | 3 188 | 4 401 |
/// | chain | 16 | 3 102 | 9 417 | 3 188 | 4 677 |
/// | chain, in a member | 12 | 6 165 | 12 345 | 3 188 | 5 424 |
/// | chain, in a member | 16 | 6 165 | 14 329 | 3 188 | 5 726 |
/// | calls | 12 | 3 102 | 9 240 | 3 188 | 4 607 |
/// | calls | 16 | 3 102 | 11 168 | 3 188 | 4 947 |
/// | calls, in a member | 12 | 3 102 | 12 319 | 3 188 | 5 360 |
/// | calls, in a member | 16 | 3 102 | 14 723 | 3 188 | 5 736 |
/// | pairs | 12 | 3 102 | 8 293 | 3 188 | 4 782 |
/// | pairs | 16 | 3 102 | 9 925 | 3 188 | 5 182 |
/// | pairs, in a member | 12 | 3 102 | 11 094 | 3 188 | 5 338 |
/// | pairs, in a member | 16 | 3 102 | 13 550 | 3 188 | 5 738 |
/// | lists | 12 | 5 598 | 35 223 | 4 132 | 10 329 |
/// | lists | 16 | 6 564 | 54 517 | 6 092 | 14 653 |
/// | lists, in a member | 12 | 6 622 | 32 532 | 4 143 | 10 741 |
/// | lists, in a member | 16 | 7 588 | 46 814 | 6 103 | 15 065 |
/// | arms | 12 | 4 135 | 23 695 | 3 188 | 8 880 |
/// | arms | 16 | 4 135 | 35 507 | 3 188 | 12 040 |
/// | arms, in a member | 12 | 6 183 | 29 337 | 3 188 | 10 194 |
/// | arms, in a member | 16 | 6 183 | 42 597 | 3 188 | 13 518 |
/// | guards | 12 | 4 136 | 23 059 | 3 188 | 6 625 |
/// | guards | 16 | 4 136 | 30 963 | 3 188 | 7 589 |
/// | guards, in a member | 12 | 6 184 | 29 853 | 3 188 | 8 481 |
/// | guards, in a member | 16 | 6 184 | 39 431 | 3 188 | 9 805 |
/// | recursions | 12 | 6 165 | 35 122 | 3 188 | 9 711 |
/// | recursions | 16 | 6 165 | 46 822 | 3 188 | 11 471 |
/// | handed | 12 | 5 157 | 14 548 | 3 188 | 5 749 |
/// | handed | 16 | 5 161 | 18 004 | 3 188 | 6 217 |
/// | aliases | 12 | 3 128 | 7 873 | 3 277 | 5 809 |
/// | aliases | 16 | 3 128 | 9 461 | 3 297 | 6 533 |
/// | arrows | 12 | 3 128 | 7 872 | 3 301 | 5 945 |
/// | arrows | 16 | 3 128 | 9 460 | 3 329 | 6 717 |
/// | two towers | 12 | 6 501 | 22 591 | 3 533 | 8 533 |
/// | two towers | 16 | 6 529 | 29 831 | 3 569 | 10 233 |
/// | apart | 12 | 6 168 | 26 926 | 3 188 | 11 366 |
/// | apart | 16 | 6 168 | 34 078 | 3 188 | 13 642 |
///
/// **No row multiplies.** A tower a checker walked once per path would read sixteen times the looks at sixteen lines that it reads at twelve; the row that grows most, the lists, whose type nests with every line and shares nothing, reads 35 223 looks and then 54 517 in the elaborator and 10 329 and then 14 653 in the kernel. A row's units are its heaviest declaration's: they stand still across the two heights in every row but six, and in those move by 966 for the lists in the elaborator and 1 960 in the kernel, in a function's body and in a member's alike, by four for the tower handed to its member, and by under forty for the aliases, the arrows and the two towers.
#[test]
#[ignore = "measurement, counted: reports what a tower costs each checker rather than asserting"]
fn tower_measurements() {
    let shapes = [
        ("chain", Shape::Chain(Stands::Function)),
        ("chain, in a member", Shape::Chain(Stands::Member)),
        ("calls", Shape::Calls(Stands::Function)),
        ("calls, in a member", Shape::Calls(Stands::Member)),
        ("pairs", Shape::Pairs(Stands::Function)),
        ("pairs, in a member", Shape::Pairs(Stands::Member)),
        ("lists", Shape::Lists(Stands::Function)),
        ("lists, in a member", Shape::Lists(Stands::Member)),
        ("arms", Shape::Arms(Stands::Function)),
        ("arms, in a member", Shape::Arms(Stands::Member)),
        ("guards", Shape::Guards(Stands::Function)),
        ("guards, in a member", Shape::Guards(Stands::Member)),
        ("recursions", Shape::Recursions),
        ("handed", Shape::Handed),
        ("aliases", Shape::Aliases),
        ("arrows", Shape::Arrows),
        ("two towers", Shape::TwoTowers),
        ("apart", Shape::Apart),
    ];

    // The prelude is restored once per thread, and its restore is not a tower's cost.
    costs(&tower(Shape::Chain(Stands::Function), 1));

    println!(
        "{:<20} {:<5} {:<9} {:>10} {:>12}   {:<9} {:>10} {:>12}",
        "tower", "lines", "elaborator", "units", "looks", "kernel", "units", "looks"
    );
    for (label, shape) in shapes {
        for lines in [12usize, 16] {
            let (elaborator, kernel) = costs(&tower(shape, lines));
            let kernel = match kernel {
                Some(kernel) => kernel.row(),
                None => "-".to_string(),
            };

            println!("{label:<20} {lines:<5} {}   {kernel}", elaborator.row());
        }
    }
}
