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
    let (verdicts, kernel) = recheck_with_prelude_measured(&module, DEFAULT_STEP_BUDGET);
    let kernel = Cost {
        verdict: match verdicts.is_empty() {
            true => "accepted",
            false => "refused",
        },
        units: Some(kernel.heaviest_declaration().units()),
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
/// Over the checkers of `55902851a`, every row accepted by both.
///
/// | tower | lines | elaborator units | elaborator looks | kernel units | kernel looks |
/// | --- | --- | --- | --- | --- | --- |
/// | chain | 12 | 26 791 | 54 185 | 29 048 | 141 779 |
/// | chain | 16 | 26 791 | 55 573 | 29 048 | 141 963 |
/// | chain, in a member | 12 | 26 791 | 84 458 | 29 048 | 142 966 |
/// | chain, in a member | 16 | 26 791 | 60 849 | 29 048 | 143 242 |
/// | calls | 12 | 26 791 | 55 326 | 29 048 | 141 959 |
/// | calls | 16 | 26 791 | 57 114 | 29 048 | 142 195 |
/// | calls, in a member | 12 | 26 791 | 173 738 | 29 048 | 143 000 |
/// | calls, in a member | 16 | 26 791 | 1 896 474 | 29 048 | 143 344 |
/// | pairs | 12 | 26 791 | 220 301 | 29 048 | 145 787 |
/// | pairs | 16 | 26 791 | 2 681 841 | 29 048 | 150 211 |
/// | pairs, in a member | 12 | 26 791 | 453 311 | 29 048 | 244 812 |
/// | pairs, in a member | 16 | 26 791 | 6 356 675 | 29 048 | 1 723 836 |
/// | lists | 12 | 26 791 | 83 679 | 29 048 | 149 205 |
/// | lists | 16 | 26 791 | 106 743 | 29 048 | 154 721 |
/// | lists, in a member | 12 | 26 791 | 79 932 | 29 048 | 148 936 |
/// | lists, in a member | 16 | 26 791 | 96 950 | 29 048 | 153 564 |
/// | arms | 12 | 26 791 | 67 494 | 50 230 | 970 291 |
/// | arms | 16 | 26 791 | 78 110 | 787 502 | 13 382 667 |
/// | arms, in a member | 12 | 26 791 | 360 404 | 50 364 | 890 406 |
/// | arms, in a member | 16 | 133 836 | 4 673 508 | 787 652 | 12 074 618 |
/// | aliases | 12 | 26 791 | 218 069 | 29 048 | 148 240 |
/// | aliases | 16 | 265 315 | 2 677 197 | 29 048 | 155 728 |
/// | arrows | 12 | 26 791 | 218 468 | 29 048 | 147 188 |
/// | arrows | 16 | 265 439 | 2 677 732 | 29 048 | 153 132 |
/// | two towers | 12 | 26 791 | 1 632 345 | 29 048 | 149 601 |
/// | two towers | 16 | 26 791 | 24 994 503 | 29 048 | 155 717 |
///
/// **The chain is flat, and a tower a checker walks per path multiplies.** Its looks grow by ten to sixteen times across four lines, and where the checker types a term per path its units do too, which is what refuses a tower at twenty lines; a tower walked in its size reads as the chain does. A row's units are its heaviest declaration's, which for the chain is not the tower, so the same 26 791 and 29 048 stand under every row until a tower's own declaration outgrows them.
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
        ("aliases", Shape::Aliases),
        ("arrows", Shape::Arrows),
        ("two towers", Shape::TwoTowers),
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
