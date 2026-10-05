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
/// Over the checkers of `aa026bb2e`, every row accepted by both.
///
/// | tower | lines | elaborator units | elaborator looks | kernel units | kernel looks |
/// | --- | --- | --- | --- | --- | --- |
/// | chain | 12 | 27 292 | 54 987 | 29 254 | 141 797 |
/// | chain | 16 | 27 292 | 56 375 | 29 254 | 141 981 |
/// | chain, in a member | 12 | 27 292 | 85 613 | 29 254 | 142 950 |
/// | chain, in a member | 16 | 27 292 | 61 648 | 29 254 | 143 226 |
/// | calls | 12 | 27 292 | 56 113 | 29 254 | 141 977 |
/// | calls | 16 | 27 292 | 57 901 | 29 254 | 142 213 |
/// | calls, in a member | 12 | 27 292 | 174 522 | 29 254 | 142 984 |
/// | calls, in a member | 16 | 27 292 | 1 897 258 | 29 254 | 143 328 |
/// | pairs | 12 | 27 292 | 57 496 | 29 254 | 145 805 |
/// | pairs | 16 | 27 292 | 61 492 | 29 254 | 150 229 |
/// | pairs, in a member | 12 | 27 292 | 159 458 | 29 254 | 244 796 |
/// | pairs, in a member | 16 | 27 292 | 1 639 198 | 29 254 | 1 723 820 |
/// | lists | 12 | 27 292 | 80 448 | 29 254 | 149 223 |
/// | lists | 16 | 27 292 | 99 064 | 29 254 | 154 739 |
/// | lists, in a member | 12 | 27 292 | 77 581 | 29 254 | 148 920 |
/// | lists, in a member | 16 | 27 292 | 91 171 | 29 254 | 153 548 |
/// | arms | 12 | 27 292 | 68 324 | 50 230 | 691 917 |
/// | arms | 16 | 27 292 | 78 940 | 787 502 | 8 926 373 |
/// | arms, in a member | 12 | 27 292 | 361 231 | 50 364 | 890 356 |
/// | arms, in a member | 16 | 133 835 | 4 674 335 | 787 652 | 12 074 568 |
/// | aliases | 12 | 27 292 | 55 837 | 29 254 | 148 258 |
/// | aliases | 16 | 27 292 | 57 839 | 29 254 | 155 746 |
/// | arrows | 12 | 27 292 | 56 236 | 29 254 | 147 206 |
/// | arrows | 16 | 27 292 | 58 374 | 29 254 | 153 150 |
/// | two towers | 12 | 27 292 | 79 108 | 29 254 | 149 627 |
/// | two towers | 16 | 27 292 | 95 216 | 29 254 | 155 743 |
/// | apart | 12 | 27 292 | 71 169 | 29 254 | 148 726 |
/// | apart | 16 | 27 292 | 77 425 | 29 254 | 151 354 |
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
