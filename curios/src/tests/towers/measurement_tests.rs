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
/// Over the checkers of `b900e392f`, every row accepted by both.
///
/// | tower | lines | elaborator units | elaborator looks | kernel units | kernel looks |
/// | --- | --- | --- | --- | --- | --- |
/// | chain | 12 | 26 791 | 54 185 | 29 048 | 142 884 |
/// | chain | 16 | 26 791 | 55 573 | 29 048 | 143 808 |
/// | chain, in a member | 12 | 26 791 | 84 458 | 29 048 | 144 819 |
/// | chain, in a member | 16 | 26 791 | 60 849 | 29 048 | 146 055 |
/// | calls | 12 | 26 791 | 55 326 | 173 016 | 583 848 |
/// | calls | 16 | 26 791 | 57 114 | 2 753 480 | 7 219 464 |
/// | calls, in a member | 12 | 26 791 | 173 738 | 173 062 | 2 034 652 |
/// | calls, in a member | 16 | 26 791 | 1 896 474 | 2 753 526 | 30 420 112 |
/// | pairs | 12 | 26 791 | 220 301 | 74 814 | 681 575 |
/// | pairs | 16 | 26 791 | 2 681 841 | 1 180 746 | 8 791 327 |
/// | pairs, in a member | 12 | 26 791 | 453 311 | 74 847 | 1 337 484 |
/// | pairs, in a member | 16 | 26 791 | 6 356 675 | 1 180 779 | 19 277 652 |
/// | lists | 12 | 26 791 | 83 679 | 151 454 | 3 316 313 |
/// | lists | 16 | 26 791 | 106 743 | 2 365 218 | 50 876 129 |
/// | lists, in a member | 12 | 26 791 | 79 932 | 151 465 | 2 654 582 |
/// | lists, in a member | 16 | 26 791 | 96 950 | 2 365 229 | 40 261 798 |
/// | arms | 12 | 26 791 | 67 494 | 74 807 | 1 396 545 |
/// | arms | 16 | 26 791 | 78 110 | 1 180 719 | 20 198 793 |
/// | arms, in a member | 12 | 26 791 | 360 404 | 74 940 | 1 262 875 |
/// | arms, in a member | 16 | 133 836 | 4 673 508 | 1 180 868 | 18 037 815 |
/// | aliases | 12 | 26 791 | 218 069 | 29 048 | 487 630 |
/// | aliases | 16 | 265 315 | 2 677 197 | 29 048 | 5 651 386 |
/// | arrows | 12 | 26 791 | 218 468 | 29 048 | 365 976 |
/// | arrows | 16 | 265 439 | 2 677 732 | 29 048 | 3 687 316 |
/// | two towers | 12 | 26 791 | 1 632 345 | 120 311 | 2 296 932 |
/// | two towers | 16 | 26 791 | 24 994 503 | 1 840 643 | 34 614 392 |
///
/// **The chain is flat and every tower multiplies.** The kernel walks each of them per path: its looks grow by ten to sixteen times across four lines, and where it types a term per path its units do too, which is what refuses a tower at twenty lines. The elaborator walks the pairs, both aliases and the two towers per path, and in a recursive member's body the calls and the arms as well, which it does not outside one. A row's units are its heaviest declaration's, which for the chain is not the tower, so the same 26 791 and 29 048 stand under every row until a tower's own declaration outgrows them.
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
