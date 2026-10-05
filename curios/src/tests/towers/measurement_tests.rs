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
/// Over the checkers of `99960c4e2`, every row accepted by both.
///
/// | tower | lines | elaborator units | elaborator looks | kernel units | kernel looks |
/// | --- | --- | --- | --- | --- | --- |
/// | chain | 12 | 27 292 | 54 943 | 29 254 | 141 376 |
/// | chain | 16 | 27 292 | 56 323 | 29 254 | 141 652 |
/// | chain, in a member | 12 | 27 292 | 84 981 | 29 254 | 142 344 |
/// | chain, in a member | 16 | 27 292 | 60 842 | 29 254 | 142 648 |
/// | calls | 12 | 27 292 | 56 169 | 29 254 | 141 582 |
/// | calls | 16 | 27 292 | 58 049 | 29 254 | 141 930 |
/// | calls, in a member | 12 | 27 292 | 59 388 | 29 254 | 142 353 |
/// | calls, in a member | 16 | 27 292 | 61 744 | 29 254 | 142 729 |
/// | pairs | 12 | 27 292 | 54 854 | 29 254 | 141 733 |
/// | pairs | 16 | 27 292 | 56 334 | 29 254 | 142 125 |
/// | pairs, in a member | 12 | 27 292 | 57 752 | 29 254 | 142 297 |
/// | pairs, in a member | 16 | 27 292 | 60 056 | 29 254 | 142 689 |
/// | lists | 12 | 27 292 | 79 234 | 29 254 | 148 842 |
/// | lists | 16 | 27 292 | 97 066 | 29 254 | 154 454 |
/// | lists, in a member | 12 | 27 292 | 75 601 | 29 254 | 147 964 |
/// | lists, in a member | 16 | 27 292 | 88 047 | 29 254 | 152 376 |
/// | arms | 12 | 27 292 | 68 050 | 29 254 | 145 787 |
/// | arms | 16 | 27 292 | 78 578 | 29 254 | 148 939 |
/// | arms, in a member | 12 | 27 292 | 73 077 | 29 254 | 147 068 |
/// | arms, in a member | 16 | 27 292 | 84 893 | 29 254 | 150 368 |
/// | guards | 12 | 27 292 | 67 386 | 29 254 | 143 308 |
/// | guards | 16 | 27 292 | 74 084 | 29 254 | 144 200 |
/// | guards, in a member | 12 | 27 292 | 72 990 | 29 254 | 145 177 |
/// | guards, in a member | 16 | 27 292 | 80 920 | 29 254 | 146 429 |
/// | recursions | 12 | 27 292 | 78 838 | 29 254 | 146 729 |
/// | recursions | 16 | 27 292 | 88 870 | 29 254 | 148 521 |
/// | aliases | 12 | 27 292 | 55 843 | 29 254 | 142 775 |
/// | aliases | 16 | 27 292 | 57 921 | 29 254 | 143 495 |
/// | arrows | 12 | 27 292 | 56 242 | 29 254 | 142 911 |
/// | arrows | 16 | 27 292 | 58 456 | 29 254 | 143 679 |
/// | two towers | 12 | 27 292 | 69 942 | 29 254 | 145 066 |
/// | two towers | 16 | 27 292 | 77 802 | 29 254 | 146 606 |
/// | apart | 12 | 27 292 | 71 337 | 29 254 | 147 907 |
/// | apart | 16 | 27 292 | 77 937 | 29 254 | 150 159 |
///
/// **Every row is flat.** Each tower adds four lines' worth of looks across four lines, as the chain does, and no row's units move: a row's units are its heaviest declaration's, which for every tower here is not the tower, so the same 27 292 and 29 254 stand under each. A tower a checker walked once per path would read ten to sixteen times the looks at sixteen lines that it reads at twelve, and where the checker types per path its units would follow.
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
