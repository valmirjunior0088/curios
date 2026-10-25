//! What each tower costs each checker, counted.

use {
    crate::tests::{Shape, Stands, tower},
    curios_core::{Zonked, take_looks},
    curios_pipeline::{DEFAULT_BUDGET, recheck_with_prelude_measured, typecheck_with_prelude},
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
    let checked = typecheck_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none());
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
    let (verdicts, heaviest) = recheck_with_prelude_measured(&module, DEFAULT_BUDGET, |kernel| {
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
/// Over the checkers of `cf3aa6e85`, every row accepted by both.
///
/// | tower | lines | elaborator units | elaborator looks | kernel units | kernel looks |
/// | --- | --- | --- | --- | --- | --- |
/// | chain | 12 | 4 046 | 12 889 | 3 039 | 8 599 |
/// | chain | 16 | 4 046 | 14 133 | 3 039 | 8 979 |
/// | chain, in a member | 12 | 4 046 | 16 946 | 3 039 | 9 620 |
/// | chain, in a member | 16 | 4 046 | 18 620 | 3 039 | 10 048 |
/// | calls | 12 | 4 046 | 14 347 | 3 039 | 8 883 |
/// | calls | 16 | 4 046 | 16 019 | 3 039 | 9 351 |
/// | calls, in a member | 12 | 4 046 | 18 254 | 3 039 | 9 731 |
/// | calls, in a member | 16 | 4 046 | 20 490 | 3 039 | 10 247 |
/// | pairs | 12 | 4 046 | 13 009 | 3 039 | 8 890 |
/// | pairs | 16 | 4 046 | 14 305 | 3 039 | 9 362 |
/// | pairs, in a member | 12 | 4 046 | 16 506 | 3 039 | 9 401 |
/// | pairs, in a member | 16 | 4 046 | 18 690 | 3 039 | 9 873 |
/// | lists | 12 | 4 046 | 27 210 | 3 039 | 11 357 |
/// | lists | 16 | 4 046 | 33 542 | 3 039 | 12 601 |
/// | lists, in a member | 12 | 4 046 | 31 063 | 3 039 | 11 826 |
/// | lists, in a member | 16 | 4 046 | 38 427 | 3 039 | 13 070 |
/// | arms | 12 | 4 046 | 20 256 | 3 039 | 12 832 |
/// | arms | 16 | 4 046 | 24 212 | 3 039 | 16 228 |
/// | arms, in a member | 12 | 4 046 | 24 655 | 3 039 | 14 252 |
/// | arms, in a member | 16 | 4 046 | 29 259 | 3 039 | 17 852 |
/// | guards | 12 | 4 046 | 24 043 | 3 039 | 10 248 |
/// | guards | 16 | 4 046 | 29 159 | 3 039 | 11 164 |
/// | guards, in a member | 12 | 4 046 | 29 423 | 3 039 | 12 929 |
/// | guards, in a member | 16 | 4 046 | 35 455 | 3 039 | 14 457 |
/// | recursions | 12 | 4 046 | 37 739 | 3 039 | 14 590 |
/// | recursions | 16 | 4 046 | 46 495 | 3 039 | 16 698 |
/// | handed | 12 | 4 046 | 20 081 | 3 039 | 10 133 |
/// | handed | 16 | 4 046 | 23 525 | 3 039 | 10 745 |
/// | aliases | 12 | 4 060 | 12 549 | 3 093 | 9 758 |
/// | aliases | 16 | 4 060 | 13 837 | 3 105 | 10 494 |
/// | arrows | 12 | 4 060 | 12 440 | 3 117 | 9 842 |
/// | arrows | 16 | 4 060 | 13 692 | 3 137 | 10 606 |
/// | fields | 12 | 4 046 | 16 399 | 3 039 | 12 299 |
/// | fields | 16 | 4 046 | 18 823 | 3 039 | 13 855 |
/// | two towers | 12 | 4 164 | 22 946 | 3 160 | 12 299 |
/// | two towers | 16 | 4 184 | 28 504 | 3 188 | 13 607 |
/// | apart | 12 | 4 046 | 24 221 | 3 039 | 13 893 |
/// | apart | 16 | 4 046 | 28 005 | 3 039 | 15 493 |
/// | bounded | 12 | 42 906 | 166 311 | 42 946 | 104 747 |
/// | bounded | 16 | 56 358 | 232 501 | 56 298 | 150 467 |
///
/// **No row multiplies.** A tower a checker walked once per path would read sixteen times the looks at sixteen lines that it reads at twelve, where a walk that visits each node once reads at most a third more. Every row but one reads under 27 per cent more, the most the arms in the kernel, 12 832 looks and then 16 228. The bounded tower, whose proof folds comparisons over every line, reads 166 311 and then 232 501 in the elaborator and 104 747 and then 150 467 in the kernel: more than its lines add, though far short of what a walk per path reads (`documentation/roadmap/compilation/000-findings.md`). A row's units are its heaviest declaration's: they stand still across the two heights in every row but four, and in those move by under forty for the aliases, the arrows and the two towers, and by 13 452 in the elaborator and 13 352 in the kernel for the bounded tower, whose proof reduces each line's comparison.
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
        ("fields", Shape::Fields),
        ("two towers", Shape::TwoTowers),
        ("apart", Shape::Apart),
        ("bounded", Shape::Bounded),
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
