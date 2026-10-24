//! What each tower costs each checker, counted.

use {
    crate::tests::{Shape, Stands, tower},
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
/// Over the checkers of `e2719c842`, every row accepted by both.
///
/// | tower | lines | elaborator units | elaborator looks | kernel units | kernel looks |
/// | --- | --- | --- | --- | --- | --- |
/// | chain | 12 | 4 055 | 13 783 | 3 059 | 8 719 |
/// | chain | 16 | 4 055 | 14 987 | 3 059 | 9 075 |
/// | chain, in a member | 12 | 4 055 | 18 047 | 3 059 | 9 803 |
/// | chain, in a member | 16 | 4 055 | 19 681 | 3 059 | 10 207 |
/// | calls | 12 | 4 055 | 15 231 | 3 059 | 9 002 |
/// | calls | 16 | 4 055 | 16 847 | 3 059 | 9 446 |
/// | calls, in a member | 12 | 4 055 | 19 169 | 3 059 | 9 831 |
/// | calls, in a member | 16 | 4 055 | 21 349 | 3 059 | 10 323 |
/// | pairs | 12 | 4 055 | 14 307 | 3 059 | 9 253 |
/// | pairs | 16 | 4 055 | 15 759 | 3 059 | 9 781 |
/// | pairs, in a member | 12 | 4 055 | 17 853 | 3 059 | 9 761 |
/// | pairs, in a member | 16 | 4 055 | 20 193 | 3 059 | 10 289 |
/// | lists | 12 | 4 055 | 30 609 | 3 059 | 11 317 |
/// | lists | 16 | 4 055 | 37 497 | 3 059 | 12 493 |
/// | lists, in a member | 12 | 4 055 | 34 493 | 3 059 | 11 774 |
/// | lists, in a member | 16 | 4 055 | 42 413 | 3 059 | 12 950 |
/// | arms | 12 | 4 055 | 22 621 | 3 059 | 12 894 |
/// | arms | 16 | 4 055 | 27 085 | 3 059 | 16 254 |
/// | arms, in a member | 12 | 4 055 | 27 194 | 3 059 | 14 378 |
/// | arms, in a member | 16 | 4 055 | 32 306 | 3 059 | 17 942 |
/// | guards | 12 | 4 055 | 26 416 | 3 059 | 10 168 |
/// | guards | 16 | 4 055 | 31 964 | 3 059 | 10 996 |
/// | guards, in a member | 12 | 4 055 | 31 968 | 3 059 | 12 911 |
/// | guards, in a member | 16 | 4 055 | 38 432 | 3 059 | 14 351 |
/// | recursions | 12 | 4 055 | 40 466 | 3 059 | 14 511 |
/// | recursions | 16 | 4 055 | 49 774 | 3 059 | 16 523 |
/// | handed | 12 | 4 055 | 22 696 | 3 059 | 10 521 |
/// | handed | 16 | 4 055 | 26 548 | 3 059 | 11 189 |
/// | aliases | 12 | 4 068 | 13 128 | 3 139 | 10 384 |
/// | aliases | 16 | 4 068 | 14 416 | 3 159 | 11 272 |
/// | arrows | 12 | 4 068 | 13 055 | 3 163 | 10 444 |
/// | arrows | 16 | 4 068 | 14 319 | 3 191 | 11 352 |
/// | fields | 12 | 4 055 | 19 761 | 3 059 | 13 936 |
/// | fields | 16 | 4 055 | 22 965 | 3 059 | 15 928 |
/// | two towers | 12 | 4 190 | 25 099 | 3 211 | 13 580 |
/// | two towers | 16 | 4 214 | 31 029 | 3 247 | 15 280 |
/// | apart | 12 | 4 055 | 25 563 | 3 059 | 14 004 |
/// | apart | 16 | 4 055 | 29 223 | 3 059 | 15 548 |
/// | bounded | 12 | 43 018 | 292 193 | 42 967 | 173 309 |
/// | bounded | 16 | 56 510 | 450 553 | 56 319 | 270 343 |
///
/// **No row multiplies.** A tower a checker walked once per path would read sixteen times the looks at sixteen lines that it reads at twelve, where a walk that visits each node once reads at most a third more. Every row but one reads at most 26 per cent more, the most the arms in the kernel, 12 894 looks and then 16 254. The bounded tower, whose proof folds comparisons over every line, reads 292 193 and then 450 553 in the elaborator and 173 309 and then 270 343 in the kernel: more than its lines add, though far short of what a walk per path reads (`documentation/roadmap/compilation/000-findings.md`). A row's units are its heaviest declaration's: they stand still across the two heights in every row but four, and in those move by under forty for the aliases, the arrows and the two towers, and by 13 492 in the elaborator and 13 352 in the kernel for the bounded tower, whose proof reduces each line's comparison.
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
