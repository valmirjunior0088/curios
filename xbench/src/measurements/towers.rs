//! What each tower costs each checker, counted.

use {
    crate::{Measure, phase, read},
    curios::{TALLIES, test_support::*},
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_pipeline::test_support::*,
    curios_surface::Entrypoint,
};

/// Put `source` to each checker, a phase each, and read what each made of it as the figures of `name`, sized by `lines`: `<name>/<checker>`, its phase, the looks its walks made among what is read off it; `<name>/<checker>/accepted`; and `<name>/<checker>/units`, what its heaviest declaration spent. The elaborator's side is the lowering, elaboration and erasure, the certifier's its walk over what the elaborator built, which it judges only where the elaborator built something.
fn read_costs(name: &str, lines: u64, source: &str) {
    let entrypoint = source.parse::<Entrypoint>().expect("a tower parses");
    let size = Some(lines);

    let elaborating = phase(TALLIES, || {
        typecheck_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none())
    });
    elaborating.read(&format!("{name}/elaborator"), size);
    let Ok(checked) = elaborating.answer else {
        read(
            &format!("{name}/elaborator/accepted"),
            Measure::Count,
            size,
            0,
        );
        return;
    };
    read(
        &format!("{name}/elaborator/accepted"),
        Measure::Count,
        size,
        u64::from(checked.obligations.is_empty()),
    );
    read(
        &format!("{name}/elaborator/units"),
        Measure::Count,
        size,
        checked.consumption.units(),
    );

    let certifying = phase(TALLIES, || {
        recheck_with_prelude_measured(&checked.program, DEFAULT_BUDGET, |certifier| {
            certifier.heaviest_declaration()
        })
    });
    certifying.read(&format!("{name}/certifier"), size);
    let (verdicts, heaviest) = certifying.answer;
    read(
        &format!("{name}/certifier/accepted"),
        Measure::Count,
        size,
        u64::from(verdicts.is_empty()),
    );
    read(
        &format!("{name}/certifier/units"),
        Measure::Count,
        size,
        heaviest.units(),
    );
}

/// What every tower costs each checker at two heights.
///
/// `cargo xbench measure tower_measurements` takes it, each tower at each height read by [`read_costs`]. Its counts hold on any machine, and each phase's time on this one. Units are what the heaviest declaration spent of its budget; looks are how many times a term handed out its node during the checker's phase, the look count the program hands the recorder — the lowering, elaboration and erasure on one side, the certifier's walk on the other. A checker that walks a tower's graph adds four lines' worth across four lines, as the chain's rows do; one that walks it once per path multiplies by sixteen.
///
/// Looks are the column a walk cannot hide from. A hit on a remembered reduct is free, so a walk that only reads — the sort of a type — spends no unit however many paths it takes, and shows in looks alone.
///
/// # Its records
///
/// `cargo xbench report tower_measurements` reads them: each figure's readings by group, and what each sitting was taken under.
pub fn tower_measurements() {
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
    let warming = tower(Shape::Chain(Stands::Function), 1)
        .parse::<Entrypoint>()
        .expect("a tower parses");
    let _ = typecheck_with_prelude(DEFAULT_BUDGET, &warming, &RootSource::none());

    for (label, shape) in shapes {
        for lines in [12usize, 16] {
            read_costs(
                &format!("{label} at {lines}"),
                lines as u64,
                &tower(shape, lines),
            );
        }
    }
}
