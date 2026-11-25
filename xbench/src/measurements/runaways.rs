//! What each runaway costs at a budget and at twice it, counted.

use {
    crate::{Measure, phase, read},
    curios::{TALLIES, test_support::*},
    curios_module::RootSource,
    curios_pipeline::compile_with_prelude,
    curios_profile::Profile,
    curios_surface::Entrypoint,
};

/// What one checker's heaviest judgment of a program spent and looked at.
struct Heaviest {
    units: u64,
    looks: u64,
}

impl Heaviest {
    /// The largest observation of each of the two samples a checker files where it restores its budget, or `None` where it filed neither: it judged nothing of the program.
    fn of(report: &Profile, checker: &str) -> Option<Self> {
        let largest = |name: &str| {
            report
                .samples
                .iter()
                .filter(|sample| sample.target.starts_with(checker) && sample.name == name)
                .map(|sample| sample.max)
                .max()
        };

        Some(Self {
            units: largest("budget::consumed")?,
            looks: largest("term::looks")?,
        })
    }

    /// Read what `checker`'s heaviest judgment of `runaway` under `budget` spent and looked at as its figures, `<runaway> at <budget>/<checker>/units` and `/looks`, sized by the budget.
    fn read(&self, runaway: &str, checker: &str, budget: u64) {
        read(
            &format!("{runaway} at {budget}/{checker}/units"),
            Measure::Count,
            Some(budget),
            self.units,
        );
        read(
            &format!("{runaway} at {budget}/{checker}/looks"),
            Measure::Count,
            Some(budget),
            self.looks,
        );
    }
}

/// What a compilation made of a runaway under one budget: how it answered, and each checker's heaviest judgment.
struct Cost {
    verdict: &'static str,
    elaborator: Option<Heaviest>,
    certifier: Option<Heaviest>,
}

/// `source` compiled under `budget` as a phase, and its fold read.
///
/// A refusal carries no consumption, which is why the fold is read and not the answer.
fn cost(budget: u64, source: &str) -> Cost {
    let entrypoint = source.parse::<Entrypoint>().expect("a runaway parses");
    let compiled = phase(TALLIES, || {
        compile_with_prelude(budget, &entrypoint, &RootSource::none(), |_| {})
            .map(|_| ())
            .map_err(|error| error.to_string())
    });
    let verdict = match &compiled.answer {
        Ok(()) => "accepted",
        Err(report) if report.contains("the certifier refused") => "certifier",
        Err(report) if report.contains("ran out of steps") => "budget",
        Err(report) if report.contains("type mismatch") => "apart",
        Err(_) => "refused",
    };
    Cost {
        verdict,
        elaborator: Heaviest::of(&compiled.profile, "curios_elab"),
        certifier: Heaviest::of(&compiled.profile, "curios_cert"),
    }
}

/// What every runaway costs each checker at two budgets.
///
/// `cargo xbench measure runaway_measurements` takes it, in release. Counted, so the figures hold on any machine. Units are what a checker's heaviest judgment of the program spent of its budget, and looks how many times a term handed out its node while it was judged (`curios_core::looks`), both read off the fold of the phase the compilation runs as. The verdict is the compilation's, a figure `<runaway> at <budget>/verdict/<verdict>` of one: `apart` for a mismatch, `budget` where the elaborator's budget ended it, `certifier` where the elaborator accepted and the certifier refused, `accepted` where both held it. The certifier judges a program only where the elaborator accepted it, and files what a judgment cost it where it begins the next, which a refused one never reaches.
///
/// **What to read.** A row the budget ends spends the whole of it, and its looks say what a unit bought: looks that double with the budget buy the same for every unit, and looks that more than double buy more the longer the row runs, which is a place a unit is not yet a bound. A row answered inside the smaller budget reads the same at both, and one answered inside the larger alone reads there what its answer cost.
///
/// # Its records
///
/// `cargo xbench report runaway_measurements` reads them: each figure's readings by group, and what each sitting was taken under.
pub fn runaway_measurements() {
    let rows = [
        ("spin", Runaway::Spin, 25_000),
        ("sums", Runaway::Sums, 25_000),
        ("sums, apart", Runaway::SumsApart, 25_000),
        ("sums of calls", Runaway::SumsOfCalls, 25_000),
        ("halvings", Runaway::Halvings, 25_000),
        ("rounds", Runaway::Rounds, 25_000),
        ("classes", Runaway::Classes(62), 250_000),
        ("powers", Runaway::Powers(250), 1_000_000),
        ("images", Runaway::Images(250, 50), 1_250_000),
        ("nests", Runaway::Nests(500), 750_000),
        ("stacks", Runaway::Stacks(500), 200_000),
        ("walks", Runaway::Walks(2000), 150_000),
    ];

    for (label, shape, budget) in rows {
        for budget in [budget, 2 * budget] {
            let Cost {
                verdict,
                elaborator,
                certifier,
            } = cost(budget, &runaway(shape));

            read(
                &format!("{label} at {budget}/verdict/{verdict}"),
                Measure::Count,
                Some(budget),
                1,
            );
            for (checker, heaviest) in [("elaborator", elaborator), ("certifier", certifier)] {
                if let Some(heaviest) = heaviest {
                    heaviest.read(label, checker, budget);
                }
            }
        }
    }
}
