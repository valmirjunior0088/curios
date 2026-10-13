//! What each runaway costs at a budget and at twice it, counted.

use {
    crate::tests::{Runaway, runaway},
    curios_pipeline::compile_with_prelude,
    curios_profile::{Destination, ProfileReport, fold_at, trace},
    curios_text::{Entrypoint, RootSource},
    std::{
        fs::create_dir_all,
        path::Path,
        time::{SystemTime, UNIX_EPOCH},
    },
};

/// What one checker's heaviest judgment of a program spent and looked at.
struct Heaviest {
    units: u64,
    looks: u64,
}

impl Heaviest {
    /// The largest observation of each of the two samples a checker files where it restores its budget, or `None` where it filed neither: it judged nothing of the program.
    fn of(report: &ProfileReport, checker: &str) -> Option<Self> {
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

    fn row(checker: Option<&Self>) -> String {
        match checker {
            Some(Self { units, looks }) => format!(
                "{units:>10} {looks:>12} {:>7.1}",
                *looks as f64 / (*units).max(1) as f64
            ),
            None => format!("{:>10} {:>12} {:>7}", "-", "-", "-"),
        }
    }
}

/// What a compilation made of a runaway under one budget: how it answered, and each checker's heaviest judgment.
struct Cost {
    verdict: &'static str,
    elaborator: Option<Heaviest>,
    kernel: Option<Heaviest>,
}

/// `source` compiled under `budget` with its stream filed at `stream`, and read back.
///
/// A refusal carries no consumption, which is why the stream is read and not the answer.
fn cost(budget: u64, source: &str, stream: &Path) -> Cost {
    let entrypoint = source.parse::<Entrypoint>().expect("a runaway parses");
    let answer = trace(Destination::File(stream.to_path_buf()), || {
        compile_with_prelude(budget, &entrypoint, &RootSource::none(), |_| {})
            .map(|_| ())
            .map_err(|error| error.to_string())
    })
    .expect("the stream opens");
    let verdict = match &answer {
        Ok(()) => "accepted",
        Err(report) if report.contains("the kernel refused") => "kernel",
        Err(report) if report.contains("ran out of steps") => "budget",
        Err(report) if report.contains("type mismatch") => "apart",
        Err(_) => "refused",
    };
    let report = fold_at(stream).expect("the stream folds");

    Cost {
        verdict,
        elaborator: Heaviest::of(&report, "curios_elab"),
        kernel: Heaviest::of(&report, "curios_cert"),
    }
}

/// What every runaway costs each checker at two budgets.
///
/// # How to take it
///
/// ```sh
/// cargo test --all-features --package curios --lib -- --ignored --nocapture runaway_measurements
/// ```
///
/// Counted, so the figures hold on any machine. The looks are the test profile's: a build with debug assertions asks every peel of a pair again, to hold their answers to each other (`curios-core`'s `peel_spine`), so a row whose comparisons peel reads fewer under the release compiler. Units are what a checker's heaviest judgment of the program spent of its budget, and looks how many times a term handed out its node while it was judged (`curios_core::take_looks`), both read off the stream the compilation files, kept under `curios/.artifacts/runaways/`. The verdict is the compilation's: `apart` for a mismatch, `budget` where the elaborator's budget ended it, `kernel` where the elaborator accepted and the kernel refused, `accepted` where both held it. The kernel judges a program only where the elaborator accepted it, and files what a judgment cost it where it begins the next, which a refused one never reaches.
///
/// **What to read.** A row the budget ends spends the whole of it, and its looks say what a unit bought: looks that double with the budget buy the same for every unit, and looks that more than double buy more the longer the row runs, which is a place a unit is not yet a bound. A row answered inside the smaller budget reads the same at both, and one answered inside the larger alone reads there what its answer cost.
///
/// # What it last printed
///
/// Over the checkers of `76a6cf3cf`, the same table at every taking.
///
/// | runaway | budget | verdict | elaborator units | elaborator looks | looks a unit | kernel units | kernel looks | looks a unit |
/// | --- | --- | --- | --- | --- | --- | --- | --- | --- |
/// | spin | 25 000 | budget | 25 000 | 43 106 | 1.7 | - | - | - |
/// | spin | 50 000 | budget | 50 000 | 90 426 | 1.8 | - | - | - |
/// | sums | 25 000 | apart | 6 156 | 5 182 | 0.8 | - | - | - |
/// | sums | 50 000 | apart | 6 156 | 5 174 | 0.8 | - | - | - |
/// | sums, apart | 25 000 | apart | 6 156 | 5 136 | 0.8 | - | - | - |
/// | sums, apart | 50 000 | apart | 6 156 | 5 136 | 0.8 | - | - | - |
/// | sums of calls | 25 000 | apart | 6 157 | 7 107 | 1.2 | - | - | - |
/// | sums of calls | 50 000 | apart | 6 157 | 7 107 | 1.2 | - | - | - |
/// | halvings | 25 000 | apart | 6 156 | 5 230 | 0.8 | - | - | - |
/// | halvings | 50 000 | apart | 6 156 | 5 230 | 0.8 | - | - | - |
/// | rounds | 25 000 | accepted | 5 132 | 3 796 | 0.7 | 1 144 | 853 | 0.7 |
/// | rounds | 50 000 | accepted | 5 132 | 3 782 | 0.7 | 1 144 | 853 | 0.7 |
/// | classes | 250 000 | apart | 204 430 | 1 292 518 | 6.3 | - | - | - |
/// | classes | 500 000 | apart | 204 430 | 1 292 518 | 6.3 | - | - | - |
/// | powers | 1 000 000 | budget | 1 000 000 | 1 191 344 | 1.2 | - | - | - |
/// | powers | 2 000 000 | apart | 1 584 702 | 2 096 960 | 1.3 | - | - | - |
/// | images | 1 250 000 | budget | 1 250 000 | 1 931 317 | 1.5 | - | - | - |
/// | images | 2 500 000 | apart | 1 868 754 | 3 695 443 | 2.0 | - | - | - |
/// | nests | 750 000 | apart | 640 362 | 444 119 | 0.7 | - | - | - |
/// | nests | 1 500 000 | apart | 640 362 | 444 119 | 0.7 | - | - | - |
/// | stacks | 200 000 | kernel | 45 551 | 237 346 | 5.2 | 200 000 | 9 140 812 | 45.7 |
/// | stacks | 400 000 | accepted | 45 551 | 237 346 | 5.2 | 272 932 | 12 503 266 | 45.8 |
///
/// **Every row but two reads two nodes a unit or fewer, at either budget.** The plain recursion reads 1.7 and then 1.8. The four pairs of calls whose unfolding meets them again are told apart, and the two recursions of one shape held by both checkers, in what their program's definitions cost to elaborate. The product pays a square of its steps and reads little over one node a unit, and the image two by the time it answers.
///
/// The classes read six nodes a unit and answer inside the smaller budget: every fold asks again which of its atoms are one, a question a unit at least, and one the elaborator has answered since it last wrote is answered for that unit alone (`same_uncommitted_inside`, `curios-elab/src/convert.rs`), so a unit buys a comparison only where the question is new. The stacks cost the elaborator its 45 551 units at five nodes each and the kernel six times as many at forty-six: it opens the second recursion alone a step at a time, a comparison nested in the last at each, and at the smaller budget it is the kernel's that ends the row.
#[test]
#[ignore = "measurement, counted: reports what a runaway costs at two budgets rather than asserting"]
fn runaway_measurements() {
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
    ];

    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("a clock past the epoch")
        .as_nanos();
    let directory = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join(".artifacts/runaways")
        .join(stamp.to_string());
    create_dir_all(&directory).expect("capture directory");
    println!("captures: {}", directory.display());

    println!(
        "{:<14} {:>9} {:<9} {:>10} {:>12} {:>7}   {:>10} {:>12} {:>7}",
        "runaway", "budget", "verdict", "units", "looks", "a unit", "kernel", "looks", "a unit"
    );
    for (label, shape, budget) in rows {
        for budget in [budget, 2 * budget] {
            let stream =
                directory.join(format!("{}-{budget}.trace", label.replace([' ', ','], "")));
            let Cost {
                verdict,
                elaborator,
                kernel,
            } = cost(budget, &runaway(shape), &stream);

            println!(
                "{label:<14} {budget:>9} {verdict:<9} {}   {}",
                Heaviest::row(elaborator.as_ref()),
                Heaviest::row(kernel.as_ref())
            );
        }
    }
}
