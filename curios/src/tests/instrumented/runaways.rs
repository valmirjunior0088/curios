//! What each runaway costs at a budget and at twice it, counted.

use {
    crate::tests::{Runaway, runaway},
    curios_pipeline::compile_with_prelude,
    curios_profile::{Destination, fold_at, trace},
    curios_text::{Entrypoint, RootSource},
    std::{
        fs::create_dir_all,
        path::Path,
        time::{SystemTime, UNIX_EPOCH},
    },
};

/// What a compilation made of a runaway under one budget: how it answered, and what the elaborator's heaviest judgment spent and looked at.
struct Cost {
    verdict: &'static str,
    units: u64,
    looks: u64,
}

/// `source` compiled under `budget` with its stream filed at `stream`, and read back.
///
/// A refusal carries no consumption, which is why the stream is read and not the answer: the elaborator files what a judgment spent and looked at where it restores its budget, and a program's heaviest judgment is each sample's largest observation.
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
    let heaviest = |name: &str| {
        report
            .samples
            .iter()
            .filter(|sample| sample.target.starts_with("curios_elab") && sample.name == name)
            .map(|sample| sample.max)
            .max()
            .unwrap_or(0)
    };

    Cost {
        verdict,
        units: heaviest("budget::consumed"),
        looks: heaviest("term::looks"),
    }
}

/// What every runaway costs the elaborator at two budgets.
///
/// # How to take it
///
/// ```sh
/// cargo test --all-features --package curios --lib -- --ignored --nocapture runaway_measurements
/// ```
///
/// Counted, so the figures hold on any machine and in either profile. Units are what the program's heaviest judgment spent of its budget, and looks how many times a term handed out its node while the elaborator judged it (`curios_core::take_looks`), both read off the stream the compilation files, kept under `curios/.artifacts/runaways/`. The verdict is the compilation's: `apart` for a mismatch, `budget` where the elaborator's budget ended it, `kernel` where the elaborator accepted and the kernel refused, `accepted` where both held it.
///
/// **What to read.** A row the budget ends spends the whole of it, and its looks say what a unit bought: looks that double with the budget buy the same for every unit, and looks that more than double buy more the longer the row runs, which is a place a unit is not yet a bound. A row answered inside the smaller budget reads the same at both.
///
/// # What it last printed
///
/// Over the elaborator of `6fe03a4c9`.
///
/// | runaway | budget | verdict | units | looks | looks a unit |
/// | --- | --- | --- | --- | --- | --- |
/// | spin | 25 000 | budget | 25 000 | 43 036 | 1.7 |
/// | spin | 50 000 | budget | 50 000 | 90 356 | 1.8 |
/// | sums | 25 000 | apart | 6 156 | 5 035 | 0.8 |
/// | sums | 50 000 | apart | 6 156 | 5 027 | 0.8 |
/// | sums, apart | 25 000 | apart | 6 156 | 4 993 | 0.8 |
/// | sums, apart | 50 000 | apart | 6 156 | 4 993 | 0.8 |
/// | sums of calls | 25 000 | apart | 6 157 | 6 939 | 1.1 |
/// | sums of calls | 50 000 | apart | 6 157 | 6 939 | 1.1 |
/// | halvings | 25 000 | apart | 6 156 | 5 086 | 0.8 |
/// | halvings | 50 000 | apart | 6 156 | 5 086 | 0.8 |
/// | rounds | 25 000 | accepted | 5 132 | 3 764 | 0.7 |
/// | rounds | 50 000 | accepted | 5 132 | 3 750 | 0.7 |
/// | classes | 250 000 | budget | 250 000 | 6 170 987 | 24.7 |
/// | classes | 500 000 | apart | 358 500 | 9 955 625 | 27.8 |
/// | powers | 1 000 000 | budget | 1 000 000 | 919 411 | 0.9 |
/// | powers | 2 000 000 | apart | 1 578 686 | 1 653 563 | 1.0 |
/// | images | 1 250 000 | budget | 1 250 000 | 1 639 942 | 1.3 |
/// | images | 2 500 000 | apart | 1 863 562 | 3 182 583 | 1.7 |
/// | nests | 750 000 | apart | 640 338 | 423 524 | 0.7 |
/// | nests | 1 500 000 | apart | 640 338 | 423 524 | 0.7 |
///
/// **Every row but one reads under two nodes a unit, at either budget.** The plain recursion reads 1.7 and then 1.8. The four pairs of calls whose unfolding meets them again are told apart, and the two recursions of one shape held, in what their program's definitions cost to elaborate. The product pays a square of its steps and reads one node a unit, and the image under two. The classes read twenty-five nodes a unit and twenty-eight by the time they answer: every fold asks again which of its atoms are one, a question a unit at least, so the budget bounds them and a unit there buys a comparison.
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
        "{:<14} {:>9} {:<9} {:>10} {:>12} {:>7}",
        "runaway", "budget", "verdict", "units", "looks", "a unit"
    );
    for (label, shape, budget) in rows {
        for budget in [budget, 2 * budget] {
            let stream =
                directory.join(format!("{}-{budget}.trace", label.replace([' ', ','], "")));
            let Cost {
                verdict,
                units,
                looks,
            } = cost(budget, &runaway(shape), &stream);

            println!(
                "{label:<14} {budget:>9} {verdict:<9} {units:>10} {looks:>12} {:>7.1}",
                looks as f64 / units.max(1) as f64
            );
        }
    }
}
