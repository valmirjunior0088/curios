//! What each runaway costs at a budget and at twice it, counted.

use {
    crate::tests::{Runaway, runaway},
    curios_module::RootSource,
    curios_pipeline::compile_with_prelude,
    curios_profile::{Destination, Report, fold_at, trace},
    curios_surface::Entrypoint,
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
    fn of(report: &Report, checker: &str) -> Option<Self> {
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
/// Over the checkers of `3b5c71a3c`, each claim read after it, two takings: the units and the looks the same at both.
///
/// | runaway | budget | verdict | elaborator units | elaborator looks | looks a unit | kernel units | kernel looks | looks a unit |
/// | --- | --- | --- | --- | --- | --- | --- | --- | --- |
/// | spin | 25 000 | budget | 25 000 | 101 439 | 4.1 | - | - | - |
/// | spin | 50 000 | budget | 50 000 | 206 202 | 4.1 | - | - | - |
/// | sums | 25 000 | apart | 5 129 | 3 825 | 0.7 | - | - | - |
/// | sums | 50 000 | apart | 5 129 | 3 825 | 0.7 | - | - | - |
/// | sums, apart | 25 000 | apart | 5 129 | 3 626 | 0.7 | - | - | - |
/// | sums, apart | 50 000 | apart | 5 129 | 3 626 | 0.7 | - | - | - |
/// | sums of calls | 25 000 | apart | 5 129 | 7 836 | 1.5 | - | - | - |
/// | sums of calls | 50 000 | apart | 5 129 | 7 728 | 1.5 | - | - | - |
/// | halvings | 25 000 | apart | 5 129 | 5 158 | 1.0 | - | - | - |
/// | halvings | 50 000 | apart | 5 129 | 5 158 | 1.0 | - | - | - |
/// | rounds | 25 000 | accepted | 4 105 | 2 892 | 0.7 | 3 485 | 3 438 | 1.0 |
/// | rounds | 50 000 | accepted | 4 105 | 2 892 | 0.7 | 3 485 | 3 438 | 1.0 |
/// | classes | 250 000 | apart | 130 300 | 393 352 | 3.0 | - | - | - |
/// | classes | 500 000 | apart | 130 300 | 393 352 | 3.0 | - | - | - |
/// | powers | 1 000 000 | budget | 1 000 000 | 1 549 145 | 1.5 | - | - | - |
/// | powers | 2 000 000 | apart | 1 584 262 | 2 554 256 | 1.6 | - | - | - |
/// | images | 1 250 000 | budget | 1 250 000 | 2 940 067 | 2.4 | - | - | - |
/// | images | 2 500 000 | apart | 1 868 375 | 5 717 296 | 3.1 | - | - | - |
/// | nests | 750 000 | apart | 595 985 | 310 452 | 0.5 | - | - | - |
/// | nests | 1 500 000 | apart | 595 985 | 310 452 | 0.5 | - | - | - |
/// | stacks | 200 000 | accepted | 44 294 | 347 105 | 7.8 | 20 740 | 463 099 | 22.3 |
/// | stacks | 400 000 | accepted | 44 294 | 347 105 | 7.8 | 20 740 | 463 099 | 22.3 |
/// | walks | 150 000 | accepted | 108 224 | 620 734 | 5.7 | 44 196 | 484 239 | 11.0 |
/// | walks | 300 000 | accepted | 108 224 | 620 734 | 5.7 | 44 196 | 484 239 | 11.0 |
///
/// **Seven rows read under three nodes a unit at either budget, and five read three or more: the spin, the classes, the image by the time it answers, the stacks and the walk.** The plain recursion reads 4.1 at either budget. The four pairs of calls whose unfolding meets them again are told apart at about one node a unit, and the recursions held by both checkers are held in what their program's definitions cost to elaborate. The product pays a square of its steps and reads one and a half nodes a unit, and the image 2.4 where its budget ends it and 3.1 by the time it answers.
///
/// The classes read three nodes a unit and answer inside the smaller budget: every fold asks again which of its atoms are one, a question a unit at least, and one the elaborator has answered since it last wrote is answered for that unit alone (`same_uncommitted_inside`, `curios-elab/src/convert.rs`), so a unit buys a comparison only where the question is new. The stacks cost the elaborator its 44 294 units at near eight nodes each and the kernel 20 740 at twenty-two, and both checkers hold them inside the smaller budget. The walk costs the elaborator 108 224 units at near six nodes each and the kernel 44 196 at eleven, which reduces its claim a step of the recursion at a time, and the rounds cost the kernel 3 485 at one. A row's looks a unit count what its units buy and what readers that charge nothing read beside them, a key consed to a table's canonical node among them, so they move with those readers where the units stand still.
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
        ("walks", Runaway::Walks(2000), 150_000),
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
