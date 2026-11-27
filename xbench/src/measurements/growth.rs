//! What each stage does as a program grows, counted.

use {
    crate::{Phase, phase, read_span},
    curios::{TALLIES, optimize, test_support::*},
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_native::test_support::*,
    curios_pipeline::compile_with_prelude,
    curios_profile::Summary,
    curios_surface::Entrypoint,
};

/// The stages a compile passes, in order, each named as the measurement reads it and found by the span its owner opens over it, with the group that span declares: the entrypoint's parse, its lowering, elaboration, the certifier's judgment, erasure, the ANF optimizer, the CPS lowering and its optimizer, emission, Binaryen and Cranelift. What a compile does before its text stage, the fold's preamble, is no stage: no pass runs there.
const STAGES: [(&str, &str, Option<&str>); 11] = [
    ("parse", "parse", Some("entrypoint")),
    ("lower", "into_core_unit", None),
    ("elaborate", "elaborate_and_zonk", None),
    ("certify", "recheck", None),
    ("erase", "erase_unit", None),
    ("anf-optm", "anf_optimize", None),
    ("cps", "lower_to_cps", None),
    ("cps-optm", "cps_optimize", None),
    ("emit", "into_wasm", None),
    ("binaryen", "binaryen_optimize", None),
    ("cranelift", "cranelift_compile", None),
];

/// One compile of `source`, from its parse to Cranelift's machine code, as a phase.
fn compiled(source: &str) -> Phase<()> {
    phase(TALLIES, || {
        let entrypoint = source.parse::<Entrypoint>().expect("a growth parses");
        let (module, _) =
            compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |_| {})
                .expect("a growth compiles");
        start_test_engine();
        curios_cranelift::compile(&optimize(&module))
            .expect("an optimized growth compiles to machine code");
    })
}

/// The span `compiled` folded for `stage`, where the compile reached it.
fn span<'a>(
    compiled: &'a Phase<()>,
    (_, name, group): (&str, &str, Option<&str>),
) -> Option<&'a Summary> {
    compiled
        .profile
        .summaries
        .iter()
        .find(|summary| summary.name == name && summary.group.as_deref() == group)
}

/// What `compiled`'s walks looked at in each stage, in [`STAGES`]' order: what the stage's span added to the look count, nought where the compile never reached it.
fn looks_by_stage(compiled: &Phase<()>) -> [u64; 11] {
    let [look] = TALLIES else {
        panic!("the program hands the recorder one tally, its look count")
    };

    STAGES.map(|stage| {
        span(compiled, stage)
            .and_then(|summary| {
                summary
                    .tallies
                    .iter()
                    .find(|tallied| tallied.name == look.name)
            })
            .map_or(0, |tallied| tallied.added)
    })
}

/// How a column grows across three sizes each double the last: the second difference over the first, so what every compile pays whatever its size cancels. Two is linear, four quadratic. `None` where the column does not grow, a stage the growth never reaches.
fn slope(small: u64, middle: u64, large: u64) -> Option<f64> {
    let first = middle.checked_sub(small)?;
    let second = large.checked_sub(middle)?;
    // Under a fiftieth of what the smallest compile did, a stage's growth is noise around its fixed cost.
    (first > 0 && first * 50 >= small.max(1)).then(|| second as f64 / first as f64)
}

/// The steepest slope of the looks over [`STAGES`] across three sizes, with the stage it is at.
fn steepest(looks: &[[u64; 11]; 3]) -> Option<(f64, &'static str)> {
    (0..STAGES.len())
        .filter_map(|stage| {
            let [small, middle, large] = looks.map(|looks| looks[stage]);
            slope(small, middle, large).map(|slope| (slope, STAGES[stage].0))
        })
        .max_by(|left, right| left.0.total_cmp(&right.0))
}

/// The size each growth is counted at, doubled twice: large enough that its own work outgrows the prelude's and a quadratic term shows past the linear one, small enough that a growth whose stage is quadratic or worse today is counted in seconds.
fn base(growth: Growth) -> usize {
    match growth {
        Growth::LetPlus | Growth::LetCall => 25,
        Growth::Statements
        | Growth::ParametersSummed
        | Growth::CodomainSummed
        | Growth::Lambdas
        | Growth::Lets
        | Growth::ChainedMatches => 100,
        Growth::Matches => 200,
        Growth::ProductCommuted | Growth::Loops => 128,
        Growth::Lists | Growth::ListOpen | Growth::Parameters => 400,
        Growth::Calls | Growth::RightPlus | Growth::LeftPlus | Growth::LetsSummed => 250,
        Growth::Definitions
        | Growth::Functions
        | Growth::Strings
        | Growth::Arms
        | Growth::Tuple
        | Growth::Fields
        | Growth::StructReaders
        | Growth::Constructors
        | Growth::Values
        | Growth::DefaultedMatches
        | Growth::ComparedConstructions
        | Growth::AliasReaders
        | Growth::ArrowAliasReaders
        | Growth::Methods => 250,
        Growth::List | Growth::Parentheses | Growth::Arrow => 1000,
        Growth::StringLiteral => 2000,
    }
}

/// The growths whose looks grow past [`HELD_SLOPE`] today, which the growths' specification holds (`documentation/roadmap/200-a-counted-growth-is-linear-in-its-size.md`): every other growth's looks are held to it in every stage.
const UNHELD: &[Growth] = &[
    Growth::Values,
    Growth::ProductCommuted,
    Growth::ChainedMatches,
    Growth::Lambdas,
    Growth::Lets,
];

/// The steepest slope a growth's looks may show in a stage: two is linear and four quadratic, so three tells them apart with room on both sides, for a logarithm's factor.
const HELD_SLOPE: f64 = 3.0;

/// How each stage's work grows as a program grows along each axis an author grows one: every growth compiled at three sizes, each double the last, each compile a phase from its parse to Cranelift's machine code, `<growth>/x<k>` at the `k`-fold size, and each stage read off the span its owner opens over it ([`STAGES`]), `<growth>/<stage>/x<k>`: its time, its calls, what it allocated and the bytes it took, and its looks, each sized by the growth's `n`.
///
/// `cargo xbench measure growth_slopes` takes it.
///
/// A slope is read off the figures, never filed: the second difference over the first across the three sizes, so what every compile pays whatever its size cancels, two linear and four quadratic. Looks alone repeat from run to run, so the measurement's own assert holds them alone to [`HELD_SLOPE`], in every stage of every growth but the [`UNHELD`] ones, a test of the suite holding them taking most of a minute; allocations and bytes move with the keys a run draws and times with the machine, and are read with their spans in the record, never bounded.
///
/// # Its records
///
/// `cargo xbench report growth_slopes` reads them: each figure's readings by group, and what each sitting was taken under.
pub fn growth_slopes() {
    let mut grew = Vec::new();

    for growth in Growth::ALL {
        let base = base(growth);
        // Compiled once unread first: the prelude's modules are restored where a compile first reads them, so the smallest size would otherwise carry its growth's first restores and steepen its slope.
        compiled(&grown(growth, base));
        let sizes = [base, base * 2, base * 4];
        let compiles = sizes.map(|size| compiled(&grown(growth, size)));

        for ((multiple, size), compile) in [1, 2, 4].into_iter().zip(sizes).zip(&compiles) {
            let size = Some(size as u64);
            compile.read(&format!("{growth:?}/x{multiple}"), size);
            for stage in STAGES {
                if let Some(summary) = span(compile, stage) {
                    read_span(
                        &format!("{growth:?}/{}/x{multiple}", stage.0),
                        summary,
                        size,
                    );
                }
            }
        }

        if UNHELD.contains(&growth) {
            continue;
        }
        if let Some((slope, stage)) = steepest(&compiles.each_ref().map(looks_by_stage))
            && slope > HELD_SLOPE
        {
            grew.push(format!("{growth:?}: looks at {stage} grow {slope:.2}"));
        }
    }

    assert!(grew.is_empty(), "held growths that grew: {grew:#?}");
}
