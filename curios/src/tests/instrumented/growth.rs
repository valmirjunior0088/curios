//! What each stage does as a program grows, counted.

use {
    crate::{
        tests::{Growth, grown},
        *,
    },
    curios_core::looks,
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_native::test_support::*,
    curios_pipeline::{Stage, compile_with_prelude},
    curios_profile::{Instructions, allocated_bytes, allocation_count, count_allocations},
    curios_surface::Entrypoint,
    xbench::{Unit, read},
};

/// What one stage did: the instructions retired where the certifier counts them, the nodes its walks looked at, and its trips through the allocator with the bytes they took.
#[derive(Clone, Copy, Debug, Default)]
struct Count {
    instructions: Option<u64>,
    looks: u64,
    allocations: u64,
    bytes: u64,
}

/// The two instruction counts a compile is read on, each opened before any pool starts: the measuring thread's alone, which a stage that starts no thread is read on, so a pool's thread waiting for work retires nothing inside it, and the thread's with every thread it starts, which Binaryen and Cranelift are read on, since their passes run on pools ([`THREADED`]).
#[derive(Default)]
struct Counters {
    alone: Option<Instructions>,
    pooled: Option<Instructions>,
}

impl Counters {
    /// The instruction counts opened, and the allocator asked to count, since no recorder listens here.
    fn open() -> Self {
        count_allocations();

        Self {
            alone: Instructions::open_alone(),
            pooled: Instructions::open(),
        }
    }
}

/// The counters as they stand, read at a stage's boundary: each column a difference of two of these.
struct Mark {
    alone: Option<u64>,
    pooled: Option<u64>,
    looks: u64,
    allocations: u64,
    bytes: u64,
}

impl Mark {
    fn now(counters: &Counters) -> Self {
        Self {
            alone: counters.alone.as_ref().map(Instructions::read),
            pooled: counters.pooled.as_ref().map(Instructions::read),
            looks: looks(),
            allocations: allocation_count() as u64,
            bytes: allocated_bytes() as u64,
        }
    }

    /// What the stage at `stage` of [`STAGES`] did since `self`, and the mark it ends at.
    fn since(self, counters: &Counters, stage: usize) -> (Count, Mark) {
        let now = Mark::now(counters);
        let (before, after) = match THREADED.contains(&STAGES[stage]) {
            true => (self.pooled, now.pooled),
            false => (self.alone, now.alone),
        };
        let count = Count {
            instructions: before.zip(after).map(|(before, after)| after - before),
            looks: now.looks - self.looks,
            allocations: now.allocations - self.allocations,
            bytes: now.bytes - self.bytes,
        };
        (count, now)
    }
}

/// The stages a compile passes, in order: the parse, each rung the pipeline hands its observer, then Binaryen and Cranelift. `setup` is what the pipeline does before it hands over the text.
const STAGES: [&str; 11] = [
    "parse",
    "setup",
    "lower",
    "elaborate",
    "erase",
    "anf-optm",
    "cps",
    "cps-optm",
    "emit",
    "binaryen",
    "cranelift",
];

/// The stages whose work runs on pools as well as on the thread that asks for it: Binaryen's passes, and Wasmtime's parallel compilation under Cranelift.
const THREADED: [&str; 2] = ["binaryen", "cranelift"];

/// What each stage of one compile of `source` did, in [`STAGES`]' order.
fn counted(source: &str, counters: &Counters) -> [Count; 11] {
    let mut counts = [Count::default(); 11];
    let mark = Mark::now(counters);

    let entrypoint = source.parse::<Entrypoint>().expect("a growth parses");
    let (count, mark) = mark.since(counters, 0);
    counts[0] = count;

    let mut mark = Some(mark);
    let (module, _) =
        compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |stage| {
            let index = match stage {
                Stage::Text(_) => 1,
                Stage::Core(..) => 2,
                Stage::CoreElab(..) => 3,
                Stage::Anf(_) => 4,
                Stage::AnfOptm(_) => 5,
                Stage::Cps(_) => 6,
                Stage::CpsOptm(_) => 7,
                Stage::Wasm(_) => 8,
            };
            let (count, next) = mark
                .take()
                .expect("a stage boundary follows the last")
                .since(counters, index);
            counts[index] = count;
            mark = Some(next);
        })
        .expect("a growth compiles");
    let mark = mark.expect("the last stage left a mark");

    start_test_engine();
    let bytes = optimize(&module);
    let (count, mark) = mark.since(counters, 9);
    counts[9] = count;

    curios_cranelift::compile(&bytes).expect("an optimized growth compiles to machine code");
    let (count, _) = mark.since(counters, 10);
    counts[10] = count;

    counts
}

/// How a column grows across three sizes each double the last: the second difference over the first, so what every compile pays whatever its size cancels. Two is linear, four quadratic. `None` where the column does not grow, a stage the growth never reaches.
fn slope(small: u64, middle: u64, large: u64) -> Option<f64> {
    let first = middle.checked_sub(small)?;
    let second = large.checked_sub(middle)?;
    // Under a fiftieth of what the smallest compile did, a stage's growth is noise around its fixed cost.
    (first > 0 && first * 50 >= small.max(1)).then(|| second as f64 / first as f64)
}

/// The steepest slope over the first `stages` of [`STAGES`] in one column, with the stage it is at.
fn steepest(
    counts: &[[Count; 11]; 3],
    stages: usize,
    column: impl Fn(&Count) -> Option<u64>,
) -> Option<(f64, &'static str)> {
    (0..stages)
        .filter_map(|stage| {
            let [small, middle, large] = counts.each_ref().map(|count| column(&count[stage]));
            slope(small?, middle?, large?).map(|slope| (slope, STAGES[stage]))
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

/// The growths whose looks grow past [`HELD_SLOPE`] today, which the workspace's growth entry holds (`documentation/roadmap/000-findings.md`): every other growth's looks are held to it in every stage.
const UNHELD: &[Growth] = &[
    Growth::Values,
    Growth::ProductCommuted,
    Growth::ChainedMatches,
    Growth::Lambdas,
    Growth::Lets,
];

/// The steepest slope a growth's looks may show in a stage: two is linear and four quadratic, so three tells them apart with room on both sides, for a logarithm's factor.
const HELD_SLOPE: f64 = 3.0;

/// How each stage's work grows as a program grows along each axis an author grows one: every growth compiled at three sizes, each double the last, each stage's counts read as the harness reads a span's, its instructions `<growth>/<stage>/x<k>` at the `k`-fold size, its allocations `…/allocations`, the bytes they took `…/allocated` and its looks `…/looks`, each sized by its `n`.
///
/// `cargo xbench measure growth_slopes` takes it, alone in its invocation as the runner invokes it: the instruction counts open before Binaryen's and Wasmtime's pools start, so a stage before Binaryen is read on this thread alone and Binaryen and Cranelift with the pools' threads. Counted, so the figures hold on any machine as far as their units carry them; a growth reads no instructions where the certifier opens no count.
///
/// A slope is read off the figures, never filed: the second difference over the first across the three sizes, so what every compile pays whatever its size cancels, two linear and four quadratic. Of the four counts, looks alone repeat from run to run, so the measurement's own assert holds them alone to [`HELD_SLOPE`], in every stage of every growth but the [`UNHELD`] ones, a test of the suite holding them taking most of a minute; instructions, allocations and bytes move with the keys a run draws, and are read with their spans in the record, never bounded.
///
/// # What it last printed
///
/// Over the compiler of `ba7b9b9fc`, as instructions, looks, allocations and bytes, each the steepest slope and the stage it is at; `-` where no stage grows.
///
/// | growth | n | instructions | looks | allocations | bytes |
/// | --- | --- | --- | --- | --- | --- |
/// | LetPlus | 25 | 2.49 cps | 2.00 erase | 2.04 elaborate | 4.71 cps |
/// | LetCall | 25 | 2.36 binaryen | 2.00 erase | 2.03 emit | 4.71 cps |
/// | Statements | 100 | 2.85 erase | 2.00 erase | 2.05 cps | 2.21 elaborate |
/// | Definitions | 250 | 2.09 lower | 2.00 erase | 2.00 elaborate | 2.52 parse |
/// | Functions | 250 | 3.36 binaryen | 2.00 erase | 2.16 cranelift | 3.07 cps-optm |
/// | Strings | 250 | 2.16 lower | 2.00 lower | 2.00 parse | 2.10 lower |
/// | List | 1000 | 2.00 parse | 2.00 erase | 2.00 parse | 2.00 anf-optm |
/// | ListOpen | 400 | 3.79 binaryen | 2.00 erase | 2.10 cranelift | 2.82 cps |
/// | Arms | 250 | 3.26 binaryen | 2.00 erase | 2.10 cranelift | 2.98 cranelift |
/// | Parameters | 400 | 3.68 erase | 2.00 erase | 2.01 anf-optm | 2.26 anf-optm |
/// | ParametersSummed | 100 | 2.84 binaryen | 2.00 erase | 2.08 cranelift | 2.24 cps-optm |
/// | CodomainSummed | 100 | 2.69 elaborate | 2.98 elaborate | 2.56 elaborate | 2.88 elaborate |
/// | LetsSummed | 250 | 3.97 cranelift | 2.00 erase | 2.16 cps | 3.48 cranelift |
/// | ProductCommuted | 128 | 3.65 erase | 3.70 erase | 3.62 erase | 3.68 erase |
/// | Tuple | 250 | 2.04 erase | 2.00 erase | 2.00 parse | 2.02 elaborate |
/// | Fields | 250 | 2.11 parse | 2.00 erase | 2.00 parse | 2.03 binaryen |
/// | StructReaders | 250 | 2.18 elaborate | 2.00 erase | 2.00 elaborate | 2.00 elaborate |
/// | Constructors | 250 | 3.81 erase | 2.00 erase | 2.01 anf-optm | 2.80 erase |
/// | Values | 250 | 4.27 erase | 3.92 erase | 3.95 erase | 7.43 erase |
/// | DefaultedMatches | 250 | 2.07 erase | 2.00 erase | 2.00 elaborate | 2.88 parse |
/// | ComparedConstructions | 250 | 2.02 erase | 2.00 erase | 2.00 elaborate | 2.00 erase |
/// | AliasReaders | 250 | 2.16 lower | 2.00 erase | 2.00 elaborate | 2.13 lower |
/// | ArrowAliasReaders | 250 | 3.54 erase | 2.00 erase | 3.26 erase | 3.64 erase |
/// | Methods | 250 | 2.34 elaborate | 2.00 erase | 2.01 cps-optm | 2.71 elaborate |
/// | Calls | 250 | 2.22 cps-optm | 2.00 erase | 2.03 cps-optm | 2.72 cps |
/// | Parentheses | 1000 | 3.87 parse | - | 2.10 parse | 3.95 parse |
/// | RightPlus | 250 | 3.64 cranelift | 2.00 erase | 2.13 cranelift | 3.54 cranelift |
/// | LeftPlus | 250 | 3.37 binaryen | 2.00 erase | 2.17 cranelift | 3.18 cps-optm |
/// | Matches | 200 | 3.46 elaborate | 2.00 erase | 2.06 cranelift | 3.14 cranelift |
/// | ChainedMatches | 100 | 2.92 binaryen | 2.00 erase | 2.22 cranelift | 2.30 cranelift |
/// | Loops | 128 | 6.30 binaryen | 2.00 erase | 2.09 cranelift | 2.94 cranelift |
/// | Lambdas | 100 | 4.84 anf-optm | 3.20 erase | 4.07 anf-optm | 3.47 erase |
/// | Lists | 400 | 3.68 binaryen | 2.00 erase | 2.08 parse | 3.75 parse |
/// | Lets | 100 | 3.34 erase | 2.00 erase | 2.00 parse | 2.42 erase |
/// | Arrow | 1000 | 2.06 parse | 2.00 erase | 2.08 elaborate | 2.03 elaborate |
/// | StringLiteral | 2000 | 2.02 erase | 2.00 erase | 2.00 elaborate | 2.00 elaborate |
///
/// Held growths read past [`HELD_SLOPE`] here, in erasure, parsing, the ANF optimizer, the CPS lowering and elaboration, and the optimizer's bytes over functions and over additions nested either way, which the workspace's findings hold (`documentation/roadmap/000-findings.md`); the hash keys a run draws move some readings, a product commuted's instructions having read 3.21, 2.38 and 2.16 over one compiler.
///
/// **Five growths' looks grow past [`HELD_SLOPE`] today**, the [`UNHELD`] ones: a family's values, a product of two spines commuted, `let`s and nested lambdas at erasure, and matches each on the `let` before it at elaboration; every other growth's looks are held in every stage. One reading below [`HELD_SLOPE`] at the grid's sizes may still hold a square too small there to show.
#[test]
#[ignore = "measurement: taken by cargo xbench measure"]
fn growth_slopes() {
    let counters = Counters::open();
    let mut grew = Vec::new();

    for growth in Growth::ALL {
        let base = base(growth);
        // Compiled once uncounted first: the prelude's modules are restored where a compile first reads them, once a process, so the smallest size would otherwise carry its growth's first restores and steepen its slope.
        counted(&grown(growth, base), &Counters::default());
        let sizes = [base, base * 2, base * 4];
        let counts = sizes.map(|size| counted(&grown(growth, size), &counters));

        for ((multiple, size), stages) in [1, 2, 4].into_iter().zip(sizes).zip(&counts) {
            for (stage, count) in STAGES.iter().zip(stages) {
                let name = format!("{growth:?}/{stage}/x{multiple}");
                let size = Some(size as u64);
                if let Some(instructions) = count.instructions {
                    read(&name, Unit::Instructions, size, instructions);
                }
                read(
                    &format!("{name}/allocations"),
                    Unit::Allocations,
                    size,
                    count.allocations,
                );
                read(&format!("{name}/allocated"), Unit::Bytes, size, count.bytes);
                read(&format!("{name}/looks"), Unit::Count, size, count.looks);
            }
        }

        if UNHELD.contains(&growth) {
            continue;
        }
        if let Some((slope, stage)) = steepest(&counts, STAGES.len(), |count| Some(count.looks))
            && slope > HELD_SLOPE
        {
            grew.push(format!("{growth:?}: looks at {stage} grow {slope:.2}"));
        }
    }

    assert!(grew.is_empty(), "held growths that grew: {grew:#?}");
}
