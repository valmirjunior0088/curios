//! What each stage does as a program grows, counted.

use {
    crate::{
        optimize,
        tests::{Growth, grown},
    },
    curios_core::take_looks,
    curios_pipeline::{DEFAULT_STEP_BUDGET, Stage, compile_with_prelude},
    curios_profile::{Instructions, allocated_bytes, allocation_count},
    curios_text::{Entrypoint, RootSource},
};

/// What one stage did: the instructions retired where the kernel counts them, the nodes its walks looked at, and its trips through the allocator with the bytes they took.
#[derive(Clone, Copy, Debug, Default)]
struct Count {
    instructions: Option<u64>,
    looks: u64,
    allocations: u64,
    bytes: u64,
}

/// The counters as they stand, read at a stage's boundary: each column a difference of two of these.
struct Mark {
    instructions: Option<u64>,
    allocations: u64,
    bytes: u64,
}

impl Mark {
    fn now(counter: Option<&Instructions>) -> Self {
        Self {
            instructions: counter.map(Instructions::read),
            allocations: allocation_count() as u64,
            bytes: allocated_bytes() as u64,
        }
    }

    /// What was done since `self`, and the mark it ends at. Looks are taken, so each read hands back what the stage before it left.
    fn since(self, counter: Option<&Instructions>) -> (Count, Mark) {
        let now = Mark::now(counter);
        let count = Count {
            instructions: self
                .instructions
                .zip(now.instructions)
                .map(|(before, after)| after - before),
            looks: take_looks(),
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
    "ersd-optm",
    "cont",
    "cont-optm",
    "emit",
    "binaryen",
    "cranelift",
];

/// What each stage of one compile of `source` did, in [`STAGES`]' order.
fn counted(source: &str, counter: Option<&Instructions>) -> [Count; 11] {
    let mut counts = [Count::default(); 11];
    take_looks();
    let mark = Mark::now(counter);

    let entrypoint = source.parse::<Entrypoint>().expect("a growth parses");
    let (count, mark) = mark.since(counter);
    counts[0] = count;

    let mut mark = Some(mark);
    let (module, _) = compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| {
            let index = match stage {
                Stage::Text(_) => 1,
                Stage::Core(_) => 2,
                Stage::CoreElab(_) => 3,
                Stage::Ersd(_) => 4,
                Stage::ErsdOptm(_) => 5,
                Stage::Cont(_) => 6,
                Stage::ContOptm(_) => 7,
                Stage::Wasm(_) => 8,
                Stage::WasmOptm(_) => return,
            };
            let (count, next) = mark
                .take()
                .expect("a stage boundary follows the last")
                .since(counter);
            counts[index] = count;
            mark = Some(next);
        },
    )
    .expect("a growth compiles");
    let mark = mark.expect("the last stage left a mark");

    let bytes = optimize(&module, false, |_| {});
    let (count, mark) = mark.since(counter);
    counts[9] = count;

    curios_cranelift::compile(&bytes).expect("an optimized growth compiles to machine code");
    let (count, _) = mark.since(counter);
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

/// The steepest slope over the stages of one column, with the stage it is at.
fn steepest(
    counts: &[[Count; 11]; 3],
    column: impl Fn(&Count) -> Option<u64>,
) -> Option<(f64, &'static str)> {
    (0..STAGES.len())
        .filter_map(|stage| {
            let [small, middle, large] = counts.each_ref().map(|count| column(&count[stage]));
            slope(small?, middle?, large?).map(|slope| (slope, STAGES[stage]))
        })
        .max_by(|left, right| left.0.total_cmp(&right.0))
}

/// The size each growth is counted at, doubled twice: large enough that its own work outgrows the prelude's and a quadratic term shows past the linear one, small enough that a growth whose stage is quadratic or worse today is counted in seconds.
fn base(growth: Growth) -> usize {
    match growth {
        Growth::LetPlus | Growth::LetCall | Growth::Statements | Growth::ParametersSummed => 25,
        Growth::Matches | Growth::Lambdas | Growth::Lets => 25,
        Growth::Lists => 100,
        Growth::ListOpen | Growth::Parameters => 100,
        Growth::Calls | Growth::RightPlus | Growth::LeftPlus => 250,
        Growth::Definitions
        | Growth::Functions
        | Growth::Strings
        | Growth::Arms
        | Growth::Tuple
        | Growth::Fields
        | Growth::Constructors
        | Growth::Methods => 250,
        Growth::List | Growth::Parentheses | Growth::Arrow => 1000,
        Growth::StringLiteral => 2000,
    }
}

/// The growths every stage of which is linear today: each slope at most [`HELD_SLOPE`], and a stage that regresses fails here.
const HELD: &[Growth] = &[
    Growth::Definitions,
    Growth::Strings,
    Growth::List,
    Growth::Constructors,
    Growth::Parentheses,
    Growth::Lets,
    Growth::Arrow,
    Growth::StringLiteral,
];

/// The steepest slope a held growth's stage may show: two is linear and four quadratic, so three tells them apart with room on both sides — for a logarithm's factor, and for the instructions a run's drawn hash keys move in a stage that grows little.
const HELD_SLOPE: f64 = 3.0;

/// How each stage's work grows as a program grows along each axis an author grows one: the slope of instructions, node looks, allocations and bytes across three sizes, at the steepest stage of each.
///
/// # How to take it
///
/// ```sh
/// cargo test --all-features --package curios --lib -- --ignored --nocapture growth_slopes
/// ```
///
/// Alone in its invocation: the instruction count opens before Binaryen's and Wasmtime's pools start, so their threads are counted with this one's, and a measurement run beside it would share the allocator's counters. Counted, so the slopes hold on any machine; the instruction column reads `-` where the kernel opens no count.
///
/// # What it last printed
///
/// Over the compiler of `ad8841072`, as instructions, looks, allocations and bytes, each the steepest slope and the stage it is at; `-` where no stage grows.
///
/// | growth | n | instructions | looks | allocations | bytes |
/// | --- | --- | --- | --- | --- | --- |
/// | LetPlus | 25 | 3.60 binaryen | 2.00 erase | 2.07 ersd-optm | 2.11 cont-optm |
/// | LetCall | 25 | 3.61 binaryen | 2.00 erase | 2.07 ersd-optm | 2.73 parse |
/// | Statements | 25 | 2.80 emit | 2.00 erase | 2.57 ersd-optm | 2.32 ersd-optm |
/// | Definitions | 250 | 2.02 elaborate | 2.00 erase | 2.00 elaborate | 2.00 binaryen |
/// | Functions | 250 | 3.96 binaryen | 2.00 erase | 2.15 cranelift | 3.54 cranelift |
/// | Strings | 250 | 2.10 elaborate | 2.00 lower | 2.00 parse | 2.00 binaryen |
/// | List | 1000 | 1.97 parse | 2.00 erase | 2.00 elaborate | 2.00 elaborate |
/// | ListOpen | 100 | 3.12 binaryen | 2.00 erase | 2.32 cont-optm | 3.67 cont |
/// | Arms | 250 | 2.80 binaryen | 2.00 erase | 2.00 cont | 2.50 binaryen |
/// | Parameters | 100 | 3.94 cont | 2.00 erase | 1.99 erase | 2.01 erase |
/// | ParametersSummed | 25 | 2.54 cranelift | 2.00 erase | 2.44 cont | 2.39 cont-optm |
/// | Tuple | 250 | 2.08 ersd-optm | 2.00 erase | 2.00 parse | 2.02 elaborate |
/// | Fields | 250 | 2.03 parse | 2.00 erase | 2.00 parse | 2.92 binaryen |
/// | Constructors | 250 | 2.35 erase | 2.00 erase | 2.01 ersd-optm | 2.44 ersd-optm |
/// | Methods | 250 | 2.90 cont-optm | 2.00 erase | 2.10 cont-optm | 2.18 cont |
/// | Calls | 250 | 2.45 cont-optm | 2.00 erase | 2.04 cont | 2.73 cont |
/// | Parentheses | 1000 | 2.71 lower | - | 2.00 parse | 2.00 parse |
/// | RightPlus | 250 | 2.96 binaryen | 2.00 erase | 2.14 cranelift | 3.77 cranelift |
/// | LeftPlus | 250 | 3.04 binaryen | 2.00 erase | 2.13 cranelift | 2.84 cranelift |
/// | Matches | 25 | 3.60 emit | 2.00 erase | 3.15 emit | 4.90 cont |
/// | Lambdas | 25 | 2.89 cont-optm | 2.11 elaborate | 2.64 cont-optm | 2.62 cont-optm |
/// | Lists | 100 | 2.70 cont-optm | 2.00 erase | 2.15 cont-optm | 3.13 cont |
/// | Lets | 25 | 2.09 erase | 2.00 erase | 2.00 parse | 2.27 erase |
/// | Arrow | 1000 | 2.07 erase | 2.00 erase | 2.00 elaborate | 2.00 elaborate |
/// | StringLiteral | 2000 | 2.01 erase | 2.00 erase | 2.00 erase | 2.00 erase |
///
/// **Eight growths are held linear in every stage**: definitions, strings, a list literal, a family's constructors, parentheses, `let`s, an arrow and a string literal, the ones [`HELD`] lists. A growth is held where the stage that steepened it has been taken up: one reading below [`HELD_SLOPE`] at the grid's sizes may still hold a square too small there to show, as nested list literals' bytes in `cont` do. Every other growth reaches a stage that grows as its square or steeper, and the stage each names is where `documentation/roadmap/compilation/100-what-is-written-is-compiled-in-its-size.md` takes it up.
#[test]
#[ignore = "measurement, counted: run by hand, alone in its invocation"]
fn growth_slopes() {
    let counter = Instructions::open();
    let mut regressed = Vec::new();

    println!("| growth | n | instructions | looks | allocations | bytes |");
    println!("| --- | --- | --- | --- | --- | --- |");
    for growth in Growth::ALL {
        let base = base(growth);
        // Compiled once uncounted first: the prelude's modules are restored where a compile first reads them, once a process, so the smallest size would otherwise carry its growth's first restores and steepen its slope.
        counted(&grown(growth, base), None);
        let counts =
            [base, base * 2, base * 4].map(|size| counted(&grown(growth, size), counter.as_ref()));
        let shown = |steepest: Option<(f64, &str)>| match steepest {
            Some((slope, stage)) => format!("{slope:.2} {stage}"),
            None => "-".to_string(),
        };
        let instructions = steepest(&counts, |count| count.instructions);
        let looks = steepest(&counts, |count| Some(count.looks));
        let allocations = steepest(&counts, |count| Some(count.allocations));
        let bytes = steepest(&counts, |count| Some(count.bytes));
        println!(
            "| {growth:?} | {base} | {} | {} | {} | {} |",
            shown(instructions),
            shown(looks),
            shown(allocations),
            shown(bytes),
        );

        if HELD.contains(&growth) {
            for (column, steepest) in [
                ("instructions", instructions),
                ("looks", looks),
                ("allocations", allocations),
                ("bytes", bytes),
            ] {
                if let Some((slope, stage)) = steepest
                    && slope > HELD_SLOPE
                {
                    regressed.push(format!("{growth:?}: {column} at {stage} grows {slope:.2}"));
                }
            }
        }
    }

    assert!(
        regressed.is_empty(),
        "held growths that grew: {regressed:#?}"
    );
}
