//! What each stage does as a program grows, counted.

use {
    crate::{
        optimize,
        tests::{Growth, grown},
    },
    curios_core::take_looks,
    curios_pipeline::{DEFAULT_BUDGET, Stage, compile_with_prelude},
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

/// The two instruction counts a compile is read on, each opened before any pool starts: the measuring thread's alone, which a stage that starts no thread is read on, so a pool's thread waiting for work retires nothing inside it, and the thread's with every thread it starts, which Binaryen and Cranelift are read on, since their passes run on pools ([`THREADED`]).
#[derive(Default)]
struct Counters {
    alone: Option<Instructions>,
    pooled: Option<Instructions>,
}

impl Counters {
    fn open() -> Self {
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
    allocations: u64,
    bytes: u64,
}

impl Mark {
    fn now(counters: &Counters) -> Self {
        Self {
            alone: counters.alone.as_ref().map(Instructions::read),
            pooled: counters.pooled.as_ref().map(Instructions::read),
            allocations: allocation_count() as u64,
            bytes: allocated_bytes() as u64,
        }
    }

    /// What the stage at `stage` of [`STAGES`] did since `self`, and the mark it ends at. Looks are taken, so each read hands back what the stage before it left.
    fn since(self, counters: &Counters, stage: usize) -> (Count, Mark) {
        let now = Mark::now(counters);
        let (before, after) = match THREADED.contains(&STAGES[stage]) {
            true => (self.pooled, now.pooled),
            false => (self.alone, now.alone),
        };
        let count = Count {
            instructions: before.zip(after).map(|(before, after)| after - before),
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
    take_looks();
    let mark = Mark::now(counters);

    let entrypoint = source.parse::<Entrypoint>().expect("a growth parses");
    let (count, mark) = mark.since(counters, 0);
    counts[0] = count;

    let mut mark = Some(mark);
    let (module, _) =
        compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |stage| {
            let index = match stage {
                Stage::Text(_) => 1,
                Stage::Core(_) => 2,
                Stage::CoreElab(_) => 3,
                Stage::Anf(_) => 4,
                Stage::AnfOptm(_) => 5,
                Stage::Cps(_) => 6,
                Stage::CpsOptm(_) => 7,
                Stage::Wasm(_) => 8,
                Stage::WasmOptm(_) => return,
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

    let bytes = optimize(&module, false, |_| {});
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
        Growth::ProductCommuted => 128,
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

/// The growths every stage of which is linear today: each slope at most [`HELD_SLOPE`], and a stage that regresses fails here.
const HELD: &[Growth] = &[
    Growth::Definitions,
    Growth::Strings,
    Growth::List,
    Growth::Parameters,
    Growth::Tuple,
    Growth::Constructors,
    Growth::Methods,
    Growth::Calls,
    Growth::Parentheses,
    Growth::Lets,
    Growth::Arrow,
    Growth::StringLiteral,
    Growth::Statements,
    Growth::Lambdas,
    Growth::ChainedMatches,
    Growth::ProductCommuted,
    Growth::StructReaders,
    Growth::Values,
    Growth::ComparedConstructions,
    Growth::AliasReaders,
    Growth::ArrowAliasReaders,
];

/// The growths held through emission.
const HELD_FRONT: &[Growth] = &[
    Growth::LetPlus,
    Growth::LetCall,
    Growth::Functions,
    Growth::ListOpen,
    Growth::Arms,
    Growth::ParametersSummed,
    Growth::Fields,
    Growth::RightPlus,
    Growth::LeftPlus,
    Growth::Matches,
    Growth::Lists,
    Growth::LetsSummed,
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
/// Alone in its invocation: the instruction counts open before Binaryen's and Wasmtime's pools start, so a stage before Binaryen is read on this thread alone and Binaryen and Cranelift with the pools' threads, and a measurement run beside it would share the allocator's counters. Counted, so the slopes hold on any machine; the instruction column reads `-` where the kernel opens no count.
///
/// # What it last printed
///
/// Over the compiler of `e1945ebc6`, as instructions, looks, allocations and bytes, each the steepest slope and the stage it is at; `-` where no stage grows.
///
/// | growth | n | instructions | looks | allocations | bytes |
/// | --- | --- | --- | --- | --- | --- |
/// | LetPlus | 25 | 2.34 binaryen | 2.00 erase | 2.23 cps-optm | 2.11 cps |
/// | LetCall | 25 | 2.37 binaryen | 2.00 erase | 2.23 cps-optm | 2.73 parse |
/// | Statements | 100 | 2.19 elaborate | 2.00 erase | 2.03 erase | 2.26 binaryen |
/// | Definitions | 250 | 2.03 parse | 2.00 erase | 2.00 erase | 2.58 parse |
/// | Functions | 250 | 3.37 binaryen | 2.00 erase | 2.09 cranelift | 2.74 cranelift |
/// | Strings | 250 | 2.00 parse | 2.00 lower | 2.00 parse | 2.00 binaryen |
/// | List | 1000 | 1.98 erase | 2.00 erase | 2.00 elaborate | 2.00 binaryen |
/// | ListOpen | 400 | 3.80 binaryen | 2.00 erase | 2.11 cps | 2.83 cps |
/// | Arms | 250 | 6.15 binaryen | 2.00 erase | 2.03 cranelift | 2.56 cranelift |
/// | Parameters | 400 | 2.20 anf-optm | 2.00 erase | 2.08 cps | 2.24 anf-optm |
/// | ParametersSummed | 100 | 2.84 binaryen | 2.00 erase | 2.06 cranelift | 2.37 cranelift |
/// | LetsSummed | 250 | 3.94 cranelift | 2.00 erase | 2.17 cps | 3.49 cranelift |
/// | ProductCommuted | 128 | 3.21 cps | 2.00 erase | 2.03 elaborate | 2.82 cps |
/// | Tuple | 250 | 2.05 lower | 2.00 erase | 2.00 parse | 2.03 elaborate |
/// | Fields | 250 | 2.07 elaborate | 2.00 erase | 2.00 erase | 2.01 elaborate |
/// | StructReaders | 250 | 2.16 elaborate | 2.00 erase | 2.00 elaborate | 2.00 elaborate |
/// | Constructors | 250 | 2.18 anf-optm | 2.00 erase | 2.02 anf-optm | 2.46 anf-optm |
/// | Values | 250 | 2.15 lower | 2.00 erase | 2.00 erase | 2.00 erase |
/// | DefaultedMatches | 250 | 2.20 binaryen | 2.00 erase | 2.00 binaryen | 3.00 parse |
/// | ComparedConstructions | 250 | 2.07 binaryen | 2.00 erase | 2.00 elaborate | 2.00 elaborate |
/// | AliasReaders | 250 | 2.02 elaborate | 2.00 erase | 2.00 elaborate | 2.00 erase |
/// | ArrowAliasReaders | 250 | 2.02 parse | 2.00 erase | 2.00 elaborate | 2.00 elaborate |
/// | Methods | 250 | 2.54 cps-optm | 2.00 erase | 2.02 cps | 2.19 cps |
/// | Calls | 250 | 2.01 erase | 2.00 erase | 2.05 cps | 2.75 cps |
/// | Parentheses | 1000 | 2.01 parse | - | 2.00 parse | 2.00 parse |
/// | RightPlus | 250 | 3.47 cranelift | 2.00 erase | 2.07 cranelift | 3.57 cranelift |
/// | LeftPlus | 250 | 3.37 binaryen | 2.00 erase | 2.07 cranelift | 2.71 cps |
/// | Matches | 200 | 3.36 binaryen | 2.00 erase | 2.01 cranelift | 2.80 cranelift |
/// | ChainedMatches | 100 | 2.93 binaryen | 2.00 erase | 2.09 cps-optm | 2.29 cranelift |
/// | Lambdas | 100 | 2.06 cps | 2.47 elaborate | 2.07 erase | 2.52 cps |
/// | Lists | 400 | 3.70 binaryen | 2.00 erase | 2.01 emit | 2.68 anf-optm |
/// | Lets | 100 | 2.13 erase | 2.00 erase | 2.00 elaborate | 2.33 erase |
/// | Arrow | 1000 | 2.06 elaborate | 2.00 erase | 2.08 elaborate | 2.05 elaborate |
/// | StringLiteral | 2000 | 2.01 elaborate | 2.00 erase | 2.00 erase | 2.00 elaborate |
///
/// A product commuted's instructions at `cps` read 3.21 here, above [`HELD_SLOPE`], and 2.38 and 2.16 in two more runs of the same compiler: the hash keys a run draws move them.
///
/// **Twenty-one growths are held in every stage**: definitions, strings, a list literal, a telescope's parameters, a tuple, a family's constructors, a concept's methods, nested calls, parentheses, `let`s, an arrow, a string literal, an `Io` block's statements, nested lambdas, matches each on the `let` before it, a product of two spines commuted, a structure's readers, a family's values, its constructions compared, and a tuple alias's and a function-type alias's readers, the ones [`HELD`] lists. **Twelve more are held through emission**, the ones [`HELD_FRONT`] lists. A family's defaulted matches are held in neither: their parse stage's bytes read 3.00, at the bound. One reading below [`HELD_SLOPE`] at the grid's sizes may still hold a square too small there to show, as nested list literals' bytes in `cps` do.
#[test]
#[ignore = "measurement, counted: run by hand, alone in its invocation"]
fn growth_slopes() {
    let counters = Counters::open();
    let mut regressed = Vec::new();
    let front = STAGES
        .iter()
        .position(|stage| *stage == "binaryen")
        .expect("Binaryen is a stage");

    println!("| growth | n | instructions | looks | allocations | bytes |");
    println!("| --- | --- | --- | --- | --- | --- |");
    for growth in Growth::ALL {
        let base = base(growth);
        // Compiled once uncounted first: the prelude's modules are restored where a compile first reads them, once a process, so the smallest size would otherwise carry its growth's first restores and steepen its slope.
        counted(&grown(growth, base), &Counters::default());
        let counts =
            [base, base * 2, base * 4].map(|size| counted(&grown(growth, size), &counters));
        let shown = |steepest: Option<(f64, &str)>| match steepest {
            Some((slope, stage)) => format!("{slope:.2} {stage}"),
            None => "-".to_string(),
        };
        let columns = |stages: usize| {
            [
                (
                    "instructions",
                    steepest(&counts, stages, |count| count.instructions),
                ),
                (
                    "looks",
                    steepest(&counts, stages, |count| Some(count.looks)),
                ),
                (
                    "allocations",
                    steepest(&counts, stages, |count| Some(count.allocations)),
                ),
                (
                    "bytes",
                    steepest(&counts, stages, |count| Some(count.bytes)),
                ),
            ]
        };
        let every = columns(STAGES.len());
        let [instructions, looks, allocations, bytes] = every.map(|(_, steepest)| steepest);
        println!(
            "| {growth:?} | {base} | {} | {} | {} | {} |",
            shown(instructions),
            shown(looks),
            shown(allocations),
            shown(bytes),
        );

        let held = match (HELD.contains(&growth), HELD_FRONT.contains(&growth)) {
            (true, _) => every,
            (false, true) => columns(front),
            (false, false) => continue,
        };
        for (column, steepest) in held {
            if let Some((slope, stage)) = steepest
                && slope > HELD_SLOPE
            {
                regressed.push(format!("{growth:?}: {column} at {stage} grows {slope:.2}"));
            }
        }
    }

    assert!(
        regressed.is_empty(),
        "held growths that grew: {regressed:#?}"
    );
}
