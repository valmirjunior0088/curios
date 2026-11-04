//! What the suites share: a program compiled and run as a fixture states it, the refusal or the stage a fixture reads off it, and a host that answers one row with a scripted reply and every other with a [`MockHost`]'s, which is how one call of a row fails where the mock's script only succeeds.

use {
    crate::*,
    curios_abi::{
        Answer, ChildExit, ChildHandles, ClockReading, Failure, FileStat, Handle, HostOp, HostOps,
        Later, Mode, Refusal, SerialFlow, SerialOp, SerialParity, Settled, StdioMode, Termination,
        TtySize, for_each_host_op,
    },
    curios_common::Qualifier,
    curios_module::RootSource,
    curios_native::{Compiled, ForeignBindings, MockHost, TestEnd},
    curios_pipeline::{DEFAULT_BUDGET, Fold, Stage, TestProgram, compile_with_prelude},
    curios_surface::Entrypoint,
    std::{any::Any, sync::Mutex},
};

/// Compile an already-parsed entrypoint under `loader` and run it.
///
/// Drops any `foreign` declarations' `ForeignStore` — this is the fused compile-and-run path the suites want, with no point to hand it back; a test that has `foreign` declarations to satisfy calls [`compile_with_prelude`] itself, builds [`ForeignBindings`] from the returned store, and calls [`run_wasm`].
pub fn run_entrypoint<H: HostOps + Send + Sync + 'static>(
    entrypoint: &Entrypoint,
    loader: &RootSource,
    host: H,
) -> Result<(), String> {
    let (module, foreigns) = compile_with_prelude(DEFAULT_BUDGET, entrypoint, loader, |_| {})?;

    run_wasm(&module, foreigns, host, ForeignBindings::empty()).map(|_| ())
}

/// Parse `source` (no external modules) and run it.
pub fn run_text<H: HostOps + Send + Sync + 'static>(source: &str, host: H) -> Result<(), String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())?;

    run_entrypoint(&entrypoint, &RootSource::none(), host)
}

pub fn run(source: &str) -> Vec<u8> {
    let (system, io) = MockHost::builder().build();
    run_text(source, system).expect("expected result");
    io.output().to_vec()
}

/// Parse `source` (no external modules) and compile it, without running it: a program compiled once — pipeline, Binaryen, Cranelift — for a fixture that runs it under more than one host. The compile is what a fixture pays for, seconds over the prelude; a run of what it made is milliseconds. So a table whose rows the host input selects costs one compile rather than one per row, which is the difference between `numeric.rs` taking a minute and taking eight.
pub fn compile(source: &str) -> Result<Compiled, String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .map_err(|error| error.format())?;
    let (module, foreigns) =
        compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |_| {})?;

    to_compiled(&module, foreigns)
}

/// Compile-only, for the programs whose point is that they are refused.
pub fn typecheck(source: &str) -> Result<(), String> {
    typecheck_within(DEFAULT_BUDGET, source)
}

/// [`typecheck`] under a stated work budget, for a program whose point is that elaboration *evaluates* something.
///
/// The budget is the only bound reduction has, and it counts *priced work*: a transition costs one unit and a construction costs what it builds, so the memory a reduction allocates as it goes is bounded by the same number that bounds how often it moves. A fixture that would otherwise run a program's whole computation at the type level states a small budget and asserts the refusal instead. Sizing it is the fixture's job: large enough that the rest of the program elaborates, small enough that the evaluation under test cannot finish.
pub fn typecheck_within(budget: u64, source: &str) -> Result<(), String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");

    compile_with_prelude(budget, &entrypoint, &RootSource::none(), |_| {})
        .map(|_| ())
        .map_err(|error| error.to_string())
}

/// Run expecting failure, returning the diagnostic.
pub fn error(source: &str) -> String {
    let (system, _io) = MockHost::builder().build();
    match run_text(source, system) {
        Ok(_) => panic!("expected an error, program succeeded"),
        Err(error) => error.to_string(),
    }
}

/// Compile `source` as its own test program, expecting a refusal, and return its report: [`error`] for a fixture whose refusal is in a test, which only a test program holds.
pub fn tests_error(source: &str) -> String {
    let entrypoint = source.parse::<Entrypoint>().expect("fixture parses");
    Fold::new(DEFAULT_BUDGET, &[], None)
        .tests(
            &entrypoint,
            &RootSource::none().tested(),
            vec![Qualifier::empty()],
            |_| {},
            |_| {},
        )
        .map(|_| ())
        .expect_err("the fixture is refused")
        .to_string()
}

/// Compile `source` as its own test program over every test it declares — the synthesized `Test/main` tail over them in place of the authored one.
pub fn test_program(source: &str) -> TestProgram {
    let entrypoint = source.parse::<Entrypoint>().expect("fixture parses");
    Fold::new(DEFAULT_BUDGET, &[], None)
        .tests(
            &entrypoint,
            &RootSource::none().tested(),
            vec![Qualifier::empty()],
            |_| {},
            |_| {},
        )
        .expect("fixture compiles as a test program")
}

/// [`test_program`], run under a fresh mock host with no arguments, returning what it wrote. That it ran to an answer is asserted, and what it answered is not: a fixture about a verdict reaches for the pieces itself.
pub fn run_tests_program(source: &str) -> Vec<u8> {
    let (module, foreigns) = test_program(source)
        .compiled
        .expect("a test program scheduling a test is built");

    let compiled = to_compiled(&module, foreigns).expect("the test program compiles");

    let (system, io) = MockHost::builder().build();
    let outcome = compiled.run_test(system, ForeignBindings::empty());
    assert!(
        matches!(outcome, Ok(TestEnd::Answered(_))),
        "the test program ran to {outcome:?}"
    );

    io.output().to_vec()
}

/// [`anf_optm`]'s sibling on the synthesized-tail path: the optimized ANF arena of `source` compiled as its own test program.
pub fn anf_optm_tests(source: &str) -> curios_anf::Module {
    let entrypoint = source.parse::<Entrypoint>().expect("fixture parses");

    let mut captured = None;
    Fold::new(DEFAULT_BUDGET, &[], None)
        .tests(
            &entrypoint,
            &RootSource::none().tested(),
            vec![Qualifier::empty()],
            |stage| {
                if let Stage::AnfOptm(module) = stage {
                    captured = Some(module.clone());
                }
            },
            |_| {},
        )
        .expect("fixture compiles as a test program");

    captured.expect("the pipeline emits the optimized ANF stage")
}

/// Compile through production and hand back the optimized ANF arena — the module the door's sequence-usage census judges.
pub fn anf_optm(source: &str) -> curios_anf::Module {
    let entrypoint = source.parse::<Entrypoint>().expect("fixture parses");

    let mut captured = None;
    compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |stage| {
        if let Stage::AnfOptm(module) = stage {
            captured = Some(module.clone());
        }
    })
    .expect("fixture compiles");

    captured.expect("the pipeline emits the optimized ANF stage")
}

/// Whether the census settles `family`'s `constructor` field named `field` in `source` — the named assertion surface over the door's verdict, so a fixture pins the settling by name instead of grepping a dump for the op the settling later becomes.
pub fn census_settles(source: &str, family: &str, constructor: &str, field: &str) -> bool {
    curios_anf::test_support::census_settles_constructor_field(
        &anf_optm(source),
        family,
        constructor,
        field,
    )
}

/// The lowered Core printout of `source` — observed when the rung is reached, whether or not a later stage refuses the program, which is what lets a pin read a transient that elaboration goes on to consume or refuse.
pub fn core(source: &str) -> String {
    let entrypoint = source.parse::<Entrypoint>().expect("fixture parses");

    let mut printed = None;
    let _ = compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |stage| {
        if let Stage::Core(..) = stage {
            printed = Some(stage.to_string());
        }
    });

    printed.expect("the pipeline emits the lowered Core stage")
}

/// `text` with its whitespace taken out: what a Core printout holds read past how it is laid out, since a decorator stands on a line of its own above what it decorates and a list holding one breaks, so a test asks about the terms and not where their lines break.
pub fn unlaid(text: &str) -> String {
    text.chars()
        .filter(|character| !character.is_whitespace())
        .collect()
}

/// The elaborated Core printout of `source`, observed as [`core`] observes the lowered one.
pub fn core_elab(source: &str) -> String {
    let entrypoint = source.parse::<Entrypoint>().expect("fixture parses");

    let mut printed = None;
    let _ = compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |stage| {
        if let Stage::CoreElab(..) = stage {
            printed = Some(stage.to_string());
        }
    });

    printed.expect("the pipeline emits the elaborated Core stage")
}

/// Compile through production and capture the optimized CPS module.
///
/// The module rather than its printout is what a shape assertion should ask: a spelling belongs to the printer, and a scraper over the text can silently match nothing after a printer change, where a program's shape is in its graph.
pub fn cps_optm_module(source: &str) -> curios_cps::Module {
    let entrypoint = source.parse::<Entrypoint>().expect("fixture parses");

    let mut captured = None;
    compile_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none(), |stage| {
        if let Stage::CpsOptm(module) = stage {
            captured = Some(module.clone());
        }
    })
    .expect("fixture compiles");

    captured.expect("the pipeline observes cps-optm")
}

/// Compile through production and capture the optimized CPS printout. For the suites whose subject genuinely is a name in the dump — which declarations survived — rather than an operation.
pub fn cps_optm(source: &str) -> String {
    cps_optm_module(source).to_string()
}

/// Every intrinsic the optimized module emits, tagged by the node kind that carries it, sorted.
///
/// The `Debug` spelling is the comparison key on purpose: it is exact, total, and carries the arity a printed name deliberately leaves to the operand list — `Bytes/chunk` is one name for `BytesChunk(2)` and `BytesChunk(3)`. Nothing here reaches a user, so the Rust spelling costs nothing and cannot drift from the enum it names.
pub fn cps_operations(module: &curios_cps::Module) -> Vec<String> {
    let mut operations: Vec<String> = module
        .nodes()
        .iter()
        .flatten()
        .filter_map(|node| match node {
            curios_cps::Node::LetIntrinsic { op, .. } => Some(format!("{op:?}")),
            curios_cps::Node::Cell { op, .. } => Some(format!("cell.{op:?}")),
            curios_cps::Node::Intrinsic { op, .. } => Some(format!("intrinsic.{op:?}")),
            _ => None,
        })
        .collect();
    operations.sort();
    operations
}

/// Whether a `Nat` literal of `value` is an operand anywhere — how a constant baked into a kernel is found, since nothing names it.
pub fn reads_nat(module: &curios_cps::Module, value: u32) -> bool {
    module.nodes().iter().flatten().any(|node| {
        curios_cps::atoms(node).into_iter().any(|atom| {
            matches!(atom, curios_cps::Atom::Literal(curios_cps::Literal::Nat(literal)) if u32::try_from(literal).ok() == Some(value))
        })
    })
}

/// Whether the optimized module emits an intrinsic the predicate accepts.
pub fn emits(
    module: &curios_cps::Module,
    predicate: impl Fn(&curios_cps::Intrinsic) -> bool,
) -> bool {
    module.nodes().iter().flatten().any(|node| match node {
        curios_cps::Node::LetIntrinsic { op, .. } => predicate(op),
        _ => false,
    })
}

/// A [`MockHost`] with one lie in it: one call to the lying row answers the scripted reply, and every other call the mock's.
pub struct Lying {
    inner: MockHost,
    lie: Mutex<Option<Lie>>,
}

/// A lie not told yet: the row, how many of its calls the mock still answers first, and the reply.
struct Lie {
    op: HostOp,
    honest: usize,
    reply: Box<dyn Any + Send>,
}

impl Lying {
    /// `inner`, except that the first call to `op` answers `reply`, which must be `op`'s reply type.
    pub fn new<R: Send + 'static>(inner: MockHost, op: HostOp, reply: R) -> Self {
        Self::after(inner, op, 0, reply)
    }

    /// `inner`, except that the call to `op` after its first `honest` answers `reply`, which must be `op`'s reply type.
    pub fn after<R: Send + 'static>(inner: MockHost, op: HostOp, honest: usize, reply: R) -> Self {
        Self {
            inner,
            lie: Mutex::new(Some(Lie {
                op,
                honest,
                reply: Box::new(reply),
            })),
        }
    }

    /// The lie, when `op` is the lying row, its honest calls are spent and it has not been told yet.
    fn lie<R: 'static>(&self, op: HostOp) -> Option<R> {
        let mut lie = self.lie.lock().unwrap();

        match lie.take() {
            Some(told) if told.op == op && told.honest == 0 => Some(
                *told
                    .reply
                    .downcast::<R>()
                    .unwrap_or_else(|_| panic!("the lie is not {op:?}'s reply")),
            ),
            Some(mut untold) => {
                if untold.op == op {
                    untold.honest -= 1;
                }
                *lie = Some(untold);

                None
            }
            None => None,
        }
    }
}

/// Every method asks for the lie first and falls back to the mock's answer.
macro_rules! declare_lying_host {
    ($(
        $(#[doc = $doc:literal])*
        $variant:ident: fn $name:ident($($p:ident: $t:ty),* $(,)?) -> $r:ty { $($contract:tt)* }
    )*) => {
        impl HostOps for Lying {
            $(
                fn $name(&self, $($p: $t),*) -> $r {
                    match self.lie::<$r>(HostOp::$variant) {
                        Some(reply) => reply,
                        None => self.inner.$name($($p),*),
                    }
                }
            )*

            // No row answers these three, so there is nothing to lie about.
            fn wait(&self, on: Vec<Handle>) -> Result<(), Refusal> {
                self.inner.wait(on)
            }

            fn later(&self, answer: Answer) -> Result<Settled, Handle> {
                self.inner.later(answer)
            }

            fn settled(&self, h: Handle) -> Result<Option<Settled>, Refusal> {
                self.inner.settled(h)
            }
        }
    };
}

for_each_host_op!(declare_lying_host);
