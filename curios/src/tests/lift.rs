//! The `Lift` embedding vocabulary and auto-lift at `!`.
//!
//! `/syn/Lift(M, N)` declares the canonical embedding of one monad into another, one witness per ordered pair, never chained. `elaborate_bang` reads the region's monad from the expected type (strict postponement — never inferred from the action) and, when the action's declared monad differs from the region's, wraps the action in `/syn/Lift`'s `lift` so the declared edge resolves — or its absence is reported as the missing witness.

use {
    super::run,
    curios_text::{Entrypoint, RootSource},
};

/// Compile-only, for the programs whose point is that they are refused.
fn typecheck(source: &str) -> Result<(), String> {
    let entrypoint = source
        .parse::<Entrypoint>()
        .expect("failed to parse source");

    curios_pipeline::compile_entrypoint(
        crate::DEFAULT_STEP_BUDGET,
        &entrypoint,
        RootSource::none(),
        |_| {},
    )
    .map(|_| ())
    .map_err(|error| error.to_string())
}

/// The acceptance flip: an `Io` action sequenced bare inside an `Async` region, lifted through the `/std/Async` edge with nothing spelled at the call site.
#[test]
fn an_io_action_lifts_into_an_async_region() {
    let source = r#"
        use /std/{Async, print};
        pub let fiber: Async({}) =
            let _ = print("a")!;
            let _ = Async/yield_now!;
            let _ = print("b")!;
            Async/pure(());
        Async/run(fiber)
        "#;

    assert_eq!(run(source), b"ab");
}

/// The explicit spelling stays available and means the same embedding: `lift` is `/syn/Lift`'s method, its target inferred from the region.
#[test]
fn an_explicit_lift_spells_the_same_embedding() {
    let source = r#"
        use /std/{Async, print};
        use /std/Lift/{lift};
        pub let fiber: Async({}) =
            let _ = lift(print("x"))!;
            Async/pure(());
        Async/run(fiber)
        "#;

    assert_eq!(run(source), b"x");
}

/// A pair with no declared edge refuses with the missing `Lift` witness — auto-lift consults the table and never invents an embedding.
#[test]
fn a_missing_edge_reports_the_lift_witness() {
    let source = r#"
        use /std/{Monad, Io, Async};
        pub struct Job(A: Type): pub Type {
            Io(A),
        }
        pub let jpure(@A: Type, a: A) -> Job(A) =
            Job { Io/pure(a) };
        pub let jbind(@A: Type, @B: Type, m: Job(A), f: (A) -> Job(B)) -> Job(B) =
            Job { Io/bind(m.0, (a) => f(a).0) };
        satisfy Monad(Job) {
            pure = jpure,
            bind = jbind,
        }
        pub let job: Job({}) =
            let _ = Async/yield_now!;
            jpure(());
        job.0
        "#;

    let error = typecheck(source).expect_err("a missing edge must refuse");
    // The former-eta display fold renders the goal's monads as bare heads, and the embedding diagnosis speaks in terms of the sequencing rather than the synthesized wrapper.
    assert!(
        error.contains("no witness of Lift(Async, /Job)")
            && error.contains("needed to sequence an Async action in this /Job region"),
        "expected the embedding report with folded formers, got: {error}"
    );
}

/// A chain of declared edges that reaches the target without a composite is reported hop by hop — embeddings never chain automatically, and the report says where each hop lives.
#[test]
fn a_missing_composite_reports_the_declared_chain() {
    let source = r#"
        use /std/{Monad, Lift, Io, print};
        pub struct Job(A: Type): pub Type {
            Io(A),
        }
        pub let jpure(@A: Type, a: A) -> Job(A) =
            Job { Io/pure(a) };
        pub let jbind(@A: Type, @B: Type, m: Job(A), f: (A) -> Job(B)) -> Job(B) =
            Job { Io/bind(m.0, (a) => f(a).0) };
        satisfy Monad(Job) {
            pure = jpure,
            bind = jbind,
        }
        satisfy Lift(Io, Job) {
            lift(action) = Job { action },
        }
        pub struct Sched(A: Type): pub Type {
            Job(A),
        }
        pub let spure(@A: Type, a: A) -> Sched(A) =
            Sched { jpure(a) };
        pub let sbind(@A: Type, @B: Type, m: Sched(A), f: (A) -> Sched(B)) -> Sched(B) =
            Sched { jbind(m.0, (a) => f(a).0) };
        satisfy Monad(Sched) {
            pure = spure,
            bind = sbind,
        }
        satisfy Lift(Job, Sched) {
            lift(action) = Sched { action },
        }
        pub let prog: Sched({}) =
            let _ = print("io into sched")!;
            spure(());
        prog.0.0
        "#;

    let error = typecheck(source).expect_err("no composite Io-into-Sched edge exists");
    assert!(
        error.contains("declared embeddings chain from Io to Sched")
            && error.contains("embeddings never chain automatically"),
        "expected the chain report, got: {error}"
    );
}

/// An action whose head is not a monad at all is called out as such — suggesting an edge that could never be declared would be a trap.
#[test]
fn a_non_monad_action_is_called_out() {
    let source = r#"
        use /std/{Io, Nat, print};
        pub struct Box(A: Type): pub Type {
            A,
        }
        pub let main: Io({}) =
            let b: Box(Nat) = Box { 1 };
            let _ = b!;
            print("unreachable");
        main
        "#;

    let error = typecheck(source).expect_err("a non-monad action must refuse");
    assert!(
        error.contains("Box is not a monad"),
        "expected the non-monad refinement, got: {error}"
    );
}

/// A monad-polymorphic action (`Monad/pure`) has no rigid monad of its own: the oracle abstains, the region pins the flex hole exactly as before auto-lift existed, and nothing is wrapped.
#[test]
fn a_flex_action_still_sequences_without_lifting() {
    let source = r#"
        use /std/{Monad, Io, Nat, print};
        pub let main: Io({}) =
            let n = Monad/pure(65)!;
            print(Nat/to_str(n));
        main
        "#;

    assert_eq!(run(source), b"65");
}

/// Embeddings never chain: an edge into `Job` from `Io` does not carry an `Async` action through `Io`, and the refusal names the pair that is actually missing.
#[test]
fn embeddings_do_not_chain_through_a_middle_monad() {
    let source = r#"
        use /std/{Monad, Lift, Io, Async};
        pub struct Job(A: Type): pub Type {
            Io(A),
        }
        pub let jpure(@A: Type, a: A) -> Job(A) =
            Job { Io/pure(a) };
        pub let jbind(@A: Type, @B: Type, m: Job(A), f: (A) -> Job(B)) -> Job(B) =
            Job { Io/bind(m.0, (a) => f(a).0) };
        satisfy Monad(Job) {
            pure = jpure,
            bind = jbind,
        }
        satisfy Lift(Io, Job) {
            lift(action) = Job { action },
        }
        pub let job: Job({}) =
            let _ = Async/yield_now!;
            jpure(());
        job.0
        "#;

    let error = typecheck(source).expect_err("no Async-into-Job edge exists");
    assert!(
        error.contains("no witness of") && error.contains("Lift"),
        "expected the missing Lift witness, got: {error}"
    );
}

/// Strict postponement: a bang in an inference-position region is refused rather than letting the action elect the monad.
#[test]
fn a_bang_in_an_inference_position_region_is_refused() {
    let source = r#"
        use /std/{Io, print};
        let f = (s) => print(s)!;
        f("never")
        "#;

    let error = typecheck(source).expect_err("an inference-position bang must refuse");
    assert!(
        error.contains("cannot infer") || error.contains("monad of this region"),
        "expected an inference refusal, got: {error}"
    );
}
