//! The `Lift` edges between monads: an action or tail lifted along a declared edge, a `Try` lifted by its base, and a missing edge or composite reported by the witness it lacks.

use crate::tests::{run, typecheck};

// === `/std/Try`: the error channel over a base monad, and its edges. ====

/// A `Try(Io, ..)` region sequences an `Io` action through the one generic `Io` edge, whose premise `Lift(Io, M)` is met by the identity edge at `M = Io`, and a bare `Result` through the `Result` edge; `raise` stops the region and `rescue` handles it, and `run` hands the outcome back as data at the boundary.
#[test]
fn a_try_region_over_io_sequences_io_actions_and_results() {
    let source = r#"
        use /std/{Nat, Str, Result, Try, Io, print};
        pub let step(n: Nat) -> Try(Io, Str, Nat) =
            let _ = print("s")!;
            let m = Result/success(n + 1)!;
            match m == 3
            | true => Try/raise("three")
            | false => Try/pure(m)
            end;
        pub let twice() -> Try(Io, Str, Nat) =
            let a = step(1)!;
            let b = step(a)!;
            Try/pure(b);
        let entry() -> /std/Io({}) =
            let outcome = Try/run(Try/rescue(twice(), (e) => Try/pure(Str/len(e))))!;
            match outcome
            | success(n) => print(Nat/to_str(n))
            | failure(e) => print(e)
            end;
        entry()
        "#;

    // Two steps print `s` each; the second raises at 3, and the rescue answers the error's length, 5.
    assert_eq!(run(source), b"ss5");
}

/// A `Try(Io, ..)` region takes every edge at once: a `Try` over `Io`, an `Io` action, and a bare `Result`.
#[test]
fn a_try_region_over_io_takes_every_declared_edge() {
    let source = r#"
        use /std/{Nat, Str, Result, Try, Io, print};
        pub let sync(n: Nat) -> Try(Io, Str, Nat) =
            let _ = print("i")!;
            Try/pure(n * 2);
        pub let body() -> Try(Io, Str, Nat) =
            let a = sync(2)!;
            let _ = print("a")!;
            let b = Result/success(a + 1)!;
            let _ = Result/raise(@Str, @{}, "stop")!;
            Try/pure(b);
        pub let program() -> Io({}) =
            let outcome = Try/run(body())!;
            match outcome
            | success(n) => print(Nat/to_str(n))
            | failure(e) => print(e)
            end;
        program()
        "#;

    assert_eq!(run(source), b"iastop");
}

/// `Try/run`'s declared result is `M(Result(E, A))` with `M` a binder, so the lift oracle reads the base off the argument's own declared shape: an `Io` outcome sequenced in a `Try` region over `Job` lifts through `Lift(Io, Job)` rather than letting unification guess the region's base.
#[test]
fn running_a_try_lifts_by_the_base_its_argument_was_declared_over() {
    let source = r#"
        use /std/{Monad, Nat, Str, Result, Try, Io, print};
        use /std/Monad/{Lift};
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
        pub let step() -> Try(Io, Str, Nat) =
            let _ = print("s")!;
            Try/pure(4);
        pub let wrapped() -> Try(Job, Str, Nat) =
            let outcome = Try/run(step())!;
            let n = outcome!;
            Try/pure(n + 1);
        pub let program() -> Io({}) =
            let r = Try/run(wrapped()).0!;
            match r
            | success(n) => print(Nat/to_str(n))
            | failure(e) => print(e)
            end;
        program()
        "#;

    assert_eq!(run(source), b"s5");
}

/// `Try/rescue`'s declared result is `Try(M, E, A)` with `M` a binder in a *context* slot, so the lift oracle reads the base off the argument in that position: an `Io`-based rescue sequenced in a `Job`-based `Try` region lifts through the `Try` edge, as the action it wraps would have.
#[test]
fn a_try_action_lifts_by_the_base_its_context_argument_was_declared_over() {
    let source = r#"
        use /std/{Monad, Nat, Str, Result, Try, Io, print};
        use /std/Monad/{Lift};
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
        pub let step() -> Try(Io, Str, Nat) =
            let _ = print("s")!;
            Try/raise("e");
        pub let rescued() -> Try(Job, Str, {}) =
            let n = Try/rescue(step(), (e) => Try/pure(4))!;
            let _ = print(Nat/to_str(n))!;
            Try/pure(());
        pub let program() -> Io({}) =
            let r = Try/run(rescued()).0!;
            match r
            | success(_) => print("!")
            | failure(e) => print(e)
            end;
        program()
        "#;

    assert_eq!(run(source), b"s4!");
}

/// The bind's monad is the region's, supplied before any action is checked, so an action of another monad that the oracle cannot read — here a projection, which has no declaration to read — is reported at its own `!` as what it is, and the well-typed actions before it are never blamed: the region is `Job` throughout, not pinned to `Io` by the unreadable action.
#[test]
fn an_unreadable_action_of_another_monad_is_reported_at_its_own_bang() {
    let source = r#"
        use /std/{Monad, Nat, Io, print};
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
        let job() -> Job({}) =
            let _ = jpure(())!;
            let acts = (print("b"), 1);
            let _ = acts.0!;
            jpure(());
        job().0
        "#;

    let error = typecheck(source).expect_err("an unlifted action must refuse");
    assert!(
        error.contains("an action of one monad where another is expected")
            && error.contains("action: Io({})")
            && error.contains("expected: Job(")
            && error.contains("acts.0!")
            && !error.contains("jpure(())!"),
        "expected the report at the projection's own `!`, got: {error}"
    );
}

// === The `Lift` embedding vocabulary and auto-lift at `!`. =======================

/// An `Io` action sequenced bare inside a region of a monad with a declared edge from `Io`, lifted through that edge with nothing spelled at the call site.
#[test]
fn an_io_action_lifts_into_a_region_with_a_declared_edge() {
    let source = r#"
        use /std/{Monad, Io, print};
        use /std/Monad/{Lift};
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
        pub let job() -> Job({}) =
            let _ = print("a")!;
            let _ = jpure(())!;
            let _ = print("b")!;
            jpure(());
        job().0
        "#;

    assert_eq!(run(source), b"ab");
}

/// A region's tail is the second place an embedding is written: an `Io` action ending a `Job` region is lifted through the same edge a `!` would use, and so is one ending a match arm, which is a region of its own.
#[test]
fn an_io_tail_lifts_into_a_region_with_a_declared_edge() {
    let source = r#"
        use /std/{Monad, Bool, Io, print};
        use /std/Monad/{Lift};
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
        pub let job(flag: Bool) -> Job({}) =
            let _ = jpure(())!;
            match flag
            | true => print("t")
            | false => print("f")
            end;
        pub let plain: Job({}) =
            print("p");
        Io/bind(job(true).0, (_) => plain.0)
        "#;

    assert_eq!(run(source), b"tp");
}

/// A format print's result is computed from its format string, which the oracle reads in where it is a literal: the print ends a `Try` region and sequences in one with nothing spelled, as an `Io` action whose result is written out does.
#[test]
fn a_format_print_lifts_into_a_try_region() {
    let source = r#"
        use /std/{Str, Try, Io, Fmt};
        let first() -> Try(Io, Str, {}) =
            let _ = Fmt/print("a %\n")(1)!;
            Fmt/print("b %\n")(2);
        first()
        "#;

    assert_eq!(run(source), b"a 1\nb 2\n");
}

/// A tail with no declared edge reports the missing embedding as a `!` does, rather than a bare type mismatch: both heads are monads, so the report names the edge to declare.
#[test]
fn a_tail_with_no_edge_reports_the_lift_witness() {
    let source = r#"
        use /std/{Monad, Io};
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
            Io/yield();
        job.0
        "#;

    let error = typecheck(source).expect_err("a missing edge must refuse");
    assert!(
        error.contains("no witness of Monad/Lift(Io, Job)"),
        "expected the embedding report, got: {error}"
    );
}

/// The explicit spelling stays available and means the same embedding: `lift` is `/std/Monad/Lift`'s method, its target inferred from the region.
#[test]
fn an_explicit_lift_spells_the_same_embedding() {
    let source = r#"
        use /std/{Monad, Io, print};
        use /std/Monad/{Lift};
        use /std/Monad/Lift/{lift};
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
        pub let job() -> Job({}) =
            let _ = lift(print("x"))!;
            jpure(());
        job().0
        "#;

    assert_eq!(run(source), b"x");
}

/// A pair with no declared edge refuses with the missing `Lift` witness — auto-lift consults the table and never invents an embedding.
#[test]
fn a_missing_edge_reports_the_lift_witness() {
    let source = r#"
        use /std/{Monad, Io};
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
        pub let job() -> Job({}) =
            let _ = Io/yield()!;
            jpure(());
        job().0
        "#;

    let error = typecheck(source).expect_err("a missing edge must refuse");
    // The former-eta display fold renders the goal's monads as bare heads, and the embedding diagnosis speaks in terms of the sequencing rather than the synthesized wrapper.
    assert!(
        error.contains("no witness of Monad/Lift(Io, Job)")
            && error.contains("needed to sequence an Io action in this Job region"),
        "expected the embedding report with folded formers, got: {error}"
    );
}

/// A pair of monads spelled through a type alias is reported as written. Unification solves the monads reduced, and `Parse(Str, …)` reduces to its representation — `Of(Str, (input, offset) => Boundary(input, offset))` — so the report reads the monads the auto-lift site named instead: the region's type without its value slot, and the action's declared result the same way.
#[test]
fn a_missing_edge_between_aliased_monads_reports_them_as_written() {
    let source = r#"
        use /std/{Byte, Str, Parse};
        pub let mixed() -> Parse(Str, Byte) =
            let b = Parse/bytes/byte!;
            Parse/pure(b);
        /std/Io/pure(())
        "#;

    let error = typecheck(source).expect_err("a byte action in a text region must refuse");
    assert!(
        error.contains("no witness of /std/Monad/Lift(Parse(/std/Bytes), Parse(Str)) found")
            && error.contains(
                "needed to sequence a Parse(/std/Bytes) action in this Parse(Str) region"
            ),
        "expected the embedding report with the monads as written, got: {error}"
    );
}

/// A chain of declared edges that reaches the target without a composite is reported hop by hop — embeddings never chain automatically, and the report says where each hop lives.
#[test]
fn a_missing_composite_reports_the_declared_chain() {
    let source = r#"
        use /std/{Monad, Io, print};
        use /std/Monad/{Lift};
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
        pub let prog() -> Sched({}) =
            let _ = print("io into sched")!;
            spure(());
        prog().0.0
        "#;

    let error = typecheck(source).expect_err("no composite Io-into-Sched edge exists");
    assert!(
        error.contains("declared embeddings chain from Io to Sched")
            && error.contains("embeddings never chain automatically"),
        "expected the chain report, got: {error}"
    );
}

/// An action that may fail has no edge into a monad that cannot hold a failure, so no witness is to be declared: the refusal names the function that hands the outcome back where the action stands, and that a region which is itself a `Try` sequences it.
#[test]
fn a_try_sequenced_in_a_region_with_no_edge_for_it_is_told_what_is_written_instead() {
    let error = typecheck(
        r#"
        use /std/{Result, Io, Try, File, Path, print};
        let entry() -> /std/Io({}) =
            let text = File/read_all(Path/of_str("notes.txt"))!;
            print("read");
        entry()
        "#,
    )
    .expect_err("a `Try` has no edge into `Io`");

    assert!(
        error.contains("needed to sequence a (x) => Try(Io, Io/Error, x) action in this Io region"),
        "expected the missing edge, got: {error}"
    );
    assert!(
        error.contains(
            "Try/run hands the action's Result back to this region, and a region whose own type is a Try sequences it"
        ),
        "expected what is written instead, got: {error}"
    );
}

/// The same refusal between two monads neither of which is a `Try` offers no runner: an edge between them is the reader's to declare.
#[test]
fn a_missing_edge_between_monads_that_hold_no_failure_offers_no_runner() {
    let error = typecheck(
        r#"
        use /std/{Monad, Io};
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
        pub let job() -> Job({}) =
            let _ = Io/yield()!;
            jpure(());
        job().0
        "#,
    )
    .expect_err("no Io-into-Job edge exists");

    assert!(
        error.contains("no witness of") && error.contains("Lift"),
        "expected the missing edge, got: {error}"
    );
    assert!(!error.contains("hands the action's"), "{error}");
}
