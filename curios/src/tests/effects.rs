//! The `/std` effect vocabularies and how a `!` region sequences each: `Io` descriptions, `State` threading, `Throw`'s early return, and the `Lift` edges between them.
//!
//! What each vocabulary *is* stays with its own section below. What they share is the shape these tests pin: a region's monad is read from its expected type, never inferred from the action, and an embedding across monads exists only where a `/syn/Lift` witness declares one.

use super::{run, typecheck};

// === The `Io` vocabulary: constructing, sequencing, and forcing descriptions. ====

#[test]
fn pure_and_bind_sequence_a_description() {
    let source = r#"
        use /std/{Io, print};
        let a : Io({}) = print("a");
        let b : Io({}) = print("b");
        Io/bind(a, (_) => b)
        "#;

    assert_eq!(run(source), b"ab");
}

/// Postfix `!` is `/syn/Monad/bind`, and the `Monad(Io)` witness is what makes it reach `Io`. The explicit chain above is the control.
#[test]
fn bang_sequences_a_description_like_an_explicit_bind() {
    let source = r#"
        use /std/{Io, print};
        let a : Io({}) = print("a");
        let b : Io({}) = print("b");
        let _ = a!;
        b
        "#;

    assert_eq!(run(source), b"ab");
}

/// The result flows through the bind, so a description is not merely a sequencing token.
#[test]
fn bind_passes_the_action_result_to_its_continuation() {
    let source = r#"
        use /std/{Io, Nat, print};
        let n : Io(Nat) = Io/pure(7);
        Io/bind(n, (k) => print(Nat/to_str(k + 1)))
        "#;

    assert_eq!(run(source), b"8");
}

/// The whole of what makes an `Io` a *description*: it erases to a thunk, so binding one to a name and forcing it twice performs it twice.
///
/// `let a = …` names a description without performing it, so the two forces below are what produce the two writes. Calling `print` is not one of them: post-retype it builds an `Io({})` and performs nothing, which is why the name can be reused at all.
#[test]
fn a_description_bound_once_and_forced_twice_performs_twice() {
    let source = r#"
        use /std/{Io, print};
        let step(_ : {}) -> Io({}) = print("x");
        let a : Io({}) = Io/bind(Io/pure(()), step);
        Io/bind(a, (_) => a)
        "#;

    assert_eq!(run(source), b"xx");
}

/// The program tail is where the retype is load-bearing: a tail of non-`Io` type describes nothing to perform, so the pipeline refuses it rather than emitting a program that runs and does nothing.
#[test]
fn a_non_io_tail_is_refused() {
    let error = typecheck(
        r#"
        use /std/{Nat};
        let n : Nat = 7;
        n
        "#,
    )
    .expect_err("a pure tail must be refused");

    assert!(
        error.contains("Nat"),
        "the refusal must name the tail's actual type: {error}"
    );
}

/// `Io` is opaque: no constructors, no `IntrinsicHead` entry, no projection, and above all no eliminator from `Io(T)` to `T`. A scrutinee of description type therefore has nothing to eliminate — a bare `_` would be an irrefutable binder match rather than an elimination, so the arms here are concrete.
#[test]
fn an_io_scrutinee_is_refused() {
    let error = typecheck(
        r#"
        use /std/{Io, print};
        let a : Io({}) = Io/pure(());
        match a
        | true => print("t")
        | false => print("f")
        end
        "#,
    )
    .expect_err("matching a description must be refused");

    assert!(
        !error.is_empty(),
        "the refusal must carry a diagnostic: {error}"
    );
}

/// `Monad(Io)` is occupied by `/std/Io` and cannot be occupied twice. A user program is refused on either of two independent grounds — the orphan rule, since it owns neither `/syn`'s concept nor `/sys`'s type head, and one-witness-per-key — and the operative fact is the same: the program's `!` always means the prelude's witness.
#[test]
fn a_program_cannot_register_a_second_monad_witness_for_io() {
    let error = typecheck(
        r#"
        use /std/{Io, Monad};

        satisfy Monad(Io) {
            pure(x) = /sys/Io/pure(x),
            bind(m, f) = /sys/Io/bind(m, f),
        }

        Io/pure(())
        "#,
    )
    .expect_err("a duplicate Monad(Io) witness must be refused");

    assert!(
        !error.is_empty(),
        "the refusal must carry a diagnostic: {error}"
    );
}

// === `/std/State`: pure, deterministic state threading. ==========================

/// The full vocabulary in one region: `get`, `put`, `modify`, and the `state` constructor, sequenced with `!` and run purely at the boundary.
#[test]
fn state_threads_through_a_bang_region() {
    let source = r#"
        use /std/{Nat, State, print};
        use /std/State/{state, get, put, modify};
        pub let fresh: State(Nat, Nat) =
            let n = get()!;
            let _ = put(n + 1)!;
            State/pure(n);
        pub let steps: State(Nat, {Nat, Nat}) =
            let a = fresh!;
            let _ = modify((n) => n * 2)!;
            let b = state((s) => (s, s + 1))!;
            State/pure((a, b));
        let out = State/run(steps, 3);
        let _ = print(Nat/to_str(out.0.0))!;
        let _ = print(Nat/to_str(out.0.1))!;
        print(Nat/to_str(out.1))
        "#;

    // init 3: fresh yields 3 (state 4), modify doubles to 8, state yields 8 (state 9): (3, 8) with final state 9.
    assert_eq!(run(source), b"389");
}

/// Purity is enforced by an absent edge, not a convention: no `Lift(Io, State(S))` witness exists, so an `Io` action cannot sequence in a `State` region.
#[test]
fn a_state_region_cannot_perform_io() {
    let source = r#"
        use /std/{Nat, State, print};
        pub let leak: State(Nat, {}) =
            let _ = print("effect")!;
            State/pure(());
        let out = State/run(leak, 0);
        print("unreachable")
        "#;

    let error = typecheck(source).expect_err("a State region must refuse Io");
    assert!(
        error.contains("no witness of Lift(Io, /std/State/State")
            || error.contains("no witness of Lift(Io, State"),
        "expected the missing Io edge, got: {error}"
    );
}

// === `/std/Throw`: short-circuiting failure — `raise` and `!` as checked early return over `Result`. ====

/// The success path computes through; the failure path short-circuits at the `raise`, skipping the rest of the region.
#[test]
fn a_raise_short_circuits_the_region() {
    let source = r#"
        use /std/{Nat, Str, Throw, print};
        use /std/Throw/{raise, rescue};
        pub let checked_div(a: Nat, b: Nat) -> Throw(Str, Nat) =
            match 0 < b
            | false => raise("division by zero")
            | true => Throw/pure(a / b)
            end;
        pub let compute(a: Nat, b: Nat) -> Throw(Str, Nat) =
            let q = checked_div(a, b)!;
            Throw/pure(q + 1);
        let ok =
            match Throw/run(compute(10, 2))
            | success(n) => Nat/to_str(n)
            | failure(e) => e
            end;
        let caught =
            match Throw/run(rescue(compute(1, 0), (_) => Throw/pure(0)))
            | success(n) => Nat/to_str(n)
            | failure(e) => e
            end;
        let _ = print(ok)!;
        print(caught)
        "#;

    // 10/2 + 1 = 6; the rescued division by zero yields the handler's 0.
    assert_eq!(run(source), b"60");
}

/// `of` and `run` bridge to the plain `Result` vocabulary in both directions.
#[test]
fn throw_bridges_result_in_both_directions() {
    let source = r#"
        use /std/{Nat, Str, Result, Throw, print};
        use /std/Throw/{of};
        pub let parse_pair(a: Result(Nat, Str), b: Result(Nat, Str)) -> Throw(Str, Nat) =
            let x = of(a)!;
            let y = of(b)!;
            Throw/pure(x + y);
        let shown =
            match Throw/run(parse_pair(Result/success(40), Result/success(2)))
            | success(n) => Nat/to_str(n)
            | failure(e) => e
            end;
        print(shown)
        "#;

    assert_eq!(run(source), b"42");
}

// === The `Lift` embedding vocabulary and auto-lift at `!`. =======================

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
