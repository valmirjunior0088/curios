//! End-to-end coverage for the two totality obligations.
//!
//! Erasure deletes types and it deletes `Prop`-sorted proofs, and both must be
//! total: a divergent type breaks type formation, a divergent proof proves
//! anything. What erasure *keeps* may diverge freely, which is why every
//! rejection here is of a position and never of a definition.
//!
//! The size lattice and the classifier are unit-tested in
//! `curios-core/src/totality/tests.rs`; these check what a user can observe,
//! through the prelude-replay path a real program takes — where the analysis
//! sees only the user suffix and reads the prelude's verdicts back from the
//! archive.
//!
//! Each rejection asserts the *diagnostic*, not merely that compilation failed.
//! A soundness test that accepts any error is worthless: a typo in the fixture
//! would pass it while the hole stayed open.

use {super::run, curios_runtime::MockHost};

fn rejected(source: &str) {
    let (system, _io) = MockHost::builder().build();
    let error =
        crate::run_text(source, system).expect_err("expected the erased position to be rejected");
    assert!(
        error.contains("not known to terminate") || error.contains("does not terminate"),
        "rejected, but not by the totality gate:\n{error}",
    );
}

/// As [`rejected`], and by **(V)** rather than by (T).
///
/// Which gate fires is the whole claim of a (V)-only fixture: (T) runs first,
/// so a fixture that accidentally put a partial definition in a type position
/// would pass `rejected` while proving nothing about proof positions.
fn rejected_as_a_proof(source: &str) {
    let (system, _io) = MockHost::builder().build();
    let error =
        crate::run_text(source, system).expect_err("expected the proof position to be rejected");
    assert!(
        error.contains("is a proof position"),
        "rejected, but not as a proof position:\n{error}",
    );
}

/// The negative functor every exploit below is built from. It is *accepted* —
/// strict positivity asks whether a declaration reaches itself, and `Sink`
/// never does. Only tying `A` back to `Sink(A)` is dangerous, and no `induct`
/// can express that.
const SINK: &str = r#"
    induct Sink(A : Type) : pub Type
    | sink(f : (A) -> /std/False)
    end
"#;

// The route no analysis of *value*-level partiality can see. The exploit has
// no value-level `rec`, no `exit`, and no negative occurrence in any
// declaration: a lambda, a constructor application, and one application. The
// knot is tied entirely by `rec Bad : Type`, which `check_positivity` never
// looks at, because a `rec` is neither an `induct` nor a `struct`.
#[test]
fn a_type_level_rec_cannot_tie_the_negative_knot() {
    rejected(&format!(
        r#"
        {SINK}

        rec Bad : Type = Sink(Bad);

        let delta(x : Bad) -> /std/False =
            match x
            | sink(g) => g(x)
            end;

        let boom : /std/False = delta(Sink/sink(delta));

        /std/print("unreachable")
        "#
    ));
}

// The same knot behind a projection. A rule keyed on "the `rec` member is
// itself sort-valued" misses this: `P`'s type is a tuple, and only `P.0` is a
// type.
#[test]
fn a_partial_type_behind_a_projection_is_still_a_type() {
    rejected(&format!(
        r#"
        {SINK}

        rec P : {{Type, /std/Nat}} = (Sink(P.0), 0);

        let delta(x : P.0) -> /std/False =
            match x
            | sink(g) => g(x)
            end;

        let boom : /std/False = delta(Sink/sink(delta));

        /std/print("unreachable")
        "#
    ));
}

// Why (T) has to be the aggressive reading. Nothing here is a partial type
// former: `Shape` is structurally recursive and total, and `inf` is an
// ordinary partial *value* of an ordinary data type. The dangerous type exists
// only because a total function was applied to a bottom argument, so a rule
// that looks at what a type-level eliminator scrutinizes does not see it.
#[test]
fn a_total_type_function_applied_to_a_partial_value_is_rejected() {
    rejected(&format!(
        r#"
        {SINK}

        induct F : pub Type
        | stop()
        | more(rest : F)
        end

        rec Shape(f : F) -> Type =
            match f
            | stop() => /std/False
            | more(rest) => Sink(Shape(rest))
            end;

        rec inf : F = F/more(inf);

        let delta(x : Shape(inf)) -> /std/False =
            match x
            | sink(g) => g(x)
            end;

        let boom : /std/False = delta(Sink/sink(delta));

        /std/print("unreachable")
        "#
    ));
}

// The entrypoint's trailing expression is not a top-level item — lowering
// leaves it in `Module::body`. An obligation seeded only from `items` sees
// nothing here. The only items below are `Sink` and `consume`, and neither is
// partial; the offending `rec` lives entirely inside the final term.
#[test]
fn the_entrypoint_expression_is_not_a_blind_spot() {
    rejected(&format!(
        r#"
        {SINK}

        let consume(u : {{}}) -> {{}} = u;

        consume(
            let forge(x : (rec Bad : Type = Sink(Bad); Bad)) -> {{}} = ();
            ()
        )
        "#
    ));
}

// The shape that used to abort the compiler rather than reject the program.
// Reducing `Bad` rebuilds an arrow whose domain is `Bad`, so elaborating the
// first *use* overflowed the stack — long before any whole-module gate could
// run. This is what the local form at `rec` elaboration exists for, and the
// only claim it makes is that the compiler answers instead of dying.
#[test]
fn a_type_level_rec_through_an_arrow_is_diagnosed_not_aborted() {
    rejected(
        r#"
        rec Bad : Type = (Bad) -> /std/False;

        let delta(x : Bad) -> /std/False = x(x);

        let boom : /std/False = delta(delta);

        /std/print("unreachable")
        "#,
    );
}

// The same shape as a *local* `rec`. `Item::Rec` and `Subterm::Rec` are
// separate elaboration paths, and the second one still aborted after the first
// was fixed.
#[test]
fn a_local_type_level_rec_through_an_arrow_is_diagnosed_too() {
    rejected(
        r#"
        let consume(u : {}) -> {} = u;

        consume(
            let forge(x : (rec Bad : Type = (Bad) -> /std/False; Bad)) -> {} = ();
            ()
        )
        "#,
    );
}

// The first route (T) cannot see. `Never` is `Type`-sorted precisely so that
// `proc/exit` is retained where it is written — but eliminating it into a
// *proposition* puts the never-returning call back in a position erasure
// deletes, so the exit does not fire and `boom` is a closed inhabitant of
// `False`. No type here reaches anything partial: the offender is reached only
// through a term whose declared type is `Prop`-sorted.
#[test]
fn an_exit_eliminated_into_a_proposition_is_rejected() {
    rejected_as_a_proof(
        r#"
        use /std/{proc};

        let forge() -> /std/False =
            match proc/exit(1) : (_) => /std/False
            end;

        let boom : /std/False = forge();

        /std/print("unreachable")
        "#,
    );
}

// The second route (T) cannot see, and the one that needs no `exit` at all.
// `forge` is an ordinary partial *value* at a `Type`-sorted carrier — nothing
// about `Box` or its type mentions a partial definition — and the certificate
// escapes through an arm binder, so `boom`'s body reaches `forge` without
// naming a partial type anywhere.
#[test]
fn a_partial_carrier_releasing_a_proof_is_rejected() {
    rejected_as_a_proof(
        r#"
        induct Box : pub Type
        | box(p : /std/False)
        end

        rec forge(n : /std/Nat) -> Box = forge(n + 1);

        let boom : /std/False =
            match forge(0)
            | box(p) => p
            end;

        /std/print("unreachable")
        "#,
    );
}

// General recursion is untouched wherever erasure keeps it. `collatz` cannot
// pass any size-change checker — it recurses on a computed quotient, and
// whether it terminates at all is an open problem — and it runs anyway,
// because its result is a `Nat` the program actually consumes.
#[test]
fn a_partial_definition_stays_usable_in_a_program() {
    let source = r#"
        use /std/{Nat};

        rec collatz(n : Nat, steps : Nat) -> Nat =
            choose
            | n <= 1 => steps
            | n % 2 == 0 => collatz(n / 2, steps + 1)
            | _ => collatz(3 * n + 1, steps + 1)
            end;

        /std/print(Nat/to_str(collatz(6, 0)))
        "#;
    assert_eq!(run(source), b"8");
}

// The corpus shapes the design is keyed to, exercised through the replay path
// rather than the prelude build. `BigNat`'s arithmetic rests on `add/raw`,
// which descends on *either* of two `Bits` depending on the arm — a shape only
// match refinement can see — and `Fmt/print`'s result type is
// `format_type_with(parse(s))`, a type-level `rec` over a parsed format.
#[test]
fn the_prelude_shapes_a_user_program_leans_on_still_elaborate() {
    let source = r#"
        use /std/{BigNat, Fmt};

        Fmt/print("%-%")(BigNat/to_str(BigNat/of_nat(99999999) + BigNat/of_nat(1)))(3)
        "#;
    assert_eq!(run(source), b"100000000-3");
}
