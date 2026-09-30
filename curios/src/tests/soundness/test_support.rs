//! Programs the soundness suites compile and run.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use {crate::tests::run_text, curios_runtime::MockHost};

pub(super) fn rejected(source: &str) {
    let (system, _io) = MockHost::builder().build();
    let error = run_text(source, system).expect_err("expected the erased position to be rejected");
    assert!(
        error.contains("not known to terminate") || error.contains("does not terminate"),
        "rejected, but not by the totality gate:\n{error}",
    );
}

/// As [`rejected`], and by **(V)** rather than by (T).
///
/// Which gate fires is the whole claim of a (V)-only fixture: (T) runs first, so a fixture that accidentally put a partial definition in a type position would pass `rejected` while proving nothing about proof positions.
pub(super) fn rejected_as_a_proof(source: &str) {
    let (system, _io) = MockHost::builder().build();
    let error = run_text(source, system).expect_err("expected the proof position to be rejected");
    assert!(
        error.contains("is a proof position"),
        "rejected, but not as a proof position:\n{error}",
    );
}

/// As [`rejected`], and by **(T)** rather than by (V).
///
/// The mirror of [`rejected_as_a_proof`]: a fixture that means to pin a type position proves nothing if a stray proof position is what actually fired.
pub(super) fn rejected_as_a_type(source: &str) {
    let (system, _io) = MockHost::builder().build();
    let error = run_text(source, system).expect_err("expected the type position to be rejected");
    assert!(
        error.contains("is a type position"),
        "rejected, but not as a type position:\n{error}",
    );
}

/// The negative functor every exploit below is built from. It is *accepted* — strict positivity asks whether a declaration reaches itself, and `Sink` never does. Only tying `A` back to `Sink(A)` is dangerous, and no `induct` can express that.
pub(super) const SINK: &str = r#"
    induct Sink(A : Type) : pub Type
    | sink(f : (A) -> /std/Bool/False)
    end
"#;

/// A *total* type-level function and a *partial* value of ordinary data type. `Shape` descends structurally on `F`; nothing here is a partial type former, which is why only the aggressive reading of (T) rejects the pair at all.
pub(super) const SHAPE: &str = r#"
    use /std/{Nat};

    induct F : pub Type
    | stop()
    | more(rest : F)
    end

    let Shape(f : F) -> Type =
        match f
        | stop() => Nat
        | more(rest) => Shape(rest)
        end;

    let inf : F = F/more(inf);
"#;

/// The productive twin of [`SHAPE`], whose `Shape(inf)` unfolds to `Sink(Shape(inf))` forever rather than to itself.
pub(super) const PRODUCTIVE_SHAPE: &str = r#"
    induct Sink(A : Type) : pub Type
    | sink(f : (A) -> /std/Bool/False)
    end

    induct F : pub Type
    | stop()
    | more(rest : F)
    end

    let Shape(f : F) -> Type =
        match f
        | stop() => /std/Bool/False
        | more(rest) => Sink(Shape(rest))
        end;

    let inf : F = F/more(inf);
"#;
