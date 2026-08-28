//! The refusal fixtures more than one theme needs.

use {crate::tests::run_text, curios_runtime::MockHost};

pub(super) fn rejected(source: &str) {
    let (system, _io) = MockHost::builder().build();
    assert!(
        run_text(source, system).is_err(),
        "expected the declaration to be rejected",
    );
}

/// [`rejected`], naming the rule that must do the rejecting.
///
/// A bare `is_err` passes on a typo in the fixture, which is worth guarding wherever the shape under test is one a future relaxation of `occurrences` could plausibly start admitting.
pub(super) fn rejected_by(source: &str, diagnostic: &str) {
    let (system, _io) = MockHost::builder().build();
    let error = run_text(source, system).expect_err("expected the declaration to be rejected");
    assert!(
        error.contains(diagnostic),
        "rejected, but not by '{diagnostic}':\n{error}",
    );
}
