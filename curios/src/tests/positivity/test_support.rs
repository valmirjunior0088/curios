//! The refusal fixtures more than one theme needs.

use {crate::tests::run_text, curios_runtime::MockHost};

/// The declaration in `source` is refused, and by the rule `diagnostic` names.
///
/// A bare `is_err` passes on a typo in the fixture, and a refusal by the wrong rule passes too, so every row says which phrase must do the rejecting.
pub(super) fn rejected_by(source: &str, diagnostic: &str) {
    let (system, _io) = MockHost::builder().build();
    let error = run_text(source, system).expect_err("expected the declaration to be rejected");
    assert!(
        error.contains(diagnostic),
        "rejected, but not by '{diagnostic}':\n{error}",
    );
}
