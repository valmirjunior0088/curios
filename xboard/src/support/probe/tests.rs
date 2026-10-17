//! What a probe says of a program, through the real checkers.

use super::*;

#[test]
fn a_program_whose_tail_proves_something_else_is_refused_and_says_by_whom() {
    let probe = Probe::put("/std/Bool/True/qed()");
    let said = probe.to_string();

    assert!(!probe.admitted());
    assert_eq!(probe.said(Checker::Elaborator), "refused");
    assert!(
        said.starts_with("REFUSED    by elaborator `TypeMismatch`\n"),
        "{said}"
    );
}

#[test]
fn a_program_that_does_not_read_is_refused_with_the_kernel_not_asked() {
    let probe = Probe::put("let");
    let said = probe.to_string();

    assert!(!probe.admitted());
    assert_eq!(probe.said(Checker::Kernel), "not asked");
    assert!(said.starts_with("REFUSED    by text\n"), "{said}");
}
