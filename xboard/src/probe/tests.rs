//! What a probe says of a program or a module written as Core text, through the real checkers.

use super::*;

#[test]
fn a_program_whose_tail_proves_something_else_is_refused_and_says_by_whom() {
    let probe = Probe::put(Proof::Program("/std/Bool/True/qed()"));
    let said = probe.to_string();

    assert!(!probe.admitted());
    assert_eq!(probe.said(Checker::Elaborator), "refused");
    assert!(
        said.starts_with("REFUSED    by elaborator `TypeMismatch` at the closing term\n"),
        "{said}"
    );
}

#[test]
fn a_program_that_does_not_read_is_refused_with_the_kernel_not_asked() {
    let probe = Probe::put(Proof::Program("let"));
    let said = probe.to_string();

    assert!(!probe.admitted());
    assert_eq!(probe.said(Checker::Kernel), "not asked");
    assert!(said.starts_with("REFUSED    by text\n"), "{said}");
}

#[test]
fn a_module_is_put_to_the_reader_and_the_kernel_and_says_by_whom() {
    let probe = Probe::put(Proof::Module("0"));
    let said = probe.to_string();

    assert!(!probe.admitted());
    assert_eq!(probe.said(Checker::Reader), "admitted");
    assert!(
        said.starts_with("REFUSED    by kernel `Mismatch` at the closing term\n           reader      admitted\n           kernel      refused\n"),
        "{said}"
    );
}

#[test]
fn a_module_that_does_not_read_is_refused_by_the_reader_with_the_kernel_not_asked() {
    let probe = Probe::put(Proof::Module("let"));

    assert_eq!(probe.said(Checker::Reader), "refused");
    assert_eq!(probe.said(Checker::Kernel), "not asked");
    assert!(probe.to_string().starts_with("REFUSED    by reader\n"));
}
