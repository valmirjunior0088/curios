//! What the guard promises: the directory goes away however the test ends.

use {super::*, std::panic};

/// The case the guard exists for, which no other test in this crate can show: a body that panics still leaves nothing behind, because unwinding drops the guard on the way out.
#[test]
fn a_failing_test_leaves_no_directory_behind() {
    let temporary = Temporary::new("guard", "failing");
    let path = temporary.to_path_buf();
    fs::create_dir_all(&path).unwrap();

    let unwound = panic::catch_unwind(move || {
        assert!(temporary.is_dir(), "the fixture was made");
        panic!("the assertion this test stands in for");
    });

    assert!(unwound.is_err(), "the body panics on purpose");
    assert!(!path.exists(), "and the directory went with it");
}

/// A guard's path is canonical from the start, so no fixture compares its own spelling of a directory against the one the code under test resolved — on any host, whatever sits behind its temporary directory.
#[test]
fn a_temporary_directory_is_spelled_canonically() {
    let temporary = Temporary::new("guard", "canonical");
    fs::create_dir_all(&temporary).unwrap();

    assert_eq!(temporary.canonicalize().unwrap(), temporary.to_path_buf());
}
