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
