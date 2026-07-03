use curios_abi::sys_io;

/// The roster the harness builds its `env` object from is the store, name for
/// name, in store order. (`abi()` itself is JS-only — the object assembly
/// can't run on the host — so the roster is pinned here instead.)
#[test]
fn import_names_are_the_store_rows() {
    let names = super::import_names();

    assert_eq!(
        names,
        sys_io()
            .iter()
            .map(|function| function.name.clone())
            .collect::<Vec<_>>()
    );
    assert_eq!(names.first().map(String::as_str), Some("io_read"));
    assert_eq!(names.last().map(String::as_str), Some("io_env"));
}
