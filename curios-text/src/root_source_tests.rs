use super::*;

// A stand-in dependency module for tests that only exercise `dependencies`'
// name validation/roster reporting — its content never matters, only the
// name it's registered under.
fn empty_module() -> Module {
    "".parse::<Module>().expect("an empty module parses")
}

#[test]
fn dependencies_rejects_a_duplicate_name() {
    let error = RootSource::dependencies(
        vec![
            ("foo".to_string(), empty_module(), RootSource::None),
            ("foo".to_string(), empty_module(), RootSource::None),
        ],
        RootSource::None,
    )
    .err()
    .expect("expected a duplicate-name rejection");

    assert!(matches!(error, Error::DuplicateRoot { name } if name == "foo"));
}

#[test]
fn dependencies_rejects_a_name_reserved_by_an_embedded_root() {
    let error = RootSource::dependencies(
        vec![("std".to_string(), empty_module(), RootSource::None)],
        RootSource::None,
    )
    .err()
    .expect("expected a reserved-name rejection");

    assert!(matches!(error, Error::DuplicateRoot { name } if name == "std"));
}

#[test]
fn dependencies_reports_their_names_via_roots() {
    let source = RootSource::dependencies(
        vec![
            ("foo".to_string(), empty_module(), RootSource::None),
            ("bar".to_string(), empty_module(), RootSource::None),
        ],
        RootSource::None,
    )
    .expect("a well-formed dependency list");

    assert_eq!(source.roots(), vec!["bar", "foo"]);
}

#[test]
fn a_dependencys_own_root_module_loads_directly() {
    let module = "pub let answer : /std/Nat = 42;"
        .parse::<Module>()
        .expect("dependency module parses");
    let source = RootSource::dependencies(
        vec![("foo".to_string(), module, RootSource::None)],
        RootSource::None,
    )
    .expect("a well-formed dependency list");

    let loaded = source
        .load(&Qualifier::from(["foo".to_string()]))
        .expect("foo's own root module loads");

    assert_eq!(loaded.items.len(), 1);
}

#[test]
fn an_unregistered_qualifier_falls_through_to_base() {
    let source = RootSource::dependencies(
        vec![("foo".to_string(), empty_module(), RootSource::None)],
        RootSource::None,
    )
    .expect("a well-formed dependency list");

    let error = source
        .load(&Qualifier::from(["bar".to_string()]))
        .err()
        .expect("bar is not a registered dependency");

    assert!(matches!(error, Error::ModuleNotFound { .. }));
}
