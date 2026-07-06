use {
    curios_rt::{ForeignBindings, MockHost},
    std::{fs, time::Duration},
};

// A named path dependency's own root module, registered under `name` with no
// nested `mod` children of its own — enough for these tests, which never
// declare a `mod` inside the dependency.
fn dependency(name: &str, source: &str) -> curios_text::RootSource {
    let module = source
        .parse::<curios_text::Module>()
        .expect("dependency module parses");

    curios_text::RootSource::dependencies(
        vec![(name.to_string(), module, curios_text::RootSource::None)],
        curios_text::RootSource::None,
    )
    .expect("a well-formed dependency list")
}

// The acceptance scenario's core claim: `foo`'s definitions are reachable
// under `/foo/...` with no `mod foo;` written anywhere in the entry program —
// the manifest (once one exists) is the only place `foo` is even named.
#[test]
fn dependency_is_implicitly_visible_with_no_mod_declaration() {
    let source = r#"
        use /std/{Nat, Io};
        let n : Nat = /foo/answer;
        Io/print(Nat/to_str(n))
        "#
    .parse::<curios_text::Entrypoint>()
    .expect("failed to parse source");

    let loader = dependency("foo", "pub let answer : /std/Nat = 42;");

    let (system, io) = MockHost::builder().build();
    crate::run_entrypoint(Duration::from_secs(10), &source, loader, system)
        .expect("expected result");

    assert_eq!(io.output(), b"42");
}

// A `foreign` declaration inside a dependency registers into the same shared
// `ForeignStore` a `foreign` at the entry program's own root would — the
// store is keyed by declaration, not by which root declared it.
#[test]
fn foreign_declaration_inside_a_dependency_populates_the_shared_store() {
    let source = r#"
        let _ : /std/False = /std/Proc/exit(/foo/double(21));
        /std/Io/write(/std/Io/stdout, /std/Str/to_bin("unreachable"))
        "#
    .parse::<curios_text::Entrypoint>()
    .expect("failed to parse source");

    let loader = dependency("foo", "pub foreign double : (Nat) -> Nat;");

    let (module, foreigns) = curios_pipeline::compile_entrypoint(Duration::from_secs(10), &source, loader, |_| {})
        .expect("compile succeeded");

    let mut bindings = ForeignBindings::new(foreigns);
    bindings.define("double", |x: u32| x * 2);

    let (system, io) = MockHost::builder().build();
    let code = crate::run_wasm(&module, system, bindings).expect("execution succeeded");

    assert_eq!(code, 42);
    assert!(io.output().is_empty());
}

// The orphan rule, generalized past the entry-vs-stdlib case
// (`tests::concepts::orphan_witness_is_rejected`): a dependency is just
// another `Ordinary` root, so a `satisfy` declared inside it for a
// concept/type it owns neither of — both are owned by the entry program here
// — is rejected exactly the same way.
#[test]
fn orphan_rule_rejects_a_witness_in_a_dependency_for_the_entrys_own_types() {
    let source = r#"
        use /std/{Nat};
        pub concept Greet(A : Type) : Type {
            greet(A) -> Nat
        }
        pub record Local : Type { n : Nat }
        let z : Nat = 0;
        z
        "#
    .parse::<curios_text::Entrypoint>()
    .expect("failed to parse source");

    let loader = dependency(
        "foo",
        r#"
        satisfy /Greet(/Local) {
            greet(l) = l.n
        }
        "#,
    );

    let (system, _io) = MockHost::builder().build();
    let error = crate::run_entrypoint(Duration::from_secs(10), &source, loader, system)
        .err()
        .expect("expected an orphan-rule error");

    assert!(error.to_lowercase().contains("orphan"));
}

// The CLI-facing boundary end to end: two real `.crs` files on disk, wired
// together by `load_with_dependencies` with no `mod foo;` in the entry
// file — exercises `RootSource::dependency_from_path` (the entrypoint-like
// "point at any file" convention) rather than the in-memory `dependency`
// helper the tests above use.
#[test]
fn load_with_dependencies_wires_a_filesystem_dependency_end_to_end() {
    let dir = std::env::temp_dir();
    let entry_path = dir.join("curios_load_with_dependencies_e2e_entry.crs");
    let dep_path = dir.join("curios_load_with_dependencies_e2e_foo.crs");

    fs::write(
        &entry_path,
        r#"
        use /std/{Nat, Io};
        let n : Nat = /foo/answer;
        Io/print(Nat/to_str(n))
        "#,
    )
    .expect("write the temp entry file");
    fs::write(&dep_path, "pub let answer : /std/Nat = 42;").expect("write the temp dependency file");

    let (entrypoint, loader) =
        crate::load_with_dependencies(&entry_path, vec![("foo".to_string(), dep_path)])
            .expect("load_with_dependencies succeeds");

    let (system, io) = MockHost::builder().build();
    crate::run_entrypoint(Duration::from_secs(10), &entrypoint, loader, system)
        .expect("expected result");

    assert_eq!(io.output(), b"42");
}
