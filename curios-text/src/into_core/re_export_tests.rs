//! `pub use` and the facades it builds: transitive chains, globs, and the constructor namespaces that may not be re-exported.

use super::test_support::*;

#[test]
fn rejects_unresolved_qualifier_in_term() {
    assert!(run_err("Foo/f").contains("unresolved qualifier"));
}

#[test]
fn rejects_private_binding_access() {
    assert!(
        run_err(
            r#"
        mod Foo
            let f : Type = Type;
        end
        Foo/f
    "#
        )
        .contains("private binding")
    );
}

#[test]
fn rejects_private_module_in_path() {
    assert!(
        run_err(
            r#"
        mod Foo
            mod Bar
                pub let f : Type = Type;
            end
        end
        Foo/Bar/f
    "#
        )
        .contains("private child module")
    );
}

#[test]
fn rejects_conflicting_use_qualifiers() {
    assert!(
        run_err(
            r#"
        mod Foo
            pub mod Baz
                pub let f : Type = Type;
            end
        end
        mod Bar
            pub mod Baz
                pub let g : Type = Type;
            end
        end
        use Foo/{Baz};
        use Bar/{Baz};
        Type
    "#
        )
        .contains("qualifier conflicts with existing scope entry")
    );
}

#[test]
fn rejects_use_of_nonexistent_child() {
    assert!(
        run_err(
            r#"
        mod Foo
        end
        use Foo/{Nonexistent};
        Type
    "#
        )
        .contains("no module or binding named Nonexistent")
    );
}

#[test]
fn rejects_absolute_use_of_nonexistent_module() {
    assert!(
        run_err(
            r#"
        use /{Nonexistent};
        Type
    "#
        )
        .contains("no module or binding named Nonexistent")
    );
}

#[test]
fn pub_use_exposes_qualifier() {
    assert_eq!(
        run(r#"
            pub mod Foo
                pub mod Bar
                    pub let f : Type = Type;
                end
            end
            pub mod MyMod
                pub use /Foo/{Bar};
            end
            MyMod/Bar/f
        "#),
        curios_core::Term::let_(
            &global("/Foo/Bar/f"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/Foo/Bar/f")))
        ),
    );
}

#[test]
fn rejects_mod_that_overwrites_prior_use() {
    assert!(
        run_err(
            r#"
        mod Foo
            pub mod Bar
                pub let f : Type = Type;
            end
        end
        use Foo/{Bar};
        mod Bar
        end
        Type
    "#
        )
        .contains("qualifier conflicts with existing scope entry: Bar")
    );
}

#[test]
fn use_of_pub_use_path_resolves_through_alias() {
    assert_eq!(
        run(r#"
            pub mod Foo
                pub mod Bar
                    pub let f : Type = Type;
                end
            end
            pub mod MyMod
                pub use /Foo/{Bar};
            end
            use /MyMod/{Bar};
            Bar/f
        "#),
        curios_core::Term::let_(
            &global("/Foo/Bar/f"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/Foo/Bar/f")))
        ),
    );
}

#[test]
fn chained_pub_use_re_exports_transitively() {
    assert_eq!(
        run(r#"
            pub mod A
                pub mod X
                    pub let f : Type = Type;
                end
            end
            pub mod B
                pub use /A/{X};
            end
            pub mod C
                pub use /B/{X};
            end
            C/X/f
        "#),
        curios_core::Term::let_(
            &global("/A/X/f"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/A/X/f")))
        ),
    );
}

// --- Module-interface redesign acceptance cases ---

// A re-exports x from B; B re-exports x from C; C declares x. A is declared before its providers. The phase-3 fixed point must resolve A/x to /C/x regardless of declaration order.
#[test]
fn chained_re_export_resolves_out_of_order() {
    assert_eq!(
        run(r#"
            pub mod A
                pub use /B/{x};
            end
            pub mod B
                pub use /C/{x};
            end
            pub mod C
                pub let x : Type = Type;
            end
            A/x
        "#),
        curios_core::Term::let_(
            &global("/C/x"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/C/x")))
        ),
    );
}

// A direct `pub let x` and a `pub use` that also yields x are two distinct sources for the same export slot: a conflict, even though one of them is the module's own declaration.
#[test]
fn rejects_direct_and_re_export_of_same_label() {
    assert!(
        run_err(
            r#"
        pub mod B
            pub let x : Type = Type;
        end
        pub mod A
            pub let x : Type = Type;
            pub use /B/{x};
        end
        Type
    "#
        )
        .contains("export conflict")
    );
}

// Two globs each exposing x are two sources for the same slot → conflict.
#[test]
fn rejects_two_globs_exposing_same_label() {
    assert!(
        run_err(
            r#"
        pub mod B
            pub let x : Type = Type;
        end
        pub mod C
            pub let x : Type = Type;
        end
        pub mod A
            pub use /B/*;
            pub use /C/*;
        end
        Type
    "#
        )
        .contains("export conflict")
    );
}

// A re-exports module M from B; a later path /A/M/x must traverse A's re-exported M into /B/M and resolve x there.
#[test]
fn deep_facade_traversal_through_re_exported_module() {
    assert_eq!(
        run(r#"
            pub mod B
                pub mod M
                    pub let x : Type = Type;
                end
            end
            pub mod A
                pub use /B/{M};
            end
            use /A/M/{x};
            x
        "#),
        curios_core::Term::let_(
            &global("/B/M/x"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/B/M/x")))
        ),
    );
}

// A module may re-export names out of its own private child via a relative path: being inside the module, its privacy does not apply to itself. This is the facade-over-private-impl pattern.
#[test]
fn re_exports_from_own_private_child() {
    assert_eq!(
        run(r#"
            pub mod Facade
                mod Impl
                    pub let helper : Type = Type;
                end
                pub use Impl/{helper};
            end
            use /Facade/{helper};
            helper
        "#),
        curios_core::Term::let_(
            &global("/Facade/Impl/helper"),
            written_type(0),
            written_type(1),
            curios_core::Term::var(curios_core::Var::free(global("/Facade/Impl/helper")))
        ),
    );
}

// The relaxation is scoped to a module's *own* child: re-exporting through another module's private child is still forbidden.
#[test]
fn rejects_re_export_through_other_modules_private_child() {
    assert!(
        run_err(
            r#"
        pub mod Other
            mod Impl
                pub let helper : Type = Type;
            end
        end
        pub mod Facade
            pub use /Other/Impl/{helper};
        end
        Type
    "#
        )
        .contains("private child module")
    );
}

// An inductive's constructor module is a first-class interface member built in phase 2, so its cases re-export by name and by glob through the fixed point.
#[test]
fn re_exports_inductive_constructor_by_name() {
    let term = run(r#"
        pub mod Foo
             pub induct U : pub Type
            | A()
            | B()
            end
        end
        pub mod Bar
            pub use /Foo/U/{A};
        end
        use /Bar/{A};
        A
    "#);

    // Asserted on the printed term, not on `Debug`: a qualifier's `Debug` shows its segments, deliberately, so that `/Foo/bar` and a single segment spelled `Foo/bar` never look alike in a dump.
    assert!(
        format!("{term}").contains("Foo/U/A"),
        "unexpected term: {term}"
    );
}

#[test]
fn re_exports_inductive_constructors_by_glob() {
    let term = run(r#"
        pub mod Foo
             pub induct U : pub Type
            | A()
            | B()
            end
        end
        pub mod Bar
            pub use /Foo/U/*;
        end
        use /Bar/{A};
        A
    "#);

    // Asserted on the printed term, not on `Debug`: a qualifier's `Debug` shows its segments, deliberately, so that `/Foo/bar` and a single segment spelled `Foo/bar` never look alike in a dump.
    assert!(
        format!("{term}").contains("Foo/U/A"),
        "unexpected term: {term}"
    );
}

#[test]
fn opaque_inductive_constructor_namespace_cannot_be_re_exported() {
    let error = run_err(
        r#"
        pub mod Foo
            pub induct U : Type
            | A()
            end
        end
        pub mod Bar
            pub use /Foo/U/*;
        end
        Type
    "#,
    );

    assert!(
        error.contains("private child module"),
        "unexpected error: {error}"
    );
}

#[test]
fn rejects_named_re_export_from_own_opaque_constructor_namespace() {
    let error = run_err(
        r#"
        pub mod Foo
            pub induct U : Type
            | A()
            end
            pub use U/{A};
        end
        Type
    "#,
    );

    assert!(
        error.contains("constructors of opaque inductive '/Foo/U' cannot be re-exported")
            && error.contains("mark its representation public"),
        "unexpected error: {error}"
    );
}

#[test]
fn rejects_glob_re_export_from_own_opaque_constructor_namespace() {
    let error = run_err(
        r#"
        pub mod Foo
            pub induct U : Type
            | A()
            end
            pub use U/*;
        end
        Type
    "#,
    );

    assert!(
        error.contains("constructors of opaque inductive '/Foo/U' cannot be re-exported")
            && error.contains("mark its representation public"),
        "unexpected error: {error}"
    );
}

#[test]
fn declaring_module_can_use_own_opaque_constructors_lexically() {
    run(r#"
        pub mod Foo
            pub induct U : Type
            | A()
            end
            use U/{A};
            pub let make : U = A();
        end
        Type
    "#);
}

#[test]
fn private_inductive_with_public_representation_can_export_constructor_facade() {
    let term = run(r#"
        pub mod Foo
            induct U : pub Type
            | A()
            end
            pub use U/{A};
        end
        use /Foo/{A};
        A
    "#);

    // Asserted on the printed term, not on `Debug`: a qualifier's `Debug` shows its segments, deliberately, so that `/Foo/bar` and a single segment spelled `Foo/bar` never look alike in a dump.
    assert!(
        format!("{term}").contains("Foo/U/A"),
        "unexpected term: {term}"
    );
}
