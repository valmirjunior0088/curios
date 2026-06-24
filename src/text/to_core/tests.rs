use {
    crate::{core, text},
    std::{
        fs,
        path::{Path, PathBuf},
        time::{SystemTime, UNIX_EPOCH},
    },
};

fn run(src: &str) -> core::Term {
    let (module, _) =
        super::to_core(&src.parse::<text::Entrypoint>().unwrap(), &text::NullLoader).unwrap();

    module.into_nested_term()
}

fn run_err(src: &str) -> String {
    super::to_core(&src.parse::<text::Entrypoint>().unwrap(), &text::NullLoader)
        .unwrap_err()
        .to_string()
}

// Lower against the real prelude (so `sys` and `std` are served and rooted),
// returning only success/error — the lens for the internal-root gate.
fn lower_with_prelude(src: &str) -> Result<(), String> {
    super::to_core(
        &src.parse::<text::Entrypoint>().unwrap(),
        &text::prelude(&text::NullLoader),
    )
    .map(|_| ())
    .map_err(|error| error.to_string())
}

fn temp_dir(name: &str) -> PathBuf {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();
    std::env::temp_dir().join(format!("curios-{name}-{}-{millis}", std::process::id()))
}

fn write_module(base: &Path, path: &str, source: &str) {
    let path = base.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, source).unwrap();
}

#[test]
fn no_items_simple_tail() {
    assert_eq!(run("Type"), core::Term::type_());
}

#[test]
fn single_let_binding() {
    assert_eq!(
        run(r#"
            let x : Type = Type;
            x
        "#),
        core::Term::let_(
            "/x",
            core::Term::type_(),
            core::Term::type_(),
            core::Term::var(core::Var::free("/x"))
        ),
    );
}

#[test]
fn nested_module_binding_reference() {
    assert_eq!(
        run(r#"
            mod Foo
                pub let f : Type = Type;
            end
            Foo/f
        "#),
        core::Term::let_(
            "/Foo/f",
            core::Term::type_(),
            core::Term::type_(),
            core::Term::var(core::Var::free("/Foo/f"))
        ),
    );
}

#[test]
fn module_named_after_type_resolves_by_qualified_path() {
    assert_eq!(
        run(r#"
            mod Nat
                pub let double : Type = Type;
            end
            Nat/double
        "#),
        core::Term::let_(
            "/Nat/double",
            core::Term::type_(),
            core::Term::type_(),
            core::Term::var(core::Var::free("/Nat/double"))
        ),
    );
}

#[test]
fn use_shorthand_resolves_qualifier() {
    assert_eq!(
        run(r#"
            mod Foo
                pub mod Bar
                    pub let f : Type = Type;
                end
            end
            use Foo/{Bar};
            Bar/f
        "#),
        core::Term::let_(
            "/Foo/Bar/f",
            core::Term::type_(),
            core::Term::type_(),
            core::Term::var(core::Var::free("/Foo/Bar/f"))
        ),
    );
}

#[test]
fn allows_pub_on_root_items() {
    run(r#"
        pub mod Foo
            pub let f : Type = Type;
        end
        pub let g : Type = Type;
        Type
    "#);
}

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
        core::Term::let_(
            "/Foo/Bar/f",
            core::Term::type_(),
            core::Term::type_(),
            core::Term::var(core::Var::free("/Foo/Bar/f"))
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
        core::Term::let_(
            "/Foo/Bar/f",
            core::Term::type_(),
            core::Term::type_(),
            core::Term::var(core::Var::free("/Foo/Bar/f"))
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
        core::Term::let_(
            "/A/X/f",
            core::Term::type_(),
            core::Term::type_(),
            core::Term::var(core::Var::free("/A/X/f"))
        ),
    );
}

// --- Module-interface redesign (SPEC.md) acceptance cases ---

// A re-exports x from B; B re-exports x from C; C declares x. A is declared
// before its providers. The phase-3 fixed point must resolve A/x to /C/x
// regardless of declaration order.
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
        core::Term::let_(
            "/C/x",
            core::Term::type_(),
            core::Term::type_(),
            core::Term::var(core::Var::free("/C/x"))
        ),
    );
}

// A direct `pub let x` and a `pub use` that also yields x are two distinct
// sources for the same export slot: a conflict, even though one of them is the
// module's own declaration.
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

// A re-exports module M from B; a later path /A/M/x must traverse A's
// re-exported M into /B/M and resolve x there.
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
        core::Term::let_(
            "/B/M/x",
            core::Term::type_(),
            core::Term::type_(),
            core::Term::var(core::Var::free("/B/M/x"))
        ),
    );
}

// A module may re-export names out of its own private child via a relative path:
// being inside the module, its privacy does not apply to itself. This is the
// facade-over-private-impl pattern.
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
        core::Term::let_(
            "/Facade/Impl/helper",
            core::Term::type_(),
            core::Term::type_(),
            core::Term::var(core::Var::free("/Facade/Impl/helper"))
        ),
    );
}

// The relaxation is scoped to a module's *own* child: re-exporting through
// another module's private child is still forbidden.
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

// An inductive's constructor module is a first-class interface member built in phase
// 2, so its cases re-export by name and by glob through the fixed point.
#[test]
fn re_exports_inductive_constructor_by_name() {
    let term = run(r#"
        pub mod Foo
            pub induct U
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

    assert!(format!("{term:?}").contains("Foo/U/A"));
}

#[test]
fn re_exports_inductive_constructors_by_glob() {
    let term = run(r#"
        pub mod Foo
            pub induct U
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

    assert!(format!("{term:?}").contains("Foo/U/A"));
}

// A re-exports x from B; B re-exports x from A; nobody declares x. Following the
// chain returns to the start without a concrete target → cyclic, not missing.
#[test]
fn rejects_cyclic_re_export_with_no_concrete_target() {
    assert!(
        run_err(
            r#"
        pub mod A
            pub use /B/{x};
        end
        pub mod B
            pub use /A/{x};
        end
        Type
    "#
        )
        .contains("cyclic re-export")
    );
}

// Two public declarations of the same label in the same namespace conflict at
// phase 2, before any elaboration.
#[test]
fn rejects_duplicate_public_declaration() {
    assert!(
        run_err(
            r#"
        pub let x : Type = Type;
        pub let x : Type = Type;
        Type
    "#
        )
        .contains("duplicate public declaration")
    );
}

// Phase 5: A.f references B.g and B.h references A.e, with e and g independent —
// no cycle, but no contiguous source order binds both references. The reorder
// must produce a valid binding order, leaving the lowered term with no free name.
#[test]
fn orders_acyclic_bidirectional_value_graph() {
    assert!(
        run(r#"
            pub mod A
                pub let e : Type = Type;
                pub let f : Type = /B/g;
            end
            pub mod B
                pub let g : Type = Type;
                pub let h : Type = /A/e;
            end
            Type
        "#)
        .free_vars()
        .is_empty()
    );
}

// A dependency through a type annotation is as much a binding-order constraint as
// one through a value: `f : T` declared before `T` must still order `T` first.
#[test]
fn orders_dependency_through_type_annotation() {
    assert!(
        run(r#"
            let f : T = Type;
            let T : Type = Type;
            f
        "#)
        .free_vars()
        .is_empty()
    );
}

// A genuine non-atomic value cycle cannot be ordered; phase 5 emits it anyway and
// leaves one reference as a free name, which core rejects as unbound. There is
// nothing to repair — cross-declaration value recursion is unexpressible.
#[test]
fn genuine_value_cycle_leaves_unbound_name() {
    assert!(
        !run(r#"
            pub mod A
                pub let f : Type = /B/g;
            end
            pub mod B
                pub let g : Type = /A/f;
            end
            Type
        "#)
        .free_vars()
        .is_empty()
    );
}

// `sys` is the trusted primitive substrate, reachable only from the standard
// library. A user entrypoint that names it — through a `use` or a bare term
// reference — is rejected at resolution; the `/std` wrappers are the door.
#[test]
fn rejects_sys_use_from_user_code() {
    let error = lower_with_prelude("use /sys/{Nat}; Nat/add(1, 2)").unwrap_err();
    assert!(
        error.contains("internal to the standard library"),
        "unexpected error: {error}"
    );
}

#[test]
fn rejects_sys_reference_in_term_from_user_code() {
    let error = lower_with_prelude("/sys/Nat/add(1, 2)").unwrap_err();
    assert!(
        error.contains("internal to the standard library"),
        "unexpected error: {error}"
    );
}

// The guard rides the *resolved* qualifier, not the spelling, so a relative
// reference is rejected exactly as the absolute one is — the leading `/` is not
// the boundary.
#[test]
fn rejects_relative_sys_reference_in_term() {
    let error = lower_with_prelude("sys/Nat/add(1, 2)").unwrap_err();
    assert!(
        error.contains("internal to the standard library"),
        "unexpected error: {error}"
    );
}

#[test]
fn rejects_relative_sys_use() {
    let error = lower_with_prelude("use sys/{Nat}; Nat/add(1, 2)").unwrap_err();
    assert!(
        error.contains("internal to the standard library"),
        "unexpected error: {error}"
    );
}

#[test]
fn rejects_relative_sys_glob() {
    let error = lower_with_prelude("use sys/*; Nat/add(1, 2)").unwrap_err();
    assert!(
        error.contains("internal to the standard library"),
        "unexpected error: {error}"
    );
}

// The interface (`pub use`) phase guards too: a user module cannot launder `sys`
// into its own public surface.
#[test]
fn rejects_sys_pub_use_reexport_from_user_code() {
    let error = lower_with_prelude("pub mod Foo\n    pub use /sys/{Nat};\nend\nType")
        .unwrap_err();
    assert!(
        error.contains("internal to the standard library"),
        "unexpected error: {error}"
    );
}

// The same primitive reached through its `/std` wrapper resolves: `std` is
// privileged to reference `sys`, and re-exports it.
#[test]
fn allows_sys_reference_through_std_wrapper() {
    assert!(lower_with_prelude("use /std/{Nat}; Nat/add(1, 2)").is_ok());
}

#[test]
fn rejects_private_root_module_via_absolute_path() {
    assert!(
        run_err(
            r#"
        mod Foo
            pub let f : Type = Type;
        end
        pub mod Bar
            use /{Foo};
        end
        Type
    "#
        )
        .contains("private child module")
    );
}

#[test]
fn allows_pub_root_module_via_absolute_path() {
    run(r#"
        pub mod Foo
            pub let f : Type = Type;
        end
        pub mod Bar
            use /{Foo};
        end
        Type
    "#);
}

#[test]
fn private_use_does_not_expose_qualifier() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            pub mod Bar
                pub let f : Type = Type;
            end
        end
        pub mod MyMod
            use /Foo/{Bar};
        end
        MyMod/Bar/f
    "#
        )
        .contains("child module not found")
    );
}

#[test]
fn use_imports_binding_by_path() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        use /Foo/{x};
        x
    "#);
}

#[test]
fn rejects_use_of_private_binding() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            let x : Type = Type;
        end
        use /Foo/{x};
        x
    "#
        )
        .contains("private binding: x")
    );
}

#[test]
fn pub_use_re_exports_binding() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        pub mod Bar
            pub use /Foo/{x};
        end
        use /Bar/{x};
        x
    "#);
}

#[test]
fn pub_use_binding_aliases_to_canonical_path() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        pub mod Bar
            pub use /Foo/{x};
        end
        Bar/x
    "#);
}

#[test]
fn rejects_use_followed_by_local_let_of_same_name() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        use /Foo/{x};
        let x : Type = Type;
        x
    "#
        )
        .contains("binding conflicts with existing scope entry: x")
    );
}

#[test]
fn rejects_two_imports_of_same_name() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        pub mod Bar
            pub let x : Type = Type;
        end
        use /Foo/{x};
        use /Bar/{x};
        x
    "#
        )
        .contains("binding conflicts with existing scope entry: x")
    );
}

#[test]
fn relative_use_imports_binding() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        pub mod Bar
            use /{Foo};
            use Foo/{x};
            pub let y : Type = x;
        end
        Bar/y
    "#);
}

#[test]
fn rejects_use_of_unknown_item() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        use /Foo/{nope};
        Type
    "#
        )
        .contains("no module or binding named nope")
    );
}

#[test]
fn use_of_dual_existence_registers_both() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let y : Type = Type;
            end
            pub use X/{y};
        end
        pub mod Bar
            use /Foo/{y};
            pub let direct : Type = y;
            pub let via_path : Type = y;
        end
        Type
    "#);
}

#[test]
fn dual_use_lets_bare_name_resolve_to_binding() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let y : Type = Type;
            end
            pub use X/{y};
        end
        use /Foo/{y};
        y
    "#);
}

#[test]
fn dual_use_lets_path_resolve_through_module() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let y : Type = Type;
                pub let q : Type = Type;
            end
            pub use X/{y};
        end
        Foo/y
    "#);
}

#[test]
fn public_child_with_private_binding_imports_only_module() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let z : Type = Type;
            end
            use X/{z};
            let X : Type = z;
        end
        use /Foo/{X};
        X/z
    "#);
}

#[test]
fn private_child_with_public_binding_imports_only_binding() {
    run(r#"
        pub mod Foo
            mod X
                pub let z : Type = Type;
            end
            use X/{z};
            pub let X : Type = z;
        end
        use /Foo/{X};
        X
    "#);
}

#[test]
fn use_of_dual_existence_from_outside_module() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let X : Type = Type;
            end
            pub use X/{X};
        end
        use /Foo/{X};
        X
    "#);
}

#[test]
fn use_of_dual_existence_from_outside_qualifier_path() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let X : Type = Type;
                pub let q : Type = Type;
            end
            pub use X/{X};
        end
        use /Foo/{X};
        X/q
    "#);
}

#[test]
fn use_brace_group_imports_all_labels() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
            pub let y : Type = Type;
        end
        use /Foo/{x, y};
        x
    "#);
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
            pub let y : Type = Type;
        end
        use /Foo/{x, y};
        y
    "#);
}

#[test]
fn rejects_use_when_both_sides_private() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            mod X
                pub let z : Type = Type;
            end
            use X/{z};
            let X : Type = z;
        end
        use /Foo/{X};
        Type
    "#
        )
        .contains("private child module: X")
    );
}

#[test]
fn use_glob_imports_all_public_bindings() {
    assert_eq!(
        run(r#"
            pub mod Foo
                pub let x : Type = Type;
                pub let y : Type = Type;
            end
            use /Foo/*;
            x
        "#),
        core::Term::let_(
            "/Foo/x",
            core::Term::type_(),
            core::Term::type_(),
            core::Term::let_(
                "/Foo/y",
                core::Term::type_(),
                core::Term::type_(),
                core::Term::var(core::Var::free("/Foo/x"))
            )
        ),
    );
}

#[test]
fn use_glob_imports_child_modules_as_qualifiers() {
    run(r#"
        pub mod Foo
            pub mod Bar
                pub let f : Type = Type;
            end
        end
        use /Foo/*;
        Bar/f
    "#);
}

#[test]
fn use_glob_skips_private_child_modules() {
    assert!(
        run_err(
            r#"
        pub mod Foo
            mod Bar
                pub let f : Type = Type;
            end
        end
        use /Foo/*;
        Bar/f
    "#
        )
        .contains("unresolved qualifier")
    );
}

#[test]
fn relative_use_glob_imports_from_qualifier() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
        end
        use Foo/*;
        x
    "#);
}

#[test]
fn pub_use_glob_re_exports_all_public_labels() {
    run(r#"
        pub mod Foo
            pub let x : Type = Type;
            pub mod Bar
                pub let f : Type = Type;
            end
        end
        pub mod Mine
            pub use /Foo/*;
        end
        use /Mine/{Bar};
        use /Mine/{x};
        Bar/f
    "#);
}

#[test]
fn use_glob_on_dual_existence_imports_once() {
    run(r#"
        pub mod Foo
            pub mod X
                pub let X : Type = Type;
                pub let q : Type = Type;
            end
            pub use X/{X};
        end
        use /Foo/*;
        X/q
    "#);
}

#[test]
fn file_loader_prepares_sibling_modules_before_to_core() {
    let base = temp_dir("sibling-order");
    write_module(
        &base,
        "A.crs",
        r#"
            use /B/{x};
            pub let y : Type = x;
        "#,
    );
    write_module(&base, "B.crs", "pub let x : Type = Type;");

    let entrypoint = r#"
            pub mod A;
            pub mod B;
            A/y
        "#
    .parse::<text::Entrypoint>()
    .unwrap();
    let loader = text::FileLoader::new(&base);

    super::to_core(&entrypoint, &loader).unwrap();

    fs::remove_dir_all(base).unwrap();
}

#[test]
fn file_backed_module_missing_from_loader_is_module_not_found() {
    let entrypoint = r#"
            pub mod A;
            Type
        "#
    .parse::<text::Entrypoint>()
    .unwrap();

    assert!(matches!(
        super::to_core(&entrypoint, &text::NullLoader).unwrap_err(),
        text::Error::Located { error, .. }
            if matches!(error.as_ref(), text::Error::ModuleNotFound { path } if path == "/A")
    ));
}

#[test]
fn hole_lowers_to_metavar() {
    assert_eq!(run("?"), core::Term::metavar(0));
}

#[test]
fn distinct_holes_get_distinct_ids() {
    // Two holes in one program draw distinct, monotonic ids from the shared counter.
    let term = run("(?, ?)");
    assert_eq!(
        term,
        core::Term::tuple([core::Term::metavar(0), core::Term::metavar(1)]),
    );
}

#[test]
fn bang_outside_let_bang_is_rejected() {
    // A postfix `!` with no enclosing `let !` has no bind to sequence it, so the
    // desugarer rejects it rather than emitting a dangling continuation.
    assert!(run_err("x!").contains("outside a `let !` block"));
}
