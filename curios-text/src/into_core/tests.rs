use std::{
    fs,
    path::{Path, PathBuf},
    time::{SystemTime, UNIX_EPOCH},
};

fn run(src: &str) -> curios_core::Term {
    let (module, _, _) = super::into_core(
        &src.parse::<crate::Entrypoint>().unwrap(),
        &crate::RootSource::none(),
    )
    .unwrap();

    module.into_nested_term()
}

fn run_err(src: &str) -> String {
    super::into_core(
        &src.parse::<crate::Entrypoint>().unwrap(),
        &crate::RootSource::none(),
    )
    .unwrap_err()
    .to_string()
}

// Lower against the real prelude (so `sys` and `std` are served and rooted),
// returning only success/error — the lens for the internal-root gate.
fn lower_with_prelude(src: &str) -> Result<(), String> {
    super::into_core(
        &src.parse::<crate::Entrypoint>().unwrap(),
        &crate::prelude(&curios_abi::sys_io(), crate::RootSource::none()),
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
    assert_eq!(run("Type"), curios_core::Term::type_());
}

#[test]
fn single_let_binding() {
    assert_eq!(
        run(r#"
            let x : Type = Type;
            x
        "#),
        curios_core::Term::let_(
            "/x",
            curios_core::Term::type_(),
            curios_core::Term::type_(),
            curios_core::Term::var(curios_core::Var::free("/x"))
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
        curios_core::Term::let_(
            "/Foo/f",
            curios_core::Term::type_(),
            curios_core::Term::type_(),
            curios_core::Term::var(curios_core::Var::free("/Foo/f"))
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
        curios_core::Term::let_(
            "/Nat/double",
            curios_core::Term::type_(),
            curios_core::Term::type_(),
            curios_core::Term::var(curios_core::Var::free("/Nat/double"))
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
        curios_core::Term::let_(
            "/Foo/Bar/f",
            curios_core::Term::type_(),
            curios_core::Term::type_(),
            curios_core::Term::var(curios_core::Var::free("/Foo/Bar/f"))
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
        curios_core::Term::let_(
            "/Foo/Bar/f",
            curios_core::Term::type_(),
            curios_core::Term::type_(),
            curios_core::Term::var(curios_core::Var::free("/Foo/Bar/f"))
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
            "/Foo/Bar/f",
            curios_core::Term::type_(),
            curios_core::Term::type_(),
            curios_core::Term::var(curios_core::Var::free("/Foo/Bar/f"))
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
            "/A/X/f",
            curios_core::Term::type_(),
            curios_core::Term::type_(),
            curios_core::Term::var(curios_core::Var::free("/A/X/f"))
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
        curios_core::Term::let_(
            "/C/x",
            curios_core::Term::type_(),
            curios_core::Term::type_(),
            curios_core::Term::var(curios_core::Var::free("/C/x"))
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
        curios_core::Term::let_(
            "/B/M/x",
            curios_core::Term::type_(),
            curios_core::Term::type_(),
            curios_core::Term::var(curios_core::Var::free("/B/M/x"))
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
        curios_core::Term::let_(
            "/Facade/Impl/helper",
            curios_core::Term::type_(),
            curios_core::Term::type_(),
            curios_core::Term::var(curios_core::Var::free("/Facade/Impl/helper"))
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

    assert!(format!("{term:?}").contains("Foo/U/A"));
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

    assert!(format!("{term:?}").contains("Foo/U/A"));
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
fn unexposed_public_representation_may_use_private_helpers() {
    run(r#"
        mod M
            struct Hidden : Type { Type }
            struct Open : pub Type { Hidden }
        end
        Type
    "#);
}

#[test]
fn transparent_alias_exposure_audits_the_complete_representation() {
    let error = run_err(
        r#"
        mod M
            struct Hidden : Type { Type }
            struct Open : pub Type { Hidden }
            pub let Alias : Type = Open;
        end
        Type
    "#,
    );

    assert!(
        error.contains("exposes private item '/M/Hidden'"),
        "unexpected error: {error}"
    );
}

#[test]
fn parameterized_transparent_alias_preserves_representation_provenance() {
    let error = run_err(
        r#"
        mod M
            struct Hidden : Type { Type }
            struct Open(A : Type) : pub Type { hidden : Hidden, value : A }
            pub let Alias(A : Type) -> Type = Open(A);
        end
        Type
    "#,
    );

    assert!(
        error.contains("exposes private item '/M/Hidden'"),
        "unexpected error: {error}"
    );
}

#[test]
fn exposure_audit_accepts_a_separately_exposed_dependency() {
    run(r#"
        mod M
            struct Hidden : Type { Type }
            pub let PublicHidden : Type = Hidden;
            struct Open : pub Type { Hidden }
            pub let PublicOpen : Type = Open;
        end
        Type
    "#);
}

#[test]
fn direct_representation_exposure_accepts_a_separately_exposed_dependency() {
    run(r#"
        mod M
            struct Hidden : Type { Type }
            pub let PublicHidden : Type = Hidden;
            pub struct Open : pub Type { Hidden }
        end
        Type
    "#);
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
    let error = lower_with_prelude("pub mod Foo\n    pub use /sys/{Nat};\nend\nType").unwrap_err();
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

// A user program cannot declare its own top-level `std`, `pub` or not — it
// would collide with the embedded standard library mounted at the same name.
#[test]
fn rejects_user_pub_mod_std_colliding_with_prelude_std() {
    let error =
        lower_with_prelude("pub mod std\n    pub let x : Type = Type;\nend\nType").unwrap_err();
    assert!(error.contains("std"), "unexpected error: {error}");
}

// The private case is the actual regression this guard closes: before
// `ModuleInfo::insert_child`'s collision check was made unconditional, a
// private redeclaration of a reserved name didn't trip the pub-only guard and
// silently overwrote the prelude's `std` registration instead of erroring.
#[test]
fn rejects_user_private_mod_std_colliding_with_prelude_std() {
    let error = lower_with_prelude("mod std\n    let x : Type = Type;\nend\nType").unwrap_err();
    assert!(error.contains("std"), "unexpected error: {error}");
}

// Without a prelude attached, `has_embedded_roots()` is false, so the fixed
// sys/syn/std machinery never runs at all — the user's own `mod std` is just
// an ordinary, unreserved entry-rooted module, not a collision.
#[test]
fn user_own_mod_std_without_prelude_is_not_a_collision() {
    run("mod std\n    pub let x : Type = Type;\nend\nuse std/{x};\nx");
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
        curios_core::Term::let_(
            "/Foo/x",
            curios_core::Term::type_(),
            curios_core::Term::type_(),
            curios_core::Term::let_(
                "/Foo/y",
                curios_core::Term::type_(),
                curios_core::Term::type_(),
                curios_core::Term::var(curios_core::Var::free("/Foo/x"))
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
    .parse::<crate::Entrypoint>()
    .unwrap();
    let loader = crate::RootSource::file_system(base.clone());

    super::into_core(&entrypoint, &loader).unwrap();

    fs::remove_dir_all(base).unwrap();
}

#[test]
fn file_backed_module_missing_from_loader_is_module_not_found() {
    let entrypoint = r#"
            pub mod A;
            Type
        "#
    .parse::<crate::Entrypoint>()
    .unwrap();

    assert!(matches!(
        super::into_core(&entrypoint, &crate::RootSource::none()).unwrap_err(),
        crate::Error::Located { error, .. }
            if matches!(error.as_ref(), crate::Error::ModuleNotFound { path } if path == "/A")
    ));
}

#[test]
fn goal_lowers_to_marked_metavar() {
    // A written `?` lowers to the same fresh metavariable a desugared hole
    // does, but marked `MetavarOrigin::Goal` so zonk reports it.
    assert_eq!(run("?"), curios_core::Term::goal(0));
}

#[test]
fn distinct_goals_get_distinct_ids() {
    // Two goals in one program draw distinct, monotonic ids from the shared counter.
    let term = run("(?, ?)");
    assert_eq!(
        term,
        curios_core::Term::tuple([curios_core::Term::goal(0), curios_core::Term::goal(1)]),
    );
}

#[test]
fn bang_desugars_through_syn_monad_bind() {
    // Every value body is a region root: `x!` hoists to it and sequences
    // through the `/syn/Monad/bind` wrapper applied to the action and the
    // continuation over a gensym'd binder. The witness slot and implicits are
    // inserted during core elaboration.
    let expected = curios_core::Term::apply(
        curios_core::Term::var(curios_core::Var::free("/syn/Monad/bind")),
        [
            curios_core::Term::var(curios_core::Var::free("x")),
            curios_core::Term::func(
                [("#0".to_string(), curios_core::Term::metavar(0))],
                curios_core::Term::var(curios_core::Var::free("#0")),
            ),
        ],
    );
    assert_eq!(run("x!"), expected);
}

#[test]
fn headless_cond_ladder_lowers_to_nested_bln_matches() {
    // `match | p => a | q => b | _ => ? end` right-folds into two nested `Bln`
    // matches: the first condition's false branch holds the second, whose own
    // false branch is the `_` default (a plain hole here).
    let term = run("match | p => a | q => b | _ => ? end");

    let curios_core::Subterm::Match(outer) = &*term else {
        panic!("expected a Match at the top, got {term:?}");
    };
    let curios_core::Cases::Bln { false_case, .. } = &outer.cases else {
        panic!("expected the outer Cases::Bln, got {:?}", outer.cases);
    };
    let curios_core::Subterm::Match(inner) = &**false_case else {
        panic!("expected a nested Match in the outer false branch, got {false_case:?}");
    };
    let curios_core::Cases::Bln {
        false_case: inner_false,
        ..
    } = &inner.cases
    else {
        panic!("expected the inner Cases::Bln, got {:?}", inner.cases);
    };
    assert!(
        matches!(&**inner_false, curios_core::Subterm::Metavar(_)),
        "the `_` default should sit at the innermost false branch, got {inner_false:?}"
    );
}

#[test]
fn bind_arm_bare_binder_is_rejected() {
    // `| x = n =>` binds irrefutably — always fires, so the rest of the ladder
    // is dead. Rejected in favor of a `let`.
    let error = run_err("match | x = n => x | _ => 0 end");
    assert!(
        error.contains("refutable") && error.contains("let"),
        "unexpected error: {error}"
    );
}

#[test]
fn named_catch_all_is_rejected() {
    // A named final arm among concrete constructor arms is not a catch-all.
    let error = run_err("match m | some(x) => x | rest => 0 end");
    assert!(
        error.contains("named final arm") && error.contains("_"),
        "unexpected error: {error}"
    );
}

#[test]
fn nested_underscore_mixed_with_concrete_stays_inconsistent_shape() {
    // A `_` *nested* inside a constructor payload (not a final top-level arm)
    // still mixes a binder with a concrete shape in the same column — the
    // pre-existing full-enumeration boundary, not a catch-all.
    let error = run_err("match m | some(some(x)) => x | some(_) => 0 | none() => 1 end");
    assert!(
        error.contains("disagree on shape"),
        "unexpected error: {error}"
    );
}

#[test]
fn nested_nat_literal_lowers_to_switch() {
    // A literal `5` inside a constructor payload, with a `_` fallthrough, is
    // value dispatch — it lowers through `compile_nat`'s switch mode to a
    // `Cases::Switch`, not the `Nat` eliminator. (`wrap`/`b` need not resolve:
    // lowering precedes name resolution.)
    let term = run("match b | wrap(5) => 1 | _ => 0 end");
    assert!(
        format!("{term:?}").contains("Switch"),
        "expected a Cases::Switch, got {term:?}"
    );
}

#[test]
fn nat_literal_mixed_with_succ_is_rejected() {
    // A literal case and a `n + 1; ih` successor arm in the same `Nat` column
    // select incompatible core forms (a value `switch` vs. the eliminator).
    let error = run_err("match b | wrap(5) => 1 | wrap(n + 1; ih) => n | _ => 0 end");
    assert!(
        error.contains("mixes successor-peeling"),
        "unexpected error: {error}"
    );
}

#[test]
fn bang_in_a_type_is_rejected() {
    // Types have no region to hoist an action to, so a `!` in an annotation is
    // rejected during desugaring.
    assert!(run_err("let a : e! = x; a").contains("not allowed inside a type"));
}

#[test]
fn foreign_declaration_populates_the_store() {
    // No loader/prelude needed at all: a `foreign` signature is parsed
    // directly into `WireType`s, not resolved as ordinary names.
    let (_, _, foreigns) = super::into_core(
        &"foreign frobnicate : (Nat, Bin) -> Nat; 0"
            .parse::<crate::Entrypoint>()
            .unwrap(),
        &crate::RootSource::none(),
    )
    .unwrap();

    let function = foreigns.get("/frobnicate").expect("frobnicate registered");
    assert_eq!(
        function.signature.params,
        vec![
            ("a0".to_string(), curios_abi::WireType::Nat),
            ("a1".to_string(), curios_abi::WireType::Bin),
        ]
    );
    assert_eq!(
        function.signature.results,
        vec![("_".to_string(), curios_abi::WireType::Nat)]
    );
}

#[test]
fn foreign_declaration_zero_arg_populates_the_store() {
    let (_, _, foreigns) = super::into_core(
        &"foreign clock : Nat; 0"
            .parse::<crate::Entrypoint>()
            .unwrap(),
        &crate::RootSource::none(),
    )
    .unwrap();

    let function = foreigns.get("/clock").expect("clock registered");
    assert!(function.signature.params.is_empty());
}

#[test]
fn foreign_declaration_call_lowers() {
    // Declaring and calling a foreign function lowers end to end (`run`
    // panics on failure) — the `Prim::Foreign` body `foreign_signature`
    // builds is well typed against the same wire-typed signature the call
    // site checks against.
    let _ = run(r#"
        foreign frobnicate : (Nat, Bin) -> Nat;
        frobnicate(5, x\00\01)
    "#);
}

// Caught during discovery now (`ModuleInfo::insert_binding`'s collision guard
// is unconditional, not pub-only), before `Context::insert_binding`'s later
// scope-conflict check would otherwise see it.
#[test]
fn duplicate_foreign_declaration_in_one_scope_is_rejected() {
    assert!(
        run_err("foreign frobnicate : Nat; foreign frobnicate : Nat; 0")
            .contains("duplicate public declaration")
    );
}

#[test]
fn foreign_declarations_across_modules_get_distinct_import_names() {
    // Two `foreign` declarations in different modules coexist: the wasm
    // import name is the declaration's fully qualified name, so the shared
    // label never collides on the wire — each module's row registers under
    // its own name.
    let (_, _, foreigns) = super::into_core(
        &r#"
        mod A
            foreign frobnicate : Nat;
        end
        mod B
            foreign frobnicate : Nat;
        end
        0
    "#
        .parse::<crate::Entrypoint>()
        .unwrap(),
        &crate::RootSource::none(),
    )
    .unwrap();

    assert!(foreigns.get("/A/frobnicate").is_some());
    assert!(foreigns.get("/B/frobnicate").is_some());
}
