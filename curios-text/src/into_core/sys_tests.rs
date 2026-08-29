//! `/sys` is reachable only through the standard library, and a user module may not collide with the prelude's.

use crate::{Intrinsic, LetSignature, Subterm, Term, TopItem, sys_module};
use curios_abi::host_ops;

use super::test_support::*;

// `sys` is the trusted intrinsic substrate, reachable only from the standard library. A user entrypoint that names it — through a `use` or a bare term reference — is rejected at resolution; the `/std` wrappers are the door.
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

// The guard rides the *resolved* qualifier, not the spelling, so a relative reference is rejected exactly as the absolute one is — the leading `/` is not the boundary.
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

// The interface (`pub use`) phase guards too: a user module cannot launder `sys` into its own public surface.
#[test]
fn rejects_sys_pub_use_reexport_from_user_code() {
    let error = lower_with_prelude("pub mod Foo\n    pub use /sys/{Nat};\nend\nType").unwrap_err();
    assert!(
        error.contains("internal to the standard library"),
        "unexpected error: {error}"
    );
}

// The same intrinsic reached through its `/std` wrapper resolves: `std` is privileged to reference `sys`, and re-exports it.
#[test]
fn allows_sys_reference_through_std_wrapper() {
    assert!(lower_with_prelude("use /std/{Nat}; Nat/add(1, 2)").is_ok());
}

// A user program cannot declare its own top-level `std`, `pub` or not — it would collide with the embedded standard library mounted at the same name.
#[test]
fn rejects_user_pub_mod_std_colliding_with_prelude_std() {
    let error =
        lower_with_prelude("pub mod std\n    pub let x : Type = Type;\nend\nType").unwrap_err();
    assert!(error.contains("std"), "unexpected error: {error}");
}

// The private case is the actual regression this guard closes: before `ModuleInfo::insert_child`'s collision check was made unconditional, a private redeclaration of a reserved name didn't trip the pub-only guard and silently overwrote the prelude's `std` registration instead of erroring.
#[test]
fn rejects_user_private_mod_std_colliding_with_prelude_std() {
    let error = lower_with_prelude("mod std\n    let x : Type = Type;\nend\nType").unwrap_err();
    assert!(error.contains("std"), "unexpected error: {error}");
}

// Without a prelude attached, `has_embedded_roots()` is false, so the fixed sys/syn/std machinery never runs at all — the user's own `mod std` is just an ordinary, unreserved entry-rooted module, not a collision.
#[test]
fn user_own_mod_std_without_prelude_is_not_a_collision() {
    run("mod std\n    pub let x : Type = Type;\nend\nuse std/{x};\nx");
}

#[test]
fn rejects_private_module_from_outside_its_declaring_subtree() {
    assert!(
        run_err(
            r#"
        pub mod Owner
            mod Foo
                pub let f : Type = Type;
            end
        end
        pub mod Bar
            use /Owner/Foo/{f};
        end
        Type
    "#
        )
        .contains("private child module")
    );
}

// A private declaration written at the root belongs to the root's subtree, which is the whole program — so a sibling module may name it. The boundary is the declaring module, and the root declares no boundary above itself.
#[test]
fn allows_a_private_root_module_from_a_sibling() {
    run(r#"
        mod Foo
            pub let f : Type = Type;
        end
        pub mod Bar
            use /{Foo};
        end
        Type
    "#);
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

/// `/sys` offers no way out of `Io`: the effect module exports its carrier and its two monad operations, and no `/sys` function anywhere takes an `Io` to something that is not one.
///
/// This *narrows* `documentation/soundness/per-term-rules/a-type-is-a-pure-term.md`; it does not discharge it. That row rests on there being no operation from `Io(T)` to `T`, and the surface an eliminator could enter through is two Rust tables rather than the whole library: a foreign row cannot introduce one, because `host_fn` wraps every store row's result in `Io` at a single site and `WireType` is a closed enum with no case that could name an `Io` in a domain; and no `.crs` can define one without already having one. So what is checkable is the roster below, and that is what is checked — the general property remains argued.
#[test]
fn the_sys_io_roster_offers_no_eliminator() {
    /// Whether a signature's type is headed by `Io`, which is the only shape that matters here: `/sys` states its effect types directly rather than behind an alias, so a domain that is an `Io` is written as one.
    fn is_io(term: &Term) -> bool {
        matches!(&**term, Subterm::Intrinsic(Intrinsic::IoType(_)))
    }

    /// Every `let` in the module tree, flattened — a nested module is walked rather than skipped, so a later `/sys` submodule cannot host an eliminator out of view.
    fn lets<'a>(items: &'a [TopItem], out: &mut Vec<(&'a str, &'a LetSignature)>) {
        for item in items {
            match item {
                TopItem::Let(bindings) => out.extend(
                    bindings
                        .iter()
                        .map(|binding| (binding.label.as_str(), &binding.signature)),
                ),
                TopItem::Mod(module) => {
                    if let Some(module) = &module.module {
                        lets(&module.items, out);
                    }
                }
                _ => {}
            }
        }
    }

    let sys = sys_module(&host_ops(), &SYNTAX);

    let io = sys
        .items
        .iter()
        .find_map(|item| match item {
            TopItem::Mod(module) if module.label == "Io" => module.module.as_ref(),
            _ => None,
        })
        .expect("/sys declares an Io module");

    let mut exported = Vec::new();
    lets(&io.items, &mut exported);
    let names = exported.iter().map(|(label, _)| *label).collect::<Vec<_>>();
    assert_eq!(
        names,
        ["Io", "pure", "bind"],
        "/sys/Io's roster changed; an entry here is a new way into or out of the effect type"
    );

    let mut all = Vec::new();
    lets(&sys.items, &mut all);
    for (label, signature) in all {
        let LetSignature::Func { params, output, .. } = signature else {
            continue;
        };

        let consumes_io = params.iter().any(|param| is_io(&param.type_));
        assert!(
            !consumes_io || is_io(output),
            "/sys/{label} takes an Io to a non-Io result, which is an eliminator for the effect type"
        );
    }
}
