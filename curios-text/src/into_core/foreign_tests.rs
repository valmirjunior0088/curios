//! Foreign declarations: the store they populate and the import names they take across modules.

use crate::{Entrypoint, RootSource};
use curios_abi::{WireResults, WireType};

use super::test_support::*;

#[test]
fn declaration_populates_the_store() {
    // No loader/prelude needed at all: a `foreign` signature is parsed directly into `WireType`s, not resolved as ordinary names.
    let (_, _, _, foreigns) = super::into_core(
        &"foreign frobnicate : (Nat, Bytes) -> Nat; 0"
            .parse::<Entrypoint>()
            .unwrap(),
        &RootSource::none(),
        syntax(),
    )
    .unwrap();

    let function = foreigns.get("/frobnicate").expect("frobnicate registered");
    assert_eq!(
        function.signature.params,
        vec![
            ("a0".to_string(), WireType::Nat),
            ("a1".to_string(), WireType::Bytes),
        ]
    );
    assert_eq!(
        function.signature.results,
        WireResults::single("_".to_string(), WireType::Nat)
    );
}

#[test]
fn declaration_zero_arg_populates_the_store() {
    let (_, _, _, foreigns) = super::into_core(
        &"foreign clock : Nat; 0".parse::<Entrypoint>().unwrap(),
        &RootSource::none(),
        syntax(),
    )
    .unwrap();

    let function = foreigns.get("/clock").expect("clock registered");
    assert!(function.signature.params.is_empty());
}

#[test]
fn declaration_call_lowers() {
    // Declaring and calling a foreign function lowers end to end (`run` panics on failure) — the `Intrinsic::Foreign` body `foreign_signature` builds is well typed against the same wire-typed signature the call site checks against.
    let _ = run(r#"
        foreign frobnicate : (Nat, Bytes) -> Nat;
        frobnicate(5, x[0x00, 0x01])
    "#);
}

// Caught during discovery now (`ModuleInfo::insert_binding`'s collision guard is unconditional, not pub-only), before `Context::insert_binding`'s later scope-conflict check would otherwise see it.
#[test]
fn duplicate_foreign_declaration_in_one_scope_is_rejected() {
    assert!(
        run_err("foreign frobnicate : Nat; foreign frobnicate : Nat; 0")
            .contains("duplicate public declaration")
    );
}

#[test]
fn declarations_across_modules_get_distinct_import_names() {
    // Two `foreign` declarations in different modules coexist: the wasm import name is the declaration's fully qualified name, so the shared label never collides on the wire — each module's row registers under its own name.
    let (_, _, _, foreigns) = super::into_core(
        &r#"
        mod A
            foreign frobnicate : Nat;
        end
        mod B
            foreign frobnicate : Nat;
        end
        0
    "#
        .parse::<Entrypoint>()
        .unwrap(),
        &RootSource::none(),
        syntax(),
    )
    .unwrap();

    assert!(foreigns.get("/A/frobnicate").is_some());
    assert!(foreigns.get("/B/frobnicate").is_some());
}
