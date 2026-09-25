//! A binding is held to the row it implements: its operands and results must cross as the row's do, which is checked when it is defined rather than when a program first calls it.

use {
    super::{ForeignBindings, sys_impls},
    crate::MockHost,
    curios_abi::{
        DeclaredForeign, ForeignFunction, ForeignStore, WireLeaf, WireResults, WireSignature,
        WireType,
    },
    std::sync::Arc,
};

/// An embedder's store of one declared row, `/double: (x: Nat) -> Nat`.
fn doubling() -> ForeignBindings {
    let mut store = ForeignStore::new();

    store.register(ForeignFunction::Declared(DeclaredForeign {
        name: "/double".to_string(),
        label: "double".to_string(),
        signature: WireSignature {
            params: vec![("x".to_string(), WireType::Nat)],
            results: WireResults::single("value".to_string(), WireType::Nat),
        },
    }));

    ForeignBindings::new(store)
}

#[test]
fn a_binding_that_crosses_as_its_row_is_accepted() {
    doubling().define("/double", |x: u64| x * 2);
}

#[test]
#[should_panic(expected = "'/double' takes [Nat]")]
fn a_binding_whose_operands_cross_otherwise_is_refused() {
    doubling().define("/double", |x: i64| x.unsigned_abs() * 2);
}

#[test]
#[should_panic(expected = "'/double' answers [Nat]")]
fn a_binding_whose_results_cross_otherwise_is_refused() {
    doubling().define("/double", |x: u64| x.is_multiple_of(2));
}

/// Every builtin binding is generated from its row, and defining it holds its lifted operands and lowered reply to that row's wire signature: building the bindings is the check that the table's operand types and the runtime's codecs cross alike.
#[test]
fn every_builtin_binding_crosses_as_its_row() {
    let (host, _io) = MockHost::builder().build();

    sys_impls(Arc::new(host));
}

/// A `Bits` value crosses as the packed bytes a `Bytes` value does, so the codec that reads one reads the other.
#[test]
fn a_bits_row_is_read_by_the_bytes_codec() {
    let mut store = ForeignStore::new();

    store.register(ForeignFunction::Declared(DeclaredForeign {
        name: "/echo".to_string(),
        label: "echo".to_string(),
        signature: WireSignature {
            params: vec![("runs".to_string(), WireType::List(WireLeaf::Bits))],
            results: WireResults::single("value".to_string(), WireType::List(WireLeaf::Bits)),
        },
    }));

    ForeignBindings::new(store).define("/echo", |runs: Vec<Vec<u8>>| runs);
}
