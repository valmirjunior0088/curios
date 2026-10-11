//! A binding is held to the row it implements: its operands and results must cross as the row's do, which is checked when it is defined rather than when a program first calls it.

use {
    super::{ForeignBindings, host_rows, shared_engine},
    crate::MockHost,
    curios_abi::{
        ForeignFunction, ForeignStore, HostOp, WireLeaf, WireResults, WireReturn, WireSignature,
        WireType,
    },
    std::sync::Arc,
    wasmtime::Linker,
};

/// An embedder's store of one declared row, `/double: (x: Nat) -> Nat`.
fn doubling() -> ForeignBindings {
    let mut store = ForeignStore::new();

    store.register(ForeignFunction {
        name: "/double".to_string(),
        label: "double".to_string(),
        signature: WireSignature {
            params: vec![WireType::Nat],
            returns: WireReturn::Answers(WireResults::single(WireType::Nat)),
        },
    });

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

/// Every answer the host gives is generated from its table row, and it crosses as that row states: linking each against the declaration the table describes is the check that the row's operand and reply types lift and lower as its wire signature says, for every row and not only the ones a program reaches.
#[test]
fn every_host_answer_crosses_as_its_row() {
    let (host, _io) = MockHost::builder().build();
    let provided = host_rows(Arc::new(host));
    let engine = shared_engine();
    let mut linker = Linker::new(engine);

    for op in HostOp::ALL {
        let declared = Arc::new(ForeignFunction {
            name: op.declared(),
            label: op.name().to_string(),
            signature: op.signature().clone(),
        });

        provided
            .link(&mut linker, engine, &op.declared(), Some(&declared))
            .unwrap_or_else(|refusal| panic!("{refusal}"));
    }
}

/// A `Bits` value crosses as the packed bytes a `Bytes` value does, so the codec that reads one reads the other.
#[test]
fn a_bits_row_is_read_by_the_bytes_codec() {
    let mut store = ForeignStore::new();

    store.register(ForeignFunction {
        name: "/echo".to_string(),
        label: "echo".to_string(),
        signature: WireSignature {
            params: vec![WireType::List(WireLeaf::Bits)],
            returns: WireReturn::Answers(WireResults::single(WireType::List(WireLeaf::Bits))),
        },
    });

    ForeignBindings::new(store).define("/echo", |runs: Vec<Vec<u8>>| runs);
}
