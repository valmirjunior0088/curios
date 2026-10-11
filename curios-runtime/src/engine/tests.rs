//! An answer is held to the row it is linked to: its operands and what it comes back with must cross as the program declares them, which is refused where the two are linked rather than when a program first calls the row.

use {
    super::{Answers, ForeignBindings, host_rows, link, shared_engine},
    crate::{Bits, MockHost, Termination},
    curios_abi::{
        ForeignFunction, ForeignStore, HostOp, WireLeaf, WireResults, WireReturn, WireSignature,
        WireType,
    },
    std::sync::Arc,
    wasmtime::Linker,
};

/// A program's rows of one declaration, `name: (operand) -> result`.
fn declaring(name: &str, operand: WireType, result: WireType) -> ForeignStore {
    let mut store = ForeignStore::new();

    store.register(ForeignFunction {
        name: name.to_string(),
        signature: WireSignature {
            params: vec![operand],
            returns: WireReturn::Answers(WireResults::single(result)),
        },
    });

    store
}

/// A program's rows of `/double: (Nat) -> Nat`.
fn doubling() -> ForeignStore {
    declaring("/double", WireType::Nat, WireType::Nat)
}

/// What linking `name` of `foreigns` answers, over `bindings` and a host that answers nothing.
fn linked(foreigns: &ForeignStore, bindings: &ForeignBindings, name: &str) -> Result<(), String> {
    let engine = shared_engine();

    link(
        &mut Linker::new(engine),
        engine,
        foreigns,
        &bindings.answers,
        &Answers::default(),
        name,
    )
}

#[test]
fn an_answer_that_crosses_as_its_row_is_linked() {
    let mut bindings = ForeignBindings::empty();
    bindings.define("/double", |x: u64| x * 2);

    linked(&doubling(), &bindings, "/double").expect("the answer crosses as the row");
}

#[test]
fn an_answer_whose_operands_cross_otherwise_is_refused() {
    let mut bindings = ForeignBindings::empty();
    bindings.define("/double", |x: i64| x.unsigned_abs() * 2);

    assert_eq!(
        linked(&doubling(), &bindings, "/double").unwrap_err(),
        "a binding answers `/double` as (Int) -> (Nat), and the program declares it (Nat) -> (Nat)"
    );
}

#[test]
fn an_answer_whose_results_cross_otherwise_is_refused() {
    let mut bindings = ForeignBindings::empty();
    bindings.define("/double", |x: u64| x.is_multiple_of(2));

    assert_eq!(
        linked(&doubling(), &bindings, "/double").unwrap_err(),
        "a binding answers `/double` as (Nat) -> (Bool), and the program declares it (Nat) -> (Nat)"
    );
}

#[test]
fn a_row_nothing_answers_is_refused_by_name() {
    assert_eq!(
        linked(&doubling(), &ForeignBindings::empty(), "/double").unwrap_err(),
        "no implementation for `/double`: no module answers it, and this host does not"
    );
}

/// Recording an answer refuses nothing, so what a binding gets wrong about the program is refused where a run admits its bindings: a row answered twice, and a name the program declares no row for.
#[test]
fn a_binding_the_program_cannot_use_is_refused_by_name() {
    let mut twice = ForeignBindings::empty();
    twice.define("/double", |x: u64| x * 2);
    twice.define("/double", |x: u64| x + x);

    assert_eq!(
        twice.answers.admit(&doubling()).unwrap_err(),
        "`/double` is answered twice by the bindings of this run"
    );

    let mut stray = ForeignBindings::empty();
    stray.define("/double", |x: u64| x * 2);
    stray.define("/halve", |x: u64| x / 2);

    assert_eq!(
        stray.answers.admit(&doubling()).unwrap_err(),
        "a binding answers `/halve`, which the program declares no foreign row for"
    );
}

/// A row that does not return is answered by an answer that does not either, one whose reply is a `Termination`. An answer that returns nothing is another row's: it would come back into a guest that holds no code for it.
#[test]
fn a_row_that_does_not_return_is_answered_by_a_termination() {
    let mut store = ForeignStore::new();
    store.register(ForeignFunction {
        name: "/abort".to_string(),
        signature: WireSignature {
            params: vec![WireType::Byte],
            returns: WireReturn::Never,
        },
    });

    let mut ending = ForeignBindings::empty();
    ending.define("/abort", Termination);
    linked(&store, &ending, "/abort").expect("the answer ends the program as the row does");

    let mut returning = ForeignBindings::empty();
    returning.define("/abort", |_: u8| ());

    assert_eq!(
        linked(&store, &returning, "/abort").unwrap_err(),
        "a binding answers `/abort` as (Byte) -> (), and the program declares it (Byte) -> Never"
    );
}

/// Every answer the host gives is generated from its table row, and it crosses as that row states: linking each against the declaration the table describes is the check that the row's operand and reply types lift and lower as its wire signature says, for every row and not only the ones a program reaches.
#[test]
fn every_host_answer_crosses_as_its_row() {
    let (host, _io) = MockHost::builder().build();
    let answers = host_rows(Arc::new(host));
    let engine = shared_engine();
    let mut linker = Linker::new(engine);
    let mut store = ForeignStore::new();

    for op in HostOp::ALL {
        store.register(ForeignFunction {
            name: op.declared(),
            signature: op.signature().clone(),
        });
    }

    for op in HostOp::ALL {
        link(
            &mut linker,
            engine,
            &store,
            &Answers::default(),
            &answers,
            &op.declared(),
        )
        .unwrap_or_else(|refusal| panic!("{refusal}"));
    }
}

/// A binding states `Bits` where its row does: the grains are two types on both sides of the wire, though one payload crosses for either, so an answer written for `Bytes` is no answer to a `Bits` row.
#[test]
fn a_bits_row_is_bound_by_a_bits_value() {
    let runs = WireType::List(WireLeaf::Bits);
    let store = declaring("/echo", runs, runs);

    let mut bindings = ForeignBindings::empty();
    bindings.define("/echo", |runs: Vec<Bits>| runs);
    linked(&store, &bindings, "/echo").expect("the answer crosses as the row");

    let mut bytes = ForeignBindings::empty();
    bytes.define("/echo", |runs: Vec<Vec<u8>>| runs);

    assert_eq!(
        linked(&store, &bytes, "/echo").unwrap_err(),
        "a binding answers `/echo` as (List(Bytes)) -> (List(Bytes)), and the program declares it (List(Bits)) -> (List(Bits))"
    );
}
