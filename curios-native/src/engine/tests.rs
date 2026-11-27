//! An answer is held to the row it is linked to: its operands and what it comes back with must cross as the program declares them, which is refused where the two are linked rather than when a program first calls the row. And a module runs under the recorder its caller set, on the thread built for it.

use {
    super::*,
    crate::test_support::*,
    curios_abi::{
        ForeignFunction, ForeignStore, HostOp, Termination, WireResults, WireReturn, WireSignature,
        WireType,
    },
    curios_tracing::tracing::{
        Event, Metadata, Subscriber,
        span::{Attributes, Id, Record},
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
    start_test_engine();
    let engine = shared_engine();

    link(
        &mut Linker::new(engine),
        engine,
        foreigns,
        &bindings.answers,
        &Answers::default(),
        &filer(),
        name,
    )
    .map_err(|stop| stop.to_string())
}

/// A host to file what an answer gives later with, which no link here calls.
fn filer() -> Arc<dyn HostOps + Send + Sync> {
    Arc::new(MockHost::builder().build().0)
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
        "signature mismatch: a binding answers `/double`; obtained: (Int) -> (Nat); expected: (Nat) -> (Nat)"
    );
}

#[test]
fn an_answer_whose_results_cross_otherwise_is_refused() {
    let mut bindings = ForeignBindings::empty();
    bindings.define("/double", |x: u64| x.is_multiple_of(2));

    assert_eq!(
        linked(&doubling(), &bindings, "/double").unwrap_err(),
        "signature mismatch: a binding answers `/double`; obtained: (Nat) -> (Bool); expected: (Nat) -> (Nat)"
    );
}

#[test]
fn a_row_nothing_answers_is_refused_by_name() {
    assert_eq!(
        linked(&doubling(), &ForeignBindings::empty(), "/double").unwrap_err(),
        "no implementation: `/double` is declared `foreign`; no module answers it, and this host does not"
    );
}

/// Recording an answer refuses nothing, so what a binding gets wrong about the program is refused where a run admits its bindings: a row answered twice, and a name the program declares no row for.
#[test]
fn a_binding_the_program_cannot_use_is_refused_by_name() {
    let mut twice = ForeignBindings::empty();
    twice.define("/double", |x: u64| x * 2);
    twice.define("/double", |x: u64| x + x);

    assert_eq!(
        twice.answers.admit(&doubling()).unwrap_err().to_string(),
        "duplicate answer: `/double` is answered twice by the bindings of this run"
    );

    let mut stray = ForeignBindings::empty();
    stray.define("/double", |x: u64| x * 2);
    stray.define("/halve", |x: u64| x / 2);

    assert_eq!(
        stray.answers.admit(&doubling()).unwrap_err().to_string(),
        "unknown declaration: a binding answers `/halve`; the program declares no `foreign` of that name"
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
        "signature mismatch: a binding answers `/abort`; obtained: (Byte) -> (); expected: (Byte) -> Never"
    );
}

/// Every answer the host gives is generated from its table row, and it crosses as that row states: linking each against the declaration the table describes is the check that the row's operand and reply types lift and lower as its wire signature says, for every row and not only the ones a program reaches.
#[test]
fn every_host_answer_crosses_as_its_row() {
    let (host, _io) = MockHost::builder().build();
    let answers = host_rows(Arc::new(host));
    start_test_engine();
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
            &filer(),
            &op.declared(),
        )
        .unwrap_or_else(|refusal| panic!("{refusal}"));
    }
}

// The engine is built once, so a process that started it one way keeps it that way: a start with the same choice is answered as the first was, and one with the other is refused.
#[test]
fn a_start_with_the_other_choice_is_refused() {
    start_test_engine();

    assert!(start_engine(GuestMap::Unwritten).is_ok());
    assert!(start_engine(GuestMap::Written).is_err());
}

/// A recorder that keeps nothing, told from every other by its type.
struct Marked;

impl Subscriber for Marked {
    fn enabled(&self, _: &Metadata<'_>) -> bool {
        false
    }

    fn new_span(&self, _: &Attributes<'_>) -> Id {
        Id::from_u64(1)
    }

    fn record(&self, _: &Id, _: &Record<'_>) {}

    fn record_follows_from(&self, _: &Id, _: &Id) {}

    fn event(&self, _: &Event<'_>) {}

    fn enter(&self, _: &Id) {}

    fn exit(&self, _: &Id) {}
}

/// A recorder its caller set for its own thread alone is the one a module runs under on the thread built for it, so a measurement's phase and a test hear the module's host calls. Mutation-checked: with the guest's thread spawned bare, the module runs under the process's default.
#[test]
fn a_module_runs_under_the_recorder_its_caller_set() {
    let recorder = Dispatch::new(Marked);
    let heard = dispatcher::with_default(&recorder, || {
        on_guest_stack(|| dispatcher::get_default(|current| current.is::<Marked>()))
    });

    assert_eq!(heard.ok(), Some(true));
}
