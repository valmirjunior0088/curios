use {
    super::{
        ClosedCode, Failure, FileKind, ForeignFunction, ForeignStore, HostOp, Mode, ResultShape,
        SerialFlow, SerialOp, SerialParity, StdioMode, WireLeaf, WireResults, WireReturn,
        WireSignature, WireType,
    },
    crate::status,
    std::fmt::Debug,
};

/// The results `op` answers with, for a row that returns.
fn answers(op: HostOp) -> &'static WireResults {
    match &op.signature().returns {
        WireReturn::Answers(results) => results,
        WireReturn::Never => panic!("`{}` does not return", op.name()),
    }
}

/// Every named code sits below `OTHER_BASE` and the errno passthrough lowers at or above it, so a raw OS errno — including the errno-less `Other(0)` — can never decode as `OK` or a named failure.
#[test]
fn errno_lane_is_disjoint_from_named_codes() {
    let named = [
        status::OK,
        status::EOF,
        status::NOT_FOUND,
        status::PERMISSION_DENIED,
        status::ALREADY_EXISTS,
        status::CONNECTION_REFUSED,
        status::WOULD_BLOCK,
        status::TLS_ERROR,
        status::NOT_EMPTY,
        status::IS_DIRECTORY,
        status::NOT_DIRECTORY,
        status::CONNECTION_RESET,
    ];

    assert!(named.iter().all(|&code| code < status::OTHER_BASE));
    assert_eq!(Failure::Other(0).code(), status::OTHER_BASE);
}

/// Result labels are the record fields the guest projects (`.status`, `.secs`, …) — renaming one is a standard-library break, so the multi-result shapes are pinned.
#[test]
fn result_records_keep_their_labels() {
    let labels = |op: HostOp| -> Vec<String> {
        match answers(op).shape() {
            ResultShape::Record(fields) => fields
                .into_iter()
                .map(|(label, _)| label.to_string())
                .collect(),
            _ => Vec::new(),
        }
    };

    assert_eq!(labels(HostOp::HandleRead), ["status", "bytes"]);
    assert_eq!(labels(HostOp::HandleWrite), ["status", "written"]);
    assert_eq!(labels(HostOp::FileOpen), ["status", "handle"]);
    assert_eq!(labels(HostOp::DnsLookup), ["status", "handle"]);
    assert_eq!(labels(HostOp::DnsResolve), ["status", "addresses"]);
    assert_eq!(labels(HostOp::SocketOpen), ["status", "handle"]);
    assert_eq!(labels(HostOp::SocketAccept), ["status", "handle"]);
    assert_eq!(labels(HostOp::TlsServerConfig), ["status", "handle"]);
    assert_eq!(labels(HostOp::ClockWall), ["secs", "nanos"]);
    assert_eq!(labels(HostOp::ClockMono), ["secs", "nanos"]);
    assert_eq!(labels(HostOp::ProcEnv), ["status", "value"]);
    assert_eq!(labels(HostOp::TtySize), ["status", "cols", "rows"]);
    assert_eq!(labels(HostOp::SerialOpen), ["status", "handle"]);
    assert_eq!(labels(HostOp::SerialDrain), ["status", "handle"]);
    assert_eq!(labels(HostOp::SerialList), ["status", "paths"]);
    assert_eq!(
        labels(HostOp::FileStat),
        ["status", "kind", "size", "mtime_secs", "mtime_nanos"]
    );
    assert_eq!(labels(HostOp::DirList), ["status", "names"]);
    assert_eq!(labels(HostOp::ProcCwd), ["status", "path"]);
    assert_eq!(
        labels(HostOp::ProcSpawn),
        ["status", "child", "stdin", "stdout", "stderr"]
    );
    assert_eq!(labels(HostOp::ProcWait), ["status", "code", "signal"]);
}

/// Results cross in the order they are written, whatever their shapes: a single result of any type, a row's status before its payload, and references in any slot — two of them, or one before a scalar — each read back where it stands.
#[test]
fn results_cross_in_the_order_they_are_written() {
    let single = WireResults::single(WireType::Bytes);
    assert_eq!(single.len(), 1);
    assert_eq!(single.types(), [WireType::Bytes]);

    assert_eq!(
        answers(HostOp::HandleRead).types(),
        [WireType::Nat, WireType::Bytes]
    );

    let mixed = WireResults::record(
        ("head".to_string(), WireType::Bytes),
        ("count".to_string(), WireType::Nat),
        vec![("rest".to_string(), WireType::List(WireLeaf::Bytes))],
    );
    assert_eq!(
        mixed.types(),
        [
            WireType::Bytes,
            WireType::Nat,
            WireType::List(WireLeaf::Bytes),
        ]
    );
}

/// The guest-facing shape is the one the reply's type states: `handle_close` answers the unit value, `rand_bytes` its bytes bare, `handle_read` a record of its two labelled results.
#[test]
fn the_guest_shape_is_the_one_the_reply_states() {
    let shape = |op: HostOp| answers(op).shape();

    assert_eq!(shape(HostOp::HandleClose), ResultShape::Unit);
    assert_eq!(
        shape(HostOp::RandBytes),
        ResultShape::Single(WireType::Bytes)
    );
    assert_eq!(
        shape(HostOp::HandleRead),
        ResultShape::Record(vec![("status", WireType::Nat), ("bytes", WireType::Bytes)])
    );
}

/// A row is declared in its module by its method's name, each segment of the module's path a segment of the name.
#[test]
fn a_row_is_declared_in_its_module_by_its_method() {
    assert_eq!(HostOp::HandleRead.declared(), "/std/Handle/handle_read");
    assert_eq!(HostOp::DnsLookup.declared(), "/std/tcp/resolve/dns_lookup");
}

/// A function is its import name: its signature does not participate, since no store holds two under one name.
#[test]
fn a_function_is_its_import_name() {
    let declared = |name: &str, params: Vec<WireType>| ForeignFunction {
        name: name.to_string(),
        signature: WireSignature {
            params,
            returns: WireReturn::Answers(WireResults::none()),
        },
    };

    assert_eq!(
        declared("/frobnicate", vec![]),
        declared("/frobnicate", vec![WireType::Nat])
    );
    assert_ne!(declared("/frobnicate", vec![]), declared("/other", vec![]));
}

/// The import name is the identity every stage links on, so a second row with the same name is a construction bug.
#[test]
#[should_panic(expected = "already registered")]
fn register_rejects_a_duplicate_name() {
    let declared = || ForeignFunction {
        name: "/frobnicate".to_string(),
        signature: WireSignature {
            params: vec![],
            returns: WireReturn::Answers(WireResults::none()),
        },
    };
    let mut store = ForeignStore::new();

    store.register(declared());
    store.register(declared());
}

/// `proc_exit` is the one row that diverges, and it crosses a `Byte` out and nothing back: its call never returns, which is not the same as a row returning nothing.
#[test]
fn exit_is_the_one_row_that_diverges() {
    let diverging = HostOp::ALL
        .iter()
        .filter(|op| op.signature().returns == WireReturn::Never)
        .collect::<Vec<_>>();
    assert_eq!(
        diverging.iter().map(|op| op.name()).collect::<Vec<_>>(),
        ["proc_exit"]
    );

    let exit = diverging[0].signature();
    assert_eq!(exit.params, [WireType::Byte]);
    assert!(exit.returns.types().is_empty());
    assert_eq!(
        HostOp::HandleClose.signature().returns,
        WireReturn::Answers(WireResults::none())
    );
}

/// A variant is its row's method name in CamelCase — the one spelling the table writes twice, since a macro cannot change an identifier's case — so a row whose two names disagree fails here rather than reading as a different operation.
#[test]
fn a_variant_is_its_method_name_in_camel_case() {
    for &op in HostOp::ALL {
        let camel = op
            .name()
            .split('_')
            .map(|part| part[..1].to_uppercase() + &part[1..])
            .collect::<String>();

        assert_eq!(format!("{op:?}"), camel);
    }
}

/// A file kind's codes are every kind's, and no other.
#[test]
fn a_file_kind_answers_one_of_its_codes() {
    let kinds = [
        FileKind::File,
        FileKind::Directory,
        FileKind::Symlink,
        FileKind::Other,
    ];

    assert_eq!(FileKind::WIRE_CODES, kinds.map(FileKind::code));
}

/// Every closed code reads back as the variant it names, and a code past the table names none: an argument outside it is malformed rather than read as a neighbour.
#[test]
fn a_closed_code_reads_back_its_variant_and_nothing_past_it() {
    fn round_trips<T: ClosedCode + Debug>() {
        for &(variant, code) in T::CODES {
            assert_eq!(T::from_code(code), Some(variant));
            assert_eq!(variant.code(), code);
        }

        let past = T::CODES.iter().map(|(_, code)| code).max().unwrap() + 1;
        assert_eq!(T::from_code(past), None);
    }

    round_trips::<Mode>();
    round_trips::<StdioMode>();
    round_trips::<SerialParity>();
    round_trips::<SerialFlow>();
    round_trips::<SerialOp>();
}
