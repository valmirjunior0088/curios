use {
    super::{ForeignFunction, Namespace, Status, WireSignature, host_ops},
    crate::status,
    std::collections::BTreeSet,
};

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
    ];

    assert!(named.iter().all(|&code| code < status::OTHER_BASE));
    assert_eq!(Status::Other(0).code(), status::OTHER_BASE);
}

/// The builtin import names, byte for byte and in declaration order — the wire ABI contract between the wasm emitter and the runtime linker. A mismatch here silently strands an import, so the whole list is pinned.
#[test]
fn names_are_the_wire_abi() {
    let names = host_ops()
        .iter()
        .map(|function| function.name.clone())
        .collect::<Vec<_>>();

    assert_eq!(
        names,
        [
            "read",
            "write",
            "open",
            "lookup",
            "resolve",
            "socket",
            "bind",
            "connect",
            "listen",
            "accept",
            "start_tls",
            "tls_server_config",
            "start_tls_server",
            "set_nonblocking",
            "set_recv_timeout",
            "set_send_timeout",
            "set_reuseaddr",
            "poll",
            "close",
            "clock_wall",
            "clock_mono",
            "random",
            "args",
            "env",
            "raw",
            "size",
        ]
    );
}

/// A row's `/sys` placement is its subject module plus its label, and two rows sharing one is a binding conflict the prelude only discovers when it is built. Labels alone are *not* unique — `file/open` and `socket/open` deliberately share a POSIX leaf name — so the pair is what this pins.
#[test]
fn placements_are_unique() {
    let store = host_ops();
    let placements: BTreeSet<_> = store
        .iter()
        .map(|function| (&function.subject, &function.label))
        .collect();

    assert_eq!(placements.len(), store.iter().count());
}

/// Every builtin states the `/sys` module it surfaces in: the prelude places rows by that column rather than by a list beside the table, so a row without one is a table it cannot read.
#[test]
fn ops_rows_name_a_subject() {
    assert!(host_ops().iter().all(|function| function.subject.is_some()));
}

/// Result labels are the record fields the guest projects (`.status`, `.secs_hi`, …) — renaming one is a standard-library break, so the multi-result shapes are pinned.
#[test]
fn result_records_keep_their_labels() {
    let store = host_ops();
    let labels = |name: &str| -> Vec<String> {
        store
            .get(name)
            .unwrap_or_else(|| panic!("host_ops lacks {name}"))
            .signature
            .results
            .iter()
            .map(|(label, _)| label.clone())
            .collect()
    };

    assert_eq!(labels("read"), ["status", "bytes"]);
    assert_eq!(labels("write"), ["status", "written"]);
    assert_eq!(labels("open"), ["status", "handle"]);
    assert_eq!(labels("lookup"), ["status", "handle"]);
    assert_eq!(labels("resolve"), ["status", "addresses"]);
    assert_eq!(labels("socket"), ["status", "handle"]);
    assert_eq!(labels("accept"), ["status", "handle"]);
    assert_eq!(labels("tls_server_config"), ["status", "handle"]);
    assert_eq!(labels("clock_wall"), ["secs_hi", "secs_lo", "nanos"]);
    assert_eq!(labels("clock_mono"), ["secs", "nanos"]);
    assert_eq!(labels("env"), ["status", "value"]);
    assert_eq!(labels("size"), ["status", "cols", "rows"]);
}

/// Every signature is well-formed: single results ride a name too (the guest type is the bare wire type, but the printer uses the label), and parameter names are unique within a signature. Nothing asserts that `List` does not nest — [`WireLeaf`](super::WireLeaf) makes a nested one unrepresentable.
#[test]
fn signatures_are_well_formed() {
    for function in host_ops().iter() {
        let signature = &function.signature;

        let params: BTreeSet<_> = signature.params.iter().map(|(name, _)| name).collect();
        assert_eq!(
            params.len(),
            signature.params.len(),
            "{} repeats a parameter name",
            function.name
        );
    }
}

/// The import name is the identity every stage links on, so a second row with the same name is a construction bug.
#[test]
#[should_panic(expected = "already registered")]
fn register_rejects_a_duplicate_name() {
    let mut store = host_ops();

    store.register(ForeignFunction {
        namespace: Namespace::Sys,
        name: "read".to_string(),
        subject: Some("Handle".to_string()),
        label: "read_again".to_string(),
        signature: WireSignature {
            params: vec![],
            results: vec![],
        },
    });
}

/// Every `host_ops` row is stamped with the `sys` wasm namespace — the fixed substrate `emit_sys_imports` reads instead of re-deriving membership by rebuilding this same store.
#[test]
fn ops_rows_are_stamped_with_the_sys_namespace() {
    assert!(
        host_ops()
            .iter()
            .all(|function| function.namespace == Namespace::Sys)
    );
}

/// Identity is the wasm import pair: `label` and `signature` don't participate (a cached row matches a freshly minted one), but the namespace does — one import name under `sys` and under `ffi` names two different imports.
#[test]
fn equality_is_the_import_pair() {
    let base = |namespace, label: &str| ForeignFunction {
        namespace,
        name: "frobnicate".to_string(),
        subject: None,
        label: label.to_string(),
        signature: WireSignature {
            params: vec![],
            results: vec![],
        },
    };

    assert_eq!(
        base(Namespace::Ffi, "frobnicate"),
        base(Namespace::Ffi, "frobnicate_again")
    );
    assert_ne!(
        base(Namespace::Sys, "frobnicate"),
        base(Namespace::Ffi, "frobnicate")
    );
}
