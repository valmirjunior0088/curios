use {
    super::{ForeignFunction, NAMESPACE_FFI, NAMESPACE_SYS, WireSignature, WireType, sys_io},
    std::collections::BTreeSet,
};

/// The `/sys/Io` import names, byte for byte and in declaration order —
/// the wire ABI contract between the wasm emitter and the runtime linker. A
/// mismatch here silently strands an import, so the whole list is pinned.
#[test]
fn names_are_the_wire_abi() {
    let names = sys_io()
        .iter()
        .map(|function| function.name.clone())
        .collect::<Vec<_>>();

    assert_eq!(
        names,
        [
            "io_read",
            "io_write",
            "io_open",
            "io_lookup",
            "io_resolve",
            "io_socket",
            "io_bind",
            "io_connect",
            "io_listen",
            "io_accept",
            "io_start_tls",
            "io_tls_server_config",
            "io_start_tls_server",
            "io_set_nonblocking",
            "io_set_recv_timeout",
            "io_set_send_timeout",
            "io_set_reuseaddr",
            "io_poll",
            "io_close",
            "io_clock_wall",
            "io_clock_mono",
            "io_random",
            "io_args",
            "io_env",
        ]
    );
}

/// Labels are the `/sys/Io` binding names, so they must be as unique as the
/// import names (`register` already enforces name uniqueness; this pins the
/// seed's labels too).
#[test]
fn labels_are_unique() {
    let store = sys_io();
    let labels: BTreeSet<_> = store.iter().map(|function| &function.label).collect();

    assert_eq!(labels.len(), store.len());
}

/// Result labels are the record fields the guest projects (`.status`,
/// `.secs_hi`, …) — renaming one is a standard-library break, so the
/// multi-result shapes are pinned.
#[test]
fn result_records_keep_their_labels() {
    let store = sys_io();
    let labels = |name: &str| -> Vec<String> {
        store
            .get(name)
            .unwrap_or_else(|| panic!("sys_io lacks {name}"))
            .signature
            .results
            .iter()
            .map(|(label, _)| label.clone())
            .collect()
    };

    assert_eq!(labels("io_read"), ["status", "bytes"]);
    assert_eq!(labels("io_write"), ["status", "written"]);
    assert_eq!(labels("io_open"), ["status", "handle"]);
    assert_eq!(labels("io_lookup"), ["status", "handle"]);
    assert_eq!(labels("io_resolve"), ["status", "addresses"]);
    assert_eq!(labels("io_socket"), ["status", "handle"]);
    assert_eq!(labels("io_accept"), ["status", "handle"]);
    assert_eq!(labels("io_tls_server_config"), ["status", "handle"]);
    assert_eq!(labels("io_clock_wall"), ["secs_hi", "secs_lo", "nanos"]);
    assert_eq!(labels("io_clock_mono"), ["secs", "nanos"]);
    assert_eq!(labels("io_env"), ["status", "value"]);
}

/// Every signature is well-formed: single results ride a name too (the guest
/// type is the bare wire type, but the printer uses the label), and parameter
/// names are unique within a signature.
#[test]
fn signatures_are_well_formed() {
    for function in sys_io().iter() {
        let signature = &function.signature;

        let params: BTreeSet<_> = signature.params.iter().map(|(name, _)| name).collect();
        assert_eq!(
            params.len(),
            signature.params.len(),
            "{} repeats a parameter name",
            function.name
        );

        for (_, type_) in signature.params.iter().chain(&signature.results) {
            if let WireType::Lst(element) = type_ {
                assert!(
                    !matches!(**element, WireType::Lst(_)),
                    "{} nests Lst — no host op does, and codegen's uniform Lst \
                     load would not distinguish the layers",
                    function.name
                );
            }
        }
    }
}

/// The import name is the identity every stage links on, so a second row with
/// the same name is a construction bug.
#[test]
#[should_panic(expected = "already registered")]
fn register_rejects_a_duplicate_name() {
    let mut store = sys_io();

    store.register(ForeignFunction {
        namespace: NAMESPACE_SYS,
        name: "io_read".to_string(),
        label: "read_again".to_string(),
        signature: WireSignature {
            params: vec![],
            results: vec![],
        },
    });
}

/// Every `sys_io` row is stamped with the `sys` wasm namespace — the fixed
/// substrate `emit_sys_imports` reads instead of re-deriving membership by
/// rebuilding this same store.
#[test]
fn sys_io_rows_are_stamped_with_the_sys_namespace() {
    assert!(
        sys_io()
            .iter()
            .all(|function| function.namespace == NAMESPACE_SYS)
    );
}

/// Identity is the wasm import pair: `label` and `signature` don't participate
/// (a cached row matches a freshly minted one), but the namespace does — one
/// import name under `sys` and under `ffi` names two different imports.
#[test]
fn equality_is_the_import_pair() {
    let base = |namespace, label: &str| ForeignFunction {
        namespace,
        name: "frobnicate".to_string(),
        label: label.to_string(),
        signature: WireSignature {
            params: vec![],
            results: vec![],
        },
    };

    assert_eq!(
        base(NAMESPACE_FFI, "frobnicate"),
        base(NAMESPACE_FFI, "frobnicate_again")
    );
    assert_ne!(
        base(NAMESPACE_SYS, "frobnicate"),
        base(NAMESPACE_FFI, "frobnicate")
    );
}
