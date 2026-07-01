use {
    super::{HostFunction, WireType},
    std::collections::BTreeSet,
};

/// `ALL` mirrors declaration order, so `f as usize` indexes per-function
/// tables (the codegen symbol table relies on it).
#[test]
fn all_is_in_discriminant_order() {
    for (index, function) in HostFunction::ALL.into_iter().enumerate() {
        assert_eq!(function as usize, index, "{function:?} is out of order");
    }
}

/// The 24 `env` import names, byte for byte — the wire ABI contract between
/// the wasm emitter and the runtime linker. A mismatch here silently strands
/// an import, so the whole list is pinned.
#[test]
fn names_are_the_wire_abi() {
    let names = HostFunction::ALL.map(HostFunction::name);

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

#[test]
fn names_and_paths_are_unique() {
    let names: BTreeSet<_> = HostFunction::ALL.iter().map(|f| f.name()).collect();
    let paths: BTreeSet<_> = HostFunction::ALL.iter().map(|f| f.path()).collect();

    assert_eq!(names.len(), HostFunction::ALL.len());
    assert_eq!(paths.len(), HostFunction::ALL.len());
}

/// Result labels are the record fields the guest projects (`.status`,
/// `.secs_hi`, …) — renaming one is a standard-library break, so the
/// multi-result shapes are pinned.
#[test]
fn result_records_keep_their_labels() {
    let labels = |f: HostFunction| -> Vec<&'static str> {
        f.signature()
            .results
            .iter()
            .map(|(label, _)| *label)
            .collect()
    };

    assert_eq!(labels(HostFunction::Read), ["status", "bytes"]);
    assert_eq!(labels(HostFunction::Write), ["status", "written"]);
    assert_eq!(labels(HostFunction::Open), ["status", "handle"]);
    assert_eq!(labels(HostFunction::Lookup), ["status", "handle"]);
    assert_eq!(labels(HostFunction::Resolve), ["status", "addresses"]);
    assert_eq!(labels(HostFunction::Socket), ["status", "handle"]);
    assert_eq!(labels(HostFunction::Accept), ["status", "handle"]);
    assert_eq!(labels(HostFunction::TlsServerConfig), ["status", "handle"]);
    assert_eq!(
        labels(HostFunction::ClockWall),
        ["secs_hi", "secs_lo", "nanos"]
    );
    assert_eq!(labels(HostFunction::ClockMono), ["secs", "nanos"]);
    assert_eq!(labels(HostFunction::Env), ["status", "value"]);
}

/// Every signature is well-formed: single results ride a name too (the guest
/// type is the bare wire type, but the printer uses the label), and parameter
/// names are unique within a signature.
#[test]
fn signatures_are_well_formed() {
    for function in HostFunction::ALL {
        let signature = function.signature();

        let params: BTreeSet<_> = signature.params.iter().map(|(name, _)| *name).collect();
        assert_eq!(
            params.len(),
            signature.params.len(),
            "{function:?} repeats a parameter name"
        );

        for (_, type_) in signature.params.iter().chain(signature.results) {
            if let WireType::Arr(element) = type_ {
                assert!(
                    !matches!(element, WireType::Arr(_)),
                    "{function:?} nests Arr — no host op does, and codegen's \
                     uniform Arr load would not distinguish the layers"
                );
            }
        }
    }
}
