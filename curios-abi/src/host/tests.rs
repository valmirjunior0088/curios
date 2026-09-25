use {
    super::{
        Check, ChildStream, ClosedCode, DeclaredForeign, Failure, FileKind, ForeignFunction,
        HostOp, Mode, Namespace, Outcome, Poll, Requirement, ResultShape, SerialFlow, SerialOp,
        SerialParity, StdioMode, WireReference, WireResults, WireSignature, WireType, host_ops,
    },
    crate::{event, status},
    std::{collections::BTreeSet, fmt::Debug},
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
        status::NOT_EMPTY,
        status::IS_DIRECTORY,
        status::NOT_DIRECTORY,
    ];

    assert!(named.iter().all(|&code| code < status::OTHER_BASE));
    assert_eq!(Failure::Other(0).code(), status::OTHER_BASE);
}

/// The builtin import names, byte for byte and in declaration order — the wire ABI contract between the wasm emitter and the runtime linker. A mismatch here silently strands an import, so the whole list is pinned.
#[test]
fn names_are_the_wire_abi() {
    let names = host_ops()
        .iter()
        .map(|function| function.name().to_string())
        .collect::<Vec<_>>();

    assert_eq!(
        names,
        [
            "handle_read",
            "handle_write",
            "handle_flush",
            "file_open",
            "dns_lookup",
            "dns_resolve",
            "socket_open",
            "socket_bind",
            "socket_connect",
            "socket_finish_connect",
            "socket_listen",
            "socket_accept",
            "tls_start",
            "tls_server_config",
            "tls_start_server",
            "socket_set_reuseaddr",
            "handle_poll",
            "handle_close",
            "clock_wall",
            "clock_mono",
            "rand_bytes",
            "proc_args",
            "proc_env",
            "proc_exit",
            "tty_raw",
            "tty_size",
            "serial_open",
            "serial_control",
            "file_stat",
            "file_remove",
            "file_rename",
            "dir_list",
            "dir_create",
            "dir_remove",
            "proc_cwd",
            "proc_spawn",
            "proc_stream",
            "proc_wait",
            "proc_kill",
        ]
    );
}

/// A row's `/sys` placement is its subject module plus its label, and two rows sharing one is a binding conflict the prelude only discovers when it is built. Labels alone are *not* unique — `file/open` and `socket/open` deliberately share a POSIX leaf name — so the pair is what this pins.
#[test]
fn placements_are_unique() {
    let store = host_ops();
    let placements: BTreeSet<_> = store
        .iter()
        .map(|function| (function.subject(), function.label()))
        .collect();

    assert_eq!(placements.len(), store.iter().count());
}

/// Every builtin states the `/sys` module it surfaces in: the prelude places rows by that column rather than by a list beside the table, so a row without one is a table it cannot read.
#[test]
fn ops_rows_name_a_subject() {
    assert!(
        host_ops()
            .iter()
            .all(|function| function.subject().is_some())
    );
}

/// Result labels are the record fields the guest projects (`.status`, `.secs`, …) — renaming one is a standard-library break, so the multi-result shapes are pinned.
#[test]
fn result_records_keep_their_labels() {
    let store = host_ops();
    let labels = |name: &str| -> Vec<String> {
        store
            .get(name)
            .unwrap_or_else(|| panic!("host_ops lacks {name}"))
            .signature()
            .results
            .iter()
            .map(|(label, _)| label.to_string())
            .collect()
    };

    assert_eq!(labels("handle_read"), ["status", "bytes"]);
    assert_eq!(labels("handle_write"), ["status", "written"]);
    assert_eq!(labels("file_open"), ["status", "handle"]);
    assert_eq!(labels("dns_lookup"), ["status", "handle"]);
    assert_eq!(labels("dns_resolve"), ["status", "addresses"]);
    assert_eq!(labels("socket_open"), ["status", "handle"]);
    assert_eq!(labels("socket_accept"), ["status", "handle"]);
    assert_eq!(labels("tls_server_config"), ["status", "handle"]);
    assert_eq!(labels("clock_wall"), ["secs", "nanos"]);
    assert_eq!(labels("clock_mono"), ["secs", "nanos"]);
    assert_eq!(labels("proc_env"), ["status", "value"]);
    assert_eq!(labels("tty_size"), ["status", "cols", "rows"]);
    assert_eq!(labels("serial_open"), ["status", "handle"]);
    assert_eq!(
        labels("file_stat"),
        ["status", "kind", "size", "mtime_secs", "mtime_nanos"]
    );
    assert_eq!(labels("dir_list"), ["status", "names"]);
    assert_eq!(labels("proc_cwd"), ["status", "path"]);
    assert_eq!(labels("proc_spawn"), ["status", "child"]);
    assert_eq!(labels("proc_stream"), ["status", "handle"]);
    assert_eq!(labels("proc_wait"), ["status", "code", "signal"]);
}

/// Every signature is well-formed: single results ride a name too (the guest type is the bare wire type, but the printer uses the label), and parameter names are unique within a signature. Nothing asserts that `List` does not nest, nor that a reference result comes last — [`WireLeaf`](super::WireLeaf) and [`WireResults`] make both unrepresentable.
#[test]
fn signatures_are_well_formed() {
    for function in host_ops().iter() {
        let signature = function.signature();

        let params: BTreeSet<_> = signature.params.iter().map(|(name, _)| name).collect();
        assert_eq!(
            params.len(),
            signature.params.len(),
            "{} repeats a parameter name",
            function.name()
        );
    }
}

/// A single result is well-formed whatever its type, and results cross scalars first and the reference last — the order a user `foreign` declaration's one result and the table's rows both read back in.
#[test]
fn results_cross_scalars_first_and_the_reference_last() {
    let single = WireResults::single("_".to_string(), WireType::Bytes);
    assert_eq!(single.len(), 1);
    assert_eq!(single.reference(), Some(("_", WireReference::Bytes)));
    assert_eq!(single.iter().collect::<Vec<_>>(), [("_", WireType::Bytes)]);

    let store = host_ops();
    let read = store.get("handle_read").expect("host_ops defines read");
    assert_eq!(
        read.signature().results.iter().collect::<Vec<_>>(),
        [("status", WireType::Nat), ("bytes", WireType::Bytes)]
    );
    assert_eq!(
        read.signature().results.reference(),
        Some(("bytes", WireReference::Bytes))
    );
    assert!(
        store
            .get("clock_wall")
            .expect("host_ops defines clock_wall")
            .signature()
            .results
            .reference()
            .is_none()
    );
}

/// The guest-facing shape is read off the count and nothing else: `handle_close` answers the unit value, `rand_bytes` its bytes bare, `handle_read` a record of its two labelled results.
#[test]
fn the_guest_shape_is_read_off_the_result_count() {
    let store = host_ops();
    let shape = |name: &str| {
        store
            .get(name)
            .expect("a host_ops row")
            .signature()
            .results
            .shape()
    };

    assert_eq!(shape("handle_close"), ResultShape::Unit);
    assert_eq!(shape("rand_bytes"), ResultShape::Single(WireType::Bytes));
    assert_eq!(
        shape("handle_read"),
        ResultShape::Record(vec![("status", WireType::Nat), ("bytes", WireType::Bytes)])
    );
}

/// The import name is the identity every stage links on, so a second row with the same name is a construction bug.
#[test]
#[should_panic(expected = "already registered")]
fn register_rejects_a_duplicate_name() {
    let mut store = host_ops();

    store.register(ForeignFunction::Builtin(HostOp::HandleRead));
}

/// Every `host_ops` row is stamped with the `sys` wasm namespace — the fixed substrate `emit_sys_imports` reads instead of re-deriving membership by rebuilding this same store.
#[test]
fn ops_rows_are_stamped_with_the_sys_namespace() {
    assert!(
        host_ops()
            .iter()
            .all(|function| function.namespace() == Namespace::Sys)
    );
}

/// A builtin is its variant and a declared row its import name: neither a declared row's label nor its signature participates, and a declared row never equals the builtin whose wire name it happens to spell, since the two import under different namespaces.
#[test]
fn a_builtin_is_its_variant_and_a_declared_row_its_name() {
    let declared = |name: &str, label: &str| {
        ForeignFunction::Declared(DeclaredForeign {
            name: name.to_string(),
            label: label.to_string(),
            signature: WireSignature {
                params: vec![],
                results: WireResults::none(),
            },
        })
    };
    let read = HostOp::HandleRead;

    assert_eq!(
        declared("/frobnicate", "frobnicate"),
        declared("/frobnicate", "frobnicate_again")
    );
    assert_eq!(
        ForeignFunction::Builtin(read),
        ForeignFunction::Builtin(read)
    );
    assert_ne!(
        ForeignFunction::Builtin(read),
        declared("handle_read", "read")
    );
}

/// Every variant names the row it was declared with, so a builtin read back through its identity is the row the table states.
#[test]
fn a_builtin_reads_back_the_row_it_names() {
    for &op in HostOp::ALL {
        assert_eq!(HostOp::named(op.name()), Some(op));
        assert_eq!(
            op.name(),
            format!("{}_{}", op.subject().to_lowercase(), op.label())
        );
    }
}

/// A row's wire name is its placement spelled flat — the subject lowercased, an underscore, the label — so no two rows sharing a label contend for one import name, and a new row cannot choose a name beside where it is placed.
#[test]
fn a_wire_name_is_its_placement_spelled_flat() {
    for function in host_ops().iter() {
        let subject = function
            .subject()
            .expect("every builtin row names its subject");

        assert_eq!(
            function.name(),
            format!("{}_{}", subject.to_lowercase(), function.label()),
            "the row placed at {subject}/{}",
            function.label(),
        );
    }
}

/// `proc_exit` is the one row that diverges, and it crosses a `Byte` out and nothing back: its call never returns, which is not the same as a row returning nothing.
#[test]
fn exit_is_the_one_row_that_diverges() {
    let diverging = HostOp::ALL
        .iter()
        .filter(|op| op.diverges())
        .collect::<Vec<_>>();
    assert_eq!(
        diverging.iter().map(|op| op.name()).collect::<Vec<_>>(),
        ["proc_exit"]
    );

    let exit = diverging[0].signature();
    assert_eq!(exit.params, [("code".to_string(), WireType::Byte)]);
    assert!(exit.results.is_empty());
    assert!(!HostOp::HandleClose.diverges());
}

/// A variant is its row's wire name in CamelCase — the one spelling the table writes twice, since a macro cannot change an identifier's case — so a row whose two names disagree fails here rather than reading as a different operation.
#[test]
fn a_variant_is_its_wire_name_in_camel_case() {
    for &op in HostOp::ALL {
        let camel = op
            .name()
            .split('_')
            .map(|part| part[..1].to_uppercase() + &part[1..])
            .collect::<String>();

        assert_eq!(format!("{op:?}"), camel);
    }
}

/// A row's outcome is its reply type's: a stream, a lookup, a fallible call, a plain value and a divergence, each read off the Rust type the row returns rather than stated beside it.
#[test]
fn outcomes_are_read_off_the_reply_types() {
    assert_eq!(HostOp::HandleRead.outcome(), Outcome::Stream);
    assert_eq!(HostOp::ProcEnv.outcome(), Outcome::Lookup);
    assert_eq!(HostOp::FileOpen.outcome(), Outcome::Fallible);
    assert_eq!(HostOp::SocketBind.outcome(), Outcome::Fallible);
    assert_eq!(HostOp::HandleClose.outcome(), Outcome::Returns);
    assert_eq!(HostOp::ClockWall.outcome(), Outcome::Returns);
    assert_eq!(HostOp::ProcExit.outcome(), Outcome::Diverges);
}

/// A check reads its row by name, and the evaluator trusts the table that the name is there and of the shape it reads: a request or a buffer is a measured operand, a row's own check reads its one payload, and a field check a result of the type it compares.
#[test]
fn every_check_reads_what_its_row_has() {
    let is_list = |wire_type: WireType| matches!(wire_type, WireType::Bytes | WireType::List(_));

    for &op in HostOp::ALL {
        let signature = op.signature();
        let operand = |name: &str| {
            signature
                .params
                .iter()
                .find(|(param, _)| param == name)
                .map(|(_, wire_type)| *wire_type)
                .unwrap_or_else(|| panic!("{op:?} checks an operand `{name}` it does not take"))
        };
        let result = |name: &str| {
            signature
                .results
                .iter()
                .find(|(label, _)| *label == name)
                .map(|(_, wire_type)| wire_type)
                .unwrap_or_else(|| panic!("{op:?} checks a field `{name}` it does not answer"))
        };
        // A row's own checks read its one payload: every result but the status.
        let payload = || {
            let mut results = signature.results.iter().map(|(_, wire_type)| wire_type);
            let payload = match op.outcome() {
                Outcome::Returns => results.collect::<Vec<_>>(),
                _ => results.by_ref().skip(1).collect(),
            };
            assert_eq!(payload.len(), 1, "{op:?} checks a payload of one value");
            payload[0]
        };

        for check in op.checks() {
            match *check {
                Check::Progress { request } | Check::Exact { request } => {
                    assert_eq!(operand(request), WireType::Nat, "{op:?}");
                    assert_eq!(payload(), WireType::Bytes, "{op:?}");
                }
                Check::Accepted { buffer } => {
                    assert_eq!(operand(buffer), WireType::Bytes, "{op:?}");
                    assert_eq!(payload(), WireType::Nat, "{op:?}");
                }
                Check::Parallel { list } => {
                    assert!(is_list(operand(list)), "{op:?}");
                    assert!(is_list(payload()), "{op:?}");
                }
                Check::NonEmpty => assert!(is_list(payload()), "{op:?}"),
                Check::Present { field } => assert_eq!(result(field), WireType::Handle, "{op:?}"),
                Check::Mask { field, .. } => assert_eq!(result(field), WireType::Bytes, "{op:?}"),
                Check::Below { field, .. } | Check::Code { field, .. } => {
                    assert_eq!(result(field), WireType::Nat, "{op:?}");
                }
                Check::Exit { code, signal } => {
                    assert_eq!(result(code), WireType::Nat, "{op:?}");
                    assert_eq!(result(signal), WireType::Nat, "{op:?}");
                }
            }
        }

        for requirement in op.requirements() {
            match *requirement {
                Requirement::SameLength { a, b } => {
                    assert!(is_list(operand(a)) && is_list(operand(b)), "{op:?}");
                }
            }
        }
    }
}

/// `would_block` and `tls` are failures, so only a row that can fail is marked with either; a lookup answers only `ok` and `not_found`, and a plain value no status at all.
#[test]
fn only_a_row_that_can_fail_is_marked() {
    for &op in HostOp::ALL {
        if op.blocks() || op.tls() {
            assert!(
                matches!(op.outcome(), Outcome::Fallible | Outcome::Stream),
                "{op:?} is marked but cannot fail"
            );
        }
    }
}

/// A file kind's check admits every kind and no other code.
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
    round_trips::<ChildStream>();
    round_trips::<SerialParity>();
    round_trips::<SerialFlow>();
    round_trips::<SerialOp>();
}

/// A poll's interest is the bits a guest may ask for, and nothing a host alone reports.
#[test]
fn an_interest_outside_read_and_write_is_malformed() {
    assert!(Poll::interest(event::READ | event::WRITE).is_some());
    assert!(Poll::interest(event::ERR).is_none());
    assert!(Poll::interest(0b1_0000).is_none());
}
