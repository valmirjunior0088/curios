//! Fetches the pinned Perfetto trace schema and generates the messages the recorder writes and reads.
//!
//! The merged `perfetto_trace.proto` is kept in `.artifacts/<revision>/` beside this crate, outside Cargo's target tree, checked against its pinned SHA-256 whether downloaded or placed there by hand (`curios-download`). `protox` parses it, the descriptor is pruned to the messages, fields and enums [`KEPT`] names, and `prost-build` writes them into `OUT_DIR`, where `src/perfetto.rs` includes them: the schema is Perfetto's, and this crate states only which of it it uses.

use {
    curios_download::Download,
    prost_types::{DescriptorProto, FileDescriptorSet},
    std::{
        collections::BTreeSet,
        env,
        path::{Path, PathBuf},
    },
};

/// The Perfetto release the schema is taken at: the one whose `trace_processor_shell` Perfetto's own v59.1 tooling pins.
const REVISION: &str = "v58.2";

const SHA256: &str = "fc2a7f7a6836b85751468b1dd23d06bef4562f791915008ae915feee4e6868e8";

/// The package every message of the schema is declared in.
const PACKAGE: &str = "perfetto.protos";

/// One message the recorder uses: its name, the fields of it it writes or reads, and the enums declared inside it that those fields, or the recorder's constants, are spelled by.
struct Kept {
    message: &'static str,
    fields: &'static [&'static str],
    enums: &'static [&'static str],
}

/// Every message the recorder writes or reads, and of each only what it uses.
const KEPT: &[Kept] = &[
    Kept {
        message: "Trace",
        fields: &["packet"],
        enums: &[],
    },
    Kept {
        message: "TracePacket",
        fields: &[
            "timestamp",
            "trusted_packet_sequence_id",
            "track_event",
            "track_descriptor",
            "interned_data",
            "sequence_flags",
            "first_packet_on_sequence",
        ],
        enums: &["SequenceFlags"],
    },
    Kept {
        message: "TrackDescriptor",
        fields: &[
            "uuid",
            "parent_uuid",
            "name",
            "process",
            "thread",
            "counter",
        ],
        enums: &[],
    },
    Kept {
        message: "ProcessDescriptor",
        fields: &["pid", "process_name"],
        enums: &[],
    },
    Kept {
        message: "ThreadDescriptor",
        fields: &["pid", "tid", "thread_name"],
        enums: &[],
    },
    Kept {
        message: "CounterDescriptor",
        fields: &["unit"],
        enums: &["Unit"],
    },
    Kept {
        message: "TrackEvent",
        fields: &[
            "type",
            "track_uuid",
            "name_iid",
            "category_iids",
            "debug_annotations",
            "extra_counter_track_uuids",
            "extra_counter_values",
        ],
        enums: &["Type"],
    },
    Kept {
        message: "DebugAnnotation",
        fields: &[
            "name_iid",
            "bool_value",
            "uint_value",
            "int_value",
            "string_value_iid",
        ],
        enums: &[],
    },
    Kept {
        message: "InternedData",
        fields: &[
            "event_categories",
            "event_names",
            "debug_annotation_names",
            "debug_annotation_string_values",
        ],
        enums: &[],
    },
    Kept {
        message: "EventCategory",
        fields: &["iid", "name"],
        enums: &[],
    },
    Kept {
        message: "EventName",
        fields: &["iid", "name"],
        enums: &[],
    },
    Kept {
        message: "DebugAnnotationName",
        fields: &["iid", "name"],
        enums: &[],
    },
    Kept {
        message: "InternedString",
        fields: &["iid", "str"],
        enums: &[],
    },
];

fn main() {
    let manifest =
        PathBuf::from(env::var("CARGO_MANIFEST_DIR").expect("cargo sets the manifest directory"));
    let directory = manifest.join(".artifacts").join(REVISION);
    let place = directory.join("perfetto_trace.proto");
    let url = format!(
        "https://raw.githubusercontent.com/google/perfetto/{REVISION}/protos/perfetto/trace/perfetto_trace.proto"
    );
    Download {
        what: "the Perfetto trace schema curios-profile's messages are generated from",
        url: &url,
        sha256: SHA256,
        place: &place,
    }
    .kept();
    // The schema sits outside Cargo's target tree, so nothing else tells Cargo when it goes away.
    println!("cargo::rerun-if-changed={}", place.display());

    let parsed = protox::compile([&place], [&directory])
        .unwrap_or_else(|error| panic!("parse {}: {error}", place.display()));
    let out = PathBuf::from(env::var("OUT_DIR").expect("cargo sets the output directory"));
    prost_build::Config::new()
        .disable_comments(["."])
        .out_dir(&out)
        .compile_fds(pruned(parsed, &place))
        .unwrap_or_else(|error| panic!("generate the trace messages: {error}"));
}

/// `parsed` with only what [`KEPT`] names: each kept message's named fields, the oneofs those fields still fill and the enums it names; every other message, enum, service and extension dropped. A kept field whose type is no kept message or enum is refused here rather than left for `prost-build` to name a type it never writes.
fn pruned(mut parsed: FileDescriptorSet, place: &Path) -> FileDescriptorSet {
    let file = parsed
        .file
        .iter_mut()
        .find(|file| file.package() == PACKAGE)
        .unwrap_or_else(|| panic!("{} declares no package `{PACKAGE}`", place.display()));

    file.message_type
        .retain(|message| KEPT.iter().any(|kept| kept.message == message.name()));
    file.enum_type.clear();
    file.service.clear();
    file.extension.clear();
    // Its locations are paths through the declarations as written, which pruning renumbers.
    file.source_code_info = None;
    for message in &mut file.message_type {
        let kept = KEPT
            .iter()
            .find(|kept| kept.message == message.name())
            .expect("a message retained is kept");
        narrowed(message, kept);
    }

    let types = file
        .message_type
        .iter()
        .flat_map(|message| {
            let name = message.name().to_string();
            message
                .enum_type
                .iter()
                .map(move |inner| format!(".{PACKAGE}.{name}.{}", inner.name()))
                .chain([format!(".{PACKAGE}.{}", message.name())])
        })
        .collect::<BTreeSet<_>>();
    for message in &file.message_type {
        for field in &message.field {
            if let Some(type_name) = &field.type_name
                && !types.contains(type_name)
            {
                panic!(
                    "{}.{} is a `{type_name}`, which KEPT does not keep",
                    message.name(),
                    field.name()
                );
            }
        }
    }

    parsed.file.retain(|file| file.package() == PACKAGE);
    parsed
}

/// `message` with only the fields, oneofs and enums `kept` names, nothing nested but those enums, its oneofs renumbered to the ones a kept field still fills.
fn narrowed(message: &mut DescriptorProto, kept: &Kept) {
    message
        .field
        .retain(|field| kept.fields.contains(&field.name()));
    for wanted in kept.fields {
        assert!(
            message.field.iter().any(|field| field.name() == *wanted),
            "{}.{wanted} is no field of Perfetto's schema at {REVISION}",
            kept.message
        );
    }
    message.nested_type.clear();
    message
        .enum_type
        .retain(|inner| kept.enums.contains(&inner.name()));
    message.extension.clear();
    message.extension_range.clear();

    let filled = message
        .field
        .iter()
        .filter_map(|field| field.oneof_index)
        .collect::<BTreeSet<_>>();
    let renumbered = filled.iter().copied().collect::<Vec<_>>();
    message.oneof_decl = renumbered
        .iter()
        .map(|&index| message.oneof_decl[index as usize].clone())
        .collect();
    for field in &mut message.field {
        if let Some(index) = field.oneof_index {
            field.oneof_index = Some(
                renumbered
                    .iter()
                    .position(|&filled| filled == index)
                    .expect("a filled oneof is renumbered") as i32,
            );
        }
    }
}
