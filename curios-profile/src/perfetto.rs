//! Perfetto's trace messages, as the build script generates them from Perfetto's schema at the revision it pins: only the messages and the fields of them the recorder writes and reads, which `build.rs` names, each number and type Perfetto's own.
#![expect(
    unnameable_types,
    reason = "prost's `Enumeration` derive implements public traits, `TryFrom<i32>` among them, for each generated enum, which reaches it from outside the crate though this module is private; prost writes no narrower visibility, and a narrower one written over its output is what clippy's `redundant_pub_crate` refuses in a private module"
)]
#![expect(
    clippy::enum_variant_names,
    reason = "the variants are Perfetto's names, `DebugAnnotation`'s `bool_value`, `uint_value` and `int_value` and `TracePacket`'s `SEQ_` flags, which prost spells as written; renamed here, they would stop reading as the schema a reader looks them up in"
)]

include!(concat!(env!("OUT_DIR"), "/perfetto.protos.rs"));
