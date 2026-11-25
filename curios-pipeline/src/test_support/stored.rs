//! What a unit stores of each of its halves, which the suites comparing two stored units and the bench's measurement of a stored unit's division both read.

use {
    curios_elab::{TraceTable, Traces},
    curios_erase::{AskedItems, AskedTable},
    curios_module::PreparedText,
};

/// What a unit stores of the lowering's baseline half `text` holds: its own part, then each record and scan, each stored alone.
pub fn lowering_stored(text: &PreparedText) -> Vec<u8> {
    let mut bytes = text.baseline_table().expect("a baseline half serializes");
    for blob in text
        .baseline_blobs(&|_| true)
        .expect("a baseline half serializes")
    {
        bytes.extend_from_slice(blob.bytes.as_slice());
    }

    bytes
}

/// What a unit stores of `traces`: the rest of them, then each declaration's trace, all in one run.
pub fn traces_stored(traces: &Traces) -> Vec<u8> {
    let mut bytes = curios_archive::to_bytes(traces)
        .expect("a unit's traces serialize")
        .to_vec();
    bytes.extend(
        TraceTable::run(
            traces
                .declarations()
                .map(|(name, held)| (name, &***held, held.concluded(), held.made())),
        )
        .expect("a unit's traces serialize"),
    );

    bytes
}

/// What a unit stores of `asked`, what erasing its items read: the rest of it, then each item's record, all in one run.
pub fn asked_stored(asked: &AskedItems) -> Vec<u8> {
    let mut bytes = curios_archive::to_bytes(asked)
        .expect("what erasing a unit read serializes")
        .to_vec();
    bytes.extend(
        AskedTable::run(
            asked
                .records()
                .map(|(name, held)| (name, &***held, held.made())),
        )
        .expect("what erasing a unit read serializes"),
    );

    bytes
}
