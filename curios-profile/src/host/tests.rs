use {
    super::*,
    tracing_log::log::{Level, log},
};

#[test]
fn capture_host_records_filters_by_target_and_scopes_the_level() {
    let ((), records) = capture_host_records("probe::target", || {
        log!(target: "probe::target", Level::Trace, "Begin GC");
        log!(target: "probe::other", Level::Trace, "elsewhere");
        log!(target: "probe::target", Level::Trace, "new size is {:#x} bytes", 0x100000);
    });

    assert_eq!(records, ["Begin GC", "new size is 0x100000 bytes"]);

    // Outside a capture the max level is `Off`, so this reaches no logger and, with a fresh capture, leaves no residue.
    log!(target: "probe::target", Level::Trace, "between captures");

    let ((), records) = capture_host_records("probe::target", || {});
    assert!(records.is_empty());
}
