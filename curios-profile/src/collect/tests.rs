use super::*;

fn outer() {
    let _span = tracing::trace_span!("outer").entered();
    inner();
    inner();
}

fn inner() {
    let _span = tracing::trace_span!("inner").entered();
}

// Falsifiable only because this crate's test binary installs `CountingAllocator` — see `count.rs`; on the system allocator every reading is zero and both assertions hold whatever the accounting does. What they pin is its shape rather than concrete byte counts: a span that holds an allocation to its exit retains, and one that drops it does not. The megabyte is the margin — the counters are process-wide, so a parallel test thread's kilobytes are noise against it.
#[test]
fn capture_accounts_retained_and_allocated_bytes() {
    let (_, report) = capture(|| {
        let held = {
            let _span = tracing::trace_span!("holds").entered();
            vec![0_u8; 4 * 1024 * 1024]
        };

        {
            let _span = tracing::trace_span!("drops").entered();
            drop(vec![0_u8; 4 * 1024 * 1024]);
        }

        drop(held);
    });

    let holds = report
        .summaries
        .iter()
        .find(|summary| summary.name == "holds")
        .expect("the holding span was collected");
    let drops = report
        .summaries
        .iter()
        .find(|summary| summary.name == "drops")
        .expect("the dropping span was collected");

    assert!(holds.retained >= drops.retained);
    assert!(holds.allocated >= drops.retained.unsigned_abs());
}

#[test]
fn capture_aggregates_nested_profile_spans() {
    let (_, report) = capture(outer);

    let outer = report
        .summaries
        .iter()
        .find(|summary| summary.name == "outer")
        .expect("outer span was collected");
    let inner = report
        .summaries
        .iter()
        .find(|summary| summary.name == "inner")
        .expect("inner spans were collected");

    assert_eq!(outer.calls, 1);
    assert_eq!(outer.target, module_path!());
    assert_eq!(inner.calls, 2);
}
