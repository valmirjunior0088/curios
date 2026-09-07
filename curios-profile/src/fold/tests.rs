//! What a fold recomputes from rows: the aggregates the old collector kept, the distributions beside them, and the two things only a stream can say — what a truncated file still yields, and what a killed run was inside.

use super::*;

fn folded(rows: &str) -> ProfileReport {
    fold(rows.as_bytes()).expect("rows fold")
}

// The round trip the whole design rests on: a stream written by `trace` folds back into the summaries the collector used to return, with the same nesting semantics — an outer span's extent covers the inner spans within it.
#[test]
fn a_written_stream_folds_back_into_its_summaries() {
    let buffer = std::sync::Arc::new(std::sync::Mutex::new(Vec::new()));
    let sink = buffer.clone();
    crate::trace(crate::Destination::Stream(Box::new(Sink(sink))), || {
        let _outer = tracing::trace_span!("outer").entered();
        for _ in 0..2 {
            let _inner = tracing::trace_span!("inner").entered();
        }
    })
    .expect("a stream destination opens");

    let rows = String::from_utf8(buffer.lock().expect("buffer lock").clone()).expect("utf-8");
    let report = folded(&rows);

    let outer = report
        .summaries
        .iter()
        .find(|summary| summary.name == "outer")
        .expect("the outer span");
    let inner = report
        .summaries
        .iter()
        .find(|summary| summary.name == "inner")
        .expect("the inner spans");

    assert_eq!(outer.calls, 1);
    assert_eq!(inner.calls, 2);
    assert!(outer.total >= inner.total, "{outer:?} {inner:?}");
}

struct Sink(std::sync::Arc<std::sync::Mutex<Vec<u8>>>);

impl std::io::Write for Sink {
    fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
        self.0.lock().expect("buffer lock").extend_from_slice(bytes);

        Ok(bytes.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

// A span's group is what its rows aggregate under, so two spans of one callsite with different groups are two rows rather than an average that hides which item cost the time.
#[test]
fn spans_of_one_callsite_aggregate_under_their_group() {
    let report = folded(
        "H\t1\t0\n\
         D\t0\tcurios_elab\tdeclaration\n\
         S\t1\t0\t0\tgroup=Nat/add\n\
         E\t1\t0\t0\t0\t0\t0\t0\n\
         X\t1\t0\t100\t0\t0\t0\t0\n\
         C\t1\t0\t100\n\
         S\t2\t0\t100\tgroup=Nat/mul\n\
         E\t2\t0\t100\t0\t0\t0\t0\n\
         X\t2\t0\t900\t0\t0\t0\t0\n\
         C\t2\t0\t900\n",
    );

    assert_eq!(report.summaries.len(), 2);
    assert_eq!(
        report.summaries[0].group.as_deref(),
        Some("Nat/mul"),
        "the costlier group sorts first"
    );
    assert_eq!(report.summaries[0].total, Duration::from_nanos(800));
    assert_eq!(report.summaries[1].total, Duration::from_nanos(100));
}

// The readings a boundary carries are differenced, so what a span retained is what it still held at its exit and what it allocated is everything it took while entered.
#[test]
fn the_boundary_readings_are_differenced_into_bytes() {
    let report = folded(
        "D\t0\tcurios_core\theld\n\
         S\t1\t0\t0\n\
         E\t1\t0\t0\t1000\t5000\t10\t1000\n\
         X\t1\t0\t500\t1400\t6000\t18\t2048\n\
         C\t1\t0\t500\n",
    );

    let summary = &report.summaries[0];
    assert_eq!(summary.retained, 400);
    assert_eq!(summary.allocated, 1000);
    assert_eq!(summary.allocations, 8);
    assert_eq!(report.peak, 2048);
}

// A magnitude site's distribution, which is the whole reason `sample!` exists beside a span: the count says how often and the spread says what it was handed.
#[test]
fn an_event_site_folds_into_its_distribution() {
    let report = folded(
        "D\t0\tcurios_elab\tuniverse::finalize_store_len\n\
         V\t0\t0\tvalue=3\n\
         V\t0\t1\tvalue=11\n\
         V\t0\t2\tvalue=1\n",
    );

    let sample = &report.samples[0];
    assert_eq!((sample.count, sample.total), (3, 15));
    assert_eq!((sample.min, sample.max), (1, 11));
    assert_eq!(sample.mean(), 5.0);
}

// A rotation discards the older file, so the surviving one opens in the middle of a span's life. Nothing is invented: an exit with no entry contributes no duration, and a span whose creation went with the discarded file is still named by the callsite its id carries.
#[test]
fn a_stream_that_opens_mid_life_folds_what_it_can() {
    let report = folded(
        "H\t1\t0\n\
         D\t0\tcurios_cert\tconvert::enter\n\
         X\t7\t0\t500\t0\t0\t0\t0\n\
         E\t9\t0\t600\t0\t0\t0\t0\n\
         X\t9\t0\t900\t0\t0\t0\t0\n\
         C\t9\t0\t900\n",
    );

    assert_eq!(report.summaries.len(), 1);
    let summary = &report.summaries[0];
    assert_eq!(summary.name, "convert::enter");
    assert_eq!(summary.calls, 1);
    assert_eq!(summary.total, Duration::from_nanos(300));
}

// The case the record stream exists for. A killed run leaves its spans open, and what they name is the stack it was inside — reported outermost first, which reads as the path down to whatever it could not finish.
#[test]
fn the_spans_a_killed_run_left_open_are_reported_outermost_first() {
    let report = folded(
        "D\t0\tcurios_pipeline\tcompile_entrypoint\n\
         D\t1\tcurios_elab\telaborate_module_item\n\
         D\t2\tcurios_elab\tuniverse::finalize\n\
         S\t1\t0\t0\n\
         E\t1\t0\t10\t0\t0\t0\t0\n\
         S\t2\t1\t20\tgroup=Nat/add\n\
         E\t2\t1\t30\t0\t0\t0\t0\n\
         S\t3\t2\t40\n\
         E\t3\t2\t50\t0\t0\t0\t0\n",
    );

    assert!(report.summaries.is_empty(), "nothing closed");
    let open = report
        .open
        .iter()
        .map(|span| span.name.as_str())
        .collect::<Vec<_>>();
    assert_eq!(
        open,
        vec![
            "compile_entrypoint",
            "elaborate_module_item",
            "universe::finalize"
        ]
    );
    assert_eq!(report.open[1].group.as_deref(), Some("Nat/add"));
    assert_eq!(report.open[2].depth, 1);
}

// A span re-entered within itself is measured over each extent rather than the outermost, which is why an entry is a stack and not a single reading.
#[test]
fn a_span_re_entered_within_itself_measures_each_extent() {
    let report = folded(
        "D\t0\tcurios_core\tnat::sum\n\
         S\t1\t0\t0\n\
         E\t1\t0\t0\t0\t0\t0\t0\n\
         E\t1\t0\t100\t0\t0\t0\t0\n\
         X\t1\t0\t200\t0\t0\t0\t0\n\
         X\t1\t0\t400\t0\t0\t0\t0\n\
         C\t1\t0\t400\n",
    );

    // The inner extent is 100 and the outer 400; both are counted, which is the same nesting rule an outer stage's row already follows.
    assert_eq!(report.summaries[0].total, Duration::from_nanos(500));
}

// A row cut short by a kill is the normal ending of a stream, not a corruption, so a partial last line is skipped and everything before it still folds.
#[test]
fn a_row_cut_short_by_a_kill_is_skipped() {
    let report = folded(
        "D\t0\tcurios_core\tnat::sum\n\
         S\t1\t0\t0\n\
         E\t1\t0\t0\t0\t0\t0\t0\n\
         X\t1\t0\t900\t0\t0\t0\t0\n\
         C\t1\t0\t900\n\
         V\t0\t9",
    );

    assert_eq!(report.summaries[0].calls, 1);
    assert!(report.samples.is_empty());
}

// The writer's escaping is undone, so a group holding a tab reads back as the string the call site passed rather than as extra columns.
#[test]
fn an_escaped_field_reads_back_as_it_was_written() {
    let report = folded(
        "D\t0\tcurios_elab\tdeclaration\n\
         S\t1\t0\t0\tgroup=a\\tb\\nc\\\\d\n\
         E\t1\t0\t0\t0\t0\t0\t0\n\
         X\t1\t0\t1\t0\t0\t0\t0\n\
         C\t1\t0\t1\n",
    );

    assert_eq!(report.summaries[0].group.as_deref(), Some("a\tb\nc\\d"));
}
