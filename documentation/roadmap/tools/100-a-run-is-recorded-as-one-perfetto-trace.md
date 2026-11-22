# A run is recorded as one Perfetto trace

**Adopted by the user on 2026-11-22.** The compiler's account of its own work becomes one stream a run, in the format the standard tracing tools read: a summary, a timeline and an SQL query are readings of one file; each span, event and allocation is on the track of the thread it happened on; and a term a span carries reads as Core text, written once a trace. The format is [Perfetto's trace](https://perfetto.dev/docs/reference/trace-packet-proto), read in process by `curios profile`, by `trace_processor`'s SQL, and as a timeline by [Perfetto's UI](https://ui.perfetto.dev). `curios-profile` writes and reads it ([its README](../../../curios-profile/README.md#the-library-emits-records-and-aggregation-is-one-of-its-consumers)); what remains is below.

## What is wrong today

- **No step reads a stream by SQL.** A question past the summary is asked in Perfetto's hosted UI by hand; no step of the task files runs `trace_processor` over a stream, so no check holds the summary's totals to what Perfetto reads of the same file.

## Goal

- Every report of a stream is a reading of that file: `curios profile`'s summary, read in process; a query through `trace_processor`, pinned beside the compiler or in Perfetto's UI; and a timeline in Perfetto's UI.

## Settled

- **The format is Perfetto's protobuf trace, written by the recorder over types generated from Perfetto's schema**, at Perfetto's v58.2, the release whose `trace_processor_shell` Perfetto's v59.1 tooling pins.
- **The summary is read in process**: `trace_processor` answers what the summary does not, by SQL, and backs Perfetto's UI.
- **Perfetto's hosted UI is the timeline**: it opens a trace in the browser without sending it anywhere, and keeps itself offline after one visit by its service worker.

**Rejected.** A build of Perfetto's UI pinned and served, a copy of what the hosted UI keeps.

## Stages

**1. The reader beside the compiler.** `trace_processor_shell` pinned among `xtask/tasks.toml`'s `programs` by a minimum version, as Node is, since Perfetto publishes it a platform at a time, Linux's and macOS's alike: launched from the path, refused where it is absent, and fetched by no step; `cargo xtask profile query <STREAM> <SQL>`; and `curios-profile`'s *Taking a profile* naming the step and where Perfetto publishes each platform's binary. Check: the query step answers a total the summary states, alike, over the prelude's stream; and what `trace_processor` makes of a stream cut inside a packet, stated there.

## Verification and retirement

Each stage's check, then `cargo xtask workspace lint`, the suite and `cargo xtask docs build`, and the measurements that read a stream run by name. Once stage 1 lands, the decisions named above are restated, this file is deleted, and [A measurement files a record its figures are read from](../100-a-measurement-files-a-record-its-figures-are-read-from.md), which reads this trace, is released.
