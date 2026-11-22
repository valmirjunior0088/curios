# A run is recorded as one Perfetto trace

**Adopted by the user on 2026-11-22.** The compiler's account of its own work becomes one stream a run, in the format the standard tracing tools read: a summary, a timeline and an SQL query are readings of one file; each span, event and allocation is on the track of the thread it happened on; and a term a span carries reads as Core text, written once a trace. The format is [Perfetto's trace](https://perfetto.dev/docs/reference/trace-packet-proto), read in process by `curios profile`, by `trace_processor`'s SQL, and as a timeline by [Perfetto's UI](https://ui.perfetto.dev). `curios-profile` writes and reads it ([its README](../../../curios-profile/README.md#the-library-emits-records-and-aggregation-is-one-of-its-consumers)); what remains is below.

## What is wrong today

- **The summary answers as no other command does.** `curios profile` prints up to five tab-separated tables, each under a header row, with the run's peak and each table's name in header cells. A field with nothing in it is left blank. Memory is in mebibytes to a tenth, so a span's five allocations read `0.0`. The stack a killed run stopped inside is the last table. [A command narrates on one transcript and reports in blocks](../../design/tools/a-command-narrates-on-one-transcript-and-reports-in-blocks.md) gives every other answer a block, then one listing of rows with no header, `-` where a field has nothing, each row splitting alike.
- **No step reads a stream by SQL.** A question past the summary is asked in Perfetto's hosted UI by hand; no step of the task files runs `trace_processor` over a stream, so no check holds the summary's totals to what Perfetto reads of the same file.

## Goal

- Every report of a stream is a reading of that file: `curios profile`'s answer, read in process and drawn as every answer of the command line is, a block of what the stream states of its run and then one view's rows; a query through `trace_processor`, pinned beside the compiler or in Perfetto's UI; and a timeline in Perfetto's UI.

## Settled

- **The format is Perfetto's protobuf trace, written by the recorder over types generated from Perfetto's schema**, at Perfetto's v58.2, the release whose `trace_processor_shell` Perfetto's v59.1 tooling pins.
- **The summary is read in process**: `trace_processor` answers what the summary does not, by SQL, and backs Perfetto's UI.
- **Perfetto's hosted UI is the timeline**: it opens a trace in the browser without sending it anywhere, and keeps itself offline after one visit by its service worker.

**Rejected.** A build of Perfetto's UI pinned and served, a copy of what the hosted UI keeps.

## Stages

**1. The summary is an answer.** `curios profile <PATH>` answers with a block on the path asked. Its facts: whether the run returned or stopped, and whether the allocator counted; the peak; and the rows each other view holds, where it holds any, with a `try;` entry naming the command that lists them. For a run that stopped, each span it stopped inside is an entry, each thread's stack in turn and innermost last, with when it was entered. The block closes on `[trace]` and the path, a kind the decision gains. After a blank line comes one listing: the spans by default, or `--slowest`, `--samples` or `--within`, one at most. Its rows have no header and `-` where a field has nothing; their fields are named in `usage.md`, times in milliseconds and memory in bytes. The measurements that file a fold beside their stream file the default answer. Check: a stopped run's stack reads as its entries; each view's rows split into as many fields as every other row of it; `usage.md`, the decision and the READMEs that quote the summary restated.

**2. The reader beside the compiler.** `trace_processor_shell` pinned among `xtask/tasks.toml`'s `programs` by a minimum version, as Node is, since Perfetto publishes it a platform at a time, Linux's and macOS's alike: launched from the path, refused where it is absent, and fetched by no step; `cargo xtask profile query <STREAM> <SQL>`; and `curios-profile`'s *Taking a profile* naming the step and where Perfetto publishes each platform's binary. Check: the query step answers a total the summary states, alike, over the prelude's stream; and what `trace_processor` makes of a stream cut inside a packet, stated there.

## Verification and retirement

Each stage's check, then `cargo xtask workspace lint`, the suite and `cargo xtask docs build`, and the measurements that read a stream run by name. Once stage 2 lands, the decisions named above are restated, this file is deleted, and [A measurement files a record its figures are read from](../100-a-measurement-files-a-record-its-figures-are-read-from.md), which reads this trace, is released.
