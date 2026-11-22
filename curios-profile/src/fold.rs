//! Aggregation, as one consumer of the record stream rather than as what the library produces: [`fold`] reads the packets [`trace`](crate::trace()) wrote and recomputes timings, allocation figures and magnitude distributions from them, and [`Profile::answer`] summarizes them as `curios profile` answers.
//!
//! Every statistic here is derived, which is the point. A duration is a slice's end differenced against its beginning, a retained byte count the same subtraction over the allocator's readings, and a sample distribution a pass over the instants that carry a `value` — so a question the summary does not answer is asked of the file directly, by `trace_processor` or Perfetto's UI, rather than by changing what a capture keeps.
//!
//! **A packet is read with what its sequence stated before it.** Each packet sequence keeps its own state: the names and strings interned on it, its defaults — the track its slices are on and the counter tracks its readings are of — and its incremental clock, each timestamp on it a step from the one before. A counter written as steps is added to where its sequence last left it. A packet that clears its sequence's state starts all of it over.
//!
//! **A track is a thread, and each keeps a stack.** A slice begins and ends on its thread's track, and the slices of one track nest, so self time is each slice's extent less what the slices begun directly inside it on its track took. A call is a slice: a span entered twice is two. A slice's bytes and allocations are its own thread's, and what it retained is the process's.
//!
//! **A stream cut short reads up to its last whole packet.** A killed run's file, and one whose writer stopped at a failed write, end on a whole packet or inside one, and the packet cut short is dropped; the slices begun and never ended are reported in [`Profile::open`], which is what a killed run was inside.
//!
//! **A packet that names what was never defined is refused.** An interned id, a track, a counter or a clock a packet uses before a packet of the stream defined it is a stream that is not this recorder's, or not whole from its first packet, and reading it as one would invent names; `trace_processor` rests on the same order.
//!
//! **Each destination has its reader.** [`fold_at`] is [`Destination::File`](crate::Destination::File)'s: it takes the path that destination took and reads the file there. [`fold`] is [`Destination::Stream`](crate::Destination::Stream)'s, for bytes a caller already holds, such as a buffer folded back in the same process.

use {
    crate::{
        Counter,
        perfetto::{
            BuiltinClock, ClockSnapshot, TracePacket, TrackDescriptor, TrackEvent,
            debug_annotation, trace_packet, track_descriptor, track_event,
        },
    },
    curios_common::{About, Kind, Report, Said, Style},
    prost::Message,
    std::{
        cmp::Reverse,
        collections::{BTreeMap, HashMap},
        fmt,
        fs::File,
        io::{self, Read},
        path::Path,
        time::Duration,
    },
};

/// Timings, allocation figures and magnitude samples recomputed from one record stream.
#[derive(Debug, Default)]
pub struct Profile {
    /// Timings ordered from greatest to least total duration.
    pub summaries: Vec<Summary>,
    /// Sampled magnitudes ordered from greatest to least total, one entry per `curios_common::sample!` site that fired.
    pub samples: Vec<SampleSummary>,
    /// The greatest peak any slice carried: the high-water mark of what the process held while counting, up to the last packet the file holds; `None` where the allocator did not count.
    pub peak: Option<usize>,
    /// How many threads recorded, a track each.
    pub threads: usize,
    /// The spans still entered when the packets ran out: each thread's in turn, in the order the threads first recorded, and outermost first within each.
    ///
    /// Empty for a stream whose operation returned. Non-empty is the useful case: it is the stack a compilation was inside when it was killed, and the reason a record stream is worth keeping over an aggregate that only exists once everything has closed.
    pub open: Vec<OpenSpan>,
}

/// A span that was entered and never left before the packets ended.
#[derive(Debug)]
pub struct OpenSpan {
    /// The tracing target that owns the span.
    pub target: String,
    /// The static span name.
    pub name: String,
    /// The group the span declared, for one carrying a `group` field.
    pub group: Option<String>,
    /// The thread it was entered on, by the name its track gives it.
    pub thread: String,
    /// When it was entered, as nanoseconds since the stream opened.
    pub entered: u128,
}

/// The distribution of one `curios_common::sample!` site's observations.
///
/// Time and memory say an operation is expensive; these say *what it was given*. The pair is what distinguishes an operation that is individually wasteful from one that is being handed inputs it should never have seen — a distinction neither duration nor byte count can make alone.
#[derive(Debug)]
pub struct SampleSummary {
    /// The module that recorded the observations.
    pub target: String,
    /// The sample site's name.
    pub name: String,
    /// How many observations were recorded.
    pub count: u64,
    /// Their sum, which is the total magnitude the site saw.
    pub total: u64,
    /// The smallest observation.
    pub min: u64,
    /// The largest observation. Read against [`mean`](Self::mean): a mean near the min with a far larger max is a tail, and a mean that tracks the max is uniform growth.
    pub max: u64,
}

impl SampleSummary {
    /// The mean observation, or zero when nothing was recorded.
    pub fn mean(&self) -> f64 {
        match self.count {
            0 => 0.0,
            count => self.total as f64 / count as f64,
        }
    }
}

/// Aggregate statistics for every span with the same target, name and group.
///
/// Every figure but the two *self* ones counts nested spans within the span's extent, exactly as [`total`](Self::total) does: an outer stage's retained bytes include what the passes inside it retained.
///
/// **The self figures are what the inclusive ones cannot be for mutually recursive work.** A judgment that re-enters itself through another — typing calls conversion, which types again — counts each nested extent once per enclosing entry, so its inclusive row overstates it and a row per judgment cannot be added up. [`self_total`](Self::self_total) and [`Memory::self_allocated`] subtract every span entered directly inside on its track, so each nanosecond and each byte is attributed to exactly one span, the innermost one entered, and the self rows of one stream partition what its spans cover.
#[derive(Debug)]
pub struct Summary {
    /// The tracing target that owns the span.
    pub target: String,
    /// The static span name.
    pub name: String,
    /// The group this row aggregates, for a span that declared one — the value of its `group` field. `None` for every span that declared none.
    pub group: Option<String>,
    /// How many slices the row aggregates: a span entered twice is two.
    pub calls: u64,
    /// Sum of the time for which the spans were entered.
    pub total: Duration,
    /// The part of [`total`](Self::total) no span entered inside these ones accounts for.
    pub self_total: Duration,
    /// Shortest slice.
    pub min: Duration,
    /// Longest slice.
    pub max: Duration,
    /// What the allocator counted while the spans were entered, `None` where it did not count: a stream recorded by a program that installed no `CountingAllocator` reads no memory, never nought.
    pub memory: Option<Memory>,
}

/// What the allocator counted over a row's spans: differences of the readings their slices carried.
#[derive(Debug, Clone, Copy, Default)]
pub struct Memory {
    /// Net bytes the process still held at exit: what it took minus what it returned while the spans were entered, on any thread, since a block one thread takes another may return. Negative where more was freed than taken, which is how a pass that consumes a representation reports itself.
    pub retained: i64,
    /// Bytes the spans' own thread took while they were entered, whether or not later returned. A pass that allocates heavily and frees as it goes shows large `allocated` beside near-zero [`retained`](Self::retained).
    pub allocated: u64,
    /// The part of [`allocated`](Self::allocated) no span entered inside these ones accounts for.
    pub self_allocated: u64,
    /// Trips the spans' own thread made through the allocator while they were entered. Divided into [`allocated`](Self::allocated) it gives the average request size, which is what separates a pass that wants fewer allocations from one that wants a smaller structure.
    pub allocations: u64,
}

/// Recompute a report from the bytes of one record stream.
///
/// The error is the reader's, or a stream's that is not this recorder's: a packet that is not `Trace`'s, or that names what no packet before it defined. A packet cut short at the end is the normal ending of a killed run or of a writer that stopped, and is dropped.
pub fn fold(mut stream: impl Read) -> io::Result<Profile> {
    let mut bytes = Vec::new();
    stream.read_to_end(&mut bytes)?;
    let mut state = Fold::default();
    for packet in packets(&bytes)? {
        state.packet(packet)?;
    }

    Ok(state.finish())
}

/// Recompute a report from the stream filed at `path`.
///
/// The mirror of [`Destination::File`](crate::Destination::File): that destination took a path, and reading its stream back takes the same one.
pub fn fold_at(path: &Path) -> io::Result<Profile> {
    fold(File::open(path)?)
}

/// How many span names a summary lists: the ones that took the most time themselves, as many as a reader takes in at a glance.
const SUMMARIZED: usize = 10;

/// One span name with every group of it folded in, as a summary ranks it: its time, its own share of the time in spans, its calls and what it took, and the costliest of its groups where it declared any.
struct Ranked<'a> {
    target: &'a str,
    name: &'a str,
    total: Duration,
    self_total: Duration,
    calls: u64,
    taken: Option<u64>,
    costliest: Option<(&'a str, Duration)>,
}

impl Profile {
    /// `curios profile`'s answer for the stream at `stream`, drawn for `style`: one block summarizing the run, closed on the stream.
    ///
    /// Its facts say whether every span closed or how many were still open, whether the allocator counted, the time spent in spans and the threads that spent it with the bytes they took, the peak, and how many span names the stream holds; a stopped run's open stacks follow, a fact a thread. Its entries are the `SUMMARIZED` span names that took the most time themselves, each with its share of the time in spans and the share so far: self time partitions what the spans cover, so the shares add up. Every other question is the trace's, asked in Perfetto's UI or by `trace_processor`.
    pub fn answer(&self, stream: &Path, style: &Style) -> String {
        let ranked = self.ranked();
        let spent = ranked
            .iter()
            .map(|ranked| ranked.self_total)
            .sum::<Duration>();
        let mut so_far = Duration::ZERO;
        let entries = ranked
            .iter()
            .take(SUMMARIZED)
            .map(|ranked| {
                so_far += ranked.self_total;
                ranked.entry(spent, so_far)
            })
            .collect::<Vec<_>>();

        format!(
            "{}\n",
            self.block(stream, &ranked, spent)
                .listing(Kind::Trace, style, &entries)
        )
    }

    /// Every span name with its groups folded in, by the time it took itself, costliest first.
    fn ranked(&self) -> Vec<Ranked<'_>> {
        let mut named = BTreeMap::<(&str, &str), Ranked<'_>>::new();
        for summary in &self.summaries {
            let ranked = named
                .entry((summary.target.as_str(), summary.name.as_str()))
                .or_insert_with(|| Ranked {
                    target: &summary.target,
                    name: &summary.name,
                    total: Duration::ZERO,
                    self_total: Duration::ZERO,
                    calls: 0,
                    taken: None,
                    costliest: None,
                });
            ranked.total += summary.total;
            ranked.self_total += summary.self_total;
            ranked.calls += summary.calls;
            if let Some(memory) = summary.memory {
                *ranked.taken.get_or_insert(0) += memory.self_allocated;
            }
            if let Some(group) = &summary.group
                && ranked
                    .costliest
                    .is_none_or(|(_, costliest)| summary.total > costliest)
            {
                ranked.costliest = Some((group.as_str(), summary.total));
            }
        }
        let mut ranked = named.into_values().collect::<Vec<_>>();
        ranked.sort_by_key(|ranked| Reverse(ranked.self_total));

        ranked
    }

    /// The block's facts, closed on `stream`.
    fn block(&self, stream: &Path, ranked: &[Ranked<'_>], spent: Duration) -> Report {
        let closed = match self.open.len() {
            0 => "every span closed".to_string(),
            1 => "1 span open".to_string(),
            open => format!("{open} spans open"),
        };
        let counted = match self.peak {
            Some(_) => "allocations counted",
            None => "allocations not counted",
        };
        let threads = match self.threads {
            1 => "1 thread".to_string(),
            threads => format!("{threads} threads"),
        };
        let taken = match ranked.iter().filter_map(|ranked| ranked.taken).sum::<u64>() {
            _ if self.peak.is_none() => String::new(),
            taken => format!(", {} taken", bytes(taken, 1)),
        };

        let mut said = Said::line(stream.display().to_string());
        said.stating(format!("{closed}, {counted}"));
        if !spent.is_zero() {
            said.stating(format!("in spans: {} on {threads}{taken}", time(spent, 1)));
        }
        if let Some(peak) = self.peak {
            said.stating(format!("peak: {}", bytes(peak as u64, 1)));
        }
        match ranked.len() {
            0 => {}
            names if names <= SUMMARIZED => {
                said.stating(format!("spans: {names} names, each below"));
            }
            names => said.stating(format!(
                "spans: {names} names, the {SUMMARIZED} that took the most time themselves below"
            )),
        }
        for (thread, open) in self.stacks() {
            let spans = open
                .iter()
                .map(|span| {
                    let group = match &span.group {
                        Some(group) => format!(", group {group}"),
                        None => String::new(),
                    };
                    format!(
                        "\n  {} in {}{group}, entered at {}",
                        span.name,
                        span.target,
                        time(Duration::from_nanos(span.entered as u64), 2)
                    )
                })
                .collect::<String>();
            said.stating(format!("open on {thread}:{spans}"));
        }

        Report::about(About::Path(stream.to_path_buf()), said)
    }

    /// Each thread's open spans, the threads in the order they first recorded.
    fn stacks(&self) -> Vec<(&str, Vec<&OpenSpan>)> {
        let mut stacks = Vec::<(&str, Vec<&OpenSpan>)>::new();
        for span in &self.open {
            match stacks.last_mut() {
                Some((thread, open)) if *thread == span.thread => open.push(span),
                _ => stacks.push((&span.thread, vec![span])),
            }
        }

        stacks
    }
}

impl Ranked<'_> {
    /// The span name as an entry of the summary: its name and target, then its figures, its share of `spent` and the share of the names ranked above it and it, `so_far`; and its costliest group where it declared any.
    fn entry(&self, spent: Duration, so_far: Duration) -> String {
        let share = |part: Duration| {
            100.0 * part.as_secs_f64() / spent.as_secs_f64().max(f64::MIN_POSITIVE)
        };
        let taken = match self.taken {
            Some(taken) => format!("; taken: {}", bytes(taken, 2)),
            None => String::new(),
        };
        let mut lines = vec![
            format!("{} in {}", self.name, self.target),
            format!(
                "self: {}, {:.1}%, {:.1}% so far; in all: {}; calls: {}{taken}",
                time(self.self_total, 2),
                share(self.self_total),
                share(so_far),
                time(self.total, 2),
                thousands(self.calls)
            ),
        ];
        if let Some((group, total)) = self.costliest {
            lines.push(format!("costliest group: {group}, {}", time(total, 2)));
        }

        lines.join("\n")
    }
}

/// `duration` in the largest of seconds, milliseconds and microseconds it reaches, to `decimals` places.
fn time(duration: Duration, decimals: usize) -> String {
    let seconds = duration.as_secs_f64();
    match seconds {
        _ if seconds >= 1.0 => format!("{seconds:.decimals$} s"),
        _ if seconds >= 1e-3 => format!("{:.decimals$} ms", seconds * 1e3),
        _ => format!("{:.decimals$} µs", seconds * 1e6),
    }
}

/// `count` bytes in the largest binary unit it reaches, to `decimals` places, or whole below a kibibyte.
fn bytes(count: u64, decimals: usize) -> String {
    let units = [("GiB", 1_u64 << 30), ("MiB", 1 << 20), ("KiB", 1 << 10)];
    match units.into_iter().find(|(_, size)| count >= *size) {
        Some((unit, size)) => format!("{:.decimals$} {unit}", count as f64 / size as f64),
        None => format!("{count} B"),
    }
}

/// `count` with a comma between each three digits.
fn thousands(count: u64) -> String {
    let digits = count.to_string();
    let mut grouped = String::with_capacity(digits.len() + digits.len() / 3);
    for (at, digit) in digits.chars().enumerate() {
        if at > 0 && (digits.len() - at).is_multiple_of(3) {
            grouped.push(',');
        }
        grouped.push(digit);
    }

    grouped
}

/// The field number and wire type of `Trace.packet`, which frames every packet of a stream.
const PACKET: u64 = (1 << 3) | 2;

/// Each whole packet of `bytes`, as `Trace` frames them; one cut short at the end is dropped.
fn packets(mut bytes: &[u8]) -> io::Result<Vec<TracePacket>> {
    let mut packets = Vec::new();
    while !bytes.is_empty() {
        let Ok(key) = prost::encoding::decode_varint(&mut bytes) else {
            break;
        };
        if key != PACKET {
            return Err(refused(format_args!(
                "a key of {key}, where a trace frames packets"
            )));
        }
        let Ok(length) = prost::encoding::decode_varint(&mut bytes) else {
            break;
        };
        let Some((whole, rest)) = usize::try_from(length)
            .ok()
            .and_then(|length| bytes.split_at_checked(length))
        else {
            break;
        };
        packets
            .push(TracePacket::decode(whole).map_err(|error| {
                refused(format_args!("a packet that does not decode: {error}"))
            })?);
        bytes = rest;
    }

    Ok(packets)
}

/// The refusal of a stream that is not this recorder's, saying what in it was not.
fn refused(what: fmt::Arguments<'_>) -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        format!("not a stream this recorder wrote: {what}"),
    )
}

/// The entry of `table` an id names, or the refusal of a packet that names what was never defined.
fn defined<'a>(table: &'a HashMap<u64, String>, id: u64, what: &str) -> io::Result<&'a String> {
    table
        .get(&id)
        .ok_or_else(|| refused(format_args!("{what} {id}, used before it was defined")))
}

/// A slice begun and not yet ended: its span's identity, the readings it began with, and what the slices begun directly inside it took — the part of its extent that is not its own.
struct Open {
    target: String,
    name: String,
    group: Option<String>,
    at: u64,
    readings: Option<Readings>,
    nested: u64,
    nested_allocated: u64,
}

#[derive(Clone, Copy)]
struct Readings {
    /// The bytes held since counting began, below zero where more was returned than taken since.
    live: i64,
    allocated: u64,
    allocations: u64,
    /// The high-water mark of what counting saw held, as of this slice, which is a reading of its own rather than the greatest `live` any slice happened to catch.
    peak: u64,
}

/// A thread's track: the thread's name, and the slices open on it, innermost last.
struct Track {
    thread: String,
    stack: Vec<Open>,
}

#[derive(Default)]
struct Aggregate {
    calls: u64,
    total: u128,
    own: u128,
    min: Option<u128>,
    max: u128,
    memory: Option<Memory>,
}

#[derive(Default)]
struct Distribution {
    count: u64,
    total: u64,
    min: Option<u64>,
    max: u64,
}

/// An annotation's value as a report reads it.
enum Annotated {
    Unsigned(u64),
    Signed(i64),
    Text(String),
}

impl Annotated {
    /// The magnitude a `value` states: a signed one below zero read as none of it.
    fn magnitude(&self) -> Option<u64> {
        match self {
            Self::Unsigned(value) => Some(*value),
            Self::Signed(value) => Some((*value).max(0) as u64),
            Self::Text(_) => None,
        }
    }
}

impl fmt::Display for Annotated {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unsigned(value) => write!(formatter, "{value}"),
            Self::Signed(value) => write!(formatter, "{value}"),
            Self::Text(text) => formatter.write_str(text),
        }
    }
}

/// A magnitude site as a report names it: the target that owns it, and its name.
type Named = (String, String);

/// What a packet sequence stated for the packets after it: the names and strings interned on it, its defaults, its incremental clock, and each counter it writes as steps as it last stood.
#[derive(Default)]
struct Sequence {
    names: HashMap<u64, String>,
    categories: HashMap<u64, String>,
    annotation_names: HashMap<u64, String>,
    strings: HashMap<u64, String>,
    /// The track its events are on.
    track: Option<u64>,
    /// The counter tracks its events' readings are of, in the order an event carries them.
    counters: Vec<u64>,
    /// The clock its packets are stamped on where a packet names none.
    clock: Option<u32>,
    steps: Option<Steps>,
    stepped: HashMap<u64, i64>,
}

/// A sequence's incremental clock: its id, and where its last packet stood on the stream's clock.
struct Steps {
    clock: u32,
    at: u64,
}

/// The stream's own clock, which a packet stamped on no clock is on.
const STREAM_CLOCK: u32 = BuiltinClock::Boottime as u32;

impl Sequence {
    /// Where a packet stamped `timestamp` on `clock` stands on the stream's clock: a step from the last packet where the clock is this sequence's incremental one, whole where it is the stream's.
    fn at(&mut self, timestamp: Option<u64>, clock: Option<u32>) -> io::Result<Option<u64>> {
        let Some(timestamp) = timestamp else {
            return Ok(None);
        };
        match clock.or(self.clock).unwrap_or(STREAM_CLOCK) {
            STREAM_CLOCK => Ok(Some(timestamp)),
            clock => match &mut self.steps {
                Some(steps) if steps.clock == clock => {
                    steps.at += timestamp;
                    Ok(Some(steps.at))
                }
                _ => Err(refused(format_args!(
                    "clock {clock}, used before it was defined"
                ))),
            },
        }
    }

    /// The incremental clock a snapshot declares, standing where the stream's clock stood when it was taken.
    fn clocked(&mut self, snapshot: &ClockSnapshot) -> io::Result<()> {
        let stream = snapshot
            .clocks
            .iter()
            .find(|clock| clock.clock_id() == STREAM_CLOCK);
        let stepped = snapshot.clocks.iter().find(|clock| clock.is_incremental());
        let (Some(stream), Some(stepped)) = (stream, stepped) else {
            return Err(refused(format_args!(
                "a clock snapshot that relates no incremental clock to the stream's"
            )));
        };
        self.steps = Some(Steps {
            clock: stepped.clock_id(),
            at: stream.timestamp(),
        });

        Ok(())
    }
}

/// A counter track the stream described: what it counts, and whether its values are steps.
struct Described {
    counter: Counter,
    stepped: bool,
}

#[derive(Default)]
struct Fold {
    sequences: HashMap<u32, Sequence>,
    tracks: BTreeMap<u64, Track>,
    counters: HashMap<u64, Described>,
    spans: BTreeMap<(String, String, Option<String>), Aggregate>,
    samples: BTreeMap<Named, Distribution>,
    peak: Option<u64>,
}

impl Fold {
    /// One packet, read with the state of the sequence it is on.
    fn packet(&mut self, packet: TracePacket) -> io::Result<()> {
        let id = match packet.optional_trusted_packet_sequence_id {
            Some(trace_packet::OptionalTrustedPacketSequenceId::TrustedPacketSequenceId(id)) => id,
            None => 0,
        };
        let mut sequence = self.sequences.remove(&id).unwrap_or_default();
        let read = self.read(&mut sequence, packet);
        self.sequences.insert(id, sequence);

        read
    }

    fn read(&mut self, sequence: &mut Sequence, packet: TracePacket) -> io::Result<()> {
        if packet.sequence_flags() & trace_packet::SequenceFlags::SeqIncrementalStateCleared as u32
            != 0
        {
            *sequence = Sequence::default();
        }
        if let Some(defaults) = &packet.trace_packet_defaults {
            let events = defaults.track_event_defaults.clone().unwrap_or_default();
            sequence.clock = defaults.timestamp_clock_id;
            sequence.track = events.track_uuid;
            sequence.counters = events.extra_counter_track_uuids;
        }
        if let Some(interned) = packet.interned_data {
            for name in interned.event_names {
                sequence.names.insert(name.iid(), name.name().to_string());
            }
            for category in interned.event_categories {
                sequence
                    .categories
                    .insert(category.iid(), category.name().to_string());
            }
            for name in interned.debug_annotation_names {
                sequence
                    .annotation_names
                    .insert(name.iid(), name.name().to_string());
            }
            for text in interned.debug_annotation_string_values {
                sequence
                    .strings
                    .insert(text.iid(), String::from_utf8_lossy(text.str()).into_owned());
            }
        }
        let at = sequence.at(packet.timestamp, packet.timestamp_clock_id)?;

        match packet.data {
            Some(trace_packet::Data::ClockSnapshot(snapshot)) => sequence.clocked(&snapshot),
            Some(trace_packet::Data::TrackDescriptor(track)) => self.described(track),
            Some(trace_packet::Data::TrackEvent(event)) => {
                let at = at.ok_or_else(|| refused(format_args!("an event with no timestamp")))?;
                self.event(sequence, at, event)
            }
            None => Ok(()),
        }
    }

    /// A track described: a thread's, which keeps a stack, or a counter's, known by its name.
    fn described(&mut self, track: TrackDescriptor) -> io::Result<()> {
        if let Some(thread) = &track.thread {
            let named = match &thread.thread_name {
                Some(name) => name.clone(),
                None => format!("thread {}", thread.tid()),
            };
            self.tracks.entry(track.uuid()).or_insert_with(|| Track {
                thread: named,
                stack: Vec::new(),
            });
        } else if let Some(counter) = &track.counter {
            let name = match &track.static_or_dynamic_name {
                Some(track_descriptor::StaticOrDynamicName::Name(name)) => name.as_str(),
                None => "",
            };
            let Some(counted) = Counter::ALL
                .into_iter()
                .find(|counted| counted.name() == name)
            else {
                return Err(refused(format_args!("a counter track named `{name}`")));
            };
            self.counters.insert(
                track.uuid(),
                Described {
                    counter: counted,
                    stepped: counter.is_incremental(),
                },
            );
        }

        Ok(())
    }

    fn event(&mut self, sequence: &mut Sequence, at: u64, event: TrackEvent) -> io::Result<()> {
        let Some(track) = sequence.track else {
            return Err(refused(format_args!(
                "an event on a sequence that names no track"
            )));
        };
        if !self.tracks.contains_key(&track) {
            return Err(refused(format_args!(
                "track {track}, used before it was described"
            )));
        }
        let Some(track_event::NameField::NameIid(name)) = event.name_field else {
            return Err(refused(format_args!("an event with no interned name")));
        };
        let name = defined(&sequence.names, name, "event name")?.clone();
        let target = match event.category_iids.first() {
            Some(&category) => defined(&sequence.categories, category, "category")?.clone(),
            None => String::new(),
        };
        let readings = self.readings(sequence, &event)?;
        if let Some(readings) = readings {
            self.peak = Some(self.peak.unwrap_or(0).max(readings.peak));
        }
        let annotations = Self::annotations(sequence, &event)?;

        match event.r#type() {
            track_event::Type::SliceBegin => {
                self.begin(track, at, target, name, annotations, readings);
            }
            track_event::Type::SliceEnd => self.end(track, at, readings),
            track_event::Type::Instant => self.instant(target, name, &annotations),
            _ => {}
        }

        Ok(())
    }

    /// The allocator's four readings a slice carries, `None` where it carries none: each value is of the counter track its sequence's defaults name in its place, and a counter's steps are added to where the sequence last left it.
    fn readings(
        &self,
        sequence: &mut Sequence,
        event: &TrackEvent,
    ) -> io::Result<Option<Readings>> {
        if event.extra_counter_values.is_empty() {
            return Ok(None);
        }
        if event.extra_counter_values.len() != sequence.counters.len() {
            return Err(refused(format_args!(
                "a slice carrying {} readings where its sequence names {} counter tracks",
                event.extra_counter_values.len(),
                sequence.counters.len()
            )));
        }
        let mut read = Vec::with_capacity(sequence.counters.len());
        for (&track, &value) in sequence.counters.iter().zip(&event.extra_counter_values) {
            let Some(described) = self.counters.get(&track) else {
                return Err(refused(format_args!(
                    "counter track {track}, used before it was described"
                )));
            };
            let value = match described.stepped {
                true => {
                    let last = sequence.stepped.entry(track).or_default();
                    *last += value;
                    *last
                }
                false => value,
            };
            read.push((described.counter, value));
        }
        let reading = |counter: Counter| {
            read.iter()
                .find(|(read, _)| *read == counter)
                .map(|&(_, value)| value)
                .ok_or_else(|| {
                    refused(format_args!(
                        "a slice carrying no reading of {}",
                        counter.name()
                    ))
                })
        };

        Ok(Some(Readings {
            live: reading(Counter::Live)?,
            allocated: reading(Counter::Allocated)? as u64,
            allocations: reading(Counter::Allocations)? as u64,
            peak: reading(Counter::Peak)? as u64,
        }))
    }

    /// A slice's annotations as names and values, each id read off what its sequence interned.
    fn annotations(
        sequence: &Sequence,
        event: &TrackEvent,
    ) -> io::Result<Vec<(String, Annotated)>> {
        event
            .debug_annotations
            .iter()
            .map(|annotation| {
                let Some(debug_annotation::NameField::NameIid(name)) = annotation.name_field else {
                    return Err(refused(format_args!("an annotation with no interned name")));
                };
                let name = defined(&sequence.annotation_names, name, "annotation name")?.clone();
                let value = match annotation.value {
                    Some(debug_annotation::Value::BoolValue(value)) => {
                        Annotated::Text(value.to_string())
                    }
                    Some(debug_annotation::Value::UintValue(value)) => Annotated::Unsigned(value),
                    Some(debug_annotation::Value::IntValue(value)) => Annotated::Signed(value),
                    Some(debug_annotation::Value::StringValueIid(text)) => {
                        Annotated::Text(defined(&sequence.strings, text, "string value")?.clone())
                    }
                    None => Annotated::Text(String::new()),
                };

                Ok((name, value))
            })
            .collect()
    }

    fn begin(
        &mut self,
        track: u64,
        at: u64,
        target: String,
        name: String,
        annotations: Vec<(String, Annotated)>,
        readings: Option<Readings>,
    ) {
        let group = annotations
            .into_iter()
            .find(|(field, _)| field == "group")
            .map(|(_, value)| value.to_string());
        let track = self
            .tracks
            .get_mut(&track)
            .expect("an event's track is described");
        track.stack.push(Open {
            target,
            name,
            group,
            at,
            readings,
            nested: 0,
            nested_allocated: 0,
        });
    }

    /// The innermost slice of `track` ended: its extent and readings differenced, credited to the slice it was begun inside, and aggregated under its row.
    fn end(&mut self, track: u64, at: u64, readings: Option<Readings>) {
        let track = self
            .tracks
            .get_mut(&track)
            .expect("an event's track is described");
        // An end with nothing begun on its track ends a slice whose beginning the stream does not hold, and is read as nothing.
        let Some(slice) = track.stack.pop() else {
            return;
        };

        let elapsed = at.saturating_sub(slice.at);
        let own = elapsed.saturating_sub(slice.nested);
        let memory = slice.readings.zip(readings).map(|(began, ended)| {
            let allocated = ended.allocated.saturating_sub(began.allocated);
            Memory {
                retained: ended.live - began.live,
                allocated,
                self_allocated: allocated.saturating_sub(slice.nested_allocated),
                allocations: ended.allocations.saturating_sub(began.allocations),
            }
        });
        if let Some(parent) = track.stack.last_mut() {
            parent.nested += elapsed;
            parent.nested_allocated += memory.map_or(0, |memory| memory.allocated);
        }

        let aggregate = self
            .spans
            .entry((slice.target, slice.name, slice.group))
            .or_default();
        let elapsed = u128::from(elapsed);
        aggregate.calls += 1;
        aggregate.total += elapsed;
        aggregate.own += u128::from(own);
        aggregate.min = Some(aggregate.min.map_or(elapsed, |least| least.min(elapsed)));
        aggregate.max = aggregate.max.max(elapsed);
        if let Some(memory) = memory {
            let sum = aggregate.memory.get_or_insert_default();
            sum.retained = sum.retained.saturating_add(memory.retained);
            sum.allocated = sum.allocated.saturating_add(memory.allocated);
            sum.self_allocated = sum.self_allocated.saturating_add(memory.self_allocated);
            sum.allocations = sum.allocations.saturating_add(memory.allocations);
        }
    }

    /// An instant: a magnitude where it carries a `value`, and nothing a report aggregates otherwise.
    fn instant(&mut self, target: String, name: String, annotations: &[(String, Annotated)]) {
        let Some(value) = annotations
            .iter()
            .find(|(field, _)| field == "value")
            .and_then(|(_, value)| value.magnitude())
        else {
            return;
        };

        let distribution = self.samples.entry((target, name)).or_default();
        distribution.count += 1;
        distribution.total = distribution.total.saturating_add(value);
        distribution.min = Some(distribution.min.map_or(value, |least| least.min(value)));
        distribution.max = distribution.max.max(value);
    }

    fn finish(self) -> Profile {
        let mut summaries = self
            .spans
            .into_iter()
            .map(|((target, name, group), aggregate)| Summary {
                target,
                name,
                group,
                calls: aggregate.calls,
                total: nanos(aggregate.total),
                self_total: nanos(aggregate.own),
                min: nanos(aggregate.min.unwrap_or_default()),
                max: nanos(aggregate.max),
                memory: aggregate.memory,
            })
            .collect::<Vec<_>>();
        summaries.sort_by(|left, right| {
            right
                .total
                .cmp(&left.total)
                .then_with(|| left.target.cmp(&right.target))
                .then_with(|| left.name.cmp(&right.name))
                .then_with(|| left.group.cmp(&right.group))
        });

        let mut samples = self
            .samples
            .into_iter()
            .map(|((target, name), distribution)| SampleSummary {
                target,
                name,
                count: distribution.count,
                total: distribution.total,
                min: distribution.min.unwrap_or_default(),
                max: distribution.max,
            })
            .collect::<Vec<_>>();
        samples.sort_by(|left, right| {
            right
                .total
                .cmp(&left.total)
                .then_with(|| left.target.cmp(&right.target))
                .then_with(|| left.name.cmp(&right.name))
        });

        // The tracks in the order their threads first recorded, which is the order the writer numbered them in; each stack outermost first, so it reads as the path down to what its thread could not finish.
        let threads = self.tracks.len();
        let open = self
            .tracks
            .into_values()
            .flat_map(|track| {
                let thread = track.thread;
                track.stack.into_iter().map(move |slice| OpenSpan {
                    target: slice.target,
                    name: slice.name,
                    group: slice.group,
                    thread: thread.clone(),
                    entered: u128::from(slice.at),
                })
            })
            .collect();

        Profile {
            summaries,
            samples,
            peak: self.peak.map(|peak| peak as usize),
            threads,
            open,
        }
    }
}

fn nanos(value: u128) -> Duration {
    Duration::from_nanos(value.min(u128::from(u64::MAX)) as u64)
}

#[cfg(test)]
mod tests;
