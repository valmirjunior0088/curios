//! Aggregation, as one consumer of the record stream rather than as what the library produces: [`fold`] reads the packets [`trace`](crate::trace()) wrote and recomputes timings, allocation figures and magnitude distributions from them.
//!
//! Every statistic here is derived, which is the point. A duration is a slice's end differenced against its beginning, a retained byte count the same subtraction over the allocator's readings, and a sample distribution a pass over the instants that carry a `value` — so a question the columns below do not answer is asked of the file directly, by `trace_processor` or Perfetto's UI, rather than by changing what a capture keeps.
//!
//! **A track is a thread, and each keeps a stack.** A slice begins and ends on its thread's track, and the slices of one track nest, so self time is each slice's extent less what the slices begun directly inside it on its track took, and what ran inside a span is what its track entered while it was entered. A call is a slice: a span entered twice is two.
//!
//! **A stream cut short reads up to its last whole packet.** A killed run's file, and one whose writer stopped at a failed write, end on a whole packet or inside one, and the packet cut short is dropped; the slices begun and never ended are reported in [`Report::open`], which is what a killed run was inside.
//!
//! **A packet that names what was never defined is refused.** An interned id, a track or a counter a packet uses before any packet of the stream defined it is a stream that is not this recorder's, or not whole from its first packet, and reading it as one would invent names; `trace_processor` rests on the same order.
//!
//! **Each destination has its reader.** [`fold_at`] is [`Destination::File`](crate::Destination::File)'s: it takes the path that destination took and reads the file there. [`fold`] is [`Destination::Stream`](crate::Destination::Stream)'s, for bytes a caller already holds, such as a buffer folded back in the same process.

use {
    crate::{
        COUNTER_TRACKS,
        perfetto::{TracePacket, TrackEvent, debug_annotation, trace_packet, track_event},
    },
    prost::Message,
    std::{
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
pub struct Report {
    /// Timings ordered from greatest to least total duration.
    pub summaries: Vec<Summary>,
    /// Sampled magnitudes ordered from greatest to least total, one entry per `curios_common::sample!` site that fired.
    pub samples: Vec<SampleSummary>,
    /// The greatest peak any slice carried: the high-water mark of what the process held while counting, up to the last packet the file holds; `None` where the allocator did not count.
    pub peak: Option<usize>,
    /// What ran inside each span name on its track: every other span name entered while one of it was entered, outer name first and costliest first beneath it.
    pub within: Vec<Within>,
    /// The spans still entered when the packets ran out, outermost first.
    ///
    /// Empty for a stream whose operation returned. Non-empty is the useful case: it is the stack a compilation was inside when it was killed, and the reason a record stream is worth keeping over an aggregate that only exists once everything has closed.
    pub open: Vec<OpenSpan>,
}

/// The spans of one name entered while a span of another name was entered on one track: how many entries, and for how long in all.
///
/// **What a row per span cannot say.** A span's row counts every call wherever it was made, so it cannot say how many of them one stage made — how many of the questions reduction put to conversion were erasure's (`erase_unit`) and how many elaboration's. Each exit is credited once to every distinct name entered above it on its track, so nesting two spans of one outer name counts an inner call once, and a span entered within its own name is not counted inside itself.
#[derive(Debug)]
pub struct Within {
    /// The tracing target of the span that was entered around it.
    pub outer_target: String,
    /// The name of the span that was entered around it.
    pub outer_name: String,
    /// The tracing target of the span entered inside.
    pub target: String,
    /// The name of the span entered inside.
    pub name: String,
    /// How many entries of the inner name exited while the outer one was entered.
    pub entries: u64,
    /// Their time, inclusive, summed over those entries.
    pub total: Duration,
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
    /// How many spans of its name its track held open at it and around it — above one for a span re-entered within itself.
    pub depth: usize,
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
    /// The costliest slices of this row, costliest first and at most `SLOWEST_KEPT` of them, each with the fields its span was created with — for a span that declared fields beyond `group`, and empty for every other.
    ///
    /// **What an aggregate cannot say, the instances can.** A row answers how long a span took in all; it cannot say *which* call was slow or what it was handed, and that is the question a hunt for a pathological input asks. A span that declares its inputs as fields — `profile!("entails", assumed = assumed.len(), lower = ?lower)` — is answered here by the calls that cost the most, with their inputs beside them. Formatting a field costs at every span creation, so a field that is there to be read this way belongs on a span added to isolate one investigation, and leaves with it.
    pub slowest: Vec<SpanInstance>,
}

/// What the allocator counted over a row's spans: differences of the readings their slices carried.
#[derive(Debug, Clone, Copy, Default)]
pub struct Memory {
    /// Net bytes still held at exit: what the spans took minus what they returned. Negative where they freed more than they took, which is how a pass that consumes a representation reports itself.
    pub retained: i64,
    /// Bytes the spans took while entered, whether or not later returned. A pass that allocates heavily and frees as it goes shows large `allocated` beside near-zero [`retained`](Self::retained).
    pub allocated: u64,
    /// The part of [`allocated`](Self::allocated) no span entered inside these ones accounts for.
    pub self_allocated: u64,
    /// Trips through the allocator while the spans were entered. Divided into [`allocated`](Self::allocated) it gives the average request size, which is what separates a pass that wants fewer allocations from one that wants a smaller structure.
    pub allocations: u64,
}

/// One slice, as [`Summary::slowest`] keeps it.
#[derive(Debug, Clone)]
pub struct SpanInstance {
    /// How long it was entered.
    pub total: Duration,
    /// The part of [`total`](Self::total) no span entered inside it accounts for.
    pub self_total: Duration,
    /// Its span's fields other than `group`, in the order the call site wrote them.
    pub fields: Vec<(String, String)>,
}

/// How many of its costliest instances a row keeps. Enough to tell one pathological call from a family of them, and bounded so that keeping them costs a fold a constant per span name.
pub const SLOWEST_KEPT: usize = 10;

/// Recompute a report from the bytes of one record stream.
///
/// The error is the reader's, or a stream's that is not this recorder's: a packet that is not `Trace`'s, or that names what no packet before it defined. A packet cut short at the end is the normal ending of a killed run or of a writer that stopped, and is dropped.
pub fn fold(mut stream: impl Read) -> io::Result<Report> {
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
pub fn fold_at(path: &Path) -> io::Result<Report> {
    fold(File::open(path)?)
}

impl Report {
    /// The report as tab-separated tables: the spans, the magnitude sites beneath them, what ran inside each span, and — for a stream whose run never returned — the stack it was inside. A memory column reads `-` where the allocator did not count.
    ///
    /// The one renderer, so no two readers of a report print different columns of it.
    pub fn render(&self) -> String {
        let peak = self.peak.map_or("-".to_string(), |peak| {
            format!("{:.1} MiB", mebibytes(peak as f64))
        });
        let mut rendered = format!(
            "total_ms\tself_ms\tcalls\tmin_ms\tmax_ms\tretained_mb\tallocated_mb\tself_allocated_mb\tallocs\ttarget\tname\tgroup\t(peak {peak})\n",
        );
        for summary in &self.summaries {
            let memory = summary.memory.map_or("-\t-\t-\t-".to_string(), |memory| {
                format!(
                    "{:.1}\t{:.1}\t{:.1}\t{}",
                    mebibytes(memory.retained as f64),
                    mebibytes(memory.allocated as f64),
                    mebibytes(memory.self_allocated as f64),
                    memory.allocations,
                )
            });
            rendered.push_str(&format!(
                "{:.3}\t{:.3}\t{}\t{:.3}\t{:.3}\t{memory}\t{}\t{}\t{}\n",
                milliseconds(summary.total),
                milliseconds(summary.self_total),
                summary.calls,
                milliseconds(summary.min),
                milliseconds(summary.max),
                summary.target,
                summary.name,
                summary.group.as_deref().unwrap_or(""),
            ));
        }

        if self
            .summaries
            .iter()
            .any(|summary| !summary.slowest.is_empty())
        {
            rendered.push_str("\ntotal_ms\tself_ms\ttarget\tname\tgroup\tfields\t(slowest)\n");
            for summary in &self.summaries {
                for instance in &summary.slowest {
                    let fields = instance
                        .fields
                        .iter()
                        .map(|(name, value)| format!("{}={}", escape(name), escape(value)))
                        .collect::<Vec<_>>()
                        .join(" ");
                    rendered.push_str(&format!(
                        "{:.3}\t{:.3}\t{}\t{}\t{}\t{}\n",
                        milliseconds(instance.total),
                        milliseconds(instance.self_total),
                        summary.target,
                        summary.name,
                        summary.group.as_deref().unwrap_or(""),
                        fields,
                    ));
                }
            }
        }

        if !self.samples.is_empty() {
            rendered.push_str("\ncount\ttotal\tmin\tmean\tmax\ttarget\tname\n");
            for sample in &self.samples {
                rendered.push_str(&format!(
                    "{}\t{}\t{}\t{:.1}\t{}\t{}\t{}\n",
                    sample.count,
                    sample.total,
                    sample.min,
                    sample.mean(),
                    sample.max,
                    sample.target,
                    sample.name,
                ));
            }
        }

        if !self.within.is_empty() {
            rendered.push_str("\nentries\ttotal_ms\touter_target\touter\ttarget\tname\t(within)\n");
            for within in &self.within {
                rendered.push_str(&format!(
                    "{}\t{:.3}\t{}\t{}\t{}\t{}\n",
                    within.entries,
                    milliseconds(within.total),
                    within.outer_target,
                    within.outer_name,
                    within.target,
                    within.name,
                ));
            }
        }

        if !self.open.is_empty() {
            rendered.push_str("\nentered_ms\tdepth\ttarget\tname\tgroup\t(still open)\n");
            for span in &self.open {
                rendered.push_str(&format!(
                    "{:.3}\t{}\t{}\t{}\t{}\n",
                    span.entered as f64 / 1_000_000.0,
                    span.depth,
                    span.target,
                    span.name,
                    span.group.as_deref().unwrap_or(""),
                ));
            }
        }

        rendered
    }
}

fn milliseconds(duration: Duration) -> f64 {
    duration.as_secs_f64() * 1_000.0
}

fn mebibytes(bytes: f64) -> f64 {
    bytes / (1024.0 * 1024.0)
}

/// The four characters a tab-separated column cannot hold bare, as the rendered report escapes a field's name and value.
fn escape(value: &str) -> String {
    let mut escaped = String::with_capacity(value.len());
    for character in value.chars() {
        match character {
            '\\' => escaped.push_str("\\\\"),
            '\t' => escaped.push_str("\\t"),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            _ => escaped.push(character),
        }
    }

    escaped
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

/// A slice begun and not yet ended: its span's identity and fields, the readings it began with, and what the slices begun directly inside it took — the part of its extent that is not its own.
struct Open {
    target: String,
    name: String,
    group: Option<String>,
    fields: Vec<(String, String)>,
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

/// A thread's track: the slices open on it, innermost last, and how many of each name, so an end finds the distinct names entered above it without walking the stack.
#[derive(Default)]
struct Track {
    stack: Vec<Open>,
    on_stack: BTreeMap<Named, u32>,
}

#[derive(Default)]
struct Aggregate {
    calls: u64,
    total: u128,
    own: u128,
    min: Option<u128>,
    max: u128,
    memory: Option<Memory>,
    slowest: Vec<SpanInstance>,
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

/// A span or a magnitude site as a report names it: the target that owns it, and its name.
type Named = (String, String);

/// The slices of one name that ended inside another name on their track: how many, and their time in all.
#[derive(Default)]
struct Credit {
    entries: u64,
    total: u128,
}

#[derive(Default)]
struct Fold {
    names: HashMap<u64, String>,
    categories: HashMap<u64, String>,
    annotation_names: HashMap<u64, String>,
    strings: HashMap<u64, String>,
    tracks: HashMap<u64, Track>,
    counters: Vec<u64>,
    spans: BTreeMap<(String, String, Option<String>), Aggregate>,
    samples: BTreeMap<Named, Distribution>,
    within: BTreeMap<(Named, Named), Credit>,
    peak: Option<u64>,
}

impl Fold {
    fn packet(&mut self, packet: TracePacket) -> io::Result<()> {
        if let Some(interned) = packet.interned_data {
            for name in interned.event_names {
                self.names.insert(name.iid(), name.name().to_string());
            }
            for category in interned.event_categories {
                self.categories
                    .insert(category.iid(), category.name().to_string());
            }
            for name in interned.debug_annotation_names {
                self.annotation_names
                    .insert(name.iid(), name.name().to_string());
            }
            for text in interned.debug_annotation_string_values {
                self.strings
                    .insert(text.iid(), String::from_utf8_lossy(text.str()).into_owned());
            }
        }

        match packet.data {
            Some(trace_packet::Data::TrackDescriptor(track)) => {
                if track.thread.is_some() {
                    self.tracks.entry(track.uuid()).or_default();
                } else if track.counter.is_some() {
                    self.counters.push(track.uuid());
                }
            }
            Some(trace_packet::Data::TrackEvent(event)) => {
                self.event(packet.timestamp.unwrap_or_default(), event)?;
            }
            None => {}
        }

        Ok(())
    }

    fn event(&mut self, at: u64, event: TrackEvent) -> io::Result<()> {
        let track = event.track_uuid();
        if !self.tracks.contains_key(&track) {
            return Err(refused(format_args!(
                "track {track}, used before it was described"
            )));
        }
        let Some(track_event::NameField::NameIid(name)) = event.name_field else {
            return Err(refused(format_args!("an event with no interned name")));
        };
        let name = defined(&self.names, name, "event name")?.clone();
        let target = match event.category_iids.first() {
            Some(&category) => defined(&self.categories, category, "category")?.clone(),
            None => String::new(),
        };
        let readings = self.readings(&event)?;
        if let Some(readings) = readings {
            self.peak = Some(self.peak.unwrap_or(0).max(readings.peak));
        }
        let annotations = self.annotations(&event)?;

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

    /// The allocator's four readings a slice carries, `None` where it carries none.
    fn readings(&self, event: &TrackEvent) -> io::Result<Option<Readings>> {
        if event.extra_counter_track_uuids.is_empty() {
            return Ok(None);
        }
        let reading = |track: u64| -> io::Result<i64> {
            if !self.counters.contains(&track) {
                return Err(refused(format_args!(
                    "counter track {track}, used before it was described"
                )));
            }
            event
                .extra_counter_track_uuids
                .iter()
                .position(|&carried| carried == track)
                .and_then(|at| event.extra_counter_values.get(at).copied())
                .ok_or_else(|| {
                    refused(format_args!(
                        "a slice carrying no reading of counter track {track}"
                    ))
                })
        };
        let [live, allocated, allocations, peak] = COUNTER_TRACKS;

        Ok(Some(Readings {
            live: reading(live)?,
            allocated: reading(allocated)? as u64,
            allocations: reading(allocations)? as u64,
            peak: reading(peak)? as u64,
        }))
    }

    /// A slice's annotations as names and values, each id read off what the stream interned.
    fn annotations(&self, event: &TrackEvent) -> io::Result<Vec<(String, Annotated)>> {
        event
            .debug_annotations
            .iter()
            .map(|annotation| {
                let Some(debug_annotation::NameField::NameIid(name)) = annotation.name_field else {
                    return Err(refused(format_args!("an annotation with no interned name")));
                };
                let name = defined(&self.annotation_names, name, "annotation name")?.clone();
                let value = match annotation.value {
                    Some(debug_annotation::Value::BoolValue(value)) => {
                        Annotated::Text(value.to_string())
                    }
                    Some(debug_annotation::Value::UintValue(value)) => Annotated::Unsigned(value),
                    Some(debug_annotation::Value::IntValue(value)) => Annotated::Signed(value),
                    Some(debug_annotation::Value::StringValueIid(text)) => {
                        Annotated::Text(defined(&self.strings, text, "string value")?.clone())
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
        let mut group = None;
        let mut fields = Vec::new();
        for (field, value) in annotations {
            match field.as_str() {
                "group" => group = Some(value.to_string()),
                _ => fields.push((field, value.to_string())),
            }
        }
        let track = self
            .tracks
            .get_mut(&track)
            .expect("an event's track is described");
        *track
            .on_stack
            .entry((target.clone(), name.clone()))
            .or_default() += 1;
        track.stack.push(Open {
            target,
            name,
            group,
            fields,
            at,
            readings,
            nested: 0,
            nested_allocated: 0,
        });
    }

    /// The innermost slice of `track` ended: its extent and readings differenced, credited to the slice it was begun inside and to every distinct name entered above it, and aggregated under its row.
    fn end(&mut self, track: u64, at: u64, readings: Option<Readings>) {
        let track = self
            .tracks
            .get_mut(&track)
            .expect("an event's track is described");
        // An end with nothing begun on its track ends a slice whose beginning the stream does not hold, and is read as nothing.
        let Some(slice) = track.stack.pop() else {
            return;
        };
        let key = (slice.target.clone(), slice.name.clone());
        if let Some(count) = track.on_stack.get_mut(&key) {
            *count -= 1;
            if *count == 0 {
                track.on_stack.remove(&key);
            }
        }

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
        let outers = track
            .on_stack
            .keys()
            .filter(|outer| **outer != key)
            .cloned()
            .collect::<Vec<_>>();
        for outer in outers {
            let credit = self.within.entry((outer, key.clone())).or_default();
            credit.entries += 1;
            credit.total += u128::from(elapsed);
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

        if !slice.fields.is_empty() {
            let instance = SpanInstance {
                total: nanos(elapsed),
                self_total: nanos(u128::from(own)),
                fields: slice.fields,
            };
            // Costliest first, and the earlier of two equal instances first, so the kept set is the same however the slices were interleaved.
            let position = aggregate
                .slowest
                .partition_point(|kept| kept.total >= instance.total);
            if position < SLOWEST_KEPT {
                aggregate.slowest.insert(position, instance);
                aggregate.slowest.truncate(SLOWEST_KEPT);
            }
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

    fn finish(self) -> Report {
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
                slowest: aggregate.slowest,
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

        let mut open = Vec::new();
        for track in self.tracks.into_values() {
            let mut depths = BTreeMap::<Named, usize>::new();
            for slice in track.stack {
                let depth = depths
                    .entry((slice.target.clone(), slice.name.clone()))
                    .or_default();
                *depth += 1;
                open.push(OpenSpan {
                    target: slice.target,
                    name: slice.name,
                    group: slice.group,
                    depth: *depth,
                    entered: u128::from(slice.at),
                });
            }
        }
        // Outermost first: the span entered earliest is the one the others are nested inside.
        open.sort_by_key(|span| span.entered);

        let mut within = self
            .within
            .into_iter()
            .map(|((outer, inner), credit)| Within {
                outer_target: outer.0,
                outer_name: outer.1,
                target: inner.0,
                name: inner.1,
                entries: credit.entries,
                total: nanos(credit.total),
            })
            .collect::<Vec<_>>();
        within.sort_by(|left, right| {
            left.outer_target
                .cmp(&right.outer_target)
                .then_with(|| left.outer_name.cmp(&right.outer_name))
                .then_with(|| right.total.cmp(&left.total))
                .then_with(|| left.target.cmp(&right.target))
                .then_with(|| left.name.cmp(&right.name))
        });

        Report {
            summaries,
            samples,
            peak: self.peak.map(|peak| peak as usize),
            within,
            open,
        }
    }
}

fn nanos(value: u128) -> Duration {
    Duration::from_nanos(value.min(u128::from(u64::MAX)) as u64)
}

#[cfg(test)]
mod tests;
