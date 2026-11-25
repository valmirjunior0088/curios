//! The report: every figure computed from the samples when it is asked, so none can disagree with what it is a figure of.
//!
//! **A difference is proven only where two spans are disjoint.** A figure's span is the least and greatest of what its groups give, and two records whose spans overlap could have produced each other, so the report says `no difference proven` rather than a percentage. The rule assumes nothing about how the samples are distributed, and it errs toward missing a real movement rather than announcing one: a single bad batch widens a span and hides a change, which is the direction worth being wrong in.
//!
//! **A movement is read against Rust (Native).** Across two records the report compares Curios's time over native Rust's, group by group. The two ran interleaved in each group, so what moved the machine during a group or between the two sittings reaches both and cancels in the ratio, and what is left is Curios's own.
//!
//! **No figure states a precision its span does not support.** Every number is rounded to the place of the first significant digit of its half-span, so a median of 438.2 with a span of six milliseconds prints as 438.

#[cfg(test)]
mod tests;

use {
    crate::{CONTESTANTS, Pin, Record, Unit, WORKLOADS, named},
    std::fmt::Write,
};

/// The anchor every other contestant is read against, within a record and across two.
const CONTROL: &str = "rust";

/// The subject, whose movement the bench exists to report.
const SUBJECT: &str = "curios";

/// What a figure's groups amount to, as the report reads them: the median of their medians, and the span about it.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Spanned {
    /// The median of the group medians.
    middle: f64,
    /// The least group median, and the greatest: between them is the span a difference must clear.
    least: f64,
    most: f64,
}

/// The middle of `samples`.
///
/// A batch is summarised by its median rather than its mean because a mean is pulled by one slow execution, and one slow execution is what a machine running anything else produces.
fn median(samples: &[f64]) -> Option<f64> {
    if samples.is_empty() {
        return None;
    }

    let mut sorted = samples.to_vec();
    sorted.sort_by(f64::total_cmp);

    let middle = sorted.len() / 2;
    Some(match sorted.len() % 2 {
        0 => (sorted[middle - 1] + sorted[middle]) / 2.0,
        _ => sorted[middle],
    })
}

/// The median of what each group gave, and the least and greatest of it.
fn spanned(values: &[f64]) -> Option<Spanned> {
    Some(Spanned {
        middle: median(values)?,
        least: values.iter().copied().fold(f64::INFINITY, f64::min),
        most: values.iter().copied().fold(f64::NEG_INFINITY, f64::max),
    })
}

/// What `groups` amount to: the median of each group's median, and the least and greatest of them.
pub fn figure(groups: &[Vec<f64>]) -> Option<Spanned> {
    spanned(
        &groups
            .iter()
            .filter_map(|group| median(group))
            .collect::<Vec<_>>(),
    )
}

/// `subject` over `against`, group by group: each group's median of the one over the other's in the same group, where the two ran interleaved, so what disturbed the machine during it reaches both and cancels.
fn relative(subject: &[Vec<f64>], against: &[Vec<f64>]) -> Option<Spanned> {
    spanned(
        &subject
            .iter()
            .zip(against)
            .filter_map(|(subject, against)| Some(median(subject)? / median(against)?))
            .collect::<Vec<_>>(),
    )
}

impl Spanned {
    /// Half the span, which is the resolution every figure read from it is stated to.
    fn half(&self) -> f64 {
        (self.most - self.least) / 2.0
    }
}

/// `value` written to the place of the first significant digit of `half`.
fn stated(value: f64, half: f64) -> String {
    let places = match half > 0.0 {
        true => (-half.log10().floor()).clamp(0.0, 3.0) as usize,
        false => 3,
    };

    format!("{value:.places$}")
}

/// How many times `figure` is `against`, stated to the resolution the wider of the two supports.
fn times(figure: &Spanned, against: &Spanned) -> String {
    let ratio = figure.middle / against.middle;
    // The ratio's own resolution: a fraction is only as sharp as the figures it divides.
    let half = ratio * (figure.half() / figure.middle + against.half() / against.middle);

    format!("{}×", stated(ratio, half.max(0.001)))
}

/// Whether two spans could not have produced each other.
fn moved(before: &Spanned, after: &Spanned) -> bool {
    after.most < before.least || after.least > before.most
}

/// How a table names `contestant`.
fn titled(contestant: &str) -> &str {
    CONTESTANTS
        .iter()
        .find(|named| named.name == contestant)
        .map_or(contestant, |named| named.title)
}

/// The size `record` timed `workload` at, as its contestants' figures record it, where it timed the workload at all.
fn sized(record: &Record, workload: &str) -> Option<u64> {
    CONTESTANTS.iter().find_map(|contestant| {
        record
            .figure(&named(workload, contestant.name), Unit::Microseconds)?
            .size
    })
}

/// Whether `record` counted any contestant's instructions.
fn counted(record: &Record) -> bool {
    record
        .figures
        .iter()
        .any(|figure| figure.unit == Unit::Instructions)
}

/// A Markdown table with every cell left-aligned and its columns padded for a terminal. Each row has one cell per header.
fn tabulated(headers: &[&str], rows: &[Vec<String>]) -> String {
    let widths = headers
        .iter()
        .enumerate()
        .map(|(column, header)| {
            rows.iter()
                .map(|row| row[column].chars().count())
                .chain([header.chars().count(), 4])
                .max()
                .unwrap_or(4)
        })
        .collect::<Vec<_>>();

    let mut out = String::new();
    let mut row = |cells: &[String]| {
        out.push('|');
        for (cell, width) in cells.iter().zip(&widths) {
            let _ = write!(out, " {cell:<width$} |");
        }
        out.push('\n');
    };

    row(&headers
        .iter()
        .map(|header| header.to_string())
        .collect::<Vec<_>>());
    row(&widths
        .iter()
        .map(|width| "-".repeat(*width))
        .collect::<Vec<_>>());
    for cells in rows {
        row(cells);
    }

    out
}

/// One workload's table in `unit`, over however its samples are held: the contestants least first, each with its span and how many times Rust it is.
pub fn table(
    title: &str,
    letter: &str,
    size: u64,
    unit: Unit,
    rows: &[(&str, Vec<Vec<f64>>)],
) -> String {
    let mut figures = rows
        .iter()
        .filter_map(|(contestant, groups)| Some((*contestant, figure(groups)?)))
        .collect::<Vec<_>>();
    let control = figures
        .iter()
        .find(|(contestant, _)| *contestant == titled(CONTROL))
        .map(|(_, figure)| *figure);
    figures.sort_by(|(_, left), (_, right)| left.middle.total_cmp(&right.middle));

    let mut rows = Vec::new();
    for (contestant, figure) in figures {
        let half = figure.half();
        // Curios is the subject, so its row stands out.
        let mark = if contestant == titled(SUBJECT) {
            "**"
        } else {
            ""
        };
        let against = match control {
            Some(control) if contestant != titled(CONTROL) => times(&figure, &control),
            _ => "—".to_string(),
        };

        rows.push(vec![
            format!("{mark}{contestant}{mark}"),
            format!(
                "{mark}{} {}{mark}",
                stated(figure.middle, half),
                unit.stated()
            ),
            format!(
                "{}–{}",
                stated(figure.least, half),
                stated(figure.most, half)
            ),
            format!("{mark}{against}{mark}"),
        ]);
    }

    let counted = match unit {
        Unit::Microseconds => String::new(),
        other => format!(", {}", other.name()),
    };

    format!(
        "### `{title}` ({letter} = {size}){counted}\n\n{}",
        tabulated(&["Contestant", "Median", "Span", "× Rust (Native)"], &rows)
    )
}

/// What a record says of itself, ahead of its tables.
fn header(number: usize, record: &Record) -> String {
    let pinned = record
        .pinned
        .iter()
        .map(|pin| format!("{} {}", pin.tool, pin.version))
        .collect::<Vec<_>>()
        .join(", ");

    format!(
        "# Record {number:02} — taken {}\n\n- **subject** {}\n- **machine** {}\n- **state** {}\n- **software** {}\n- **pinned** {pinned}\n",
        record.taken,
        record.subject,
        record.platform.machine,
        record.platform.state,
        record.platform.software,
    )
}

/// The record numbered `number`, in full: what it says of itself, then a table per workload it measured and unit it counted it in.
pub fn one(number: usize, record: &Record) -> String {
    let mut out = header(number, record);

    for workload in WORKLOADS {
        let Some(size) = sized(record, workload.name) else {
            continue;
        };
        for unit in [Unit::Microseconds, Unit::Instructions] {
            let rows = CONTESTANTS
                .iter()
                .filter_map(|contestant| {
                    let figure = record.figure(&named(workload.name, contestant.name), unit)?;
                    Some((contestant.title, figure.stated()))
                })
                .collect::<Vec<_>>();
            if rows.is_empty() {
                continue;
            }

            let _ = write!(
                out,
                "\n{}",
                table(workload.title, workload.letter, size, unit, &rows)
            );
        }
    }

    out
}

/// Why `before` and `after` cannot be compared in `unit`, where they cannot: across the platform lines its unit is gated by ([`Unit::gated`]) where the two differ in them.
///
/// The software line is not among these. A kernel or microcode update that moves a figure moves Rust with Curios, and the ratio a comparison reads cancels it, so gating on it would strand the record at every patch for nothing.
fn incomparable(before: &Record, after: &Record, unit: Unit) -> Option<String> {
    let differs = |what: &str, before: &str, after: &str| {
        (before != after).then(|| format!("  {what}  {before:?}\n       -> {after:?}"))
    };

    // A pin is spelled as the tool and the version it holds, the pair a reader compares.
    let pairs = |pinned: &[Pin]| {
        pinned
            .iter()
            .map(|pin| (pin.tool.clone(), pin.version.clone()))
            .collect::<Vec<_>>()
    };

    let gated = unit.gated();
    gated
        .machine
        .then(|| differs("machine", &before.platform.machine, &after.platform.machine))
        .flatten()
        .or_else(|| {
            gated
                .state
                .then(|| differs("state", &before.platform.state, &after.platform.state))
                .flatten()
        })
        .or_else(|| {
            (gated.pins && before.pinned != after.pinned).then(|| {
                format!(
                    "  pinned   {:?}\n       -> {:?}",
                    pairs(&before.pinned),
                    pairs(&after.pinned)
                )
            })
        })
}

/// What a workload's figure did between two records: not compared where its size changed, and otherwise moved or not as its two spans say.
fn verdict(workload: &str, before: &Record, after: &Record, was: &Spanned, is: &Spanned) -> String {
    // The size is held by each figure rather than over the record: a workload resized, or one entering the record later, says so on its own row and leaves every other row comparable.
    match (sized(before, workload), sized(after, workload)) {
        (Some(then), Some(now)) if then != now => format!("sized {then}, then {now}; not compared"),
        _ => match moved(was, is) {
            false => "no difference proven".to_string(),
            true => format!(
                "moved, {}%",
                stated(
                    (is.middle - was.middle) / was.middle * 100.0,
                    was.half().max(is.half()) / was.middle * 100.0
                )
            ),
        },
    }
}

/// The two records' times, read as Curios over Rust (Native) group by group, where they compare at all.
fn times_compared(before: &Record, after: &Record) -> String {
    if let Some(why) = incomparable(before, after, Unit::Microseconds) {
        return format!("Not comparable.\n\n```\n{why}\n```\n");
    }

    let mut rows = Vec::new();
    for workload in WORKLOADS {
        let read = |record: &Record| {
            let groups = |contestant| {
                Some(
                    record
                        .figure(&named(workload.name, contestant), Unit::Microseconds)?
                        .stated(),
                )
            };
            relative(&groups(SUBJECT)?, &groups(CONTROL)?)
        };
        let (Some(was), Some(is)) = (read(before), read(after)) else {
            continue;
        };

        let half = was.half().max(is.half());
        rows.push(vec![
            format!("`{}`", workload.name),
            format!("{}×", stated(was.middle, half)),
            format!("{}×", stated(is.middle, half)),
            verdict(workload.name, before, after, &was, &is),
        ]);
    }

    tabulated(
        &[
            "Workload",
            "Was, × Rust (Native)",
            "Is, × Rust (Native)",
            "",
        ],
        &rows,
    )
}

/// The two records' counts of Curios's instructions, where they compare at all: a count needs no control to cancel what moved the machine.
fn counts_compared(before: &Record, after: &Record) -> String {
    if let Some(why) = incomparable(before, after, Unit::Instructions) {
        return format!("Not comparable.\n\n```\n{why}\n```\n");
    }

    let mut rows = Vec::new();
    for workload in WORKLOADS {
        let read = |record: &Record| {
            figure(
                &record
                    .figure(&named(workload.name, SUBJECT), Unit::Instructions)?
                    .stated(),
            )
        };
        let (Some(was), Some(is)) = (read(before), read(after)) else {
            continue;
        };

        let half = was.half().max(is.half());
        rows.push(vec![
            format!("`{}`", workload.name),
            format!("{} M", stated(was.middle, half)),
            format!("{} M", stated(is.middle, half)),
            verdict(workload.name, before, after, &was, &is),
        ]);
    }

    tabulated(&["Workload", "Was, Curios", "Is, Curios", ""], &rows)
}

/// What changed between two records, where anything can be said of it at all: their times, and their instructions where both counted them.
pub fn compare(number: usize, before: &Record, after: &Record) -> String {
    let mut out = format!("## Record {:02} → {number:02}\n\n", number - 1);
    out.push_str(&times_compared(before, after));
    if counted(before) && counted(after) {
        let _ = write!(
            out,
            "\n### Instructions\n\n{}",
            counts_compared(before, after)
        );
    }

    out
}

/// Every record so far: each in full, with what changed between consecutive ones.
pub fn report(records: &[Record]) -> String {
    let Some((first, rest)) = records.split_first() else {
        return "No record filed. `cargo xbench measure contest` takes the first.\n".to_string();
    };

    let mut out = one(0, first);
    for (number, record) in rest.iter().enumerate() {
        let _ = write!(
            out,
            "\n{}\n{}",
            one(number + 1, record),
            compare(number + 1, &records[number], record)
        );
    }

    out
}
