//! The report: every figure computed from the samples when it is asked, so none can disagree with what it is a figure of.
//!
//! **A difference is proven only where two spans are disjoint.** A figure's span is the least and greatest of what its groups give, and two readings whose spans overlap could have produced each other, so the report says `no difference proven` rather than a percentage. The rule assumes nothing about how the samples are distributed, and it errs toward missing a real movement rather than announcing one: a single bad batch widens a span and hides a change, which is the direction worth being wrong in.
//!
//! **A movement is read against Rust.** Across two readings the report compares Curios's time over Rust's, group by group. The two ran interleaved in each group, so what moved the machine during a group or between the two sittings reaches both and cancels in the ratio, and what is left is Curios's own.
//!
//! **No figure states a precision its span does not support.** Every number is rounded to the place of the first significant digit of its half-span, so a median of 438.2 with a span of six milliseconds prints as 438.

#[cfg(test)]
mod tests;

use {
    crate::{CONTESTANTS, Reading, WORKLOADS},
    std::fmt::Write,
};

/// The anchor every other contestant is read against, within a reading and across two.
const CONTROL: &str = "rust";

/// The subject, whose movement the bench exists to report.
const SUBJECT: &str = "curios";

/// What a contestant did, as the report reads it.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Figure {
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
fn spanned(values: &[f64]) -> Option<Figure> {
    Some(Figure {
        middle: median(values)?,
        least: values.iter().copied().fold(f64::INFINITY, f64::min),
        most: values.iter().copied().fold(f64::NEG_INFINITY, f64::max),
    })
}

/// What `groups` amount to: the median of each group's median, and the least and greatest of them.
pub(crate) fn figure(groups: &[&[f64]]) -> Option<Figure> {
    spanned(
        &groups
            .iter()
            .filter_map(|group| median(group))
            .collect::<Vec<_>>(),
    )
}

/// `subject` over `against`, group by group: each group's median of the one over the other's in the same group, where the two ran interleaved, so what disturbed the machine during it reaches both and cancels.
fn relative(subject: &[&[f64]], against: &[&[f64]]) -> Option<Figure> {
    spanned(
        &subject
            .iter()
            .zip(against)
            .filter_map(|(subject, against)| Some(median(subject)? / median(against)?))
            .collect::<Vec<_>>(),
    )
}

impl Figure {
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
fn times(figure: &Figure, against: &Figure) -> String {
    let ratio = figure.middle / against.middle;
    // The ratio's own resolution: a fraction is only as sharp as the figures it divides.
    let half = ratio * (figure.half() / figure.middle + against.half() / against.middle);

    format!("{}×", stated(ratio, half.max(0.001)))
}

/// Whether two spans could not have produced each other.
fn moved(before: &Figure, after: &Figure) -> bool {
    after.most < before.least || after.least > before.most
}

/// How a table names `contestant`.
fn titled(contestant: &str) -> &str {
    CONTESTANTS
        .iter()
        .find(|named| named.name == contestant)
        .map_or(contestant, |named| named.title)
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

/// One workload's table, over however its samples are held: the contestants fastest first, each with its span and how many times Rust it is.
pub(crate) fn table(title: &str, letter: &str, size: u64, rows: &[(&str, &[&[f64]])]) -> String {
    let mut figures = rows
        .iter()
        .filter_map(|&(contestant, groups)| Some((contestant, figure(groups)?)))
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
            format!("{mark}{} ms{mark}", stated(figure.middle, half)),
            format!(
                "{}–{}",
                stated(figure.least, half),
                stated(figure.most, half)
            ),
            format!("{mark}{against}{mark}"),
        ]);
    }

    format!(
        "### `{title}` ({letter} = {size})\n\n{}",
        tabulated(&["Contestant", "Median", "Span", "× Rust"], &rows)
    )
}

/// What a reading says of itself, ahead of its tables.
fn header(number: usize, reading: &Reading) -> String {
    let pinned = reading
        .pinned
        .iter()
        .map(|&(tool, version)| format!("{tool} {version}"))
        .collect::<Vec<_>>()
        .join(", ");

    format!(
        "# Reading {number:02} — taken {}\n\n- **subject** {}\n- **machine** {}\n- **state** {}\n- **software** {}\n- **pinned** {pinned}\n",
        reading.taken,
        reading.subject,
        reading.platform.machine,
        reading.platform.state,
        reading.platform.software,
    )
}

/// The reading numbered `number`, in full: what it says of itself, then a table per workload it measured.
pub(crate) fn one(number: usize, reading: &Reading) -> String {
    let mut out = header(number, reading);

    for workload in WORKLOADS {
        let Some(size) = reading.size(workload.name) else {
            continue;
        };
        let rows = CONTESTANTS
            .iter()
            .filter_map(|contestant| {
                let timed = reading.took(workload.name, contestant.name)?;
                Some((contestant.title, timed.groups))
            })
            .collect::<Vec<_>>();
        if rows.is_empty() {
            continue;
        }

        let _ = write!(
            out,
            "\n{}",
            table(workload.title, workload.letter, size, &rows)
        );
    }

    out
}

/// Why `before` and `after` cannot be compared, where they cannot.
///
/// The software line is not among these. A kernel or microcode update that moves a figure moves Rust with Curios, and the ratio a comparison reads cancels it, so gating on it would strand the record at every patch for nothing.
fn incomparable(before: &Reading, after: &Reading) -> Option<String> {
    let differs = |what: &str, before: &str, after: &str| {
        (before != after).then(|| format!("  {what}  {before:?}\n       -> {after:?}"))
    };

    differs("machine", before.platform.machine, after.platform.machine)
        .or_else(|| differs("state", before.platform.state, after.platform.state))
        .or_else(|| {
            (before.pinned != after.pinned).then(|| {
                format!(
                    "  pinned   {:?}\n       -> {:?}",
                    before.pinned, after.pinned
                )
            })
        })
}

/// What changed between two readings, where anything can be said of it at all.
pub(crate) fn compare(number: usize, before: &Reading, after: &Reading) -> String {
    let heading = format!("## Reading {:02} → {number:02}\n\n", number - 1);

    if let Some(why) = incomparable(before, after) {
        return format!("{heading}Not comparable.\n\n```\n{why}\n```\n");
    }

    let mut out = heading;
    let mut rows = Vec::new();
    for workload in WORKLOADS {
        let read = |reading: &Reading| {
            let groups = |contestant| Some(reading.took(workload.name, contestant)?.groups);
            relative(groups(SUBJECT)?, groups(CONTROL)?)
        };
        let (Some(was), Some(is)) = (read(before), read(after)) else {
            continue;
        };

        let half = was.half().max(is.half());
        // The size is held per workload rather than over the reading: one resized, or one entering the record later, says so on its own row and leaves every other row comparable.
        let said = match (before.size(workload.name), after.size(workload.name)) {
            (Some(then), Some(now)) if then != now => {
                format!("sized {then}, then {now}; not compared")
            }
            _ => match moved(&was, &is) {
                false => "no difference proven".to_string(),
                true => format!(
                    "moved, {}%",
                    stated(
                        (is.middle - was.middle) / was.middle * 100.0,
                        half / was.middle * 100.0
                    )
                ),
            },
        };

        rows.push(vec![
            format!("`{}`", workload.name),
            format!("{}×", stated(was.middle, half)),
            format!("{}×", stated(is.middle, half)),
            said,
        ]);
    }

    out.push_str(&tabulated(
        &["Workload", "Was, × Rust", "Is, × Rust", ""],
        &rows,
    ));

    out
}

/// Every reading so far: each in full, with what changed between consecutive ones.
pub(crate) fn report(readings: &[&Reading]) -> String {
    let Some((first, rest)) = readings.split_first() else {
        return "No reading recorded. `cargo xbench collect` takes the first.\n".to_string();
    };

    let mut out = one(0, first);
    for (number, reading) in rest.iter().enumerate() {
        let _ = write!(
            out,
            "\n{}\n{}",
            one(number + 1, reading),
            compare(number + 1, readings[number], reading)
        );
    }

    out
}
