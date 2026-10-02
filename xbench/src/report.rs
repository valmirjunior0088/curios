//! The report: every figure computed from the samples when it is asked, so none can disagree with what it is a figure of.
//!
//! **A difference is proven only where two spans are disjoint.** A contestant's span is the least and greatest of its group medians, and two readings whose spans overlap could have produced each other, so the report says `no difference proven` rather than a ratio. The rule assumes nothing about how the samples are distributed, and it errs toward missing a real movement rather than announcing one: a single bad batch widens a span and hides a change, which is the direction worth being wrong in.
//!
//! **A control that moved disqualifies the comparison.** Rust is the control. Where it moved between two readings the machine was not the same machine twice, whatever the platform lines say, and no Curios figure is drawn from it.
//!
//! **No figure states a precision its span does not support.** Every number is rounded to the place of the first significant digit of its half-span, so a median of 438.2 with a span of six milliseconds prints as 438.

#[cfg(test)]
mod tests;

use {
    crate::{CONTESTANTS, Reading, WORKLOADS},
    std::fmt::Write,
};

/// The anchor every other contestant is read against, and the control a comparison across readings is held by.
const CONTROL: &str = "rust";

/// The subject, whose movement the bench exists to report.
const SUBJECT: &str = "curios";

/// What a contestant did, as the report reads it.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Figure {
    /// The median of the group medians.
    pub middle: f64,
    /// The least group median, and the greatest: between them is the span a difference must clear.
    pub least: f64,
    pub most: f64,
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

/// What `groups` amount to: the median of each group's median, and the least and greatest of them.
pub fn figure(groups: &[&[f64]]) -> Option<Figure> {
    let middles = groups
        .iter()
        .filter_map(|group| median(group))
        .collect::<Vec<_>>();
    let middle = median(&middles)?;

    Some(Figure {
        middle,
        least: middles.iter().copied().fold(f64::INFINITY, f64::min),
        most: middles.iter().copied().fold(f64::NEG_INFINITY, f64::max),
    })
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

/// One workload's table, over however its samples are held: the contestants fastest first, each with its span and how many times the control it is.
pub fn table(title: &str, letter: &str, size: u64, rows: &[(&str, &[&[f64]])]) -> String {
    let mut figures = rows
        .iter()
        .filter_map(|&(contestant, groups)| Some((contestant, figure(groups)?)))
        .collect::<Vec<_>>();
    let control = figures
        .iter()
        .find(|(contestant, _)| *contestant == titled(CONTROL))
        .map(|(_, figure)| *figure);
    figures.sort_by(|(_, left), (_, right)| left.middle.total_cmp(&right.middle));

    let mut out = format!("### `{title}` ({letter} = {size})\n\n");
    out.push_str("| Contestant | Median | Span | × Rust |\n| :--- | ---: | ---: | ---: |\n");

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

        let _ = writeln!(
            out,
            "| {mark}{contestant}{mark} | {mark}{} ms{mark} | {}–{} | {mark}{against}{mark} |",
            stated(figure.middle, half),
            stated(figure.least, half),
            stated(figure.most, half),
        );
    }

    out
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

/// What each contestant's build weighed.
///
/// It is read down a column and never across one: the Curios contestant is a self-contained executable with the engine compiled into it, so its weight against `rust`'s says nothing. What a column answers is whether that contestant's build grew between two readings.
fn weights(reading: &Reading) -> String {
    let mut out = String::from("\n### Builds (bytes)\n\n| Workload |");
    for contestant in CONTESTANTS {
        let _ = write!(out, " {} |", contestant.title);
    }
    out.push_str("\n| :--- |");
    out.push_str(&" ---: |".repeat(CONTESTANTS.len()));
    out.push('\n');

    for workload in WORKLOADS {
        let weighed = CONTESTANTS
            .iter()
            .map(|contestant| reading.weight(workload.name, contestant.name))
            .collect::<Vec<_>>();
        if weighed.iter().all(Option::is_none) {
            continue;
        }

        let _ = write!(out, "| `{}` |", workload.name);
        for bytes in weighed {
            let _ = write!(
                out,
                " {} |",
                bytes.map_or_else(|| "—".to_string(), |bytes| bytes.to_string())
            );
        }
        out.push('\n');
    }

    out
}

/// The reading numbered `number`, in full: what it says of itself, then a table per workload it measured.
pub fn one(number: usize, reading: &Reading) -> String {
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

    if !reading.built.is_empty() {
        out.push_str(&weights(reading));
    }

    out
}

/// Why `before` and `after` cannot be compared, where they cannot.
///
/// The software line is not among these. A kernel or microcode update rarely moves a figure past its span, and where it does the control moves with it and the comparison is disqualified anyway, so gating on it would strand the record at every patch while catching nothing the control does not.
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
pub fn compare(number: usize, before: &Reading, after: &Reading) -> String {
    let heading = format!("## Reading {:02} → {number:02}\n\n", number - 1);

    if let Some(why) = incomparable(before, after) {
        return format!("{heading}Not comparable.\n\n```\n{why}\n```\n");
    }

    let drifted = before.platform.software != after.platform.software;
    let mut out = heading;
    if drifted {
        let _ = writeln!(
            out,
            "The software differs — `{}` → `{}` — and is not gated: a movement it caused moves the control too.\n",
            before.platform.software, after.platform.software
        );
    }

    let mut lines = Vec::new();
    for workload in WORKLOADS {
        let read = |reading: &Reading, contestant: &str| {
            figure(reading.took(workload.name, contestant)?.groups)
        };
        let (Some(was), Some(is)) = (read(before, SUBJECT), read(after, SUBJECT)) else {
            continue;
        };

        // The control first: where it moved, the machine was not the same machine twice and nothing is drawn from the subject's rows.
        let control = match (read(before, CONTROL), read(after, CONTROL)) {
            (Some(was), Some(is)) => moved(&was, &is),
            _ => false,
        };

        let half = was.half().max(is.half());
        // The size is held per workload rather than over the reading: one resized, or one entering the record later, says so on its own row and leaves every other row comparable.
        let said = match (before.size(workload.name), after.size(workload.name)) {
            (Some(then), Some(now)) if then != now => {
                format!("sized {then}, then {now}; not compared")
            }
            _ => match (control, moved(&was, &is)) {
                (true, _) => "control did not hold; nothing drawn".to_string(),
                (false, false) => "no difference proven".to_string(),
                (false, true) => format!(
                    "moved, {}%",
                    stated(
                        (is.middle - was.middle) / was.middle * 100.0,
                        half / was.middle * 100.0
                    )
                ),
            },
        };

        lines.push(format!(
            "| `{}` | {} ms | {} ms | {said} |",
            workload.name,
            stated(was.middle, half),
            stated(is.middle, half),
        ));
    }

    out.push_str("| Workload | Was | Is | |\n| :--- | ---: | ---: | :--- |\n");
    out.push_str(&lines.join("\n"));
    out.push('\n');

    // A build's weight is deterministic under the pins, so a difference is stated as itself: there is no span to clear and no control to hold.
    let reweighed = reweighed(before, after);
    if !reweighed.is_empty() {
        out.push_str(
            "\n### Builds that changed (bytes)\n\n| Workload | Contestant | Was | Is | Δ |\n| :--- | :--- | ---: | ---: | ---: |\n",
        );
        out.push_str(&reweighed.join("\n"));
        out.push('\n');
    }

    out
}

/// Every build whose weight differs between two readings.
fn reweighed(before: &Reading, after: &Reading) -> Vec<String> {
    let mut lines = Vec::new();

    for workload in WORKLOADS {
        for contestant in CONTESTANTS {
            let (Some(then), Some(now)) = (
                before.weight(workload.name, contestant.name),
                after.weight(workload.name, contestant.name),
            ) else {
                continue;
            };

            if then != now {
                lines.push(format!(
                    "| `{}` | {} | {then} | {now} | {:+} |",
                    workload.name,
                    contestant.title,
                    i128::from(now) - i128::from(then)
                ));
            }
        }
    }

    lines
}

/// Every reading so far: each in full, with what changed between consecutive ones.
pub fn report(readings: &[&Reading]) -> String {
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
