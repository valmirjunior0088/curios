//! The runner: a measurement of the bench's, taken by name, a group a process of its own, and filed as its measurement's next record.
//!
//! **A group is a process.** What a run draws — the keys its maps hash with, the addresses it is handed, its restore of the prelude — differs between processes, so the span between groups is the span between processes; inside one, the harness turns the variants a measurement compares round-robin.
//!
//! **Built in release.** A figure is the compiler a user runs, without the checks a debug build keeps, which move counts as well as times: `cargo xbench` runs the bench in the dev profile, so the runner builds it in release and sits each group in that build.

#[cfg(test)]
mod tests;

use {
    crate::{
        Figure, Line, Measurement, Record, Spread, asked, capture, file, held, measurement_named,
        root, subject, today,
    },
    serde::Deserialize,
    std::{
        path::{Path, PathBuf},
        process::{Command, Stdio},
    },
};

/// What cargo says of an artifact it built, as much of it as names the bench's executable.
#[derive(Deserialize)]
struct Built {
    reason: String,
    target: Target,
    #[serde(default)]
    executable: Option<PathBuf>,
}

/// The target an artifact was built for.
#[derive(Deserialize)]
struct Target {
    name: String,
}

/// Build the bench in release, and the shipped `curios` beside it, which a measurement of the language server runs as its server, and name the executable a group sits in.
fn built(root: &Path) -> Result<PathBuf, String> {
    let built = Command::new("cargo")
        .args([
            "build",
            "--release",
            "--package",
            "xbench",
            "--bin",
            "xbench",
            "--package",
            "curios",
            "--bin",
            "curios",
            "--message-format=json-render-diagnostics",
        ])
        .current_dir(root)
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .output()
        .map_err(|error| format!("cargo did not start: {error}"))?;
    if !built.status.success() {
        return Err(format!("the bench did not build: {}", built.status));
    }

    String::from_utf8_lossy(&built.stdout)
        .lines()
        .filter_map(|line| serde_json::from_str::<Built>(line).ok())
        .filter(|built| built.reason == "compiler-artifact" && built.target.name == "xbench")
        .find_map(|built| built.executable)
        .ok_or_else(|| "cargo named no executable of the bench".to_string())
}

/// One group of `measurement`: a process of `bench` of its own, and the readings it printed, or why it failed, in which case nothing is filed.
fn sat(bench: &Path, measurement: &str, root: &Path) -> Result<Vec<Line>, String> {
    let ran = Command::new(bench)
        .args(["sit", measurement])
        .current_dir(root)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .output()
        .map_err(|error| format!("{} did not start: {error}", bench.display()))?;
    if !ran.status.success() {
        return Err(format!(
            "{measurement} failed, so nothing is filed: {}",
            ran.status
        ));
    }

    String::from_utf8_lossy(&ran.stdout)
        .lines()
        .filter_map(Line::read)
        .collect()
}

/// Each figure the groups read, its readings by group, in the order they first read them: refused where a figure is read in some invocations and not others, or at two sizes.
fn grouped(sittings: Vec<Vec<Line>>) -> Result<Vec<Figure>, String> {
    let groups = sittings.len();
    let mut figures: Vec<Figure> = Vec::new();
    for (group, lines) in sittings.into_iter().enumerate() {
        for line in lines {
            let at = match figures
                .iter()
                .position(|figure| figure.name == line.name && figure.measure == line.measure)
            {
                Some(at) => at,
                None => {
                    figures.push(Figure {
                        name: line.name.clone(),
                        measure: line.measure,
                        size: line.size,
                        readings: vec![Vec::new(); groups],
                    });
                    figures.len() - 1
                }
            };
            let figure = &mut figures[at];
            if figure.size != line.size {
                return Err(format!(
                    "{} is read at size {:?} and at size {:?}",
                    figure.name, figure.size, line.size
                ));
            }
            figure.readings[group].push(line.value);
        }
    }

    if let Some(figure) = figures
        .iter()
        .find(|figure| figure.readings.iter().any(Vec::is_empty))
    {
        return Err(format!(
            "{} is read in some groups and not in others",
            figure.name
        ));
    }
    if figures.is_empty() {
        return Err("the measurement read no figure".to_string());
    }

    Ok(figures)
}

/// Refuse figures `measurement`'s spread does not describe: one that moves from run to run read by a measurement stated to repeat, which its groups are too few to compare, or none read by one stated to move, which spends groups on readings that repeat.
fn spread_held(measurement: &Measurement, figures: &[Figure]) -> Result<(), String> {
    let moving = figures.iter().find(|figure| figure.measure.moves());

    match (measurement.spread, moving) {
        (Spread::Repeats, Some(figure)) => Err(format!(
            "{} is stated to repeat, and reads {} in {}, which moves from run to run",
            measurement.name,
            figure.name,
            figure.measure.name()
        )),
        (Spread::Moves, None) => Err(format!(
            "{} is stated to move, and every figure it reads repeats",
            measurement.name
        )),
        _ => Ok(()),
    }
}

/// Take a sitting of the measurement `measurement` and file it as its next record, numbered after `filed`, the ones already filed.
pub fn measurement_sitting(measurement: &str, filed: usize) -> Result<(), String> {
    let root = root();

    // Before anything is built: building with the wrong compiler and measuring it afterwards spends the run to produce the wrong record.
    let named = measurement_named(measurement)?;
    let subject = subject(&root)?;
    let pinned = held(&root)?;
    let platform = capture();

    let bench = built(&root)?;
    let mut sittings = Vec::new();
    let groups = named.spread.groups();
    for group in 1..=groups {
        eprintln!(">> {measurement}, group {group} of {groups}");
        sittings.push(sat(&bench, measurement, &root)?);
    }
    let figures = grouped(sittings)?;
    spread_held(named, &figures)?;

    let record = Record {
        taken: today(&asked("date", &["-I"], &root)?)?,
        subject,
        platform,
        pinned,
        figures,
    };

    file(&root, measurement, filed, &record)
}
