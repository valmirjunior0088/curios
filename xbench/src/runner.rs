//! The runner: a measurement beside the code it measures, taken by name, a group an invocation of its test of its own, and filed as its measurement's next record.
//!
//! **Found by its name.** Cargo builds the workspace's test executables and names each in its messages; the runner asks each which ignored tests it holds and takes the one executable holding the name, a name unique across the workspace.
//!
//! **A group is an invocation.** What a run draws — the keys its maps hash with, the addresses it is handed, its restore of the prelude — differs between processes, so the span between groups is the span between processes; inside one, the harness turns the variants a measurement compares round-robin.
//!
//! **Built in release.** A figure is the compiler a user runs, without the checks a test's build keeps, which move counts as well as times.

#[cfg(test)]
mod tests;

use {
    crate::{Figure, Line, Record, asked, capture, file, held, root, subject, today},
    serde::Deserialize,
    std::{
        path::{Path, PathBuf},
        process::{Command, Stdio},
    },
};

/// The groups a sitting takes, each an invocation: five, the fewest whose ranks separate two records entirely at about p = 0.008, as the decision reads a comparison.
const GROUPS: usize = 5;

/// A test executable cargo built, and the directory of the package it belongs to, which cargo runs it in.
#[derive(Debug)]
struct Executable {
    program: PathBuf,
    directory: PathBuf,
}

/// What cargo says of an artifact it built, as much of it as the runner reads.
#[derive(Deserialize)]
struct Built {
    reason: String,
    #[serde(default)]
    executable: Option<PathBuf>,
    #[serde(default)]
    manifest_path: Option<PathBuf>,
    #[serde(default)]
    profile: Option<Profiled>,
}

/// The build profile cargo built an artifact under, as much of it as says whether the artifact is a test.
#[derive(Deserialize)]
struct Profiled {
    test: bool,
}

/// Build the workspace's test executables in release, and name each with the directory it runs in.
fn executables(root: &Path) -> Result<Vec<Executable>, String> {
    let built = Command::new("cargo")
        .args([
            "test",
            "--workspace",
            "--release",
            "--no-run",
            "--message-format=json-render-diagnostics",
        ])
        .current_dir(root)
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .output()
        .map_err(|error| format!("cargo did not start: {error}"))?;
    if !built.status.success() {
        return Err(format!("the measurements did not build: {}", built.status));
    }

    Ok(String::from_utf8_lossy(&built.stdout)
        .lines()
        .filter_map(|line| serde_json::from_str::<Built>(line).ok())
        .filter_map(|built| {
            let test = built.reason == "compiler-artifact" && built.profile?.test;
            let directory = built.manifest_path?.parent()?.to_path_buf();
            test.then_some(Executable {
                program: built.executable?,
                directory,
            })
        })
        .collect())
}

/// The path of the test `name` in what an executable lists of its ignored tests, where it lists one by that name, its path or its last segment.
fn listed(list: &str, name: &str) -> Option<String> {
    list.lines()
        .filter_map(|line| line.strip_suffix(": test"))
        .find(|path| *path == name || path.ends_with(&format!("::{name}")))
        .map(str::to_string)
}

/// The one executable holding the ignored test `name`, and the test's path in it.
fn found<'a>(
    executables: &'a [Executable],
    name: &str,
) -> Result<(&'a Executable, String), String> {
    let mut holding = Vec::new();
    for executable in executables {
        let list = Command::new(&executable.program)
            .args(["--list", "--ignored"])
            .current_dir(&executable.directory)
            .output()
            .map_err(|error| format!("{} did not start: {error}", executable.program.display()))?;
        if let Some(path) = listed(&String::from_utf8_lossy(&list.stdout), name) {
            holding.push((executable, path));
        }
    }

    match holding.len() {
        1 => Ok(holding.remove(0)),
        0 => Err(format!("no measurement of the workspace is named {name}")),
        _ => Err(format!(
            "{name} names a measurement in each of {}",
            holding
                .iter()
                .map(|(executable, _)| executable.program.display().to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )),
    }
}

/// One invocation of the test at `path`: the readings it printed, or why it failed, in which case nothing is filed.
///
/// It is read from libtest's terse formatter, which writes nothing before a test runs: the pretty one writes `test <path> ... ` with no newline, so the first line the test prints would open behind it and read as no reading, in every invocation alike, and the figure would leave the record with nothing to refuse.
fn invoked(executable: &Executable, path: &str) -> Result<Vec<Line>, String> {
    let ran = Command::new(&executable.program)
        .args([
            "--exact",
            path,
            "--ignored",
            "--nocapture",
            "--quiet",
            "--test-threads=1",
        ])
        .current_dir(&executable.directory)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .output()
        .map_err(|error| format!("{} did not start: {error}", executable.program.display()))?;
    if !ran.status.success() {
        return Err(format!(
            "{path} failed, so nothing is filed: {}",
            ran.status
        ));
    }

    String::from_utf8_lossy(&ran.stdout)
        .lines()
        .filter_map(Line::read)
        .collect()
}

/// Each figure the invocations read, its readings by group, an invocation a group, in the order they first read them: refused where a figure is read in some invocations and not others, or at two sizes.
fn grouped(invocations: Vec<Vec<Line>>) -> Result<Vec<Figure>, String> {
    let groups = invocations.len();
    let mut figures: Vec<Figure> = Vec::new();
    for (group, lines) in invocations.into_iter().enumerate() {
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
            "{} is read in some invocations and not in others",
            figure.name
        ));
    }
    if figures.is_empty() {
        return Err("the measurement read no figure".to_string());
    }

    Ok(figures)
}

/// Take a sitting of the measurement `measurement` and file it as its next record, numbered after `filed`, the ones already filed.
pub fn test_sitting(measurement: &str, filed: usize) -> Result<(), String> {
    let root = root();

    // Before anything is built: building with the wrong compiler and measuring it afterwards spends the run to produce the wrong record.
    let subject = subject(&root)?;
    let pinned = held(&root)?;
    let platform = capture();

    let executables = executables(&root)?;
    let (executable, path) = found(&executables, measurement)?;
    let mut invocations = Vec::new();
    for group in 1..=GROUPS {
        eprintln!(">> {path}, group {group} of {GROUPS}");
        invocations.push(invoked(executable, &path)?);
    }

    let record = Record {
        taken: today(&asked("date", &["-I"], &root)?)?,
        subject,
        platform,
        pinned,
        figures: grouped(invocations)?,
    };

    file(&root, measurement, filed, &record)
}
