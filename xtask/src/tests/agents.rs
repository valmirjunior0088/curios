//! What the agent files hold of the tree: `CLAUDE.md` and every Markdown file under `.claude/`.
//!
//! A rule's globs decide where it loads, and a citation is where a session is sent. Each is text no compiler reads, so a moved file fails nothing until it is put to the tree here.

use {
    crate::*,
    std::{collections::BTreeSet, fs, path::Path, process::Command},
};

/// Every file the repository tracks, by its path from the root.
fn tracked() -> Vec<String> {
    ask(Command::new("git"), &["ls-files"])
        .expect("git lists the tracked files")
        .lines()
        .map(str::to_string)
        .collect()
}

/// The agent files among `tracked`: `CLAUDE.md` and each Markdown file under `.claude/`, with its text.
fn agent_files(tracked: &[String]) -> Vec<(&str, String)> {
    tracked
        .iter()
        .filter(|path| {
            *path == "CLAUDE.md" || (path.starts_with(".claude/") && path.ends_with(".md"))
        })
        .map(|path| {
            let text = fs::read_to_string(root().join(path)).expect("a tracked file is read");

            (path.as_str(), text)
        })
        .collect()
}

/// Every directory that holds a tracked file, by its path from the root.
fn directories(tracked: &[String]) -> BTreeSet<&str> {
    tracked
        .iter()
        .flat_map(|path| path.match_indices('/').map(|(at, _)| &path[..at]))
        .collect()
}

/// The globs a rule's front matter lists under `paths:`, and the text past the front matter.
fn globs_and_body(text: &str) -> (Vec<&str>, &str) {
    let Some(matter) = text
        .strip_prefix("---\n")
        .and_then(|rest| rest.split_once("\n---\n"))
    else {
        return (Vec::new(), text);
    };
    let globs = matter
        .0
        .lines()
        .skip_while(|line| *line != "paths:")
        .skip(1)
        .map_while(|line| line.trim_start().strip_prefix("- "))
        .map(|glob| glob.trim_matches('"'))
        .collect();

    (globs, matter.1)
}

/// Whether `glob` matches `path`: `**` crosses directories, `*` stays inside one.
fn matches(glob: &str, path: &str) -> bool {
    if let Some(rest) = glob.strip_prefix("**/") {
        return matches(rest, path)
            || path
                .match_indices('/')
                .any(|(at, _)| matches(rest, &path[at + 1..]));
    }
    if let Some(rest) = glob.strip_prefix("**") {
        return (0..=path.len())
            .filter(|at| path.is_char_boundary(*at))
            .any(|at| matches(rest, &path[at..]));
    }
    if let Some(rest) = glob.strip_prefix('*') {
        let segment = path.find('/').unwrap_or(path.len());

        return (0..=segment)
            .filter(|at| path.is_char_boundary(*at))
            .any(|at| matches(rest, &path[at..]));
    }
    match (glob.chars().next(), path.chars().next()) {
        (None, None) => true,
        (Some(expected), Some(found)) if expected == found => {
            matches(&glob[expected.len_utf8()..], &path[found.len_utf8()..])
        }
        _ => false,
    }
}

/// Every code span of `body` outside its fenced blocks: the text between two runs of backticks of one length, on one line.
fn code_spans(body: &str) -> Vec<&str> {
    let mut spans = Vec::new();
    let mut fenced = false;

    for line in body.lines() {
        if line.trim_start().starts_with("```") {
            fenced = !fenced;
            continue;
        }
        if fenced {
            continue;
        }
        let mut rest = line;
        while let Some(start) = rest.find('`') {
            let run = rest[start..].chars().take_while(|c| *c == '`').count();
            let inside = &rest[start + run..];
            let fence = "`".repeat(run);
            let close = inside.match_indices(&fence).find(|(at, _)| {
                !inside[..*at].ends_with('`') && !inside[at + run..].starts_with('`')
            });
            let Some((close, _)) = close else {
                break;
            };
            spans.push(inside[..close].trim());
            rest = &inside[close + run..];
        }
    }

    spans
}

/// Every target of a Markdown link in `body` that names a file rather than an address or an anchor of its own page.
fn link_targets(body: &str) -> Vec<&str> {
    body.match_indices("](")
        .filter_map(|(at, _)| body[at + 2..].split_once(')'))
        .map(|(target, _)| target.split('#').next().unwrap_or(target))
        .filter(|target| !target.is_empty() && !target.contains("://") && !target.contains(' '))
        .collect()
}

/// `path` without its `.` and `..` segments, as a link from `directory` reads it.
fn resolved(directory: &Path, target: &str) -> String {
    let mut segments = Vec::new();
    for segment in directory
        .iter()
        .filter_map(|segment| segment.to_str())
        .chain(target.split('/'))
    {
        match segment {
            "" | "." => {}
            ".." => {
                segments.pop();
            }
            segment => segments.push(segment),
        }
    }

    segments.join("/")
}

/// Whether a code span is written as an identifier of the sources: lowercase words joined by underscores.
fn is_identifier(span: &str) -> bool {
    span.contains('_')
        && span.starts_with(|c: char| c.is_ascii_lowercase())
        && span.split('_').all(|word| {
            !word.is_empty()
                && word
                    .chars()
                    .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit())
        })
}

/// Each glob of a rule's `paths:` matches a file the repository tracks: one that matches none names a place that moved, and the rule has stopped loading for it.
#[test]
fn every_glob_of_a_rule_matches_a_tracked_file() {
    let tracked = tracked();
    let mut stranded = Vec::new();

    for (file, text) in agent_files(&tracked) {
        for glob in globs_and_body(&text).0 {
            if !tracked.iter().any(|path| matches(glob, path)) {
                stranded.push(format!("{file}: {glob}"));
            }
        }
    }

    assert!(stranded.is_empty(), "{stranded:#?}");
}

/// What an agent file cites is in the tree: a link leads to a tracked file or a directory of them; a path of the repository in a code span is tracked, a directory of tracked files, or one the ignore file names, which a build makes; and an identifier in a code span is a word of a tracked source. A path or a name written with a placeholder, a glob or a space cites nothing.
#[test]
fn every_citation_of_an_agent_file_is_in_the_tree() {
    let tracked = tracked();
    let files = tracked.iter().map(String::as_str).collect::<BTreeSet<_>>();
    let directories = directories(&tracked);
    let held = |path: &str| files.contains(path) || directories.contains(path);
    let top = tracked
        .iter()
        .map(|path| path.split('/').next().unwrap_or(path))
        .collect::<BTreeSet<_>>();
    let mut stale = Vec::new();

    for (file, text) in agent_files(&tracked) {
        let body = globs_and_body(&text).1;
        let directory = Path::new(file).parent().unwrap_or(Path::new(""));

        for target in link_targets(body) {
            if !held(&resolved(directory, target)) {
                stale.push(format!("{file}: the link `{target}` leads nowhere"));
            }
        }

        for span in code_spans(body) {
            if span.contains(|c: char| c.is_whitespace() || "<>*{}$|…".contains(c)) {
                continue;
            }
            if is_identifier(span) {
                let found = ask(
                    Command::new("git"),
                    &[
                        "grep", "-q", "-w", "-F", span, "--", "*.rs", "*.crs", "*.toml", "*.js",
                        "*.sh", "*.yml",
                    ],
                );
                if found.is_err() {
                    stale.push(format!("{file}: no source holds the name `{span}`"));
                }
                continue;
            }
            // A path of the repository starts at one of its top-level entries; `/std/Str` is a path of the language, and `origin/main` a branch.
            let path = span.split(':').next().unwrap_or(span).trim_end_matches('/');
            let first = path.split('/').next().unwrap_or(path);
            if !span.contains('/') || !top.contains(first) || held(path) {
                continue;
            }
            // A directory the ignore file names is asked for by a file inside it: one a build has not made yet is no directory to git.
            let inside = [path, "/-"].concat();
            if ask(Command::new("git"), &["check-ignore", path, &inside]).is_err() {
                stale.push(format!("{file}: the path `{span}` is not in the tree"));
            }
        }
    }

    assert!(stale.is_empty(), "{stale:#?}");
}
