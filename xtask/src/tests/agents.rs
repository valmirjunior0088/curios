//! What the agent files hold of the tree: `CLAUDE.md` and every Markdown file under `.claude/`, with the hunter's guards beside them.
//!
//! A rule's globs decide where it loads, a citation is where a session is sent, and a guard is what stands between a hunter and the tree. Each is text no compiler reads, so a moved file or a pattern that reads a command line otherwise than the shell does fails nothing until it is put to the tree here. The judge of a hunter's command line is put to a table, and the box its hook runs a command in is put to the kernel, where `bwrap` makes one.

use {
    crate::*,
    std::{
        collections::BTreeSet,
        fs,
        io::Write,
        path::Path,
        process::{Command, Stdio},
    },
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

/// `text` as a JSON string, as a hook's input spells a command line or a path.
fn json(text: &str) -> String {
    let mut spelled = String::from("\"");
    for character in text.chars() {
        match character {
            '"' => spelled.push_str("\\\""),
            '\\' => spelled.push_str("\\\\"),
            '\n' => spelled.push_str("\\n"),
            '\t' => spelled.push_str("\\t"),
            other => spelled.push(other),
        }
    }
    spelled.push('"');

    spelled
}

/// Whether the hunter's hook `script` admits a tool call whose input holds `value` at `field`, by the code a hook ends with: 0 admits and 2 refuses.
fn admits(script: &str, field: &str, value: &str) -> bool {
    let input = ["{\"tool_input\":{\"", field, "\":", &json(value), "}}"].concat();
    let mut hook = Command::new("bash")
        .arg(root().join(".claude/agents/hunter").join(script))
        .env("CLAUDE_PROJECT_DIR", root())
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("bash runs the hook");
    hook.stdin
        .take()
        .expect("the hook reads its input")
        .write_all(input.as_bytes())
        .expect("the hook takes its input");

    match hook.wait().expect("the hook ends").code() {
        Some(0) => true,
        Some(2) => false,
        code => panic!("`{script}` ended with {code:?}"),
    }
}

/// What a row of a guard's table expects.
const ADMITTED: bool = true;
const REFUSED: bool = false;

/// The rows of `table` the hook `script` judges otherwise than the row says, `ROOT` in a row standing for the repository's root.
fn misjudged(script: &str, field: &str, table: &[(bool, &str, &str)]) -> Vec<String> {
    let root = root().to_str().expect("the root is written in UTF-8");

    table
        .iter()
        .filter(|(expected, _, value)| {
            admits(script, field, &value.replace("ROOT", root)) != *expected
        })
        .map(|(expected, row, _)| {
            let verdict = if *expected { "admitted" } else { "refused" };

            format!("{row}: not {verdict}")
        })
        .collect()
}

/// A hunter's shell runs the readers and the probes a hunt needs and nothing else: the judge admits each command line of the first rows, and refuses each of the rest, where the shell would run a program the guard never judged, hand one a flag it refuses, or write a file. The guard reads a command line as the shell does or refuses it: a heredoc is `<<'NAME'` alone, whose body is data, a backslash before a newline joins two lines, and a redirection is admitted only where its word ends.
#[test]
fn a_hunter_s_shell_runs_what_its_guard_judged_and_nothing_else() {
    let table = [
        (ADMITTED, "a reader", "cat CLAUDE.md"),
        (ADMITTED, "a line range", "sed -n 1,5p CLAUDE.md"),
        (
            ADMITTED,
            "a pipe of readers",
            "git log --oneline -3 | head -2",
        ),
        (
            ADMITTED,
            "a list of readers",
            "wc -l CLAUDE.md && ls xtask; pwd",
        ),
        (ADMITTED, "an input redirection", "wc -l < CLAUDE.md"),
        (ADMITTED, "a bounded reader", "timeout 5 rg guard CLAUDE.md"),
        (
            ADMITTED,
            "a quoted word holding operators",
            "grep 'a; b > c' CLAUDE.md",
        ),
        (
            ADMITTED,
            "a backslash kept inside double quotes",
            "grep \"a\\.b\" CLAUDE.md",
        ),
        (
            ADMITTED,
            "an escaped quote inside double quotes",
            "grep \"say \\\"hi\\\"\" CLAUDE.md",
        ),
        (
            ADMITTED,
            "a line joined between two words",
            "cat \\\nCLAUDE.md",
        ),
        (
            ADMITTED,
            "a line joined inside double quotes",
            "echo \"a\\\nb\"",
        ),
        (ADMITTED, "a merge of two descriptors", "cat CLAUDE.md 2>&1"),
        (
            ADMITTED,
            "a merge, then a pipe",
            "cat CLAUDE.md 2>&1|head -1",
        ),
        (ADMITTED, "the null device", "cat CLAUDE.md > /dev/null"),
        (
            ADMITTED,
            "the null device for one descriptor",
            "cat CLAUDE.md 2>/dev/null; pwd",
        ),
        (
            ADMITTED,
            "a probe of a snapshot's compiler",
            "ROOT/.claude/snapshots/0abc123/binaries/curios run - <<'CRS'\n/std/print(\"ok\")\nCRS",
        ),
        (
            ADMITTED,
            "a probe of a snapshot's board",
            "ROOT/.claude/snapshots/0abc123/binaries/xboard probe - <<'CRS'\ntext\nCRS",
        ),
        (
            ADMITTED,
            "a heredoc, then a reader",
            "cat <<'CRS'\ntext\nCRS\ncat CLAUDE.md",
        ),
        (
            ADMITTED,
            "a heredoc whose body holds a substitution",
            "cat <<'CRS'\n$(touch x) costs $5 => `y`\nCRS",
        ),
        (
            ADMITTED,
            "a heredoc whose body ends a line in a backslash",
            "cat <<'CRS'\nCR\\\nS\nCRS",
        ),
        (
            ADMITTED,
            "a heredoc under a name in double quotes",
            "cat <<\"CRS\"\ntext\nCRS",
        ),
        (
            ADMITTED,
            "two heredocs on one line",
            "cat <<'A' <<'B'\none\nA\ntwo\nB",
        ),
        (
            ADMITTED,
            "a heredoc and a pipe",
            "cat <<'CRS' | head -1\ntext\nCRS",
        ),
        (
            ADMITTED,
            "a heredoc whose body holds its name indented",
            "cat <<'CRS'\n  CRS\nbody\nCRS",
        ),
        (
            ADMITTED,
            "a heredoc after a space",
            "cat << 'CRS'\ntext\nCRS",
        ),
        (REFUSED, "a writer", "touch x"),
        (REFUSED, "a build", "cargo build"),
        (REFUSED, "a directory entered", "cd xtask"),
        (
            REFUSED,
            "a compiler outside every snapshot",
            "ROOT/target/debug/curios run -",
        ),
        (
            REFUSED,
            "a snapshot's compiler asked to build",
            "ROOT/.claude/snapshots/0abc123/binaries/curios compile -",
        ),
        (REFUSED, "a redirection", "cat CLAUDE.md > x"),
        (
            REFUSED,
            "an appending redirection",
            "cat CLAUDE.md >>/dev/null",
        ),
        (REFUSED, "a substitution", "cat $(touch x)"),
        (REFUSED, "a parameter", "cat $HOME/x"),
        (REFUSED, "a detached command", "cat CLAUDE.md &"),
        (REFUSED, "sed writing in place", "sed -i s/a/b/ CLAUDE.md"),
        (REFUSED, "sort writing a file", "sort -o x CLAUDE.md"),
        (
            REFUSED,
            "git asked for an output file",
            "git log --output=x -1",
        ),
        (
            REFUSED,
            "git grep's pager, by its short flag",
            "git grep -Otouch -l guard",
        ),
        (
            REFUSED,
            "git grep's pager, by its long flag",
            "git grep --open-files-in-pager=touch -l guard",
        ),
        (
            REFUSED,
            "git grep's pager, with no value",
            "git grep --open-files-in-pager -l guard",
        ),
        (REFUSED, "rg's preprocessor", "rg --pre cat guard CLAUDE.md"),
        (
            REFUSED,
            "rg's hostname program",
            "rg --hyperlink-format=default --hostname-bin=/usr/bin/true guard CLAUDE.md",
        ),
        (
            REFUSED,
            "a writer after a heredoc",
            "cat <<'CRS'\ntext\nCRS\ntouch x",
        ),
        (REFUSED, "a here-string", "cat <<< CRS\ntouch x"),
        (
            REFUSED,
            "a heredoc under a bare name",
            "cat <<CRS\ntext\nCRS",
        ),
        (
            REFUSED,
            "a heredoc that strips tabs",
            "cat <<-'CRS'\n\ttext\n\tCRS",
        ),
        (
            REFUSED,
            "a heredoc under a name with a dash",
            "cat <<CRS-1\ntext\nCRS-1\ntouch x",
        ),
        (
            REFUSED,
            "a heredoc under two quoted words",
            "cat <<'C R S'\ntext\nC R S\ntouch x",
        ),
        (
            REFUSED,
            "a heredoc under a name glued to a quote",
            "cat <<\"A\"B\ntext\nAB\ntouch x",
        ),
        (
            REFUSED,
            "a heredoc under a quoted name glued to a backslash",
            "cat <<'CRS'\\x\nCRSx\ntouch x\nCRS",
        ),
        (
            REFUSED,
            "a heredoc under an escaped name",
            "cat <<\\CRS\ntext\nCRS",
        ),
        (
            REFUSED,
            "a heredoc's opening inside double quotes",
            "echo \"<<CRS\"\ntouch x",
        ),
        (
            REFUSED,
            "a heredoc's opening inside single quotes",
            "echo '<<CRS'\ntouch x",
        ),
        (
            REFUSED,
            "a heredoc's opening in a comment",
            "cat CLAUDE.md # <<'CRS'\ntouch x",
        ),
        (
            REFUSED,
            "a substitution in a body the shell expands",
            "cat <<CRS\n$(touch x)\nCRS",
        ),
        (
            REFUSED,
            "a name joined by a backslash in a body the shell expands",
            "cat <<CRS\na\nCR\\\nS\ntouch x\nCRS",
        ),
        (REFUSED, "a heredoc never closed", "cat <<'CRS'\ntext"),
        (REFUSED, "a heredoc opened on the last line", "cat <<'CRS'"),
        (
            REFUSED,
            "a flag behind a joined line",
            "sort \\\n-o x CLAUDE.md",
        ),
        (
            REFUSED,
            "a flag split by a joined line",
            "git log --out\\\nput=x -1",
        ),
        (
            REFUSED,
            "a flag split by a joined line inside double quotes",
            "git log \"--out\\\nput=x\" -1",
        ),
        (
            REFUSED,
            "a writer behind a joined line",
            "cat CLAUDE.md;\\\ntouch x",
        ),
        (
            REFUSED,
            "a writer after a comment ending in a backslash",
            "cat CLAUDE.md # note \\\ntouch x",
        ),
        (
            REFUSED,
            "a backslash that escapes nothing",
            "cat CLAUDE.md\\",
        ),
        (
            REFUSED,
            "words past a closing brace, which the shell hands the program before it",
            "sort CLAUDE.md } cat -o x",
        ),
        (
            REFUSED,
            "words past an opening brace",
            "sort CLAUDE.md { cat -o x",
        ),
        (
            REFUSED,
            "a flag spelled by a brace expansion",
            "git log --out{put,}=x -1",
        ),
        (REFUSED, "a brace group", "{ cat CLAUDE.md; }"),
        (
            REFUSED,
            "a merge that is a file's name",
            "cat CLAUDE.md >&1x",
        ),
        (
            REFUSED,
            "a merge joined to a name",
            "cat CLAUDE.md >&1\\\nx",
        ),
        (
            REFUSED,
            "a merge that moves a descriptor",
            "cat CLAUDE.md >&1-",
        ),
        (
            REFUSED,
            "a file named after the null device",
            "cat CLAUDE.md >/dev/nullx",
        ),
        (
            REFUSED,
            "the null device joined to a name",
            "cat CLAUDE.md >/dev/null\\\nx",
        ),
    ];

    let misjudged = misjudged("judge.sh", "command", &table);

    assert!(misjudged.is_empty(), "{misjudged:#?}");
}

/// A hunter writes only inside its own directory of a snapshot, by the path as it resolves: the tree, a snapshot's files and binaries, another's directory and a path that climbs out are each refused.
#[test]
fn a_hunter_writes_only_inside_its_directory_of_a_snapshot() {
    let table = [
        (
            ADMITTED,
            "a file of its directory",
            "ROOT/.claude/snapshots/0abc123/hunters/hunter-1/notes.md",
        ),
        (
            ADMITTED,
            "a file deeper in it",
            "ROOT/.claude/snapshots/0abc123/hunters/hunter-1/probes/a.crs",
        ),
        (REFUSED, "a file of the tree", "ROOT/CLAUDE.md"),
        (
            REFUSED,
            "a file of the snapshot's tree",
            "ROOT/.claude/snapshots/0abc123/tree/CLAUDE.md",
        ),
        (
            REFUSED,
            "a snapshot's compiler",
            "ROOT/.claude/snapshots/0abc123/binaries/curios",
        ),
        (
            REFUSED,
            "a path that climbs out of the directory",
            "ROOT/.claude/snapshots/0abc123/hunters/hunter-1/../../binaries/curios",
        ),
        (
            REFUSED,
            "a directory that is no hunter's",
            "ROOT/.claude/snapshots/0abc123/hunters/other/x",
        ),
        (
            REFUSED,
            "a snapshot that is no commit",
            "ROOT/.claude/snapshots/not-hex/hunters/hunter-1/x",
        ),
        (
            REFUSED,
            "the directory itself",
            "ROOT/.claude/snapshots/0abc123/hunters/hunter-1",
        ),
        (REFUSED, "no path", ""),
    ];

    let misjudged = misjudged("write-guard.sh", "file_path", &table);

    assert!(misjudged.is_empty(), "{misjudged:#?}");
}

/// The command line the hunter's hook hands back for `command`, with `project` as its project: what the harness runs in the place of the one it was asked.
fn handed_back(project: &Path, command: &str) -> String {
    let input = ["{\"tool_input\":{\"command\":", &json(command), "}}"].concat();
    let mut hook = Command::new("bash")
        .arg(root().join(".claude/agents/hunter/guard.sh"))
        .env("CLAUDE_PROJECT_DIR", project)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("bash runs the hook");
    hook.stdin
        .take()
        .expect("the hook reads its input")
        .write_all(input.as_bytes())
        .expect("the hook takes its input");
    let answered = hook.wait_with_output().expect("the hook ends");
    assert!(answered.status.success(), "the hook admits `{command}`");

    let mut read = Command::new("jq")
        .args(["-r", ".hookSpecificOutput.updatedInput.command"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("jq reads the hook's answer");
    read.stdin
        .take()
        .expect("jq reads its input")
        .write_all(&answered.stdout)
        .expect("jq takes its input");
    let read = read.wait_with_output().expect("jq ends");

    String::from_utf8(read.stdout)
        .expect("a command line is text")
        .trim_end()
        .to_string()
}

/// A hunter's command runs where the kernel holds its writes: the hook hands the command back inside a box, under bash, and in that box a write lands in a hunter's directory of a snapshot and is refused anywhere else, its project's own tree included. Held where `bwrap` makes a box: a machine without it, or one that forbids the namespaces it takes, runs no hunt.
#[test]
fn a_hunter_s_command_runs_where_the_kernel_holds_its_writes() {
    let boxes = Command::new("bwrap")
        .args(["--ro-bind", "/", "/", "true"])
        .stderr(Stdio::null())
        .status()
        .is_ok_and(|status| status.success());
    if !boxes {
        return;
    }

    let project = target_directory()
        .join("tmp")
        .join(["hunter-box-", &std::process::id().to_string()].concat());
    let hunter = project.join(".claude/snapshots/0abc123/hunters/hunter-1");
    fs::create_dir_all(&hunter).expect("the project is made");

    let handed = handed_back(&project, "true");
    let within = handed
        .strip_suffix("-- bash -c 'true'")
        .expect("the box ends in the command, under bash");
    let inside = hunter.join("inside");
    let outside = project.join("outside");
    let writes = [
        "touch '",
        &inside.to_string_lossy(),
        "' && ! touch '",
        &outside.to_string_lossy(),
        "' 2>/dev/null",
    ]
    .concat();
    let ran = Command::new("bash")
        .arg("-c")
        .arg([within, "-- bash -c \"", &writes, "\""].concat())
        .status()
        .expect("bash runs the boxed command");

    let landed = (ran.success(), inside.exists(), outside.exists());
    fs::remove_dir_all(&project).expect("the project is removed");
    assert_eq!(landed, (true, true, false));
}

/// The hook asks the judge before it boxes anything: a command line the judge refuses is refused by the hook, whether or not a box can be made.
#[test]
fn a_hunter_s_hook_refuses_what_its_judge_refuses() {
    assert!(!admits("guard.sh", "command", "touch x"));
}
