//! What the formatter's suites share: a source formatted to its canonical text, and the sources of the tree.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use {
    super::*,
    curios_common::Source,
    std::path::{Path, PathBuf},
};

/// The canonical text in the default format, either way: production callers match the variants directly, since changed-ness decides the exit path, and only these round-trip tests want the text alone.
pub fn formatted(source: &str) -> String {
    formatted_within(source, Format::default())
}

/// [`formatted`] within `format`.
pub fn formatted_within(source: &str, format: Format) -> String {
    match Formatted::from_source(&Source::inline(source), WrittenIn::Program, format)
        .expect("fixture formats")
    {
        Formatted::Unchanged(text) | Formatted::Changed(text) => text,
    }
}

/// The format of lines `columns` wide, indented and keeping a match on one line as the defaults do.
pub fn at(columns: usize) -> Format {
    let line_width = Width::new(columns).expect("the line width is admitted");
    Format {
        line_width,
        indent_width: Indent::default(),
        flat_match: FlatMatch::within(line_width, None, None).expect("the defaults fit every line"),
    }
}

/// Every format a file may be formatted within, at the extremes: at each admitted line width and indentation, a match kept on one line by the defaults and at each extreme the settings admit — kept off every line, as wide as the line with the fewest arms, and as wide as the line with the most.
pub fn admitted_formats() -> Vec<Format> {
    LINE_WIDTHS
        .into_iter()
        .flat_map(|columns| {
            let line_width = Width::new(columns).expect("the line width is admitted");
            let within = move |width, arms| {
                FlatMatch::within(line_width, Some(width), Some(arms))
                    .expect("the extreme is admitted")
            };
            [
                FlatMatch::within(line_width, None, None).expect("the defaults fit every line"),
                within(0, DEFAULT_FLAT_MATCH_ARMS),
                within(columns, MIN_FLAT_MATCH_ARMS),
                within(columns, MAX_FLAT_MATCH_ARMS),
            ]
            .map(|flat_match| (line_width, flat_match))
        })
        .flat_map(|(line_width, flat_match)| {
            INDENT_WIDTHS.map(|columns| Format {
                line_width,
                indent_width: Indent::new(columns).expect("the indentation is admitted"),
                flat_match,
            })
        })
        .collect()
}

/// `check` run at every format of [`admitted_formats`], each on a thread of its own, so a sweep of the tree costs its slowest format's run rather than every format's in turn: what each answers, in the formats' order, and a panic in any raised again as it was.
pub fn at_every_admitted_format<T: Send>(check: impl Fn(Format) -> T + Sync) -> Vec<T> {
    let check = &check;
    std::thread::scope(|scope| {
        admitted_formats()
            .into_iter()
            .map(|format| scope.spawn(move || check(format)))
            .collect::<Vec<_>>()
            .into_iter()
            .map(|run| {
                run.join()
                    .unwrap_or_else(|panic| std::panic::resume_unwind(panic))
            })
            .collect()
    })
}

/// Every `.crs` of the tree, by its path, with the text it holds.
pub fn sources_of_the_tree() -> Vec<(PathBuf, String)> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("..");
    let mut paths = Vec::new();
    collect_sources(&root, &mut paths);
    assert!(
        paths.len() > 100,
        "found only {} sources under {}",
        paths.len(),
        root.display()
    );

    paths
        .into_iter()
        .map(|path| {
            let text = std::fs::read_to_string(&path).expect("a readable source");
            (path, text)
        })
        .collect()
}

/// Every `.crs` under `directory`, skipping the directories [`skipped`] names.
pub fn collect_sources(directory: &Path, sources: &mut Vec<PathBuf>) {
    for entry in std::fs::read_dir(directory).expect("the tree is readable") {
        let path = entry.expect("a readable entry").path();
        if path.is_dir() {
            if !skipped(&path) {
                collect_sources(&path, sources);
            }
        } else if path.extension().is_some_and(|extension| extension == "crs") {
            sources.push(path);
        }
    }
}

/// Whether the directory at `path` holds no text the formatter is held to: a generated or foreign tree; `editors/`, whose files are highlighting fixtures for the editor grammars; and the soundness board's archive, whose witnesses are written in constructs the language no longer has and must not parse.
pub fn skipped(path: &Path) -> bool {
    let name = path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("");

    matches!(
        name,
        "target" | ".git" | ".curios" | ".claude" | "node_modules" | "editors"
    ) || path.ends_with("xboard/archive")
}

/// Every form Core text writes beyond a program's, in the layout the program's printer gives it: a decorator beside its operand where the two fit their line and above it otherwise, a declaration under its absolute name, a group of nominal declarations of one kind and one of definitions, each joined by `and`, items a blank line apart, and the entry's type on the line beneath it.
pub const EVERY_FORM: &str = "\
induct /Box(A: Type): pub Type
| mk(x: A)
end
#{module = /}
and induct /Crate(A: Type): pub Type
| crate(x: A)
end

struct /Pair(A: Type): pub Type {
    fst: A,
    snd: A,
}

let /f: (n: #NatType) -> #NatType =
    (n: #NatType) => #NatAdd(n, 1)
and /g: #NatType =
    /f(0);

#{module = /, universe_context = {parameter_count = 1, constraints = [u + 1 <= w]}}
let /p: #{level = u} Type =
    (#{node = Name} /f)(#{node = NumLit} 2);

test /held =
    1;

let /m/witness~/std/ops/Add(Nat)#1: (use w: /C()) -> #BoolType =
    /m/witness~3;

/Wit/w(
    #NatAdd(n, 1),
    #CellPoll(T, c, {levels = [u], qualities = [Param(0)]}),
    (#BoolType)(x),
    #{a = (x) => x} (y + 1))
: Prop
";
