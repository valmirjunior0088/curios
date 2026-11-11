//! What the formatter's suites share: a source formatted to its canonical text, and the sources of the tree.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use {
    super::*,
    curios_common::Source,
    std::path::{Path, PathBuf},
};

/// The canonical text at the default width, either way: production callers match the variants directly, since changed-ness decides the exit path, and only these round-trip tests want the text alone.
pub fn formatted(source: &str) -> String {
    formatted_within(source, Width::default())
}

/// [`formatted`] within `width`.
pub fn formatted_within(source: &str, width: Width) -> String {
    match Formatted::from_source(&Source::inline(source), width).expect("fixture formats") {
        Formatted::Unchanged(text) | Formatted::Changed(text) => text,
    }
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

/// Every `.crs` under `directory`, skipping the directories that hold generated or foreign trees.
pub fn collect_sources(directory: &Path, sources: &mut Vec<PathBuf>) {
    for entry in std::fs::read_dir(directory).expect("the tree is readable") {
        let path = entry.expect("a readable entry").path();
        let name = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("");
        if path.is_dir() {
            // `editors/` holds highlighting fixtures for the editor grammars, which are not programs.
            if !matches!(
                name,
                "target" | ".git" | ".curios" | ".claude" | "node_modules" | "editors"
            ) {
                collect_sources(&path, sources);
            }
        } else if path.extension().is_some_and(|extension| extension == "crs") {
            sources.push(path);
        }
    }
}

/// Every form Core text writes beyond a program's, in the layout the program's printer gives it: a decorator beside its operand where the two fit their line and above it otherwise, a declaration under its absolute name, a group of any kinds joined by `and`, items a blank line apart, and the entry's type on the line beneath it.
pub const EVERY_FORM: &str = "\
#{absent = [/Box/mk]}
induct /Box(A: Type): pub Type
| #{params = [@Prop]} mk(x: A)
end
#{node = Rec}
and struct /Pair(A: Type): pub Type {
    fst: A,
    snd: A,
}
and /f: (n: #NatType) -> #NatType =
    (n: #NatType) => #NatAdd(n, 1);

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
