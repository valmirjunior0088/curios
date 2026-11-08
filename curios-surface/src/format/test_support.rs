//! What the formatter's suites share: a source formatted to its canonical text, and the sources of the tree.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use {
    super::*,
    curios_common::Source,
    std::path::{Path, PathBuf},
};

/// The canonical text, either way: production callers match the variants directly, since changed-ness decides the exit path, and only these round-trip tests want the text alone.
pub(super) fn formatted(source: &str) -> String {
    match Formatted::from_source(&Source::inline(source)).expect("fixture formats") {
        Formatted::Unchanged(text) | Formatted::Changed(text) => text,
    }
}

/// Every `.crs` under `directory`, skipping the directories that hold generated or foreign trees.
pub(super) fn collect_sources(directory: &Path, sources: &mut Vec<PathBuf>) {
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
pub(super) const EVERY_FORM: &str = "\
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
