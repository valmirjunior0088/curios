//! A file of Core text formatted: laid out by the program's printer, its comments kept where they were written, and read without being checked.

use {
    super::{test_support::*, *},
    crate::{CoreFile, to_core},
    curios_common::Source,
    std::path::{Path, PathBuf},
};

/// The canonical text of `text` read as a `.crsc`.
fn core_formatted(text: &str) -> String {
    match Formatted::from_source(&Source::held("a.crsc", text), Width::default())
        .expect("the file formats")
    {
        Formatted::Unchanged(text) | Formatted::Changed(text) => text,
    }
}

#[test]
fn every_form_of_core_text_is_laid_out_by_the_programs_printer() {
    let written = "\
#{absent = [/Box/mk]}
induct /Box(A: Type): pub Type
| #{params = [@Prop]} mk(x: A)
end
#{node = Rec}
and struct /Pair(A: Type): pub Type { fst: A, snd: A }
and /f : (n: #NatType) -> #NatType = (n: #NatType) => #NatAdd(n, 1);
#{module = /, universe_context = {parameter_count = 1, constraints = [u + 1 <= w]}}
let /p : #{level = u} Type = (#{node = Name} /f)(#{node = NumLit} 2);
test /held = 1;
let /m/witness~/std/ops/Add(Nat)#1 : (use w: /C()) -> #BoolType = /m/witness~3;
/Wit/w(#NatAdd(n, 1), #CellPoll(T, c, {levels = [u], qualities = [Param(0)]}), (#BoolType)(x), #{a = (x) => x} (y + 1))
: Prop
";

    assert_eq!(core_formatted(written), EVERY_FORM);
    assert_eq!(
        Formatted::from_source(&Source::held("a.crsc", EVERY_FORM), Width::default()),
        Ok(Formatted::Unchanged(EVERY_FORM.to_string()))
    );
}

/// A decorator states the fields of the operand it is written before, a primary with its call chain: under a call the head is parenthesized, and a larger term under the decorator, so neither reads back as another node's fields.
#[test]
fn a_decorators_operand_keeps_the_parentheses_that_bound_it() {
    for text in [
        "(#{node = Name} /f)(x)\n",
        "#{node = Name} /f(x)\n",
        "#{a = 1} (x + 1)\n",
        "#{a = 1} ((x) => x)\n",
        "(#BoolType)(x)\n",
        "#{a = 1} (#{b = 2} x)\n",
    ] {
        assert_eq!(core_formatted(text), text);
    }
}

/// A comment is placed by the source text, as a program's is: one above an item stays above it, one riding a line stays on it, and one before the entry stays before it.
#[test]
fn a_comment_keeps_its_place_in_core_text() {
    let written = "\
-- Before the first item.
#{absent = [/Box/mk]}
induct /Box(A: Type): pub Type
| #{params = [@Prop]} mk(x: A) -- riding a case
end
let /p : Prop = /q; -- riding a definition
-- Before the entry.
/p
";

    assert_eq!(
        core_formatted(written),
        "\
-- Before the first item.
#{absent = [/Box/mk]}
induct /Box(A: Type): pub Type
| #{params = [@Prop]} mk(x: A) -- riding a case
end

let /p: Prop =
    /q; -- riding a definition

-- Before the entry.
/p
"
    );
}

/// The formatter reads a `.crsc` and checks nothing: a decorator setting a field no node holds is refused by the reader and laid out all the same.
#[test]
fn a_file_its_reader_refuses_is_formatted_all_the_same() {
    let written = "#{surplus = 1} let /x : Type = Type;\n";
    let file = written.parse::<CoreFile>().expect("the file parses");

    assert!(to_core(&file, None).is_err());
    assert_eq!(
        core_formatted(written),
        "#{surplus = 1}\nlet /x: Type =\n    Type;\n"
    );
}

/// Core text is told from a program by the path it was read from: the same text read from a `.crs` is a program, which writes no decorator.
#[test]
fn core_text_is_read_where_the_path_says_so() {
    let refusal = Formatted::from_source(&Source::held("a.crs", "#{a = 1} x\n"), Width::default())
        .expect_err("a program writes no decorator")
        .render();

    assert!(refusal.contains("obtained: `#`"), "{refusal}");
}

/// Every `.crsc` under `directory`, skipping the directories that hold generated or foreign trees, and `editors/`, whose files are highlighting fixtures for the editor grammars, as [`collect_sources`] skips it.
fn core_text_files(directory: &Path, files: &mut Vec<PathBuf>) {
    for entry in std::fs::read_dir(directory).expect("the tree is readable") {
        let path = entry.expect("a readable entry").path();
        let name = path
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("");
        if path.is_dir() {
            if !matches!(
                name,
                "target" | ".git" | ".curios" | ".claude" | "node_modules" | "editors"
            ) {
                core_text_files(&path, files);
            }
        } else if path
            .extension()
            .is_some_and(|extension| extension == "crsc")
        {
            files.push(path);
        }
    }
}

/// **The Core text the tree holds is written as `curios format` writes it**, as its programs are: the board's witnesses.
#[test]
fn every_file_of_core_text_the_tree_holds_is_canonically_formatted() {
    let mut files = Vec::new();
    core_text_files(
        &Path::new(env!("CARGO_MANIFEST_DIR")).join(".."),
        &mut files,
    );
    let wrong = files
        .iter()
        .filter_map(|path| match Formatted::from_path(path, Width::default()) {
            Ok(Formatted::Unchanged(_)) => None,
            Ok(Formatted::Changed(_)) => Some(format!("{}: not canonical", path.display())),
            Err(refusal) => Some(format!(
                "{}: {refusal}",
                path.display(),
                refusal = refusal.render()
            )),
        })
        .collect::<Vec<_>>();

    assert!(!files.is_empty(), "the tree holds Core text");
    assert!(
        wrong.is_empty(),
        "{} of these files are not as `curios format` writes them:\n  {}\n\nrun `cargo run --package curios -- format <file>` on each",
        wrong.len(),
        wrong.join("\n  ")
    );
}
