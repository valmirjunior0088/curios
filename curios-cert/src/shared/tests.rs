//! The roster's one rule: no other module of this crate imports a shared function for itself.

use std::{
    fs,
    path::{Path, PathBuf},
};

/// The crates whose functions the roster names.
const SHARED: [&str; 4] = [
    "curios_analysis",
    "curios_core",
    "curios_algebra",
    "curios_num",
];

/// Every source of this crate that a judgment is written in: its tests and their fixtures are left out, since a test names what it exercises.
fn sources(directory: &Path, found: &mut Vec<PathBuf>) {
    let entries = fs::read_dir(directory).expect("the crate's sources are read");

    for entry in entries {
        let path = entry.expect("a directory entry is read").path();
        let name = path
            .file_name()
            .and_then(|name| name.to_str())
            .expect("a source is named in UTF-8")
            .to_string();

        if path.is_dir() {
            sources(&path, found);
        } else if name.ends_with(".rs")
            && name != "tests.rs"
            && name != "test_support.rs"
            && !name.ends_with("_tests.rs")
        {
            found.push(path);
        }
    }
}

/// The functions `items` names, `items` being what follows a shared crate's `::`: one path, or a braced list of them, nested lists included. A function is a path whose last segment is lowercase and that passes through no type, a type's own functions being the representation's.
fn functions(items: &str, found: &mut Vec<String>) {
    let items = items.trim();

    if let Some(listed) = items.strip_prefix('{') {
        let listed = listed.strip_suffix('}').unwrap_or(listed);
        let (mut depth, mut start) = (0usize, 0usize);

        for (at, character) in listed.char_indices() {
            match character {
                '{' => depth += 1,
                '}' => depth = depth.saturating_sub(1),
                ',' if depth == 0 => {
                    functions(&listed[start..at], found);
                    start = at + 1;
                }
                _ => {}
            }
        }
        functions(&listed[start..], found);
    } else if let Some((_, nested)) = items.split_once("::{") {
        functions(&format!("{{{nested}"), found);
    } else if let Some(path) = items.split(" as ").next()
        && !path
            .split("::")
            .any(|segment| segment.starts_with(|first: char| first.is_ascii_uppercase()))
        && let Some(name) = path.rsplit("::").next()
        && name.starts_with(|first: char| first.is_ascii_lowercase())
        && name != "self"
    {
        found.push(name.to_string());
    }
}

/// The shared functions `source` imports: what each `use` of a shared crate names, comments left out so a document may still cite one by its path.
fn imported(source: &str) -> Vec<String> {
    let code = source
        .lines()
        .filter(|line| !line.trim_start().starts_with("//"))
        .collect::<Vec<_>>()
        .join("\n");
    let mut found = Vec::new();

    for shared in SHARED {
        let mut rest = code.as_str();

        while let Some(at) = rest.find(&format!("{shared}::")) {
            let after = &rest[at + shared.len() + 2..];
            let end = match after.starts_with('{') {
                true => {
                    let mut depth = 0usize;
                    after
                        .char_indices()
                        .find_map(|(at, character)| {
                            match character {
                                '{' => depth += 1,
                                '}' => depth -= 1,
                                _ => {}
                            }
                            (depth == 0).then_some(at + 1)
                        })
                        .unwrap_or(after.len())
                }
                false => after
                    .find(|character: char| {
                        !(character.is_alphanumeric() || "_:".contains(character))
                    })
                    .unwrap_or(after.len()),
            };

            functions(&after[..end], &mut found);
            rest = &after[end..];
        }
    }

    found
}

#[test]
fn a_use_names_its_functions_and_not_its_types() {
    let source = "use {\n    super::Error,\n    curios_analysis::{Env, answers, inner::{Deep, deeper}},\n    curios_core::{Term, refold as again},\n};\n// curios_core::reduce_closed, cited\nuse curios_num::Natural;\nuse curios_core::take_looks;\nlet zero = curios_core::Level::zero();\nlet folded = curios_core::reduce_intrinsic(&zero);\n";

    assert_eq!(
        imported(source),
        [
            "answers",
            "deeper",
            "refold",
            "take_looks",
            "reduce_intrinsic"
        ],
    );
}

/// A function of the shared layer enters a judgment's reach through the roster alone, so the list is where a reader finds what a verdict calls and where a change to it is seen.
///
/// Mutation-checked against the roster: `instantiate_universe_levels_scoped` imported from `curios_core` again in `kernel/at.rs` fails this for that file.
#[test]
fn the_kernel_names_a_shared_function_here_alone() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("src");
    let roster = root.join("shared.rs");
    let mut found = Vec::new();
    sources(&root, &mut found);
    found.sort();

    let strays = found
        .iter()
        .filter(|path| **path != roster)
        .filter_map(|path| {
            let named = imported(&fs::read_to_string(path).expect("a source is read"));
            let file = path.strip_prefix(&root).unwrap_or(path).display();

            (!named.is_empty()).then(|| format!("{file}: {}", named.join(", ")))
        })
        .collect::<Vec<_>>();

    assert!(
        strays.is_empty(),
        "a shared function is imported outside the roster:\n{}",
        strays.join("\n"),
    );
}
