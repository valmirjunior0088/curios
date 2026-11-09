//! Every function of the shared layer a judgment of this crate calls, listed: the roster.
//!
//! What a verdict reaches of the layer both checkers share is [`ROSTER`]: a function of `curios-analysis`, `curios-core`, `curios-algebra` or `curios-num` enters a judgment's reach by a line added there beside the import that calls it, and the test here fails where a module of the crate imports one the roster does not list, or the roster lists one no module imports. What holds each stands where the function is — the audit of conversion's laws, a differential against the path it shortens, a test that its cap refuses — and the grade it is held to is `documentation/design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md`'s.
//!
//! **A list, and not a path.** Each module imports a shared function from the crate that defines it: a module re-exporting the layer inside the crate would be a second path to names their crates own, which `documentation/design/visibility-is-decided-at-each-hop.md` rules out. The list names the same reach, and each import says where the function is.
//!
//! **Functions, and not the representation.** The types of `curios-core` are what a term *is*, imported wherever one is built or read, and their methods come with them: a term's substitution, a telescope's opening, a level's arithmetic. No judgment fails to reach them, so they are not listed.
//!
//! **One instrument.** `take_looks` is the profile's count of what walks cost, read under the `profile` feature and by no verdict. It is listed so the rule has no exception.

use {
    curios_common::fill,
    std::{
        collections::BTreeSet,
        fs,
        path::{Path, PathBuf},
    },
};

/// Every function of the shared layer a judgment of this crate calls, by its crate's path.
const ROSTER: [&str; 54] = [
    "curios_analysis::another_case",
    "curios_analysis::answers",
    "curios_analysis::answers_classed",
    "curios_analysis::atoms_within",
    "curios_analysis::binder_bounds",
    "curios_analysis::blocker",
    "curios_analysis::case_target_indices",
    "curios_analysis::connectives_agree",
    "curios_analysis::connectives_agree_classed",
    "curios_analysis::convert_classed",
    "curios_analysis::convert_intrinsics",
    "curios_analysis::could_reduce_to",
    "curios_analysis::decide",
    "curios_analysis::foreign_congruence",
    "curios_analysis::grade",
    "curios_analysis::group_applications",
    "curios_analysis::handed",
    "curios_analysis::holds_no_type",
    "curios_analysis::known_to_terminate",
    "curios_analysis::member_arities",
    "curios_analysis::nonzero_by",
    "curios_analysis::one_up_to_instances",
    "curios_analysis::pinned_instance",
    "curios_analysis::positivity_vectors",
    "curios_analysis::read_operands",
    "curios_analysis::read_scrutinee",
    "curios_analysis::read_scrutinee_classed",
    "curios_analysis::reads_solved",
    "curios_analysis::records_case_equation",
    "curios_analysis::refined_by_arm",
    "curios_analysis::refold",
    "curios_analysis::retyped",
    "curios_analysis::scrutinee_solution",
    "curios_analysis::solve_indices",
    "curios_analysis::spine",
    "curios_analysis::struct_reaches_itself",
    "curios_analysis::variance_vectors",
    "curios_analysis::wholly_pinned",
    "curios_analysis::yields_a_sort_where",
    "curios_core::accelerable",
    "curios_core::foreign_operands",
    "curios_core::foreign_signature",
    "curios_core::free_locals_of",
    "curios_core::instantiate_qualities",
    "curios_core::instantiate_universes",
    "curios_core::open_positions_of",
    "curios_core::open_quality",
    "curios_core::probe_spellings",
    "curios_core::reduce_closed",
    "curios_core::reduce_intrinsic",
    "curios_core::rewrite_universe_levels",
    "curios_core::take_looks",
    "curios_core::universe_metas",
    "curios_core::weak_head_value",
];

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

        // A `tests` directory holds tests alone, each named by its subject and not by a suffix.
        if path.is_dir() {
            if name != "tests" {
                sources(&path, found);
            }
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
        functions(&fill!("{${nested}", nested), found);
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

/// The shared functions `source` imports, by their crate's path: what each `use` of a shared crate names, comments left out so a document may still cite one by its path.
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

            let mut named = Vec::new();
            functions(&after[..end], &mut named);
            found.extend(
                named
                    .into_iter()
                    .map(|name| fill!("${shared}::${name}", shared, name)),
            );
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
            "curios_analysis::answers",
            "curios_analysis::deeper",
            "curios_core::refold",
            "curios_core::take_looks",
            "curios_core::reduce_intrinsic"
        ],
    );
}

/// A function of the shared layer enters a judgment's reach through the roster alone, so the list is where a reader finds what a verdict calls and where a change to it is seen; and the roster lists nothing a judgment does not call, so it states the reach and no more.
///
/// Mutation-checked both ways: `curios_analysis::refold` struck from the roster fails this for `kernel/whnf.rs`, and `curios_core::reduce` added to it fails this as imported nowhere.
#[test]
fn the_roster_is_what_the_kernel_imports() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("src");
    let mut found = Vec::new();
    sources(&root, &mut found);
    found.sort();

    let mut imports = BTreeSet::new();
    let strays = found
        .iter()
        .filter_map(|path| {
            let named = imported(&fs::read_to_string(path).expect("a source is read"));
            let off = named
                .iter()
                .filter(|name| !ROSTER.contains(&name.as_str()))
                .cloned()
                .collect::<Vec<_>>();
            imports.extend(named);
            let file = path.strip_prefix(&root).unwrap_or(path).display();

            (!off.is_empty()).then(|| format!("{file}: {}", off.join(", ")))
        })
        .collect::<Vec<_>>();
    let unread = ROSTER
        .into_iter()
        .filter(|name| !imports.contains(*name))
        .collect::<Vec<_>>();

    assert!(
        strays.is_empty(),
        "a shared function is imported off the roster:\n{}",
        strays.join("\n"),
    );
    assert!(
        unread.is_empty(),
        "the roster lists what no module imports: {}",
        unread.join(", "),
    );
}
