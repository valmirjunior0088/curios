//! What an object answers for and what is filed: another project's source and a shared dependency, a baseline for a query or a build, a moved project, an executable's declarations, and the blobs a refiling writes.

use {
    super::test_support::*, crate::verdicts::*, curios_common::fill, curios_package::Cabinet,
    std::path::Path,
};

/// What a build's fold did to the one unit, through the cabinet itself.
fn folded(root: &Path) -> Vec<String> {
    folded_as(root, Compiled::Built)
}

/// [`folded`], with the one unit compiled for `compiled`.
fn folded_as(root: &Path, compiled: Compiled) -> Vec<String> {
    let verdicts = Verdicts::at(own(root));

    folded_through(root, &root.join("shape"), &verdicts, compiled)
}

/// What a query's fold did to the one unit, through the cabinet read as the `wonder` engine reads it.
fn folded_overlaid(root: &Path, overlay: &Overlay) -> Vec<String> {
    folded_overlaid_in(root, overlay, &own(root))
}

/// The same, reading `cabinet`.
fn folded_overlaid_in(root: &Path, overlay: &Overlay, cabinet: &Cabinet) -> Vec<String> {
    let verdicts = Verdicts::at(cabinet.clone());

    folded_through(
        root,
        &root.join("shape"),
        &Overlaid(&verdicts, overlay),
        Compiled::Built,
    )
}

/// Copy the tree `from` into `into`, replacing whatever was there.
fn copied(from: &Path, into: &Path) {
    let _ = fs::remove_dir_all(into);
    fs::create_dir_all(into).unwrap();

    for entry in fs::read_dir(from).expect("a tree with files in it") {
        let entry = entry.unwrap().path();
        let to = into.join(entry.file_name().unwrap());
        match entry.is_dir() {
            true => copied(&entry, &to),
            false => {
                fs::copy(&entry, to).unwrap();
            }
        }
    }
}

/// The dependency's library, saying `word` beside two declarations that never change.
fn library_of_three(word: &str) -> String {
    format!(
        "{}\npub let other: Str = \"other\";\n\npub let third: Str = \"third\";\n",
        library(word)
    )
}

/// How many blobs each pack of the cabinet at `root` holds, fewest first.
fn packed_counts(root: &Path) -> Vec<usize> {
    let mut counts = packs(root)
        .into_iter()
        .map(|pack| {
            let mut file = fs::File::open(pack).unwrap();
            curios_unit::pack_names(&mut file).expect("a pack").len()
        })
        .collect::<Vec<_>>();
    counts.sort();

    counts
}

/// An object filed by one project does not answer for another's source where its files hold other texts, even when both address it identically — which two projects holding a package of one name, compiled by one compiler after one chain, always do: its record's names resolve to this project's files.
///
/// A cabinet is shared wherever one is named for every project a run sees, so this is reachable. `theirs` files first and alone: an object keeps the record its last filing replaced, so a record `mine` filed before would answer for `mine` beside theirs.
#[test]
fn an_object_does_not_answer_for_another_projects_source() {
    let mine = project("mine");
    let theirs = project("theirs");
    let directory = temporary("not-answering-shared");
    let cabinet = shared(&directory);

    write(&theirs, "shape/lib.crs", &library("theirs"));
    reused_in(&theirs, &cabinet);

    assert!(
        !reused_in(&mine, &cabinet),
        "their record names texts my files do not hold"
    );
}

/// The other half of that clause, and the reason it checks containment rather than re-deriving the read set: a dependency materialized once and read from that same path by every project *is* shared, and must still hit.
///
/// The dependency sits outside both projects, standing in for the `sources/` tree `curate` materializes in a shared cabinet — the only way two projects ever read one unit's source from one path. `mine` never compiles it before `theirs` does, so a hit has nowhere to come from but the object `theirs` filed.
#[test]
fn an_object_answers_for_a_dependency_both_projects_read() {
    let materialized = temporary("materialized");
    write(
        &materialized,
        "curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"shape\"\n"
        ),
    );
    write(&materialized, "lib.crs", &library("first"));

    let theirs = temporary("theirs-sharing");
    let mine = temporary("mine-sharing");
    let directory = temporary("dependency-shared");
    let cabinet = shared(&directory);
    write(&theirs, "exe.crs", ENTRY);
    write(&mine, "exe.crs", ENTRY);

    assert!(
        !reused_from(&theirs, &materialized, &cabinet),
        "nothing is stored for the first compile, which files it"
    );
    assert!(
        reused_from(&mine, &materialized, &cabinet),
        "and the record names a path mine reads from too, so the unit crosses"
    );
}

/// The cabinet holds one object per unit rather than one per compile — the property the address exists to have.
#[test]
fn compiling_repeatedly_files_one_object() {
    let root = project("bounded");

    for _ in 0..4 {
        reused(&root);
    }

    let objects = own(&root).object_directories().len();

    assert_eq!(objects, 1, "four compiles of one unit, one object");
}

/// Opening a cabinet is not a decision to file anything in it: the compiler's memo is written by the first object that needs the identity, so a compilation refused before any unit is addressed leaves no `.curios/` behind.
#[test]
fn opening_a_cabinet_writes_nothing_until_a_unit_is_addressed() {
    let root = temporary("untouched");
    fs::create_dir_all(&root).unwrap();

    let verdicts = Verdicts::at(own(&root));
    assert!(
        !root.join(".curios").exists(),
        "opening the cabinet created its directory"
    );

    verdicts.compiler();
    assert!(
        own(&root).compiler().is_file(),
        "asking for the identity did not write the memo"
    );
}

/// A lint stores no copy of its source: it stands in a text of its unit, whose anchors hold that text once, beside the object. So a library that leaves one binder unused files unit files larger than its clean twin's by about the lint, not by the library's text again.
#[test]
fn a_lint_stores_no_copy_of_its_source() {
    let source = |binders: &str| {
        let mut text =
            String::from("use /std/{Nat, Str};\n\npub let message: Str =\n    \"first\";\n");
        for index in 0..200 {
            text.push_str(
                &("\npub let padding_".to_owned() + &index.to_string() + "(n: Nat) -> Nat = n;\n"),
            );
        }
        text.push_str(&("\npub let dropped(".to_owned() + binders + ") -> Nat = n;\n"));
        text
    };
    let filed = |name: &str, library: &str| {
        let root = project(name);
        write(&root, "shape/lib.crs", library);
        folded(&root);

        filed(&root).iter().map(Vec::len).sum::<usize>()
    };
    let clean = source("n: Nat");

    let grown = filed("lint-filed", &source("n: Nat, unused: Nat")) - filed("lint-clean", &clean);

    assert!(
        grown < clean.len() / 4,
        "one lint grew the unit files by {grown} bytes, over a library of {} bytes",
        clean.len()
    );
}

/// A unit held built is the baseline of its source compiled tested, and one held tested the baseline of it built: the two differ by the source's tests alone, so neither compiles whole after the other, and each is then filed under an object of its own.
#[test]
fn a_unit_held_for_one_kind_is_the_baseline_of_the_other() {
    let tested = fill!(
        "${library}\nuse /std/{Test};\n\ntest the_message_holds =\n    Test/assert(\"it holds\", true);\n",
        library = library("first")
    );

    let built_first = project("baseline-built-first");
    write(&built_first, "shape/lib.crs", &tested);
    assert_eq!(folded(&built_first), ["compiling /shape"]);
    assert_eq!(
        folded_as(&built_first, Compiled::Tested),
        ["recompiling /shape"]
    );
    assert_eq!(folded_as(&built_first, Compiled::Tested), ["reused /shape"]);
    assert_eq!(folded(&built_first), ["reused /shape"]);

    let tested_first = project("baseline-tested-first");
    write(&tested_first, "shape/lib.crs", &tested);
    assert_eq!(
        folded_as(&tested_first, Compiled::Tested),
        ["compiling /shape"]
    );
    assert_eq!(folded(&tested_first), ["recompiling /shape"]);
}

/// An object whose files were edited since it was filed is a baseline, for a query and for a build alike: each compiles the unit over it.
#[test]
fn an_edited_object_is_a_baseline_for_a_query_and_for_a_build() {
    let root = project("baseline-edited");
    reused(&root);

    write(&root, "shape/lib.crs", &library("second"));

    assert_eq!(
        folded_overlaid(&root, &Overlay::default()),
        ["recompiling /shape"]
    );
    assert_eq!(folded(&root), ["recompiling /shape"]);
}

/// A unit kept without being filed leaves its object holding what it held; a build files what it compiled.
#[test]
fn a_unit_kept_without_being_filed_leaves_its_object_as_it_was() {
    let root = project("baseline-unfiled");
    reused(&root);
    let before = filed(&root);

    write(&root, "shape/lib.crs", &library("second"));

    assert_eq!(
        folded_overlaid(&root, &Overlay::default()),
        ["recompiling /shape"]
    );
    assert_eq!(filed(&root), before, "keeping a unit rewrote its object");

    assert_eq!(folded(&root), ["recompiling /shape"]);
    assert_ne!(filed(&root), before, "a build files what it compiled");
}

/// Another project's object for a package of the same name is a baseline as any object is: a recompile over it keeps what it lowered from the texts this project still holds, and lowers again what this project wrote otherwise.
#[test]
fn another_projects_object_is_a_baseline() {
    let mine = project("baseline-mine");
    let theirs = project("baseline-theirs");
    let directory = temporary("baseline-shared");
    let cabinet = shared(&directory);

    write(&theirs, "shape/lib.crs", &library("theirs"));
    reused_in(&theirs, &cabinet);

    assert_eq!(
        folded_overlaid_in(&mine, &Overlay::default(), &cabinet),
        ["recompiling /shape"]
    );
}

/// An object another project filed answers for this one where this project's files hold the texts it was compiled from: the unit is the one this project would compile, so a cabinet shared across projects compiles it once.
#[test]
fn an_object_answers_for_another_project_holding_its_texts() {
    let mine = project("sharing-mine");
    let theirs = project("sharing-theirs");
    let directory = temporary("sharing-shared");
    let cabinet = shared(&directory);

    assert!(
        !reused_in(&theirs, &cabinet),
        "nothing is stored for the first compile"
    );
    assert!(reused_in(&mine, &cabinet));
}

/// A project moved to another directory keeps its stored units: a record names each file by its package, and the package's files hold the texts they did wherever it stands.
#[test]
fn a_moved_project_keeps_its_stored_units() {
    let root = project("moved-from");
    assert!(!reused(&root), "nothing is stored for the first compile");

    let moved = temporary("moved-to");
    copied(&root, &moved);

    assert!(reused(&moved));
}

/// A program's declarations are a unit, filed by the executable it is. Two entries each claim the empty prefix and are filed apart, so each finds its own unit where it left it; an edit to one's declarations compiles it over the unit it filed, and leaves the other's where it was.
///
/// Mutation-checked: with an entry filed by the prefix it claims, as any unit is, the second entry's unit takes the first one's place and the first is compiled whole again.
#[test]
fn an_entrys_declarations_are_filed_by_the_executable_it_is() {
    let root = project("entries");
    write(&root, "exe.crs", DECLARING_ENTRY);
    write(&root, "tool.crs", DECLARING_ENTRY);

    assert_eq!(entered(&root, "exe.crs", Some("exe")), "compiled");
    assert_eq!(entered(&root, "tool.crs", Some("tool")), "compiled");
    assert_eq!(
        entered(&root, "exe.crs", Some("exe")),
        "reused",
        "another entry's unit took this one's place"
    );
    assert_eq!(entered(&root, "tool.crs", Some("tool")), "reused");

    write(
        &root,
        "exe.crs",
        &DECLARING_ENTRY.replace("concat(message, message)", "concat(message, \"!\")"),
    );
    assert_eq!(entered(&root, "exe.crs", Some("exe")), "recompiled");
    assert_eq!(entered(&root, "tool.crs", Some("tool")), "reused");
}

/// An executable's object answers only for the file its entry is: a manifest that points the executable at another file finds the object's record naming the first, whose text still stands, and compiles the second's declarations rather than believing the first's.
///
/// Mutation-checked at `agrees`: with the record held only to the files it names, the second file's term is compiled over the first file's declarations.
#[test]
fn an_executables_object_answers_for_no_file_but_the_one_it_was_filed_from() {
    let root = project("entry-moved");
    write(&root, "x.crs", DECLARING_ENTRY);
    write(
        &root,
        "y.crs",
        &DECLARING_ENTRY.replace("concat(message, message)", "concat(message, \"!\")"),
    );

    assert_eq!(entered(&root, "x.crs", Some("prog")), "compiled");
    assert_eq!(entered(&root, "x.crs", Some("prog")), "reused");
    assert_ne!(
        entered(&root, "y.crs", Some("prog")),
        "reused",
        "the object filed from x.crs answered for y.crs"
    );
}

/// A handle told of no executable files no entry's unit: an entry claims the empty prefix whichever program it is, and nothing else says which.
#[test]
fn an_entry_no_executable_names_is_filed_nowhere() {
    let root = project("entry-unnamed");
    write(&root, "exe.crs", DECLARING_ENTRY);

    assert_eq!(entered(&root, "exe.crs", None), "compiled");
    assert_eq!(entered(&root, "exe.crs", None), "compiled");
}

/// A unit compiled over its object files the blobs it made and none it kept: an edit to one declaration's text, of three, writes a pack of that declaration's lowered item, elaborated item, erased form and lowering record, of its file's scan and texts, and of the records of its elaboration, its erasure and its judgment, each named by what it was made from, beside the pack the first filing wrote.
#[test]
fn a_refiling_writes_only_the_blobs_it_made() {
    let root = project("refiled-blobs");
    write(&root, "shape/lib.crs", &library_of_three("first"));
    reused(&root);
    let [first] = packed_counts(&root)[..] else {
        panic!("one pack after one filing");
    };

    write(&root, "shape/lib.crs", &library_of_three("second"));
    assert!(!reused(&root), "an edited unit is compiled again");

    assert_eq!(packed_counts(&root), [9, first]);
    assert!(reused(&root), "and what it filed is restored");
}
