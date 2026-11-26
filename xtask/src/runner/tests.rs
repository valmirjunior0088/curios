//! What a run reports, from task files of the test's own whose commands are `true` and `false`, which every platform this crate builds for has.

use {super::*, crate::*, curios_common::fill};

fn tree(steps: &str) -> Tree {
    Tree::parse(&[
        ("xtask/tasks.toml", "what = \"The top\"\n"),
        ("xtask/tasks/a.toml", &format!("what = \"A\"\n{steps}")),
    ])
    .unwrap()
}

fn group() -> Name {
    vec!["a".to_string()]
}

/// A step behind one that did not pass is not run, since what it reads is not there, and a step that reads nothing of the failure goes on: each writes a file where it runs, and the run hands back that not every step passed. Mutation-checked: with no step held back, `behind`'s file is written.
#[test]
fn a_step_behind_one_that_failed_is_not_run_and_the_rest_go_on() {
    let directory =
        std::env::temp_dir().join(format!("curios-xtask-behind-{}", std::process::id()));
    let (behind, apart) = (directory.join("behind"), directory.join("apart"));
    let tree = tree(&format!(
        "[broken]\nwhat = \"B\"\nrun = [[\"false\"]]\n[behind]\nwhat = \"Behind\"\nrequires = [\".a.broken\"]\nrun = [{{ write = \"{}\", contents = \"ran\" }}]\n[apart]\nwhat = \"Apart\"\nrun = [{{ write = \"{}\", contents = \"ran\" }}]\n",
        behind.display(),
        apart.display()
    ));

    let performed = perform(&tree, &group(), &GivenValues::new());
    let (behind_ran, apart_ran) = (behind.exists(), apart.exists());
    let _ = std::fs::remove_dir_all(&directory);

    assert_eq!(performed, Ok(false));
    assert!(!behind_ran, "a step behind one that failed ran");
    assert!(
        apart_ran,
        "a step that reads nothing of the failure did not run"
    );
}

#[test]
fn a_step_that_leaves_no_file_it_produces_fails() {
    let tree = tree(
        "[maker]\nwhat = \"M\"\nproduces = { out = \"xtask/.artifacts/never-written\" }\nrun = [[\"true\"]]\n",
    );

    assert_eq!(perform(&tree, &group(), &GivenValues::new()), Ok(false));
}

#[test]
fn a_run_of_steps_that_pass_passes() {
    let tree = tree(
        "[first]\nwhat = \"F\"\nrun = [[\"true\"], [\"true\"]]\n[second]\nwhat = \"S\"\nrequires = [\".a.first\"]\nrun = [[\"true\"]]\n",
    );

    assert_eq!(perform(&tree, &group(), &GivenValues::new()), Ok(true));
}

#[test]
fn a_write_leaves_its_file_holding_its_contents() {
    let written = std::env::temp_dir()
        .join(format!("curios-xtask-write-{}", std::process::id()))
        .join("written");
    let tree = tree(&format!(
        "[writer]\nwhat = \"W\"\nrun = [{{ write = \"{}\", contents = \"held\" }}]\n",
        written.display()
    ));

    let performed = perform(&tree, &group(), &GivenValues::new());
    let held = std::fs::read_to_string(&written);
    let _ = std::fs::remove_dir_all(written.parent().expect("the file is in a directory"));

    assert_eq!(performed, Ok(true));
    assert_eq!(held.expect("the file is written"), "held");
}

/// A command runs where its `in` says, read from the repository's root, and a write lands under its own `in` where it states one and from the root where it does not, as every path a step states is read: here a shell that finds itself in `xtask`, a write from the root and a write under `xtask/.artifacts`. Mutation-checked: with the command run at the root, the shell's `test` fails.
#[test]
fn an_instruction_stands_where_its_in_says_and_at_the_root_otherwise() {
    let name = format!("curios-xtask-in-{}", std::process::id());
    let landing = format!("xtask/.artifacts/{name}");
    let under = format!("{name}-under");
    let tree = tree(&format!(
        "[placed]\nwhat = \"P\"\nrun = [{{ in = \"xtask\", do = [\"sh\", \"-c\", 'test \"$(basename \"$PWD\")\" = xtask'] }}, {{ write = \"{landing}\", contents = \"held\" }}, {{ in = \"xtask/.artifacts\", write = \"{under}\", contents = \"under\" }}]\n",
    ));

    let performed = perform(&tree, &group(), &GivenValues::new());
    let held = fs::read_to_string(root().join(&landing));
    let held_under = fs::read_to_string(artifacts().join(&under));
    let _ = fs::remove_file(root().join(&landing));
    let _ = fs::remove_file(artifacts().join(&under));

    assert_eq!(performed, Ok(true));
    assert_eq!(held.expect("the write lands from the root"), "held");
    assert_eq!(held_under.expect("the write lands under its `in`"), "under");
}

/// A step runs with the directory of each tool it names that is kept as a directory first on its `PATH`, so a launch inside the tool finds what ships beside it: here a shell that finds the first directory on its `PATH` to be the one holding the tool's executable.
#[test]
fn a_step_runs_with_its_tools_directories_first_on_its_path() {
    let name = format!("shelf-{}", std::process::id());
    let kept = artifacts().join(format!("tools/{name}-1.0"));
    fs::create_dir_all(kept.join("bin")).unwrap();
    let digest = "0".repeat(64);
    let tree = Tree::parse(&[
        (
            "xtask/tasks.toml",
            &fill!(
                r#"what = "The top"
[tools.${name}]
version = "1.0"
url.${HOST_TRIPLE} = "https://example.org/${name}.tar.gz"
sha256.${HOST_TRIPLE} = "${digest}"
path = "bin/${name}"
"#,
                name,
                HOST_TRIPLE,
                digest
            ),
        ),
        (
            "xtask/tasks/a.toml",
            &fill!(
                r#"what = "A"
[s]
what = "S"
run = [["sh", "-c", 'test "$(echo "$PATH" | cut -d: -f1)" = "$(dirname "$0")"', "{.tools.${name}}"]]
"#,
                name
            ),
        ),
    ])
    .unwrap();

    let outcome = perform(&tree, &group(), &GivenValues::new());

    fs::remove_dir_all(&kept).unwrap();
    assert_eq!(outcome, Ok(true));
}

/// A write from a template leaves the template's text with each of its values filled and every other brace kept, a shell's `${HOME}` and `{ …; }` among them. Mutation-checked: with each value read as the host's triple, the variable's value is not written.
#[test]
fn a_write_from_a_template_fills_its_values() {
    let name = format!("curios-xtask-template-{}", std::process::id());
    let template = artifacts().join(format!("{name}.in"));
    let written = artifacts().join(format!("{name}.out"));
    fs::create_dir_all(artifacts()).unwrap();
    fs::write(&template, "v={.a.v} home=${HOME} f() { y; }\n").unwrap();
    let tree = Tree::parse(&[
        ("xtask/tasks.toml", "what = \"The top\"\n"),
        (
            "xtask/tasks/a.toml",
            &format!(
                "what = \"A\"\nv = \"held\"\n[w]\nwhat = \"W\"\nrun = [\n    {{ write = \"{}\", template = \"{}\" }},\n]\n",
                written.display(),
                template.display()
            ),
        ),
    ])
    .unwrap();

    let performed = perform(&tree, &group(), &GivenValues::new());
    let held = fs::read_to_string(&written);
    let _ = fs::remove_file(&template);
    let _ = fs::remove_file(&written);

    assert_eq!(performed, Ok(true));
    assert_eq!(
        held.expect("the write lands"),
        "v=held home=${HOME} f() { y; }\n"
    );
}
