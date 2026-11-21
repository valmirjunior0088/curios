use {
    super::*,
    curios_common::test_support::*,
    std::{fs, path::Path},
};

/// A tree of `(relative path, contents)` pairs, in a directory of its own that goes away with the test.
fn tree(name: &str, files: &[(&str, &str)]) -> Temporary {
    let root = Temporary::new("reach", name);
    for (path, contents) in files {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, contents).unwrap();
    }

    root
}

/// A manifest for the package `name`, declaring `rows` as written.
fn package(name: &str, rows: &str) -> String {
    format!(
        "compiler = \"{}\"\nname = \"{name}\"\n{rows}",
        env!("CARGO_PKG_VERSION")
    )
}

/// An umbrella over `members`, each of which inherits its compiler and so states none.
fn umbrella(members: &str) -> String {
    format!(
        "compiler = \"{}\"\nmembers = [{members}]\n",
        env!("CARGO_PKG_VERSION")
    )
}

/// Each package `root` reaches, by its name, with its executables.
fn reached(root: &Path) -> Vec<(String, Vec<String>)> {
    Reach::of(root)
        .expect("a reach read whole")
        .packages
        .into_iter()
        .map(|package| (package.name, package.executables))
        .collect()
}

/// An umbrella's root reaches every member it enumerates, each with its executables, and a package's root reaches the package.
#[test]
fn a_root_reaches_each_member_of_its_umbrella_or_its_package() {
    let root = tree(
        "members",
        &[
            ("curios.toml", &umbrella("\"app\", \"base\"")),
            ("app/curios.toml", "name = \"app\"\n"),
            ("app/lib.crs", ""),
            ("app/exe.crs", "{}\n"),
            ("base/curios.toml", "name = \"base\"\n"),
            ("base/lib.crs", ""),
        ],
    );
    let lone = tree(
        "lone",
        &[("curios.toml", &package("lone", "")), ("lib.crs", "")],
    );

    assert_eq!(
        reached(&root),
        [
            ("app".to_string(), vec!["app".to_string()]),
            ("base".to_string(), vec![])
        ]
    );
    assert_eq!(reached(&lone), [("lone".to_string(), vec![])]);
}

/// A member whose manifest is refused leaves nothing of the reach, since a reach read in part would leave out what that member reaches.
#[test]
fn a_refused_member_reaches_nothing() {
    let root = tree(
        "refused",
        &[
            ("curios.toml", &umbrella("\"app\", \"broken\"")),
            ("app/curios.toml", "name = \"app\"\n"),
            ("app/lib.crs", ""),
            ("broken/curios.toml", "name = \n"),
        ],
    );

    let refused = Reach::of(&root).err().expect("a member refused").render();
    assert!(refused.contains("broken"), "{refused}");
}

/// A package's tests are filed beside its executables, its library's and each executable's under a name no executable takes.
#[test]
fn a_package_files_its_tests_beside_its_executables() {
    let root = tree(
        "tests",
        &[
            (
                "curios.toml",
                &package("app", "\n[executables]\nserve = {}\n"),
            ),
            ("lib.crs", ""),
            ("serve.crs", "{}\n"),
        ],
    );
    let reach = Reach::of(&root).expect("a reach read whole");

    assert_eq!(
        reach.packages[0].payloads(),
        ["serve", "tests/", "tests/serve"]
    );
}
