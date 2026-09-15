use {super::*, curios_utilities::test_support::Temporary, std::fs};

/// A tree of `(relative path, contents)` pairs, in a directory of its own that goes away with the test.
fn tree(name: &str, files: &[(&str, &str)]) -> Temporary {
    let root = Temporary::new("govern", name);

    for (path, source) in files {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, source).unwrap();
    }

    fs::create_dir_all(&root).unwrap();

    root
}

/// A package with no umbrella over it governs itself, and is its own governing root.
#[test]
fn a_lone_package_governs_itself() {
    let root = tree(
        "govern-lone",
        &[("curios.toml", "name = \"json\"\n"), ("lib.crs", "")],
    );

    let governing = Governing::of(&root).expect("a package governs the directory it is in");

    assert_eq!(governing.package.name, "json");
    assert!(governing.umbrella.is_none());
    assert!(same_directory(&governing.root, &root));
    assert_eq!(governing.manifest, root.join("curios.toml"));
}

/// **A subdirectory of a package is that package.** The nearest manifest governs, so a command run in a directory holding modules rather than a manifest means the package whose directory holds it — and names the manifest it found there.
#[test]
fn a_subdirectory_of_a_package_is_governed_by_that_package() {
    let root = tree(
        "govern-subdirectory",
        &[
            ("curios.toml", "name = \"json\"\n"),
            ("lib.crs", "pub mod parse;"),
            ("parse/lexer.crs", ""),
        ],
    );

    for directory in ["parse", "."] {
        let governing = Governing::of(&root.join(directory)).expect("the package above");

        assert_eq!(governing.package.name, "json", "{directory}");
        assert!(same_directory(&governing.directory, &root), "{directory}");
        assert_eq!(governing.manifest, root.join("curios.toml"), "{directory}");
    }
}

/// The first manifest found decides, so a package nested in another's directory governs its own directory and everything under it.
#[test]
fn a_nested_package_governs_its_own_directory() {
    let root = tree(
        "govern-nested",
        &[
            ("curios.toml", "name = \"json\"\n"),
            ("lib.crs", ""),
            ("tools/curios.toml", "name = \"tools\"\n"),
            ("tools/lib.crs", ""),
            ("tools/src/extra.crs", ""),
        ],
    );

    for directory in ["tools", "tools/src"] {
        let governing = Governing::of(&root.join(directory)).expect("the nearer package");

        assert_eq!(governing.package.name, "tools", "{directory}");
    }
}

/// A directory with no manifest at or above it is governed by nothing, and the refusal names where the walk began, since that is where the reader stood.
#[test]
fn a_directory_no_manifest_is_above_is_governed_by_nothing() {
    let root = tree("govern-nowhere", &[("scratch.crs", "")]);

    let refusal = Governing::of(&root)
        .map(|_| ())
        .expect_err("no manifest at or above the tree");
    assert!(
        refusal.contains(&format!(
            "no `curios.toml` in {} or any directory above it",
            root.display()
        )),
        "{refusal}"
    );
}

/// An umbrella governs what it enumerates, and its directory is where the store goes.
#[test]
fn an_umbrella_governs_a_member_it_enumerates() {
    let root = tree(
        "govern-member",
        &[
            ("curios.toml", "members = [\"json\", \"tools/cli\"]\n"),
            ("json/curios.toml", "name = \"json\"\n"),
            ("json/lib.crs", ""),
            ("tools/cli/curios.toml", "name = \"cli\"\n"),
            ("tools/cli/lib.crs", ""),
        ],
    );

    // Enumeration may point deep, which is what gives deep organization with a flat manifest.
    for (member, name) in [("json", "json"), ("tools/cli", "cli")] {
        let governing = Governing::of(&root.join(member)).expect("an enumerated member");

        assert_eq!(governing.package.name, name);
        assert!(governing.umbrella.is_some(), "{member} is enumerated");
        assert!(same_directory(&governing.root, &root), "{member}");
    }
}

/// Enumeration bounds the walk: a package sitting inside an umbrella's tree that the umbrella does not list is governed by nothing above it.
#[test]
fn an_umbrella_governs_nothing_it_does_not_enumerate() {
    let root = tree(
        "govern-unenumerated",
        &[
            ("curios.toml", "members = [\"json\"]\n"),
            ("json/curios.toml", "name = \"json\"\n"),
            ("json/lib.crs", ""),
            ("scratch/curios.toml", "name = \"scratch\"\n"),
            ("scratch/lib.crs", ""),
        ],
    );

    let governing = Governing::of(&root.join("scratch")).expect("an unenumerated package");

    assert!(governing.umbrella.is_none());
    assert!(same_directory(&governing.root, &root.join("scratch")));
}

/// An umbrella is not a package, so standing in its tree outside every member there is nothing for `run` to compile — the walk stops at the umbrella rather than passing it.
#[test]
fn an_umbrella_root_is_governed_by_no_package() {
    let root = tree(
        "govern-umbrella-root",
        &[
            ("curios.toml", "members = [\"json\"]\n"),
            ("json/curios.toml", "name = \"json\"\n"),
            ("json/lib.crs", ""),
            ("notes/plan.md", ""),
        ],
    );

    for directory in [".", "notes"] {
        let refusal = Governing::of(&root.join(directory))
            .map(|_| ())
            .expect_err("an umbrella declares no package");
        // A manifest *is* there, so the refusal names what it declares rather than reporting one missing.
        assert!(
            refusal.contains("declares an umbrella"),
            "{directory}: {refusal}"
        );
    }
}

/// An entry in `members` that no manifest answers is the umbrella's fault, and the refusal says so: the umbrella's manifest, the entry as written, and where it looked — never the operating system's word for a file the reader did not spell.
#[test]
fn a_member_with_no_manifest_is_refused_against_the_umbrella() {
    let root = tree(
        "govern-member-missing",
        &[
            ("curios.toml", "members = [\"app\", \"bsae\"]\n"),
            (
                "app/curios.toml",
                "name = \"app\"\n\n[dependencies]\nbase = { source = \"member\" }\n",
            ),
            ("app/lib.crs", ""),
            ("base/curios.toml", "name = \"base\"\n"),
            ("base/lib.crs", ""),
        ],
    );

    let refusal = Governing::of(&root.join("app"))
        .map(|_| ())
        .expect_err("a member nothing declares");
    assert!(
        refusal.contains("enumerates the member \"bsae\""),
        "{refusal}"
    );
    assert!(refusal.contains("no `curios.toml` sits in"), "{refusal}");
    assert!(!refusal.contains("os error"), "{refusal}");
}

/// One member listed twice is one member, whatever the two spellings, and the refusal says the umbrella listed it twice — never that two members declare the name, which would send the reader after a second package that does not exist.
#[test]
fn a_member_listed_twice_is_refused_as_listed_twice() {
    let root = tree(
        "govern-member-twice",
        &[
            ("curios.toml", "members = [\"base\", \"./base\", \"app\"]\n"),
            (
                "app/curios.toml",
                "name = \"app\"\n\n[dependencies]\nbase = { source = \"member\" }\n",
            ),
            ("app/lib.crs", ""),
            ("base/curios.toml", "name = \"base\"\n"),
            ("base/lib.crs", ""),
        ],
    );

    let refusal = Governing::of(&root.join("app"))
        .map(|_| ())
        .expect_err("one member listed twice");
    assert!(
        refusal.contains("lists the member \"base\" twice, as \"base\" and as \"./base\""),
        "{refusal}"
    );
    assert!(!refusal.contains("two members"), "{refusal}");
}
