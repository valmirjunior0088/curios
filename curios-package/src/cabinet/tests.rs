use {super::*, crate::Products};

/// A hash's scheme is a directory of its own, which is what lets a successor sit beside `c1` rather than replace it.
#[test]
fn a_materialized_tree_files_under_its_scheme() {
    let hash = TreeHash::parse(&format!("c1:{}", "a".repeat(64))).unwrap();

    assert_eq!(
        Cabinet::of(Path::new("/w/u"), None).source(&hash),
        PathBuf::from(format!(
            "/w/u/.curios/cabinet/sources/c1/{}",
            "a".repeat(64)
        ))
    );
}

/// The families never share a namespace, so a package named `c1` cannot land on a scheme's directory, a package named `cabinet` cannot land in the cabinet, and a unit slot cannot land on a payload's.
#[test]
fn the_families_do_not_share_a_namespace() {
    let root = PathBuf::from("/w/u");
    let products = Products::at(root.clone());
    let cabinet = Cabinet::of(&root, None);
    let hash = TreeHash::parse(&format!("c1:{}", "b".repeat(64))).unwrap();

    let module = FileHash::parse(&format!("f1:{}", "b".repeat(64))).unwrap();

    let binary = products.built(Target::Native, "c1", "tool");
    let tree = cabinet.source(&hash);
    let fetched = cabinet.foreign(&module);

    assert!(!binary.starts_with(tree.parent().unwrap()));
    assert_eq!(
        fetched,
        PathBuf::from(format!(
            "/w/u/.curios/cabinet/foreign/f1/{}",
            "b".repeat(64)
        )),
        "a fetched module sits in a family of its own"
    );
    assert!(
        !fetched.starts_with(tree.parent().unwrap()),
        "one digest, a tree and a module, two places"
    );
    for product in [
        products.built(Target::Native, "cabinet", "tool"),
        products.built(Target::Browser, "cabinet", "tool"),
        products.documentation("cabinet"),
    ] {
        assert!(
            !product.starts_with(cabinet.directory()),
            "what a command emits stands beside the cabinet: {}",
            product.display()
        );
    }
    assert_eq!(
        products.documentation("c1"),
        PathBuf::from("/w/u/.curios/documentation/c1"),
        "and documentation sits beside the executables, under its package"
    );
    assert_ne!(
        cabinet.verdict("key"),
        cabinet.payload(Runs::Native { engine: "e" }, "key"),
        "one slot name, two families, two places"
    );
}

/// Every path a cabinet gives, a family's and a pin's staging alike, for a test holding that one directory holds them all.
fn kept(cabinet: &Cabinet) -> [PathBuf; 8] {
    let hash = TreeHash::parse(&format!("c1:{}", "c".repeat(64))).unwrap();
    let module = FileHash::parse(&format!("f1:{}", "c".repeat(64))).unwrap();

    [
        cabinet.source(&hash),
        cabinet.foreign(&module),
        cabinet.verdict("key"),
        cabinet.slot("key"),
        cabinet.payload(Runs::Native { engine: "e" }, "key"),
        cabinet.payload(Runs::Browser, "key"),
        cabinet.compiler(),
        cabinet.staging("1"),
    ]
}

/// A project's own cabinet is one directory of its `.curios/`, holding every family kept for reuse and a pin's staging, so the project shares, moves or clears it as one.
#[test]
fn a_projects_own_cabinet_holds_whole_what_is_kept() {
    let cabinet = Cabinet::of(Path::new("/w/u"), None);

    assert_eq!(cabinet.directory(), Path::new("/w/u/.curios/cabinet"));
    for path in kept(&cabinet) {
        assert!(path.starts_with(cabinet.directory()), "{}", path.display());
    }
}

/// A cabinet named for a run stands in for a project's own whole: everything kept is in the directory named, and nothing in the project's `.curios/`.
#[test]
fn a_named_cabinet_holds_whole_what_is_kept() {
    let cabinet = Cabinet::of(Path::new("/w/u"), Some(Path::new("/shared")));

    for path in kept(&cabinet) {
        assert!(path.starts_with("/shared"), "{}", path.display());
    }
}

/// A browser's payload is a module any browser runs, so it needs no engine in its address, and the two targets file under two directories, neither read for the other.
#[test]
fn each_target_files_its_payloads_apart() {
    let cabinet = Cabinet::of(Path::new("/w/u"), None);

    assert_ne!(
        cabinet.payload(Runs::Native { engine: "e" }, "key"),
        cabinet.payload(Runs::Browser, "key")
    );
    assert!(
        cabinet
            .payload(Runs::Browser, "key")
            .ends_with("payloads/browser/key")
    );
    assert!(
        cabinet
            .payload(Runs::Native { engine: "e" }, "key")
            .ends_with("payloads/native/key")
    );
}
