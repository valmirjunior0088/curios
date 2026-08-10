use super::*;

/// A binary nests under the package that declares it, segment by segment — so two members of one umbrella declaring `serve` cannot collide, and nothing has to refuse it.
#[test]
fn a_binary_nests_under_its_package() {
    let root = Path::new("/w/u");

    assert_eq!(
        bin(root, &Qualifier::from(["myorg", "json"]), "serve"),
        PathBuf::from("/w/u/.curios/bin/myorg/json/serve")
    );
    assert_ne!(
        bin(root, &Qualifier::from(["myorg", "json"]), "serve"),
        bin(root, &Qualifier::from(["other"]), "serve")
    );
}

/// The path inside `.curios/` does not depend on what encloses the package, so a binary does not move when its package joins an umbrella — only the root it hangs from changes.
#[test]
fn joining_an_umbrella_moves_only_the_root() {
    let alone = bin(Path::new("/w/json"), &Qualifier::from(["json"]), "serve");
    let enclosed = bin(Path::new("/w/u"), &Qualifier::from(["json"]), "serve");

    assert!(alone.ends_with(".curios/bin/json/serve"));
    assert!(enclosed.ends_with(".curios/bin/json/serve"));
}

/// A hash's scheme is a directory of its own, which is what lets a successor sit beside `c1` rather than replace it.
#[test]
fn a_materialized_tree_files_under_its_scheme() {
    let hash = TreeHash::parse(&format!("c1:{}", "a".repeat(64))).unwrap();

    assert_eq!(
        src(Path::new("/w/u"), &hash),
        PathBuf::from(format!("/w/u/.curios/src/c1/{}", "a".repeat(64)))
    );
}

/// The three families never share a namespace, so a package named `c1` cannot land on a scheme's directory.
#[test]
fn the_families_do_not_share_a_namespace() {
    let root = Path::new("/w/u");
    let hash = TreeHash::parse(&format!("c1:{}", "b".repeat(64))).unwrap();

    let binary = bin(root, &Qualifier::from(["c1"]), "tool");
    let tree = src(root, &hash);

    assert!(!binary.starts_with(tree.parent().unwrap()));
    assert!(!tree.starts_with(root.join(STORE).join("bin")));
    assert!(!unit(root, "key").starts_with(root.join(STORE).join("bin")));
}
