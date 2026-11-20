use super::*;

/// A binary nests under the package that declares it — so two members of one umbrella declaring `serve` cannot collide, and nothing has to refuse it.
#[test]
fn a_binary_nests_under_its_package() {
    let products = Products::at(PathBuf::from("/w/u"));

    assert_eq!(
        products.built(Target::Native, "json", "serve"),
        PathBuf::from("/w/u/.curios/native/json/serve")
    );
    assert_ne!(
        products.built(Target::Native, "json", "serve"),
        products.built(Target::Native, "other", "serve")
    );
}

/// Each target's builds are a family of their own, spelled as `--target` spells the target, so a page and an executable of one name never stand at one path.
#[test]
fn each_target_builds_apart() {
    let products = Products::at(PathBuf::from("/w/u"));

    assert_eq!(
        products.built(Target::Browser, "json", "serve"),
        PathBuf::from("/w/u/.curios/browser/json/serve")
    );
    assert_ne!(
        products.built(Target::Native, "json", "serve"),
        products.built(Target::Browser, "json", "serve")
    );
}

/// The path inside `.curios/` does not depend on what encloses the package, so a binary does not move when its package joins an umbrella — only the root it hangs from changes.
#[test]
fn joining_an_umbrella_moves_only_the_root() {
    let alone = Products::at(PathBuf::from("/w/json")).built(Target::Native, "json", "serve");
    let enclosed = Products::at(PathBuf::from("/w/u")).built(Target::Native, "json", "serve");

    assert!(alone.ends_with(".curios/native/json/serve"));
    assert!(enclosed.ends_with(".curios/native/json/serve"));
}
