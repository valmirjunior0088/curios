use {super::*, curios_base::Qualifier};

/// A binary nests under the package that declares it — so two members of one umbrella declaring `serve` cannot collide, and nothing has to refuse it.
#[test]
fn a_binary_nests_under_its_package() {
    let store = Store::at(PathBuf::from("/w/u"));

    assert_eq!(
        store.bin("json", "serve"),
        PathBuf::from("/w/u/.curios/bin/json/serve")
    );
    assert_ne!(store.bin("json", "serve"), store.bin("other", "serve"));
}

/// The path inside `.curios/` does not depend on what encloses the package, so a binary does not move when its package joins an umbrella — only the root it hangs from changes.
#[test]
fn joining_an_umbrella_moves_only_the_root() {
    let alone = Store::at(PathBuf::from("/w/json")).bin("json", "serve");
    let enclosed = Store::at(PathBuf::from("/w/u")).bin("json", "serve");

    assert!(alone.ends_with(".curios/bin/json/serve"));
    assert!(enclosed.ends_with(".curios/bin/json/serve"));
}

/// A hash's scheme is a directory of its own, which is what lets a successor sit beside `c1` rather than replace it.
#[test]
fn a_materialized_tree_files_under_its_scheme() {
    let hash = TreeHash::parse(&format!("c1:{}", "a".repeat(64))).unwrap();

    assert_eq!(
        Store::at(PathBuf::from("/w/u")).src(&hash),
        PathBuf::from(format!("/w/u/.curios/src/c1/{}", "a".repeat(64)))
    );
}

/// The three families never share a namespace, so a package named `c1` cannot land on a scheme's directory.
#[test]
fn the_families_do_not_share_a_namespace() {
    let root = PathBuf::from("/w/u");
    let store = Store::at(root.clone());
    let hash = TreeHash::parse(&format!("c1:{}", "b".repeat(64))).unwrap();

    let binary = store.bin("c1", "tool");
    let tree = store.src(&hash);

    assert!(!binary.starts_with(tree.parent().unwrap()));
    assert!(!tree.starts_with(root.join(STORE).join("bin")));
    assert!(!store.unit("key").starts_with(root.join(STORE).join("bin")));
}

/// A slot is a function of all three parts, and of the *order* of the third — which is what the universe-seed table forces: the same source lowered after a different prefix is a different unit.
#[test]
fn every_part_of_a_slot_changes_it() {
    let mounts = [Mount::new(Qualifier::from(["json"]), RootKind::Ordinary)];
    let other = [Mount::new(Qualifier::from(["yaml"]), RootKind::Ordinary)];
    let before = ["one".to_string(), "two".to_string()];

    let slot = unit_slot("compiler", &before, &mounts);

    assert_eq!(
        slot,
        unit_slot("compiler", &before, &mounts),
        "and it is a function"
    );
    assert_ne!(
        slot,
        unit_slot("another", &before, &mounts),
        "the compiler is in it"
    );
    assert_ne!(
        slot,
        unit_slot("compiler", &before, &other),
        "the mounts are in it"
    );
    assert_ne!(
        slot,
        unit_slot("compiler", &["one".to_string()], &mounts),
        "the predecessors are in it"
    );
    assert_ne!(
        slot,
        unit_slot("compiler", &["two".to_string(), "one".to_string()], &mounts),
        "and their order is, because two orders of one set are two lowerings"
    );
}

/// The privilege a mount carries decides what its modules may reference, so two roots of one name and different tiers are two units rather than one.
#[test]
fn a_mounts_tier_is_part_of_its_slot() {
    let ordinary = [Mount::new(Qualifier::from(["std"]), RootKind::Ordinary)];
    let privileged = [Mount::new(Qualifier::from(["std"]), RootKind::Privileged)];

    assert_ne!(
        unit_slot("compiler", &[], &ordinary),
        unit_slot("compiler", &[], &privileged)
    );
}

/// What a unit was compiled *from* is verified rather than addressed, so editing its source must leave the address alone — that is what keeps a project's slot count equal to its unit count instead of growing by one per compile, which is exactly how the tree-hashed scheme this replaced went wrong.
#[test]
fn a_slot_does_not_move_when_its_source_changes() {
    let mounts = [Mount::new(Qualifier::from(["json"]), RootKind::Ordinary)];

    assert_eq!(
        unit_slot("compiler", &[], &mounts),
        unit_slot("compiler", &[], &mounts)
    );
    assert_ne!(
        digest(b"one"),
        digest(b"two"),
        "and the digest that does the verifying still separates them"
    );
}

/// Framing, for the reason the tree hash frames its own: without it, moving a boundary between two parts feeds the digest identical bytes.
#[test]
fn a_moved_boundary_is_a_different_slot() {
    let mounts = [Mount::new(Qualifier::from(["c"]), RootKind::Ordinary)];

    assert_ne!(
        unit_slot("ab", &["c".to_string()], &mounts),
        unit_slot("a", &["bc".to_string()], &mounts)
    );
}
