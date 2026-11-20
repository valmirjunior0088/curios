use {
    super::*,
    crate::Products,
    curios_common::{Qualifier, RootKind, digest},
};

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

/// A slot is a function of each of its parts, and of nothing else: no unit before its own is in it, so what those hold is verified where the slot is opened.
#[test]
fn every_part_of_a_slot_changes_it() {
    let mounts = [Mount::new(Qualifier::from(["json"]), RootKind::Ordinary)];
    let other = [Mount::new(Qualifier::from(["yaml"]), RootKind::Ordinary)];

    let slot = unit_slot("compiler", &mounts, None, Compiled::Built, None);

    assert_eq!(
        slot,
        unit_slot("compiler", &mounts, None, Compiled::Built, None),
        "and it is a function"
    );
    assert_ne!(
        slot,
        unit_slot("another", &mounts, None, Compiled::Built, None),
        "the compiler is in it"
    );
    assert_ne!(
        slot,
        unit_slot("compiler", &other, None, Compiled::Built, None),
        "the mounts are in it"
    );
    assert_ne!(
        slot,
        unit_slot("compiler", &mounts, None, Compiled::Tested, None),
        "what the unit is compiled for is in it"
    );
    assert_ne!(
        slot,
        unit_slot(
            "compiler",
            &mounts,
            None,
            Compiled::Built,
            Some(&[Qualifier::from(["std"])])
        ),
        "and the root a unit of its fold stands in for"
    );
}

/// What a unit declared decides which names resolve in it, so one source over one scope declaring different prefixes is two units rather than one. Three shapes, because "declared nothing" is not "declared none": the first sees every open prefix in scope and the second sees only its own.
#[test]
fn a_units_declared_dependencies_are_part_of_its_slot() {
    let mounts = [Mount::new(Qualifier::from(["app"]), RootKind::Ordinary)];
    let json = [Qualifier::from(["json"])];

    let undeclared = unit_slot("compiler", &mounts, None, Compiled::Built, None);
    let none = unit_slot("compiler", &mounts, Some(&[]), Compiled::Built, None);
    let one = unit_slot("compiler", &mounts, Some(&json), Compiled::Built, None);

    assert_ne!(undeclared, none);
    assert_ne!(none, one);
    assert_ne!(undeclared, one);
}

/// What a unit was compiled *from* is verified rather than addressed, so editing its source must leave the address alone — that is what keeps a project's slot count equal to its unit count instead of growing by one per compile, as an address hashed over the unit's tree would.
#[test]
fn a_slot_does_not_move_when_its_source_changes() {
    let mounts = [Mount::new(Qualifier::from(["json"]), RootKind::Ordinary)];

    assert_eq!(
        unit_slot("compiler", &mounts, None, Compiled::Built, None),
        unit_slot("compiler", &mounts, None, Compiled::Built, None)
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
        unit_slot("1", &mounts, None, Compiled::Built, None),
        unit_slot("", &mounts, Some(&[]), Compiled::Built, None),
        "a length ahead of each part, so no part's bytes are read as another's"
    );
    assert_ne!(
        payload_slot("c", "ab", "c", Runs::Native { engine: "e" }),
        payload_slot("c", "a", "bc", Runs::Native { engine: "e" }),
        "and the payload address frames its own parts for the same reason"
    );
}

/// A payload's address is a function of every part — the same argument [`unit_slot`]'s makes, with the two this one adds in place of the mounts.
#[test]
fn every_part_of_a_payload_slot_changes_it() {
    let slot = payload_slot(
        "compiler",
        "app",
        "serve",
        Runs::Native { engine: "engine" },
    );

    assert_eq!(
        slot,
        payload_slot(
            "compiler",
            "app",
            "serve",
            Runs::Native { engine: "engine" }
        ),
        "and it is a function"
    );
    assert_ne!(
        slot,
        payload_slot("another", "app", "serve", Runs::Native { engine: "engine" }),
        "the compiler is in it"
    );
}

/// The executable's identity is the pair, so neither half alone decides a slot: two executables of one package are two, and one name declared by two packages is two.
#[test]
fn an_executables_identity_is_its_package_and_its_name() {
    let slot = payload_slot(
        "compiler",
        "app",
        "serve",
        Runs::Native { engine: "engine" },
    );

    assert_ne!(
        slot,
        payload_slot(
            "compiler",
            "app",
            "worker",
            Runs::Native { engine: "engine" }
        ),
        "two executables of one package occupy two slots"
    );
    assert_ne!(
        slot,
        payload_slot(
            "compiler",
            "other",
            "serve",
            Runs::Native { engine: "engine" }
        ),
        "and two packages declaring one name do too"
    );
}

/// The part the unit family has no counterpart for. A unit is a judgment and travels wherever the compiler binary does; a payload is machine code compiled for one host, so two engines that cannot read each other's artifacts must not look in one place for them.
#[test]
fn two_engines_address_two_slots() {
    assert_ne!(
        payload_slot("compiler", "app", "serve", Runs::Native { engine: "one" }),
        payload_slot(
            "compiler",
            "app",
            "serve",
            Runs::Native { engine: "another" }
        )
    );
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

/// What a payload was compiled *from* is verified rather than addressed, exactly as a unit's source is — which is what bounds the family at one slot per executable rather than one per compile.
#[test]
fn a_payload_slot_does_not_move_when_its_source_changes() {
    assert_eq!(
        payload_slot(
            "compiler",
            "app",
            "serve",
            Runs::Native { engine: "engine" }
        ),
        payload_slot(
            "compiler",
            "app",
            "serve",
            Runs::Native { engine: "engine" }
        )
    );
}

/// The two families version independently, so an entry written by one layout is never found by the other — which is the whole reason each carries its own schema tag.
#[test]
fn the_two_families_are_tagged_apart() {
    assert_ne!(VERDICT_SCHEMA, PAYLOAD_SCHEMA);
}
