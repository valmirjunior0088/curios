use {
    super::*,
    crate::TreeHash,
    std::{
        fs,
        time::{SystemTime, UNIX_EPOCH},
    },
};

/// A tree of `(relative path, contents)` pairs, rooted at a fresh directory nothing else is using.
fn tree(name: &str, files: &[(&str, &str)]) -> PathBuf {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();
    let root = std::env::temp_dir().join(format!("curios-{name}-{}-{millis}", std::process::id()));

    for (path, source) in files {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, source).unwrap();
    }

    root
}

/// The prefixes `directory`'s order mounts, in fold order.
fn mounts(directory: &Path) -> Result<Vec<String>, String> {
    let governing = Governing::of(directory)?;

    Ok(order(&governing)?
        .iter()
        .flat_map(|source| source.mounts())
        .map(|mount| mount.prefix.join())
        .collect())
}

/// A package with no dependencies is one unit: its own library, last because everything is.
#[test]
fn a_lone_package_is_one_unit() {
    let root = tree(
        "graph-lone",
        &[("curios.toml", "name = \"json\"\n"), ("lib.crs", "")],
    );

    assert_eq!(mounts(&root).unwrap(), vec!["/json".to_string()]);

    fs::remove_dir_all(root).unwrap();
}

/// A path dependency is live and unpinned, and it lands before the package that names it.
#[test]
fn a_path_dependency_lands_before_its_dependent() {
    let root = tree(
        "graph-path",
        &[
            (
                "app/curios.toml",
                "name = \"app\"\n\n[dependencies]\nbase = { source = \"path\", path = \"../base\" }\n",
            ),
            ("app/lib.crs", ""),
            ("base/curios.toml", "name = \"base\"\n"),
            ("base/lib.crs", ""),
        ],
    );

    assert_eq!(
        mounts(&root.join("app")).unwrap(),
        vec!["/base".to_string(), "/app".to_string()]
    );

    fs::remove_dir_all(root).unwrap();
}

/// **The diamond.** Two packages depending on one package compile it once, and it lands before both.
#[test]
fn a_diamond_compiles_its_point_once() {
    let root = tree(
        "graph-diamond",
        &[
            (
                "app/curios.toml",
                "name = \"app\"\n\n[dependencies]\nleft = { source = \"path\", path = \"../left\" }\nright = { source = \"path\", path = \"../right\" }\n",
            ),
            ("app/lib.crs", ""),
            (
                "left/curios.toml",
                "name = \"left\"\n\n[dependencies]\nbase = { source = \"path\", path = \"../base\" }\n",
            ),
            ("left/lib.crs", ""),
            (
                "right/curios.toml",
                "name = \"right\"\n\n[dependencies]\nbase = { source = \"path\", path = \"../base\" }\n",
            ),
            ("right/lib.crs", ""),
            ("base/curios.toml", "name = \"base\"\n"),
            ("base/lib.crs", ""),
        ],
    );

    let mounts = mounts(&root.join("app")).unwrap();

    assert_eq!(mounts.iter().filter(|at| *at == "/base").count(), 1);
    assert_eq!(mounts.last().map(String::as_str), Some("/app"));
    assert!(
        mounts.iter().position(|at| at == "/base") < mounts.iter().position(|at| at == "/left"),
        "{mounts:?}"
    );

    fs::remove_dir_all(root).unwrap();
}

/// **The conflict**, in the form a live source can take it: two dependents resolving one name to two places, refused naming both.
#[test]
fn two_dependents_resolving_one_name_two_ways_is_refused() {
    let root = tree(
        "graph-conflict",
        &[
            (
                "app/curios.toml",
                "name = \"app\"\n\n[dependencies]\nleft = { source = \"path\", path = \"../left\" }\nright = { source = \"path\", path = \"../right\" }\n",
            ),
            ("app/lib.crs", ""),
            (
                "left/curios.toml",
                "name = \"left\"\n\n[dependencies]\nbase = { source = \"path\", path = \"../one\" }\n",
            ),
            ("left/lib.crs", ""),
            (
                "right/curios.toml",
                "name = \"right\"\n\n[dependencies]\nbase = { source = \"path\", path = \"../two\" }\n",
            ),
            ("right/lib.crs", ""),
            ("one/curios.toml", "name = \"base\"\n"),
            ("one/lib.crs", ""),
            ("two/curios.toml", "name = \"base\"\n"),
            ("two/lib.crs", ""),
        ],
    );

    let refusal = mounts(&root.join("app")).expect_err("one name, two places");
    assert!(refusal.contains("resolves two ways"), "{refusal}");
    assert!(
        refusal.contains("\"left\"") && refusal.contains("\"right\""),
        "{refusal}"
    );

    fs::remove_dir_all(root).unwrap();
}

/// A package of nothing but programs is nothing to depend *on*, and saying so beats every one of its names arriving unbound.
#[test]
fn a_dependency_with_no_library_is_refused() {
    let root = tree(
        "graph-libraryless-dependency",
        &[
            (
                "app/curios.toml",
                "name = \"app\"\n\n[dependencies]\ntool = { source = \"path\", path = \"../tool\" }\n",
            ),
            ("app/lib.crs", ""),
            (
                "tool/curios.toml",
                "name = \"tool\"\n\n[[executables]]\nname = \"tool\"\n",
            ),
            ("tool/tool.crs", ""),
        ],
    );

    let refusal = mounts(&root.join("app")).expect_err("nothing to import");
    assert!(refusal.contains("has no library"), "{refusal}");

    fs::remove_dir_all(root).unwrap();
}

/// **The conflict**, over exact pins: two dependents pinning one canonical name two ways is refused naming both dependents and both pins.
///
/// This needed M3 before it could be written at all — a `git` row could not be located, so no second one was ever reached. It needs only *one* materialized tree even now, and that is the property under test as much as the refusal is: the pin is read off the row before anything is located, so the disagreement is caught whether or not the second delivery exists. "Before any of the three elaborates" has to mean that.
#[test]
fn two_dependents_pinning_one_name_two_ways_is_refused() {
    let root = tree(
        "graph-pin-conflict",
        &[
            ("curios.toml", "members = [\"app\", \"left\", \"right\"]\n"),
            ("app/lib.crs", ""),
            ("left/lib.crs", ""),
            ("right/lib.crs", ""),
        ],
    );

    // One delivery, in the store, under the hash it actually hashes to.
    let delivered = root.join("delivered");
    fs::create_dir_all(&delivered).unwrap();
    fs::write(delivered.join("curios.toml"), "name = \"http\"\n").unwrap();
    fs::write(delivered.join("lib.crs"), "").unwrap();
    let hash = TreeHash::of(&delivered).unwrap();

    let placed = crate::Store::at(root.clone()).src(&hash);
    fs::create_dir_all(placed.parent().unwrap()).unwrap();
    fs::rename(&delivered, &placed).unwrap();

    let row = |rev: &str, hash: &TreeHash| {
        format!(
            "http = {{ source = \"git\", url = \"https://example/http\", rev = \"{rev}\", hash = \"{hash}\" }}"
        )
    };
    let member = |name: &str, dependency: String| {
        format!("name = \"{name}\"\n\n[dependencies]\n{dependency}\n")
    };

    fs::write(
        root.join("app/curios.toml"),
        "name = \"app\"\n\n[dependencies]\nleft = { source = \"member\" }\nright = { source = \"member\" }\n",
    )
    .unwrap();

    // Same tree, two revisions: what "same revision" rests on when a tag moves is the hash, and disagreeing about the instruction is still a disagreement.
    fs::write(
        root.join("left/curios.toml"),
        member("left", row("abc123", &hash)),
    )
    .unwrap();
    fs::write(
        root.join("right/curios.toml"),
        member("right", row("def456", &hash)),
    )
    .unwrap();

    let refusal = mounts(&root.join("app")).expect_err("one name, two pins");
    assert!(refusal.contains("pinned two ways"), "{refusal}");
    assert!(
        refusal.contains("abc123") && refusal.contains("def456"),
        "{refusal}"
    );
    assert!(
        refusal.contains("\"left\"") && refusal.contains("\"right\""),
        "both dependents are named: {refusal}"
    );

    // And the other half the specification names: one revision, two criteria. The second hash was never delivered, which is the point — the disagreement is caught before anything is looked for.
    let absent = TreeHash::parse(&format!("c1:{}", "e".repeat(64))).unwrap();
    fs::write(
        root.join("right/curios.toml"),
        member("right", row("abc123", &absent)),
    )
    .unwrap();

    let refusal = mounts(&root.join("app")).expect_err("one name, two hashes");
    assert!(refusal.contains("pinned two ways"), "{refusal}");

    fs::remove_dir_all(root).unwrap();
}

/// A pin reached through the catalog is a pin: a member drawing `http` from the umbrella's `[catalog]` and a member pinning it directly at another revision are refused naming both pins, before the direct one's tree exists — where reading the snapshot off the marker itself found none, sent the reader to `curate`, and after it reported two store paths.
#[test]
fn a_catalogued_pin_against_a_direct_pin_is_refused_as_two_pins() {
    let root = tree(
        "graph-catalog-conflict",
        &[
            ("app/lib.crs", ""),
            ("left/lib.crs", ""),
            ("right/lib.crs", ""),
            (
                "app/curios.toml",
                "name = \"app\"\n\n[dependencies]\nleft = { source = \"member\" }\nright = { source = \"member\" }\n",
            ),
            (
                "left/curios.toml",
                "name = \"left\"\n\n[dependencies]\nhttp = { source = \"catalog\" }\n",
            ),
        ],
    );

    // The catalogued delivery, in the store under the hash it hashes to; the direct pin's never arrives.
    let delivered = root.join("delivered");
    fs::create_dir_all(&delivered).unwrap();
    fs::write(delivered.join("curios.toml"), "name = \"http\"\n").unwrap();
    fs::write(delivered.join("lib.crs"), "").unwrap();
    let hash = TreeHash::of(&delivered).unwrap();
    let placed = crate::Store::at(root.clone()).src(&hash);
    fs::create_dir_all(placed.parent().unwrap()).unwrap();
    fs::rename(&delivered, &placed).unwrap();
    let absent = TreeHash::parse(&format!("c1:{}", "e".repeat(64))).unwrap();

    let row = |rev: &str, hash: &TreeHash| {
        format!(
            "http = {{ source = \"git\", url = \"https://example/http\", rev = \"{rev}\", hash = \"{hash}\" }}"
        )
    };
    fs::write(
        root.join("curios.toml"),
        format!(
            "members = [\"app\", \"left\", \"right\"]\n\n[catalog]\n{}\n",
            row("abc123", &hash)
        ),
    )
    .unwrap();
    fs::write(
        root.join("right/curios.toml"),
        format!(
            "name = \"right\"\n\n[dependencies]\n{}\n",
            row("def456", &absent)
        ),
    )
    .unwrap();

    let refusal =
        mounts(&root.join("app")).expect_err("one name, a catalogued pin and a direct one");
    assert!(refusal.contains("pinned two ways"), "{refusal}");
    assert!(
        refusal.contains("abc123") && refusal.contains("def456"),
        "{refusal}"
    );
    assert!(
        refusal.contains("\"left\"") && refusal.contains("\"right\""),
        "both dependents are named: {refusal}"
    );

    fs::remove_dir_all(root).unwrap();
}

/// **The cycle.** A dependency cycle is refused naming the chain.
#[test]
fn a_dependency_cycle_is_refused() {
    let root = tree(
        "graph-cycle",
        &[
            (
                "a/curios.toml",
                "name = \"a\"\n\n[dependencies]\nb = { source = \"path\", path = \"../b\" }\n",
            ),
            ("a/lib.crs", ""),
            (
                "b/curios.toml",
                "name = \"b\"\n\n[dependencies]\na = { source = \"path\", path = \"../a\" }\n",
            ),
            ("b/lib.crs", ""),
        ],
    );

    let refusal = mounts(&root.join("a")).expect_err("a cycle");
    assert!(refusal.contains("cycle"), "{refusal}");
    assert!(
        refusal.contains("\"a\"") && refusal.contains("\"b\""),
        "{refusal}"
    );

    fs::remove_dir_all(root).unwrap();
}

/// A cycle *through an already-placed name* is still a cycle.
///
/// `app` places `b` on its way in, so when `c` names `b` again the agreement branch would answer "already placed, and it agrees" and return — leaving `c` emitted before the `b` it depends on. A wrong fold order rather than a refusal is what a cycle looks like when the check runs second.
#[test]
fn a_cycle_reached_through_a_placed_name_is_refused() {
    let root = tree(
        "graph-cycle-placed",
        &[
            (
                "app/curios.toml",
                "name = \"app\"\n\n[dependencies]\nb = { source = \"path\", path = \"../b\" }\n",
            ),
            ("app/lib.crs", ""),
            (
                "b/curios.toml",
                "name = \"b\"\n\n[dependencies]\nc = { source = \"path\", path = \"../c\" }\n",
            ),
            ("b/lib.crs", ""),
            (
                "c/curios.toml",
                "name = \"c\"\n\n[dependencies]\nb = { source = \"path\", path = \"../b\" }\n",
            ),
            ("c/lib.crs", ""),
        ],
    );

    let refusal = mounts(&root.join("app")).expect_err("b and c cycle");
    assert!(refusal.contains("cycle"), "{refusal}");
    assert!(
        refusal.contains("\"b\"") && refusal.contains("\"c\""),
        "{refusal}"
    );

    fs::remove_dir_all(root).unwrap();
}

/// A package is referred to by the name it declares, so a row keyed by anything else is refused.
#[test]
fn a_row_keyed_by_the_wrong_name_is_refused() {
    let root = tree(
        "graph-misnamed",
        &[
            (
                "app/curios.toml",
                "name = \"app\"\n\n[dependencies]\nnope = { source = \"path\", path = \"../base\" }\n",
            ),
            ("app/lib.crs", ""),
            ("base/curios.toml", "name = \"base\"\n"),
            ("base/lib.crs", ""),
        ],
    );

    let refusal = mounts(&root.join("app")).expect_err("a consumer cannot rename a package");
    assert!(refusal.contains("declares itself \"base\""), "{refusal}");

    fs::remove_dir_all(root).unwrap();
}

/// **The umbrella**, all four mismatches: each marker names exactly the umbrella-side list that answers it, and each way of getting that wrong is its own refusal.
#[test]
fn the_four_marker_mismatches_are_four_refusals() {
    let shared = &[
        (
            "curios.toml",
            "members = [\"app\", \"base\"]\n\n[catalog]\nvendored = { source = \"path\", path = \"vendor/vendored\" }\n",
        ),
        ("base/curios.toml", "name = \"base\"\n"),
        ("base/lib.crs", ""),
        ("vendor/vendored/curios.toml", "name = \"vendored\"\n"),
        ("vendor/vendored/lib.crs", ""),
        ("app/lib.crs", ""),
    ];

    let cases = [
        // A `member` row naming something no governing umbrella enumerates.
        (
            "graph-marker-unenumerated",
            "name = \"app\"\n\n[dependencies]\nabsent = { source = \"member\" }\n",
            "no governing umbrella enumerates a member",
        ),
        // A `catalog` row naming something the catalog does not hold.
        (
            "graph-marker-uncatalogued",
            "name = \"app\"\n\n[dependencies]\nabsent = { source = \"catalog\" }\n",
            "`[catalog]` holds that name",
        ),
        // A `catalog` row whose name is a live member.
        (
            "graph-marker-member-in-catalog",
            "name = \"app\"\n\n[dependencies]\nbase = { source = \"catalog\" }\n",
            "is a live member",
        ),
        // A direct pin of a name the umbrella enumerates.
        (
            "graph-marker-direct-member",
            "name = \"app\"\n\n[dependencies]\nbase = { source = \"path\", path = \"../base\" }\n",
            "is a live member",
        ),
    ];

    for (name, manifest, expected) in cases {
        let mut files = shared.to_vec();
        files.push(("app/curios.toml", manifest));
        let root = tree(name, &files);

        let refusal = mounts(&root.join("app")).expect_err(name);
        assert!(refusal.contains(expected), "{name}: {refusal}");

        fs::remove_dir_all(root).unwrap();
    }
}

/// The markers that do match resolve: `member` through `members`, `catalog` through `[catalog]`.
#[test]
fn the_markers_resolve_through_the_lists_that_answer_them() {
    let root = tree(
        "graph-markers",
        &[
            (
                "curios.toml",
                "members = [\"app\", \"base\"]\n\n[catalog]\nvendored = { source = \"path\", path = \"vendor/vendored\" }\n",
            ),
            (
                "app/curios.toml",
                "name = \"app\"\n\n[dependencies]\nbase = { source = \"member\" }\nvendored = { source = \"catalog\" }\n",
            ),
            ("app/lib.crs", ""),
            ("base/curios.toml", "name = \"base\"\n"),
            ("base/lib.crs", ""),
            ("vendor/vendored/curios.toml", "name = \"vendored\"\n"),
            ("vendor/vendored/lib.crs", ""),
        ],
    );

    let mounts = mounts(&root.join("app")).unwrap();

    assert_eq!(mounts.last().map(String::as_str), Some("/app"));
    assert!(mounts.contains(&"/base".to_string()), "{mounts:?}");
    assert!(mounts.contains(&"/vendored".to_string()), "{mounts:?}");

    fs::remove_dir_all(root).unwrap();
}

/// A fetchable row states what it needs and where it goes, and says so rather than guessing while nothing has delivered it.
#[test]
fn an_unmaterialized_fetchable_row_names_curate() {
    let root = tree(
        "graph-unmaterialized",
        &[
            (
                "curios.toml",
                &format!(
                    "name = \"app\"\n\n[dependencies]\nhttp = {{ source = \"git\", url = \"https://example/http\", rev = \"abc123\", hash = \"c1:{}\" }}\n",
                    "a".repeat(64)
                ),
            ),
            ("lib.crs", ""),
        ],
    );

    let refusal = mounts(&root).expect_err("nothing has materialized it");
    assert!(refusal.contains("curios curate"), "{refusal}");

    fs::remove_dir_all(root).unwrap();
}
