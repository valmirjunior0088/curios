use {
    super::*,
    std::time::{SystemTime, UNIX_EPOCH},
};

/// A tree of `(relative path, contents)` pairs, rooted at a fresh directory nothing else is using.
fn tree(name: &str, files: &[(&str, &str)]) -> PathBuf {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();
    let root = std::env::temp_dir().join(format!("curios-{name}-{}-{millis}", std::process::id()));

    for (path, contents) in files {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, contents).unwrap();
    }

    root
}

/// A delivery that hashes to what it was pinned to is accepted and placed under that hash.
#[test]
fn a_delivery_matching_its_pin_is_placed() {
    let root = tree("curate-accept", &[("curios.toml", "name = \"app\"\n")]);
    let scratch = root.join("scratch");
    fs::create_dir_all(&scratch).unwrap();
    fs::write(scratch.join("lib.crs"), "pub let x : Type = Type;").unwrap();

    let hash = TreeHash::of(&scratch).unwrap();
    let pin = Pin {
        name: Qualifier::from(["http"]),
        url: "https://example/http".to_string(),
        rev: "abc123".to_string(),
        hash: hash.clone(),
    };

    accept(&scratch, &Store::at(root.clone()), &pin).expect("a delivery matching its pin");

    let placed = Store::at(root.clone()).src(&hash);
    assert!(placed.join("lib.crs").is_file(), "{}", placed.display());
    assert!(!scratch.exists(), "the scratch directory is consumed");

    fs::remove_dir_all(root).unwrap();
}

/// **The hash.** A delivery that is not what it was pinned to is refused, and the refusal states what actually arrived — because nobody writes a hash by hand, so the only way to fix a wrong one is to be told the right one.
#[test]
fn a_delivery_failing_its_pin_is_refused_stating_what_arrived() {
    let root = tree("curate-refuse", &[("curios.toml", "name = \"app\"\n")]);
    let scratch = root.join("scratch");
    fs::create_dir_all(&scratch).unwrap();
    fs::write(scratch.join("lib.crs"), "pub let x : Type = Type;").unwrap();

    let delivered = TreeHash::of(&scratch).unwrap();
    let pin = Pin {
        name: Qualifier::from(["http"]),
        url: "https://example/http".to_string(),
        rev: "abc123".to_string(),
        hash: TreeHash::parse(&format!("c1:{}", "a".repeat(64))).unwrap(),
    };

    let refusal =
        accept(&scratch, &Store::at(root.clone()), &pin).expect_err("a tampered delivery");

    assert!(refusal.contains("not what it is pinned to"), "{refusal}");
    assert!(refusal.contains(&delivered.to_string()), "{refusal}");
    assert!(
        !Store::at(root.clone()).src(&pin.hash).exists(),
        "nothing is placed under a hash it does not have"
    );

    fs::remove_dir_all(root).unwrap();
}

/// A project whose every dependency is live has nothing to fetch, so `materialize` reaches its fixed point without asking `git` anything.
#[test]
fn a_live_project_materializes_nothing() {
    let root = tree(
        "curate-live",
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

    let governing = Governing::of(&root.join("app")).unwrap();

    assert!(materialize(&governing).unwrap().is_empty());

    fs::remove_dir_all(root).unwrap();
}

/// A catalog entry no member references materializes nothing, and saying so is the whole of what it earns — activation lives in the package that names it.
#[test]
fn reconcile_reports_a_catalog_entry_nothing_references() {
    let root = tree(
        "curate-unreferenced",
        &[
            (
                "curios.toml",
                "members = [\"app\"]\n\n[catalog]\nunused = { source = \"path\", path = \"vendor/unused\" }\n",
            ),
            ("app/curios.toml", "name = \"app\"\n"),
            ("app/lib.crs", ""),
        ],
    );

    let governing = Governing::of(&root.join("app")).unwrap();
    let reports = reconcile(&governing).unwrap();

    assert!(
        reports.iter().any(|report| report.contains("\"unused\"")),
        "{reports:?}"
    );

    fs::remove_dir_all(root).unwrap();
}

/// A `.crs` file nothing enumerates is inert wherever it sits, which is the scratch-file freedom this design keeps by construction — so it is reported, never guessed at.
#[test]
fn reconcile_reports_an_unenumerated_file() {
    let root = tree(
        "curate-inert",
        &[
            (
                "curios.toml",
                "name = \"app\"\n\n[[executables]]\nname = \"serve\"\n",
            ),
            ("lib.crs", "pub mod parse;"),
            ("parse.crs", ""),
            ("serve.crs", ""),
            ("scratch.crs", ""),
        ],
    );

    let governing = Governing::of(&root).unwrap();
    let reports = reconcile(&governing).unwrap();

    assert_eq!(reports.len(), 1, "{reports:?}");
    assert!(reports[0].contains("scratch.crs"), "{reports:?}");
    assert!(reports[0].contains("nothing names it"), "{reports:?}");

    fs::remove_dir_all(root).unwrap();
}

/// A revision this machine is serving, fetched end to end.
///
/// Ignored because it shells out to `git`, which the rest of the suite does not depend on — but the remote is a `file://` URL on this machine, so it needs no network, and it exercises every step the real path takes: init, fetch, checkout, dropping the repository's metadata, hashing what is left, and placing it under that hash. Run it with
///
/// ```sh
/// cargo test --package curios-package -- --ignored a_fetched_revision
/// ```
#[test]
#[ignore = "shells out to git; the remote is local, so this needs no network"]
fn a_fetched_revision_is_verified_and_placed() {
    let files = &[
        ("curios.toml", "name = \"http\"\n"),
        ("lib.crs", "pub let get : Type = Type;"),
    ];

    // What the delivery must hash to: the same files, with no repository around them.
    let expected = TreeHash::of(&tree("curate-expected", files)).unwrap();

    let origin = tree("curate-origin", files);
    for arguments in [
        vec!["init", "--quiet"],
        vec!["config", "user.email", "probe@example"],
        vec!["config", "user.name", "probe"],
        vec!["add", "."],
        vec!["commit", "--quiet", "-m", "one"],
    ] {
        git(&origin, &arguments).expect("a repository on this machine");
    }

    let revision = String::from_utf8(
        Command::new("git")
            .current_dir(&origin)
            .args(["rev-parse", "HEAD"])
            .output()
            .unwrap()
            .stdout,
    )
    .unwrap()
    .trim()
    .to_string();

    let root = tree("curate-fetch", &[("curios.toml", "name = \"app\"\n")]);
    let pin = Pin {
        name: Qualifier::from(["http"]),
        url: format!("file://{}", origin.display()),
        rev: revision,
        hash: expected.clone(),
    };

    fetch(&Store::at(root.clone()), &pin).expect("a revision this machine is serving");

    let placed = Store::at(root.clone()).src(&expected);
    assert!(placed.join("lib.crs").is_file(), "{}", placed.display());
    assert!(
        !placed.join(".git").exists(),
        "source is what was delivered; the object store is how it arrived"
    );

    fs::remove_dir_all(origin).unwrap();
    fs::remove_dir_all(root).unwrap();
}
