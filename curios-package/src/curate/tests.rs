use {super::*, curios_utilities::test_support::Temporary};

/// A tree of `(relative path, contents)` pairs, in a directory of its own that goes away with the test.
fn tree(name: &str, files: &[(&str, &str)]) -> Temporary {
    let root = Temporary::new("curate", name);

    for (path, contents) in files {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, contents).unwrap();
    }

    fs::create_dir_all(&root).unwrap();

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
    let acquisition = Acquisition {
        name: "http".to_string(),
        url: "https://example/http".to_string(),
        snapshot: Snapshot {
            rev: "abc123".to_string(),
            hash: hash.clone(),
        },
    };

    accept(&scratch, &Store::at(root.to_path_buf()), &acquisition)
        .expect("a delivery matching its pin");

    let placed = Store::at(root.to_path_buf()).source(&hash);
    assert!(placed.join("lib.crs").is_file(), "{}", placed.display());
    assert!(!scratch.exists(), "the scratch directory is consumed");
}

/// **The hash.** A delivery that is not what it was pinned to is refused, and the refusal states what actually arrived — because nobody writes a hash by hand, so the only way to fix a wrong one is to be told the right one.
#[test]
fn a_delivery_failing_its_pin_is_refused_stating_what_arrived() {
    let root = tree("curate-refuse", &[("curios.toml", "name = \"app\"\n")]);
    let scratch = root.join("scratch");
    fs::create_dir_all(&scratch).unwrap();
    fs::write(scratch.join("lib.crs"), "pub let x : Type = Type;").unwrap();

    let delivered = TreeHash::of(&scratch).unwrap();
    let acquisition = Acquisition {
        name: "http".to_string(),
        url: "https://example/http".to_string(),
        snapshot: Snapshot {
            rev: "abc123".to_string(),
            hash: TreeHash::parse(&format!("c1:{}", "a".repeat(64))).unwrap(),
        },
    };

    let refusal = accept(&scratch, &Store::at(root.to_path_buf()), &acquisition)
        .expect_err("a tampered delivery");

    assert!(refusal.contains("not what it is pinned to"), "{refusal}");
    assert!(refusal.contains(&delivered.to_string()), "{refusal}");
    assert!(
        !Store::at(root.to_path_buf())
            .source(&acquisition.snapshot.hash)
            .exists(),
        "nothing is placed under a hash it does not have"
    );
}

/// A project whose every dependency is live has nothing to fetch, so `curate` reaches its fixed point without asking `git` anything.
#[test]
fn a_live_project_acquires_nothing() {
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

    assert!(curate(&governing).unwrap().is_empty());
}

/// A pin of a name the umbrella enumerates is refused by `order` as a direct pin of a live member; the walk declines to fetch on its behalf first, so a refused row costs no network round trip.
#[test]
fn a_pin_of_a_live_member_is_not_acquired() {
    let root = tree(
        "curate-pinned-member",
        &[
            ("curios.toml", "members = [\"app\", \"base\"]\n"),
            (
                "app/curios.toml",
                "name = \"app\"\n\n[dependencies]\nbase = { source = \"git\", url = \"https://example/base\", rev = \"abc123\", hash = \"c1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\" }\n",
            ),
            ("app/lib.crs", ""),
            ("base/curios.toml", "name = \"base\"\n"),
            ("base/lib.crs", ""),
        ],
    );

    let governing = Governing::of(&root.join("app")).unwrap();

    assert!(curate(&governing).unwrap().is_empty());
}

/// **A fetchable catalog row is acquired, and this is the regression.** The marker used to be resolved *after* the dispatch that decides what to fetch, so it landed in the store at a hash nothing had put there: `curate` acquired nothing, `order` then refused the dependency naming `curate`, and running it changed nothing. A dead end whose error message named the command that could not escape it.
///
/// Asserted against the walk rather than against a fetch, so it needs no `git` and no network: what was broken is which acquisitions the walk collects.
#[test]
fn a_fetchable_catalog_row_is_acquired() {
    let root = tree(
        "curate-catalog-fetchable",
        &[
            (
                "curios.toml",
                "members = [\"app\"]\n\n[catalog]\nhttp = { source = \"git\", url = \"https://example/http\", rev = \"abc123\", hash = \"c1:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\" }\n",
            ),
            (
                "app/curios.toml",
                "name = \"app\"\n\n[dependencies]\nhttp = { source = \"catalog\" }\n",
            ),
            ("app/lib.crs", ""),
        ],
    );

    let governing = Governing::of(&root.join("app")).unwrap();
    let (wanted, _) = acquisitions(&governing).unwrap();

    assert_eq!(wanted.len(), 1, "{:?}", wanted.len());
    let acquired = wanted.iter().next().unwrap();
    assert_eq!(acquired.name, "http");
    assert_eq!(acquired.url, "https://example/http");
    assert_eq!(acquired.snapshot.rev, "abc123");
}

/// The other half of the same dispatch: a `path` catalog row resolves against the *umbrella's* root rather than the depending package's directory, because a relative path is relative to whoever wrote it.
#[test]
fn a_path_catalog_row_resolves_against_the_umbrella() {
    let root = tree(
        "curate-catalog-path",
        &[
            (
                "curios.toml",
                "members = [\"app\"]\n\n[catalog]\nbase = { source = \"path\", path = \"vendor/base\" }\n",
            ),
            (
                "app/curios.toml",
                "name = \"app\"\n\n[dependencies]\nbase = { source = \"catalog\" }\n",
            ),
            ("app/lib.crs", ""),
            // Reached only if the base is the umbrella's root: from `app/` this path names nothing.
            (
                "vendor/base/curios.toml",
                "name = \"base\"\n\n[dependencies]\nhttp = { source = \"git\", url = \"https://example/http\", rev = \"def456\", hash = \"c1:bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\" }\n",
            ),
            ("vendor/base/lib.crs", ""),
        ],
    );

    let governing = Governing::of(&root.join("app")).unwrap();
    let (wanted, _) = acquisitions(&governing).unwrap();

    // The catalog row itself fetches nothing, but the walk descended into it and found what it depends on.
    assert_eq!(wanted.len(), 1, "{wanted:?}");
    assert_eq!(wanted.iter().next().unwrap().name, "http");
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
    let measured = tree("curate-expected", files);
    let expected = TreeHash::of(&measured).unwrap();

    let (origin, revision) = origin("curate-origin", files);

    let root = tree("curate-fetch", &[("curios.toml", "name = \"app\"\n")]);
    let acquisition = Acquisition {
        name: "http".to_string(),
        url: format!("file://{}", origin.display()),
        snapshot: Snapshot {
            rev: revision,
            hash: expected.clone(),
        },
    };

    fetch(&Store::at(root.to_path_buf()), &acquisition)
        .expect("a revision this machine is serving");

    let placed = Store::at(root.to_path_buf()).source(&expected);
    assert!(placed.join("lib.crs").is_file(), "{}", placed.display());
    assert!(
        !placed.join(".git").exists(),
        "source is what was delivered; the object store is how it arrived"
    );
}

/// A repository on this machine holding `files`, on a branch and at a tag, with the object name of its one commit.
///
/// The remote every fetching test serves from: local, so none of them needs a network.
fn origin(name: &str, files: &[(&str, &str)]) -> (Temporary, String) {
    let origin = tree(name, files);

    for arguments in [
        vec!["init", "--quiet"],
        vec!["config", "user.email", "probe@example"],
        vec!["config", "user.name", "probe"],
        vec!["add", "."],
        vec!["commit", "--quiet", "-m", "one"],
        vec!["tag", "v1"],
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

    (origin, revision)
}

/// One acquisition of `origin` at `rev`, pinned to `hash`.
fn pinned(origin: &Path, rev: &str, hash: &TreeHash) -> Acquisition {
    Acquisition {
        name: "http".to_string(),
        url: format!("file://{}", origin.display()),
        snapshot: Snapshot {
            rev: rev.to_string(),
            hash: hash.clone(),
        },
    }
}

/// A revision the remote does not hold is refused naming the revision, never the hash.
///
/// **The regression for the deep fetch reaching for `FETCH_HEAD`.** A refspec-less fetch points `FETCH_HEAD` at the remote's default branch, so falling back to it delivered *that* for any pin the shallow fetch could not serve — a wrong `rev` included. The hash then refused what arrived, which reads as "your hash is wrong" and sends a reader to correct the one column that was right; correcting it would have pinned whatever the default branch held that day.
#[test]
#[ignore = "shells out to git; the remote is local, so this needs no network"]
fn a_revision_the_remote_does_not_hold_is_refused_naming_the_revision() {
    let files = &[
        ("curios.toml", "name = \"http\"\n"),
        ("lib.crs", "pub let get : Type = Type;"),
    ];
    let measured = tree("curate-absent-expected", files);
    let expected = TreeHash::of(&measured).unwrap();
    let (origin, _) = origin("curate-absent-origin", files);

    let root = tree("curate-absent", &[("curios.toml", "name = \"app\"\n")]);
    let absent = "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef";

    let refusal = fetch(
        &Store::at(root.to_path_buf()),
        &pinned(&origin, absent, &expected),
    )
    .expect_err("a revision this remote does not hold");

    assert!(refusal.contains(absent), "{refusal}");
    assert!(
        !refusal.contains("not what it is pinned to"),
        "the fault is the revision, not the hash: {refusal}"
    );
}

/// A pin may name a branch or a tag, not only an object, and both still deliver.
///
/// The other side of the refusal above, and the reason it is not simply a deleted fallback: a shallow fetch of a *ref* leaves no local branch of that name, so `FETCH_HEAD` is the only spelling of what it brought. Checking out by name is what the deep fetch does, and only it.
#[test]
#[ignore = "shells out to git; the remote is local, so this needs no network"]
fn a_pin_naming_a_branch_or_a_tag_is_delivered() {
    let files = &[
        ("curios.toml", "name = \"http\"\n"),
        ("lib.crs", "pub let get : Type = Type;"),
    ];
    let measured = tree("curate-refs-expected", files);
    let expected = TreeHash::of(&measured).unwrap();
    let (origin, revision) = origin("curate-refs-origin", files);

    for rev in [revision.as_str(), "v1", "master", "main"] {
        let root = tree("curate-refs", &[("curios.toml", "name = \"app\"\n")]);
        let store = Store::at(root.to_path_buf());

        // Whichever name `git init` gave the branch is the one that exists; the other is simply absent from this remote.
        if rev == "master" || rev == "main" {
            let held = git(&origin, &["rev-parse", "--verify", rev]).is_ok();
            if !held {
                continue;
            }
        }

        fetch(&store, &pinned(&origin, rev, &expected)).unwrap_or_else(|refusal| {
            panic!("{rev} names a revision this remote holds: {refusal}")
        });
        assert!(store.source(&expected).join("lib.crs").is_file(), "{rev}");
    }
}

/// A tree pinned through two mirrors is one acquisition: `app` pins `shape` through its origin and `app`'s path dependency `mid` pins the same snapshot through a bare clone of it, and `curate` fetches it once and reports it once. The set of acquisitions tells the two apart by `url`, so this is the loop's own dedup under test, not the set's.
#[test]
#[ignore = "shells out to git; the remote is local, so this needs no network"]
fn a_tree_pinned_through_two_mirrors_is_fetched_once() {
    let files = &[
        ("curios.toml", "name = \"shape\"\n"),
        ("lib.crs", "pub let message : /std/Str = \"shape\";"),
    ];
    let measured = tree("curate-mirrors-expected", files);
    let expected = TreeHash::of(&measured).unwrap();
    let (origin, revision) = origin("curate-mirrors-origin", files);
    // A directory nothing has created yet, which the bare clone creates.
    let mirror = Temporary::new("curate", "curate-mirrors-mirror");
    git(
        &origin,
        &[
            "clone",
            "--quiet",
            "--bare",
            ".",
            &mirror.display().to_string(),
        ],
    )
    .expect("a bare clone on this machine");

    let pin = |remote: &Path| {
        format!(
            "shape = {{ source = \"git\", url = \"file://{}\", rev = \"{revision}\", hash = \"{expected}\" }}\n",
            remote.display()
        )
    };
    let root = tree(
        "curate-mirrors",
        &[
            (
                "app/curios.toml",
                &format!(
                    "name = \"app\"\n\n[dependencies]\n{}mid = {{ source = \"path\", path = \"../mid\" }}\n",
                    pin(&origin)
                ),
            ),
            ("app/lib.crs", ""),
            (
                "mid/curios.toml",
                &format!("name = \"mid\"\n\n[dependencies]\n{}", pin(&mirror)),
            ),
            ("mid/lib.crs", ""),
        ],
    );

    let governing = Governing::of(&root.join("app")).unwrap();
    let fetched = curate(&governing).expect("a snapshot both mirrors serve");

    assert_eq!(fetched.packages.len(), 1, "{fetched:?}");
    assert!(
        governing
            .store()
            .source(&expected)
            .join("lib.crs")
            .is_file()
    );
}

/// A fetched module matching its pin is placed under it, keyed by the digest alone so two packages naming one module share the entry.
///
/// Delivered over `file://`, which is a transport like any other — the point of accepting by hash is that it does not matter which one carried the bytes, and it keeps this test offline.
///
/// It does mean this exercises `curl` specifically: `wget` speaks no `file://`, so on a machine with only the fallback installed this fails where the product would not. That is a property of the scheme a test can reach without a network, not of the fetch.
#[test]
fn a_fetched_module_matching_its_pin_is_placed() {
    let root = tree("curate-module", &[("curios.toml", "name = \"app\"\n")]);
    let module = root.join("libsha2.wasm");
    fs::write(&module, b"\0asm\x01\0\0\0").unwrap();

    let hash = FileHash::of_bytes(&fs::read(&module).unwrap());
    let acquisition = ModuleAcquisition {
        package: "crypto".to_string(),
        name: "sha2".to_string(),
        url: format!("file://{}", module.display()),
        hash: hash.clone(),
    };

    let store = Store::at(root.to_path_buf());
    fetch_module(&store, &acquisition).expect("a delivery matching its pin");

    assert_eq!(fs::read(store.foreign(&hash)).unwrap(), b"\0asm\x01\0\0\0");
}

/// A fetched module that is not what it was pinned to is refused stating both digests, and nothing is filed — the store never holds bytes under a key they do not have.
#[test]
fn a_fetched_module_failing_its_pin_is_refused_and_files_nothing() {
    let root = tree(
        "curate-module-refuse",
        &[("curios.toml", "name = \"app\"\n")],
    );
    let module = root.join("libsha2.wasm");
    fs::write(&module, b"\0asm\x01\0\0\0").unwrap();

    let pinned = FileHash::parse(&format!("f1:{}", "a".repeat(64))).unwrap();
    let acquisition = ModuleAcquisition {
        package: "crypto".to_string(),
        name: "sha2".to_string(),
        url: format!("file://{}", module.display()),
        hash: pinned.clone(),
    };

    let store = Store::at(root.to_path_buf());
    let refusal = fetch_module(&store, &acquisition).expect_err("a delivery failing its pin");

    assert!(refusal.contains("is not what it is pinned to"), "{refusal}");
    assert!(
        refusal.contains(&FileHash::of_bytes(b"\0asm\x01\0\0\0").to_string()),
        "the refusal states what arrived: {refusal}"
    );
    assert!(!store.foreign(&pinned).exists(), "nothing was filed");
}
