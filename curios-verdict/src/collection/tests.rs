use {
    super::*,
    crate::{STALE, Verdicts},
    curios_common::{fill, test_support::*},
    curios_module::open_entrypoint,
    curios_package::{FileHash, Governing, TreeHash, order},
    curios_pipeline::Fold,
    std::fs,
};

/// Write `contents` at `path`, the directories above it made.
fn put(path: &Path, contents: &str) {
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}

/// A tree of `(relative path, contents)` pairs, in a directory of its own that goes away with the test.
fn tree(name: &str, files: &[(&str, &str)]) -> Temporary {
    let root = Temporary::new("collection", name);
    for (path, contents) in files {
        put(&root.join(path), contents);
    }

    root
}

/// An umbrella's manifest enumerating `members`, as its TOML array spells them.
fn members(members: &str) -> String {
    format!(
        "compiler = \"{}\"\nmembers = [{members}]\n",
        env!("CARGO_PKG_VERSION")
    )
}

/// An umbrella over the members `app` and `base`, each a library of one declaration, beside an entry naming neither.
fn umbrella(name: &str) -> Temporary {
    tree(
        name,
        &[
            ("curios.toml", &members("\"app\", \"base\"")),
            ("app/curios.toml", "name = \"app\"\n"),
            ("app/lib.crs", "pub let word : /std/Str = \"app\";\n"),
            ("base/curios.toml", "name = \"base\"\n"),
            ("base/lib.crs", "pub let word : /std/Str = \"base\";\n"),
            ("entry.crs", "/std/print(\"\")\n"),
        ],
    )
}

/// File every unit of the graph of `member`, of the project at `root`, into the project's own cabinet, compiled for `compiled`: what a build over that graph files.
fn file(root: &Path, member: &str, compiled: Compiled) {
    let governing = Governing::of(&root.join(member), None).expect("a governed member");
    let units = order(&governing)
        .expect("a resolvable graph")
        .into_iter()
        .map(|unit| match compiled {
            Compiled::Built => unit,
            Compiled::Tested => unit.tested(),
        })
        .collect::<Vec<_>>();
    let (entrypoint, loader, _) =
        open_entrypoint(&root.join("entry.crs"), None).expect("an openable entry");
    let verdicts = Verdicts::at(governing.cabinet);

    Fold::new(1_000_000, &units, Some(&verdicts))
        .compile(&entrypoint, &loader, |_| {}, |_| {})
        .expect("a compiling program");
}

/// The objects the own cabinet of the project at `root` holds, by address.
fn objects(root: &Path) -> BTreeSet<String> {
    Cabinet::of(root, None).addresses()
}

/// A collection keeps every object a compilation of its project files: each member's graph, built and tested.
#[test]
fn a_collection_keeps_every_object_its_project_files() {
    let root = umbrella("kept");
    file(&root, "app", Compiled::Built);
    file(&root, "app", Compiled::Tested);
    file(&root, "base", Compiled::Built);
    let filed = objects(&root);

    collect_project(&root, SystemTime::now());

    assert_eq!(filed.len(), 3, "a unit a member and a kind");
    assert_eq!(objects(&root), filed);
}

/// A member the umbrella stops enumerating goes with every object only it reached, and its sibling's stay.
#[test]
fn a_member_no_longer_enumerated_goes_and_its_sibling_stays() {
    let root = umbrella("dropped");
    file(&root, "app", Compiled::Built);
    let sibling = objects(&root);
    file(&root, "base", Compiled::Built);
    put(&root.join("curios.toml"), &members("\"app\""));

    collect_project(&root, SystemTime::now());

    assert_eq!(objects(&root), sibling);
}

/// Nothing is taken where the reach cannot be read whole: with a member's manifest refused, the object of a member the umbrella stopped enumerating stays too.
#[test]
fn nothing_is_taken_where_a_manifest_is_refused() {
    let root = umbrella("refused");
    file(&root, "app", Compiled::Built);
    file(&root, "base", Compiled::Built);
    let filed = objects(&root);
    put(&root.join("curios.toml"), &members("\"app\""));
    put(&root.join("app/curios.toml"), "name = \n");

    collect_project(&root, SystemTime::now());

    assert_eq!(objects(&root), filed);
}

/// What no family of the cabinet or of `.curios/` holds goes, a staging once it is a day old: a layout moved away from is taken, and what a pin is still writing stays.
#[test]
fn what_no_family_holds_goes_and_a_staging_stays_a_day() {
    let root = umbrella("unknown");
    file(&root, "app", Compiled::Built);
    let cabinet = Cabinet::of(&root, None);
    let left = [
        root.join(".curios/stray"),
        cabinet.directory().join("verdicts/old/verdict"),
        cabinet.directory().join("collection"),
    ];
    for path in &left {
        put(path, "left behind");
    }
    let (young, old) = (
        cabinet.directory().join("pinning.1"),
        cabinet.directory().join("pinning.2"),
    );
    put(&young, "being written");
    put(&old, "stopped short");
    fs::File::options()
        .write(true)
        .open(&old)
        .and_then(|file| file.set_modified(SystemTime::now() - STALE * 2))
        .expect("a staging's time is set");

    collect_project(&root, SystemTime::now());

    for path in &left {
        assert!(!path.exists(), "{}", path.display());
    }
    assert!(!cabinet.directory().join("verdicts").exists());
    assert!(young.exists(), "a staging younger than a day stays");
    assert!(!old.exists(), "and an older one goes");
    assert_eq!(objects(&root).len(), 1, "beside the families");
}

/// A payload stays where an executable of the project, its tests or its library's tests are filed, on every target, and one filed under no such name goes, as does a target no compiler builds for.
#[test]
fn a_payload_stays_where_an_executable_or_its_tests_are_filed() {
    let compiler = env!("CARGO_PKG_VERSION");
    let root = tree(
        "payloads",
        &[
            (
                "curios.toml",
                &fill!(
                    "compiler = \"${compiler}\"\nname = \"app\"\n\n[executables]\napp = {}\n",
                    compiler
                ),
            ),
            ("lib.crs", ""),
            ("app.crs", "/std/print(\"\")\n"),
        ],
    );
    let cabinet = Cabinet::of(&root, None);
    let filed = |name: &str, target: Target| {
        let path = cabinet.payloads(target).join(payload_address("app", name));
        put(&path, name);

        path
    };
    let kept = ["app", "tests/", "tests/app"]
        .into_iter()
        .flat_map(|name| Target::ALL.map(|target| filed(name, target)))
        .collect::<Vec<_>>();
    let gone = filed("gone", Target::Native);
    let elsewhere = cabinet.families().payloads.join("wasi/payload");
    put(&elsewhere, "for a target no compiler builds for");

    collect_project(&root, SystemTime::now());

    for path in &kept {
        assert!(path.exists(), "{}", path.display());
    }
    assert!(!gone.exists());
    assert!(!elsewhere.exists());
}

/// A tree a manifest pins stays, and a tree and a foreign module no manifest pins any longer go, under its scheme's directory or under another.
#[test]
fn a_tree_stays_while_a_manifest_pins_it() {
    let root = Temporary::new("collection", "trees");
    let delivered = root.join("delivered");
    put(
        &delivered.join("curios.toml"),
        &format!(
            "compiler = \"{}\"\nname = \"http\"\n",
            env!("CARGO_PKG_VERSION")
        ),
    );
    put(&delivered.join("lib.crs"), "");
    let hash = TreeHash::of(&delivered).expect("a tree of regular files");
    let cabinet = Cabinet::of(&root, None);
    let pinned = cabinet.source(&hash);
    fs::create_dir_all(pinned.parent().unwrap()).unwrap();
    fs::rename(&delivered, &pinned).unwrap();
    let (compiler, hash) = (env!("CARGO_PKG_VERSION"), hash.to_string());
    put(
        &root.join("curios.toml"),
        &fill!(
            "compiler = \"${compiler}\"\nname = \"app\"\n\n[dependencies]\nhttp = { git = \"https://example/http\", rev = \"abc123\", hash = \"${hash}\" }\n",
            compiler,
            hash
        ),
    );
    put(&root.join("lib.crs"), "");
    let unpinned = cabinet.source(&TreeHash::parse(&format!("c1:{}", "e".repeat(64))).unwrap());
    put(&unpinned.join("lib.crs"), "");
    let elsewhere = cabinet.families().sources.join("c0/tree");
    put(&elsewhere.join("lib.crs"), "");
    let module = cabinet.foreign(&FileHash::of_bytes(b"a module"));
    put(&module, "a module");

    collect_project(&root, SystemTime::now());

    assert!(pinned.join("lib.crs").is_file());
    assert!(!unpinned.exists());
    assert!(!elsewhere.exists(), "nor a scheme no pinned tree is under");
    assert!(!module.exists());
}
