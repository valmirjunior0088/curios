use {
    super::*,
    std::{
        fs,
        time::{SystemTime, UNIX_EPOCH},
    },
};

/// A tree of `(relative path, contents)` pairs, rooted at a fresh directory nothing else is using.
///
/// **Canonical, because everything it is compared against is.** `Governing::of` canonicalizes the directory it walks from and `Walk::locate` canonicalizes every resolution it returns — deliberately, since a location is compared and two spellings of one directory would compile a diamond's point twice. So an expectation built from an *uncanonical* root tests the platform's symlinks rather than anything this crate decided: on macOS `std::env::temp_dir()` is `/var/…`, which is really `/private/var/…`, and every path assertion here failed on that difference alone.
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

    root.canonicalize()
        .expect("the tree was just written, so it resolves")
}

/// The entry file `argument` resolves to inside `directory`, or the refusal it earns.
fn entry(argument: Option<&str>, directory: &Path) -> Result<PathBuf, String> {
    match Target::of(argument, None, directory)? {
        Target::Stdin => panic!("standard input has no entry file"),
        Target::File(path) => Ok(path),
        Target::Executable { entry, .. } => Ok(entry),
    }
}

/// A package declaring exactly one executable is what a bare `run` means.
#[test]
fn a_bare_run_means_the_sole_executable() {
    let root = tree(
        "run-sole",
        &[
            (
                "curios.toml",
                "name = \"app\"\n\n[[executables]]\nname = \"serve\"\n",
            ),
            ("lib.crs", ""),
            ("serve.crs", ""),
        ],
    );

    assert_eq!(entry(None, &root).unwrap(), root.join("serve.crs"));

    fs::remove_dir_all(root).unwrap();
}

/// With more than one, `default` decides — and without a `default`, a bare run refuses listing the candidates rather than picking.
#[test]
fn a_bare_run_needs_a_default_when_there_is_a_choice() {
    let files = &[("lib.crs", ""), ("serve.crs", ""), ("bench.crs", "")];

    let ambiguous = tree("run-ambiguous", &{
        let mut files = files.to_vec();
        files.push((
            "curios.toml",
            "name = \"app\"\n\n[[executables]]\nname = \"serve\"\n\n[[executables]]\nname = \"bench\"\n",
        ));
        files
    });

    let refusal = entry(None, &ambiguous).expect_err("two executables and no default");
    assert!(refusal.contains("more than one executable"), "{refusal}");
    assert!(
        refusal.contains("\"serve\"") && refusal.contains("\"bench\""),
        "{refusal}"
    );
    fs::remove_dir_all(ambiguous).unwrap();

    let decided = tree("run-default", &{
        let mut files = files.to_vec();
        files.push((
            "curios.toml",
            "name = \"app\"\ndefault = \"bench\"\n\n[[executables]]\nname = \"serve\"\n\n[[executables]]\nname = \"bench\"\n",
        ));
        files
    });

    assert_eq!(entry(None, &decided).unwrap(), decided.join("bench.crs"));
    fs::remove_dir_all(decided).unwrap();
}

/// `run <name>` names a declared executable, and an undeclared one is refused listing what there is.
#[test]
fn a_name_selects_a_declared_executable() {
    let root = tree(
        "run-named",
        &[
            (
                "curios.toml",
                "name = \"app\"\n\n[[executables]]\nname = \"serve\"\n\n[[executables]]\nname = \"bench\"\npath = \"tools/bench.crs\"\n",
            ),
            ("lib.crs", ""),
            ("serve.crs", ""),
            ("tools/bench.crs", ""),
        ],
    );

    assert_eq!(
        entry(Some("bench"), &root).unwrap(),
        root.join("tools/bench.crs")
    );

    let refusal = entry(Some("absent"), &root).expect_err("no such executable");
    assert!(
        refusal.contains("no executable named \"absent\""),
        "{refusal}"
    );
    assert!(refusal.contains("\"serve\""), "{refusal}");

    fs::remove_dir_all(root).unwrap();
}

/// **The dispatch.** A file argument is never captured by a manifest — not even standing inside a package that declares an executable of a colliding name.
#[test]
fn a_file_argument_is_never_captured_by_a_manifest() {
    let root = tree(
        "run-file",
        &[
            (
                "curios.toml",
                "name = \"app\"\n\n[[executables]]\nname = \"serve\"\n",
            ),
            ("lib.crs", ""),
            ("serve.crs", ""),
            ("scratch.crs", ""),
        ],
    );

    for argument in ["scratch.crs", "serve.crs", "./serve", "sub/dir/x.crs"] {
        let target = Target::of(Some(argument), None, &root).expect("a file argument");
        assert!(
            matches!(&target, Target::File(path) if path == Path::new(argument)),
            "{argument} should dispatch as a file"
        );
    }

    fs::remove_dir_all(root).unwrap();
}

/// A file argument compiles standalone *everywhere*, so it works where no manifest governs at all.
#[test]
fn a_file_argument_needs_no_project() {
    let root = tree("run-standalone", &[("scratch.crs", "")]);

    assert_eq!(
        entry(Some("scratch.crs"), &root).unwrap(),
        PathBuf::from("scratch.crs")
    );

    fs::remove_dir_all(root).unwrap();
}

/// A declared executable's binary lands in the governing root's store, nested under the package that declares it.
#[test]
fn a_declared_executable_builds_into_the_store() {
    let root = tree(
        "run-output",
        &[
            ("curios.toml", "members = [\"json\"]\n"),
            (
                "json/curios.toml",
                "name = \"json\"\n\n[[executables]]\nname = \"serve\"\n",
            ),
            ("json/lib.crs", ""),
            ("json/serve.crs", ""),
        ],
    );

    let target = Target::of(None, None, &root.join("json")).expect("an enumerated member");

    // The umbrella governs, so the store is its own — but the path *within* the store names the package, so it would not move if the member left.
    let output = target
        .output()
        .expect("a declared executable has a default output");
    assert!(
        output.ends_with(".curios/bin/json/serve"),
        "{}",
        output.display()
    );

    fs::remove_dir_all(root).unwrap();
}

/// A bare file has no project, hence no store: its build lands beside the working directory under its own stem.
#[test]
fn a_bare_file_builds_beside_itself() {
    let target = Target::of(Some("hello.crs"), None, Path::new(".")).expect("a file argument");

    assert_eq!(target.output(), Some(PathBuf::from("hello")));
}

/// `-` is standard input, and it answers before anything looks for a manifest — so it means the same thing in a package as outside one, which is the whole point of dispatching lexically.
#[test]
fn a_dash_is_standard_input_everywhere() {
    let root = tree(
        "run-stdin",
        &[
            (
                "curios.toml",
                "name = \"app\"\n\n[[executables]]\nname = \"serve\"\n",
            ),
            ("lib.crs", ""),
            ("serve.crs", ""),
        ],
    );

    for directory in [root.as_path(), Path::new(".")] {
        assert!(
            matches!(
                Target::of(Some("-"), None, directory).expect("standard input needs no project"),
                Target::Stdin
            ),
            "`-` should dispatch as standard input in {}",
            directory.display()
        );
    }

    fs::remove_dir_all(root).unwrap();
}

/// An anonymous program has neither a file to read nor a stem to name an executable after, so both answers are absent rather than invented — `compile` turns the second into a refusal that names `-o`.
#[test]
fn standard_input_has_neither_an_entry_nor_an_output() {
    let target = Target::of(Some("-"), None, Path::new(".")).expect("standard input");

    assert_eq!(target.entry(), None);
    assert_eq!(target.output(), None);
}

/// A package of nothing but programs compiles them against its dependencies alone — there is no library of its own to put last.
#[test]
fn a_package_of_programs_alone_runs_them() {
    let root = tree(
        "run-programs-only",
        &[
            (
                "app/curios.toml",
                "name = \"app\"\n\n[dependencies]\nbase = { source = \"path\", path = \"../base\" }\n\n[[executables]]\nname = \"serve\"\n",
            ),
            ("app/serve.crs", ""),
            ("base/curios.toml", "name = \"base\"\n"),
            ("base/lib.crs", ""),
        ],
    );

    let Target::Executable { entry, units, .. } =
        Target::of(None, None, &root.join("app")).expect("a package with no library")
    else {
        panic!("a declared name is not a file");
    };

    assert_eq!(entry, root.join("app/serve.crs"));
    assert_eq!(
        units
            .iter()
            .flat_map(|source| source.mounts())
            .map(|mount| mount.prefix.join())
            .collect::<Vec<_>>(),
        vec!["/base".to_string()]
    );

    fs::remove_dir_all(root).unwrap();
}

/// An executable compiles against its package's full scope: its own library last, everything it depends on before that.
#[test]
fn an_executable_compiles_against_its_package_and_its_dependencies() {
    let root = tree(
        "run-scope",
        &[
            (
                "app/curios.toml",
                "name = \"app\"\n\n[dependencies]\nbase = { source = \"path\", path = \"../base\" }\n\n[[executables]]\nname = \"serve\"\n",
            ),
            ("app/lib.crs", ""),
            ("app/serve.crs", ""),
            ("base/curios.toml", "name = \"base\"\n"),
            ("base/lib.crs", ""),
        ],
    );

    let Target::Executable { name, units, .. } =
        Target::of(None, None, &root.join("app")).expect("a sole executable")
    else {
        panic!("a declared name is not a file");
    };

    assert_eq!(name, "serve");
    assert_eq!(
        units
            .iter()
            .flat_map(|source| source.mounts())
            .map(|mount| mount.prefix.join())
            .collect::<Vec<_>>(),
        vec!["/base".to_string(), "/app".to_string()]
    );

    fs::remove_dir_all(root).unwrap();
}
