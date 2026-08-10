//! A package, compiled and run the way the CLI does it.
//!
//! Everything below `Target::of` is already covered where it lives — the manifest's refusals in `curios-package`, the layout rule in `curios-text`, the fold in `curios-pipeline`. What no other test reaches is the whole chain at once: a manifest on disk deciding what to compile, a governance walk deciding what governs it, a dependency graph deciding the order, and a program that actually runs at the end of it. A wiring mistake anywhere in that chain passes every unit test and fails here.

use {
    crate::{DEFAULT_STEP_BUDGET, Verdicts, compile_with_units, load, run_wasm},
    curios_package::Target,
    curios_pipeline::Cache,
    curios_runtime::{ForeignBindings, MockHost},
    std::{
        fs,
        path::{Path, PathBuf},
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

    for (path, contents) in files {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, contents).unwrap();
    }

    root
}

/// What `curios run` prints, standing in `directory` and asking for `target`.
///
/// The same three steps `main` takes: resolve what was asked for, compile it against what its manifest declares, run it.
fn run(directory: &Path, target: Option<&str>) -> Vec<u8> {
    cached(directory, target, None)
}

/// The same, consulting `cache` — which is what the command line does inside a project.
fn cached(directory: &Path, target: Option<&str>, cache: Option<&dyn Cache>) -> Vec<u8> {
    let (entry, units) = match Target::of(target, None, directory).expect("a governed package") {
        Target::Executable { entry, units, .. } => (entry, units),
        Target::File(path) => (path, Vec::new()),
    };

    let (entrypoint, loader) = load(&entry).expect("the entry parses");
    let (module, _foreigns) = compile_with_units(
        DEFAULT_STEP_BUDGET,
        &units,
        &entrypoint,
        loader,
        cache,
        |_| {},
    )
    .expect("the package compiles");

    let (system, io) = MockHost::builder().build();
    run_wasm(&module, system, ForeignBindings::empty()).expect("the program runs");

    io.output().to_vec()
}

/// A bare `run` means the sole executable, and that executable reaches its own package's library.
///
/// The library is mounted at the package's declared name, so `greeting` is reached as `/hello/greeting` — the manifest, not the directory, is what names that prefix.
#[test]
fn a_package_runs_its_sole_executable() {
    let root = tree(
        "e2e-solo",
        &[
            (
                "curios.toml",
                "name = \"hello\"\n\n[[executables]]\nname = \"hello\"\n",
            ),
            (
                "lib.crs",
                "pub let greeting : /std/Str = \"from the library\";\n",
            ),
            ("hello.crs", "/std/print(/hello/greeting)\n"),
        ],
    );

    assert_eq!(run(&root, None), b"from the library");
    // Naming it explicitly is the same program: the two forms differ in dispatch, not in what they resolve to.
    assert_eq!(run(&root, Some("hello")), b"from the library");

    fs::remove_dir_all(root).unwrap();
}

/// A multi-segment name mounts every segment, and the executable reaches through all of them.
#[test]
fn a_multi_segment_package_mounts_its_whole_name() {
    let root = tree(
        "e2e-multi",
        &[
            (
                "curios.toml",
                "name = \"myorg/json\"\n\n[[executables]]\nname = \"dump\"\n",
            ),
            ("lib.crs", "pub let tag : /std/Str = \"myorg/json\";\n"),
            ("dump.crs", "/std/print(/myorg/json/tag)\n"),
        ],
    );

    assert_eq!(run(&root, None), b"myorg/json");

    fs::remove_dir_all(root).unwrap();
}

/// The layout rule, through a real compilation: `mod` in the library header reads a sibling of the manifest, and that module's own children stem-nest below it.
#[test]
fn a_librarys_modules_read_from_the_manifests_directory() {
    let root = tree(
        "e2e-layout",
        &[
            (
                "curios.toml",
                "name = \"app\"\n\n[[executables]]\nname = \"app\"\n",
            ),
            ("lib.crs", "pub mod parse;\n"),
            ("parse.crs", "pub mod lexer;\n"),
            (
                "parse/lexer.crs",
                "pub let token : /std/Str = \"nested\";\n",
            ),
            ("app.crs", "/std/print(/app/parse/lexer/token)\n"),
        ],
    );

    assert_eq!(run(&root, None), b"nested");

    fs::remove_dir_all(root).unwrap();
}

/// One member reaching another through the umbrella that enumerates them both.
///
/// The whole graph in one program: the walk finds the umbrella above `app`, the `member` marker resolves `base` against its `members` list, the fold puts `base` first, and `app`'s library reaches into it.
#[test]
fn a_member_reaches_another_member_through_its_umbrella() {
    let root = tree(
        "e2e-umbrella",
        &[
            ("curios.toml", "members = [\"app\", \"base\"]\n"),
            ("base/curios.toml", "name = \"base\"\n"),
            ("base/lib.crs", "pub let answer : /std/Nat = 42;\n"),
            (
                "app/curios.toml",
                "name = \"app\"\n\n[dependencies]\nbase = { source = \"member\" }\n\n[[executables]]\nname = \"app\"\n",
            ),
            (
                "app/lib.crs",
                "pub let doubled : /std/Nat = /std/Nat/add(/base/answer, /base/answer);\n",
            ),
            ("app/app.crs", "/std/print(/std/Nat/to_str(/app/doubled))\n"),
        ],
    );

    assert_eq!(run(&root.join("app"), None), b"84");

    fs::remove_dir_all(root).unwrap();
}

/// A package of nothing but programs compiles them against its dependencies alone — no vestigial library, and nothing mounted for it.
#[test]
fn a_package_with_no_library_still_runs_its_program() {
    let root = tree(
        "e2e-programs-only",
        &[
            ("curios.toml", "members = [\"tool\", \"base\"]\n"),
            ("base/curios.toml", "name = \"base\"\n"),
            ("base/lib.crs", "pub let word : /std/Str = \"tool\";\n"),
            (
                "tool/curios.toml",
                "name = \"tool\"\n\n[dependencies]\nbase = { source = \"member\" }\n\n[[executables]]\nname = \"tool\"\n",
            ),
            ("tool/tool.crs", "/std/print(/base/word)\n"),
        ],
    );

    assert_eq!(run(&root.join("tool"), None), b"tool");

    fs::remove_dir_all(root).unwrap();
}

/// A file argument is captured by no manifest: standing inside a package, a `.crs` path compiles standalone, with the package's library *not* in scope.
#[test]
fn a_file_argument_compiles_standalone_inside_a_package() {
    let root = tree(
        "e2e-file",
        &[
            (
                "curios.toml",
                "name = \"hello\"\n\n[[executables]]\nname = \"hello\"\n",
            ),
            ("lib.crs", "pub let greeting : /std/Str = \"library\";\n"),
            ("hello.crs", "/std/print(/hello/greeting)\n"),
            ("scratch.crs", "/std/print(\"standalone\")\n"),
        ],
    );

    let scratch = root.join("scratch.crs");
    assert_eq!(
        run(&root, Some(scratch.to_str().unwrap())),
        b"standalone",
        "a file argument brings no project with it"
    );

    // And the proof that it brought none: the same file naming the library does not compile.
    let orphan = root.join("orphan.crs");
    fs::write(&orphan, "/std/print(/hello/greeting)\n").unwrap();
    let (entry, units) = match Target::of(Some(orphan.to_str().unwrap()), None, &root).unwrap() {
        Target::Executable { entry, units, .. } => (entry, units),
        Target::File(path) => (path, Vec::new()),
    };
    assert!(units.is_empty(), "a file argument mounts nothing");

    let (entrypoint, loader) = load(&entry).expect("the entry parses");
    assert!(
        compile_with_units(
            DEFAULT_STEP_BUDGET,
            &units,
            &entrypoint,
            loader,
            None,
            |_| {}
        )
        .is_err(),
        "the package's library is not in a bare file's scope"
    );

    fs::remove_dir_all(root).unwrap();
}

/// **A cached unit and a freshly elaborated one produce the same program**, and changing the terms invalidates.
///
/// The dependency is what gets stored — the entry never is, because it is what you are editing. So the third run below is the one that matters: it reuses a verdict reached in the first, and has to agree with it. The middle run is the control that says the store is keyed on content rather than merely written to, since an edited dependency must not be answered from it.
#[test]
fn a_stored_verdict_produces_the_program_a_fresh_one_does() {
    let root = tree(
        "e2e-cache",
        &[
            ("curios.toml", "members = [\"app\", \"base\"]\n"),
            ("base/curios.toml", "name = \"base\"\n"),
            ("base/lib.crs", "pub let answer : /std/Nat = 42;\n"),
            (
                "app/curios.toml",
                "name = \"app\"\n\n[dependencies]\nbase = { source = \"member\" }\n\n[[executables]]\nname = \"app\"\n",
            ),
            ("app/app.crs", "/std/print(/std/Nat/to_str(/base/answer))\n"),
        ],
    );
    let app = root.join("app");
    let store = Verdicts::at(root.clone());

    let cold = cached(&app, None, Some(&store));
    assert_eq!(cold, b"42");
    assert!(
        root.join(".curios/unit").is_dir(),
        "the dependency's verdict is recorded"
    );

    // Keyed on content: an edited dependency is a different unit, whatever is in the store.
    fs::write(
        root.join("base/lib.crs"),
        "pub let answer : /std/Nat = 7;\n",
    )
    .unwrap();
    assert_eq!(cached(&app, None, Some(&Verdicts::at(root.clone()))), b"7");

    // And restoring the content restores the verdict — the run this whole half exists for.
    fs::write(
        root.join("base/lib.crs"),
        "pub let answer : /std/Nat = 42;\n",
    )
    .unwrap();
    assert_eq!(cached(&app, None, Some(&Verdicts::at(root.clone()))), cold);

    fs::remove_dir_all(root).unwrap();
}
