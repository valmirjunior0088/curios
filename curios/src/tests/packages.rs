//! A package, compiled and run the way the CLI does it.
//!
//! Everything below `Selection::of` is already covered where it lives — the manifest's refusals in `curios-package`, the layout rule in `curios-text`, the fold in `curios-pipeline`. What no other test reaches is the whole chain at once: a manifest on disk deciding what to compile, a governance walk deciding what governs it, a dependency graph deciding the order, and a program that actually runs at the end of it. A wiring mistake anywhere in that chain passes every unit test and fails here.

use {
    crate::run_wasm,
    curios_package::{Entry, Selection, Spelling},
    curios_pipeline::{Cache, DEFAULT_STEP_BUDGET, Fold, Progress},
    curios_runtime::{ForeignBindings, MockHost},
    curios_text::{Entrypoint, Overlay, RootSource},
    curios_utilities::test_support::Temporary,
    curios_verdicts::Verdicts,
    std::{
        fs,
        path::{Path, PathBuf},
    },
};

/// The entry file and the units `target` resolves to as `run` resolves it, standing in `directory`.
fn resolved(directory: &Path, target: Option<&str>) -> (PathBuf, Vec<RootSource>) {
    let program = match Selection::of(Spelling::of(target), None, directory, &Overlay::default())
        .expect("a governed package")
    {
        Selection::Program(program) => program,
        Selection::Entire(entire) => entire.default_program().expect("a governed package"),
        Selection::Library(_) => panic!("these fixtures name their executables, never a library"),
    };
    let Entry::File(entry) = program.entry().clone() else {
        panic!("these fixtures name a target on disk");
    };

    (entry, program.into_units())
}

/// A tree of `(relative path, contents)` pairs, in a directory of its own that goes away with the test.
fn tree(name: &str, files: &[(&str, &str)]) -> Temporary {
    let root = Temporary::new("packages", name);

    for (path, contents) in files {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, contents).unwrap();
    }

    fs::create_dir_all(&root).unwrap();

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
    let (entry, units) = resolved(directory, target);

    let (entrypoint, loader, _source) = Entrypoint::opened(&entry).expect("the entry parses");
    let (module, _foreigns) = Fold::new(DEFAULT_STEP_BUDGET, &units, cache)
        .compile(&entrypoint, &loader, |_| {}, |_| {})
        .expect("the package compiles");

    let (system, io) = MockHost::builder().build();
    run_wasm(&module, system, ForeignBindings::empty()).expect("the program runs");

    io.output().to_vec()
}

/// What the fold did to each unit `target` resolves to, standing in `directory`, through `cache`: read off its progress, one line a unit, in fold order.
fn folded(directory: &Path, target: Option<&str>, cache: &dyn Cache) -> Vec<String> {
    let (entry, units) = resolved(directory, target);

    let (entrypoint, loader, _source) = Entrypoint::opened(&entry).expect("the entry parses");
    let mut events = Vec::new();
    Fold::new(DEFAULT_STEP_BUDGET, &units, Some(cache))
        .compile(
            &entrypoint,
            &loader,
            |_| {},
            |progress| match progress {
                Progress::Compiling(prefix) => events.push(format!("compiling {}", prefix.join())),
                Progress::Recompiling(prefix) => {
                    events.push(format!("recompiling {}", prefix.join()));
                }
                Progress::Reused(prefix) => events.push(format!("reused {}", prefix.join())),
                _ => {}
            },
        )
        .expect("the package compiles");

    events
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
}

/// A file no unit declares is loose: standing inside a package, a `.crs` path the library's `mod` lines do not reach compiles on its own, with the package's library *not* in scope.
#[test]
fn a_file_no_unit_declares_compiles_loose_inside_a_package() {
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
    let (entry, units) = resolved(&root, Some(orphan.to_str().unwrap()));
    assert!(units.is_empty(), "a file argument mounts nothing");

    let (entrypoint, loader, _source) = Entrypoint::opened(&entry).expect("the entry parses");
    assert!(
        Fold::new(DEFAULT_STEP_BUDGET, &units, None)
            .compile(&entrypoint, &loader, |_| {}, |_| {})
            .is_err(),
        "the package's library is not in a bare file's scope"
    );
}

/// A loose program is compiled against the prelude and nothing else: it names `/std`, and `/sys`, the root beneath it, stays the standard library's own.
#[test]
fn a_loose_program_names_std_and_is_refused_sys() {
    let root = tree(
        "e2e-loose-scope",
        &[
            ("std.crs", "/std/print(\"std\")\n"),
            (
                "sys.crs",
                "let _mode = /sys/open_mode/read;\n\n/std/print(\"sys\")\n",
            ),
        ],
    );

    assert_eq!(
        run(&root, Some(root.join("std.crs").to_str().unwrap())),
        b"std"
    );

    let (entry, units) = resolved(&root, Some(root.join("sys.crs").to_str().unwrap()));
    let (entrypoint, loader, _source) = Entrypoint::opened(&entry).expect("the entry parses");
    let refusal = Fold::new(DEFAULT_STEP_BUDGET, &units, None)
        .compile(&entrypoint, &loader, |_| {}, |_| {})
        .map(|_| ())
        .expect_err("a loose program cannot name /sys")
        .to_string();
    assert!(
        refusal.contains("`sys` is internal to the standard library"),
        "{refusal}"
    );
}

/// The orphan rule between two roots, neither of them the standard library and neither the entry.
///
/// Every other orphan fixture sets an entry program against `/std`, the one pairing a real project never meets trouble in. Here `base` declares a concept and a type, and `app`'s library — which depends on `base` and owns neither — declares the witness: the refusal that keeps two independent packages from each claiming one key. The control keys the same concept on a type `app` declares, which the rule's second clause admits, and the program runs through it.
#[test]
fn a_package_may_not_witness_anothers_concept_at_anothers_type() {
    let base = "pub concept Tag(A: Type): pub Type {\n    tag(A) -> /std/Nat,\n}\n\npub induct Thing: pub Type\n| mk()\nend\n";
    let manifest = "name = \"app\"\n\n[dependencies]\nbase = { source = \"member\" }\n\n[[executables]]\nname = \"app\"\n";
    let package = |name: &str, library: &str, program: &str| {
        tree(
            name,
            &[
                ("curios.toml", "members = [\"app\", \"base\"]\n"),
                ("base/curios.toml", "name = \"base\"\n"),
                ("base/lib.crs", base),
                ("app/curios.toml", manifest),
                ("app/lib.crs", library),
                ("app/app.crs", program),
            ],
        )
    };

    let orphan = package(
        "e2e-orphan",
        "use /base/{Tag, Thing};\n\nsatisfy Tag(Thing) {\n    tag(_) = 1,\n}\n",
        "/std/print(\"unreached\")\n",
    );
    let (entry, units) = resolved(&orphan.join("app"), None);
    let (entrypoint, loader, _source) = Entrypoint::opened(&entry).expect("the entry parses");
    let refusal = Fold::new(DEFAULT_STEP_BUDGET, &units, None)
        .compile(&entrypoint, &loader, |_| {}, |_| {})
        .map(|_| ())
        .expect_err("a package owning neither the concept nor the type declares no witness")
        .to_string();
    assert!(
        refusal.contains("orphan witness of 'Tag' for head 'Thing'"),
        "{refusal}"
    );

    let owned = package(
        "e2e-orphan-control",
        "use /base/{Tag};\n\npub induct Mine: pub Type\n| mk()\nend\n\nsatisfy Tag(Mine) {\n    tag(_) = 7,\n}\n\npub let seven: /std/Nat = Tag/tag(Mine/mk());\n",
        "/std/print(/std/Nat/to_str(/app/seven))\n",
    );
    assert_eq!(run(&owned.join("app"), None), b"7");
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
    let store = Verdicts::at(root.to_path_buf());

    let cold = cached(&app, None, Some(&store));
    assert_eq!(cold, b"42");
    assert!(
        root.join(".curios/verdicts").is_dir(),
        "the dependency's verdict is recorded"
    );

    // Keyed on content: an edited dependency is a different unit, whatever is in the store.
    fs::write(
        root.join("base/lib.crs"),
        "pub let answer : /std/Nat = 7;\n",
    )
    .unwrap();
    assert_eq!(
        cached(&app, None, Some(&Verdicts::at(root.to_path_buf()))),
        b"7"
    );

    // And restoring the content restores the verdict — the run this whole half exists for.
    fs::write(
        root.join("base/lib.crs"),
        "pub let answer : /std/Nat = 42;\n",
    )
    .unwrap();
    assert_eq!(
        cached(&app, None, Some(&Verdicts::at(root.to_path_buf()))),
        cold
    );
}

/// Registration through a library unit and the store: a dependency's `test` declaration is an item of its unit, so the unit that carries it is filed on the cold compile and restored on the warm one — a schema the field broke would fail loudly here.
#[test]
fn a_library_test_declaration_rides_through_the_store() {
    let root = tree(
        "e2e-test-decl",
        &[
            ("curios.toml", "members = [\"app\", \"base\"]\n"),
            ("base/curios.toml", "name = \"base\"\n"),
            (
                "base/lib.crs",
                "use /std/{Nat, Test};\npub let answer: Nat = 42;\ntest answer_holds =\n    Test/assert(answer == 42);\n",
            ),
            (
                "app/curios.toml",
                "name = \"app\"\n\n[dependencies]\nbase = { source = \"member\" }\n\n[[executables]]\nname = \"app\"\n",
            ),
            ("app/app.crs", "/std/print(/std/Nat/to_str(/base/answer))\n"),
        ],
    );
    let app = root.join("app");
    let store = Verdicts::at(root.to_path_buf());

    assert_eq!(cached(&app, None, Some(&store)), b"42");
    assert!(
        root.join(".curios/verdicts").is_dir(),
        "the dependency's verdict is recorded"
    );
    assert_eq!(
        cached(&app, None, Some(&Verdicts::at(root.to_path_buf()))),
        b"42"
    );
}

/// An umbrella of three libraries and a program: `app` depends on `alpha` and on `beta`, which do not name each other, and its library reads a declaration of each.
fn siblings(name: &str) -> Temporary {
    tree(
        name,
        &[
            ("curios.toml", "members = [\"app\", \"alpha\", \"beta\"]\n"),
            ("alpha/curios.toml", "name = \"alpha\"\n"),
            ("alpha/lib.crs", "pub let one : /std/Nat = 1;\n"),
            ("beta/curios.toml", "name = \"beta\"\n"),
            ("beta/lib.crs", "pub let two : /std/Nat = 2;\n"),
            (
                "app/curios.toml",
                "name = \"app\"\n\n[dependencies]\nalpha = { source = \"member\" }\nbeta = { source = \"member\" }\n\n[[executables]]\nname = \"app\"\n",
            ),
            (
                "app/lib.crs",
                "pub let sum : /std/Nat = /std/Nat/add(/alpha/one, /beta/two);\n",
            ),
            ("app/app.crs", "/std/print(/std/Nat/to_str(/app/sum))\n"),
        ],
    )
}

/// A unit is addressed by who it is, and believed on the units it could have read: an edit to one of two packages that do not name each other leaves the other's unit reused, and the unit that depends on both is compiled over the one it filed, each stage asking again what it read of the package that moved.
///
/// Mutation-checked against the record: with a unit's record naming every unit the fold took before it, `/beta` is compiled over its baseline for an edit to `/alpha` it cannot read.
#[test]
fn an_edit_to_one_package_leaves_a_sibling_it_cannot_read_reused() {
    let root = siblings("e2e-siblings");
    let app = root.join("app");
    let store = || Verdicts::at(root.to_path_buf());

    assert_eq!(
        folded(&app, None, &store()),
        ["compiling /alpha", "compiling /beta", "compiling /app"]
    );
    assert_eq!(
        folded(&app, None, &store()),
        ["reused /alpha", "reused /beta", "reused /app"]
    );

    fs::write(root.join("alpha/lib.crs"), "pub let one : /std/Nat = 3;\n").unwrap();
    assert_eq!(
        folded(&app, None, &store()),
        ["recompiling /alpha", "reused /beta", "recompiling /app"],
        "a sibling was compiled again, or a dependent compiled whole after a dependency it had a baseline for"
    );
    assert_eq!(cached(&app, None, Some(&store())), b"5");
}

/// A unit two folds reach after different others is filed once: `left` depends on `alpha` and `beta`, `right` on `beta` alone, and the `/beta` the first fold filed is the one the second is handed, though the first took `/alpha` ahead of it.
///
/// Mutation-checked against the record: naming every unit the fold took before its own, the `/beta` filed after `/alpha` is a baseline to the second fold and no hit.
#[test]
fn a_unit_reached_after_different_others_is_filed_once() {
    let root = tree(
        "e2e-shared",
        &[
            (
                "curios.toml",
                "members = [\"left\", \"right\", \"alpha\", \"beta\"]\n",
            ),
            ("alpha/curios.toml", "name = \"alpha\"\n"),
            ("alpha/lib.crs", "pub let one : /std/Nat = 1;\n"),
            ("beta/curios.toml", "name = \"beta\"\n"),
            ("beta/lib.crs", "pub let two : /std/Nat = 2;\n"),
            (
                "left/curios.toml",
                "name = \"left\"\n\n[dependencies]\nalpha = { source = \"member\" }\nbeta = { source = \"member\" }\n\n[[executables]]\nname = \"left\"\n",
            ),
            (
                "left/left.crs",
                "/std/print(/std/Nat/to_str(/std/Nat/add(/alpha/one, /beta/two)))\n",
            ),
            (
                "right/curios.toml",
                "name = \"right\"\n\n[dependencies]\nbeta = { source = \"member\" }\n\n[[executables]]\nname = \"right\"\n",
            ),
            (
                "right/right.crs",
                "/std/print(/std/Nat/to_str(/beta/two))\n",
            ),
        ],
    );
    let store = || Verdicts::at(root.to_path_buf());

    assert_eq!(
        folded(&root.join("left"), None, &store()),
        ["compiling /alpha", "compiling /beta"]
    );
    assert_eq!(
        folded(&root.join("right"), None, &store()),
        ["reused /beta"],
        "a unit filed after another it cannot read was not the one a fold without that other is handed"
    );
    assert_eq!(
        folded(&root.join("left"), None, &store()),
        ["reused /alpha", "reused /beta"]
    );
}
