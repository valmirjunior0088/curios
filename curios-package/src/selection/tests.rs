//! What an argument selects: how it is spelled, the program a name or no argument means, and the unit that declares a file.

use {super::*, curios_utilities::test_support::Temporary, std::fs};

/// A tree of `(relative path, contents)` pairs, in a directory of its own that goes away with the test.
fn tree(name: &str, files: &[(&str, &str)]) -> Temporary {
    let root = Temporary::new("selection", name);

    for (path, source) in files {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, source).unwrap();
    }

    fs::create_dir_all(&root).unwrap();

    root
}

/// What `argument` selects standing in `directory` — a file argument made whole under `directory`, since placement reads a file where it is and a test cannot move the process there.
fn selection(argument: Option<&str>, directory: &Path) -> Result<Selection, String> {
    let spelling = match Spelling::of(argument) {
        Spelling::File(path) => Spelling::File(directory.join(path)),
        spelling => spelling,
    };

    Selection::of(spelling, None, directory, &Overlay::default())
}

/// The program `argument` means to a command that needs one, standing in `directory`, or the refusal it earns.
fn selected(argument: Option<&str>, directory: &Path) -> Result<Program, String> {
    match selection(argument, directory)? {
        Selection::Program(program) => Ok(program),
        Selection::Entire(entire) => entire.default_program(),
        Selection::Library(_) => panic!("{argument:?} selects a library"),
    }
}

/// The entry file `argument` resolves to inside `directory`, or the refusal it earns.
fn entry(argument: Option<&str>, directory: &Path) -> Result<PathBuf, String> {
    match selected(argument, directory)?.entry() {
        Entry::Stdin => panic!("standard input has no entry file"),
        Entry::File(path) => Ok(path.clone()),
    }
}

/// The prefixes `units` mount, in order.
fn prefixes(units: Vec<RootSource>) -> Vec<String> {
    units
        .iter()
        .flat_map(|source| source.mounts())
        .map(|mount| mount.prefix.join())
        .collect()
}

/// **The dispatch.** A spelling is decided by its text alone — `-`, anything path-shaped, and every other word — before anything looks at the disk.
#[test]
fn a_spelling_is_decided_by_its_text_alone() {
    assert_eq!(Spelling::of(None), Spelling::Nothing);
    assert_eq!(Spelling::of(Some("-")), Spelling::Stdin);
    for file in ["scratch.crs", "./serve", "sub/dir/x", "sub\\dir\\x"] {
        assert_eq!(
            Spelling::of(Some(file)),
            Spelling::File(PathBuf::from(file)),
            "{file}"
        );
    }
    assert_eq!(
        Spelling::of(Some("serve")),
        Spelling::Name("serve".to_string())
    );
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
    // `compile` and `wonder stage` resolve a bare target through the same call, so the refusal names no subcommand.
    assert!(!refusal.contains("`run`"), "{refusal}");

    let decided = tree("run-default", &{
        let mut files = files.to_vec();
        files.push((
            "curios.toml",
            "name = \"app\"\ndefault = \"bench\"\n\n[[executables]]\nname = \"serve\"\n\n[[executables]]\nname = \"bench\"\n",
        ));
        files
    });

    assert_eq!(entry(None, &decided).unwrap(), decided.join("bench.crs"));
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
}

/// **The placement.** A file is placed by what declares it, whichever command asks: an executable's entry is that executable, and a file beside it that no `mod` declares is loose, told what would declare it.
#[test]
fn a_file_is_placed_by_what_declares_it_not_by_where_it_sits() {
    let root = tree(
        "placed-by-declaration",
        &[
            (
                "curios.toml",
                "name = \"app\"\n\n[[executables]]\nname = \"serve\"\n",
            ),
            ("lib.crs", ""),
            ("serve.crs", "/std/print(\"\")\n"),
            ("scratch.crs", ""),
        ],
    );

    let entry = selected(Some("serve.crs"), &root).expect("the entry");
    assert_eq!(
        entry.home().expect("a declared program").executable,
        "serve"
    );

    let scratch = selected(Some("scratch.crs"), &root).expect("a loose file");
    assert!(scratch.home().is_none());
    let unlinked = scratch
        .unlinked()
        .expect("the package holds it, and says why it is in no unit");
    assert_eq!(
        unlinked.message,
        format!(
            "{} is in no unit of `/app`, so it was checked on its own against `/std`: declare it with `mod scratch;` in lib.crs",
            root.join("scratch.crs").display()
        )
    );
}

/// A file no manifest governs is loose, and has nothing to be told.
#[test]
fn a_file_argument_needs_no_project() {
    let root = tree("run-standalone", &[("scratch.crs", "")]);

    let program = selected(Some("scratch.crs"), &root).expect("a loose file");
    assert_eq!(program.entry(), &Entry::File(root.join("scratch.crs")));
    assert!(program.home().is_none() && program.unlinked().is_none());
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

    let program = selected(None, &root.join("json")).expect("an enumerated member");

    // The umbrella governs, so the store is its own — but the path *within* the store names the package, so it would not move if the member left.
    let home = program
        .home()
        .expect("an enumerated member is a declared executable");
    assert!(
        home.output.ends_with(".curios/executables/json/serve"),
        "{}",
        home.output.display()
    );
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

    for directory in [&*root, Path::new(".")] {
        let program = selected(Some("-"), directory).expect("standard input needs no project");
        assert!(
            program.entry() == &Entry::Stdin && program.home().is_none(),
            "`-` should dispatch as standard input in {}",
            directory.display()
        );
    }
}

/// An anonymous program has no file to read, so the answer is absent rather than invented.
#[test]
fn standard_input_has_no_entry_file() {
    let program = selected(Some("-"), Path::new(".")).expect("standard input");

    assert_eq!(program.entry(), &Entry::Stdin);
}

/// Standard input is answered before any manifest is looked for, so naming one beside it is refused rather than ignored.
#[test]
fn standard_input_is_governed_by_no_manifest() {
    let root = tree("stdin-manifest", &[("curios.toml", "name = \"app\"\n")]);

    let Err(refusal) = Selection::of(
        Spelling::Stdin,
        Some(&root.join("curios.toml")),
        &root,
        &Overlay::default(),
    ) else {
        panic!("a manifest cannot govern standard input");
    };
    assert!(refusal.contains("no manifest governs"), "{refusal}");
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

    let program = selected(None, &root.join("app")).expect("a package with no library");

    assert_eq!(program.entry(), &Entry::File(root.join("app/serve.crs")));
    assert_eq!(prefixes(program.into_units()), vec!["/base".to_string()]);
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

    let program = selected(None, &root.join("app")).expect("a sole executable");

    assert_eq!(
        program.home().expect("a declared executable").executable,
        "serve"
    );
    assert_eq!(
        prefixes(program.into_units()),
        vec!["/base".to_string(), "/app".to_string()]
    );
}

/// The package's own executable is found rather than declared, so a refusal listing it says what declared it: a reader who wrote one row and is told the package declares two would otherwise look for the second in the manifest.
#[test]
fn a_found_executable_is_listed_with_the_file_that_declares_it() {
    let root = tree(
        "run-found-listed",
        &[
            (
                "curios.toml",
                "name = \"app\"\n\n[[executables]]\nname = \"serve\"\n",
            ),
            ("serve.crs", ""),
            ("exe.crs", ""),
        ],
    );

    let refusal = entry(None, &root).expect_err("two executables and no default");
    assert!(
        refusal.ends_with(
            "; it declares the executable \"serve\", the package's own executable \"app\" that `exe.crs` beside the manifest declares"
        ),
        "{refusal}"
    );
}

/// A `default` naming the package itself is the one the parser lets through without a row, on the promise that a run says what is missing: so the refusal names `exe.crs`, the file whose presence would have declared it.
#[test]
fn a_default_naming_the_absent_own_executable_names_the_file_that_would_declare_it() {
    let root = tree(
        "run-own-absent",
        &[
            (
                "curios.toml",
                "name = \"app\"\ndefault = \"app\"\n\n[[executables]]\nname = \"serve\"\n",
            ),
            ("lib.crs", ""),
            ("serve.crs", ""),
        ],
    );

    let refusal = entry(None, &root).expect_err("no exe.crs to mean");
    assert_eq!(
        refusal,
        "\"app\" has no executable of its own: no `exe.crs` sits beside the manifest, and no `[[executables]]` row declares one by that name; it declares the executable \"serve\""
    );

    // Asked for by name rather than through `default`, the same absence earns the same words.
    let refusal = entry(Some("app"), &root).expect_err("no exe.crs to mean");
    assert!(
        refusal.starts_with("\"app\" has no executable of its own"),
        "{refusal}"
    );
}

/// A package declaring no executable refuses a bare target naming the two ways to declare one — and no subcommand, since `compile` and `wonder stage` reach the same refusal.
#[test]
fn a_bare_target_on_a_package_of_a_library_alone_says_how_to_declare_one() {
    let root = tree(
        "run-no-executable",
        &[("lib.crs", ""), ("curios.toml", "name = \"app\"\n")],
    );

    let refusal = entry(None, &root).expect_err("no executable to mean");
    assert_eq!(
        refusal,
        "\"app\" declares no executable: add `exe.crs`, or declare one with `[[executables]]`"
    );
}

/// No argument selects the package entire: its library, and then every program it declares, in order.
#[test]
fn no_argument_selects_the_package_entire() {
    let root = tree(
        "entire",
        &[
            (
                "curios.toml",
                "name = \"app\"\n\n[[executables]]\nname = \"serve\"\n\n[[executables]]\nname = \"bench\"\n",
            ),
            ("lib.crs", ""),
            ("serve.crs", ""),
            ("bench.crs", ""),
        ],
    );

    let Selection::Entire(entire) =
        Selection::of(Spelling::Nothing, None, &root, &Overlay::default())
            .expect("a governed package")
    else {
        panic!("no argument is the package entire");
    };

    let library = entire.library().expect("a scope").expect("a library");
    assert_eq!(prefixes(library.units), vec!["/app".to_string()]);

    let programs = entire.programs().expect("every program");
    let names = programs
        .iter()
        .map(|program| {
            program
                .home()
                .expect("a declared program")
                .executable
                .as_str()
        })
        .collect::<Vec<_>>();
    assert_eq!(names, vec!["serve", "bench"]);
}

/// A module the library's header declares is placed in the library, selected through that file.
#[test]
fn a_declared_library_module_selects_the_library_through_it() {
    let root = tree(
        "placed-library",
        &[
            ("curios.toml", "name = \"app\"\n"),
            ("lib.crs", "pub mod util;\n"),
            ("util.crs", ""),
        ],
    );

    let Selection::Library(library) = selection(Some("util.crs"), &root).expect("a placed file")
    else {
        panic!("a declared module of the library is the library");
    };

    assert_eq!(library.through, Some(root.join("util.crs")));
    assert_eq!(library.units.len(), 1);
}

/// An executable's entry is its program, a module the entry's `mod` chain reaches is the same program selected through that module, and a file under the entry's stem directory that no `mod` reaches is loose — told the line that would declare it.
#[test]
fn a_module_an_entry_declares_selects_its_program_and_one_it_does_not_is_loose() {
    let root = tree(
        "placed-program",
        &[
            (
                "curios.toml",
                "name = \"app\"\n\n[[executables]]\nname = \"serve\"\n",
            ),
            ("serve.crs", "mod helper;\n/std/print(\"\")\n"),
            ("serve/helper.crs", ""),
            ("serve/other.crs", ""),
        ],
    );

    let entry = selected(Some("serve.crs"), &root).expect("the entry");
    assert_eq!(entry.through(), None);

    let module = selected(Some("serve/helper.crs"), &root).expect("a declared module");
    assert_eq!(module.entry(), &Entry::File(root.join("serve.crs")));
    assert_eq!(
        module.through(),
        Some(root.join("serve/helper.crs").as_path())
    );

    let other = selected(Some("serve/other.crs"), &root).expect("an undeclared module");
    assert!(other.home().is_none());
    assert!(
        other
            .unlinked()
            .expect("the package holds it")
            .message
            .ends_with("declare it with `mod other;` in serve.crs"),
        "{:?}",
        other.unlinked()
    );
}

/// What would put an unlinked file in a unit: the `mod` line in the header of the module above it, nothing for a spelling no module has, and a library to write it in for a package that has none.
#[test]
fn an_unlinked_file_is_told_what_would_declare_it() {
    let root = tree(
        "unlinked-remedies",
        &[
            ("curios.toml", "name = \"app\"\n"),
            ("lib.crs", "pub mod parse;\n"),
            ("parse.crs", ""),
            ("parse/lexer.crs", ""),
            ("notes.txt", ""),
            ("odd-name.crs", ""),
        ],
    );

    for (file, remedy) in [
        (
            "parse/lexer.crs",
            "declare it with `mod lexer;` in parse.crs",
        ),
        (
            "./notes.txt",
            "its spelling names no module a `mod` could declare",
        ),
        (
            "odd-name.crs",
            "its spelling names no module a `mod` could declare",
        ),
    ] {
        let message = selected(Some(file), &root)
            .expect("a loose file")
            .unlinked()
            .map(|unlinked| unlinked.message.clone())
            .unwrap_or_else(|| panic!("{file} is held by the package"));
        assert!(message.ends_with(remedy), "{file}: {message}");
    }

    let programs = tree(
        "unlinked-no-library",
        &[
            ("curios.toml", "name = \"tool\"\n"),
            ("exe.crs", "/std/print(\"\")\n"),
            ("stray.crs", ""),
        ],
    );
    let message = selected(Some("stray.crs"), &programs)
        .expect("a loose file")
        .unlinked()
        .map(|unlinked| unlinked.message.clone())
        .expect("the package holds it");
    assert!(
        message.ends_with("`/tool` has no library to declare it in"),
        "{message}"
    );
}

/// A file no manifest above it governs is loose, with nothing to be told.
#[test]
fn a_placed_file_no_manifest_governs_is_loose() {
    let root = tree("placed-loose", &[("scratch.crs", "")]);

    let Selection::Program(program) =
        selection(Some("scratch.crs"), &root).expect("an answer, not a refusal")
    else {
        panic!("a file no unit declares is a program of its own");
    };

    assert_eq!(program.entry(), &Entry::File(root.join("scratch.crs")));
    assert!(program.home().is_none() && program.declares().is_none());
}

/// An umbrella enumerating one member, with a stray file at its root and another in a directory no member holds.
fn umbrella(name: &str) -> Temporary {
    tree(
        name,
        &[
            ("curios.toml", "members = [\"app\"]\n"),
            ("app/curios.toml", "name = \"app\"\n"),
            ("app/lib.crs", "pub mod util;\n"),
            ("app/util.crs", ""),
            ("scratch.crs", ""),
            ("tools/scratch.crs", ""),
        ],
    )
}

/// An umbrella declares no units, so a file whose nearest manifest is one belongs to nothing and is loose, with no package to say anything about it.
#[test]
fn a_file_no_member_holds_under_an_umbrella_is_loose() {
    let root = umbrella("umbrella-stray");

    for stray in ["scratch.crs", "tools/scratch.crs"] {
        let program = selected(Some(stray), &root).expect("an answer, not a refusal");
        assert!(
            program.home().is_none() && program.unlinked().is_none(),
            "{stray}"
        );
    }
}

/// A member's module is the member's library, under the umbrella's root — the nearest manifest is the member's, and the umbrella governs it.
#[test]
fn a_members_module_is_placed_in_its_library_under_the_umbrella_root() {
    let root = umbrella("umbrella-member");

    let Selection::Library(library) =
        selection(Some("app/util.crs"), &root).expect("a placed file")
    else {
        panic!("a declared module of the library is the library");
    };
    assert_eq!(library.root, root.to_path_buf());
    assert_eq!(library.units.len(), 1);
}

/// Naming an umbrella outright with `--manifest` is the refusal it always was: nothing can be asked of a manifest that compiles nothing.
#[test]
fn an_umbrella_named_outright_is_still_refused() {
    let root = umbrella("umbrella-named");

    let Err(refusal) = Selection::of(
        Spelling::File(root.join("scratch.crs")),
        Some(&root.join("curios.toml")),
        &root,
        &Overlay::default(),
    ) else {
        panic!("an umbrella named by hand is refused");
    };
    assert!(refusal.contains("declares an umbrella"), "{refusal}");
}

/// `--manifest` names which package places a file, so a file outside that package's directory is refused rather than placed by a manifest that cannot declare it.
#[test]
fn a_named_manifest_places_no_file_outside_its_package() {
    let root = umbrella("manifest-outside");

    let Err(refusal) = Selection::of(
        Spelling::File(root.join("scratch.crs")),
        Some(&root.join("app/curios.toml")),
        &root,
        &Overlay::default(),
    ) else {
        panic!("a file outside the named package is refused");
    };
    assert!(refusal.contains("is outside the package"), "{refusal}");
}
