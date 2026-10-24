use {
    super::*,
    curios_utilities::{Qualifier, test_support::Temporary},
    std::fs,
};

/// A package in a directory of its own that goes away with the test, holding `manifest` and the files `files` names, relative to it.
fn package(name: &str, manifest: &str, files: &[(&str, &str)]) -> Temporary {
    let directory = Temporary::new("layout", name);
    fs::create_dir_all(&directory).unwrap();
    fs::write(directory.join(MANIFEST), manifest).unwrap();

    for (path, source) in files {
        let path = directory.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, source).unwrap();
    }

    directory
}

/// The library header anchors at the manifest's directory: `mod parse` is `parse.crs` beside it, and `parse`'s own children stem-nest below.
#[test]
fn a_package_mounts_its_declared_name_over_its_manifest_directory() {
    let directory = package(
        "layout-package",
        "compiler = \"0.0.0\"\nname = \"json\"\n",
        &[
            ("lib.crs", "pub mod parse;"),
            ("parse.crs", "pub mod lexer;"),
            ("parse/lexer.crs", "pub let token : Type = Type;"),
        ],
    );

    let (package, source) = package_at(&directory).expect("a package with a library header");

    assert_eq!(package.name, "json");
    assert_eq!(
        source
            .expect("a package with a library header")
            .mounts()
            .first()
            .map(|mount| mount.prefix),
        Some(Qualifier::from(["json"]))
    );
}

/// A package of nothing but programs has no body, and no vestigial file to write saying so.
#[test]
fn a_package_with_no_library_header_has_no_library() {
    let directory = package(
        "layout-headerless",
        "compiler = \"0.0.0\"\nname = \"json\"\n\n[[executables]]\nname = \"serve\"\n",
        &[("serve.crs", "")],
    );

    let (package, source) = package_at(&directory).expect("a package of programs alone");

    assert!(source.is_none());
    assert_eq!(package.executables.len(), 1);
}

/// Only the header's *absence* is an answer: one that fails to parse is still mounted, and the refusal is discovery's to raise — located at the line that failed, rather than text raised here.
#[test]
fn an_unparsable_library_header_is_mounted_for_discovery_to_refuse() {
    let directory = package(
        "layout-broken-header",
        "compiler = \"0.0.0\"\nname = \"json\"\n",
        &[("lib.crs", "pub let x : = ;")],
    );

    let (_, source) = package_at(&directory).expect("the layout is not what is wrong");
    assert!(source.is_some(), "the header is there to be read");
}

/// The package's own program and its library both read their modules beside the manifest, and the package is one unit apiece where their chains reach different files.
#[test]
fn a_program_beside_the_manifest_reads_its_modules_beside_it() {
    let directory = package(
        "layout-program-beside",
        "compiler = \"0.0.0\"\nname = \"json\"\n",
        &[
            ("lib.crs", "pub mod parse;"),
            ("parse.crs", ""),
            ("exe.crs", "mod util;\n/std/print(util/greeting)\n"),
            ("util.crs", "pub let greeting: /std/Str = \"hello\\n\";"),
        ],
    );

    package_at(&directory).expect("two units, each with files of its own");
}

/// A module two units' chains reach would be compiled into each as two unrelated modules: refused naming both, with the library as where it lives and the path the program reaches it by.
#[test]
fn a_module_two_units_declare_is_refused() {
    let directory = package(
        "layout-shared-module",
        "compiler = \"0.0.0\"\nname = \"json\"\n",
        &[
            ("lib.crs", "pub mod parse;"),
            ("exe.crs", "mod parse;\n/std/print(\"\")\n"),
            ("parse.crs", ""),
        ],
    );

    let refusal = package_at(&directory)
        .map(|_| ())
        .expect_err("a module two units declare");
    assert!(
        refusal.contains(
            "`parse.crs` is a module of the library and of the package's own executable \"json\""
        ),
        "{refusal}"
    );
    assert!(
        refusal.contains("reach it from the other through `/json/parse`"),
        "{refusal}"
    );
}

/// An executable compiled from a file the library declares as a module is a program and a module at once.
#[test]
fn a_program_the_library_declares_as_a_module_is_refused() {
    let directory = package(
        "layout-program-named-as-module",
        "compiler = \"0.0.0\"\nname = \"json\"\n\n[[executables]]\nname = \"parse\"\n",
        &[("lib.crs", "pub mod parse;"), ("parse.crs", "")],
    );

    let refusal = package_at(&directory)
        .map(|_| ())
        .expect_err("a program and a module at once");
    assert!(
        refusal.contains("the executable \"parse\" is compiled from `parse.crs`, which the library also declares as the module `/json/parse`"),
        "{refusal}"
    );
}

/// Two programs compiled from one file are refused whether or not there is a library: a package of nothing but programs has units sharing its directory too.
///
/// Checked only where there is a library, a row aliasing the discovered `exe.crs` — two names for one file, two payload slots, two emitted binaries — would be refused in a package with a library and accepted in one without, so adding or deleting a header would turn the rule on and off for a manifest that had not changed.
#[test]
fn two_programs_compiled_from_one_file_are_refused_without_a_library() {
    let directory = package(
        "layout-one-file-two-programs",
        "compiler = \"0.0.0\"\nname = \"json\"\n\n[[executables]]\nname = \"foo\"\npath = \"exe.crs\"\n",
        &[("exe.crs", "")],
    );

    let refusal = package_at(&directory)
        .map(|_| ())
        .expect_err("two programs from one file, library or no library");
    assert!(refusal.contains("the executable \"foo\""), "{refusal}");
    // The second claimant is one the manifest never wrote, so the refusal says what wrote it.
    assert!(
        refusal.contains(
            "the package's own executable \"json\" that `exe.crs` beside the manifest declares"
        ),
        "{refusal}"
    );
    assert!(
        refusal.contains("are both compiled from `exe.crs`"),
        "{refusal}"
    );
}

/// A header that does not parse is discovery's refusal to report, and what the executables claim does not wait on it.
#[test]
fn an_unparseable_header_still_leaves_the_executables_checked() {
    let directory = package(
        "layout-one-file-unparseable",
        "compiler = \"0.0.0\"\nname = \"json\"\n\n[[executables]]\nname = \"foo\"\npath = \"exe.crs\"\n",
        &[("lib.crs", "pub let ="), ("exe.crs", "")],
    );

    let refusal = package_at(&directory)
        .map(|_| ())
        .expect_err("two programs from one file");
    assert!(
        refusal.contains("are both compiled from `exe.crs`"),
        "{refusal}"
    );
}

/// An executable below the package root reads its modules from where its entry sits, and a library module spelled like it is another file.
#[test]
fn an_executable_below_the_root_reads_its_modules_where_it_sits() {
    let directory = package(
        "layout-program-below",
        "compiler = \"0.0.0\"\nname = \"json\"\n\n[[executables]]\nname = \"parse\"\npath = \"tools/parse.crs\"\n",
        &[
            ("lib.crs", "pub mod parse;"),
            ("parse.crs", ""),
            ("tools/parse.crs", "mod helper;\n/std/print(\"\")\n"),
            ("tools/helper.crs", ""),
        ],
    );

    package_at(&directory).expect("an executable below the package root");
}

/// A row may point below the root, into a module's namespace directory, and a `mod` there may reach the same file. The refusal names the row and the module, where the module loader alone would report a parse error at the program's first token.
#[test]
fn an_executable_the_library_declares_as_a_module_is_refused() {
    let directory = package(
        "layout-program-as-module",
        "compiler = \"0.0.0\"\nname = \"nested\"\n\n[[executables]]\nname = \"tool\"\npath = \"app/main.crs\"\n",
        &[
            ("lib.crs", "pub mod app;"),
            ("app.crs", "pub mod main;"),
            ("app/main.crs", "/std/print(\"hello\\n\")"),
        ],
    );

    let refusal = package_at(&directory)
        .map(|_| ())
        .expect_err("a file that is a program and a module");
    assert!(refusal.contains("the executable \"tool\""), "{refusal}");
    assert!(
        refusal.contains("declares as the module `/nested/app/main`"),
        "{refusal}"
    );
}

/// An umbrella compiles nothing of its own, and saying so beats an absent-library refusal about a file it never wanted.
#[test]
fn an_umbrella_is_not_a_unit() {
    let directory = package(
        "layout-umbrella",
        "compiler = \"0.0.0\"\nmembers = [\"json\"]\n",
        &[],
    );

    let refusal = package_at(&directory)
        .map(|_| ())
        .expect_err("an umbrella is not a unit");
    assert!(refusal.contains("compiles nothing of its own"), "{refusal}");
}
