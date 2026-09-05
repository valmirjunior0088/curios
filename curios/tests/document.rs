//! What the `document` subcommand writes, end to end: the bundle under the store, its layout, and the override.
//!
//! The record itself is covered in `curios/src/tests/document.rs`; this decides what the *subcommand* does with it — where the pages land, that a link from one page reaches another, and that a package without a library is refused by name.

use std::{
    fs,
    path::{Path, PathBuf},
    process::{Command, Output},
    time::{SystemTime, UNIX_EPOCH},
};

/// A directory of its own, shared with no other test.
fn temporary(name: &str) -> PathBuf {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();

    std::env::temp_dir().join(format!(
        "curios-cli-document-{name}-{}-{millis}",
        std::process::id()
    ))
}

fn write(root: &Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}

/// A package with a described library of two modules, the child documented on its `mod`.
fn project(name: &str) -> PathBuf {
    let root = temporary(name);

    write(
        &root,
        "curios.toml",
        "name = \"shapes\"\ndescription = \"Shapes & their areas.\"\n",
    );
    write(
        &root,
        "lib.crs",
        concat!(
            "use /std/{Nat};\n\n",
            "-- | Points.\npub mod geometry;\n\n",
            "-- | A shape.\npub induct Shape: pub Type\n| circle(Nat)\nend\n\n",
            "pub let area(s: Shape) -> Nat =\n    match s | circle(r) => r * r end;\n",
        ),
    );
    write(
        &root,
        "geometry.crs",
        "use /std/{Nat};\nuse /shapes/{Shape};\n\npub let unit: Shape =\n    Shape/circle(1);\n",
    );

    root
}

/// Run the compiler in `root`, with the arguments given.
fn curios(root: &Path, arguments: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_curios"))
        .current_dir(root)
        .args(arguments)
        .output()
        .expect("run the compiler")
}

#[test]
fn document_writes_the_bundle_under_the_store() {
    let root = project("store");

    let output = curios(&root, &["document"]);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(output.stdout.is_empty(), "success prints nothing");

    let bundle = root.join(".curios/documentation/shapes");
    let landing = fs::read_to_string(bundle.join("index.html")).expect("a landing page");
    assert!(landing.contains("<h1>shapes</h1>"), "{landing}");
    assert!(
        landing.contains("Shapes &amp; their areas."),
        "the manifest's description, escaped: {landing}"
    );
    assert!(landing.contains("href=\"lib.html\""), "{landing}");
    assert!(landing.contains("href=\"geometry.html\""), "{landing}");

    let library = fs::read_to_string(bundle.join("lib.html")).expect("the root module's page");
    assert!(library.contains("<li id=\"Shape\">"), "{library}");
    assert!(library.contains("<li id=\"Shape/circle\">"), "{library}");
    assert!(library.contains("<p>A shape.</p>"), "{library}");
    assert!(
        library.contains("<a href=\"lib.html#Shape\">Shape</a>"),
        "a signature links a name declared in the unit: {library}"
    );
    assert!(
        library.contains("-&gt; Nat<"),
        "a name outside the unit is plain text: {library}"
    );

    let geometry =
        fs::read_to_string(bundle.join("geometry.html")).expect("the child module's page");
    assert!(geometry.contains("<p>Points.</p>"), "{geometry}");
    assert!(
        geometry.contains("<a href=\"lib.html#Shape\">Shape</a>"),
        "a link reaches the root's page from a sibling: {geometry}"
    );
    assert!(bundle.join("static/style.css").is_file());

    fs::remove_dir_all(root).unwrap();
}

#[test]
fn output_names_another_directory() {
    let root = project("output");
    let site = root.join("site");

    let output = curios(&root, &["document", "-o", "site"]);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(site.join("index.html").is_file());
    assert!(site.join("lib.html").is_file());
    assert!(
        !root.join(".curios/documentation").exists(),
        "the store holds nothing when the pages went elsewhere"
    );

    fs::remove_dir_all(root).unwrap();
}

#[test]
fn a_package_without_a_library_is_refused_by_name() {
    let root = temporary("no-library");
    write(&root, "curios.toml", "name = \"tool\"\n");
    write(&root, "exe.crs", "/std/print(\"hi\")\n");

    let output = curios(&root, &["document"]);
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("declares no library"), "{stderr}");
    assert!(!root.join(".curios/documentation").exists());

    fs::remove_dir_all(root).unwrap();
}
