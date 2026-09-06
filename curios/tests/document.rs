//! What the `document` subcommand writes, end to end: the bundle under the store, its layout, and the override.
//!
//! The record itself is covered in `curios/src/tests/document.rs`; this decides what the *subcommand* does with it — where the pages land, that a link from one page reaches another, that the prelude image documents the standard library through the file form, and that a package without a library is refused by name.

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
    // The landing page is the root module's page: the description, the module cards, then the root's own declarations.
    let landing = fs::read_to_string(bundle.join("index.html")).expect("a landing page");
    assert!(landing.contains("<h1>/shapes</h1>"), "{landing}");
    assert!(
        landing.contains("Shapes &#38; their areas."),
        "the manifest's description, escaped: {landing}"
    );
    assert!(landing.contains("href=\"geometry.crs.html\""), "{landing}");
    assert!(landing.contains("id=\"Shape\""), "{landing}");
    assert!(landing.contains("id=\"Shape/circle\""), "{landing}");
    assert!(
        landing.contains("<p class=\"prose\">A shape.</p>"),
        "{landing}"
    );
    assert!(
        landing.contains("<a href=\"index.html#Shape\">Shape</a>"),
        "a signature links a name declared in the unit: {landing}"
    );
    assert!(
        landing.contains("<span class=\"name\">Nat</span>"),
        "a name outside the unit is a name, not a link: {landing}"
    );

    let geometry =
        fs::read_to_string(bundle.join("geometry.crs.html")).expect("the child module's page");
    assert!(
        geometry.contains("<p class=\"lead\">Points.</p>"),
        "{geometry}"
    );
    assert!(
        geometry.contains("<a href=\"index.html#Shape\">Shape</a>"),
        "a link reaches the root's page from a sibling: {geometry}"
    );
    assert!(bundle.join("static/style.css").is_file());
    assert!(bundle.join("static/fonts/geist.woff2").is_file());
    let index = fs::read_to_string(bundle.join("static/index.js")).expect("the search index");
    assert!(
        index.contains("\"/shapes/Shape/circle\",\"index.html#Shape/circle\"]"),
        "a member is indexed at its anchor: {index}"
    );

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
    assert!(site.join("geometry.crs.html").is_file());
    assert!(
        !root.join(".curios/documentation").exists(),
        "the store holds nothing when the pages went elsewhere"
    );

    fs::remove_dir_all(root).unwrap();
}

/// The image the compiler was built with, where its build script filed it: the one `.rkyv` every checkout that built `curios` has.
const IMAGE: &str = concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../curios-prelude-archive/.artifacts/archive.rkyv"
);

#[test]
fn the_prelude_image_documents_the_standard_library_into_output() {
    let root = temporary("image");
    fs::create_dir_all(&root).unwrap();

    let output = curios(&root, &["document", IMAGE, "-o", "site"]);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let site = root.join("site");
    let landing = fs::read_to_string(site.join("index.html")).expect("a landing page");
    assert!(landing.contains("<h1>/std</h1>"), "{landing}");
    assert!(
        landing.contains("The standard library"),
        "the image's description: {landing}"
    );
    assert!(landing.contains("href=\"Result.crs.html\""), "{landing}");
    let result = fs::read_to_string(site.join("Result.crs.html")).expect("a module's page");
    assert!(result.contains("id=\"Result/success\""), "{result}");
    // A nested module's page climbs back to the bundle's static files and the landing page.
    let signal = fs::read_to_string(site.join("Async/Signal.crs.html")).expect("a nested page");
    assert!(
        signal.contains("href=\"../static/style.css\"")
            && signal.contains("src=\"../static/index.js\"")
            && signal.contains("data-root=\"../\"")
            && signal.contains("href=\"../index.html\""),
        "{signal}"
    );
    assert!(
        !root.join(".curios").exists(),
        "a file has no package, so nothing is filed under a store"
    );

    fs::remove_dir_all(root).unwrap();
}

/// A verdict slot frames a record ahead of its unit, and `document` reads the unit off it as it reads the image, so a library filed under a store documents without compiling again. `test` is what files it: `document` itself reads the store as every query does and never writes it.
#[test]
fn a_verdict_slot_documents_the_unit_it_holds() {
    let root = project("slot");

    let output = curios(&root, &["test"]);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let slot = fs::read_dir(root.join(".curios/verdicts"))
        .expect("a store with the library's unit in it")
        .map(|slot| slot.unwrap().path())
        .next()
        .expect("the library's slot");

    let output = curios(&root, &["document", slot.to_str().unwrap(), "-o", "site"]);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let landing = fs::read_to_string(root.join("site/index.html")).expect("a landing page");
    assert!(landing.contains("<h1>/shapes</h1>"), "{landing}");

    fs::remove_dir_all(root).unwrap();
}

#[test]
fn a_file_without_output_is_refused_before_it_is_read() {
    let root = temporary("image-no-output");
    fs::create_dir_all(&root).unwrap();

    let output = curios(&root, &["document", IMAGE]);
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("--output"), "{stderr}");

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
