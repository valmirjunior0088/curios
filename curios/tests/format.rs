//! What `curios format` does at the command line: which files a target names — every file a package declares for none, a file itself, standard input for `-` — and that `--check` writes nothing.

use {
    curios_utilities::test_support::Temporary,
    std::{
        fs,
        io::Write,
        path::Path,
        process::{Command, Output, Stdio},
    },
};

/// A directory of its own, shared with no other test and gone with it.
fn temporary(name: &str) -> Temporary {
    Temporary::new("cli-format", name)
}

fn write(root: &Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}

/// Run the compiler in `root`, with the arguments given and `stdin` on its standard input.
fn curios(root: &Path, arguments: &[&str], stdin: &str) -> Output {
    let mut child = Command::new(env!("CARGO_BIN_EXE_curios"))
        .current_dir(root)
        .args(arguments)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("run the compiler");
    child
        .stdin
        .take()
        .unwrap()
        .write_all(stdin.as_bytes())
        .unwrap();
    child.wait_with_output().expect("the compiler exits")
}

/// A declaration the formatter moves: indented where the canonical form puts it at the margin.
const UNFORMATTED: &str = "    pub let word: /std/Str = \"formatted\";\n";

/// No target is every file the governing package declares, so a file beside them that nothing declares is left as it is — and `--check` names what would change without writing it.
#[test]
fn no_target_formats_what_the_package_declares_and_nothing_else() {
    let root = temporary("package");
    write(&root, "curios.toml", "name = \"app\"\n");
    write(&root, "lib.crs", "pub mod util;\n");
    write(&root, "util.crs", UNFORMATTED);
    write(&root, "stray.crs", UNFORMATTED);

    let checked = curios(&root, &["format", "--check"], "");
    let stderr = String::from_utf8_lossy(&checked.stderr);
    assert!(!checked.status.success(), "{stderr}");
    assert!(stderr.contains("util.crs"), "{stderr}");
    assert!(!stderr.contains("stray.crs"), "{stderr}");
    assert_eq!(
        fs::read_to_string(root.join("util.crs")).unwrap(),
        UNFORMATTED,
        "--check writes nothing"
    );

    let formatted = curios(&root, &["format"], "");
    assert!(
        formatted.status.success(),
        "{}",
        String::from_utf8_lossy(&formatted.stderr)
    );
    assert_ne!(
        fs::read_to_string(root.join("util.crs")).unwrap(),
        UNFORMATTED
    );
    assert_eq!(
        fs::read_to_string(root.join("stray.crs")).unwrap(),
        UNFORMATTED,
        "a file nothing declares is formatted only when named"
    );
}

/// `-` formats standard input onto standard output, exactly as the file form rewrites the same text.
#[test]
fn a_dash_formats_standard_input_onto_standard_output() {
    let root = temporary("stdin");
    write(&root, "scratch.crs", UNFORMATTED);

    let piped = curios(&root, &["format", "-"], UNFORMATTED);
    assert!(
        piped.status.success(),
        "{}",
        String::from_utf8_lossy(&piped.stderr)
    );

    let rewritten = curios(&root, &["format", "scratch.crs"], "");
    assert!(
        rewritten.status.success(),
        "{}",
        String::from_utf8_lossy(&rewritten.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&piped.stdout),
        fs::read_to_string(root.join("scratch.crs")).unwrap()
    );
}
