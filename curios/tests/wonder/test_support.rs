//! What the transport suites share: a temporary project on disk and the `curios` process run in it.

use {
    curios_utilities::test_support::Temporary,
    std::{
        fs,
        io::Write,
        path::Path,
        process::{Command, Output, Stdio},
    },
};

/// A directory of its own, shared with no other test and gone with it — canonical, which matters here because the server publishes the root the governance walk canonicalizes, and a URI computed from any other spelling would never match the one published.
pub fn temporary(name: &str) -> Temporary {
    Temporary::new("cli-wonder", name)
}

pub fn write(root: &Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}

/// A package whose library spreads over two files and whose executable uses it.
pub fn project(name: &str) -> Temporary {
    let root = temporary(name);
    write(
        &root,
        "curios.toml",
        "compiler = \"0.0.0\"\nname = \"app\"\n\n[[executables]]\nname = \"app\"\n",
    );
    write(&root, "lib.crs", "pub mod util;\n");
    write(&root, "util.crs", "pub let word : /std/Str = \"placed\";\n");
    write(&root, "app.crs", "/std/print(/app/util/word)\n");
    root
}

/// Run the compiler in `root`, with the arguments given and `stdin` on its standard input.
pub fn curios(root: &Path, arguments: &[&str], stdin: &str) -> Output {
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

pub fn stdout(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}
