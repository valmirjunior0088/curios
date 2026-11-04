//! The board's records, embedded: every file under `tickets/`, by its path below it, written as a table the binary includes, so a binary carries the board of the commit it was built from and a run reads no path.

use std::{
    env, fs,
    path::{Path, PathBuf},
};

fn main() {
    let root = PathBuf::from(
        env::var("CARGO_MANIFEST_DIR").expect("cargo names the manifest's directory"),
    )
    .join("tickets");
    // A directory is watched whole, so a file added, edited or removed below it rebuilds the table.
    println!("cargo::rerun-if-changed={}", root.display());

    let mut files = Vec::new();
    walk(&root, &root, &mut files);
    files.sort();

    let mut table = String::from("&[\n");
    for (below, path) in files {
        table.push_str(&format!("    ({below:?}, include_str!({path:?})),\n"));
    }
    table.push_str("]\n");

    let written = PathBuf::from(env::var("OUT_DIR").expect("cargo names the output directory"))
        .join("tickets.rs");
    fs::write(&written, table).expect("the table is written to the output directory");
}

/// Every file below `directory`, with its path below `root` joined by `/`, which is how the loader reads one.
fn walk(root: &Path, directory: &Path, files: &mut Vec<(String, String)>) {
    for entry in fs::read_dir(directory).expect("a directory of the board is readable") {
        let path = entry.expect("an entry of the board is readable").path();
        if path.is_dir() {
            walk(root, &path, files);
            continue;
        }
        let below = path
            .strip_prefix(root)
            .expect("a file found below the root is below it")
            .components()
            .map(|component| {
                component
                    .as_os_str()
                    .to_str()
                    .expect("a board's paths are UTF-8")
            })
            .collect::<Vec<_>>()
            .join("/");
        let path = path
            .to_str()
            .expect("a board's paths are UTF-8")
            .to_string();

        files.push((below, path));
    }
}
