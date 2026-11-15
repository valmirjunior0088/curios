//! The board's records, embedded: every file under `tickets/` and under `archive/`, each by its path below its root, written as a table per root the binary includes, so a binary carries the board of the commit it was built from and a run reads no path. A root that is absent is an empty table.

use std::{
    env, fs,
    path::{Path, PathBuf},
};

fn main() {
    let manifest = PathBuf::from(
        env::var("CARGO_MANIFEST_DIR").expect("cargo names the manifest's directory"),
    );
    let out = PathBuf::from(env::var("OUT_DIR").expect("cargo names the output directory"));
    for root in ["tickets", "archive"] {
        let directory = manifest.join(root);
        // A directory is watched whole, so a file added, edited or removed below it rebuilds the table; one that is absent is watched through the crate's own, which its making changes.
        let watched = match directory.is_dir() {
            true => &directory,
            false => &manifest,
        };
        println!("cargo::rerun-if-changed={}", watched.display());

        let mut files = Vec::new();
        if directory.is_dir() {
            walk(&directory, &directory, &mut files);
        }
        files.sort();

        let mut table = String::from("&[\n");
        for (below, path) in files {
            table.push_str(&format!("    ({below:?}, include_str!({path:?})),\n"));
        }
        table.push_str("]\n");

        fs::write(out.join(format!("{root}.rs")), table)
            .expect("the table is written to the output directory");
    }
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
