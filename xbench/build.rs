//! Embeds every record under `records/<measurement>/` in the binary, so it carries the records of the commit it was built at and reads no path where it runs.
//!
//! A record is filed under its number, the next after the last of its measurement's: a file named otherwise, or a gap in the numbering, fails the build, since the report numbers a record by its place.

use std::{
    env, fs,
    path::{Path, PathBuf},
};

/// The names of the entries of `directory`, in order.
fn names(directory: &Path) -> Vec<String> {
    let mut names = fs::read_dir(directory)
        .unwrap_or_else(|error| panic!("{} cannot be read: {error}", directory.display()))
        .map(|entry| {
            entry
                .expect("a record's entry is readable")
                .file_name()
                .into_string()
                .expect("a record's name is UTF-8")
        })
        .collect::<Vec<_>>();
    names.sort();
    names
}

fn main() {
    let root = PathBuf::from(
        env::var_os("CARGO_MANIFEST_DIR").expect("cargo names the manifest's directory"),
    );
    let records = root.join("records");
    println!("cargo:rerun-if-changed={}", records.display());

    let mut filed = String::from(
        "/// Every record under `xbench/records/`, by its measurement, each measurement's oldest first, as the build found them.\nconst FILED: &[(&str, &[&str])] = &[\n",
    );
    for measurement in names(&records) {
        let directory = records.join(&measurement);
        println!("cargo:rerun-if-changed={}", directory.display());
        filed.push_str(&format!("    ({measurement:?}, &[\n"));
        for (number, name) in names(&directory).iter().enumerate() {
            assert_eq!(
                *name,
                format!("{number:02}.toml"),
                "a record is filed under its number, the next after the last of {measurement}'s"
            );
            filed.push_str(&format!(
                "        include_str!({:?}),\n",
                directory.join(name).display().to_string()
            ));
        }
        filed.push_str("    ]),\n");
    }
    filed.push_str("];\n");

    let out = PathBuf::from(env::var_os("OUT_DIR").expect("cargo names the output directory"));
    fs::write(out.join("filed.rs"), filed).expect("the embedded records are written");
}
