//! Embeds every reading under `readings/` in the binary, so it carries the readings of the commit it was built at and reads no path where it runs.
//!
//! A reading is filed under its number, the next after the last: a file named otherwise, or a gap in the numbering, fails the build, since the report numbers a reading by its place.

use std::{env, fs, path::PathBuf};

fn main() {
    let root = PathBuf::from(
        env::var_os("CARGO_MANIFEST_DIR").expect("cargo names the manifest's directory"),
    );
    let readings = root.join("readings");
    println!("cargo:rerun-if-changed={}", readings.display());

    let mut names = fs::read_dir(&readings)
        .expect("`xbench/readings/` holds the readings")
        .map(|entry| {
            entry
                .expect("a reading's entry is readable")
                .file_name()
                .into_string()
                .expect("a reading's name is UTF-8")
        })
        .collect::<Vec<_>>();
    names.sort();

    let mut filed = String::from(
        "/// Every reading under `xbench/readings/`, oldest first, as the build found it.\nconst FILED: &[&str] = &[\n",
    );
    for (number, name) in names.iter().enumerate() {
        assert_eq!(
            *name,
            format!("{number:02}.toml"),
            "a reading is filed under its number, the next after the last"
        );
        filed.push_str(&format!(
            "    include_str!({:?}),\n",
            readings.join(name).display().to_string()
        ));
    }
    filed.push_str("];\n");

    let out = PathBuf::from(env::var_os("OUT_DIR").expect("cargo names the output directory"));
    fs::write(out.join("filed.rs"), filed).expect("the embedded readings are written");
}
