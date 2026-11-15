//! Embeds every timing under `timings/` in the binary, so it carries the timings of the commit it was built at and reads no path where it runs.
//!
//! A timing is filed under its number, the next after the last: a file named otherwise, or a gap in the numbering, fails the build, since the report numbers a timing by its place.

use std::{env, fs, path::PathBuf};

fn main() {
    let root = PathBuf::from(
        env::var_os("CARGO_MANIFEST_DIR").expect("cargo names the manifest's directory"),
    );
    let timings = root.join("timings");
    println!("cargo:rerun-if-changed={}", timings.display());

    let mut names = fs::read_dir(&timings)
        .expect("`xbench/timings/` holds the timings")
        .map(|entry| {
            entry
                .expect("a timing's entry is readable")
                .file_name()
                .into_string()
                .expect("a timing's name is UTF-8")
        })
        .collect::<Vec<_>>();
    names.sort();

    let mut filed = String::from(
        "/// Every timing under `xbench/timings/`, oldest first, as the build found it.\nconst FILED: &[&str] = &[\n",
    );
    for (number, name) in names.iter().enumerate() {
        assert_eq!(
            *name,
            format!("{number:02}.toml"),
            "a timing is filed under its number, the next after the last"
        );
        filed.push_str(&format!(
            "    include_str!({:?}),\n",
            timings.join(name).display().to_string()
        ));
    }
    filed.push_str("];\n");

    let out = PathBuf::from(env::var_os("OUT_DIR").expect("cargo names the output directory"));
    fs::write(out.join("filed.rs"), filed).expect("the embedded timings are written");
}
