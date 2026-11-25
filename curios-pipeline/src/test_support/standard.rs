//! The standard library as its own package, and what its suites and the bench's measurements of it compile it over: the edits measured, and a module's declarations.

use {
    curios_common::{Qualifier, RootKind},
    curios_core::{Definitions, Global},
    curios_module::RootSource,
    curios_prelude_build::std_directory,
    std::collections::BTreeSet,
};

/// The standard library's own tree, as the package claiming `/std` from it.
pub fn std_from_its_tree() -> RootSource {
    let directory = std_directory();

    RootSource::mounted(
        "std",
        RootKind::Ordinary,
        directory.join("lib.crs"),
        directory,
    )
    .declaring([Qualifier::from(["sys"])])
}

/// One edit the census measures: what it is, the file it touches, and the text it leaves there.
pub struct Edit {
    pub label: &'static str,
    pub file: &'static str,
    pub edit: fn(&str) -> String,
}

/// The two edits the standard library is measured over: `leaf`, a declaration added to `/std/Nat`, which nothing reads, and `hub`, the body of `/std/Bool/not` respelled, which most of the library reaches.
pub fn edits() -> [Edit; 2] {
    [
        Edit {
            label: "leaf",
            file: "Nat.crs",
            edit: |text| format!("{text}\npub let _census_probe(a: Nat) -> Nat =\n    a;\n"),
        },
        Edit {
            label: "hub",
            file: "Bool.crs",
            edit: |text| text.replacen("xor(b, true)", "xor(true, b)", 1),
        },
    ]
}

/// The names of each declaration of `module`, in item order: each item's, a family's constructors among its own.
pub fn declarations(definitions: &Definitions) -> Vec<BTreeSet<Global>> {
    definitions
        .items
        .iter()
        .map(|item| item.declared_names().into_iter().collect())
        .collect()
}
