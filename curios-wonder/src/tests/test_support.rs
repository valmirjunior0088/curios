//! A scope of two packages to ask about, the second declaring the first: the fixture more than one suite here builds on.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use {
    curios_utilities::test_support::Temporary,
    std::{fs, path::Path},
};

/// Two packages in a directory of their own that goes away with the test, the second declaring the first — which is what makes them a scope of two, in that order.
pub(super) fn mounted_project(name: &str) -> Temporary {
    let root = Temporary::new("wonder", name);

    for (directory, package, declares) in [("a", "alpha", ""), ("b", "beta", "alpha")] {
        let dependency = match declares.is_empty() {
            true => String::new(),
            false => {
                format!("\n[dependencies]\n{declares} = {{ source = \"path\", path = \"../a\" }}\n")
            }
        };
        write(
            &root,
            &format!("{directory}/curios.toml"),
            &format!("name = \"{package}\"\n{dependency}"),
        );
        write(
            &root,
            &format!("{directory}/lib.crs"),
            &format!("use /std/{{Str}};\n\npub let said: Str =\n    \"{package}\";\n"),
        );
    }

    root
}

/// Both packages, in the order they are compiled in — resolved from `beta`'s declared dependency on `alpha` rather than listed, which is the only way a scope is assembled now.
pub(super) fn mounted(root: &Path) -> Vec<curios_text::RootSource> {
    let governing =
        curios_package::Governing::of(&root.join("b")).expect("a governed second package");

    curios_package::order(&governing).expect("two resolvable units")
}

pub(super) fn write(root: &Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}
