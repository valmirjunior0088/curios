//! A scope of two packages to ask about, the second declaring the first: the fixture more than one suite here builds on.

use {
    curios_common::{fill, test_support::*},
    std::{fs, path::Path},
};

/// Two packages in a directory of their own that goes away with the test, the second declaring the first — which is what makes them a scope of two, in that order.
pub fn mounted_project(name: &str) -> Temporary {
    let root = Temporary::new("wonder", name);

    for (directory, package, declares) in [("a", "alpha", ""), ("b", "beta", "alpha")] {
        let dependency = match declares.is_empty() {
            true => String::new(),
            false => {
                fill!(
                    "\n[dependencies]\n${declares} = { source = \"path\", path = \"../a\" }\n",
                    declares
                )
            }
        };
        write(
            &root,
            &format!("{directory}/curios.toml"),
            &format!(
                "compiler = \"{compiler}\"\nname = \"{package}\"\n{dependency}",
                compiler = env!("CARGO_PKG_VERSION")
            ),
        );
        write(
            &root,
            &format!("{directory}/lib.crs"),
            &fill!(
                "use /std/{Str};\n\npub let said: Str =\n    \"${package}\";\n",
                package
            ),
        );
    }

    root
}

/// Both packages, in the order they are compiled in — resolved from `beta`'s declared dependency on `alpha` rather than listed, which is the only way a scope is assembled.
pub fn mounted(root: &Path) -> Vec<curios_module::RootSource> {
    let governing =
        curios_package::Governing::of(&root.join("b")).expect("a governed second package");

    curios_package::order(&governing).expect("two resolvable units")
}

pub fn write(root: &Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}
