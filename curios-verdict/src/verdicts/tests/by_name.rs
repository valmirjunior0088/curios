//! A unit a store filed is reused only where every name it read means what it meant: read of a package of one name in another project, or of a package named `std` standing in for the archived root.

use super::*;

/// A manifest for the package `name`, depending on the package beside it named `below` where there is one, as `chained` writes them.
fn beside(name: &str, below: Option<&str>) -> String {
    let dependency = below.map_or(String::new(), |below| {
        fill!(
            "\n[dependencies]\n${below} = { source = \"path\", path = \"../${below}\" }\n",
            below
        )
    });

    fill!(
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"${name}\"\n${dependency}"
        ),
        name,
        dependency
    )
}

/// What a build of `root`'s package `package` through the store at `root` was refused with, or nothing.
fn built_library(root: &Path, package: &str) -> Result<(), String> {
    let verdicts = Verdicts::at(root.to_path_buf());
    let governing = Governing::of(&root.join(package)).expect("a governed package");
    let library = order(&governing).expect("a resolvable library");

    Fold::new(1_000_000, &library, Some(&verdicts))
        .check_units(|_| {})
        .map_err(String::from)
}

/// A package named `std`, first in a fold, takes the archived `/std`'s place, so a unit compiled after it read a `/std` that is not the compiler's own. `p` proves `NonZero(0)` while `q` depends on such a package, under which `NonZero` holds of every number; once `q` depends on nothing, `/std` is the archived root, where `NonZero(0)` is `Bool/Holds(0 != 0)`, and `p`'s slot is the one it was filed in. A build over the store answers what a build with nothing stored answers: `d` is refused.
#[test]
fn a_unit_compiled_after_the_package_std_is_judged_again_over_the_archived_std() {
    let project = |name: &str, under: Option<&str>| {
        let root = temporary(name);
        write(&root, "std/curios.toml", &beside("std", None));
        write(&root, "std/lib.crs", "pub mod Nat;\n\npub mod Bool;\n");
        write(
            &root,
            "std/Nat.crs",
            "pub use /sys/{let Nat};\n\npub let NonZero(n: Nat) -> Prop =\n    /sys/Bool/True;\n",
        );
        write(&root, "std/Bool.crs", "pub use /sys/Bool/{True, False};\n");
        write(&root, "q/curios.toml", &beside("q", under));
        write(&root, "q/lib.crs", "pub induct Q: pub Type\n| q()\nend\n");
        write(&root, "p/curios.toml", &beside("p", Some("q")));
        write(
            &root,
            "p/lib.crs",
            "use /std/Nat/{NonZero};\nuse /std/Bool/{True};\n\npub let d: NonZero(0) =\n    True/qed();\n",
        );

        root
    };
    let root = project("std-package", Some("std"));
    built_library(&root, "p").expect("`NonZero(0)` holds where `/std` is the package `std`");

    write(&root, "q/curios.toml", &beside("q", None));
    // Each refusal names the file it stands in, under a directory of its own.
    let refused = |root: &Path| {
        built_library(root, "p")
            .expect_err("`NonZero(0)` is `False` over the archived `/std`")
            .replace(&root.display().to_string(), "<root>")
    };
    let fresh = project("std-package-fresh", None);
    assert_eq!(
        refused(&root),
        refused(&fresh),
        "a build over the store answers what a build with nothing stored answers"
    );
}
