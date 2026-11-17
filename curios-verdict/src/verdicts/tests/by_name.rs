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

/// Two projects each hold a package `base` and a package `top` of one text, `top` proving `Holds(t)` of `base`'s `t`. The slot `top` filed in the project whose `t` is `true` is the other project's baseline, a record naming each file by its package: its read of `t` is asked again of this project's `base`, whose `t` is `false`, and a build over the shared store answers what a build with nothing stored answers.
#[test]
fn a_proof_another_project_filed_over_a_package_of_one_name_is_judged_again_over_this_ones() {
    let twin = |name: &str, t: &str| {
        let root = temporary(name);
        write(&root, "base/curios.toml", &beside("base", None));
        write(
            &root,
            "base/lib.crs",
            &fill!("use /std/{Bool};\n\npub let t: Bool =\n    ${t};\n", t),
        );
        write(&root, "top/curios.toml", &beside("top", Some("base")));
        write(
            &root,
            "top/lib.crs",
            "use /std/Bool/{True, Holds};\nuse /base/{t};\n\npub let d: Holds(t) =\n    True/qed();\n",
        );

        root
    };

    let theirs = twin("twin-theirs", "true");
    built_library(&theirs, "top").expect("`Holds(t)` holds where `t` is `true`");
    let mine = twin("twin-mine", "false");
    stage(&theirs, &mine);

    // Each refusal names the file it stands in, under a directory of its own.
    let refused = |root: &Path| {
        built_library(root, "top")
            .expect_err("`Holds(t)` is `False` where `t` is `false`")
            .replace(&root.display().to_string(), "<root>")
    };
    let fresh = twin("twin-fresh", "false");
    assert_eq!(
        refused(&mine),
        refused(&fresh),
        "a build over the shared store answers what a build with nothing stored answers"
    );
}

/// A verdict a store filed is found by the item it judged, its reads digested again where it is taken. `d` proves `NonZero(0)` while `/std` is the package `std`, where `NonZero` holds of every number, and its first filing completes its record with what it read of that package; `p` is then filed without `d`, so its slot holds no `d` to adopt; once `/std` is the archived root, where `NonZero(0)` is `Bool/Holds(0 != 0)`, `d` written again is judged and not taken from that record, and is refused as a build with nothing stored refuses it.
#[test]
fn a_verdict_filed_over_the_package_std_is_not_taken_over_the_archived_std() {
    const PROVED: &str =
        "use /std/Nat/{NonZero};\nuse /std/Bool/{True};\n\npub let d: NonZero(0) =\n    True/qed();\n";
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
        write(&root, "p/lib.crs", PROVED);

        root
    };

    let root = project("std-package-record", Some("std"));
    built_library(&root, "p").expect("`NonZero(0)` holds where `/std` is the package `std`");
    write(&root, "p/lib.crs", "pub induct P: pub Type\n| p()\nend\n");
    built_library(&root, "p").expect("`p` without `d`");

    write(&root, "q/curios.toml", &beside("q", None));
    write(&root, "p/lib.crs", PROVED);
    // Each refusal names the file it stands in, under a directory of its own.
    let refused = |root: &Path| {
        built_library(root, "p")
            .expect_err("`NonZero(0)` is `False` over the archived `/std`")
            .replace(&root.display().to_string(), "<root>")
    };
    let fresh = project("std-package-record-fresh", None);
    assert_eq!(
        refused(&root),
        refused(&fresh),
        "a build over the store answers what a build with nothing stored answers"
    );
}
