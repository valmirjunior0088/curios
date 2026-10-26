//! What counts as a release version, which version a bump word names, and what a bump is allowed to have changed.
//!
//! The dance itself is not here: exercising it would cut a release. What is testable without one is every judgment it makes before it writes, and the judgment it makes about what it wrote.

use super::*;

/// The shape `git diff` gives Cargo's part of a bump, trimmed to one member: the two file headers, a hunk header, context lines, and the version pair.
const BUMP: &str = "\
diff --git a/Cargo.lock b/Cargo.lock
index ecf729fc..6eabedfc 100644
--- a/Cargo.lock
+++ b/Cargo.lock
@@ -555,7 +555,7 @@ dependencies = [

 [[package]]
 name = \"curios\"
-version = \"0.14.5\"
+version = \"0.14.6\"
 dependencies = [
  \"clap\",
diff --git a/Cargo.toml b/Cargo.toml
index 1d112d84..ab77b38a 100644
--- a/Cargo.toml
+++ b/Cargo.toml
@@ -35,7 +35,7 @@ members = [
 exclude = [\"editors/zed\"]

 [workspace.package]
-version = \"0.14.5\"
+version = \"0.14.6\"
 edition = \"2024\"
";

const STANDARD_BUMP: &str = "\
diff --git a/curios-prelude/std/curios.toml b/curios-prelude/std/curios.toml
--- a/curios-prelude/std/curios.toml
+++ b/curios-prelude/std/curios.toml
@@ -1,3 +1,3 @@
 name = \"std\"
-compiler = \"0.14.5\"
+compiler = \"0.14.6\"
 description = \"The standard library\"
";

const NAMES: &str = "Cargo.lock\nCargo.toml\ncurios-prelude/std/curios.toml\n";

fn version(major: u64, minor: u64, patch: u64) -> Version {
    Version {
        major,
        minor,
        patch,
    }
}

#[test]
fn a_version_is_three_numbers_that_print_back_as_they_arrived() {
    assert_eq!(Version::parse("0.14.6"), Ok(version(0, 14, 6)));
    assert_eq!(Version::parse("1.0.0"), Ok(version(1, 0, 0)));
    assert_eq!(version(0, 14, 6).to_string(), "0.14.6");
}

#[test]
fn a_prerelease_a_leading_zero_and_a_missing_part_are_not_versions() {
    for refused in [
        "",
        "0.14",
        "0.14.6.1",
        "0.14.06",
        "0.14.+6",
        "0.14.6 ",
        "1.0.0-rc.1",
        "1.0.0+build.7",
        "release/0.14.6",
        "patch",
    ] {
        assert!(Version::parse(refused).is_err(), "{refused:?}");
    }
}

#[test]
fn a_major_bump_zeroes_the_minor_and_the_patch() {
    let [_, _, major] = version(0, 14, 6).bumps();

    assert_eq!(major, version(1, 0, 0));
}

#[test]
fn a_minor_bump_zeroes_the_patch() {
    let [patch, minor, _] = version(0, 14, 6).bumps();

    assert_eq!(patch, version(0, 14, 7));
    assert_eq!(minor, version(0, 15, 0));
}

#[test]
fn a_bump_word_and_a_version_above_the_current_one_both_resolve() {
    let current = version(0, 14, 6);

    assert_eq!(resolve(Some("patch"), current), Ok(version(0, 14, 7)));
    assert_eq!(resolve(Some("minor"), current), Ok(version(0, 15, 0)));
    assert_eq!(resolve(Some("major"), current), Ok(version(1, 0, 0)));
    assert_eq!(resolve(Some("2.3.4"), current), Ok(version(2, 3, 4)));
}

#[test]
fn a_target_that_is_not_above_the_current_version_is_refused() {
    let current = version(0, 14, 6);

    // A version in a range the history skipped has no tag to collide with, which is what leaves this to the comparison.
    for refused in ["0.13.11", "0.4.4", "0.14.6"] {
        assert!(resolve(Some(refused), current).is_err(), "{refused}");
    }
}

#[test]
fn no_argument_names_the_current_version_and_the_three_candidates() {
    let refusal = resolve(None, version(0, 14, 6)).unwrap_err();

    for named in ["0.14.6", "0.14.7", "0.15.0", "1.0.0"] {
        assert!(refusal.contains(named), "{named} missing from {refusal:?}");
    }
}

#[test]
fn the_diff_of_a_bump_is_accepted() {
    assert_eq!(
        verify(
            NAMES,
            BUMP,
            STANDARD_BUMP,
            version(0, 14, 5),
            version(0, 14, 6)
        ),
        Ok(())
    );
}

#[test]
fn an_unrelated_changed_file_or_a_missing_manifest_is_refused() {
    let names = format!("{NAMES}README.md\n");

    for names in [&names, "Cargo.toml\n", "Cargo.lock\nCargo.toml\n"] {
        assert!(
            verify(
                names,
                BUMP,
                STANDARD_BUMP,
                version(0, 14, 5),
                version(0, 14, 6)
            )
            .is_err()
        );
    }
}

#[test]
fn a_changed_line_that_is_not_the_version_is_refused() {
    let edition = BUMP.replace(" edition = \"2024\"", "+edition = \"2024\"");
    assert!(
        verify(
            NAMES,
            &edition,
            STANDARD_BUMP,
            version(0, 14, 5),
            version(0, 14, 6)
        )
        .is_err()
    );

    // The versions the diff carries are the ones being checked, so a diff for another bump is not this bump's.
    assert!(
        verify(
            NAMES,
            BUMP,
            STANDARD_BUMP,
            version(0, 14, 5),
            version(0, 15, 0)
        )
        .is_err()
    );
    assert!(
        verify(
            NAMES,
            BUMP,
            STANDARD_BUMP,
            version(0, 14, 4),
            version(0, 14, 6)
        )
        .is_err()
    );
}

#[test]
fn a_bump_preserves_both_manifests_beside_the_versions_it_owns() {
    let workspace = "# workspace\n[workspace.package]\nversion = \"0.14.5\"\nedition = \"2024\"\n";
    let standard = "# bundled library\nname = \"std\" # its name stays\ncompiler = \"0.14.5\"\ndescription = \"The standard library\"\n";
    let mut document = workspace.parse::<DocumentMut>().unwrap();
    let mut library = standard.parse::<DocumentMut>().unwrap();

    bump_manifests(
        &mut document,
        &mut library,
        version(0, 14, 5),
        version(0, 14, 6),
    )
    .unwrap();

    assert_eq!(document.to_string(), workspace.replace("0.14.5", "0.14.6"));
    assert_eq!(library.to_string(), standard.replace("0.14.5", "0.14.6"));
}

#[test]
fn a_missing_or_stale_library_minimum_is_refused_before_mutation() {
    let workspace = "[workspace.package]\nversion = \"0.14.5\"\n";
    for standard in [
        "name = \"std\"\n",
        "name = \"std\"\ncompiler = \"0.14.4\"\n",
    ] {
        let mut document = workspace.parse::<DocumentMut>().unwrap();
        let mut library = standard.parse::<DocumentMut>().unwrap();
        let error = bump_manifests(
            &mut document,
            &mut library,
            version(0, 14, 5),
            version(0, 14, 6),
        )
        .unwrap_err();
        assert!(error.contains(STANDARD_MANIFEST), "{error}");
        assert_eq!(document.to_string(), workspace);
        assert_eq!(library.to_string(), standard);
    }
}

#[test]
fn an_unrelated_library_change_is_refused() {
    let renamed = STANDARD_BUMP.replace(" name = \"std\"", "+name = \"other\"");
    assert!(verify(NAMES, BUMP, &renamed, version(0, 14, 5), version(0, 14, 6)).is_err());
}

#[test]
fn a_compiler_field_in_cargo_or_a_version_field_in_the_library_is_refused() {
    let cargo = BUMP.replace("version =", "compiler =");
    let standard = STANDARD_BUMP.replace("compiler =", "version =");
    for (cargo, standard) in [(&cargo[..], STANDARD_BUMP), (BUMP, &standard[..])] {
        assert!(verify(NAMES, cargo, standard, version(0, 14, 5), version(0, 14, 6)).is_err());
    }
}
