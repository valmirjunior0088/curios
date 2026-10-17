//! Required minima, umbrella ownership, and dependency checks before compilation.

use {
    super::*,
    crate::{Governing, Manifest, Selection, Spelling, order},
    curios_text::Overlay,
    curios_utilities::{fill, test_support::Temporary},
    std::fs,
};

fn write(root: &Path, path: &str, source: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, source).unwrap();
}

fn future() -> CompilerVersion {
    let mut version = CompilerVersion::current();
    version.0.major += 1;
    version
}

#[test]
fn a_compiler_minimum_is_a_version_and_never_a_range() {
    for spelling in ["", "1.2", ">=1.2.3", "^1.2.3", "1.2.3, <2.0.0"] {
        let error = spelling.parse::<CompilerVersion>().unwrap_err();
        assert!(error.contains("bare SemVer version"), "{error}");
    }
    assert_eq!(
        "1.2.3".parse::<CompilerVersion>().unwrap().to_string(),
        "1.2.3"
    );
}

#[test]
fn a_standalone_package_requires_its_own_minimum() {
    let root = Temporary::new("compatibility", "standalone");
    write(&root, MANIFEST, "name = \"app\"\n");
    let error = Governing::of(&root).err().expect("a minimum is required");
    assert!(
        error.contains("standalone package must state `compiler"),
        "{error}"
    );
    assert!(
        error.contains(&root.join(MANIFEST).display().to_string()),
        "{error}"
    );
}

#[test]
fn an_umbrella_requires_the_minimum_its_members_inherit() {
    let error = "members = []".parse::<Manifest>().unwrap_err();
    assert!(error.contains("an umbrella must declare"), "{error}");
}

#[test]
fn current_and_older_minima_accept_the_running_compiler() {
    let root = Temporary::new("compatibility", "supported");
    for version in [
        CompilerVersion::current().to_string(),
        "0.0.0".to_string(),
        format!("{}+build", CompilerVersion::current()),
    ] {
        write(
            &root,
            MANIFEST,
            &format!("name = \"app\"\ncompiler = \"{version}\"\n"),
        );
        let governing = Governing::of(&root).expect("the declaration is valid");
        order(&governing)
            .unwrap_or_else(|error| panic!("the installed compiler meets the minimum: {error}"));
    }
}

#[test]
fn a_newer_minimum_names_the_manifest_and_both_versions() {
    let root = Temporary::new("compatibility", "newer");
    let required = future();
    write(
        &root,
        MANIFEST,
        &format!("name = \"app\"\ncompiler = \"{required}\"\n"),
    );
    let governing = Governing::of(&root).expect("the manifest is well formed");
    let error = order(&governing).err().expect("this compiler is too old");
    assert!(
        error.contains(&root.join(MANIFEST).display().to_string()),
        "{error}"
    );
    assert!(
        error.contains(&format!("requires Curios {required} or newer")),
        "{error}"
    );
    assert!(
        error.contains(&format!(
            "this compiler is Curios {}",
            CompilerVersion::current()
        )),
        "{error}"
    );
}

#[test]
fn only_enumerated_members_inherit_the_umbrellas_minimum() {
    let root = Temporary::new("compatibility", "membership");
    write(
        &root,
        MANIFEST,
        &format!(
            "compiler = \"{}\"\nmembers = [\"member\"]\n",
            CompilerVersion::current()
        ),
    );
    write(&root, "member/curios.toml", "name = \"member\"\n");
    write(&root, "stray/curios.toml", "name = \"stray\"\n");
    Governing::of(&root.join("member")).expect("the enumerated member inherits");
    let error = Governing::of(&root.join("stray"))
        .err()
        .expect("an unlisted package stands alone");
    assert!(
        error.contains("standalone package must state `compiler"),
        "{error}"
    );
    write(
        &root,
        "member/curios.toml",
        "name = \"member\"\ncompiler = \"0.0.0\"\n",
    );
    let error = Governing::of(&root.join("member"))
        .err()
        .expect("a member cannot override its umbrella");
    assert!(error.contains("inherits `compiler`"), "{error}");
    assert!(error.contains("remove its local `compiler`"), "{error}");
}

#[test]
fn an_inherited_newer_minimum_is_reported_against_the_umbrella() {
    let root = Temporary::new("compatibility", "inherited-newer");
    write(
        &root,
        MANIFEST,
        &format!("compiler = \"{}\"\nmembers = [\"member\"]\n", future()),
    );
    write(&root, "member/curios.toml", "name = \"member\"\n");
    let governing = Governing::of(&root.join("member")).expect("the manifest is well formed");
    let error = order(&governing)
        .err()
        .expect("the inherited minimum is too new");
    assert!(
        error.contains(&root.join(MANIFEST).display().to_string()),
        "{error}"
    );
    assert!(error.contains("requires Curios"), "{error}");
}

#[test]
fn dependencies_use_their_own_umbrella_and_never_the_consumers() {
    let root = Temporary::new("compatibility", "dependency");
    write(
        &root,
        "app/curios.toml",
        &fill!(
            "name = \"app\"\ncompiler = \"${compiler}\"\n[dependencies]\nbase = { source = \"path\", path = \"../vendor/base\" }\n",
            compiler = CompilerVersion::current()
        ),
    );
    write(
        &root,
        "vendor/curios.toml",
        &format!("compiler = \"{}\"\nmembers = [\"base\"]\n", future()),
    );
    write(&root, "vendor/base/curios.toml", "name = \"base\"\n");
    write(&root, "vendor/base/lib.crs", "");
    let governing = Governing::of(&root.join("app")).expect("the root package is supported");
    let error = order(&governing)
        .err()
        .expect("the dependency requires a newer compiler");
    assert!(
        error.contains(&root.join("vendor/curios.toml").display().to_string()),
        "{error}"
    );
    assert!(error.contains("requires Curios"), "{error}");
    fs::remove_file(root.join("vendor/curios.toml")).unwrap();
    let error = order(&governing)
        .err()
        .expect("the dependency has no standalone minimum");
    assert!(
        error.contains("standalone package must state `compiler"),
        "{error}"
    );
}

#[test]
fn a_loose_file_does_not_use_the_surrounding_packages_compiler_minimum() {
    let root = Temporary::new("compatibility", "loose");
    write(
        &root,
        MANIFEST,
        &format!("name = \"app\"\ncompiler = \"{}\"\n", future()),
    );
    write(&root, "scratch.crs", "()");
    for library in [false, true] {
        if library {
            write(&root, "lib.crs", "");
        }
        let selected = Selection::of(
            Spelling::File(root.join("scratch.crs")),
            None,
            &root,
            &Overlay::default(),
        )
        .expect("the file is in none of the package's units");
        let Selection::Program(program) = selected else {
            panic!("an unlinked file is a loose program");
        };
        assert!(program.into_units().is_empty());
    }
    write(&root, "lib.crs", "mod scratch;");
    let error = Selection::of(
        Spelling::File(root.join("scratch.crs")),
        None,
        &root,
        &Overlay::default(),
    )
    .err()
    .expect("a declared file uses its package's minimum");
    assert!(error.contains("requires Curios"), "{error}");
}
