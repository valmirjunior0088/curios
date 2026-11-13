//! Stated versions, umbrella ownership, and dependency checks before compilation.

use {
    super::*,
    crate::*,
    curios_common::{fill, test_support::*},
    curios_module::Overlay,
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

fn past() -> CompilerVersion {
    "0.0.0".parse().expect("a bare version")
}

#[test]
fn a_compiler_version_is_bare_and_never_a_range() {
    for spelling in ["", "1.2", ">=1.2.3", "^1.2.3", "1.2.3, <2.0.0"] {
        let error = spelling
            .parse::<CompilerVersion>()
            .unwrap_err()
            .said
            .to_string();
        assert!(error.contains("bare SemVer version"), "{error}");
    }
    assert_eq!(
        "1.2.3".parse::<CompilerVersion>().unwrap().to_string(),
        "1.2.3"
    );
}

#[test]
fn a_standalone_package_states_its_own_compiler() {
    let root = Temporary::new("compatibility", "standalone");
    write(&root, MANIFEST, "name = \"app\"\n");
    let error = Governing::of(&root)
        .err()
        .expect("a version is required")
        .render();
    assert!(
        error.contains("a standalone package states one, as `compiler"),
        "{error}"
    );
    assert!(
        error.contains(&root.join(MANIFEST).display().to_string()),
        "{error}"
    );
}

#[test]
fn an_umbrella_states_the_compiler_its_members_inherit() {
    let error = "members = []"
        .parse::<Manifest>()
        .unwrap_err()
        .said
        .to_string();
    assert!(
        error.contains("this umbrella states no `compiler`"),
        "{error}"
    );
}

#[test]
fn the_running_compilers_version_is_accepted_whatever_its_build() {
    let root = Temporary::new("compatibility", "supported");
    for version in [
        CompilerVersion::current().to_string(),
        format!("{}+build", CompilerVersion::current()),
    ] {
        write(
            &root,
            MANIFEST,
            &format!("name = \"app\"\ncompiler = \"{version}\"\n"),
        );
        let governing = Governing::of(&root).expect("the declaration is valid");
        order(&governing).unwrap_or_else(|error| {
            panic!(
                "the installed compiler is the one stated: {}",
                error.render()
            )
        });
    }
}

#[test]
fn another_version_newer_or_older_names_the_manifest_and_both_versions() {
    let root = Temporary::new("compatibility", "other");
    for required in [future(), past()] {
        write(
            &root,
            MANIFEST,
            &format!("name = \"app\"\ncompiler = \"{required}\"\n"),
        );
        let governing = Governing::of(&root).expect("the manifest is well formed");
        let error = order(&governing)
            .err()
            .expect("this compiler is not the one stated")
            .render();
        assert!(
            error.contains(&root.join(MANIFEST).display().to_string()),
            "{error}"
        );
        assert!(
            error.contains(&format!("this package requires Curios {required}\n")),
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
}

#[test]
fn only_enumerated_members_inherit_the_umbrellas_compiler() {
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
        .expect("an unlisted package stands alone")
        .render();
    assert!(
        error.contains("a standalone package states one, as `compiler"),
        "{error}"
    );
    write(
        &root,
        "member/curios.toml",
        concat!(
            "name = \"member\"\ncompiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\n"
        ),
    );
    let error = Governing::of(&root.join("member"))
        .err()
        .expect("a member cannot override its umbrella")
        .render();
    assert!(
        error.contains("`compiler` is inherited from the umbrella at"),
        "{error}"
    );
    assert!(error.contains("remove the local field"), "{error}");
}

#[test]
fn an_inherited_other_version_is_reported_against_the_umbrella() {
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
        .expect("the inherited version is not this compiler's")
        .render();
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
        .expect("the dependency is written for another compiler")
        .render();
    assert!(
        error.contains(&root.join("vendor/curios.toml").display().to_string()),
        "{error}"
    );
    assert!(error.contains("requires Curios"), "{error}");
    fs::remove_file(root.join("vendor/curios.toml")).unwrap();
    let error = order(&governing)
        .err()
        .expect("the dependency states no version of its own")
        .render();
    assert!(
        error.contains("a standalone package states one, as `compiler"),
        "{error}"
    );
}

#[test]
fn a_loose_file_does_not_use_the_surrounding_packages_compiler_version() {
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
    .expect("a declared file uses its package's version")
    .render();
    assert!(error.contains("requires Curios"), "{error}");
}
