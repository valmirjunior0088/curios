//! Which unit a file is placed in for a question — and when none is.

use {
    super::*,
    std::{
        fs,
        time::{SystemTime, UNIX_EPOCH},
    },
};

/// A tree of `(relative path, contents)` pairs, rooted at a fresh directory nothing else is using.
fn tree(name: &str, files: &[(&str, &str)]) -> PathBuf {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();
    let root = std::env::temp_dir().join(format!("curios-{name}-{}-{millis}", std::process::id()));

    for (path, source) in files {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, source).unwrap();
    }

    root
}

/// An umbrella enumerating one member, with a stray file at its root and another in a directory no member holds.
fn umbrella(name: &str) -> PathBuf {
    tree(
        name,
        &[
            ("curios.toml", "members = [\"app\"]\n"),
            ("app/curios.toml", "name = \"app\"\n"),
            ("app/lib.crs", ""),
            ("app/util.crs", ""),
            ("scratch.crs", ""),
            ("tools/scratch.crs", ""),
        ],
    )
}

/// An umbrella declares no units, so a file whose nearest manifest is one belongs to nothing and is asked about standalone — the answer `run` already gives it, where `Governing` would refuse the umbrella instead.
#[test]
fn a_file_no_member_holds_under_an_umbrella_is_standalone() {
    let root = umbrella("membership-stray");

    for stray in ["scratch.crs", "tools/scratch.crs"] {
        assert!(
            matches!(
                Membership::of(&root.join(stray), None).expect("an answer, not a refusal"),
                Membership::Standalone
            ),
            "{stray}"
        );
    }

    fs::remove_dir_all(root).unwrap();
}

/// A member's module is the member's library, under the umbrella's root — the nearest manifest is the member's, and the umbrella governs it.
#[test]
fn a_members_module_is_placed_in_its_library_under_the_umbrella_root() {
    let root = umbrella("membership-member");

    match Membership::of(&root.join("app/util.crs"), None).expect("a placed file") {
        Membership::Library {
            root: governing,
            units,
        } => {
            assert_eq!(governing, root.canonicalize().unwrap());
            assert_eq!(units.len(), 1);
        }
        _ => panic!("a module of the library is the library"),
    }

    fs::remove_dir_all(root).unwrap();
}

/// Naming an umbrella outright with `--manifest` is the refusal it always was: nothing can be asked of a manifest that compiles nothing.
#[test]
fn an_umbrella_named_outright_is_still_refused() {
    let root = umbrella("membership-named");

    let Err(refusal) = Membership::of(&root.join("scratch.crs"), Some(&root.join("curios.toml")))
    else {
        panic!("an umbrella named by hand is refused");
    };
    assert!(refusal.contains("declares an umbrella"), "{refusal}");

    fs::remove_dir_all(root).unwrap();
}
