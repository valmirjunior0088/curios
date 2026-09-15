//! Which unit a file is placed in for a question — and when none is.

use {super::*, curios_utilities::test_support::Temporary, std::fs};

/// A tree of `(relative path, contents)` pairs, in a directory of its own that goes away with the test.
fn tree(name: &str, files: &[(&str, &str)]) -> Temporary {
    let root = Temporary::new("membership", name);

    for (path, source) in files {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, source).unwrap();
    }

    fs::create_dir_all(&root).unwrap();

    root
}

/// An umbrella enumerating one member, with a stray file at its root and another in a directory no member holds.
fn umbrella(name: &str) -> Temporary {
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
}

/// A member's module is the member's library, under the umbrella's root — the nearest manifest is the member's, and the umbrella governs it.
#[test]
fn a_members_module_is_placed_in_its_library_under_the_umbrella_root() {
    let root = umbrella("membership-member");

    match Membership::of(&root.join("app/util.crs"), None).expect("a placed file") {
        Membership::Library {
            root: governing,
            units,
            ..
        } => {
            assert_eq!(governing, root.to_path_buf());
            assert_eq!(units.len(), 1);
        }
        _ => panic!("a module of the library is the library"),
    }
}

/// A placed file carries the module its spelling names, by the layout rule and its exception: the header is the root, a `.crs` under the directory is its path, and a file no `mod` could declare — another extension, a segment no identifier — names none.
#[test]
fn a_placed_file_carries_the_module_its_spelling_names() {
    let root = tree(
        "membership-module",
        &[
            ("curios.toml", "name = \"app\"\n"),
            ("lib.crs", ""),
            ("util.crs", ""),
            ("parse/lexer.crs", ""),
            ("notes.txt", ""),
            ("odd-name.crs", ""),
        ],
    );

    for (file, expected) in [
        ("lib.crs", Some("/app")),
        ("util.crs", Some("/app/util")),
        ("parse/lexer.crs", Some("/app/parse/lexer")),
        ("notes.txt", None),
        ("odd-name.crs", None),
    ] {
        let Membership::Library { module, .. } =
            Membership::of(&root.join(file), None).expect("a placed file")
        else {
            panic!("{file} is under the package directory");
        };
        assert_eq!(
            module.as_ref().map(Qualifier::join),
            expected.map(str::to_string),
            "{file}"
        );
    }
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
}
