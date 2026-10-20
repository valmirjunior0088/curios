use {
    super::*,
    crate::{Delivery, FileHash, Module},
    curios_utilities::test_support::Temporary,
    std::{collections::BTreeMap, os::unix},
};

/// A package directory holding `files`, each `(path, contents)`, in a directory of its own that goes away with the test.
fn package(name: &str, files: &[(&str, &str)]) -> Temporary {
    let root = Temporary::new("served", name);
    fs::create_dir_all(&*root).unwrap();

    for (path, contents) in files {
        let path = root.join(path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, contents).unwrap();
    }

    root
}

/// A gateway serving `statics`, each a page path beside the package path it is read from.
fn gateway(statics: &[(&str, &str)]) -> Gateway {
    Gateway {
        delivery: Delivery {
            module: Module::Filed(PathBuf::from("gateway.js")),
            hash: FileHash::of_bytes(b""),
        },
        statics: statics
            .iter()
            .map(|(page, path)| ((*page).to_string(), PathBuf::from(path)))
            .collect::<BTreeMap<_, _>>(),
    }
}

/// What `gateway` serves from `root`, as `(entry, page path, file under root)`.
fn served(gateway: &Gateway, root: &Path) -> Result<Vec<(String, String, PathBuf)>, String> {
    Ok(gateway
        .served("page", root)?
        .into_iter()
        .map(|served| {
            (
                served.entry,
                served.page,
                served.file.strip_prefix(root).unwrap().to_path_buf(),
            )
        })
        .collect())
}

#[test]
fn a_file_entry_is_served_at_its_key_whatever_its_path_is() {
    let root = package("file", &[("web/logo.svg", "<svg/>")]);

    assert_eq!(
        served(&gateway(&[("logo.svg", "web/logo.svg")]), &root).unwrap(),
        [(
            "logo.svg".to_string(),
            "logo.svg".to_string(),
            PathBuf::from("web/logo.svg")
        )]
    );
}

#[test]
fn a_directory_entry_serves_every_file_under_it_at_its_key_joined_with_its_path_inside() {
    let root = package(
        "directory",
        &[
            ("assets/app.js", ""),
            ("assets/images/a.png", ""),
            ("assets/images/b.png", ""),
        ],
    );

    let pages = served(&gateway(&[("static", "assets")]), &root)
        .unwrap()
        .into_iter()
        .map(|(entry, page, _)| (entry, page))
        .collect::<Vec<_>>();

    assert_eq!(
        pages,
        [
            ("static".to_string(), "static/app.js".to_string()),
            ("static".to_string(), "static/images/a.png".to_string()),
            ("static".to_string(), "static/images/b.png".to_string()),
        ]
    );
}

/// What a person serving a directory does not mean to publish: a `.git`, a `.DS_Store`, an editor's swap file.
#[test]
fn a_name_starting_with_a_dot_inside_a_directory_is_skipped() {
    let root = package(
        "dotted",
        &[
            ("assets/app.js", ""),
            ("assets/.DS_Store", ""),
            ("assets/.git/HEAD", ""),
        ],
    );

    let pages = served(&gateway(&[("assets", "assets")]), &root)
        .unwrap()
        .into_iter()
        .map(|(_, page, _)| page)
        .collect::<Vec<_>>();

    assert_eq!(pages, ["assets/app.js"]);
}

#[test]
fn a_symlink_is_refused_wherever_it_stands() {
    let root = package("linked", &[("real/app.js", ""), ("assets/app.js", "")]);
    unix::fs::symlink(root.join("real"), root.join("alias")).unwrap();
    unix::fs::symlink(root.join("real/app.js"), root.join("assets/linked.js")).unwrap();

    for (entry, path, at) in [
        ("whole", "alias", "alias"),
        ("through", "alias/app.js", "alias"),
        ("inside", "assets", "assets/linked.js"),
    ] {
        let refusal = served(&gateway(&[(entry, path)]), &root).unwrap_err();

        assert!(
            refusal.contains(&format!(
                "the static entry {entry:?} of the foreign row \"page\""
            )) && refusal.contains(&format!(
                "{}, which is a symbolic link",
                root.join(at).display()
            )),
            "{refusal}"
        );
    }
}

#[test]
fn an_entry_whose_path_names_nothing_is_refused_by_the_entry() {
    let root = package("missing", &[]);

    let refusal = served(&gateway(&[("logo.svg", "web/logo.svg")]), &root).unwrap_err();

    assert!(
        refusal.contains("the static entry \"logo.svg\" of the foreign row \"page\" reads")
            && refusal.contains("which holds nothing"),
        "{refusal}"
    );
}
