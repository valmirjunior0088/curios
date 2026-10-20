use super::*;

/// The subresource integrity of no bytes, as any SHA-256 tool spells it.
const EMPTY: &str = "sha256-47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=";

/// A file at `path` holding `bytes`.
fn placed(path: &str, bytes: &[u8]) -> Placed {
    Placed {
        path: path.to_string(),
        bytes: bytes.to_vec(),
    }
}

/// The row `row` of `package`, its gateway at `gateway` answering `/<package>/ping` by `ping`, serving `statics`.
fn linked(package: &str, row: &str, gateway: &str, statics: &[(&str, &str)]) -> Linked {
    Linked {
        package: package.to_string(),
        row: row.to_string(),
        gateway: placed(gateway, b""),
        answers: vec![(format!("/{package}/ping"), "ping".to_string())],
        statics: statics
            .iter()
            .map(|(entry, path)| ((*entry).to_string(), placed(path, b"")))
            .collect(),
    }
}

/// The page of the program `app`, declared by the package `app`, linking `rows`.
fn page_of(rows: Vec<Linked>) -> Result<BTreeMap<String, Vec<u8>>, String> {
    page(
        Program {
            name: "app",
            package: Some("app"),
            module: b"\0asm".to_vec(),
            foreigns: &ForeignStore::new(),
        },
        rows,
    )
}

fn text(files: &BTreeMap<String, Vec<u8>>, path: &str) -> String {
    String::from_utf8(files[path].clone()).expect("a page's text is UTF-8")
}

#[test]
fn the_programs_package_stands_at_the_root_and_a_dependency_under_its_name() {
    let files = page_of(vec![
        linked(
            "app",
            "dom",
            "web/dom.js",
            &[("assets/logo.svg", "assets/logo.svg")],
        ),
        linked("chart", "draw", "draw.js", &[("chart.css", "chart.css")]),
    ])
    .unwrap();

    assert_eq!(
        files.keys().map(String::as_str).collect::<Vec<_>>(),
        [
            "assets/logo.svg",
            "chart/chart.css",
            "chart/draw.js",
            "index.html",
            "index.js",
            "index.wasm",
            "web/dom.js",
        ]
    );
    assert_eq!(files[INDEX_WASM], b"\0asm");
}

#[test]
fn a_path_claimed_twice_is_refused_naming_both_claimants() {
    let refusal = page_of(vec![
        linked("app", "dom", "dom.js", &[("shared", "shared/a.js")]),
        linked("app", "draw", "shared/a.js", &[]),
    ])
    .unwrap_err();

    assert_eq!(
        refusal,
        "the page path `shared/a.js` is claimed by the static entry \"shared\" of the foreign row \"dom\" of app and by the gateway of the foreign row \"draw\" of app; a page holds one file at a path"
    );
}

/// `curios-package` refuses each of the page's own names where a manifest is read, so this is the page holding to its own layout whatever it is handed.
#[test]
fn a_file_at_one_of_the_pages_own_names_is_refused_as_the_pages() {
    let refusal = page_of(vec![linked("app", "dom", "index.js", &[])]).unwrap_err();

    assert!(
        refusal.contains("`index.js` is claimed by the page itself and by the gateway"),
        "{refusal}"
    );
}

#[test]
fn the_import_map_states_every_javascript_files_integrity_and_the_page_the_loaders() {
    let files = page_of(vec![linked(
        "app",
        "dom",
        "dom.js",
        &[("assets", "assets/app.mjs"), ("assets", "assets/logo.svg")],
    )])
    .unwrap();
    let document = text(&files, INDEX_HTML);

    assert!(
        document.contains(&format!(
            r#"<script type="importmap">{{"integrity":{{"./assets/app.mjs":"{EMPTY}","./dom.js":"{EMPTY}"}}}}</script>"#
        )),
        "{document}"
    );
    assert!(
        document.contains(&format!(
            r#"<script type="module" src="index.js" integrity="{}"></script>"#,
            subresource(&files[INDEX_JS])
        )),
        "{document}"
    );
}

#[test]
fn the_loader_imports_each_gateway_and_holds_the_program_to_its_integrity() {
    let files = page_of(vec![
        linked("app", "dom", "web/dom.js", &[]),
        linked("chart", "draw", "draw.js", &[]),
    ])
    .unwrap();
    let loader = text(&files, INDEX_JS);

    assert!(
        loader.contains("import * as gateway0 from \"./web/dom.js\";")
            && loader.contains("import * as gateway1 from \"./chart/draw.js\";"),
        "{loader}"
    );
    assert!(
        loader.contains(&format!(
            "integrity: {:?},",
            subresource(&files[INDEX_WASM])
        )),
        "{loader}"
    );
}

/// A path is the package's to spell, and a URL is not a path: what a segment cannot carry as itself is percent-encoded, so `#` starts no fragment and `?` no query.
#[test]
fn a_page_path_is_a_relative_url_with_what_a_segment_cannot_carry_percent_encoded() {
    assert_eq!(relative("web/a b#c?.js"), "./web/a%20b%23c%3F.js");
    assert_eq!(relative("é.js"), "./%C3%A9.js");
}

/// Raw text in a `<script>` ends at the first `</script`, which a page path may spell, and a URL spells `<` percent-encoded.
#[test]
fn no_angle_bracket_reaches_the_import_map_as_itself() {
    let files = page_of(vec![linked("app", "dom", "a</script>.js", &[])]).unwrap();
    let document = text(&files, INDEX_HTML);
    let map = document
        .split(r#"<script type="importmap">"#)
        .nth(1)
        .and_then(|rest| rest.split("</script>").next())
        .unwrap();

    assert!(!map.contains('<') && map.contains("%3C/script%3E"), "{map}");
}

#[test]
fn nothing_the_page_writes_is_root_absolute() {
    let files = page_of(vec![linked("app", "dom", "dom.js", &[])]).unwrap();

    for file in [INDEX_HTML, INDEX_JS] {
        let written = text(&files, file);

        for absolute in ["src=\"/", "from \"/", "\"/index"] {
            assert!(!written.contains(absolute), "{file} holds {absolute}");
        }
    }
}

#[test]
fn a_file_is_typed_by_its_extension_whatever_its_case_and_by_nothing_else() {
    for (path, expected) in [
        ("index.html", "text/html; charset=utf-8"),
        ("index.js", "text/javascript; charset=utf-8"),
        ("web/app.MJS", "text/javascript; charset=utf-8"),
        ("index.wasm", "application/wasm"),
        ("assets.v2/logo", "application/octet-stream"),
        ("archive.tar.gz", "application/octet-stream"),
    ] {
        assert_eq!(content_type(path), expected, "{path}");
    }
}
