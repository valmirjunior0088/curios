//! A manifest's foreign rows and host tables: a carried or fetched module, one delivery a row, the exports it answers, and what the browser and native tables serve.

use {
    super::test_support::{parse, refuse},
    super::*,
};

/// The two deliveries a `[[foreign]]` row states, and the table naming which declaration each export answers.
#[test]
fn foreign_rows_state_a_carried_module_or_a_fetched_one() {
    let Manifest::Package(package) = parse(
        r#"
            name = "crypto"

            [[foreign]]
            name = "sha2"

            [foreign.native]
            path = "foreign/libsha2.wasm"
            hash = "f1:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

            [foreign.answers]
            "digest/sha256" = "SHA256_Digest"

            [[foreign]]
            name = "blake3"
            native = { url = "https://example.com/libblake3.wasm", hash = "f1:fedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210" }
        "#,
    ) else {
        panic!("a package");
    };

    assert_eq!(
        package.foreign[0]
            .native
            .as_ref()
            .map(|native| native.module.clone()),
        Some(Module::Filed(PathBuf::from("foreign/libsha2.wasm")))
    );
    assert_eq!(
        package.foreign[0]
            .answers
            .get("/crypto/digest/sha256")
            .map(String::as_str),
        Some("SHA256_Digest")
    );
    assert_eq!(
        package.foreign[1]
            .native
            .as_ref()
            .map(|native| native.module.clone()),
        Some(Module::Fetched(
            "https://example.com/libblake3.wasm".to_string()
        ))
    );
}

/// A row answers its own package's declarations: each is named from the package's root, so a claim on another package's row, or on the standard library's, has no spelling. A path a manifest once wrote in full is refused with the spelling it has now, and one that leads elsewhere with why it names nothing here.
#[test]
fn an_export_answers_a_declaration_named_from_its_own_package() {
    const ROW: &str = "name = \"crypto\"\n\n[[foreign]]\nname = \"sha2\"\n\n[foreign.native]\npath = \"m.wasm\"\nhash = \"f1:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\"\n\n[foreign.answers]\n";

    for (claim, expected) in [
        (
            "/crypto/digest/sha256",
            "this one is written `digest/sha256`",
        ),
        ("/std/rand/rand_bytes", "is no declaration of crypto"),
        ("/other/give", "is no declaration of crypto"),
        ("/cryptography/give", "is no declaration of crypto"),
        ("digest//sha256", "it is empty"),
        ("digest/let", "it is a keyword"),
        ("../other/give", "it is no identifier"),
    ] {
        let refusal = format!("{ROW}\"{claim}\" = \"answer\"\n")
            .parse::<Manifest>()
            .expect_err("a claim outside the package");

        assert!(refusal.contains(expected), "{claim}: {refusal}");
    }
}

/// A declaration is answered once across a package's rows: a second row that names an export for it is refused where the manifest is read, naming both rows, since nothing chooses between two answers. Within a row the table is keyed by declaration, so one row cannot state two.
#[test]
fn a_declaration_two_rows_answer_is_refused() {
    const PIN: &str =
        "hash = \"f1:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\"";
    let row = |name: &str, export: &str| {
        format!(
            "[[foreign]]\nname = \"{name}\"\n\n[foreign.native]\npath = \"{name}.wasm\"\n{PIN}\n\n[foreign.answers]\n\"digest/sha256\" = \"{export}\"\n\n"
        )
    };

    let refusal = format!(
        "name = \"crypto\"\n\n{}{}",
        row("sha2", "SHA256_Digest"),
        row("other", "digest")
    )
    .parse::<Manifest>()
    .expect_err("one declaration, two rows");

    assert!(
        refusal.contains(
            "the foreign rows \"sha2\" and \"other\" both name an export that answers `/crypto/digest/sha256`"
        ),
        "{refusal}"
    );
}

/// Every wrong combination of a delivery's fields, each refused by name rather than by whichever shape was tried second.
#[test]
fn a_foreign_row_states_one_delivery_and_no_half_of_another() {
    const PIN: &str =
        "hash = \"f1:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\"";

    for (row, expected) in [
        (
            format!("path = \"m.wasm\"\nurl = \"https://e.com/m.wasm\"\n{PIN}"),
            "never both",
        ),
        (PIN.to_string(), "names no file"),
        ("".to_string(), "names no file"),
        // Stated for a carried module too: the tree holding it need not be pinned, so this is the delivery the hash is load-bearing for.
        ("path = \"m.wasm\"".to_string(), "states no `hash`"),
        ("url = \"https://e.com/m.wasm\"".to_string(), "states no `hash`"),
        (
            format!("path = \"../outside.wasm\"\n{PIN}"),
            "no plain relative path",
        ),
        (
            format!("url = \"-oProxyCommand=x\"\n{PIN}"),
            "would read it as an option",
        ),
        (
            "url = \"https://e.com/m.wasm\"\nhash = \"c1:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\"".to_string(),
            "names no hash scheme",
        ),
    ] {
        let row = row.as_str();
        let refusal = refuse(&format!(
            "name = \"crypto\"\n\n[[foreign]]\nname = \"sha2\"\n\n[foreign.native]\n{row}\n"
        ));

        assert!(refusal.contains(expected), "{row:?} reported {refusal}");
    }
}

/// A row states its module in the native target's table, and one with no such table links nothing: refused where the manifest is read, saying where a module goes.
#[test]
fn a_foreign_row_with_no_delivery_says_where_one_goes() {
    let refusal = refuse("name = \"crypto\"\n\n[[foreign]]\nname = \"sha2\"\n");

    assert!(
        refusal.contains("names no module: state `[foreign.native]`"),
        "{refusal}"
    );
}

/// A row's browser table names its gateway and the files the page serves beside it, and a row may state it alone: what each target links is optional, and a target reaching a row without its table is refused where it links.
#[test]
fn a_browser_table_names_a_gateway_and_what_its_page_serves() {
    let Manifest::Package(package) = parse(
        r#"
            name = "clock"

            [[foreign]]
            name = "dom"

            [foreign.browser]
            path = "web/dom.js"
            hash = "f1:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

            [foreign.browser.static]
            "app" = "build/app"
            "logo.png" = "art/logo.png"
        "#,
    ) else {
        panic!("a package");
    };

    let row = &package.foreign[0];
    assert!(row.native.is_none());
    let gateway = row.browser.as_ref().expect("a browser table");
    assert_eq!(
        gateway.delivery.module,
        Module::Filed(PathBuf::from("web/dom.js"))
    );
    assert_eq!(
        gateway.statics,
        BTreeMap::from([
            ("app".to_string(), PathBuf::from("build/app")),
            ("logo.png".to_string(), PathBuf::from("art/logo.png")),
        ])
    );
    assert_eq!(row.delivery(Target::Browser), Some(&gateway.delivery));
    assert_eq!(row.delivery(Target::Native), None);
}

/// The page holds `index.html`, `index.js` and `index.wasm` for itself, so no package's file may stand at one: a static key, a carried gateway's path and a fetched gateway's `<row>.js` are each refused where the manifest is read.
#[test]
fn the_page_s_own_names_are_refused() {
    const PIN: &str =
        "hash = \"f1:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\"";

    for (name, table) in [
        (
            "dom",
            format!(
                "path = \"web/dom.js\"\n{PIN}\n\n[foreign.browser.static]\n\"index.js\" = \"own.js\""
            ),
        ),
        ("dom", format!("path = \"index.html\"\n{PIN}")),
        (
            "index",
            format!("url = \"https://e.com/gateway.js\"\n{PIN}"),
        ),
    ] {
        let refusal = refuse(&format!(
            "name = \"clock\"\n\n[[foreign]]\nname = \"{name}\"\n\n[foreign.browser]\n{table}\n"
        ));

        assert!(
            refusal.contains("which the page holds for itself"),
            "{table:?} reported {refusal}"
        );
    }
}

/// Both sides of a `static` entry are plain relative paths naming something, and the store is no file a page serves.
#[test]
fn a_static_entry_names_plain_paths_outside_the_store() {
    const PIN: &str =
        "hash = \"f1:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\"";

    for (entry, expected) in [
        ("\"../up\" = \"art\"", "no plain relative path"),
        ("\"art\" = \"/etc\"", "no plain relative path"),
        ("\"\" = \"art\"", "an empty page path"),
        ("\"art\" = \"\"", "an empty package path"),
        ("\"art\" = \".curios/native\"", "under `.curios`"),
    ] {
        let refusal = refuse(&format!(
            "name = \"clock\"\n\n[[foreign]]\nname = \"dom\"\n\n[foreign.browser]\npath = \"web/dom.js\"\n{PIN}\n\n[foreign.browser.static]\n{entry}\n"
        ));

        assert!(refusal.contains(expected), "{entry:?} reported {refusal}");
    }
}

/// A file a page serves is the browser's concern, so the native table takes no `static`.
#[test]
fn the_native_table_serves_nothing() {
    let refusal = refuse(
        "name = \"clock\"\n\n[[foreign]]\nname = \"dom\"\n\n[foreign.native]\npath = \"dom.wasm\"\nhash = \"f1:0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef\"\n\n[foreign.native.static]\n\"a\" = \"b\"\n",
    );

    assert!(refusal.contains("static"), "{refusal}");
}
