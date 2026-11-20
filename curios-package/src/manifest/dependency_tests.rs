//! A manifest's dependency rows: named by a path, live or pinned, told by the one source each states, and the columns a fetchable row must state.

use {super::test_support::*, super::*, curios_common::fill};

/// A dependency key is a canonical name too, and earns the same refusal.
#[test]
fn a_dependency_named_by_no_path_is_refused() {
    let refusal = refuse(
        r#"
            name = "json"

            [dependencies]
            "my-org" = { member = true }
        "#,
    );

    assert!(refusal.contains("no name a path could spell"), "{refusal}");
}

/// There is no string shorthand: a revision is an opaque string, so a bare one could not be told from a source.
#[test]
fn a_string_shorthand_row_is_refused() {
    let refusal = refuse(
        r#"
            name = "json"

            [dependencies]
            http = "1.0"
        "#,
    );

    assert!(refusal.contains("`http` is a table"), "{refusal}");
}

/// Live code has no pin, so a marker row carrying one is refused rather than ignored.
#[test]
fn a_pin_on_a_live_row_is_refused() {
    let refusal = refuse(
        r#"
            name = "json"

            [dependencies]
            base = { member = true, rev = "abc123" }
        "#,
    );

    assert!(refusal.contains("takes no `rev`"), "{refusal}");
}

/// A fetchable row states both columns beside its source: the instruction that resolves it, and the criterion that accepts it; and a row that states no source names nothing to resolve.
#[test]
fn a_fetchable_row_missing_a_column_is_refused() {
    for (row, missing) in [
        (r#"{ git = "https://e", hash = "c1:x" }"#, "requires `rev`"),
        (r#"{ git = "https://e", rev = "abc" }"#, "requires `hash`"),
        (r#"{ rev = "abc", hash = "c1:x" }"#, "names no source"),
        ("{}", "names no source"),
    ] {
        let refusal = refuse(&format!("name = \"json\"\n[dependencies]\nhttp = {row}"));
        assert!(refusal.contains(missing), "{row}: {refusal}");
    }
}

/// The hash's scheme is checked where the hash is written, not where it is verified.
#[test]
fn a_malformed_hash_is_refused() {
    let refusal = refuse(
        r#"
            name = "json"

            [dependencies]
            http = { git = "https://e", rev = "abc", hash = "sha256:beef" }
        "#,
    );

    assert!(refusal.contains("names no hash scheme"), "{refusal}");
}

/// A `git` URL or a `rev` beginning with `-` is refused where the row is read, because `curate` hands both to `git` and git reads a leading `-` as an option.
///
/// **The hash answers for the delivery, not for the invocation that fetched it.** It is checked after `git remote add`, `git fetch` and `git checkout` have already run on these two values, and all three read a dash-leading positional as an option — so a row could reach git's option parser on a machine that had only vouched for a tree's bytes. `curate` walks each fetched dependency's own manifest for further rows, which is what puts the values under somebody else's hand.
#[test]
fn a_fetchable_column_that_would_read_as_an_option_is_refused() {
    for (field, row) in [
        (
            "git",
            r#"{ git = "-some-option", rev = "abc", hash = "c1:x" }"#,
        ),
        (
            "rev",
            r#"{ git = "https://e", rev = "-some-option", hash = "c1:x" }"#,
        ),
    ] {
        let refusal = refuse(&format!("name = \"json\"\n[dependencies]\nhttp = {row}"));
        assert!(
            refusal.contains(&format!("`{field}` begins with `-`")),
            "{row}: {refusal}"
        );
        assert!(refusal.contains("would read it as an option"), "{refusal}");
    }
}

/// A dash inside a `git` URL or a `rev` is ordinary, and only the leading one is refused: the rule is about what git's option parser reads, not about the character.
#[test]
fn a_dash_inside_a_fetchable_column_is_ordinary() {
    let Manifest::Package(package) = parse(&fill!(
        r#"
            name = "json"

            [dependencies]
            http = { git = "https://example/a-b", rev = "release-1.2", hash = "c1:${digest}" }
        "#,
        digest = "a".repeat(64)
    )) else {
        panic!("a manifest declaring `name` is a package");
    };

    assert!(package.dependencies.contains_key("http"));
}

/// A row's source is the one key of `git`, `path`, `member` and `catalog` it states: a second is refused, naming both, and a marker written anything but `true` names a source and denies it.
#[test]
fn a_row_states_one_source() {
    for (row, expected) in [
        (
            r#"{ git = "https://e", path = "../e", rev = "abc", hash = "c1:x" }"#,
            "conflicting fields\n  this row states `git` beside `path`",
        ),
        (
            "{ member = true, catalog = true }",
            "conflicting fields\n  this row states `catalog` beside `member`",
        ),
        (
            "{ member = false }",
            "invalid value\n  written: `false`\n  `member` is `true`",
        ),
        (
            r#"{ catalog = "yes" }"#,
            "invalid value\n  written: `\"yes\"`\n  `catalog` is a boolean",
        ),
        (
            r#"{ path = "../e", rev = "abc" }"#,
            "surplus field\n  this row's source is `path`\n  that source takes no `rev`",
        ),
    ] {
        let refusal = refuse(&format!("name = \"json\"\n[dependencies]\nhttp = {row}"));
        assert!(refusal.starts_with(expected), "{row}: {refusal}");
    }
}
