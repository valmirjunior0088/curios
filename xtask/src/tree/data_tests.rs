//! What the reader holds data to: declared once at the root, its version, where it is published, what of it is kept — one directory of an archive or files published one by one — that directory's digest and, where a crate's build reads it, the crate it is kept beside; named by a step as its kept directory.

use super::{test_support::*, *};

const DIGEST: &str = "abc15ec8c3df7ff6fa771b051a8950012403b241af2688e82374e0bcafcdd21c";

/// The tree whose root declares `data` and whose file `a` holds one step, `step` its keys past its description.
fn under(data: &str, step: &str) -> Result<Tree, Refusal> {
    Tree::parse(&[
        (ROOT_FILE, &format!("{TOP}{data}")),
        (
            "xtask/tasks/a.toml",
            &format!("description = \"A\"\n[s]\ndescription = \"S\"\n{step}\n"),
        ),
    ])
}

/// The data `cases`, one directory of the archive at `url`.
fn cases(url: &str) -> String {
    format!(
        "[data.cases]\nversion = \"1ef36fa\"\nurl = \"{url}\"\npath = \"test_parsing\"\nsha256 = \"{DIGEST}\"\n"
    )
}

/// The data `tables`, the files `files` published under `url`.
fn tables(url: &str, files: &str) -> String {
    format!(
        "[data.tables]\nversion = \"18.0.0\"\nurl = \"{url}\"\nfiles = [{files}]\nsha256 = \"{DIGEST}\"\n"
    )
}

#[test]
fn data_is_read_with_its_version_its_archive_its_directory_and_its_digest() {
    let tree = under(
        &cases("https://example.invalid/archive/{.data.cases.version}.tar.gz"),
        "run = [[\"true\", \"{.data.cases}\"]]",
    )
    .unwrap();

    let data = &tree.data[&name("data.cases")];
    assert_eq!(data.version, "1ef36fa");
    assert_eq!(data.form, Form::Directory("test_parsing".to_string()));
    assert_eq!(data.sha256, DIGEST);
    assert!(data.beside.is_none());
    assert_eq!(
        data.url.0[1],
        Piece::Value(Fill::Variable(name("data.cases.version")))
    );
    let Instruction::Command(words) = &tree.steps[0].run[0] else {
        panic!("the step runs a command");
    };
    assert_eq!(words[1].alone(), Some(&Fill::Data(name("data.cases"))));
}

/// Files a site publishes one by one are listed by their paths under the address, and data a crate's build reads names the crate it is kept beside.
#[test]
fn data_published_as_files_lists_them_and_names_the_crate_whose_build_reads_them() {
    let tree = under(
        &(tables(
            "https://example.invalid/{.data.tables.version}/ucd",
            "\"UnicodeData.txt\", \"emoji/emoji-data.txt\"",
        ) + "beside = \"some-crate\"\n"),
        "run = [[\"true\"]]",
    )
    .unwrap();

    let data = &tree.data[&name("data.tables")];
    assert_eq!(
        data.form,
        Form::Files(vec![
            "UnicodeData.txt".to_string(),
            "emoji/emoji-data.txt".to_string()
        ])
    );
    assert_eq!(data.beside.as_deref(), Some("some-crate"));
}

/// Data is no file a step produces, so a step does not consume it: one a command reads is named in the command, and one a crate's build reads is fetched for every run.
#[test]
fn data_in_consumes_is_refused() {
    let refusal = under(
        &tables("https://example.invalid/ucd", "\"UnicodeData.txt\""),
        "consumes = [\".data.tables\"]\nrun = [[\"true\"]]",
    )
    .unwrap_err()
    .headline;

    assert!(refusal.contains("is no produced file"), "{refusal}");
}

#[test]
fn data_below_the_root_is_refused() {
    let refusal = refused(&[(
        "a",
        &format!(
            "description = \"A\"\n{}",
            cases("https://example.invalid/cases.tar.gz")
        ),
    )]);

    assert!(refusal.contains("`data` belongs at the top"), "{refusal}");
}

/// Data states what is kept of it one way: a directory of a gzipped tarball, or files under an address that is no archive, each a path of names; and it holds its keys and no other.
#[test]
fn data_that_states_what_is_kept_of_it_no_one_way_or_holds_another_key_is_refused() {
    let headline = |data: &str| under(data, "run = [[\"true\"]]").unwrap_err().headline;

    let zipped = headline(&cases("https://example.invalid/cases.zip"));
    assert!(zipped.contains("names the gzipped tarball"), "{zipped}");

    let archived = headline(&tables(
        "https://example.invalid/cases.tar.gz",
        "\"UnicodeData.txt\"",
    ));
    assert!(archived.contains("no archive"), "{archived}");

    let both = headline(
        &(cases("https://example.invalid/cases.tar.gz") + "files = [\"UnicodeData.txt\"]\n"),
    );
    assert!(both.contains("and one of them"), "{both}");

    let neither = headline(&format!(
        "[data.cases]\nversion = \"1\"\nurl = \"https://example.invalid/ucd\"\nsha256 = \"{DIGEST}\"\n"
    ));
    assert!(neither.contains("and one of them"), "{neither}");

    let none = headline(&tables("https://example.invalid/ucd", ""));
    assert!(none.contains("at least one of its `files`"), "{none}");

    let outside = headline(&tables(
        "https://example.invalid/ucd",
        "\"../UnicodeData.txt\"",
    ));
    assert!(outside.contains("is no path under the `url`"), "{outside}");

    let path = headline(
        &(tables("https://example.invalid/ucd", "\"UnicodeData.txt\"")
            + "beside = \"some-crate/.artifacts\"\n"),
    );
    assert!(path.contains("is no crate"), "{path}");

    let other =
        headline(&(cases("https://example.invalid/cases.tar.gz") + "archive = \".tools.node\"\n"));
    assert!(
        other.contains("`archive` is no key of data `cases`"),
        "{other}"
    );
}
