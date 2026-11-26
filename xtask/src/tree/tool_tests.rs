//! What the reader holds a tool to: a place at the root's top, a download and a digest for each host, a URL ending in what it fetches, and an archive that ships what names it.

use {super::test_support::*, super::*, curios_common::fill};

const DIGEST: &str = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef";

/// The tree whose root declares the tool `t`, its table holding `table`, and whose file `a` runs it.
fn with_tool(table: &str) -> Result<Tree, Refusal> {
    Tree::parse(&[
        (
            ROOT_FILE,
            &fill!("what = \"The top\"\n[tools.t]\n${table}\n", table),
        ),
        (
            "xtask/tasks/a.toml",
            "what = \"A\"\n[s]\nwhat = \"S\"\nrun = [[\"{.tools.t}\", \"go\"]]\n",
        ),
    ])
}

/// What the reader refuses the tree [`with_tool`] makes with.
fn refused_with(table: &str) -> String {
    with_tool(table).unwrap_err().headline
}

/// A tool `t` at version 1.0 fetched on one host from a URL ending in `tail`.
fn fetched(tail: &str) -> String {
    fill!(
        "version = \"1.0\"\nurl.x86_64-unknown-linux-gnu = \"https://example.org/{.tools.t.version}/${tail}\"\nsha256.x86_64-unknown-linux-gnu = \"${DIGEST}\"",
        tail,
        DIGEST
    )
}

fn tool(tree: &Tree) -> &Fetched {
    match &tree.tools[&name("tools.t")] {
        Tool::Fetched(fetched) => fetched,
        Tool::Shipped { .. } => panic!("the tool is fetched itself"),
    }
}

/// A tool's version is a value its URLs write, and a step that names the tool is handed its executable.
#[test]
fn a_tool_s_url_writes_its_version_and_a_step_names_the_tool() {
    let tree = with_tool(&fetched("t.tar.gz")).unwrap();

    let published = &tool(&tree).downloads["x86_64-unknown-linux-gnu"];
    assert!(
        published
            .url
            .fills()
            .any(|fill| *fill == Fill::Variable(name("tools.t.version")))
    );
    assert_eq!(published.sha256, DIGEST);
    let Instruction::Command { words, .. } = &tree.steps[0].run[0] else {
        panic!("the step runs a command");
    };
    assert_eq!(words[0].alone(), Some(&Fill::Tool(name("tools.t"))));
}

/// A download is kept by what its URL ends in: a tarball unpacked, a gzipped file decoded, anything else as it is.
#[test]
fn a_download_s_shape_is_read_off_the_end_of_its_url() {
    for (tail, shape) in [
        ("t-linux.tar.gz", Shape::Tarball),
        ("t-linux.gz", Shape::Gzipped),
        ("t-linux", Shape::Bare),
    ] {
        let tree = with_tool(&fetched(tail)).unwrap();

        assert_eq!(
            tool(&tree).downloads["x86_64-unknown-linux-gnu"].shape,
            shape,
            "{tail}"
        );
    }
}

#[test]
fn a_tool_kept_inside_a_directory_is_its_own_name_there_unless_a_path_says_otherwise() {
    let named = with_tool(&fetched("t.tar.gz")).unwrap();
    let pathed = with_tool(&format!("{}\npath = \"bin/t\"", fetched("t.tar.gz"))).unwrap();

    assert_eq!(tool(&named).path, "t");
    assert_eq!(tool(&pathed).path, "bin/t");
}

#[test]
fn a_digest_that_is_no_sha256_is_refused() {
    for digest in ["0123", &DIGEST.to_uppercase()] {
        let table = fetched("t").replace(DIGEST, digest);

        assert!(refused_with(&table).contains("is a SHA-256"), "{digest}");
    }
}

/// Every host a tool runs on has a download and a digest, so a digest without a URL, or a URL without one, is refused.
#[test]
fn a_url_and_a_digest_stand_for_the_same_hosts() {
    let digest_alone = format!(
        "{}\nsha256.aarch64-apple-darwin = \"{DIGEST}\"",
        fetched("t")
    );
    let url_alone = format!(
        "{}\nurl.aarch64-apple-darwin = \"https://example.org/t\"",
        fetched("t")
    );

    assert!(refused_with(&digest_alone).contains("has no `url.aarch64-apple-darwin`"));
    assert!(refused_with(&url_alone).contains("has no `sha256.aarch64-apple-darwin`"));
}

/// What a download is kept as is read off the end of its URL, which a value would hide until the run.
#[test]
fn a_url_ending_in_a_value_is_refused() {
    let table = fetched("t").replace("/t\"", "/{.tools.t.version}\"");

    assert!(refused_with(&table).contains("ends in the file it names"));
}

/// An executable another tool's download holds is found beside it in the directory that download is unpacked into, which a download kept as one executable has not.
#[test]
fn a_tool_shipped_in_one_kept_as_a_lone_executable_is_refused() {
    let table = format!(
        "{}\n[tools.u]\narchive = \".tools.t\"\npath = \"bin/u\"",
        fetched("t")
    );

    assert!(refused_with(&table).contains("nothing ships beside it"));
}

#[test]
fn a_tool_shipped_in_another_names_its_path_there() {
    let table = format!("{}\n[tools.u]\narchive = \".tools.t\"", fetched("t.tar.gz"));

    assert!(refused_with(&table).contains("names its `path` there"));
}

/// Every tool is declared at the root's top, one table of them, so a subject's file declaring one is refused.
#[test]
fn a_tool_outside_the_root_is_refused() {
    assert!(
        refused(&[("a", &format!("what = \"A\"\n[tools.t]\n{}\n", fetched("t")))])
            .contains("belongs at the top of `xtask/tasks.toml`")
    );
}

#[test]
fn tools_below_a_file_s_top_are_refused() {
    assert!(
        refused(&[(
            "a",
            "what = \"A\"\n[s]\nwhat = \"S\"\nrun = [[\"true\"]]\ntools.t = { version = \"1\" }\n"
        )])
        .contains("belongs at the top of `xtask/tasks.toml`")
    );
}
