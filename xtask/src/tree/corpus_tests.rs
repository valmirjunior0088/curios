//! What the reader holds a corpus to: declared once at the root, its commit, the archive its forge generates at it, the directory kept and that directory's digest, and named by a step as its kept directory.

use super::{test_support::*, *};

const DIGEST: &str = "abc15ec8c3df7ff6fa771b051a8950012403b241af2688e82374e0bcafcdd21c";

/// The tree whose root declares `corpora` and whose file `a` holds one step running `run`.
fn under(corpora: &str, run: &str) -> Result<Tree, Box<Report>> {
    Tree::parse(&[
        (ROOT_FILE, &format!("{TOP}{corpora}")),
        (
            "xtask/tasks/a.toml",
            &format!("description = \"A\"\n[s]\ndescription = \"S\"\nrun = [{run}]\n"),
        ),
    ])
}

/// The corpus `cases`, its archive's URL `url`.
fn cases(url: &str) -> String {
    format!(
        "[corpora.cases]\nversion = \"1ef36fa\"\nurl = \"{url}\"\npath = \"test_parsing\"\nsha256 = \"{DIGEST}\"\n"
    )
}

#[test]
fn a_corpus_is_read_with_its_commit_its_archive_its_directory_and_its_digest() {
    let tree = under(
        &cases("https://example.invalid/archive/{.corpora.cases.version}.tar.gz"),
        "[\"true\", \"{.corpora.cases}\"]",
    )
    .unwrap();

    let corpus = &tree.corpora[&name("corpora.cases")];
    assert_eq!(corpus.version, "1ef36fa");
    assert_eq!(corpus.path, "test_parsing");
    assert_eq!(corpus.sha256, DIGEST);
    assert_eq!(
        corpus.url.0[1],
        Piece::Value(Fill::Variable(name("corpora.cases.version")))
    );
    let Instruction::Command(words) = &tree.steps[0].run[0] else {
        panic!("the step runs a command");
    };
    assert_eq!(words[1].alone(), Some(&Fill::Corpus(name("corpora.cases"))));
}

#[test]
fn a_corpus_below_the_root_is_refused() {
    let refusal = refused(&[(
        "a",
        &format!(
            "description = \"A\"\n{}",
            cases("https://example.invalid/cases.tar.gz")
        ),
    )]);

    assert!(
        refusal.contains("`corpora` belongs at the top"),
        "{refusal}"
    );
}

/// A corpus is a directory of the archive a forge generates at its commit, so its URL names a gzipped tarball, and it holds its four keys and no other.
#[test]
fn a_corpus_whose_archive_is_no_gzipped_tarball_or_that_holds_another_key_is_refused() {
    let zipped = under(&cases("https://example.invalid/cases.zip"), "[\"true\"]")
        .unwrap_err()
        .headline;
    assert!(zipped.contains("names the gzipped tarball"), "{zipped}");

    let other = under(
        &(cases("https://example.invalid/cases.tar.gz") + "archive = \".tools.node\"\n"),
        "[\"true\"]",
    )
    .unwrap_err()
    .headline;
    assert!(
        other.contains("`archive` is no key of corpus `cases`"),
        "{other}"
    );
}
