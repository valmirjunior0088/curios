//! What the agent-file check reads of a rule's globs and of what a file cites, over text of the tests' own.

use {super::*, std::path::Path};

#[test]
fn a_double_star_crosses_directories_and_a_star_stays_inside_one() {
    assert!(matches("curios/src/**", "curios/src/a/b.rs"));
    assert!(matches(
        "**/000-findings.md",
        "documentation/roadmap/000-findings.md"
    ));
    assert!(matches("xtask/*.toml", "xtask/tasks.toml"));
    assert!(!matches("xtask/*.toml", "xtask/tasks/zed.toml"));
    assert!(!matches("curios/src/**", "curios-js/src/lib.rs"));
}

#[test]
fn a_rule_s_globs_are_its_front_matter_s_paths_and_its_body_what_follows() {
    let (globs, body) =
        globs_and_body("---\npaths:\n  - \"xtask/**\"\n  - \".github/**\"\n---\n\n# Body\n");

    assert_eq!(globs, ["xtask/**", ".github/**"]);
    assert_eq!(body, "\n# Body\n");
    assert_eq!(globs_and_body("# No front matter\n").0, Vec::<&str>::new());
}

#[test]
fn a_code_span_is_read_on_its_line_and_never_inside_a_fenced_block() {
    let body = "Run `cargo xtask` and ``a `tick` inside``.\n```sh\n`fenced`\n```\nThen `after`.\n";

    assert_eq!(
        code_spans(body),
        ["cargo xtask", "a `tick` inside", "after"]
    );
}

#[test]
fn a_link_names_a_file_and_not_an_address_or_its_own_page_s_anchor() {
    let body = "[a](../a.md#part) [b](https://example.invalid/b) [c](#here) [d](d.md)";

    assert_eq!(link_targets(body), ["../a.md", "d.md"]);
}

#[test]
fn a_link_is_resolved_from_its_file_s_directory() {
    assert_eq!(
        resolved(Path::new(".claude/rules"), "../../documentation/./usage.md"),
        "documentation/usage.md"
    );
}

#[test]
fn an_identifier_is_lowercase_words_joined_by_underscores() {
    assert!(is_identifier("every_step_the_handoff_runs_has_a_job"));
    assert!(!is_identifier("handoff"));
    assert!(!is_identifier("Tree_read"));
    assert!(!is_identifier("a__b"));
}
