//! The pure path algebra: bytes underneath, `/` as the one separator, and the functions a listing entry or a specification needs.

use super::run;

// The four path functions, with `/` as the one separator: a join never doubles it, a name is the last component, a parent of a root child is `/`, and an extension is what follows the last `.` of a name that does not start with it.
#[test]
fn the_path_functions_split_and_join_on_the_separator() {
    let source = r#"
        use /std/{Str, Option, Show, Path, print};
        let p(s: Str) -> Path = Path/of_str(s);
        let shown =
            Str/join(" ", [
                Show/show(Path/join(p("a/"), p("b"))), Show/show(Path/join(p("a"), p("b"))), Show/show(Path/join(p(""), p("b"))),
                Show/show(Path/name(p("a/b/c.txt"))),
                Show/show(Path/parent(p("a/b/c"))), Show/show(Path/parent(p("c"))), Show/show(Path/parent(p("/c"))),
                Show/show(Path/extension(p("archive.tar.gz"))), Show/show(Path/extension(p(".bashrc"))), Show/show(Path/extension(p("noext")))]);
        print(shown)
        "#;

    assert_eq!(
        run(source),
        b"a/b a/b b c.txt some(a/b) none() some(/) some(gz) none() none()"
    );
}

// A path is bytes, so a name that is not UTF-8 is a path like any other: it joins, splits and compares, and only `to_str` refuses it.
#[test]
fn a_path_that_is_not_utf8_is_a_path_like_any_other() {
    let source = r#"
        use /std/{Str, Bytes, Option, Bool, Show, Path, print};
        let odd = Path/of_bytes(x[0xFF, 'a']);
        let full = Path/join(Path/of_str("dir"), odd);
        let same = Path/name(full) == odd;
        let decodes = match Path/to_str(odd) | some(_) => "text" | none() => "bytes" end;
        print(Str/join(" ", [Bool/to_str(same), decodes, Show/show(Path/parent(full))]))
        "#;

    assert_eq!(run(source), b"true bytes some(dir)");
}
