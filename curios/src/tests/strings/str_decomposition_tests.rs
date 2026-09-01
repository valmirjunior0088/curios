//! Taking a `Str` apart: splitting, searching, replacing, trimming one end, padding, and the scalar list.

use crate::tests::run;

// The separator is a two-byte scalar and two of them sit side by side, so the split has to advance by scalar and hand back the empty piece between them; joining the pieces on the separator is the identity.
#[test]
fn split_advances_by_scalar_and_keeps_empty_pieces() {
    let source = r#"
        use /std/{Str, List, Bool};
        let parts = Str/split("a€€b€c", "€");
        /std/print(Str/flatten([
            Str/join("|", parts), " ",
            Bool/to_str(Str/join("€", parts) == "a€€b€c"), " ",
            Str/join("|", Str/split("héj", "")), " ",
            Str/join("|", Str/split("abc", "x"))
        ]))
        "#;

    assert_eq!(run(source), b"a||b|c true h|\xc3\xa9|j abc");
}

#[test]
fn split_once_takes_the_first_occurrence_and_contains_reads_it() {
    let source = r#"
        use /std/{Str, Option, Bool};
        let first =
            match Str/split_once("key=value=x", "=")
            | some((k, v)) => Str/flatten(["(", k, ")(", v, ")"])
            | none() => "none"
            end;
        let missing =
            match Str/split_once("plain", "=") | some(_) => "some" | none() => "none" end;
        /std/print(Str/flatten([
            first, " ", missing, " ",
            Bool/to_str(Str/contains("hello", "ell")), " ",
            Bool/to_str(Str/contains("hello", "elo")), " ",
            Bool/to_str(Str/contains("hello", ""))
        ]))
        "#;

    assert_eq!(run(source), b"(key)(value=x) none true false true");
}

// `"a\r\nb\n"` is two lines: the carriage return is stripped and the final terminator closes `b` rather than opening a third, empty line. `""` has no lines at all.
#[test]
fn lines_drop_the_carriage_return_and_the_final_terminator() {
    let source = r#"
        use /std/{Str, List, Nat};
        let render(ls: List(Str)) -> Str = Str/flatten(List/map(ls, (l: Str) => Str/flatten(["[", l, "]"])));
        /std/print(Str/flatten([
            render(Str/lines("a\r\nb\n")), " ",
            render(Str/lines("solo")), " ",
            Nat/to_str(List/len(Str/lines("")))
        ]))
        "#;

    assert_eq!(run(source), b"[a][b] [solo] 0");
}

// Both tests compare scalar windows: `hé` is three bytes and two scalars. A suffix longer than the string is refused, and the empty string is a prefix and a suffix of everything.
#[test]
fn starts_with_and_ends_with_compare_scalar_windows() {
    let source = r#"
        use /std/{Str, Bool};
        /std/print(Str/join(" ", [
            Bool/to_str(Str/starts_with("héllo", "hé")),
            Bool/to_str(Str/starts_with("héllo", "hi")),
            Bool/to_str(Str/ends_with("héllo", "llo")),
            Bool/to_str(Str/ends_with("lo", "héllo")),
            Bool/to_str(Str/starts_with("", "")),
            Bool/to_str(Str/ends_with("x", ""))
        ]))
        "#;

    assert_eq!(run(source), b"true false true false true true");
}

// `aaa` with `a` replaced by `aa` doubles once: the replacement is never rescanned, so the result is six characters and not an unbounded growth. An empty pattern occurs nowhere.
#[test]
fn replace_substitutes_every_occurrence_without_rescanning() {
    let source = r#"
        use /std/{Str};
        /std/print(Str/join(" ", [
            Str/replace("aaa", "a", "aa"),
            Str/replace("a-b-c", "-", "€"),
            Str/replace("abc", "", "x"),
            Str/replace("abc", "z", "x")
        ]))
        "#;

    assert_eq!(run(source), "aaaaaa a€b€c abc abc".as_bytes());
}

#[test]
fn padding_widens_to_the_count_and_trimming_one_end_keeps_the_other() {
    let source = r#"
        use /std/{Str};
        /std/print(Str/join("|", [
            Str/pad_start("7", 3, '0'),
            Str/pad_end("ab", 4, '.'),
            Str/pad_start("long", 2, '-'),
            Str/pad_start("é", 3, 'é'),
            Str/trim_start("  x  "),
            Str/trim_end("  x  "),
            Str/trim_start("   ")
        ]))
        "#;

    assert_eq!(run(source), "007|ab..|long|ééé|x  |  x|".as_bytes());
}

#[test]
fn to_list_yields_the_scalars_and_of_char_encodes_one() {
    let source = r#"
        use /std/{Str, List, Char, Nat, Bool};
        let chars = Str/to_list("a€😀");
        /std/print(Str/flatten([
            Nat/to_str(List/len(chars)), " ",
            Str/flatten(List/map(chars, Str/of_char)), " ",
            Bool/to_str(Str/of_char('€') == "€")
        ]))
        "#;

    assert_eq!(run(source), "3 a€😀 true".as_bytes());
}
