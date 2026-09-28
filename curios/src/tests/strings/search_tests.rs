//! Searching text and cutting it where a search lands: each walk held to Rust's reading of the same UTF-8.

use crate::tests::run;

/// Texts mixing every width of character, whitespace at either end, line endings of both kinds, and a needle repeated.
const TEXTS: [&str; 7] = [
    "héllo wörld",
    "日本語 日本",
    "a𝄞b𝄞c",
    "",
    "  é\t",
    "x\r\ny\nz\n",
    "ab𝄞ab𝄞",
];

/// Needles beginning on every width of character, one spanning a space, and one absent from most texts.
const NEEDLES: [&str; 6] = ["é", "𝄞", "日本", "ab", "o w", "z"];

/// Spells `text` as a Curios literal: every character past ASCII, and the control characters, by its `\u{…}` escape, so the source stays one ASCII line.
fn literal(text: &str) -> String {
    text.chars()
        .map(|c| match c.is_ascii() && !c.is_ascii_control() {
            true => c.to_string(),
            false => format!("\\u{{{:x}}}", c as u32),
        })
        .collect()
}

/// A Curios list literal of `texts`.
fn list(texts: &[&str]) -> String {
    let items = texts
        .iter()
        .map(|text| format!("\"{}\"", literal(text)))
        .collect::<Vec<_>>()
        .join(", ");

    format!("[{items}]")
}

/// Where a character, a character a predicate accepts, and a run of characters first occur, as the byte offset of the position each search answers: Rust's `str::find` over the same text, and nothing where Rust finds nothing.
#[test]
fn a_search_answers_where_rust_finds() {
    let source = format!(
        r#"
        use /std/{{Str, Nat, List, Option, Char}};

        let offset(@s: Str, found: Option(Str/At(s))) -> Str =
            match found | some(at) => Nat/to_str(Str/At/to_offset(at)) | none() => "-" end;
        let row(text: Str) -> Str =
            Str/flatten([
                offset(Str/index_of(text, 'b')),
                ",",
                offset(Str/find_index(text, (c) => Char/to_nat(c) > 0x7F)),
                ",",
                Str/join(",", List/map({needles}, (needle) => offset(Str/index_of_substr(text, needle)))),
                ";",
            ]);

        /std/print(Str/flatten(List/map({texts}, row)))
        "#,
        texts = list(&TEXTS),
        needles = list(&NEEDLES),
    );
    let offset = |found: Option<usize>| match found {
        Some(k) => k.to_string(),
        None => "-".to_owned(),
    };
    let expected = TEXTS
        .iter()
        .map(|text| {
            let needles = NEEDLES
                .iter()
                .map(|needle| offset(text.find(needle)))
                .collect::<Vec<_>>()
                .join(",");

            format!(
                "{},{},{needles};",
                offset(text.find('b')),
                offset(text.find(|c: char| !c.is_ascii())),
            )
        })
        .collect::<String>();

    assert_eq!(String::from_utf8(run(&source)).unwrap(), expected);
}

/// Text cut where its searches land reads as Rust's cuts of the same text: `split_once`, `split`, `strip_prefix`, `strip_suffix`, `trim_start`, `trim_end`, `trim`, `lines` and `replace`, each over every text and needle.
#[test]
fn text_cuts_where_rust_cuts_it() {
    let source = format!(
        r#"
        use /std/{{Str, List, Option}};

        let piece(found: Option(Str)) -> Str =
            match found | some(s) => Str/concat(s, "|") | none() => "-|" end;
        let pieces(parts: List(Str)) -> Str =
            Str/concat(Str/join("/", parts), "|");
        let cuts(text: Str, needle: Str) -> Str =
            Str/flatten([
                match Str/split_once(text, needle) | some((a, b)) => Str/flatten([a, "/", b, "|"]) | none() => "-|" end,
                pieces(Str/split(text, needle)),
                piece(Str/strip_prefix(text, needle)),
                piece(Str/strip_suffix(text, needle)),
                Str/replace(text, needle, "_"),
                ";",
            ]);
        let row(text: Str) -> Str =
            Str/flatten([
                Str/trim_start(text), "|", Str/trim_end(text), "|", Str/trim(text), "|",
                pieces(Str/lines(text)),
                Str/flatten(List/map({needles}, (needle) => cuts(text, needle))),
                "\n",
            ]);

        /std/print(Str/flatten(List/map({texts}, row)))
        "#,
        texts = list(&TEXTS),
        needles = list(&NEEDLES),
    );
    let piece = |found: Option<&str>| match found {
        Some(s) => format!("{s}|"),
        None => "-|".to_owned(),
    };
    let expected = TEXTS
        .iter()
        .map(|text| {
            let cuts = NEEDLES
                .iter()
                .map(|needle| {
                    let once = match text.split_once(needle) {
                        Some((a, b)) => format!("{a}/{b}|"),
                        None => "-|".to_owned(),
                    };

                    format!(
                        "{once}{}|{}{}{};",
                        text.split(needle).collect::<Vec<_>>().join("/"),
                        piece(text.strip_prefix(needle)),
                        piece(text.strip_suffix(needle)),
                        text.replace(needle, "_"),
                    )
                })
                .collect::<String>();

            format!(
                "{}|{}|{}|{}|{cuts}\n",
                text.trim_start(),
                text.trim_end(),
                text.trim(),
                text.lines().collect::<Vec<_>>().join("/"),
            )
        })
        .collect::<String>();

    assert_eq!(String::from_utf8(run(&source)).unwrap(), expected);
}
