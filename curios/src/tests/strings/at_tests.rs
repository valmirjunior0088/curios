//! Positions in text: where a character may begin, and the text on either side of one — held to Rust's own reading of the same UTF-8.

use crate::tests::run;

/// Texts covering every width of character, the empty text, and a text beginning and ending on multi-byte characters.
const TEXTS: [&str; 5] = ["héllo", "日本語", "a𝄞b", "", "𝄞é"];

/// Spells `text` as a Curios literal: every character past ASCII by its `\u{…}` escape, so the source stays ASCII.
fn literal(text: &str) -> String {
    text.chars()
        .map(|c| match c.is_ascii() {
            true => c.to_string(),
            false => format!("\\u{{{:x}}}", c as u32),
        })
        .collect()
}

/// Whether a character may begin at each byte offset of each text, one past its end included, decided by `At/try_of_offset`: the one-byte rule agrees with `str::is_char_boundary` everywhere, and an offset past the end is no position.
#[test]
fn a_position_is_where_rust_places_a_char_boundary() {
    for text in TEXTS {
        let source = format!(
            r#"
            use /std/{{Str, Nat, List, Option}};
            use /std/Str/{{At}};

            let text: Str = "{}";
            let bit(k: Nat) -> Str =
                match At/try_of_offset(text, k) | some(_) => "1" | none() => "0" end;

            /std/print(Str/flatten(List/map(List/range(0, {}), bit)))
            "#,
            literal(text),
            text.len() + 2,
        );
        let expected = (0..text.len() + 2)
            .map(|k| match k <= text.len() && text.is_char_boundary(k) {
                true => '1',
                false => '0',
            })
            .collect::<String>();

        assert_eq!(
            String::from_utf8(run(&source)).unwrap(),
            expected,
            "{text:?}"
        );
    }
}

/// The text before a position and the text after it are Rust's `split_at` there, at every boundary of each text: a cut is where valid text divides into valid text, which `Str/Valid/before` and `after` prove.
#[test]
fn text_divides_at_a_position_as_rust_splits_it() {
    for text in TEXTS {
        let source = format!(
            r#"
            use /std/{{Str, Nat, List, Option}};
            use /std/Str/{{At}};

            let text: Str = "{}";
            let cut(k: Nat) -> Str =
                match At/try_of_offset(text, k)
                | some(at) => Str/flatten([Str/before(text, at), "|", Str/after(text, at), ";"])
                | none() => ""
                end;

            /std/print(Str/flatten(List/map(List/range(0, {}), cut)))
            "#,
            literal(text),
            text.len() + 1,
        );
        let expected = (0..=text.len())
            .filter(|&k| text.is_char_boundary(k))
            .map(|k| {
                let (before, after) = text.split_at(k);
                format!("{before}|{after};")
            })
            .collect::<String>();

        assert_eq!(
            String::from_utf8(run(&source)).unwrap(),
            expected,
            "{text:?}"
        );
    }
}
