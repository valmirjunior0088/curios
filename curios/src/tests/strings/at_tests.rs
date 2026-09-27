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

/// Whether a character may begin at each byte offset of each text, one past its end included, decided by `Str/At/try_of_offset`: the one-byte rule agrees with `str::is_char_boundary` everywhere, and an offset past the end is no position.
#[test]
fn a_position_is_where_rust_places_a_char_boundary() {
    for text in TEXTS {
        let source = format!(
            r#"
            use /std/{{Str, Nat, List, Option}};

            let text: Str = "{}";
            let bit(k: Nat) -> Str =
                match Str/At/try_of_offset(text, k) | some(_) => "1" | none() => "0" end;

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

            let text: Str = "{}";
            let cut(k: Nat) -> Str =
                match Str/At/try_of_offset(text, k)
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

/// Stepping forward from the start with `Str/At/try_next`, and back from the end with `try_prev`, reads each character at the offset `char_indices` places it, and ends where the text does: the two directions agree with Rust and so with each other.
#[test]
fn stepping_reads_each_character_where_rust_does() {
    for text in TEXTS {
        let source = format!(
            r#"
            use /std/{{Str, Nat, Option, Char}};

            let text: Str = "{}";
            let mark(at: Str/At(text), char: Char) -> Str =
                Str/flatten([Nat/to_str(Str/At/to_offset(at)), ":", Str/of_char(char), " "]);
            let forward(at: Str/At(text)) -> Str =
                match Str/At/try_next(text, at)
                | some(n) => Str/concat(mark(at, n.char), forward(n.past))
                | none() => Str/concat(Nat/to_str(Str/At/to_offset(at)), ";")
                end;
            let backward(at: Str/At(text)) -> Str =
                match Str/At/try_prev(text, at)
                | some(p) => Str/concat(mark(p.past, p.char), backward(p.past))
                | none() => Str/concat(Nat/to_str(Str/At/to_offset(at)), ";")
                end;

            /std/print(Str/concat(forward(Str/At/start(text)), backward(Str/At/finish(text))))
            "#,
            literal(text),
        );
        let marks = |indices: &mut dyn Iterator<Item = (usize, char)>| {
            indices
                .map(|(k, c)| format!("{k}:{c} "))
                .collect::<String>()
        };
        let expected = format!(
            "{}{};{}0;",
            marks(&mut text.char_indices()),
            text.len(),
            marks(&mut text.char_indices().rev()),
        );

        assert_eq!(
            String::from_utf8(run(&source)).unwrap(),
            expected,
            "{text:?}"
        );
    }
}

/// `Str/between` two positions is Rust's slice between the same offsets, at every ordered pair of boundaries of each text; the guard `from <= to` over positions is what discharges the order it takes.
#[test]
fn text_between_two_positions_is_rusts_slice() {
    for text in TEXTS {
        let source = format!(
            r#"
            use /std/{{Str, Nat, List, Option}};

            let text: Str = "{}";
            let positions: List(Str/At(text)) =
                List/filter_map(List/range(0, {}), (k) => Str/At/try_of_offset(text, k));
            let cut(from: Str/At(text), to: Str/At(text)) -> Str =
                match from <= to
                | true => Str/concat(Str/between(text, from, to), ";")
                | false => ""
                end;

            /std/print(Str/flatten(List/map(positions, (from) => Str/flatten(List/map(positions, (to) => cut(from, to))))))
            "#,
            literal(text),
            text.len() + 1,
        );
        let boundaries = (0..=text.len())
            .filter(|&k| text.is_char_boundary(k))
            .collect::<Vec<_>>();
        let expected = boundaries
            .iter()
            .flat_map(|&from| {
                boundaries
                    .iter()
                    .filter(move |&&to| from <= to)
                    .map(move |&to| format!("{};", &text[from..to]))
            })
            .collect::<String>();

        assert_eq!(
            String::from_utf8(run(&source)).unwrap(),
            expected,
            "{text:?}"
        );
    }
}

/// A position in either operand of a concatenation is one in the whole, at every boundary of every pair of texts: `Str/At/of_left` keeps its offset and `of_right` moves it past the left operand, and the text after each is Rust's.
#[test]
fn a_position_keeps_its_place_across_a_concatenation() {
    let texts = TEXTS
        .iter()
        .map(|text| format!("\"{}\"", literal(text)))
        .collect::<Vec<_>>()
        .join(", ");
    let source = format!(
        r#"
        use /std/{{Str, Nat, List, Option, Bytes}};

        let texts: List(Str) = [{texts}];
        let offsets(s: Str) -> List(Nat) = List/range(0, Bytes/len(s.bytes) + 1);
        let pair(left: Str, right: Str) -> Str =
            let whole = Str/concat(left, right);
            let rest(at: Str/At(whole)) -> Str =
                Str/flatten([Nat/to_str(Str/At/to_offset(at)), ":", Str/after(whole, at), ";"]);
            let of_left(k: Nat) -> Str =
                match Str/At/try_of_offset(left, k) | some(at) => rest(Str/At/of_left(left, right, at)) | none() => "" end;
            let of_right(k: Nat) -> Str =
                match Str/At/try_of_offset(right, k) | some(at) => rest(Str/At/of_right(left, right, at)) | none() => "" end;
            Str/flatten([
                Str/flatten(List/map(offsets(left), of_left)),
                Str/flatten(List/map(offsets(right), of_right)),
                "\n",
            ]);

        /std/print(Str/flatten(List/map(texts, (left) => Str/flatten(List/map(texts, (right) => pair(left, right))))))
        "#,
    );
    let expected = TEXTS
        .iter()
        .flat_map(|left| TEXTS.iter().map(move |right| (left, right)))
        .map(|(left, right)| {
            let whole = format!("{left}{right}");
            (0..=left.len())
                .filter(|&k| left.is_char_boundary(k))
                .chain(
                    (0..=right.len())
                        .filter(|&k| right.is_char_boundary(k))
                        .map(|k| left.len() + k),
                )
                .map(|k| format!("{k}:{};", &whole[k..]))
                .chain(["\n".to_owned()])
                .collect::<String>()
        })
        .collect::<String>();

    assert_eq!(String::from_utf8(run(&source)).unwrap(), expected);
}
