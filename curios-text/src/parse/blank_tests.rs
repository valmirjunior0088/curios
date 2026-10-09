//! Which characters separate tokens, and what a report says of one that does not.

use crate::*;

/// The four blanks separate tokens: a space, a tab, a newline and a carriage return.
#[test]
fn each_of_the_four_blanks_separates_tokens() {
    for blank in [' ', '\t', '\n', '\r'] {
        let source = format!("let{blank}x = 1;{blank}x");

        assert!(source.parse::<Entrypoint>().is_ok(), "{blank:?}");
    }
}

/// No other character Unicode calls white space is a blank, and neither is one that prints as nothing. Its reader sees a space there, or nothing, so each is refused where it stands and by its code point: after a keyword, ahead of a term, and behind an operator, which reads as spaced and is not.
#[test]
fn a_character_no_reader_sees_is_refused_by_its_code_point() {
    for (unseen, code) in [
        ('\u{00A0}', "U+00A0"),
        ('\u{2028}', "U+2028"),
        ('\u{3000}', "U+3000"),
        ('\u{200B}', "U+200B"),
        ('\u{FEFF}', "U+FEFF"),
    ] {
        for source in [
            format!("let{unseen}x = 1; x"),
            format!("let x = 1;{unseen}x"),
            format!("let x = 1 +{unseen}2; x"),
        ] {
            let report = source.parse::<Entrypoint>().unwrap_err().format();

            assert!(report.contains(code), "{source:?}: {report}");
        }
    }
}

/// A byte-order mark at the head of a file is passed over, in a program and in a module, and nowhere else: the same mark after the first position is the character the test above refuses.
#[test]
fn a_leading_byte_order_mark_is_passed_over() {
    assert!("\u{FEFF}let x = 1;\nx".parse::<Entrypoint>().is_ok());
    assert!("let x: Nat = 1;\n".parse::<Module>().is_ok());
    assert!("\u{FEFF}let x: Nat = 1;\n".parse::<Module>().is_ok());
    assert!(
        "\u{FEFF}\u{FEFF}let x = 1;\nx"
            .parse::<Entrypoint>()
            .is_err()
    );
}

/// Inside a literal and a comment a character is text, whatever it prints as.
#[test]
fn an_unseen_character_is_text_inside_a_literal_and_a_comment() {
    let source = "-- a no-break\u{00A0}space\nlet s = \"a\u{00A0}b\u{200B}\";\ns";

    assert!(source.parse::<Entrypoint>().is_ok());
}
