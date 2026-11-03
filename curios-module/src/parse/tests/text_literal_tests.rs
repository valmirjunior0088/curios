//! Character and string literals: their escapes, braced and refused, and the block string's lines.

use crate::*;

#[test]
fn char_literal_ascii() {
    assert_eq!(
        "'a'".parse::<Term>().unwrap(),
        Term::from(Subterm::ProofLiteral(ProofLiteral::Char('a')))
    );
}

#[test]
fn char_literal_escape() {
    assert_eq!(
        "'\\n'".parse::<Term>().unwrap(),
        Term::from(Subterm::ProofLiteral(ProofLiteral::Char('\n')))
    );
}

#[test]
fn char_literals_round_trip_unicode_and_supported_escapes() {
    for source in [
        "'a'", "'λ'", "'😀'", "'\\n'", "'\\t'", "'\\r'", "'\\''", "'\\\\'",
    ] {
        assert_eq!(source.parse::<Term>().unwrap().to_string(), source);
    }
}

#[test]
fn a_character_literal_is_a_dispatch_pattern() {
    // The former exclusion, overturned: a character pattern is a `Nat` dispatch case spelled by its scalar value, and it round-trips as written.
    let source = "match 97 | 'a' => 0 | '\\n' => 1 | _ => 2 end";
    let term = source.parse::<Term>().unwrap();
    let Subterm::Match(match_) = term.as_subterm() else {
        panic!("expected a match");
    };
    assert!(
        matches!(match_.arms[0].pattern, MatchPattern::Char('a')),
        "{:?}",
        match_.arms[0].pattern
    );
    assert!(
        matches!(match_.arms[1].pattern, MatchPattern::Char('\n')),
        "{:?}",
        match_.arms[1].pattern
    );
    assert_eq!(term.to_string(), source);
}

#[test]
fn string_literal_is_str() {
    assert_eq!(
        "\"a\"".parse::<Term>().unwrap(),
        Term::from(Subterm::ProofLiteral(ProofLiteral::Str(StrLit::line("a"))))
    );
}

#[test]
fn unrecognized_string_escape_is_literal_backslash_and_char() {
    assert_eq!(
        "\"\\%\"".parse::<Term>().unwrap(),
        Term::from(Subterm::ProofLiteral(ProofLiteral::Str(StrLit::line(
            "\\%"
        ))))
    );
}

#[test]
fn a_braced_unicode_escape_names_a_scalar_in_a_string() {
    // `\u{…}` is the one way to spell a scalar without pasting it; a combining mark is the case that needs it.
    for (source, expected) in [
        ("\"\\u{65}\"", "e"),
        ("\"e\\u{301}\"", "e\u{301}"),
        ("\"\\u{1F600}\"", "😀"),
        ("\"\\u{0}\"", "\0"),
    ] {
        assert_eq!(
            source.parse::<Term>().unwrap(),
            Term::from(Subterm::ProofLiteral(ProofLiteral::Str(StrLit::line(
                expected
            )))),
            "{source}"
        );
    }
}

#[test]
fn a_braced_unicode_escape_names_a_scalar_in_a_character() {
    assert_eq!(
        "'\\u{301}'".parse::<Term>().unwrap(),
        Term::from(Subterm::ProofLiteral(ProofLiteral::Char('\u{301}')))
    );
    // The printer writes a visible scalar itself, so its escape has no round trip of its own: one scalar, one spelling.
    assert_eq!("'\\u{65}'".parse::<Term>().unwrap().to_string(), "'e'");
    assert_eq!("\"\\u{65}\"".parse::<Term>().unwrap().to_string(), "\"e\"");
}

/// A control character's one spelling is its braced escape, in a character literal, a string and a match pattern alike: written verbatim it would be a byte no reader sees, which is what `curios format` made of `'\u{8}'` before.
#[test]
fn a_control_character_prints_as_its_braced_escape() {
    for source in [
        "'\\u{0}'",
        "'\\u{8}'",
        "'\\u{C}'",
        "'\\u{1B}'",
        "'\\u{7F}'",
        "'\\u{9F}'",
        "\"a\\u{8}b\\u{C}\"",
        "match 8 | '\\u{8}' => 0 | _ => 1 end",
    ] {
        assert_eq!(source.parse::<Term>().unwrap().to_string(), source);
    }
    // The digits print in the numerals' case, however the escape was written.
    assert_eq!(
        "'\\u{1b}'".parse::<Term>().unwrap().to_string(),
        "'\\u{1B}'"
    );
}

#[test]
fn a_backslash_u_without_a_brace_still_stands_for_itself() {
    // Only the brace reserves the form: the break is confined to source that spelled `\u{`.
    for (source, expected) in [
        ("\"\\u\"", "\\u"),
        ("\"\\ux\"", "\\ux"),
        ("\"\\u {41}\"", "\\u {41}"),
    ] {
        assert_eq!(
            source.parse::<Term>().unwrap(),
            Term::from(Subterm::ProofLiteral(ProofLiteral::Str(StrLit::line(
                expected
            )))),
            "{source}"
        );
    }
}

#[test]
fn a_malformed_braced_escape_is_refused() {
    // Once `\u{` is read the form is committed: an empty brace, a non-hex digit, a surrogate, a value past the last scalar, a seventh digit, or a missing brace is the diagnosis rather than a fallback, in a string and in a character alike.
    for source in [
        "\"\\u{}\"",
        "\"\\u{zz}\"",
        "\"\\u{D800}\"",
        "\"\\u{110000}\"",
        "\"\\u{0000041}\"",
        "\"\\u{41\"",
        "'\\u{}'",
        "'\\u{D800}'",
        "'\\u{41'",
    ] {
        assert!(source.parse::<Term>().is_err(), "{source}");
    }
    // The fault is named: a digit count, or a value that is no scalar — a reader told to write one to six digits when they wrote four would only count them again.
    let report = "'\\u{D800}'".parse::<Term>().unwrap_err().format();
    assert!(
        report.contains("\\u{D800} names no Unicode scalar: a surrogate"),
        "reported {report}"
    );
    let report = "'\\u{0000041}'".parse::<Term>().unwrap_err().format();
    assert!(
        report.contains("takes one to six hex digits") && !report.contains("scalar"),
        "reported {report}"
    );
}

#[test]
fn an_unrecognized_character_escape_is_refused_by_name() {
    // The backslash discriminates the escape, so what follows it is the diagnosis. Uncommitted, the alternative below it reads the backslash as an ordinary character and the closing quote takes the blame for the reader's mistyped escape. A string literal is the opposite case, above: there an unrecognized escape stands for itself.
    let report = "'\\q'".parse::<Term>().unwrap_err().format();
    assert!(
        report.contains("a character escape is one of"),
        "reported {report}"
    );
}

#[test]
fn char_literal_multi_char_is_error() {
    assert!("'ab'".parse::<Term>().is_err());
}

#[test]
fn char_literal_empty_is_error() {
    assert!("''".parse::<Term>().is_err());
}

fn block(value: &str) -> Term {
    Term::from(Subterm::ProofLiteral(ProofLiteral::Str(StrLit::block(
        value,
    ))))
}

/// A block's value is the lines between its delimiters with the indentation they share with the closer removed: a block reads at the indentation of the code around it.
#[test]
fn a_block_string_is_the_lines_between_its_delimiters_dedented() {
    for (source, expected) in [
        (
            "\"\"\"\n    <ul>\n      <li>\n    </ul>\n    \"\"\"",
            "<ul>\n  <li>\n</ul>",
        ),
        // Content indented past the closer keeps the difference: the closer's column is the dial.
        ("\"\"\"\n        a\n        b\n    \"\"\"", "    a\n    b"),
        // A whitespace-only line becomes an empty line and takes no part in the prefix.
        ("\"\"\"\n    a\n  \n    b\n    \"\"\"", "a\n\nb"),
        // Trailing whitespace is stripped; `\u{20}` is translated after the stripping and stays.
        ("\"\"\"\n    a   \n    b\\u{20}\n    \"\"\"", "a\nb "),
        // A backslash before a newline joins the lines, keeping the space before it.
        ("\"\"\"\n    a \\\n    b\n    \"\"\"", "a b"),
        // An escape at a line's start ends its leading whitespace, so one escape keeps every line's indentation.
        (
            "\"\"\"\n    \\u{20}   a\n        b\n    \"\"\"",
            "    a\n    b",
        ),
        // A quote is itself; three are spelled with an escape.
        (
            "\"\"\"\n    say \"hi\" and \"\"\\\"\n    \"\"\"",
            "say \"hi\" and \"\"\"",
        ),
        // Nothing between the delimiters is the empty string.
        ("\"\"\"\n\"\"\"", ""),
    ] {
        assert_eq!(source.parse::<Term>().unwrap(), block(expected), "{source}");
    }
}

/// The delimiters own their newlines, so a closer sharing a line with content and an opener followed by content are each refused by name, and a one-line literal that spans lines is sent to the block form.
#[test]
fn a_string_that_spans_lines_is_a_block_or_is_refused() {
    for (source, expected) in [
        ("\"\"\"\n    a\"\"\"", "closes with a newline"),
        ("\"\"\"a\n\"\"\"", "opens with `\"\"\"` and a newline"),
        ("\"a\nb\"", "ends before its line does"),
    ] {
        let report = source.parse::<Term>().unwrap_err().format();
        assert!(report.contains(expected), "{source} reported {report}");
    }
}

/// A block prints as a block: its lines one level in from the opener, the closer at that level, and what a reader would strip, mistake or close on escaped — so the printed text reads back as the value it prints.
#[test]
fn a_block_string_prints_as_a_block_that_reads_back() {
    for (value, printed) in [
        ("a\nb", "\"\"\"\n    a\n    b\n    \"\"\""),
        // Shared leading whitespace sits right of the closer, which is exactly what the reader keeps.
        ("  a\n  b", "\"\"\"\n      a\n      b\n    \"\"\""),
        // A trailing space, a tab and a third consecutive quote are escaped; a lone quote is not.
        (
            "a \n\t\"b\"\n\"\"\"\"",
            "\"\"\"\n    a\\u{20}\n    \\t\"b\"\n    \"\"\\\"\"\n    \"\"\"",
        ),
        // An empty line prints empty, and a whitespace-only one as escapes.
        (
            "a\n\n \nb",
            "\"\"\"\n    a\n\n    \\u{20}\n    b\n    \"\"\"",
        ),
        // A control character prints as its braced escape, as in the one-line form.
        ("a\u{8}b", "\"\"\"\n    a\\u{8}b\n    \"\"\""),
    ] {
        assert_eq!(block(value).to_string(), printed, "{value:?}");
        assert_eq!(printed.parse::<Term>().unwrap(), block(value), "{printed}");
    }
}
