//! A form the grammar does not have, refused by what was read: two terms side by side, where the refusal stands and what follows a term without being one; a head or a form missing a part, named for the part; and a word another language reserves, named with the form written in its place.

use crate::*;

fn refusal(source: &str) -> String {
    source.parse::<Entrypoint>().unwrap_err().format()
}

#[test]
fn two_terms_on_one_line_are_refused_as_standing_side_by_side() {
    for source in [
        // In a `let`'s body, in an argument and in an arm's body.
        "let big(n : Nat) -> Nat = if n > 10 then 1 else 0;\nbig(1)",
        "let double(n : Nat) -> Nat = return n + n;\ndouble(1)",
        "let three : Nat = add 1 2;\nthree",
        "f(add 1 2)",
        "match b | true => add 1 2 | false => 0 end",
        // Glued, as another language indexes.
        "let first(xs : List(Nat)) -> Nat = xs[0];\nfirst([1])",
        // And past a program's final term on its own line, where a later line is the program's end to word.
        "fn double(n : Nat) -> Nat { n + n }",
    ] {
        let report = refusal(source);
        assert!(
            report.contains("two terms stand side by side"),
            "{source:?} reported {report}"
        );
    }
}

#[test]
fn the_refusal_of_two_terms_stands_at_the_second() {
    let error = "let three : Nat = add 1 2;\nthree"
        .parse::<Entrypoint>()
        .unwrap_err();

    assert_eq!(
        error.report().span.expect("located").line_column(),
        (1, 23),
        "{}",
        error.format()
    );
}

/// What follows a term and begins none is left alone: the next member of a `let` group, which `and` opens on the same line; a call's list after a blank; and a term on a later line, a sequence the form around it words.
#[test]
fn what_rightly_follows_a_term_is_not_a_second_term() {
    for source in [
        "let f(n : Nat) -> Nat = n and g(n : Nat) -> Nat = f(n);\ng(1)",
        "let f = (n) => n and g : Nat = f(1); g",
        "let x = 1; let y = f (x); y",
        "match b | true => 1 | false => 0 end",
    ] {
        if let Err(error) = source.parse::<Entrypoint>() {
            panic!("{source:?} stopped parsing: {}", error.format());
        }
    }

    let later = refusal("let a : Nat = 1\nlet b : Nat = 2;\nb");
    assert!(later.contains("Expected ';'"), "{later}");
}

/// A head or a form missing a part its grammar requires is refused in a sentence that names the part: a structure's and a concept's sort, a definition's result, the `=` a definition is written with, a lambda's parentheses, a constructor's payload list, and the bars of a `match`.
#[test]
fn a_form_missing_its_part_is_refused_by_naming_the_part() {
    for (source, said) in [
        (
            "struct Point { x : Nat, y : Nat }\n0",
            "a structure states its sort before its fields",
        ),
        (
            "pub concept Shape(A : Type) { area(A) -> Nat }\n0",
            "a concept states its sort before its fields",
        ),
        (
            "let double(n : Nat) = n + n;\ndouble(1)",
            "a definition with parameters states its result",
        ),
        ("let x : Nat := 3;\nx", "`:=` is no token of Curios"),
        ("let x := 3; x", "`:=` is no token of Curios"),
        (
            "List/map(xs, x => x + 1)",
            "a lambda's parameters are written in parentheses",
        ),
        (
            "let f = x => x + 1; f(1)",
            "a lambda's parameters are written in parentheses",
        ),
        (
            "induct Color : Type | red | green end\n0",
            "a constructor is declared with its payload list",
        ),
        (
            "match n { 0 => 1, _ => 2 }",
            "a `match` writes its arms after bars",
        ),
    ] {
        let report = refusal(source);
        assert!(report.contains(said), "{source:?} reported {report}");
    }
}

/// The braced `match` is refused only where no structure literal could be meant: a scrutinee that is one is still a scrutinee, a positional entry that is a lambda included.
#[test]
fn a_match_on_a_structure_literal_is_still_a_match() {
    for source in [
        "match Point { x = 1, y = 2 } | Point { x, y } => x end",
        "match Wrap { (x) => x } | Wrap { f } => f(1) end",
    ] {
        if let Err(error) = source.parse::<Entrypoint>() {
            panic!("{source:?} stopped parsing: {}", error.format());
        }
    }
}

/// A word another language reserves, standing in the refusal of two terms, is named with what Curios writes in its place, whether it ends the first term or begins the second. Each is a name like any other: a function declared under one and called compiles as it did.
#[test]
fn a_word_another_language_reserves_is_named_with_the_form_written_instead() {
    let leading = [
        "if", "return", "fn", "def", "fun", "function", "var", "const", "type", "enum", "data",
        "class", "import", "for", "while",
    ];
    let trailing = ["then", "else"];
    let written = leading
        .iter()
        .map(|word| (word, format!("let f(n : Nat) -> Nat = {word} n;\nf(1)")))
        .chain(
            trailing
                .iter()
                .map(|word| (word, format!("let f(n : Nat) -> Nat = n {word} 1;\nf(1)"))),
        );
    for (word, source) in written {
        let report = refusal(&source);
        assert!(
            report.contains("two terms stand side by side")
                && report.contains(&format!("`{word}` is no keyword of Curios")),
            "{source:?} reported {report}"
        );

        let declared = format!("let {word}(n : Nat) -> Nat = n;\n{word}(1)");
        if let Err(error) = declared.parse::<Entrypoint>() {
            panic!("{declared:?} stopped parsing: {}", error.format());
        }
    }

    // A refusal that names no such word says nothing of one.
    let plain = refusal("let three : Nat = add 1 2;\nthree");
    assert!(!plain.contains("is no keyword of Curios"), "{plain}");
}
