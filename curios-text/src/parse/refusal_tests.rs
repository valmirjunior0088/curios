//! A form the grammar does not have, refused by what was read: two terms side by side, where the refusal stands, and what follows a term without being one.

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
