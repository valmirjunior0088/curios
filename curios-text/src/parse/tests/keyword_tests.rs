//! The words a declaration may not take as its name, the ones the grammar spells itself, and the contextual and retired words that stay identifiers.

use crate::*;

// The declaring and the referring side agree on what a name is: `parse_name` refuses a keyword segment in a path, so a declaration may not bind one either — bound, it names something no later reference can spell.
//
// The refusal is committed at a declaration's own name, which is what makes it the report. Uncommitted it lost `Parser::or`'s tie-break every time: this input reported through the struct-pattern alternative as `path 'match' contains a reserved keyword`.
#[test]
fn a_reserved_keyword_cannot_name_a_declaration() {
    let error = "let match : Nat = 1;".parse::<Module>().unwrap_err();
    assert!(
        error.format().contains("'match' is a reserved keyword"),
        "{}",
        error.format()
    );
}

#[test]
fn a_reserved_keyword_cannot_name_a_use_selector() {
    let error = "use /std/{end};".parse::<Module>().unwrap_err();
    assert!(
        error.format().contains("'end' is a reserved keyword"),
        "{}",
        error.format()
    );
}

// The refusal reaches every declaring form, not only the one it was found through.
#[test]
fn a_reserved_keyword_cannot_name_an_induct_a_struct_or_a_module() {
    for source in [
        "induct end : Type\nend",
        "struct use : Type { n : Type }",
        "mod true\nend",
    ] {
        let error = source.parse::<Module>().unwrap_err();
        assert!(
            error.format().contains("is a reserved keyword"),
            "{source}: {}",
            error.format()
        );
    }
}

// Every keyword-valued spelling the grammar does admit is parsed by an alternative earlier than the binder the refusal sits in, so it still parses.
#[test]
fn the_keywords_the_grammar_spells_itself_still_parse() {
    for source in [
        "let f : (Bool) -> Nat = (b) => match b | true => 1 | false => 0 end;",
        "let g : (Nat) -> Nat = (n) => match n | 0 => 0 | p + 1; ih => ih end;",
    ] {
        if let Err(error) = source.parse::<Module>() {
            panic!("{source}\n{}", error.format());
        }
    }
}

#[test]
fn record_is_an_identifier_and_legacy_declarations_are_not_grammar() {
    "let record : Type = Type; record"
        .parse::<Entrypoint>()
        .expect("record should be an ordinary identifier");
    assert!("record R : Type { Type } r".parse::<Entrypoint>().is_err());
}

#[test]
fn rec_is_an_ordinary_identifier() {
    // `rec` is no keyword, so it names a binding like any other word.
    let entrypoint = "let rec : Type = Type;\nrec".parse::<Entrypoint>().unwrap();
    assert!(matches!(&entrypoint.module.items[0], TopItem::Let(items) if items[0].label == "rec"));
}

#[test]
fn a_test_takes_no_pub_and_stays_a_contextual_word() {
    // The name is a report line, not an export.
    assert!("pub test t = Test/assert(true);".parse::<Module>().is_err());
    // `test` stays a contextual word everywhere else.
    assert!("let test : Type = Type;".parse::<Module>().is_ok());
    assert!("test(1)".parse::<Entrypoint>().is_ok());
    assert!(
        "let test : Nat = 1;\ntest(test)"
            .parse::<Entrypoint>()
            .is_ok()
    );
}
