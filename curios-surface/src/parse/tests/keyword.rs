//! The words a declaration may not take as its name, a keyword or one that begins with a digit, the ones the grammar spells itself, and the contextual and retired words that stay identifiers.

use crate::*;

// The declaring and the referring side agree on what a name is: `parse_name` refuses a keyword segment in a path, so a declaration may not bind one either — bound, it names something no later reference can spell.
//
// The refusal is committed at a declaration's own name, which is what makes it the report. Uncommitted it lost `Parser::or`'s tie-break every time: this input reported through the struct-pattern alternative as `path 'match' contains a reserved keyword`.
#[test]
fn a_reserved_keyword_cannot_name_a_declaration() {
    let error = "let match : Nat = 1;".parse::<Module>().unwrap_err();
    assert!(
        error.format().contains("`match` is a reserved keyword"),
        "{}",
        error.format()
    );
}

#[test]
fn a_reserved_keyword_cannot_name_a_use_selector() {
    let error = "use /std/{end};".parse::<Module>().unwrap_err();
    assert!(
        error.format().contains("`end` is a reserved keyword"),
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

// A word that begins with a digit is read as a number wherever a term stands, so a declaration of one binds what no reference can spell, as a keyword's would.
#[test]
fn a_word_that_begins_with_a_digit_cannot_name_a_declaration() {
    for (source, word) in [
        ("let 5 = 3;", "5"),
        ("let 2x : Nat = 4;", "2x"),
        ("let 0x10 = 1;", "0x10"),
        ("induct 3d : Type\nend", "3d"),
        ("struct 1st : Type { n : Type }", "1st"),
        ("mod 2d\nend", "2d"),
        ("use /std/{5};", "5"),
    ] {
        let error = source.parse::<Module>().unwrap_err();
        assert!(
            error.format().contains(&format!(
                "`{word}` begins with a digit, so it cannot be a name"
            )),
            "{source}: {}",
            error.format()
        );
    }
}

// A binder is a name too, and so are a field's label and a constructor's: the refusal stands where each is read, a lambda's parameter held until its arrow says the list is one.
#[test]
fn a_word_that_begins_with_a_digit_cannot_be_a_binder_or_a_label() {
    for (source, word) in [
        ("let f : (Nat) -> Nat = (7) => 7 + 1;", "7"),
        ("let f : (Nat) -> Nat = (7: Nat) => 7 + 1;", "7"),
        ("let f : (Nat, Nat) -> Nat = (a, 2b) => a;", "2b"),
        ("let f(9: Nat) -> Nat => 9;", "9"),
        ("struct P : Type { 1st : Nat }", "1st"),
        ("induct T : Type | 2nd(Nat) end", "2nd"),
        ("let g : Nat = let 3 = 1; 2;", "3"),
    ] {
        let error = source.parse::<Module>().unwrap_err();
        assert!(
            error.format().contains(&format!(
                "`{word}` begins with a digit, so it cannot be a name"
            )),
            "{source}: {}",
            error.format()
        );
    }
}

// A parenthesized number is a term where no arrow follows it, which the held refusal of a lambda's parameter leaves alone.
#[test]
fn a_parenthesized_number_is_a_term_where_no_arrow_follows() {
    for source in ["let n : Nat = (7);", "let p : {Nat, Nat} = (7, 8);"] {
        if let Err(error) = source.parse::<Module>() {
            panic!("{source}\n{}", error.format());
        }
    }
}

#[test]
fn a_path_takes_no_segment_that_begins_with_a_digit() {
    for (source, path) in [
        ("let x : Nat = Shapes/2d/area;", "Shapes/2d/area"),
        ("use /std/2d/{x};", "std/2d"),
    ] {
        let error = source.parse::<Module>().unwrap_err();
        assert!(
            error.format().contains(&format!(
                "the path `{path}` contains a segment that begins with a digit"
            )),
            "{source}: {}",
            error.format()
        );
    }
}

// A digit inside a name, a number where a term or a dispatch pattern stands and a projection by index are what they were.
#[test]
fn a_digit_past_a_names_first_character_and_a_number_still_parse() {
    for source in [
        "let x1 : Nat = 5;",
        "let _1 : Nat = 0x10;",
        "let f : (Nat) -> Nat = (n2) => match n2 | 5 => 1 | _ => 0 end;",
        "let g : ({Nat, Nat}) -> Nat = (p) => p.0;",
        "let h : Nat = Shapes/d2/area;",
    ] {
        if let Err(error) = source.parse::<Module>() {
            panic!("{source}\n{}", error.format());
        }
    }
}

// Every keyword-valued spelling the grammar does admit is parsed by an alternative earlier than the binder the refusal sits in, so it still parses.
#[test]
fn the_keywords_the_grammar_spells_itself_still_parse() {
    for source in [
        "let f : (Bool) -> Nat = (b) => match b | true => 1 | false => 0 end;",
        "let g : (Nat) -> Nat = (n) => match n | 0 => 0 | p + 1 => g(p) end;",
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

/// A letter is a letter of the database the toolchain pins: U+0558, which Unicode 18.0.0 adds, names a binding and reads it back as any letter does.
#[test]
fn a_letter_the_pinned_database_adds_names_a_binding() {
    let entrypoint = "let a\u{558} : Type = Type;\na\u{558}"
        .parse::<Entrypoint>()
        .expect("U+0558 is a letter");
    assert!(
        matches!(&entrypoint.module.items[0], TopItem::Let(items) if items[0].label == "a\u{558}")
    );
}

#[test]
fn a_test_takes_no_pub_and_stays_a_contextual_word() {
    // The name is a report line, not an export.
    assert!(
        "pub test t => Test/assert(true);"
            .parse::<Module>()
            .is_err()
    );
    // `test` stays a contextual word everywhere else.
    assert!("let test : Type = Type;".parse::<Module>().is_ok());
    assert!("test(1)".parse::<Entrypoint>().is_ok());
    assert!(
        "let test : Nat = 1;\ntest(test)"
            .parse::<Entrypoint>()
            .is_ok()
    );
}

// A binder is read only by the terms in its scope, and every term reads a sort's word as the sort, so a binder spelled as one binds what nothing reads: it is refused at the word, wherever a binder is written.
#[test]
fn a_sort_cannot_name_a_binder() {
    for (source, word) in [
        ("let f(Type: Nat) -> Nat => 0;", "Type"),
        ("let f: (Nat) -> Nat = (Prop: Nat) => 0;", "Prop"),
        ("let f: (Sort: Nat) -> Nat = (x) => x;", "Sort"),
        ("let f: Nat = match 1 | Type => 0 end;", "Type"),
        ("let f: Nat = let a: Nat = 1 and Prop: Nat = 2; a;", "Prop"),
        ("induct Box: Type | box(Prop: Nat)\nend", "Prop"),
        ("induct Box(@Type: Type): Type | box()\nend", "Type"),
        (
            "induct Box: (Sort: Nat) -> Type | box(): Box(0)\nend",
            "Sort",
        ),
    ] {
        let error = source.parse::<Module>().unwrap_err();
        assert!(
            error.format().contains(&format!(
                "`{word}` is a sort wherever a term stands, so it cannot name a binder"
            )),
            "{source}: {}",
            error.format()
        );
    }
}

// The positions that read a binder's name through its `:` refuse a reserved word too, as a pattern's binder always has.
#[test]
fn a_reserved_keyword_cannot_name_a_binder_written_with_its_type() {
    for (source, word) in [
        ("let f: (match: Nat) -> Nat = (x) => x;", "match"),
        ("induct Box: Type | box(match: Nat)\nend", "match"),
        ("induct Box(@end: Type): Type | box()\nend", "end"),
    ] {
        let error = source.parse::<Module>().unwrap_err();
        assert!(
            error.format().contains(&format!(
                "`{word}` is a reserved keyword, so it cannot be a name"
            )),
            "{source}: {}",
            error.format()
        );
    }
}

// What a later read can still reach keeps a sort's word: a declaration's name by its path, and a field's label by its projection.
#[test]
fn a_sort_still_names_a_declaration_and_a_field() {
    for source in [
        "let Sort: Nat = 1;",
        "struct S: Type { Type: Nat }",
        "let s: Nat = /Sort;",
    ] {
        assert!(source.parse::<Module>().is_ok(), "{source}");
    }
}
