//! A file of Core text formatted: laid out by the program's printer, its comments kept where they were written, and read without being checked.

use {
    super::{test_support::*, *},
    crate::{CoreFile, to_core},
    curios_common::Source,
};

/// The canonical text of `text` read as a `.crsc`.
fn core_formatted(text: &str) -> String {
    match Formatted::from_source(&Source::held("a.crsc", text)).expect("the file formats") {
        Formatted::Unchanged(text) | Formatted::Changed(text) => text,
    }
}

#[test]
fn every_form_of_core_text_is_laid_out_by_the_programs_printer() {
    let written = "\
#{absent = [/Box/mk]}
induct /Box(A: Type): pub Type
| #{params = [@Prop]} mk(x: A)
end
#{node = Rec}
and struct /Pair(A: Type): pub Type { fst: A, snd: A }
and /f : (n: #NatType) -> #NatType = (n: #NatType) => #NatAdd(n, 1);
#{module = /, universe_context = {parameter_count = 1, constraints = [u + 1 <= w]}}
let /p : #{level = u} Type = (#{node = Name} /f)(#{node = NumLit} 2);
test /held = 1;
let /m/witness~/std/ops/Add(Nat)#1 : (use w: /C()) -> #BoolType = /m/witness~3;
/Wit/w(#NatAdd(n, 1), #CellPoll(T, c, {levels = [u], qualities = [Param(0)]}), (#BoolType)(x), #{a = (x) => x} (y + 1))
: Prop
";

    assert_eq!(core_formatted(written), EVERY_FORM);
    assert_eq!(
        Formatted::from_source(&Source::held("a.crsc", EVERY_FORM)),
        Ok(Formatted::Unchanged(EVERY_FORM.to_string()))
    );
}

/// A decorator states the fields of the operand it is written before, a primary with its call chain: under a call the head is parenthesized, and a larger term under the decorator, so neither reads back as another node's fields.
#[test]
fn a_decorators_operand_keeps_the_parentheses_that_bound_it() {
    for text in [
        "(#{node = Name} /f)(x)\n",
        "#{node = Name} /f(x)\n",
        "#{a = 1} (x + 1)\n",
        "#{a = 1} ((x) => x)\n",
        "(#BoolType)(x)\n",
        "#{a = 1} (#{b = 2} x)\n",
    ] {
        assert_eq!(core_formatted(text), text);
    }
}

/// A comment is placed by the source text, as a program's is: one above an item stays above it, one riding a line stays on it, and one before the entry stays before it.
#[test]
fn a_comment_keeps_its_place_in_core_text() {
    let written = "\
-- Before the first item.
#{absent = [/Box/mk]}
induct /Box(A: Type): pub Type
| #{params = [@Prop]} mk(x: A) -- riding a case
end
let /p : Prop = /q; -- riding a definition
-- Before the entry.
/p
";

    assert_eq!(
        core_formatted(written),
        "\
-- Before the first item.
#{absent = [/Box/mk]}
induct /Box(A: Type): pub Type
| #{params = [@Prop]} mk(x: A) -- riding a case
end

let /p: Prop =
    /q; -- riding a definition

-- Before the entry.
/p
"
    );
}

/// The formatter reads a `.crsc` and checks nothing: a decorator setting a field no node holds is refused by the reader and laid out all the same.
#[test]
fn a_file_its_reader_refuses_is_formatted_all_the_same() {
    let written = "#{surplus = 1} let /x : Type = Type;\n";
    let file = written.parse::<CoreFile>().expect("the file parses");

    assert!(to_core(&file, None).is_err());
    assert_eq!(
        core_formatted(written),
        "#{surplus = 1}\nlet /x: Type =\n    Type;\n"
    );
}

/// Core text is told from a program by the path it was read from: the same text read from a `.crs` is a program, which writes no decorator.
#[test]
fn core_text_is_read_where_the_path_says_so() {
    let refusal = Formatted::from_source(&Source::held("a.crs", "#{a = 1} x\n"))
        .expect_err("a program writes no decorator");

    assert!(refusal.contains("obtained '#'"), "{refusal}");
}
