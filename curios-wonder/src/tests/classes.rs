//! What `wonder --classes` and the server's semantic tokens answer: each span of a file the compiler classes, as what stands there, read off the check of its subject.

use {
    crate::*, curios_elab::DEFAULT_BUDGET, curios_module::Overlay, curios_surface::WrittenIn,
    std::path::Path,
};

/// One row of [`classes`]: the text a span covers, its class's name, whether what it names is declared there, and whether what it names is the library's the compiler was built with.
type Row = (String, &'static str, bool, bool);

/// Every span the check of `program`, read from standard input, classes ([`Row`]) — or the diagnostics of a program that has nothing to read.
fn classes(program: &str) -> Result<Vec<Row>, Vec<Diagnosis>> {
    let classed = classed(
        DEFAULT_BUDGET,
        Subject::Entry {
            units: Vec::new(),
            origin: Origin::Text {
                text: program.to_string(),
                written: WrittenIn::Program,
            },
            declares: None,
            package: None,
        },
        Path::new("<stdin>"),
        &Overlay::default(),
        None,
        &mut |_| {},
    )?;

    Ok(classed
        .into_iter()
        .map(|classed| {
            let span = &classed.span;
            (
                span.source.text()[span.start..span.end].to_string(),
                classed.class.name(),
                classed.declaration,
                classed.default_library,
            )
        })
        .collect())
}

/// One row of [`classes`], from what a test writes of it.
fn row(written: &str, class: &'static str, declaration: bool, library: bool) -> Row {
    (written.to_string(), class, declaration, library)
}

const CLASSED: &str = r#"induct Shape(A: Type): Type
| circle(radius: A)
end
struct Pair(A: Type): Type {
    first: A,
}
concept Sized(T: Type): Type {
    size: (value: T) -> /std/Nat,
}
let lift(F: (Type) -> Type, apply: (x: /std/Nat) -> /std/Nat, Pair { first = inner }: Pair(/std/Nat)) -> /std/Nat =
    let held = apply(inner);
    held;
let one: /std/Nat = lift(/std/List, (n) => n, Pair(/std/Nat) { first = 1 });
let round: Shape(/std/Nat) = Shape/circle(one);
let flipped: /std/Bool = /std/Bool/not(true);
/std/print("")"#;

/// Each span is classed as what stands there, in the order the file writes them and none overlapping another: a declaration's name by what it is declared as, each segment of a path on its own, a binder by the form that binds it — a type parameter where its stated type ends in a sort, `F: (Type) -> Type` among them, a destructured parameter's leaf a parameter, a `let`'s binder a local — a field's label and a method's name where they are declared and read, the contextual word that opens a concept the keyword; `declaration` where what a span names is declared there, and `default-library` where it lies under `/std` or `/sys`.
#[test]
fn each_span_is_classed_as_what_stands_there() {
    let declared = |written, class| row(written, class, true, false);
    let read = |written, class| row(written, class, false, false);
    let library = |written, class| row(written, class, false, true);

    assert_eq!(
        classes(CLASSED).expect("the program lowers"),
        [
            declared("Shape", "type"),
            declared("A", "type-parameter"),
            declared("circle", "constructor"),
            declared("radius", "parameter"),
            read("A", "type-parameter"),
            declared("Pair", "type"),
            declared("A", "type-parameter"),
            declared("first", "field"),
            read("A", "type-parameter"),
            read("concept", "keyword"),
            declared("Sized", "concept"),
            declared("T", "type-parameter"),
            declared("size", "method"),
            declared("value", "parameter"),
            read("T", "type-parameter"),
            library("std", "module"),
            library("Nat", "type"),
            declared("lift", "function"),
            declared("F", "type-parameter"),
            declared("apply", "parameter"),
            declared("x", "parameter"),
            library("std", "module"),
            library("Nat", "type"),
            library("std", "module"),
            library("Nat", "type"),
            read("Pair", "type"),
            read("first", "field"),
            declared("inner", "parameter"),
            read("Pair", "type"),
            library("std", "module"),
            library("Nat", "type"),
            library("std", "module"),
            library("Nat", "type"),
            declared("held", "local"),
            read("apply", "parameter"),
            read("inner", "parameter"),
            read("held", "local"),
            declared("one", "value"),
            library("std", "module"),
            library("Nat", "type"),
            read("lift", "function"),
            library("std", "module"),
            library("List", "type"),
            declared("n", "parameter"),
            read("n", "parameter"),
            read("Pair", "type"),
            library("std", "module"),
            library("Nat", "type"),
            read("first", "field"),
            declared("round", "value"),
            read("Shape", "type"),
            library("std", "module"),
            library("Nat", "type"),
            read("Shape", "type"),
            read("circle", "constructor"),
            read("one", "value"),
            declared("flipped", "value"),
            library("std", "module"),
            library("Bool", "type"),
            library("std", "module"),
            library("Bool", "module"),
            library("not", "function"),
            library("std", "module"),
            library("print", "function"),
        ]
    );
}

/// A local group's member is classed where it is read, as a `let`'s binder is: a sibling's read of it and the tail's are each a row.
#[test]
fn a_local_groups_member_is_classed_where_it_is_read() {
    let rows = classes(WORDS).expect("the program lowers");
    let members = rows
        .iter()
        .filter(|(written, ..)| written == "left" || written == "right")
        .map(|(written, class, declaration, _)| (written.as_str(), *class, *declaration))
        .collect::<Vec<_>>();

    assert_eq!(
        members,
        [
            ("left", "local", true),
            ("right", "local", false),
            ("right", "local", true),
            ("left", "local", false),
        ]
    );
}

const AGAIN: &str = "let twice(n: /std/Nat) -> /std/Nat = n + n;\n/std/print(\"\")";

/// A local read again is classed where it stands, as its first read is: each read of a parameter is a row of its own.
#[test]
fn a_local_read_again_is_classed_where_it_stands() {
    let read = row("n", "parameter", false, false);
    let reads = classes(AGAIN)
        .expect("the program lowers")
        .into_iter()
        .filter(|classed| *classed == read)
        .count();

    assert_eq!(reads, 2);
}

const WORDS: &str = r#"use /std/{Nat, Bool, Test};

concept Sized(T: Type): Type {
    size: (value: T) -> Nat,
}

satisfy Sized(Nat) {
    size(value) = value,
}

let even(n: Nat) -> Bool =
    match n
    | 0 => true
    | p + 1 => odd(p)
    end
and odd(n: Nat) -> Bool =
    match n
    | 0 => false
    | p + 1 => even(p)
    end;

let twice(n: Nat) -> Nat =
    let test = n;
    let left(k: Nat) -> Nat = right(k)
    and right(k: Nat) -> Nat = k;
    left(test) + test;

test twice_holds = Test/assert("twice", twice(1) == 2);

/std/print("")"#;

/// A contextual word is the keyword where it opened a member — `concept`, `satisfy` and `test` at an item's head, `and` before a later member of a top-level and of a local group — and a name elsewhere: the `test` a `let` binds is a local where it is bound and read.
#[test]
fn a_contextual_word_is_the_keyword_where_it_opened_a_member() {
    let rows = classes(WORDS).expect("the program lowers");
    let keywords = rows
        .iter()
        .filter(|(_, class, ..)| *class == "keyword")
        .map(|(written, ..)| written.as_str())
        .collect::<Vec<_>>();
    let tests = rows
        .iter()
        .filter(|(written, ..)| written == "test")
        .map(|(_, class, declaration, _)| (*class, *declaration))
        .collect::<Vec<_>>();

    assert_eq!(keywords, ["concept", "satisfy", "and", "and", "test"]);
    assert_eq!(
        tests,
        [
            ("local", true),
            ("local", false),
            ("local", false),
            ("keyword", false)
        ]
    );
}

/// A subject that does not lower has nothing to class: its refusal stands in the answer's place.
#[test]
fn a_subject_that_does_not_lower_classes_nothing() {
    assert!(classes("use /std/{Nope};\n/std/print(\"\")").is_err());
}
