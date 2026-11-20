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

/// Each span is classed as what stands there, in the order the file writes them and none overlapping another: a declaration's name by what it is declared as, each segment of a path on its own, a binder by the form that binds it — a type parameter where its stated type ends in a sort, `F: (Type) -> Type` among them, a destructured parameter's leaf a parameter, a `let`'s binder a local — a field's label and a method's name where they are declared and read; `declaration` where what a span names is declared there, and `default-library` where it lies under `/std` or `/sys`.
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

/// A subject that does not lower has nothing to class: its refusal stands in the answer's place.
#[test]
fn a_subject_that_does_not_lower_classes_nothing() {
    assert!(classes("use /std/{Nope};\n/std/print(\"\")").is_err());
}
