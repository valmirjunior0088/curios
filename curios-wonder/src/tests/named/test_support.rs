//! What the suites of `named` share: the question a program is asked at a place, and the rows its answers are read as.

use {
    crate::*, curios_core::Declared, curios_elab::DEFAULT_BUDGET, curios_module::Overlay,
    curios_surface::WrittenIn, std::path::Path,
};

/// What a check of `program` leaves of it, read as the editor's check reads a document, of standard input, through `overlay`.
pub fn asked(program: &str, overlay: &Overlay) -> Placed {
    crate::checked(
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
        overlay,
        None,
        Path::new("<stdin>"),
        &mut |_| {},
    )
    .placed
    .expect("the program lowers")
}

/// What stands at the `nth` occurrence of `written` in `program`, `within` bytes into it.
pub fn at(program: &str, written: &str, nth: usize, within: usize) -> Option<Answer> {
    let placed = asked(program, &Overlay::default());
    let (offset, _) = program
        .match_indices(written)
        .nth(nth)
        .expect("the program writes it");

    placed.at(offset + within)
}

/// What a row says of the declaration a name stands for: the path it is answered by, or a witness's head as written, what it is declared as, whether the name is where it is named, and the line and column its name is written at.
pub fn row(
    described: &Described,
    declares: bool,
) -> (&str, Declared, bool, Option<(usize, usize)>) {
    (
        described.asked.as_str(),
        described.declared,
        declares,
        described
            .declaration
            .as_ref()
            .and_then(Declaration::place)
            .map(|place| place.line_column()),
    )
}

/// What a row says of the declaration the name `answer` stands at names.
pub fn said(answer: &Answer) -> (&str, Declared, bool, Option<(usize, usize)>) {
    match &answer.stands {
        Stands::Declaration {
            declares,
            described,
            ..
        } => row(described, *declares),
        Stands::Local { name, .. } => panic!("a local stands there, `{name}`, and no declaration"),
        Stands::Field { label, .. } => {
            panic!("a field stands there, `{label}`, and no declaration")
        }
        Stands::Module { path, .. } => {
            panic!("a module stands there, `{path}`, and no declaration")
        }
        Stands::Term { written, .. } => panic!("a term stands there, `{written}`, and no name"),
    }
}

/// A program writing a word in each place elaboration resolves one: a structure's field labels in a literal, a projection and a pattern, a concept's method in a witness, and a constructor in a match.
pub const LABELLED: &str = "\
use /std/{Nat, Show, print};
struct Pair(Left: Type): Type {
    first: Left,
    second: Nat,
}
induct Shade: Type
| light()
| dark(Nat)
end
satisfy Show(Shade) {
    show(_) = \"shade\",
}
let made: Pair(Nat) = Pair { first = 1, second = 2 };
let read(held: Pair(Nat)) -> Nat = held.second;
let opened(held: Pair(Nat)) -> Nat =
    let Pair { first = inner, second = _ } = held;
    inner;
let toned(shade: Shade) -> Nat =
    match shade
    | light() => 0
    | dark(depth) => depth
    end;
print(\"\")";
