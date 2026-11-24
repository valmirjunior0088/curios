//! What a refusal says of where it stands and how it is printed.

use super::*;

const FILE: &str = "description = \"A\"\n[s]\nrun = [[\"{.tools.nope}\"]]\n";

// A refusal about a place names its line and its column from one, quotes the line and puts a caret under each character refused.
#[test]
fn a_refusal_about_a_place_quotes_its_line_with_a_caret_under_what_is_refused() {
    let start = FILE.find("\"{.tools").unwrap();
    let refusal = Refusal::at(
        "xtask/tasks/a.toml",
        FILE,
        start..start + "\"{.tools.nope}\"".len(),
        "`.tools.nope` names nothing".to_string(),
    );

    assert_eq!(
        refusal.rendered(false),
        "`.tools.nope` names nothing\n│\n│  3 │ run = [[\"{.tools.nope}\"]]\n│    │         ^^^^^^^^^^^^^^^\n│\n└─ [error] xtask/tasks/a.toml:3:9"
    );
}

// A refusal about a file as a whole names the file and quotes nothing.
#[test]
fn a_refusal_about_a_file_names_it_alone() {
    let refusal = Refusal::about("xtask/tasks/a.toml", "the file is no table".to_string());

    assert_eq!(
        refusal.rendered(false),
        "the file is no table\n│\n└─ [error] xtask/tasks/a.toml"
    );
}

// A span that runs past its line is shown as far as the line goes, an empty one as one character, and a column counts characters, so a letter of several bytes before the place moves it by one.
#[test]
fn a_place_is_counted_in_characters_on_the_line_it_starts_on() {
    let text = "é = [\n  1,\n]\n";
    let open = text.find('[').unwrap();

    let table = Refusal::at("a.toml", text, open..text.len(), "a list".to_string());
    let place = table.place.expect("the refusal is about a place");
    assert_eq!((place.line, place.column, place.width), (1, 5, 1));
    assert_eq!(place.text, "é = [");

    let empty = Refusal::at("a.toml", text, open..open, "nothing".to_string());
    assert_eq!(empty.place.expect("a place").width, 1);
}

// Where the stream takes colour, the message is bold and the carets and the tag are red; the text is otherwise the same.
#[test]
fn a_painted_refusal_holds_the_same_text() {
    let refusal = Refusal::about("a.toml", "wrong".to_string());

    let painted = refusal.rendered(true);

    assert_eq!(
        painted,
        "\u{1b}[1mwrong\u{1b}[0m\n│\n└─ \u{1b}[31m[error]\u{1b}[0m a.toml"
    );
}
