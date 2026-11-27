//! Where each contextual word that opened a member was read: what the tree keeps of `and`, `test`, `concept` and `satisfy` where each stands as a word, the word alone, and of none where a reserved word opened the member.

use crate::*;

/// What each place in `words` covers of its text, `None` where no word opened the member.
fn spelled<'a>(words: impl IntoIterator<Item = &'a WordAt>) -> Vec<Option<String>> {
    words
        .into_iter()
        .map(|word| {
            word.span()
                .map(|span| span.source.text()[span.start..span.end].to_string())
        })
        .collect()
}

/// What `words` would read, from what a test writes of it.
fn read(words: &[Option<&str>]) -> Vec<Option<String>> {
    words.iter().map(|word| word.map(str::to_string)).collect()
}

const GROUPS: &str = r#"concept Sized(T: Type): Type { size: (T) -> Type }
and Shown(T: Type): Type { show: (T) -> Type }

satisfy Sized(Type) { size(t) => t }
and Shown(Type) { show(t) => t }

let one: Type = Type
and two: Type = Type;

induct A: Type
| a()
and B: Type
| b()
end

struct P: Type { x: Type }
and Q: Type { y: Type }

test holds => Type;
"#;

/// Each member keeps where the contextual word that opened it was read, the word alone: `concept`, `satisfy` and `test` at an item's head and `and` before a later member, and none where a reserved word, `let`, `induct` or `struct`, opened the first.
#[test]
fn each_member_keeps_where_its_contextual_word_was_read() {
    let module = GROUPS.parse::<Module>().unwrap();
    let words = module
        .items
        .iter()
        .map(|item| match item {
            TopItem::Concept(group) => spelled(group.iter().map(|member| &member.word)),
            TopItem::Witness(group) => spelled(group.iter().map(|member| &member.word)),
            TopItem::Let(group) => spelled(group.iter().map(|member| &member.word)),
            TopItem::Induct(group) => spelled(group.iter().map(|member| &member.word)),
            TopItem::Struct(group) => spelled(group.iter().map(|member| &member.word)),
            TopItem::Test(test) => spelled([&test.word]),
            item => panic!("no item of this kind is written: {item:?}"),
        })
        .collect::<Vec<_>>();

    assert_eq!(
        words,
        [
            read(&[Some("concept"), Some("and")]),
            read(&[Some("satisfy"), Some("and")]),
            read(&[None, Some("and")]),
            read(&[None, Some("and")]),
            read(&[None, Some("and")]),
            read(&[Some("test")]),
        ]
    );
}

/// A local group's later member keeps where its `and` was read, and a `test` a `let` binds is a name that opened nothing.
#[test]
fn a_local_and_is_kept_and_a_bound_test_is_no_word() {
    let module = "let value: Type =\n    let test = Type;\n    let a: Type = Type\n    and b: Type = a;\n    test;\n"
        .parse::<Module>()
        .unwrap();
    let [TopItem::Let(group)] = module.items.as_slice() else {
        panic!("one `let` item is written");
    };
    let LetSignature::Name { body, .. } = &group[0].signature else {
        panic!("the definition is a name's");
    };
    let Subterm::Let(local) = &**body else {
        panic!("the body is a local `let`");
    };
    let words = local
        .groups
        .iter()
        .map(|group| spelled(group.members.iter().map(|member| &member.word)))
        .collect::<Vec<_>>();

    assert_eq!(words, [read(&[None]), read(&[None, Some("and")])]);
}
