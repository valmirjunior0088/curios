//! The editors' grammars and the syntax reference state the parser's words, and the editors' grammars paint in the class set: every statement of a word outside the parser — each tree-sitter grammar, each TextMate grammar, each Zed query and `documentation/language.md` — is held to [`Keyword`], so a word the language adds reaches each of them or fails here; and every scope a TextMate grammar writes and every capture a Zed query writes stands for one [`Class`], so a scope or a capture added reaches a class or fails here.
//!
//! **`classes.json` is the TextMate grammars' table**, beside them in `editors/textmate/` and shipped with them: an object whose `classes` maps each class's name to the scopes the grammars write for it, and whose `unclassed` lists the scopes a grammar writes for a name it leaves to the server's semantic tokens. A page maps a token's scope to a class by it. The queries' captures are read by nothing but Zed's themes, so their classes are this test's own table ([`CAPTURES`]).

use {
    curios_common::{Class, Keyword, Standing, is_identifier},
    serde_json::Value,
    std::{
        collections::{BTreeMap, BTreeSet},
        fs,
        path::Path,
    },
};

/// The tree-sitter grammars, each by its directory.
const TREE_SITTER: [&str; 2] = ["editors/tree-sitter/base", "editors/tree-sitter/core"];

/// The TextMate grammar of a program, which states every word, and Core text's, which includes it and states the words its own forms begin with.
const TEXTMATE: &str = "editors/textmate/curios.tmLanguage.json";
const TEXTMATE_CORE: &str = "editors/textmate/curios-core.tmLanguage.json";

/// The TextMate grammars' table of classes.
const TABLE: &str = "editors/textmate/classes.json";

/// The Zed queries, Core text's opening with the program's.
const QUERIES: [&str; 2] = [
    "editors/zed/languages/curios/highlights.scm",
    "editors/zed/languages/curios_core/highlights.scm",
];

/// What each capture the queries write stands for, by the class's name: a capture Zed's themes paint, or one falling back by its prefix to the key they paint the class's text by today.
const CAPTURES: [(&str, &str); 24] = [
    ("comment", "comment"),
    ("comment.doc", "documentation-comment"),
    ("keyword", "keyword"),
    ("boolean", "boolean"),
    ("type.builtin", "sort"),
    ("number", "number"),
    ("string", "string"),
    ("string.special.symbol", "character"),
    ("string.escape", "escape"),
    ("operator", "operator"),
    ("punctuation", "punctuation"),
    ("punctuation.bracket", "punctuation"),
    ("punctuation.delimiter", "punctuation"),
    ("punctuation.goal", "goal"),
    ("attribute", "decorator"),
    ("module", "module"),
    ("type", "type"),
    ("constructor", "constructor"),
    ("type.interface", "concept"),
    ("function.method", "method"),
    ("function", "function"),
    ("function.builtin", "function"),
    ("constant", "value"),
    ("variable.member", "field"),
];

/// The captures a query writes for a name it leaves to the server's semantic tokens.
const UNCLASSED_CAPTURES: [&str; 1] = ["variable"];

/// The file at `path` from the repository's root.
fn read(path: &str) -> String {
    let file = Path::new(env!("CARGO_MANIFEST_DIR")).join("..").join(path);
    fs::read_to_string(&file).unwrap_or_else(|error| panic!("{path}: {error}"))
}

fn json(path: &str) -> Value {
    serde_json::from_str(&read(path)).unwrap_or_else(|error| panic!("{path}: {error}"))
}

/// The words whose standing `wanted` admits.
fn words(wanted: impl Fn(Standing) -> bool) -> BTreeSet<String> {
    Keyword::ALL
        .into_iter()
        .filter(|keyword| wanted(keyword.standing()))
        .map(|keyword| keyword.spelling().to_string())
        .collect()
}

/// The words of `class`.
fn words_of(class: Class) -> BTreeSet<String> {
    Keyword::ALL
        .into_iter()
        .filter(|keyword| keyword.class() == class)
        .map(|keyword| keyword.spelling().to_string())
        .collect()
}

/// Every value `root` holds, itself included, walked by a stack, as a grammar nests its patterns arbitrarily deep.
fn every_value(root: &Value) -> Vec<&Value> {
    let mut found = Vec::new();
    let mut stack = vec![root];
    while let Some(value) = stack.pop() {
        found.push(value);
        match value {
            Value::Array(items) => stack.extend(items),
            Value::Object(fields) => stack.extend(fields.values()),
            _ => {}
        }
    }
    found
}

/// Every scope a TextMate grammar writes: each pattern's and each capture's `name` and `contentName`.
fn scopes_of(grammar: &Value) -> Vec<String> {
    every_value(grammar)
        .into_iter()
        .filter_map(Value::as_object)
        .flat_map(|fields| ["name", "contentName"].map(|key| fields.get(key)))
        .flatten()
        .filter_map(Value::as_str)
        .filter(|scope| scope.contains('.'))
        .map(str::to_string)
        .collect()
}

/// The table's scopes of each class, by the class's name.
fn table() -> BTreeMap<String, Vec<String>> {
    let table = json(TABLE);
    let strings = |value: &Value| {
        value
            .as_array()
            .unwrap_or_else(|| panic!("{TABLE} lists scopes"))
            .iter()
            .map(|scope| scope.as_str().expect("a scope is a string").to_string())
            .collect::<Vec<_>>()
    };
    let mut classes = table["classes"]
        .as_object()
        .unwrap_or_else(|| panic!("{TABLE} maps its classes"))
        .iter()
        .map(|(name, scopes)| (name.clone(), strings(scopes)))
        .collect::<BTreeMap<_, _>>();
    classes.insert(String::new(), strings(&table["unclassed"]));
    classes
}

/// The scopes the table gives the words' classes: what a TextMate grammar paints a word in.
fn word_scopes() -> BTreeSet<String> {
    let table = table();
    [Class::Keyword, Class::Boolean, Class::Sort]
        .into_iter()
        .flat_map(|class| table[class.name()].clone())
        .collect()
}

/// The words a regular expression spells in a group of nothing but alternatives of words, `(let|match|end)` and `(and\s+induct)` alike: what a TextMate pattern matches a word by.
fn alternated_words(pattern: &str) -> BTreeSet<String> {
    let mut found = BTreeSet::new();
    for (open, _) in pattern.match_indices('(') {
        let Some(length) = pattern[open + 1..].find(')') else {
            continue;
        };
        let group = &pattern[open + 1..open + 1 + length];
        let group = group.strip_prefix("?:").unwrap_or(group);
        let alternatives = group
            .split('|')
            .map(|alternative| alternative.split("\\s+").collect::<Vec<_>>())
            .collect::<Vec<_>>();
        let worded = alternatives
            .iter()
            .flatten()
            .all(|word| !word.is_empty() && word.chars().all(|char| char.is_ascii_alphabetic()));
        if worded {
            found.extend(alternatives.into_iter().flatten().map(str::to_string));
        }
    }
    found
}

/// The words a TextMate grammar paints as words: those of each word group of every pattern that paints any scope of the words' classes, whole or by a capture.
fn textmate_words(grammar: &Value) -> BTreeSet<String> {
    let word_scopes = word_scopes();
    every_value(grammar)
        .into_iter()
        .filter(|pattern| {
            scopes_of(pattern)
                .iter()
                .any(|scope| word_scopes.contains(scope))
        })
        .filter_map(|pattern| pattern.get("match").and_then(Value::as_str))
        .flat_map(alternated_words)
        .collect()
}

/// The strings a rule of a tree-sitter grammar spells, read off its `grammar.json`.
fn rule_strings(grammar: &Value, rule: &str) -> BTreeSet<String> {
    every_value(&grammar["rules"][rule])
        .into_iter()
        .filter(|value| value["type"] == "STRING")
        .filter_map(|value| value["value"].as_str().map(str::to_string))
        .collect()
}

/// A query's text, its comment lines left out.
fn query_code(query: &str) -> String {
    read(query)
        .lines()
        .filter(|line| !line.trim_start().starts_with(';'))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Every backticked word of `text`.
fn backticked(text: &str) -> BTreeSet<String> {
    text.split('`')
        .skip(1)
        .step_by(2)
        .filter(|word| is_identifier(word))
        .map(str::to_string)
        .collect()
}

#[test]
fn a_tree_sitter_grammar_reads_every_word_and_no_other() {
    for grammar in TREE_SITTER {
        let types = json(&format!("{grammar}/src/node-types.json"));
        let tokens = every_value(&types)
            .into_iter()
            .filter(|value| value["named"] == false)
            .filter_map(|value| value["type"].as_str())
            .filter(|token| is_identifier(token))
            .map(str::to_string)
            .collect::<BTreeSet<_>>();

        assert_eq!(tokens, words(|_| true), "{grammar}");
    }
}

#[test]
fn a_textmate_grammar_paints_only_the_words_and_the_programs_paints_every_one() {
    let program = textmate_words(&json(TEXTMATE));
    assert_eq!(program, words(|_| true), "{TEXTMATE}");

    let core = textmate_words(&json(TEXTMATE_CORE));
    assert!(
        core.is_subset(&words(|_| true)),
        "{TEXTMATE_CORE} paints {:?}, which are no words",
        core.difference(&words(|_| true)).collect::<Vec<_>>()
    );
}

/// A query captures each word of the keyword class as a keyword, and leaves a boolean and a sort to the nodes its grammar reads them as.
#[test]
fn a_zed_query_captures_each_word_by_its_class() {
    for (query, grammar) in QUERIES.into_iter().zip(TREE_SITTER) {
        let code = query_code(query);
        let quoted = code
            .split('"')
            .skip(1)
            .step_by(2)
            .filter(|token| is_identifier(token))
            .map(str::to_string)
            .collect::<BTreeSet<_>>();
        assert!(
            quoted.is_subset(&words(|_| true)),
            "{query} names {:?}, which are no words",
            quoted.difference(&words(|_| true)).collect::<Vec<_>>()
        );

        let list = code
            .split("] @keyword")
            .next()
            .and_then(|before| before.rsplit('[').next())
            .unwrap_or_else(|| panic!("{query} captures a list as `@keyword`"));
        assert_eq!(
            backticked(&list.replace('"', "`")),
            words_of(Class::Keyword),
            "{query}"
        );

        let grammar = json(&format!("{grammar}/src/grammar.json"));
        assert_eq!(
            rule_strings(&grammar, "boolean"),
            words_of(Class::Boolean),
            "{query}"
        );
        assert_eq!(
            rule_strings(&grammar, "sort"),
            words_of(Class::Sort),
            "{query}"
        );
    }
}

#[test]
fn the_syntax_reference_states_the_reserved_the_contextual_and_the_sort_words() {
    let reference = read("documentation/language.md");
    let section = reference
        .split("### Identifiers")
        .nth(1)
        .and_then(|rest| rest.split("\n### ").next())
        .expect("`language.md` has an Identifiers section");

    let table = section
        .lines()
        .filter(|line| line.starts_with('|') && !line.starts_with("| ---"))
        .skip(1)
        .collect::<Vec<_>>()
        .join("\n");
    assert_eq!(
        backticked(&table),
        words(|standing| standing == Standing::Reserved)
    );

    let sentence = |phrase: &str| {
        let sentence = section
            .split(". ")
            .find(|sentence| sentence.contains(phrase))
            .unwrap_or_else(|| panic!("a sentence says the words {phrase}"));
        let before = &sentence[..sentence.find(phrase).unwrap_or_default()];
        backticked(before.rsplit('\n').next().unwrap_or_default())
    };
    assert_eq!(
        sentence("are contextual"),
        words(|standing| matches!(standing, Standing::ItemHead | Standing::GroupJoin))
    );
    assert_eq!(
        sentence("are sorts"),
        words(|standing| standing == Standing::Sort)
    );
}

#[test]
fn the_textmate_table_names_every_class_and_no_other() {
    let named = table()
        .into_keys()
        .filter(|name| !name.is_empty())
        .collect::<BTreeSet<_>>();
    let classes = Class::ALL
        .map(|class| class.name().to_string())
        .into_iter()
        .collect::<BTreeSet<_>>();

    assert_eq!(named, classes);
}

#[test]
fn every_scope_a_textmate_grammar_writes_stands_for_one_class() {
    let mut listed = BTreeMap::new();
    for (class, scopes) in table() {
        for scope in scopes {
            let earlier = listed.insert(scope.clone(), class.clone());
            assert!(earlier.is_none(), "{TABLE} lists `{scope}` twice");
        }
    }

    let written = [TEXTMATE, TEXTMATE_CORE]
        .into_iter()
        .flat_map(|grammar| scopes_of(&json(grammar)))
        .collect::<BTreeSet<_>>();
    let unlisted = written
        .iter()
        .filter(|scope| !listed.contains_key(*scope))
        .collect::<Vec<_>>();
    let unwritten = listed
        .keys()
        .filter(|scope| !written.contains(*scope))
        .collect::<Vec<_>>();

    assert!(
        unlisted.is_empty(),
        "{TABLE} lists no class for {unlisted:?}"
    );
    assert!(unwritten.is_empty(), "no grammar writes {unwritten:?}");
}

#[test]
fn every_capture_a_zed_query_writes_stands_for_one_class() {
    for (_, class) in CAPTURES {
        assert!(
            Class::ALL.iter().any(|known| known.name() == class),
            "`{class}` is no class"
        );
    }
    let captures = CAPTURES.map(|(capture, _)| capture);
    for (place, capture) in captures.iter().enumerate() {
        assert!(
            !captures[..place].contains(capture),
            "`@{capture}` is listed twice"
        );
    }

    for query in QUERIES {
        for capture in query_code(query)
            .split('@')
            .skip(1)
            .map(|rest| {
                rest.split(|char: char| {
                    !(char.is_ascii_alphanumeric() || char == '.' || char == '_')
                })
                .next()
                .unwrap_or_default()
            })
            .filter(|capture| !capture.is_empty())
        {
            assert!(
                captures.contains(&capture) || UNCLASSED_CAPTURES.contains(&capture),
                "{query} writes `@{capture}`, which stands for no class"
            );
        }
    }
}
