//! The editors' grammars and the syntax reference state the parser's words: every statement of a word outside the parser — each tree-sitter grammar, each TextMate grammar, each Zed query and `documentation/language.md` — is held to [`Keyword`], so a word the language adds reaches each of them or fails here.

use {
    curios_common::{Keyword, Standing, is_identifier},
    serde_json::Value,
    std::{collections::BTreeSet, fs, path::Path},
};

/// The tree-sitter grammars, each by its directory.
const TREE_SITTER: [&str; 2] = ["editors/grammar/base", "editors/grammar/core"];

/// The TextMate grammar of a program, which states every word, and Core text's, which includes it and states the words its own forms begin with.
const TEXTMATE: &str = "editors/vscode/syntaxes/curios.tmLanguage.json";
const TEXTMATE_CORE: &str = "editors/vscode/syntaxes/curios-core.tmLanguage.json";

/// The Zed queries, Core text's opening with the program's.
const QUERIES: [&str; 2] = [
    "editors/zed/languages/curios/highlights.scm",
    "editors/zed/languages/curios_core/highlights.scm",
];

/// The scopes a TextMate grammar paints a word in: a keyword, a boolean, a sort.
const WORD_SCOPES: [&str; 4] = [
    "keyword.control.curios",
    "keyword.other.curios",
    "constant.language.boolean.curios",
    "storage.type.sort.curios",
];

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

/// The words a TextMate grammar paints as words: those of each word group of every pattern that paints any of [`WORD_SCOPES`], whole or by a capture.
fn textmate_words(grammar: &Value) -> BTreeSet<String> {
    let paints_a_word = |pattern: &Value| {
        let scopes = every_value(pattern)
            .into_iter()
            .filter_map(Value::as_object)
            .filter_map(|fields| fields.get("name").and_then(Value::as_str));
        scopes.into_iter().any(|scope| WORD_SCOPES.contains(&scope))
    };
    every_value(grammar)
        .into_iter()
        .filter(|pattern| paints_a_word(pattern))
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

/// A query captures as a keyword every word its grammar reads alone, and leaves a boolean and a sort to the nodes the grammar reads them as.
#[test]
fn a_zed_query_captures_every_word_its_grammar_reads_alone_as_a_keyword() {
    for (query, grammar) in QUERIES.into_iter().zip(TREE_SITTER) {
        let text = read(query);
        let code = text
            .lines()
            .filter(|line| !line.trim_start().starts_with(';'))
            .collect::<Vec<_>>()
            .join("\n");
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
        let keywords = backticked(&list.replace('"', "`"));
        let grammar = json(&format!("{grammar}/src/grammar.json"));
        let read_as_nodes = &rule_strings(&grammar, "boolean") | &rule_strings(&grammar, "sort");

        assert!(keywords.is_disjoint(&read_as_nodes), "{query}");
        assert_eq!(&keywords | &read_as_nodes, words(|_| true), "{query}");
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
