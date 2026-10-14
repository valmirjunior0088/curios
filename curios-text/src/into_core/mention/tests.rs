//! What a text names, held over one program that writes a name in every position the lowering resolves one.
//!
//! The program is written with a mark around each name the lowering does not resolve, `«name»`, and is read with the marks taken out. Every path it writes is then held to what its text mentions: one that ends in a declaration's name, and stands in no mark, is one mention of that declaration over the whole path, and every other is none.

use {
    super::super::{UnitSource, into_core_entry, test_support::syntax},
    crate::{Entrypoint, PreparedText, RootSource},
    std::{collections::BTreeMap, ops::Range},
};

/// A name in each position: a signature and a body, a module's member by its path and through a `use`, a type's parameter, payload, field and index, a concept's field, a witness's head and body, a literal's head bare and applied, a pattern's, a group whose members name one another, a definition that names itself, a test, and the term the program closes with.
const PROGRAM: &str = r#"
pub mod Shapes
    pub induct Colour: pub Type
    | red()
    | green()
    end

    pub let favourite: Colour = Colour/red();
end

use /Shapes/{«Colour», «favourite»};

induct Tree(Leafy: Type): pub Type
| tip()
| fork(Tree(Leafy), Leafy, Tree(Leafy))
end

induct Counted(Held: Type): (size: Tree(Colour)) -> pub Type
| bare(): (Tree/tip())
| more(@rest: Tree(Colour), Held, Counted(Held)(rest)): (Tree/fork(rest, favourite, rest))
end

struct Pairing(Lefty: Type): pub Type {
    first: Lefty,
    shade: Colour,
}

concept Painted(Canvas: Type): pub Type {
    paint(Canvas) -> Colour,
}

satisfy Painted(Colour) {
    «paint»(shown) = shown,
}

let twice(turn: (Colour) -> Colour, start: Colour) -> Colour =
    turn(turn(start));

let chosen: Colour = twice((given) => given, favourite);

let depth(grown: Tree(Colour)) -> Colour =
    match grown
    | «tip»() => chosen
    | «fork»(_, here, _) => Painted/paint(here)
    end;

let wrapped: Pairing(Colour) = «Pairing» { first = chosen, shade = /Shapes/favourite };

let stated: Pairing(Colour) = Pairing(Colour) { first = chosen, shade = favourite };

let opened(held: Pairing(Colour)) -> Colour =
    let «Pairing» { first = inner, shade = _ } = held;
    inner;

let again(count: Colour) -> Colour = again(count);

let even_step(count: Colour) -> Colour = odd_step(count)
and odd_step(count: Colour) -> Colour = even_step(count);

test stays_painted = depth(Tree/tip());

depth(Tree/fork(Tree/tip(), wrapped.shade, Tree/tip()))
"#;

/// `marked` with its marks taken out, and the ranges of the result they held.
fn unmarked(marked: &str) -> (String, Vec<Range<usize>>) {
    let mut text = String::new();
    let mut held = Vec::new();
    let mut opened = None;
    for character in marked.chars() {
        match character {
            '«' => opened = Some(text.len()),
            '»' => held.push(opened.take().expect("a mark closes one it opened")..text.len()),
            _ => text.push(character),
        }
    }

    (text, held)
}

/// Every path `text` writes, with where it begins: a run of identifier characters and `/`.
fn paths(text: &str) -> Vec<(usize, &str)> {
    let within =
        |character: char| character.is_alphanumeric() || character == '_' || character == '/';
    let mut paths = Vec::new();
    let mut rest = 0;
    while let Some(start) = text[rest..].find(within).map(|found| rest + found) {
        let end = text[start..]
            .find(|character| !within(character))
            .map_or(text.len(), |found| start + found);
        paths.push((start, &text[start..end]));
        rest = end;
    }

    paths
}

/// Lower `marked` over `baseline` and hold every path it writes to what it answers — see the module documentation.
fn answers(marked: &str, baseline: Option<&PreparedText>) -> PreparedText {
    let (text, silent) = unmarked(marked);
    let entrypoint = text.parse::<Entrypoint>().unwrap();
    let loader = RootSource::none();
    let (unit, entry) = into_core_entry(
        &UnitSource::entry(&entrypoint, &loader),
        &[],
        syntax(),
        baseline,
    )
    .unwrap();
    let minted = unit.minted();
    assert!(
        minted
            .declarations
            .iter()
            .chain([&minted.entry])
            .all(|mints| mints.unbound.is_empty()),
        "every name the program writes resolves"
    );

    let texts = unit.texts(Some(&entry), |_| true);
    let mut declared = BTreeMap::new();
    for mention in texts.iter().flat_map(|text| text.declares()) {
        if let Some(path) = mention.name.qualifier() {
            let earlier = declared.insert(path.last().to_string(), mention.name);
            assert_eq!(earlier, None, "`{}` names one declaration", path.last());
        }
    }
    let mentions = texts
        .iter()
        .flat_map(|text| text.mentions())
        .collect::<Vec<_>>();

    // Every disagreement is told at once: a name whose place the lowering lost is seldom alone.
    let mut disagreements = Vec::new();
    for (start, path) in paths(&text) {
        let end = start + path.len();
        let named = path.rsplit('/').next().expect("a path has a last segment");
        let expected = declared
            .get(named)
            .filter(|_| !silent.iter().any(|held| held.contains(&start)));
        let within = mentions
            .iter()
            .filter(|mention| start <= mention.span.start && mention.span.end <= end)
            .collect::<Vec<_>>();

        let agrees = match (expected, within.as_slice()) {
            (None, []) => true,
            (Some(expected), [mention]) => {
                let at = (mention.span.start, mention.span.end);
                let label = curios_core::Anchors::label(&[unit.anchors()], &mention.name);

                mention.name == *expected
                    && at == (start, end)
                    && mention.declared.is_some()
                        == label.is_some_and(|label| (label.start, label.end) == at)
            }
            _ => false,
        };
        if !agrees {
            let line = text[..start].lines().count();
            let found = within
                .iter()
                .map(|mention| {
                    let written = &text[mention.span.start..mention.span.end];

                    format!(
                        "{:?} over `{written}`, declared {:?}",
                        mention.name, mention.declared
                    )
                })
                .collect::<Vec<_>>();
            disagreements.push(format!(
                "`{path}` on line {line}: {found:?} where {expected:?} is named"
            ));
        }
    }
    assert!(disagreements.is_empty(), "{}", disagreements.join("\n"));

    unit
}

#[test]
fn a_written_name_answers_the_declaration_it_names() {
    answers(PROGRAM, None);
}

#[test]
fn a_declaration_kept_from_an_earlier_text_answers_where_it_now_stands() {
    let baseline = answers(PROGRAM, None);
    let moved = PROGRAM.replacen(
        "pub mod Shapes",
        "let before: Type = Type;\n\npub mod Shapes",
        1,
    );
    let unit = answers(&moved, Some(&baseline));

    assert!(
        !unit.kept_from(&baseline).is_empty(),
        "the declarations below the new one are the earlier lowering's"
    );
}

#[test]
fn a_witness_is_declared_by_its_head_and_a_member_by_its_name() {
    let (text, _) = unmarked(PROGRAM);
    let entrypoint = text.parse::<Entrypoint>().unwrap();
    let loader = RootSource::none();
    let (unit, entry) = into_core_entry(
        &UnitSource::entry(&entrypoint, &loader),
        &[],
        syntax(),
        None,
    )
    .unwrap();

    let declared = unit
        .texts(Some(&entry), |_| true)
        .iter()
        .flat_map(|text| text.declares())
        .map(|mention| {
            let written = &text[mention.span.start..mention.span.end];

            format!(
                "{:?} {written}",
                mention.declared.expect("a label declares")
            )
        })
        .collect::<Vec<_>>();

    assert_eq!(
        declared,
        [
            "Inductive Colour",
            "Constructor red",
            "Constructor green",
            "Authored favourite",
            "Inductive Tree",
            "Constructor tip",
            "Constructor fork",
            "Inductive Counted",
            "Constructor bare",
            "Constructor more",
            "Struct Pairing",
            "Concept Painted",
            "Method paint",
            "Witness Painted(Colour)",
            "Authored twice",
            "Authored chosen",
            "Authored depth",
            "Authored wrapped",
            "Authored stated",
            "Authored opened",
            "Authored again",
            "Authored even_step",
            "Authored odd_step",
            "Test stays_painted",
        ]
    );
}
