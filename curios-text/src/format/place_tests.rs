//! Where a comment lands. The law is that one stays where it was written — on a line of its own between the two tokens it stood between, or on the line of the token it followed — and that a second run changes nothing. It is put to every position a comment can be written in.

use {
    super::{
        Formatted,
        test_support::{collect_sources, formatted},
    },
    curios_utilities::Source,
    std::path::Path,
};

/// The comment a position is tried with.
const MARKER: &str = "-- MARK";

/// Every syntactic position a comment can be written in, one fixture each: a `let` signature over a `choose`, documentation comments, an `induct` and a `struct`, a `match` with a local `let`, an `induct` of bare cases, a `concept` body, a `satisfy` body, the expressions that break over lines, and the declarations that do.
///
/// Unbound names are deliberate — formatting is syntax-only, so these need to parse and nothing more.
const POSITIONS: [&str; 9] = [
    "use /std/{Bool, Str, Option};\n\npub let of_str(s: Str) -> Option(Bool) =\n    choose\n    | s == \"true\" => Option/some(true)\n    | _ => Option/none()\n    end;\n",
    "use /std/{Nat};\n\n--- Doubles.\npub let double(n: Nat) -> Nat =\n    n + n;\n\n--- Nothing.\nlet zero: Nat =\n    0\n--- One more.\nand one: Nat =\n    1;\n",
    "--- Shapes.\npub induct Shape: pub Type\n--- Round.\n| circle(Nat)\n| square(Nat)\nend\n\n--- A point.\nstruct Point: pub Type {\n    --- Across.\n    x: Nat,\n    y: Nat,\n}\n",
    "use /std/{Bool, Str};\n\npub let to_str(b: Bool) -> Str =\n    let x: Str = \"y\";\n    match b | true => x | false => \"false\" end;\n",
    "pub induct Ordering: pub Type\n| lt()\n| eq()\n| gt()\nend\n",
    "use /std/{Equal, Ordering};\n\npub concept Ordered(A: Type): pub Type {\n    use Equal(A),\n    cmp(A, A) -> Ordering,\n}\n",
    "use /std/{Equal, Bool};\n\nsatisfy Equal(Bool) {\n    eql = eql,\n    neq = xor,\n}\n",
    concat!(
        "use /std/{Io, Nat};\n",
        "\n",
        "let pair: Shape =\n",
        "    (\n",
        "        first_field_name = some_long_function_name(argument_one, argument_two),\n",
        "        second_field_name = another_long_function_name(argument_three, argument_four));\n",
        "\n",
        "let listed: Shape =\n",
        "    [\n",
        "        some_long_function_name(argument_one, argument_two),\n",
        "        another_long_function_name(argument_three, argument_four),\n",
        "        third_one(argument_five)];\n",
        "\n",
        "let Record: Type =\n",
        "    {\n",
        "        first_field_name: Some/Long/Type/Name(argument_one, argument_two),\n",
        "        second_field_name: Another/Long/Type/Name(argument_three, argument_four)};\n",
        "\n",
        "let point: Point =\n",
        "    Point {\n",
        "        first_field_name = some_long_function_name(argument_one, argument_two),\n",
        "        second_field_name = another_long_function_name(argument_three, argument_four),\n",
        "    };\n",
        "\n",
        "let run(queue: Queue, resumed: Job) -> Io(Queue) =\n",
        "    let widened = (queue + resumed) * some_long_function_name(argument_one, argument_two);\n",
        "    Io/pure(\n",
        "        enqueue(\n",
        "            enqueue(queue, Job { resume = resumed, token = widened, guards = [] }),\n",
        "            List/map(names, (name) => Str/concat(name, another_long_function_name(suffix)))));\n",
    ),
    concat!(
        "use /std/{Io, Monad};\n",
        "\n",
        "pub mod Inner\n",
        "    use /std/{Nat};\n",
        "\n",
        "    pub let one: Nat =\n",
        "        1;\n",
        "\n",
        "    pub let two: Nat =\n",
        "        2;\n",
        "end\n",
        "\n",
        "pub let fold(\n",
        "    first_parameter_name: Some/Long/Type/Name(Nat),\n",
        "    second_parameter_name: Another/Long/Type/Name(Nat),\n",
        "    step: (Nat, Nat) -> Nat,\n",
        ") -> Nat =\n",
        "    step(first_parameter_name, second_parameter_name);\n",
        "\n",
        "satisfy (\n",
        "    @M: (Type) -> Type,\n",
        "    @E: Type,\n",
        "    use Monad(M),\n",
        "    use Monad/Lift(Io, M),\n",
        ") => Monad/Lift(Io, (A: Type) => Carrier(M, E, A)) {\n",
        "    lift(@A, m) = m,\n",
        "}\n",
        "\n",
        "satisfy Spell(Inner/Marker) {\n",
        "    ..\n",
        "}\n",
        "\n",
        "concept Marker(A: Type): Type {}\n",
    ),
];

/// What `text` holds ahead of its marker, blanks and commas dropped: the tokens a comment written there stands after. A comma is dropped because a list's last one is the printer's to write or leave out.
fn ahead_of_marker(text: &str) -> String {
    text[..text.find(MARKER).expect("the marker is in the text")]
        .chars()
        .filter(|char| !char.is_whitespace() && *char != ',')
        .collect()
}

/// The lines of `source`, which is what a position is counted in.
fn lines(source: &str) -> Vec<&str> {
    source.trim_end_matches('\n').split('\n').collect()
}

/// Every position a writer has in `lines`, as a line and whether the comment rides it: on a line of its own above each line, and riding the end of each line that holds anything.
///
/// A position inside a documentation block is left out: a comment there splits the block, which is a refusal by design rather than a formatting question, and one riding a `---` line is that line's text.
fn positions(lines: &[&str]) -> Vec<(usize, bool)> {
    let documents = |index: usize| lines[index].trim_start().starts_with("---");
    let mut found = Vec::new();

    for (index, line) in lines.iter().enumerate() {
        if !(documents(index) && index > 0 && documents(index - 1)) {
            found.push((index, false));
        }
        if !line.trim().is_empty() && !documents(index) {
            found.push((index, true));
        }
    }

    found
}

/// What goes wrong with the marker written at one position of `lines` — riding the line's end, or above it at its indentation — or `None` where it keeps its place and a second run agrees.
fn misplaced(lines: &[&str], (index, rides): (usize, bool)) -> Option<String> {
    let line = lines[index];
    let marked = match rides {
        true => format!("{line} {MARKER}"),
        false => format!(
            "{}{MARKER}\n{line}",
            &line[..line.len() - line.trim_start().len()]
        ),
    };
    let mut written = lines.to_vec();
    written[index] = &marked;
    let perturbed = &format!("{}\n", written.join("\n"));

    let once = match Formatted::from_source(&Source::inline(perturbed)) {
        Ok(Formatted::Unchanged(text) | Formatted::Changed(text)) => text,
        Err(refusal) => return Some(format!("refused: {refusal}\n--- written ---\n{perturbed}")),
    };

    let (written, placed) = (ahead_of_marker(perturbed), ahead_of_marker(&once));
    let kept = match rides {
        // What closes the token it followed may slide under a comment riding that token's line.
        true => placed.strip_prefix(&*written).is_some_and(|slid| {
            slid.chars()
                .all(|char| matches!(char, ')' | ']' | '}' | ';'))
        }),
        false => written == placed,
    };
    if !kept {
        return Some(format!(
            "moved\n--- written ---\n{perturbed}--- formatted ---\n{once}"
        ));
    }

    match Formatted::from_source(&Source::inline(&once)) {
        Ok(Formatted::Unchanged(_)) => None,
        Ok(Formatted::Changed(twice)) => Some(format!(
            "moved again\n--- once ---\n{once}--- twice ---\n{twice}"
        )),
        Err(refusal) => Some(format!(
            "its own output was refused: {refusal}\n--- once ---\n{once}"
        )),
    }
}

/// **A comment keeps its place, and a second run over the first run's output changes nothing.**
///
/// The two properties the formatter's own verification cannot check, and the reason it cannot is structural. [`verify`](super::verify) compares *programs* — comments are not part of one — and counts comments, so a comment that merely *moves* passes both halves. One that moves to a position the next parse reads differently moves again, and a single `curios format` then leaves a file that no later run agrees with.
///
/// Every line of every fixture is tried twice, once with a comment on its own line above it and once riding its end, which is the whole space of positions a writer has.
#[test]
fn a_comment_keeps_its_place_from_every_position() {
    let mut wandering = Vec::new();

    for (fixture, source) in POSITIONS.iter().enumerate() {
        let lines = lines(source);

        for position in positions(&lines) {
            if let Some(fault) = misplaced(&lines, position) {
                wandering.push(format!("fixture {fixture}: {fault}"));
            }
        }
    }

    assert!(
        wandering.is_empty(),
        "{} positions do not keep their comment:\n\n{}",
        wandering.len(),
        wandering.join("\n\n")
    );
}

/// How many positions the sweep below steps over between two it tries. The whole of the tree is some fifty thousand positions and two formats each, which is an acceptance measurement rather than a test; a stride prime to every line count walks a different residue through every file.
const STRIDE: usize = 251;

/// Whether `line` opens a top-level item as a canonical source writes one: a head word at column zero.
fn opens_item(line: &str) -> bool {
    [
        "use ", "mod ", "let ", "pub ", "induct ", "struct ", "concept ", "satisfy ", "foreign ",
        "test ",
    ]
    .iter()
    .any(|head| line.starts_with(head))
}

/// The item of `lines` that holds line `index`, with the comments written above it, and `index` counted within it. A comment's place is decided inside the item it is written in, and formatting an item costs its own length where formatting its file costs the file's.
fn item_at<'a>(lines: &[&'a str], index: usize) -> (Vec<&'a str>, usize) {
    // Where an item's own lines begin: its head, or the first of the comment lines standing on it.
    let begins = |head: usize| {
        (0..head)
            .rev()
            .take_while(|above| lines[*above].starts_with("--"))
            .last()
            .unwrap_or(head)
    };
    let starts = (0..lines.len())
        .filter(|line| opens_item(lines[*line]))
        .map(begins)
        .collect::<Vec<_>>();

    let start = starts
        .iter()
        .rev()
        .find(|start| **start <= index)
        .copied()
        .unwrap_or(0);
    let end = starts
        .iter()
        .find(|start| **start > index)
        .copied()
        .unwrap_or(lines.len());

    (lines[start..end].to_vec(), index - start)
}

/// The same law over the sources of the tree, which hold the shapes no fixture thought of: every [`STRIDE`]-th position of every source, tried within the item that holds it.
#[test]
fn a_comment_keeps_its_place_in_the_sources_of_the_tree() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("..");
    let mut paths = Vec::new();
    collect_sources(&root, &mut paths);
    let sources = paths
        .iter()
        .map(|path| std::fs::read_to_string(path).expect("a readable source"))
        .collect::<Vec<_>>();

    let tried = sources
        .iter()
        .enumerate()
        .flat_map(|(file, source)| {
            positions(&lines(source))
                .into_iter()
                .map(move |position| (file, position))
        })
        .step_by(STRIDE)
        .collect::<Vec<_>>();
    assert!(tried.len() > 100, "only {} positions tried", tried.len());

    let wandering = tried
        .into_iter()
        .filter_map(|(file, (index, rides))| {
            let whole = lines(&sources[file]);
            let (item, within) = item_at(&whole, index);
            // An item cut where the source does not divide is tried in its whole file instead.
            let alone = format!("{}\n", item.join("\n").trim_end());
            let divides = Formatted::from_source(&Source::inline(&alone))
                == Ok(Formatted::Unchanged(alone.clone()));
            let fault = match divides {
                true => misplaced(&item, (within, rides)),
                false => misplaced(&whole, (index, rides)),
            };

            fault.map(|fault| format!("{}: {fault}", paths[file].display()))
        })
        .collect::<Vec<_>>();

    assert!(
        wandering.is_empty(),
        "{} positions do not keep their comment:\n\n{}",
        wandering.len(),
        wandering.join("\n\n")
    );
}

/// A comment written under an arm's arrow, on a line of its own, stays there: between `=>` and the body it leads.
#[test]
fn a_comment_under_an_arms_arrow_stays_under_it() {
    let source = "let f(n: /std/Nat) -> /std/Nat =\n    match n\n    | 0 => 1\n    | p + 1 =>\n        -- what the successor case does\n        p\n    end;\n\nf(1)\n";
    assert_eq!(formatted(source), source);
}

/// A comment riding an arm's arrow stays on the arm it was written on, and the body it pushed down goes one level in.
#[test]
fn a_comment_riding_an_arms_arrow_stays_on_its_arm() {
    let source = "let f(o: /std/Option(/std/Nat)) -> /std/Nat =\n    match o\n    | some(x) => -- the value\n        x\n    | none() => -- nothing\n        0\n    end;\n\nf(o)\n";
    assert_eq!(formatted(source), source);
}

/// A witness block that holds no entry keeps the comments written in and after it, where the next declaration used to take them.
#[test]
fn a_comment_in_or_after_a_witness_block_with_no_entry_stays_with_it() {
    let source = concat!(
        "satisfy Spell(Point) {\n",
        "    -- above\n",
        "    .. -- beside\n",
        "} -- after\n",
        "\n",
        "satisfy Marker(Nat) {} -- after the empty one\n",
        "\n",
        "concept Marker(A: Type): Type {}\n",
    );
    assert_eq!(formatted(source), source);
}

/// A comment riding an opener and one leading the first member are two lines of output, where one line holding both would read back as a single comment and be refused.
#[test]
fn a_comment_riding_an_opener_and_one_leading_the_first_member_keep_their_lines() {
    let source = concat!(
        "let build: Shape =\n",
        "    some_long_function_name( -- opener\n",
        "        -- leads the first\n",
        "        argument_one,\n",
        "        argument_two);\n",
    );
    assert_eq!(formatted(source), source);
}

/// A comment written before a closer stays in its block, at the members' column, with the closer on the line below: one fixture for every shape that closes — a `use` group, an inline module, the three declaration bodies, a telescope and its item's terminator, a tuple, a list, a packed literal, a tuple type, a struct literal, a call in both layouts, a parenthesized operand, a tuple of one, an empty call and a ladder.
#[test]
fn a_comment_before_a_closer_stays_at_its_members_column() {
    let source = concat!(
        "use /std/{Nat, Bool\n",
        "    -- Str,\n",
        "};\n",
        "\n",
        "pub mod Inner\n",
        "    pub let one: Nat =\n",
        "        1;\n",
        "    -- pub let two: Nat = 2;\n",
        "end\n",
        "\n",
        "struct Point: pub Type {\n",
        "    x: Nat,\n",
        "    -- y: Nat,\n",
        "}\n",
        "\n",
        "concept Marker(A: Type): Type {\n",
        "    -- nothing yet\n",
        "}\n",
        "\n",
        "satisfy Spell(Point) {\n",
        "    ..\n",
        "    -- after the dots\n",
        "}\n",
        "\n",
        "let fold(\n",
        "    first: Nat,\n",
        "    -- second: Nat,\n",
        ") -> Nat =\n",
        "    first\n",
        "    -- before the terminator\n",
        ";\n",
        "\n",
        "let pair: Shape =\n",
        "    (\n",
        "        first,\n",
        "        second\n",
        "        -- third,\n",
        "    );\n",
        "\n",
        "let listed: Shape =\n",
        "    [\n",
        "        first,\n",
        "        second\n",
        "        -- third,\n",
        "    ];\n",
        "\n",
        "let packed: Bytes =\n",
        "    x[\n",
        "        1, 2\n",
        "        -- 3,\n",
        "    ];\n",
        "\n",
        "let Record: Type =\n",
        "    {\n",
        "        first: Nat\n",
        "        -- second: Nat,\n",
        "    };\n",
        "\n",
        "let point: Point =\n",
        "    Point {\n",
        "        first = 1,\n",
        "        -- second = 2,\n",
        "    };\n",
        "\n",
        "let called: Shape =\n",
        "    some_function(\n",
        "        first,\n",
        "        second\n",
        "        -- third,\n",
        "    );\n",
        "\n",
        "let hugged: Shape =\n",
        "    List/map(names, (name) => name\n",
        "        -- the body's last word\n",
        "    );\n",
        "\n",
        "let grouped: Nat =\n",
        "    (first + second\n",
        "        -- + third\n",
        "    )\n",
        "        * fourth;\n",
        "\n",
        "let single: Shape =\n",
        "    (first,\n",
        "        -- nothing more\n",
        "    );\n",
        "\n",
        "let empty: Shape =\n",
        "    some_function(\n",
        "        -- no arguments yet\n",
        "    );\n",
        "\n",
        "let matched(o: Option(Nat)) -> Nat =\n",
        "    match o\n",
        "    | some(x) => x\n",
        "    | none() => 0\n",
        "    -- | other() => 1\n",
        "    end;\n",
    );
    assert_eq!(formatted(source), source);
}
