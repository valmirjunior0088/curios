use {super::Formatted, curios_utilities::Source};

/// The canonical text, either way: production callers match the variants directly, since changed-ness decides the exit path, and only these round-trip tests want the text alone.
fn formatted(source: &str) -> String {
    match Formatted::from_source(&Source::inline(source)).expect("fixture formats") {
        Formatted::Unchanged(text) | Formatted::Changed(text) => text,
    }
}

#[test]
fn a_program_normalizes_to_the_canonical_shape() {
    // Top-level `=` always breaks; exactly one blank line between items; the tail closes the file.
    let source = "use /std/{Nat};\nlet double(n : Nat) -> Nat = n + n;\n\n\n\ndouble(21)\n";
    assert_eq!(
        formatted(source),
        "use /std/{Nat};\n\nlet double(n: Nat) -> Nat =\n    n + n;\n\ndouble(21)\n"
    );
}

#[test]
fn a_module_file_without_a_tail_formats() {
    let source = "use /std/{Nat};\nlet one : Nat = 1;";
    assert_eq!(
        formatted(source),
        "use /std/{Nat};\n\nlet one: Nat =\n    1;\n"
    );
}

#[test]
fn a_derived_witness_stays_a_declaration() {
    // The formatter leaves the body-less form unexpanded, alone and inside a mixed group: `;` is the whole of what was written, so nothing is there to break across lines.
    let source = "use /std/{Nat, Bool, Spell, Equal};\n\nsatisfy Spell(Nat);\n\nsatisfy Spell(Bool);\nand Equal(Bool) {\n    eql = eql,\n    neq = xor,\n}\n";
    assert_eq!(formatted(source), source);
}

/// A `foreign` declaration prints as it was written: every wire type, both list forms, a zero-argument constant and a `pub` one. The formatter's guard refuses output that does not reparse to the same program, so this pins the one thing it cannot see — a rendering that reparses to a different declaration and would rewrite every foreign row in a project.
#[test]
fn foreign_declarations_are_canonical_as_written() {
    let source = "foreign clock: Nat;\n\nforeign frobnicate: (Nat, Bytes) -> Int;\n\npub foreign log: (Bytes) -> Bool;\n\nforeign scan: (List(Bytes), Handle) -> List(Handle);\n\nforeign poll: (List(Nat), Int) -> List(Nat);\n\n/std/print(\"x\")\n";
    assert_eq!(formatted(source), source);
}

#[test]
fn formatting_is_idempotent() {
    let source = "use /std/{Nat};\n\nlet double(n : Nat) -> Nat =\n    n + n;\n\ndouble(21)\n";
    let once = formatted(source);
    assert_eq!(formatted(&once), once);
}

#[test]
fn a_leading_comment_stays_before_its_item() {
    let source = "-- doubles the input\nlet double(n : /std/Nat) -> /std/Nat = n + n;\ndouble(2)\n";
    let output = formatted(source);
    assert!(
        output.starts_with("-- doubles the input\nlet double"),
        "unexpected output: {output}"
    );
}

#[test]
fn a_trailing_comment_rides_its_line() {
    let source = "let one : /std/Nat = 1; -- unity\none\n";
    let output = formatted(source);
    assert!(
        output.contains("    1; -- unity\n") || output.contains("1; -- unity\n"),
        "unexpected output: {output}"
    );
}

#[test]
fn a_comment_above_a_later_let_binding_stays_above_it() {
    // The let-chain tail must not claim comments leading later bindings (it once built eagerly, ahead of the binding documents), and a binding-leading comment claims at the binding head, so this shape is a fixed point.
    let source = "let compute(n: /std/Nat) -> /std/Nat =\n    let a = n + n;\n    -- the second binding\n    let b = a + a;\n    b;\n\ncompute(1)\n";
    assert_eq!(formatted(source), source);
}

/// A clause joined by `and` records no span of its own, so a comment leading one used to fall to the first *descendant* that had one — printing between a parameter and its type. That relocation reparsed as a leading comment somewhere new, so the next run moved it again: the one shape in which this formatter failed to converge. Each fixture below is already canonical, so equality pins the placement and the fixed point at once.
#[test]
fn a_comment_above_a_let_group_clause_stays_above_and() {
    let source = "let even(n: /std/Nat) -> /std/Bool =\n    odd(n)\n-- what the second clause is for\nand odd(n: /std/Nat) -> /std/Bool =\n    even(n);\n";
    assert_eq!(formatted(source), source);
}

#[test]
fn a_comment_above_a_local_let_group_clause_stays_above_and() {
    let source = "let main(n: /std/Nat) -> /std/Bool =\n    let a(x: /std/Nat) -> /std/Bool = b(x)\n    -- the local second clause\n    and b(x: /std/Nat) -> /std/Bool = a(x);\n    a(n);\n";
    assert_eq!(formatted(source), source);
}

#[test]
fn a_comment_above_an_induct_clause_stays_above_and() {
    // This clause has neither parameters nor indices, and a sort parses spanless, so its head carries no offset at all and the claim falls back to the first case's payload.
    let source = "induct Even: Type\n| zero()\n| from_odd(Odd)\n-- what the second family is for\nand Odd: Type\n| from_even(Even)\nend\n";
    assert_eq!(formatted(source), source);
}

/// A member of a delimited list — a `match` or `choose` arm, a struct-literal, concept or witness field, an `induct` case — records no span of its own, exactly as an `and` clause does not. A comment leading one therefore used to fall to the first spanned *descendant*, which is the member's body, and printed *inside* it: `injective(a, b, same) =` on one line and the comment that was written above the field on the next. Each fixture below is already canonical, so asserting the placement and the fixed point pins both halves.
#[test]
fn a_comment_above_a_match_arm_stays_above_the_bar() {
    let source = "let f(n: /std/Nat) -> /std/Nat =\n    match n\n    | 0 => 1\n    -- the successor case\n    | p + 1 => p\n    end;\n\nf(1)\n";
    let output = formatted(source);
    assert!(
        output.contains("\n    -- the successor case\n    | p + 1"),
        "unexpected output: {output}"
    );
    assert_eq!(formatted(&output), output);
}

#[test]
fn a_comment_above_a_choose_arm_stays_above_the_bar() {
    let source = "let f(n: /std/Nat) -> /std/Nat =\n    choose\n    | n == 0 => 1\n    -- the fallback\n    | _ => n\n    end;\n\nf(1)\n";
    let output = formatted(source);
    assert!(
        output.contains("\n    -- the fallback\n    | _"),
        "unexpected output: {output}"
    );
    assert_eq!(formatted(&output), output);
}

#[test]
fn a_comment_above_a_witness_field_stays_above_it() {
    let source = "concept Key(K: Type): Type {\n    to_bin(K) -> /std/Bytes,\n}\n\nsatisfy Key(/std/Bytes) {\n    -- the identity encoding\n    to_bin(b) = b,\n}\n\n1\n";
    let output = formatted(source);
    assert!(
        output.contains("\n    -- the identity encoding\n    to_bin(b) = b,"),
        "unexpected output: {output}"
    );
    assert_eq!(formatted(&output), output);
}

#[test]
fn a_comment_above_a_concept_field_stays_above_it() {
    let source = "concept Key(K: Type): Type {\n    to_bin(K) -> /std/Bytes,\n    -- the injectivity obligation\n    injective(a: K) -> K,\n}\n\n1\n";
    let output = formatted(source);
    assert!(
        output.contains("\n    -- the injectivity obligation\n    injective(a: K) -> K,"),
        "unexpected output: {output}"
    );
    assert_eq!(formatted(&output), output);
}

/// The repair half: a comment already sitting between a member's `=>` and its body is claimed by the member and lifted back out. That is what makes the fix retroactive — the corpus carries instances an earlier formatter run relocated, and re-running the formatter now returns each to the arm it documents.
#[test]
fn a_comment_relocated_into_an_arm_body_is_lifted_back_out() {
    let source = "let f(n: /std/Nat) -> /std/Nat =\n    match n\n    | 0 => 1\n    | p + 1 =>\n        -- what the successor case does\n        p\n    end;\n\nf(1)\n";
    let output = formatted(source);
    assert!(
        output.contains("\n    -- what the successor case does\n    | p + 1"),
        "unexpected output: {output}"
    );
    assert_eq!(formatted(&output), output);
}

/// A trailing comment stays on the arm it was written on, however many arms carry one.
///
/// **The regression for a trailing comment drifting to the next break.** Claimed by the node that *follows* it, it was prefixed to that node's document and reached the suffix channel after the newline closing its own line had gone out — so each comment surfaced one arm down, and the last one, having no break left inside the construct, flushed at the document's end past `end;`. Here `-- affirmative` documented the negative arm and `-- negative` documented the declaration.
#[test]
fn a_trailing_comment_stays_on_the_arm_it_was_written_on() {
    let source = "let d(b: /std/Bool) -> /std/Str =\n    choose\n    | b == true => \"yes\" -- affirmative\n    | _ => \"no\" -- negative\n    end;\n\nd(true)\n";
    let output = formatted(source);

    let affirmative = output
        .lines()
        .position(|line| line.contains("-- affirmative"))
        .expect("the comment survives");
    let negative = output
        .lines()
        .position(|line| line.contains("-- negative"))
        .expect("the comment survives");

    assert!(
        output
            .lines()
            .nth(affirmative)
            .is_some_and(|line| line.contains("\"yes\"")),
        "the affirmative comment rides the arm it documents: {output}"
    );
    assert!(
        output
            .lines()
            .nth(negative)
            .is_some_and(|line| line.contains("\"no\"")),
        "the negative comment rides the arm it documents: {output}"
    );
    assert_eq!(formatted(&output), output, "and formatting converges");
}

/// A trailing comment inside a construct does not escape it, and a second one on the construct's own last line survives beside it.
///
/// **The regression for the formatter refusing its own output.** Two comments driven onto one output line are *one* comment when it reparses — a line comment runs to the end of its line — so the verifier saw one fewer than it captured and refused to write. The shape below is exactly what the drift above used to produce, which is how a single `curios format` could leave a file that every later `curios format --check` rejected.
#[test]
fn an_interior_trailing_comment_does_not_escape_its_construct() {
    let source = "let d(b: /std/Bool) -> /std/Str =\n    choose\n    | b == true => \"yes\"\n    | _ => \"no\" -- inner\n    end; -- tail\n\nd(true)\n";
    let output = formatted(source);

    assert!(
        output.contains("-- inner"),
        "the interior comment survives: {output}"
    );
    assert!(
        output.contains("-- tail"),
        "the item's own comment survives: {output}"
    );
    assert!(
        output.lines().all(|line| line.matches("--").count() <= 1),
        "two comments on one line reparse as one: {output}"
    );
    assert_eq!(formatted(&output), output, "and formatting converges");
}

#[test]
fn an_interior_comment_survives_and_forces_a_break() {
    // The comment claims into the argument's document; its hard break keeps the call broken. `f` is unbound as far as formatting cares — formatting is syntax-only — so this must format, conserve the comment, and reparse.
    let source = "let one : /std/Nat = f( -- why\n    1);\none\n";
    let output = formatted(source);
    assert!(output.contains("-- why"), "comment lost: {output}");
}

#[test]
fn consecutive_use_declarations_stack_without_blank_lines() {
    let source = "use /std/{Nat};\n\n\nuse /std/{Bool};\nuse /std/{Str};\nlet a : Nat = 1;\na\n";
    assert_eq!(
        formatted(source),
        "use /std/{Nat};\nuse /std/{Bool};\nuse /std/{Str};\n\nlet a: Nat =\n    1;\n\na\n"
    );
}

#[test]
fn a_dangling_comment_closes_the_file() {
    let source = "let one : /std/Nat = 1;\none\n-- the end\n";
    let output = formatted(source);
    assert!(
        output.trim_end().ends_with("-- the end"),
        "unexpected output: {output}"
    );
}

#[test]
fn blank_lines_normalize_to_exactly_one_between_items() {
    let source = "let a : /std/Nat = 1;\nlet b : /std/Nat = 2;\n\n\n\nlet c : /std/Nat = 3;\na\n";
    let output = formatted(source);
    assert!(output.contains(";\n\nlet b"), "unexpected output: {output}");
    assert!(output.contains(";\n\nlet c"), "unexpected output: {output}");
    assert!(!output.contains("\n\n\n"), "unexpected output: {output}");
}

/// Every syntactic position a comment can be attached at, one fixture each: a `let` signature over a `choose`, a `match` with a local `let`, an `induct`, a `concept` body, and a `satisfy` body.
///
/// Unbound names are deliberate — formatting is syntax-only, so these need to parse and nothing more.
const POSITIONS: [&str; 5] = [
    "use /std/{Bool, Str, Option};\n\npub let of_str(s: Str) -> Option(Bool) =\n    choose\n    | s == \"true\" => Option/some(true)\n    | _ => Option/none()\n    end;\n",
    "use /std/{Bool, Str};\n\npub let to_str(b: Bool) -> Str =\n    let x: Str = \"y\";\n    match b | true => x | false => \"false\" end;\n",
    "pub induct Ordering: pub Type\n| lt()\n| eq()\n| gt()\nend\n",
    "use /std/{Equal, Ordering};\n\npub concept Ordered(A: Type): pub Type {\n    use Equal(A),\n    cmp(A, A) -> Ordering,\n}\n",
    "use /std/{Equal, Bool};\n\nsatisfy Equal(Bool) {\n    eql = eql,\n    neq = xor,\n}\n",
];

/// **Formatting converges: a second run over the first run's output changes nothing.**
///
/// The one property the formatter's own verification cannot check, and the reason it cannot is structural. [`verify`](super::verify) compares *programs* — comments are not part of one — and counts comments, so a comment that merely *moves* passes both halves. A comment that moves to a position the next parse reads differently moves again, and a single `curios format` then leaves a file that no later run agrees with.
///
/// Every line of every fixture is tried twice, once with a comment on its own line above it and once riding its end, which is the whole space of positions a writer has.
///
/// **This held at 7 of 54 failures while a comment's place was decided by whichever node's geometry happened to reach it**, and the claimant differed between the written form and the printed one. It reads zero now that a trailing comment is placed by the renderer, which is the only thing that knows where an output line ends.
#[test]
fn formatting_converges_from_every_comment_position() {
    let mut wandering = Vec::new();

    for (fixture, source) in POSITIONS.iter().enumerate() {
        let lines = source
            .trim_end_matches('\n')
            .split('\n')
            .collect::<Vec<_>>();

        for (index, line) in lines.iter().enumerate() {
            let mut placements = vec![("leading", with_line(&lines, index, "-- MARK"))];
            if !line.trim().is_empty() {
                placements.push((
                    "trailing",
                    replacing(&lines, index, &format!("{line} -- MARK")),
                ));
            }

            for (placement, perturbed) in placements {
                let once = match Formatted::from_source(&Source::inline(&perturbed)) {
                    Ok(Formatted::Unchanged(text) | Formatted::Changed(text)) => text,
                    Err(refusal) => {
                        wandering.push(format!(
                            "{fixture}:{} {placement}: refused: {refusal}",
                            index + 1
                        ));
                        continue;
                    }
                };

                match Formatted::from_source(&Source::inline(&once)) {
                    Ok(Formatted::Unchanged(_)) => {}
                    Ok(Formatted::Changed(twice)) => wandering.push(format!(
                        "{fixture}:{} {placement}: moved again\n--- once ---\n{once}--- twice ---\n{twice}",
                        index + 1
                    )),
                    Err(refusal) => wandering.push(format!(
                        "{fixture}:{} {placement}: its own output was refused: {refusal}",
                        index + 1
                    )),
                }
            }
        }
    }

    assert!(
        wandering.is_empty(),
        "{} positions do not converge:\n\n{}",
        wandering.len(),
        wandering.join("\n\n")
    );
}

/// `lines` with `inserted` on a line of its own before `index`.
fn with_line(lines: &[&str], index: usize, inserted: &str) -> String {
    let mut out = lines[..index].to_vec();
    out.push(inserted);
    out.extend_from_slice(&lines[index..]);

    format!("{}\n", out.join("\n"))
}

/// `lines` with the line at `index` replaced by `replacement`.
fn replacing(lines: &[&str], index: usize, replacement: &str) -> String {
    let mut out = lines.to_vec();
    out[index] = replacement;

    format!("{}\n", out.join("\n"))
}

/// A telescope that overflows breaks the way a `let`'s already did: one binder per line, a trailing comma, and the closer dedented to the declaration's own column.
///
/// The closer used to ride the last binder, which left it and the first field of the brace body at one indent with only a mid-line `)` between them. `satisfy`, `struct`, `concept` and `induct` all took that shape; this pins all four against the `let` they now share it with.
#[test]
fn an_overflowing_telescope_dedents_its_closer() {
    let source = concat!(
        "use /std/{Show, Str};\n\n",
        "pub concept Rendering(Alphabet: Type, Beta: Type, Gamma: Type, Delta: Type, Epsilon: Type, Zeta: Type): pub Type {\n",
        "    render(Alphabet) -> Str,\n",
        "}\n\n",
        "pub concept Renderable(Alphabet: Type): pub Type {\n",
        "    render(Alphabet) -> Str,\n",
        "}\n\n",
        "pub struct Holder(Alphabet: Type, Beta: Type, Gamma: Type, Delta: Type, Epsilon: Type, Zeta: Type): pub Type {\n",
        "    first: Alphabet,\n",
        "}\n\n",
        "pub induct Choice(Alphabet: Type, Beta: Type, Gamma: Type, Delta: Type, Epsilon: Type, Zeta: Type): pub Type\n",
        "| only(Alphabet)\n",
        "end\n\n",
        "satisfy (@Alphabet: Type, @Beta: Type, use Show(Alphabet), use Show(Beta), use Show(Str), use Show(Alphabet)) => Renderable(Alphabet) {\n",
        "    render(a) = Show/show(a),\n",
        "}\n",
    );
    let output = formatted(source);

    for closer in [
        "\n): pub Type {\n    render(Alphabet) -> Str,",
        "\n): pub Type {\n    first: Alphabet,",
        "\n): pub Type\n| only(Alphabet)",
        "\n) => Renderable(Alphabet) {",
    ] {
        assert!(output.contains(closer), "missing {closer:?} in:\n{output}");
    }
    // Every broken telescope ends in a trailing comma, which is what keeps adding a binder a one-line change.
    assert_eq!(output.matches("Zeta: Type,\n)").count(), 3);
    assert_eq!(formatted(&output), output, "and the shape is idempotent");
}
