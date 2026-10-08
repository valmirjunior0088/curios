use {
    super::{
        Formatted,
        test_support::{collect_sources, formatted},
    },
    curios_utilities::Source,
};

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

/// A program whose tail is garbled is refused where the tail's own parse stopped, naming what it expected there — as the compiler refuses it — and not at the top of the file expecting its end, which is what the optional tail reported while the term's failure was discarded.
#[test]
fn a_garbled_tail_is_refused_where_the_compiler_refuses_it() {
    let refusal = Formatted::from_source(&Source::inline("let x = (\n")).expect_err("garbled tail");
    assert!(
        refusal.starts_with("Expected ')', obtained 'end-of-file'"),
        "{refusal}"
    );
    assert!(refusal.contains("\n    2 | "), "{refusal}");
}

#[test]
fn a_derived_witness_keeps_its_dots_on_a_line_of_their_own() {
    // The derived block is always broken, alone and inside a mixed group, as a block of one entry is: the dots sit one level in, and take no comma since they are no entry.
    let source = "use /std/{Nat, Bool, Spell, Equal};\n\nsatisfy Spell(Nat) {\n    ..\n}\n\nsatisfy Spell(Bool) {\n    ..\n}\nand Equal(Bool) {\n    eql = eql,\n    neq = xor,\n}\n";
    assert_eq!(formatted(source), source);
    assert_eq!(formatted(&source.replace("{\n    ..\n}", "{ .. }")), source);
}

/// A `foreign` declaration prints as it was written: every wire type, both list forms, a zero-argument constant and a `pub` one. The formatter's guard refuses output that does not reparse to the same program, so this pins the one thing it cannot see — a rendering that reparses to a different declaration and would rewrite every foreign row in a project.
#[test]
fn foreign_declarations_are_canonical_as_written() {
    let source = "foreign clock: Nat;\n\nforeign frobnicate: (Nat, Bytes) -> Int;\n\npub foreign log: (Bytes) -> Bool;\n\nforeign scan: (List(Bytes), Handle) -> List(Handle);\n\nforeign poll: (List(Nat), Int) -> List(Nat);\n\nforeign close: (Handle) -> {};\n\nforeign read: (Handle, Nat) -> {status: Nat, bytes: Bytes};\n\n/std/print(\"x\")\n";
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

/// A comment riding a top-level definition's `=` line, or a test's, stays on it rather than being carried onto the body's.
#[test]
fn a_comment_riding_a_definitions_equals_stays_on_its_line() {
    for source in [
        "pub let f(n: /std/Nat) -> /std/Nat = -- trailing\n    n;\n",
        "test holds = -- trailing\n    /std/Test/assert(true);\n",
    ] {
        assert_eq!(formatted(source), source);
    }
}

#[test]
fn a_comment_above_a_later_let_binding_stays_above_it() {
    let source = "let compute(n: /std/Nat) -> /std/Nat =\n    let a = n + n;\n    -- the second binding\n    let b = a + a;\n    b;\n\ncompute(1)\n";
    assert_eq!(formatted(source), source);
}

/// A comment leading a clause joined by `and` stays above the `and`, where it could print between a parameter and its type. Each fixture below is already canonical, so equality pins the placement and the fixed point at once.
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
    let source = "induct Even: Type\n| zero()\n| from_odd(Odd)\n-- what the second family is for\nand Odd: Type\n| from_even(Even)\nend\n";
    assert_eq!(formatted(source), source);
}

/// A comment leading a member of a delimited list — a `match` or `choose` arm, a struct-literal, concept or witness field, an `induct` case — stays above the member, where it could print *inside* it: `injective(a, b, same) =` on one line and the comment that was written above the field on the next. Each fixture below is already canonical, so asserting the placement and the fixed point pins both halves.
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

/// A trailing comment written after a separator — a local `let`'s `;`, a list element's, a call argument's or a tuple field's `,` — rides the line the separator ends, on every run.
///
/// **A comment does not surface one construct down per run**: `-- after a` documenting `b`, `-- one` closing the list, and the tail's comment written inside the call that follows.
#[test]
fn a_trailing_comment_after_a_separator_rides_the_line_it_was_written_on() {
    let source = "let f(n: /std/Nat) -> /std/Nat =\n    let a = 1; -- after a\n    let b = [\n        1, -- one\n        2, -- two\n    ];\n    g(\n        a, -- first\n        n); -- the call\n\nf(1)\n";
    let output = formatted(source);

    for (comment, rides) in [
        ("-- after a", "let a = 1;"),
        ("-- one", "1,"),
        ("-- two", "2"),
        ("-- first", "a,"),
        ("-- the call", "n);"),
    ] {
        let line = output
            .lines()
            .find(|line| line.contains(comment))
            .unwrap_or_else(|| panic!("{comment} survives: {output}"));
        assert!(
            line.trim_start().starts_with(rides),
            "{comment} rides the line ending in `{rides}`: {output}"
        );
    }
    assert_eq!(formatted(&output), output);
}

/// A trailing comment stays on the arm it was written on, however many arms carry one.
///
/// **A trailing comment does not drift to the next break**, each surfacing one arm down and the last, having no break left inside the construct, past `end;`: `-- affirmative` would document the negative arm and `-- negative` the declaration.
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
/// **The regression for the formatter refusing its own output.** Two comments driven onto one output line are *one* comment when it reparses — a line comment runs to the end of its line — so the verifier saw one fewer than it captured and refused to write. The shape below is exactly what that drift would produce, and a single `curios format` would then leave a file that every later `curios format --check` rejects.
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
    // The comment keeps the call broken. `f` is unbound as far as formatting cares — formatting is syntax-only — so this must format, conserve the comment, and reparse.
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

/// A telescope that overflows breaks the way a `let`'s already did: one binder per line, a trailing comma, and the closer dedented to the declaration's own column.
///
/// The closer does not ride the last binder, which would leave it and the first field of the brace body at one indent with only a mid-line `)` between them. This pins `satisfy`, `struct`, `concept` and `induct` against the `let` they share the shape with.
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

/// A witness head too wide for its line breaks at its telescope, as a `let` breaks at its parameters before its result: the application the telescope concludes then fits beside the arrow, where breaking it instead stacks the concept's arguments at the body's own indent.
#[test]
fn a_witness_head_breaks_its_telescope_before_its_application() {
    let source = concat!(
        "use /std/{Monad, Io};\n\n",
        "satisfy (@M: (Type) -> Type, @E: Type, use Monad(M), use Monad/Lift(Io, M)) => Monad/Lift(Io, (A: Type) => Carrier(M, E, A)) {\n",
        "    lift(@A, m) = m,\n",
        "}\n",
    );
    let output = concat!(
        "use /std/{Monad, Io};\n\n",
        "satisfy (\n",
        "    @M: (Type) -> Type,\n",
        "    @E: Type,\n",
        "    use Monad(M),\n",
        "    use Monad/Lift(Io, M),\n",
        ") => Monad/Lift(Io, (A: Type) => Carrier(M, E, A)) {\n",
        "    lift(@A, m) = m,\n",
        "}\n",
    );
    assert_eq!(formatted(source), output);
    assert_eq!(formatted(output), output, "and the shape is idempotent");
}

#[test]
fn an_inline_module_keeps_the_blank_lines_between_its_items() {
    // One rule separates module items, and a file and an inline `mod … end` share it: an inline printer joining its items with a single line would eat every blank line written inside one on the first run while the file around it kept its own.
    let source = concat!(
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
        "pub use Inner/{let one};\n",
    );

    assert_eq!(formatted(source), source);
}

/// A documentation comment is part of the tree, so the formatter prints it where it was written: above the declaration, the constructor, the field, the method, the witness and the module it documents, with a plain comment above the block kept above it and one between the block and its declaration kept between them.
#[test]
fn documentation_comments_are_printed_where_they_were_written() {
    let source = concat!(
        "--- Numbers.\n",
        "mod nat;\n",
        "\n",
        "-- above\n",
        "--- Doubles.\n",
        "---\n",
        "--- Twice.\n",
        "-- between\n",
        "pub let double(n: Nat) -> Nat =\n",
        "    n + n;\n",
        "\n",
        "pub induct Shape: pub Type\n",
        "--- Round.\n",
        "| circle(Nat)\n",
        "| square(Nat)\n",
        "end\n",
        "\n",
        "struct Point: pub Type {\n",
        "    --- Across.\n",
        "    x: Nat,\n",
        "    y: Nat,\n",
        "}\n",
        "\n",
        "concept Show(A: Type): pub Type {\n",
        "    --- Renders.\n",
        "    show(A) -> Str,\n",
        "}\n",
        "\n",
        "--- Structural.\n",
        "satisfy Equal(Nat) {\n",
        "    ..\n",
        "}\n",
        "and Equal(Bool) {\n",
        "    eql = eql,\n",
        "    neq = xor,\n",
        "}\n",
    );

    assert_eq!(formatted(source), source);
}

/// Every Curios source in the tree formats without refusal — the parse and the round trip hold over the whole corpus, the standard library included — which is what proves a grammar change left every existing file meaning what it meant.
#[test]
fn every_source_in_the_tree_formats_without_refusal() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("..");
    let mut sources = Vec::new();
    collect_sources(&root, &mut sources);
    assert!(
        sources.len() > 100,
        "found only {} sources under {}",
        sources.len(),
        root.display()
    );

    let refused = sources
        .iter()
        .filter_map(|path| {
            Formatted::from_path(path)
                .err()
                .map(|refusal| format!("{}: {refusal}", path.display()))
        })
        .collect::<Vec<_>>();
    assert!(refused.is_empty(), "{}", refused.join("\n"));
}

/// A block string literal is laid out by the formatter: its lines and its closer one level in from the opener's line, wherever the author left them, and the value unchanged because the closer's column is what a reader strips.
#[test]
fn a_block_string_literal_takes_the_canonical_indentation() {
    let source = "use /std/{Str};\n\nlet page: Str =\n    \"\"\"\n    <p>\n      x\n    </p>\n    \"\"\";\n\npage\n";
    assert_eq!(
        formatted(source),
        "use /std/{Str};\n\nlet page: Str =\n    \"\"\"\n        <p>\n          x\n        </p>\n        \"\"\";\n\npage\n"
    );
}
