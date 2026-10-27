//! A program formatted whole: its canonical shape and idempotence, what it refuses, the blank lines and separations between items, and every source in the tree.

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

/// A byte-order mark at a file's head is no part of the program and a carriage return is a blank, so the formatter writes neither: `format --check` reports a file that leads with a mark as it reports one whose lines end in a carriage return.
#[test]
fn a_leading_byte_order_mark_is_not_written_back() {
    let source = "use /std/{Nat};\n\nlet one: Nat =\n    1;\n";

    assert_eq!(formatted(&format!("\u{FEFF}{source}")), source);
    assert_eq!(formatted(&source.replace('\n', "\r\n")), source);
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

/// A `foreign` declaration prints as it was written: every wire type, both list forms, a row of no operand and a `pub` one. The formatter's guard refuses output that does not reparse to the same program, so this pins the one thing it cannot see — a rendering that reparses to a different declaration and would rewrite every foreign row in a project.
#[test]
fn foreign_declarations_are_canonical_as_written() {
    let source = "foreign clock() -> Nat;\n\nforeign frobnicate(Nat, Bytes) -> Int;\n\npub foreign log(Bytes) -> Bool;\n\nforeign scan(List(Bytes), Handle) -> List(Handle);\n\nforeign poll(List(Nat), Int) -> List(Nat);\n\nforeign close(Handle) -> {};\n\nforeign read(Handle, Nat) -> {status: Nat, bytes: Bytes};\n\n/std/print(\"x\")\n";
    assert_eq!(formatted(source), source);
}

#[test]
fn formatting_is_idempotent() {
    let source = "use /std/{Nat};\n\nlet double(n : Nat) -> Nat =\n    n + n;\n\ndouble(21)\n";
    let once = formatted(source);
    assert_eq!(formatted(&once), once);
}

#[test]
fn a_use_keeps_the_separation_it_was_written_with() {
    let source = "use /std/{Nat};\n\n\nuse /std/{Bool};\nuse /std/{Str};\nlet a : Nat = 1;\na\n";
    assert_eq!(
        formatted(source),
        "use /std/{Nat};\n\nuse /std/{Bool};\nuse /std/{Str};\n\nlet a: Nat =\n    1;\n\na\n"
    );
}

/// A facade's lines stack under its `mod`, and the imports written apart after the last facade stay apart from it.
#[test]
fn imports_written_apart_from_a_facade_stay_apart() {
    let source = "mod a;\npub use a/{let a};\n\nmod b;\n\nuse /std/{Nat};\nuse /std/{Bool};\n\nlet n: Nat =\n    1;\n\nn\n";
    assert_eq!(formatted(source), source);
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
