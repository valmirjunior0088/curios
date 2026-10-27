//! Comments through formatting: a leading one before its item, a trailing one on its line, one above a clause, arm or field above it, an interior one in its construct, and documentation where it was written.

use super::test_support::formatted;

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
        ("-- one", "[1,"),
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

/// A comment between two items is no blank line: what stacks around it stays stacked.
#[test]
fn a_comment_between_two_uses_does_not_part_them() {
    let source = "use /std/{Nat};\n-- why\nuse /std/{Bool};\n\nlet n: Nat =\n    1;\n\nn\n";
    assert_eq!(formatted(source), source);
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
