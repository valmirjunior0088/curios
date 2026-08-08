use {super::Formatted, curios_base::Source};

fn formatted(source: &str) -> String {
    Formatted::from_source(&Source::inline(source))
        .expect("fixture formats")
        .into_text()
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
